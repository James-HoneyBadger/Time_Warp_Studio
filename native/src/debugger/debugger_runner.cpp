// SPDX-License-Identifier: MIT
//
// Implementation of the background debugger runner.  See the header for why the
// control channel is atomic rather than slot-based.

#include "timewarp/debugger/debugger_runner.hpp"

#include <limits>
#include <string_view>
#include <utility>

#include "timewarp/scheme/errors.hpp"
#include "timewarp/scheme/source_map.hpp"

namespace timewarp::debugger {

namespace {

using timewarp::scheme::CallFrame;
using timewarp::scheme::SchemeError;
using timewarp::scheme::SuspendReason;
using timewarp::scheme::SuspendState;
using timewarp::scheme::VariableEntry;

/// The step budget the runner uses: effectively unbounded, so a run only ever
/// stops because the program finished, the user stopped it, or a debugger
/// condition fired.  The *debugger* is what makes the run interruptible; it does
/// not need a time-sliced budget to be responsive.
constexpr std::uint64_t kUnlimitedSteps = std::numeric_limits<std::uint64_t>::max();

/// The length of the longest prefix of `text` that ends on a UTF-8 boundary.
///
/// Output is streamed as it appears, which means a multi-byte character can sit
/// half-written in the buffer when a snapshot is taken.  Handing a truncated
/// sequence to `QString::fromUtf8` would surface a replacement character that the
/// program never printed, so the tail is held back until it completes.
[[nodiscard]] std::size_t utf8_safe_prefix(std::string_view text) noexcept {
    std::size_t cut = text.size();
    std::size_t back = 0;
    while (back < cut && back < 3 &&
           (static_cast<unsigned char>(text[cut - 1 - back]) & 0xC0U) == 0x80U) {
        ++back;
    }
    if (back >= cut) {
        return 0;
    }
    const unsigned char lead = static_cast<unsigned char>(text[cut - 1 - back]);
    std::size_t expected = 0;
    if ((lead & 0x80U) == 0x00U) {
        expected = 1;
    } else if ((lead & 0xE0U) == 0xC0U) {
        expected = 2;
    } else if ((lead & 0xF0U) == 0xE0U) {
        expected = 3;
    } else if ((lead & 0xF8U) == 0xF0U) {
        expected = 4;
    } else {
        return cut;  // not a lead byte at all; let Qt substitute it
    }
    if (1 + back < expected) {
        return cut - 1 - back;  // the sequence is not complete yet
    }
    return cut;
}

}  // namespace

std::string_view to_string(PauseCause cause) noexcept {
    switch (cause) {
        case PauseCause::Breakpoint:
            return "breakpoint";
        case PauseCause::Step:
            return "step";
        case PauseCause::PauseRequest:
            return "pause";
        case PauseCause::DataBreakpoint:
            return "data";
        case PauseCause::Error:
            return "error";
        case PauseCause::Finished:
            return "finished";
    }
    return "unknown";
}

std::string_view to_string(DebugCommand command) noexcept {
    switch (command) {
        case DebugCommand::None:
            return "none";
        case DebugCommand::Continue:
            return "continue";
        case DebugCommand::Pause:
            return "pause";
        case DebugCommand::StepInto:
            return "step-into";
        case DebugCommand::StepOver:
            return "step-over";
        case DebugCommand::StepOut:
            return "step-out";
        case DebugCommand::Stop:
            return "stop";
    }
    return "unknown";
}

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

DebuggerRunner::DebuggerRunner(QObject* parent) : QThread(parent) {
    // Nothing to do here on purpose.  The arena, machine and source map are
    // created inside run() so they live and die on the runner thread; the
    // controller, however, is this object and it is already attached.
}

DebuggerRunner::~DebuggerRunner() {
    // Never leave a run behind.  A stopped runner unblocks by itself; the wait is
    // what makes destruction safe rather than a use-after-free waiting to happen.
    request_stop();
    if (isRunning()) {
        wait();
    }
}

void DebuggerRunner::load_program(const QString& source, const QString& name) {
    source_ = source;
    name_ = name;
    const std::lock_guard<std::mutex> lock{snapshot_mutex_};
    captured_ = false;
    snapshot_ = DebugSnapshot{};
}

// ---------------------------------------------------------------------------
// Control channel
// ---------------------------------------------------------------------------

void DebuggerRunner::post(DebugCommand command) {
    {
        const std::lock_guard<std::mutex> lock{command_mutex_};
        command_ = command;
        command_pending_.store(true, std::memory_order_release);
    }
    // The bump is load-bearing: std::atomic::wait re-checks the value after each
    // notification, so notifying a word that nobody changed releases nobody.
    // Incrementing makes the change observable, and reading the sequence before
    // testing the command means a parked runner can never miss a request.
    notification_.fetch_add(1, std::memory_order_release);
    notification_.notify_all();
}

DebugCommand DebuggerRunner::take_command() {
    const std::lock_guard<std::mutex> lock{command_mutex_};
    const DebugCommand command = command_;
    command_ = DebugCommand::None;
    command_pending_.store(false, std::memory_order_release);
    return command;
}

DebugCommand DebuggerRunner::await_resume_command() {
    for (;;) {
        if (stop_requested_.load(std::memory_order_acquire)) {
            return DebugCommand::Stop;
        }
        // Snapshot before testing, so a request that lands between the two
        // cannot be slept through.
        const std::uint64_t observed = notification_.load(std::memory_order_acquire);
        const DebugCommand command = take_command();
        if (command != DebugCommand::None && command != DebugCommand::Pause) {
            return command;
        }
        // A Pause while already paused asks for nothing; fall through and keep
        // waiting rather than returning, otherwise the GUI's pause button would
        // silently unpause the program.
        notification_.wait(observed, std::memory_order_acquire);
    }
}

void DebuggerRunner::request_continue() { post(DebugCommand::Continue); }
void DebuggerRunner::request_pause() { post(DebugCommand::Pause); }
void DebuggerRunner::request_step_into() { post(DebugCommand::StepInto); }
void DebuggerRunner::request_step_over() { post(DebugCommand::StepOver); }
void DebuggerRunner::request_step_out() { post(DebugCommand::StepOut); }

void DebuggerRunner::request_stop() {
    // Sticky as well as posted: the flag survives the case where the runner is
    // not parked at all, and it cannot be cleared by a later continue.
    stop_requested_.store(true, std::memory_order_release);
    post(DebugCommand::Stop);
}

void DebuggerRunner::set_breakpoints(QVector<int> lines) {
    {
        const std::lock_guard<std::mutex> lock{breakpoint_mutex_};
        breakpoints_ = std::move(lines);
    }
    breakpoint_generation_.fetch_add(1, std::memory_order_release);
}

// ---------------------------------------------------------------------------
// Inspection
// ---------------------------------------------------------------------------

DebugSnapshot DebuggerRunner::snapshot() const {
    const std::lock_guard<std::mutex> lock{snapshot_mutex_};
    return snapshot_;
}

QString DebuggerRunner::output() const {
    const std::lock_guard<std::mutex> lock{output_mutex_};
    return output_text_;
}

// ---------------------------------------------------------------------------
// Runner-thread helpers
// ---------------------------------------------------------------------------

void DebuggerRunner::refresh_breakpoints() {
    // One atomic load per form; the mutex is only taken when the set actually
    // changed, so editing breakpoints never slows the running program down.
    const std::uint64_t generation = breakpoint_generation_.load(std::memory_order_acquire);
    if (generation == breakpoint_generation_seen_) {
        return;
    }
    const std::lock_guard<std::mutex> lock{breakpoint_mutex_};
    breakpoint_cache_ = breakpoints_;
    breakpoint_generation_seen_ = generation;
}

bool DebuggerRunner::breakpoint_hit(int line) const {
    if (line <= 0 || breakpoint_cache_.isEmpty()) {
        return false;
    }
    return breakpoint_cache_.contains(line);
}

void DebuggerRunner::set_data_breakpoints(const QStringList& names) {
    QStringList trimmed;
    for (const QString& name : names) {
        const QString value = name.trimmed();
        if (!value.isEmpty() && !trimmed.contains(value)) {
            trimmed.append(value);
        }
    }
    {
        const std::lock_guard<std::mutex> lock{data_breakpoint_mutex_};
        data_breakpoints_ = std::move(trimmed);
    }
    data_breakpoint_generation_.fetch_add(1, std::memory_order_release);
}

void DebuggerRunner::refresh_data_breakpoints() {
    const std::uint64_t generation =
        data_breakpoint_generation_.load(std::memory_order_acquire);
    if (generation == data_breakpoint_generation_seen_) {
        return;
    }
    {
        const std::lock_guard<std::mutex> lock{data_breakpoint_mutex_};
        data_breakpoint_cache_ = data_breakpoints_;
    }
    data_breakpoint_generation_seen_ = generation;
    // A new set is a new comparison: the previous form's bindings are not a
    // baseline for a watch the user has only just added, or the first form after
    // it would report a change that happened before the watch existed.
    bindings_primed_ = false;
    last_bindings_.clear();
}

QString DebuggerRunner::data_changed() {
    if (machine_ == nullptr || data_breakpoint_cache_.isEmpty()) {
        return {};
    }
    QHash<QString, QString> current;
    // The display's four-frame window is not enough for a change: a watched local
    // in a deeper recursion is still the variable the user asked about. `max` is
    // deliberate and only paid when data breakpoints are armed.
    for (const VariableEntry& entry :
         machine_->visible_variables(std::numeric_limits<std::uint32_t>::max())) {
        // Innermost frame first: a shadowed name must keep the inner binding, or
        // a recursive local would be compared against its caller's copy and the
        // change the user is watching would be invisible.
        const QString name = QString::fromStdString(entry.name);
        if (!current.contains(name)) {
            current.insert(name, QString::fromStdString(entry.rendered));
        }
    }
    if (!bindings_primed_) {
        last_bindings_ = std::move(current);
        bindings_primed_ = true;
        return {};
    }

    QString changed;
    for (auto it = current.cbegin(); it != current.cend() && changed.isEmpty(); ++it) {
        const auto previous = last_bindings_.constFind(it.key());
        if (previous == last_bindings_.cend() || previous.value() != it.value()) {
            // Only a watched name matters. Checking the watch set here rather
            // than the whole set keeps an unrelated assignment from allocating a
            // snapshot and calling it a pause.
            for (const QString& watched : data_breakpoint_cache_) {
                if (watched.compare(it.key(), Qt::CaseInsensitive) == 0) {
                    changed = it.key();
                    break;
                }
            }
        }
    }
    if (changed.isEmpty()) {
        for (auto it = last_bindings_.cbegin(); it != last_bindings_.cend() && changed.isEmpty();
             ++it) {
            if (current.contains(it.key())) {
                continue;
            }
            for (const QString& watched : data_breakpoint_cache_) {
                if (watched.compare(it.key(), Qt::CaseInsensitive) == 0) {
                    changed = it.key();
                    break;
                }
            }
        }
    }
    last_bindings_ = std::move(current);
    return changed;
}

void DebuggerRunner::stream_output() {
    if (machine_ == nullptr) {
        return;
    }
    const std::string& text = machine_->output();
    const std::size_t pending = emitted_output_;
    if (text.size() <= pending) {
        return;
    }
    const std::size_t safe =
        pending + utf8_safe_prefix(std::string_view{text}.substr(pending));
    if (safe <= pending) {
        return;  // a partial character is all that is new so far
    }
    emitted_output_ = safe;
    const QString piece =
        QString::fromUtf8(text.data() + pending, static_cast<int>(safe - pending));
    {
        const std::lock_guard<std::mutex> lock{output_mutex_};
        output_text_ += piece;
    }
    emit output_available(piece);
}

void DebuggerRunner::capture_snapshot(const SuspendState& state, PauseCause cause,
                                      const QString& error, QString changed) {
    DebugSnapshot snapshot;
    snapshot.line = static_cast<int>(state.span.first_line);
    snapshot.last_line = static_cast<int>(state.span.last_line);
    snapshot.depth = static_cast<int>(state.depth);
    snapshot.steps = state.steps;
    snapshot.procedure = QString::fromUtf8(state.procedure.data(),
                                          static_cast<int>(state.procedure.size()));
    snapshot.error = error;
    snapshot.cause = cause;
    snapshot.changed_variable = std::move(changed);

    if (machine_ != nullptr) {
        // Both of these read a state that is consistent here: on_form is called
        // at the boundary *before* the current form's own frames are pushed.
        for (const CallFrame& frame : machine_->call_stack()) {
            StackFrameInfo info;
            info.procedure = QString::fromStdString(frame.procedure);
            info.line = static_cast<int>(frame.span.first_line);
            snapshot.stack.push_back(std::move(info));
        }
        for (const VariableEntry& entry : machine_->visible_variables()) {
            snapshot.variables.push_back({QString::fromStdString(entry.name),
                                          QString::fromStdString(entry.kind),
                                          QString::fromStdString(entry.rendered)});
        }
    }

    {
        const std::lock_guard<std::mutex> lock{snapshot_mutex_};
        snapshot_ = std::move(snapshot);
        captured_ = true;
    }
}

// ---------------------------------------------------------------------------
// The suspension controller
// ---------------------------------------------------------------------------

void DebuggerRunner::apply_step(DebugCommand command, std::uint32_t depth) {
    step_mode_ = command == DebugCommand::StepInto
                     ? StepMode::Into
                     : (command == DebugCommand::StepOver ? StepMode::Over
                                                          : StepMode::Out);
    stepping_ = true;
    step_depth_ = depth;
}

bool DebuggerRunner::on_form(const SuspendState& state) {
    if (stop_requested_.load(std::memory_order_acquire)) {
        return false;
    }
    if (machine_ == nullptr) {
        return true;
    }

    refresh_breakpoints();

    const bool breakpoint =
        state.span.known() &&
        breakpoint_hit(static_cast<int>(state.span.first_line));

    // A command may already be waiting: a pause asked for while the program was
    // running, or a step issued from a previous suspension.
    if (command_pending_.load(std::memory_order_acquire)) {
        const DebugCommand command = take_command();
        switch (command) {
            case DebugCommand::Stop:
                return false;
            case DebugCommand::Pause:
                return suspend(state, PauseCause::PauseRequest);
            case DebugCommand::Continue:
                stepping_ = false;
                break;
            case DebugCommand::StepInto:
            case DebugCommand::StepOver:
            case DebugCommand::StepOut:
                apply_step(command, state.depth);
                break;
            case DebugCommand::None:
                break;
        }
    }

    // The form we resumed from has already been reported.  Deciding again would
    // either re-pause on the spot or, for a step, report the very form the step
    // was asked for as its own target -- so "step" would look like it did
    // nothing.  Keying off the step count is precise: the parked form keeps its
    // count until it is charged, and everything after it has moved on.
    if (state.steps == skip_until_steps_) {
        stream_output();
        return true;
    }

    if (stepping_) {
        const bool reached =
            step_mode_ == StepMode::Into ||
            (step_mode_ == StepMode::Over && state.depth <= step_depth_) ||
            (step_mode_ == StepMode::Out && state.depth < step_depth_);
        if (reached) {
            stepping_ = false;
            return suspend(state, PauseCause::Step);
        }
        stream_output();
        return true;
    }

    // A data breakpoint is checked after stepping, so a Step command stops for
    // the step the user asked for rather than being reported as a variable hit.
    // The comparison only runs when a set is armed; an ordinary debug run pays a
    // generation load per form.
    refresh_data_breakpoints();
    QString changed;
    if (!data_breakpoint_cache_.isEmpty()) {
        changed = data_changed();
    }

    // A line breakpoint the user set explicitly wins when both fire at the same
    // form, which is what a debugger that stops at the line first does. The data
    // comparison has already run, so its baseline is current either way.
    if (breakpoint) {
        return suspend(state, PauseCause::Breakpoint);
    }
    if (!changed.isEmpty()) {
        return suspend(state, PauseCause::DataBreakpoint, changed);
    }

    stream_output();
    return true;
}

bool DebuggerRunner::suspend(const SuspendState& state, PauseCause cause,
                             QString changed) {
    capture_snapshot(state, cause, QString{}, std::move(changed));
    paused_.store(true, std::memory_order_release);
    emit paused(static_cast<int>(state.span.first_line), static_cast<int>(state.depth),
                QString::fromUtf8(state.procedure.data(),
                                  static_cast<int>(state.procedure.size())));

    const DebugCommand command = await_resume_command();

    paused_.store(false, std::memory_order_release);
    emit resumed();

    // Whatever the user asked for, the form we were stopped at has already been
    // reported.  Making the decision again would re-pause on the spot -- which
    // for a breakpoint means "continue" never gets past it.
    skip_until_steps_ = state.steps;

    switch (command) {
        case DebugCommand::Stop:
            // The driver loop sees the flag and unwinds cleanly.
            stop_requested_.store(true, std::memory_order_release);
            break;
        case DebugCommand::StepInto:
        case DebugCommand::StepOver:
        case DebugCommand::StepOut:
            apply_step(command, state.depth);
            break;
        case DebugCommand::Continue:
        case DebugCommand::Pause:
        case DebugCommand::None:
            stepping_ = false;
            break;
    }

    // Always refuse to continue *inside this form*.  The machine re-tests the
    // form when the driver calls back in, which is what keeps the step count
    // honest and what lets a stop or an error unwind through one clean boundary.
    return false;
}

// ---------------------------------------------------------------------------
// The runner thread
// ---------------------------------------------------------------------------

void DebuggerRunner::run() {
    emit run_started();

    timewarp::scheme::NullHost fallback;
    timewarp::scheme::Host& host = host_ != nullptr ? *host_ : fallback;

    // Locals: they must outlive the run, and putting them on this stack is the
    // simplest way to guarantee that.
    timewarp::scheme::Arena arena;
    timewarp::scheme::SourceMap source_map;
    timewarp::scheme::Machine machine{arena, host};

    machine_ = &machine;
    emitted_output_ = 0;
    {
        const std::lock_guard<std::mutex> lock{output_mutex_};
        output_text_.clear();
    }
    stepping_ = false;
    skip_until_steps_ = ~std::uint64_t{0};
    breakpoint_generation_seen_ = 0;
    breakpoint_cache_.clear();
    // A runner can be reused for a second program. The data-breakpoint cache is
    // per-run state: carrying the previous program's final bindings into the new
    // one would either report a change that never happened or hide the first real
    // one behind a stale baseline.
    data_breakpoint_generation_seen_ = 0;
    data_breakpoint_cache_.clear();
    last_bindings_.clear();
    bindings_primed_ = false;
    machine.set_suspension_controller(this);

    bool completed = false;
    QString error;

    try {
        machine.load(source_.toStdString(), &source_map);

        for (;;) {
            const SuspendReason reason = machine.step_quantum(kUnlimitedSteps);
            if (reason == SuspendReason::Finished) {
                completed = true;
                break;
            }
            if (stop_requested_.load(std::memory_order_acquire)) {
                break;
            }
            // A debugger pause.  The controller has already waited for and
            // applied the user's next command; re-entering continues from the
            // very form that was suspended.
            if (reason == SuspendReason::QuantumExhausted) {
                continue;
            }
        }
    } catch (const SchemeError& thrown) {
        error = QString::fromUtf8(thrown.what());
        if (break_on_error_) {
            // Best effort: the machine's stack is no longer meaningful after an
            // error, so this reports where it died rather than a live frame.
            const SuspendState state = machine.suspend_state();
            capture_snapshot(state, PauseCause::Error, error);
            paused_.store(true, std::memory_order_release);
            emit paused(static_cast<int>(state.span.first_line),
                        static_cast<int>(state.depth), state.procedure.empty()
                                                           ? QString{}
                                                           : QString::fromUtf8(
                                                                 state.procedure.data(),
                                                                 static_cast<int>(
                                                                     state.procedure.size())));
            (void)await_resume_command();
            paused_.store(false, std::memory_order_release);
            emit resumed();
        }
    } catch (const std::exception& thrown) {
        error = QString::fromUtf8(thrown.what());
    }

    stream_output();
    // A run that never paused still leaves bindings behind, and the variables
    // panel shows what the program ended with. Taken here because the machine is
    // released on the next line: afterwards there is nothing left to ask. A pause
    // or an error already recorded a more precise state, so this defers to it.
    if (machine_ != nullptr) {
        bool already = false;
        {
            const std::lock_guard<std::mutex> lock{snapshot_mutex_};
            already = captured_;
        }
        if (!already) {
            capture_snapshot(machine.suspend_state(),
                             error.isEmpty() ? PauseCause::Finished : PauseCause::Error,
                             error);
        }
    }
    machine_ = nullptr;
    machine.set_suspension_controller(nullptr);

    emit run_finished(completed && error.isEmpty(), error);
}

}  // namespace timewarp::debugger
