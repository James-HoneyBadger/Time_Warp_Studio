// SPDX-License-Identifier: MIT
//
// A background debugger runner: executes a loaded Scheme program on its own
// thread and services pause / step / breakpoint requests from the GUI thread.
//
// ═══════════════════════════════════════════════════════════════════════════
// Why this subclasses QThread, and why control does NOT go through slots
// ═══════════════════════════════════════════════════════════════════════════
//
// A long-running interpreter loop and a Qt event loop cannot both own the same
// thread.  `run()` here *is* the interpreter loop, so the thread is not spinning
// an event loop and queued slot invocations to this object would never be
// delivered -- they would queue up forever.
//
// That interacts with a classic QThread trap: a QThread *object* lives in the
// thread that constructed it, so its slots execute there, not in `run()`'s
// thread.  Subclassing QThread and expecting `run()`'s thread to receive slot
// calls is the single most common Qt threading mistake.
//
// The consequence is deliberate: `request_*` are **plain thread-safe methods**,
// not slots.  They may be called from the GUI thread (or anywhere) and they
// never touch interpreter state -- they publish an atomic command and wake the
// runner.  Making them slots would invite the assumption that they run on the
// runner thread, which they must not.
//
// The reverse direction is fine and is used: Qt signals emitted from `run()` are
// thread-safe and arrive queued on the GUI thread.
//
// ═══════════════════════════════════════════════════════════════════════════
// Latency
// ═══════════════════════════════════════════════════════════════════════════
//
// The controller is consulted at the start of every *compound form*, so a pause
// or a step lands within one expression of being requested rather than at some
// arbitrary quantum boundary.  The cost on the hot path is one atomic load plus
// one virtual call per compound form, and it disappears entirely when no
// controller is attached.
//
// ═══════════════════════════════════════════════════════════════════════════
// Why the wakeup word is bumped rather than notified
// ═══════════════════════════════════════════════════════════════════════════
//
// `std::atomic::wait` re-checks the value after every notification and goes back
// to sleep if it is unchanged, so `notify_all()` on a word nobody modified
// releases nobody.  Every request therefore *increments* `notification_`, which
// also means the parked runner cannot miss one: the sequence is read before the
// command is tested.

#pragma once

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <mutex>
#include <string>
#include <vector>

#include <QHash>
#include <QPair>
#include <QString>
#include <QThread>
#include <QVector>
#include <QStringList>

#include "timewarp/scheme/host.hpp"
#include "timewarp/scheme/machine.hpp"

namespace timewarp::debugger {

/// Why the machine stopped, from the debugger's point of view.
enum class PauseCause : std::uint8_t {
    Breakpoint = 0,
    Step,
    PauseRequest,
    /// A watched variable changed value.
    DataBreakpoint,
    Error,
    /// The run ended without pausing. The snapshot is still taken, because a
    /// program that finished has bindings and the variables panel shows what a
    /// run left behind -- but nothing was suspended, so the GUI shows no pause.
    Finished,
};

/// A command the GUI thread sends to the runner thread.
enum class DebugCommand : std::uint8_t {
    None = 0,
    Continue,
    Pause,
    StepInto,
    StepOver,
    StepOut,
    Stop,
};

[[nodiscard]] std::string_view to_string(PauseCause cause) noexcept;
[[nodiscard]] std::string_view to_string(DebugCommand command) noexcept;

/// One entry of the paused call stack.
struct StackFrameInfo {
    QString procedure;
    int line = 0;
};

/// One binding the machine could see when it paused.
///
/// The kind is the machine's own `type_name` for the value, not a guess from the
/// printed form: `3` and `"3"` render differently but `#t` and `(#t)` are easier
/// to tell apart by kind than by eye.
struct DebugVariable {
    QString name;
    QString kind;
    QString rendered;
};

/// Everything the GUI needs to render the paused state.
///
/// Returned by value from `snapshot()`, which copies under a lock, so a caller
/// never holds a reference into state the runner thread is about to rewrite.
struct DebugSnapshot {
    int line = 0;
    int last_line = 0;
    int depth = 0;
    std::uint64_t steps = 0;
    QString procedure;
    QString error;
    PauseCause cause = PauseCause::PauseRequest;
    QVector<StackFrameInfo> stack;
    /// Innermost bindings first.
    QVector<DebugVariable> variables;
    /// The variable whose change caused a `DataBreakpoint` pause, else empty.
    QString changed_variable;
};

/// Executes a program on a background thread with debugger control.
///
/// Typical use:
///
///     DebuggerRunner runner;
///     connect(&runner, &DebuggerRunner::paused, ...);
///     runner.set_breakpoints({4, 9});
///     runner.load_program(source);
///     runner.start();                 // run() becomes the interpreter loop
///     ...
///     runner.request_pause();         // callable from the GUI thread
///     ...
///     runner.request_stop();
///     runner.wait();
class DebuggerRunner : public QThread,
                        public timewarp::scheme::SuspensionController {
    Q_OBJECT

public:
    explicit DebuggerRunner(QObject* parent = nullptr);
    ~DebuggerRunner() override;

    /// Stores the program.  Call before `start()`; the source is read on the
    /// runner thread.
    void load_program(const QString& source, const QString& name = {});

    /// Optional turtle / step-budget host.  Must outlive the run.  When null the
    /// runner uses a do-nothing host.
    void set_host(timewarp::scheme::Host* host) noexcept { host_ = host; }

    // -- control: thread-safe, callable from any thread -------------------

    void request_continue();
    void request_pause();
    void request_step_into();
    void request_step_over();
    void request_step_out();
    /// Stops the program and unblocks the runner.  Sticky: it cannot be undone.
    void request_stop();

    /// Replaces the breakpoint set (1-based lines).  The runner picks the new
    /// set up on its next form, without locking on the hot path.
    ///
    /// A breakpoint fires whenever a form that *opens* on the line is about to
    /// run, which is deliberately the simple rule.  The obvious refinement --
    /// stop only on the first form of the line -- requires knowing that
    /// execution has left the line and come back, and the answer is not always
    /// observable: a tail-recursive loop whose body sits on one line never
    /// reports another line, so a breakpoint added there would silently never
    /// fire.  Over-stopping is recoverable, with one extra press of continue;
    /// a breakpoint that does nothing is not.
    void set_breakpoints(QVector<int> lines);

    /// Variable names whose change pauses the run. The machine's visible
    /// bindings are compared at each form; the first form that changes one of
    /// these stops. Empty disables the comparison, which is what keeps a normal
    /// debug run from allocating a binding table per form.
    void set_data_breakpoints(const QStringList& names);

    /// When true, a runtime error pauses instead of ending the run.
    void set_break_on_error(bool enabled) noexcept { break_on_error_ = enabled; }

    // -- inspection ------------------------------------------------------

    /// Valid once `paused` has been emitted.  Copies under a lock.
    [[nodiscard]] DebugSnapshot snapshot() const;
    /// Everything the program has printed so far. Updated as output streams, so a
    /// caller that is not connected to the signal (the API polls the runner) can
    /// still read it while the run is paused or finished.
    [[nodiscard]] QString output() const;
    [[nodiscard]] bool is_paused() const noexcept {
        return paused_.load(std::memory_order_acquire);
    }

    // -- scheme::SuspensionController -------------------------------------
    //
    // Runs on the *runner* thread, once per compound form.  Public only because
    // it is an override; do not call it.

    bool on_form(const timewarp::scheme::SuspendState& state) override;

signals:
    /// Emitted on the runner thread when `run()` begins.
    void run_started();
    /// Incremental program output.  Emitted as it is produced, not in one lump.
    void output_available(const QString& text);
    /// Emitted when the program suspends.  The full state is in `snapshot()`.
    void paused(int line, int depth, const QString& procedure);
    /// Emitted when a suspended program resumes.
    void resumed();
    /// Emitted exactly once, at the end.  `completed` is false for a stop, an
    /// error, or a cancellation.
    void run_finished(bool completed, const QString& error);

protected:
    void run() override;

private:
    /// Publishes a command.  Bumps the wakeup word so a parked runner wakes.
    void post(DebugCommand command);
    /// Consumes a pending command, if any.  Runner thread only.
    [[nodiscard]] DebugCommand take_command();
    /// Parks until a command that actually resumes arrives.  Runner thread only.
    [[nodiscard]] DebugCommand await_resume_command();

    /// Runner thread only.
    void capture_snapshot(const timewarp::scheme::SuspendState& state,
                          PauseCause cause, const QString& error = {},
                          QString changed = {});
    /// Publishes the pause, waits for the user's next command, and applies it.
    /// Always returns false so the machine yields at this exact form.  Runner
    /// thread only.
    [[nodiscard]] bool suspend(const timewarp::scheme::SuspendState& state,
                               PauseCause cause, QString changed = {});
    /// Arms the stepping state for a step command.  Runner thread only.
    void apply_step(DebugCommand command, std::uint32_t depth);
    void refresh_breakpoints();
    [[nodiscard]] bool breakpoint_hit(int line) const;
    /// Copies the data-breakpoint set when the GUI changed it. Runner thread only.
    void refresh_data_breakpoints();
    /// The first watched binding that changed since the previous form, or empty.
    /// Primes the comparison on the first call and reports nothing then.
    [[nodiscard]] QString data_changed();
    void stream_output();

    /// Whether this run has already published a snapshot. A pause or an error
    /// records a more precise state than "where it ended", so the final capture
    /// defers to it. Guarded by `snapshot_mutex_`.
    bool captured_ = false;

    // -- command channel --------------------------------------------------
    std::atomic<std::uint64_t> notification_{0};
    std::atomic<bool> command_pending_{false};
    std::atomic<bool> stop_requested_{false};
    std::atomic<bool> paused_{false};
    mutable std::mutex command_mutex_;
    DebugCommand command_ = DebugCommand::None;

    // -- breakpoints ------------------------------------------------------
    mutable std::mutex breakpoint_mutex_;
    QVector<int> breakpoints_;
    std::atomic<std::uint64_t> breakpoint_generation_{1};
    /// Runner-thread cache, refreshed when the generation changes.
    QVector<int> breakpoint_cache_;
    std::uint64_t breakpoint_generation_seen_ = 0;

    // -- data breakpoints -------------------------------------------------
    mutable std::mutex data_breakpoint_mutex_;
    QStringList data_breakpoints_;
    std::atomic<std::uint64_t> data_breakpoint_generation_{1};
    QStringList data_breakpoint_cache_;
    std::uint64_t data_breakpoint_generation_seen_ = 0;
    /// The bindings as of the previous form, name to rendered value. Runner
    /// thread only.
    QHash<QString, QString> last_bindings_;
    bool bindings_primed_ = false;

    // -- stepping ---------------------------------------------------------
    enum class StepMode : std::uint8_t { Into, Over, Out };

    bool stepping_ = false;
    StepMode step_mode_ = StepMode::Into;
    std::uint32_t step_depth_ = 0;
    /// Step count at which the machine was suspended.  The form it is parked at
    /// keeps that count until it is charged, so ignoring any form that shares it
    /// is exactly "leave the form I resumed from alone" -- with no blind spot
    /// for the form that follows.
    std::uint64_t skip_until_steps_ = ~std::uint64_t{0};

    // -- published state --------------------------------------------------
    mutable std::mutex snapshot_mutex_;
    DebugSnapshot snapshot_;
    mutable std::mutex output_mutex_;
    QString output_text_;

    // -- runner-thread state ----------------------------------------------
    timewarp::scheme::Machine* machine_ = nullptr;
    timewarp::scheme::Host* host_ = nullptr;
    std::size_t emitted_output_ = 0;
    QString source_;
    QString name_;
    bool break_on_error_ = false;
};

}  // namespace timewarp::debugger
