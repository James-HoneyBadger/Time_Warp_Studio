// SPDX-License-Identifier: MIT
//
// A debugging session: breakpoints, and the command word that decides whether
// the interpreter may run the next statement.
//
// The interpreter calls `note_line` before every statement it is about to
// execute. That call blocks when a breakpoint is set there or when the user
// asked for one step, and returns once the UI sends a command. This is the same
// shape as the reference's `note_step`, and it is deliberately the only place
// the engine knows about a debugger.
//
// #### Why a sequence counter as well as a condition variable
//
// `condition_variable` rules out lost wakeups but not a lost *command*: a
// `step()` arriving between "publish the pause" and "start waiting" would bump
// the command and notify nobody listening. So every command bumps `sequence_`,
// and the waiter captures that counter before it publishes the pause and waits
// for it to change -- a command that overtook the wait is therefore already
// visible to the predicate.
//
// #### Threading
//
// `note_line` runs on the interpreter's thread. Everything else runs on the GUI
// thread. The pause is published through `on_paused`, which the runner re-emits
// as a queued Qt signal, so a callback must not touch Qt widgets directly.

#pragma once

#include <atomic>
#include <condition_variable>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <mutex>
#include <optional>
#include <set>

namespace timewarp::lang {

class DebugSession {
public:
    /// Called on the interpreter's thread just before the run blocks. `line` is
    /// the source line as the program numbers it, which is what a gutter shows.
    using PauseCallback = std::function<void(int line)>;
    /// The same, for a pause caused by a watched variable changing. The name is
    /// the variable as the engine spells it, so the panel can say which one.
    using DataPauseCallback = std::function<void(int line, const std::string& name)>;

    DebugSession() = default;
    DebugSession(const DebugSession&) = delete;
    DebugSession& operator=(const DebugSession&) = delete;

    /// Blocks before executing `index` when a breakpoint is set there or a step
    /// was requested. Returns false when the session asked for the run to stop,
    /// which the interpreter treats as "end the run cleanly".
    bool note_line(std::size_t index, int line);

    /// A pause point keyed by *source line* rather than statement index, for the
    /// whole-program languages: their code is not one statement per line, and
    /// Logo's statement list skips the procedure definition lines.
    ///
    /// `line` is 1-based; 0 means "no source line" (a binary CHIP-8 ROM), where a
    /// step still stops but a breakpoint cannot match. A breakpoint pauses once
    /// per line: a Smalltalk line can hold several message sends, and a Continue
    /// that stops again on the same line is not a continue. A step still stops
    /// at every send.
    ///
    /// Returns false when the session asked for the run to stop.
    bool note_source_line(int line);

    /// Zero-based statement indices a run may pause on.
    void set_breakpoints(std::set<std::size_t> lines);
    [[nodiscard]] std::size_t breakpointCount() const;

    /// Variable names whose *change* pauses the run, compared case-insensitively.
    ///
    /// The interpreter compares its bindings after each statement (or at each
    /// whole-program pause point) and asks here only when something changed. A
    /// data breakpoint therefore costs nothing on a statement that touched
    /// nothing, and the GUI thread may replace the set while the program runs.
    void set_data_breakpoints(std::set<std::string> names);
    [[nodiscard]] bool data_breakpoints_active() const;
    /// True when `name` is one of the watched variables.
    [[nodiscard]] bool data_breakpoint_matches(std::string_view name) const;
    /// Blocks at a pause caused by `name` changing. Returns false when the user
    /// stopped the run instead of continuing.
    bool note_data_change(int line, const std::string& name);

    /// Resume from a pause and run to the end, or to the next breakpoint.
    void request_continue();
    /// Resume from a pause and stop again on the next statement.
    void request_step();
    /// End the run. Safe to call when nothing is paused.
    void request_stop();

    [[nodiscard]] bool paused() const;
    /// True when the current pause was a step rather than a breakpoint or a data
    /// change. Read by an adapter that reports *why* a run stopped.
    [[nodiscard]] bool paused_for_step() const;
    /// True when the current pause is where a rewind landed rather than where the
    /// program got to on its own.
    [[nodiscard]] bool paused_for_rewind() const;
    [[nodiscard]] bool stopRequested() const { return stop_.load(); }

    /// Asks the run to go back `steps` snapshots and stop there.
    ///
    /// The run owns its own state, so this is a request rather than a call: the
    /// interpreter's loop performs the rewind on its own thread and then pauses, and a
    /// run that has no history yet simply carries on. Like every command here it wakes
    /// a paused run, because a debugger that only listens when it is already running is
    /// a debugger with a dead button.
    void request_rewind(std::size_t steps);
    /// The steps the run should go back, once, or nullopt. Read by the interpreter
    /// loop on its own thread.
    [[nodiscard]] std::optional<std::size_t> take_rewind();
    /// Says that the pause about to happen is where a rewind landed.
    void mark_rewound() noexcept;

    PauseCallback on_paused;
    DataPauseCallback on_data_changed;

private:
    /// The one wait both pause points use. `breakpoint` is already decided by the
    /// caller, which is what lets `note_line` match an index and
    /// `note_source_line` match a line.
    bool wait_at(bool breakpoint, int line, const std::string* changed = nullptr);

    mutable std::mutex mutex_;
    std::condition_variable resumed_;
    std::set<std::size_t> breakpoints_;
    /// Watched variable names, upper-cased. Guarded by `mutex_`; the atomic is
    /// the fast path the interpreter reads before walking its bindings.
    std::set<std::string> data_breakpoints_;
    std::atomic<bool> data_engaged_{false};
    /// Bumped by every command, so a command cannot be lost between publishing a
    /// pause and waiting for one.
    std::uint64_t sequence_ = 0;
    std::atomic<bool> stop_{false};
    /// A rewind the loop has not taken yet, and whether the next pause is one.
    std::optional<std::size_t> pending_rewind_;
    bool paused_for_rewind_ = false;
    bool paused_ = false;
    bool step_armed_ = false;
    bool paused_for_step_ = false;
    int paused_line_ = -1;
    /// The line the last whole-program pause point was on, so a line-keyed
    /// breakpoint fires once per visit rather than once per message send on that
    /// line: a Continue that stops again on the line it just left is not a
    /// continue, and returning to the line after running another one pauses again.
    int last_seen_line_ = -1;
    /// Fast path for the hot loops: false when there are no breakpoints and no
    /// step pending, so a CHIP-8 opcode pays one atomic load rather than a lock.
    std::atomic<bool> engaged_{false};
};

}  // namespace timewarp::lang
