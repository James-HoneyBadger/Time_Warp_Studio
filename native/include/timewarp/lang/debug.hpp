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
#include <set>

namespace timewarp::lang {

class DebugSession {
public:
    /// Called on the interpreter's thread just before the run blocks. `line` is
    /// the source line as the program numbers it, which is what a gutter shows.
    using PauseCallback = std::function<void(int line)>;

    DebugSession() = default;
    DebugSession(const DebugSession&) = delete;
    DebugSession& operator=(const DebugSession&) = delete;

    /// Blocks before executing `index` when a breakpoint is set there or a step
    /// was requested. Returns false when the session asked for the run to stop,
    /// which the interpreter treats as "end the run cleanly".
    bool note_line(std::size_t index, int line);

    /// Zero-based statement indices a run may pause on.
    void set_breakpoints(std::set<std::size_t> lines);
    [[nodiscard]] std::size_t breakpointCount() const;

    /// Resume from a pause and run to the end, or to the next breakpoint.
    void request_continue();
    /// Resume from a pause and stop again on the next statement.
    void request_step();
    /// End the run. Safe to call when nothing is paused.
    void request_stop();

    [[nodiscard]] bool paused() const;
    [[nodiscard]] bool stopRequested() const { return stop_.load(); }

    PauseCallback on_paused;

private:
    mutable std::mutex mutex_;
    std::condition_variable resumed_;
    std::set<std::size_t> breakpoints_;
    /// Bumped by every command, so a command cannot be lost between publishing a
    /// pause and waiting for one.
    std::uint64_t sequence_ = 0;
    std::atomic<bool> stop_{false};
    bool paused_ = false;
    bool step_armed_ = false;
    int paused_line_ = -1;
};

}  // namespace timewarp::lang
