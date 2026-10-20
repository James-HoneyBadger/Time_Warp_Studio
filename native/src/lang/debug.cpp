// SPDX-License-Identifier: MIT

#include "timewarp/lang/debug.hpp"

#include <utility>

namespace timewarp::lang {

bool DebugSession::note_line(std::size_t index, int line) {
    // A run with no breakpoints and no step pending must not pay a lock per
    // statement; CHIP-8 calls a pause point once per opcode.
    if (!engaged_.load(std::memory_order_acquire)) {
        return true;
    }
    bool breakpoint = false;
    {
        const std::scoped_lock lock{mutex_};
        if (stop_.load()) {
            return false;
        }
        breakpoint = breakpoints_.count(index) != 0;
    }
    return wait_at(breakpoint, line);
}

bool DebugSession::note_source_line(int line) {
    if (!engaged_.load(std::memory_order_acquire)) {
        return true;
    }
    bool breakpoint = false;
    {
        const std::scoped_lock lock{mutex_};
        if (stop_.load()) {
            return false;
        }
        // Line-keyed breakpoints pause once per visit: a Smalltalk line can hold
        // several sends, and `last_seen_line_` is what stops Continue from
        // stopping again on the line it just left while still pausing when a
        // loop comes back to it after running another line.
        const bool line_changed = line != last_seen_line_;
        last_seen_line_ = line;
        if (line > 0 && line_changed) {
            breakpoint = breakpoints_.count(static_cast<std::size_t>(line - 1)) != 0;
        }
    }
    return wait_at(breakpoint, line);
}

bool DebugSession::wait_at(bool breakpoint, int line) {
    std::unique_lock lock{mutex_};
    if (stop_.load()) {
        return false;
    }
    if (!breakpoint && !step_armed_) {
        return true;
    }
    // A step is one statement: consuming it here is what makes the next pause the
    // *next* line rather than this one again.
    step_armed_ = false;
    paused_ = true;
    paused_line_ = line;
    last_seen_line_ = line;
    // With the step consumed, only a breakpoint can stop the run again.
    engaged_.store(!breakpoints_.empty(), std::memory_order_release);
    const std::uint64_t sequence = sequence_;
    const PauseCallback callback = on_paused;
    lock.unlock();

    if (callback) {
        callback(line);
    }

    lock.lock();
    resumed_.wait(lock, [this, sequence] {
        return sequence_ != sequence || stop_.load();
    });
    paused_ = false;
    paused_line_ = -1;
    return !stop_.load();
}

void DebugSession::set_breakpoints(std::set<std::size_t> lines) {
    const std::scoped_lock lock{mutex_};
    breakpoints_ = std::move(lines);
    engaged_.store(!breakpoints_.empty() || step_armed_, std::memory_order_release);
}

std::size_t DebugSession::breakpointCount() const {
    const std::scoped_lock lock{mutex_};
    return breakpoints_.size();
}

void DebugSession::request_continue() {
    {
        const std::scoped_lock lock{mutex_};
        step_armed_ = false;
        engaged_.store(!breakpoints_.empty(), std::memory_order_release);
        ++sequence_;
    }
    resumed_.notify_all();
}

void DebugSession::request_step() {
    {
        const std::scoped_lock lock{mutex_};
        step_armed_ = true;
        engaged_.store(true, std::memory_order_release);
        ++sequence_;
    }
    resumed_.notify_all();
}

void DebugSession::request_stop() {
    {
        const std::scoped_lock lock{mutex_};
        stop_.store(true);
        // Wake a run that is not paused: its next pause point takes the slow
        // path and returns false.
        engaged_.store(true, std::memory_order_release);
        ++sequence_;
    }
    resumed_.notify_all();
}

bool DebugSession::paused() const {
    const std::scoped_lock lock{mutex_};
    return paused_;
}

}  // namespace timewarp::lang
