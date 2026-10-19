// SPDX-License-Identifier: MIT

#include "timewarp/lang/debug.hpp"

#include <utility>

namespace timewarp::lang {

bool DebugSession::note_line(std::size_t index, int line) {
    std::unique_lock lock{mutex_};
    if (stop_.load()) {
        return false;
    }
    const bool breakpoint = breakpoints_.count(index) != 0;
    if (!breakpoint && !step_armed_) {
        return true;
    }
    // A step is one statement: consuming it here is what makes the next pause the
    // *next* line rather than this one again.
    step_armed_ = false;
    paused_ = true;
    paused_line_ = line;
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
}

std::size_t DebugSession::breakpointCount() const {
    const std::scoped_lock lock{mutex_};
    return breakpoints_.size();
}

void DebugSession::request_continue() {
    {
        const std::scoped_lock lock{mutex_};
        step_armed_ = false;
        ++sequence_;
    }
    resumed_.notify_all();
}

void DebugSession::request_step() {
    {
        const std::scoped_lock lock{mutex_};
        step_armed_ = true;
        ++sequence_;
    }
    resumed_.notify_all();
}

void DebugSession::request_stop() {
    {
        const std::scoped_lock lock{mutex_};
        stop_.store(true);
        ++sequence_;
    }
    resumed_.notify_all();
}

bool DebugSession::paused() const {
    const std::scoped_lock lock{mutex_};
    return paused_;
}

}  // namespace timewarp::lang
