// SPDX-License-Identifier: MIT

#include "timewarp/lang/debug.hpp"

#include <cctype>
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

bool DebugSession::wait_at(bool breakpoint, int line, const std::string* changed) {
    std::unique_lock lock{mutex_};
    if (stop_.load()) {
        return false;
    }
    if (!breakpoint && !step_armed_) {
        return true;
    }
    // A step is one statement: consuming it here is what makes the next pause the
    // *next* line rather than this one again.
    paused_for_step_ = !breakpoint && step_armed_;
    step_armed_ = false;
    paused_ = true;
    paused_line_ = line;
    last_seen_line_ = line;
    // With the step consumed, only a breakpoint can stop the run again.
    engaged_.store(!breakpoints_.empty(), std::memory_order_release);
    const std::uint64_t sequence = sequence_;
    const PauseCallback callback = on_paused;
    const DataPauseCallback data_callback = on_data_changed;
    lock.unlock();

    if (changed != nullptr && data_callback) {
        data_callback(line, *changed);
    } else if (callback) {
        callback(line);
    }

    lock.lock();
    resumed_.wait(lock, [this, sequence] {
        return sequence_ != sequence || stop_.load();
    });
    paused_ = false;
    paused_for_step_ = false;
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

void DebugSession::set_data_breakpoints(std::set<std::string> names) {
    std::set<std::string> normalised;
    for (const std::string& name : names) {
        std::string upper;
        upper.reserve(name.size());
        for (const char character : name) {
            upper.push_back(static_cast<char>(
                std::toupper(static_cast<unsigned char>(character))));
        }
        if (!upper.empty()) {
            normalised.insert(std::move(upper));
        }
    }
    {
        const std::scoped_lock lock{mutex_};
        data_breakpoints_ = std::move(normalised);
        data_engaged_.store(!data_breakpoints_.empty(), std::memory_order_release);
    }
    // A data breakpoint alone does not need `engaged_`: the interpreter asks
    // `note_data_change` directly after a statement. A step still does.
}

bool DebugSession::data_breakpoints_active() const {
    return data_engaged_.load(std::memory_order_acquire);
}

bool DebugSession::data_breakpoint_matches(std::string_view name) const {
    if (!data_breakpoints_active()) {
        return false;
    }
    std::string upper;
    upper.reserve(name.size());
    for (const char character : name) {
        upper.push_back(static_cast<char>(
            std::toupper(static_cast<unsigned char>(character))));
    }
    const std::scoped_lock lock{mutex_};
    return data_breakpoints_.count(upper) != 0;
}

bool DebugSession::note_data_change(int line, const std::string& name) {
    return wait_at(true, line, &name);
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

bool DebugSession::paused_for_step() const {
    const std::scoped_lock lock{mutex_};
    return paused_for_step_;
}

}  // namespace timewarp::lang
