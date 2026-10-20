// SPDX-License-Identifier: MIT
//
// Debugger sessions for the automation API.
//
// The IDE's Debug panel drives a runner through Qt signals; an API caller has no
// widgets, so this adapter turns the same engines into a request/response shape:
// `start` returns at the first pause or at the end, and `resume`/`step`/`stop`
// each return the next state. A line-loop language uses `lang::DebugSession` on
// its own thread; Scheme uses `DebuggerRunner` with direct signal connections so
// a pause is observed even without a Qt event loop.

#pragma once

#include <cstddef>
#include <functional>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <string_view>
#include <vector>

#include "timewarp/lang/language.hpp"

namespace timewarp::api::debug {

struct Variable {
    std::string name;
    std::string type;
    std::string value;
};

struct Snapshot {
    bool paused = false;
    bool finished = false;
    int line = 0;
    int depth = 0;
    std::string procedure;
    /// `breakpoint`, `step`, `data`, `pause`, `finished` or `error`.
    std::string reason;
    std::string changed_variable;
    std::string error;
    std::vector<Variable> variables;
    std::string output;
    std::string svg;
    std::size_t drawing_commands = 0;
};

/// One run with debugger control.
class Session {
public:
    /// Runs `source` until the first pause or completion and returns the session.
    /// Breakpoints are 1-based source lines, the same number the editor shows.
    [[nodiscard]] static std::unique_ptr<Session> start(
        timewarp::lang::Language language, std::string source,
        const std::set<int>& breakpoints, const std::set<std::string>& data_breakpoints,
        std::function<std::string(std::string_view)> sql);

    virtual ~Session() = default;

    [[nodiscard]] virtual Snapshot snapshot() const = 0;
    /// Continue to the next breakpoint or the end, returning the new state.
    [[nodiscard]] virtual Snapshot resume() = 0;
    /// `mode`: 0 step into, 1 step over, 2 step out.
    [[nodiscard]] virtual Snapshot step(int mode) = 0;
    /// End the run and return its final state.
    [[nodiscard]] virtual Snapshot stop() = 0;
};

}  // namespace timewarp::api::debug
