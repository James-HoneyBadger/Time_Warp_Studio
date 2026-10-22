// SPDX-License-Identifier: MIT
//
// A REPL session: one interpreter, one submission at a time.
//
// The difference between this and running a program is the one thing a REPL
// promises: the second line sees the first line's bindings. `Interpreter` stores
// a program and its variables separately for exactly this reason, and
// `run_snippet` replaces the former while preserving the latter.
//
// Whole-program languages (Logo, Smalltalk, CHIP-8) still execute one submission
// as a complete program; Logo procedure definitions therefore do not survive a
// submission, and the panel says so rather than pretending otherwise.

#pragma once

#include <map>
#include <string>
#include <string_view>

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/lang/value.hpp"

namespace timewarp::lang {

class ReplSession {
public:
    ReplSession(Language language, TurtleHost& turtle);

    ReplSession(const ReplSession&) = delete;
    ReplSession& operator=(const ReplSession&) = delete;

    /// Executes one submission and returns the text it produced. Errors are part
    /// of the returned text, with the engine's own ❌ prefix.
    [[nodiscard]] std::string execute(std::string_view submission);
    void request_stop() { interpreter_.request_stop(); }
    /// Forgets every binding. The next submission starts from nothing.
    void reset() { interpreter_.load_program({}); }

    void set_sql_callback(Interpreter::SqlCallback callback) {
        interpreter_.set_sql_callback(std::move(callback));
    }
    /// The machine a `POKE` in a prompt line reaches. See `Interpreter::MachineBus`.
    void set_machine_bus(Interpreter::MachineBus bus) {
        interpreter_.set_machine_bus(std::move(bus));
    }

    [[nodiscard]] Language language() const noexcept { return interpreter_.language(); }
    [[nodiscard]] const std::map<std::string, Value>& variables() const noexcept {
        return interpreter_.variables();
    }
    [[nodiscard]] const std::string& error() const noexcept { return interpreter_.error(); }

private:
    Interpreter interpreter_;
};

}  // namespace timewarp::lang
