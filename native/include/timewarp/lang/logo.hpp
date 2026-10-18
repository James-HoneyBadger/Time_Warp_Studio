// SPDX-License-Identifier: MIT
//
// Logo.
//
// Whole-program rather than line-by-line: `TO ... END` defines a procedure
// instead of running, and a bracketed command can span lines, so the statement
// boundaries are decided by the loader in `logo.cpp` before anything executes.

#pragma once

#include <string>
#include <string_view>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

/// Runs a Logo program, streaming its output through the interpreter.
std::string execute_logo(Interpreter& interpreter, std::string_view source,
                         TurtleHost& turtle);

/// Runs one Logo command and returns its text without logging it.
///
/// This is the path BASIC takes for a statement it does not recognise: the
/// reference hands such a line to the Logo executor, so `FORWARD 15` in a
/// `.bas` file is Logo's FORWARD, drawn by Logo's turtle code, not a BASIC
/// statement. The interpreter is the caller's, so variables are shared.
std::string execute_logo_command(Interpreter& interpreter, std::string_view command,
                                 TurtleHost& turtle);

/// Whether *word* (in any case) names a command the Logo executor knows.
[[nodiscard]] bool is_logo_command(std::string_view word);

}  // namespace timewarp::lang
