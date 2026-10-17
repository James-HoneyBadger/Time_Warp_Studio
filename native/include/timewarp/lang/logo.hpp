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

}  // namespace timewarp::lang
