// SPDX-License-Identifier: MIT
//
// BASIC, ported from `time_warp/languages/basic.py`.

#pragma once

#include <string>
#include <string_view>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

/// Runs one BASIC line and returns the text it produced.
///
/// The line loop is the interpreter's, as it is for every line-by-line
/// language: this function sees one line, may move `current_line` to steer the
/// loop, and returns output (or "" for a statement that prints nothing).
[[nodiscard]] std::string execute_basic(Interpreter& interpreter, std::string_view command,
                                        TurtleHost& turtle);

}  // namespace timewarp::lang
