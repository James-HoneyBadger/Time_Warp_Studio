// SPDX-License-Identifier: MIT
//
// REXX, ported from `time_warp/languages/rexx.py`.

#pragma once

#include <string>
#include <string_view>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

/// Runs one line of REXX and returns the text to display.
[[nodiscard]] std::string execute_rexx(Interpreter& interpreter,
                                       std::string_view source,
                                       TurtleHost& turtle);

}  // namespace timewarp::lang
