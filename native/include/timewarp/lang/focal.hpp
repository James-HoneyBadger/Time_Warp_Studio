// SPDX-License-Identifier: MIT
//
// FOCAL-69: DEC's line-numbered teaching language from 1969.
//
// The reference is `time_warp/languages/focal.py`, and every rule that file
// documents is a rule here -- branching by sign, DO returning at the end of the
// program, undefined variables being zero, `!` suppressing the newline inside
// one TYPE. Those are the points where FOCAL differs from BASIC, and a port that
// quietly "fixed" any of them would teach the wrong language.

#pragma once

#include "timewarp/lang/executor.hpp"

namespace timewarp::lang {

/// Executes one line of FOCAL and returns the text to display.
[[nodiscard]] std::string execute_focal(Interpreter& interpreter, std::string_view source,
                                        TurtleHost& turtle);

}  // namespace timewarp::lang
