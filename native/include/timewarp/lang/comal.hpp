// SPDX-License-Identifier: AGPL-3.0-only
//
// COMAL-80, ported from `time_warp/languages/comal.py`.

#pragma once

#include "timewarp/lang/executor.hpp"

namespace timewarp::lang {

/// Executes one line of COMAL and returns the text to display.
[[nodiscard]] std::string execute_comal(Interpreter& interpreter, std::string_view source,
                                        TurtleHost& turtle);

}  // namespace timewarp::lang
