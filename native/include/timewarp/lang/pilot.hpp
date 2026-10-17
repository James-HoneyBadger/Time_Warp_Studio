// SPDX-License-Identifier: MIT
//
// PILOT, ported from `time_warp/languages/pilot.py`.
//
// A PILOT unit is `X:` or `XY:` where the optional suffix is a condition on the
// previous `M:` ("Y" when it matched, "N" when it did not). Labels are declared
// as `L: NAME` or `*NAME`, and `U:`/`S:` call into them with `E:` returning.
//
// The string-function forms in `C:` (LEN, UPPER, LEFT, ...) are kept exactly:
// they are PILOT's own, not the expression evaluator's.

#pragma once

#include "timewarp/lang/executor.hpp"

namespace timewarp::lang {

/// Executes one PILOT unit and returns the text to display.
[[nodiscard]] std::string execute_pilot(Interpreter& interpreter, std::string_view source,
                                        TurtleHost& turtle);

}  // namespace timewarp::lang
