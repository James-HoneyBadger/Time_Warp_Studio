// SPDX-License-Identifier: MIT
//
// APL, as a native `timewarp_lang` executor.

#pragma once

#include <string>
#include <string_view>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

/// Runs one line of APL and returns the text to display.
///
/// APL is line by line: each line is an expression whose value is shown, so an
/// executor call is one line of the program and the interpreter's main loop is
/// the session.
[[nodiscard]] std::string execute_apl(Interpreter& interpreter,
                                      std::string_view source,
                                      TurtleHost& turtle);

}  // namespace timewarp::lang
