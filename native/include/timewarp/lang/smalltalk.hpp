// SPDX-License-Identifier: MIT
//
// Smalltalk, as a native `timewarp_lang` executor.

#pragma once

#include <string>
#include <string_view>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

/// Reads a Smalltalk file: definitions first, then the code in it.
[[nodiscard]] std::string execute_smalltalk(Interpreter& interpreter,
                                            std::string_view source,
                                            TurtleHost& turtle);

}  // namespace timewarp::lang
