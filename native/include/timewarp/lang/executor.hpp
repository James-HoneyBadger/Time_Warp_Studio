// SPDX-License-Identifier: MIT
//
// The executor interface and the table that maps a language to one.
//
// An executor is a plain function, exactly as in the Python reference, and for
// the same reason: it holds no state of its own. Everything a run needs lives on
// the `Interpreter` (program lines, variables, the current line) or on the
// `TurtleHost` (the drawing), so a second run cannot inherit the first one's
// leftovers and a test can construct one without an IDE.

#pragma once

#include "timewarp/lang/language.hpp"

#include <string>
#include <string_view>

namespace timewarp::lang {

class Interpreter;
class TurtleHost;

using Executor = std::string (*)(Interpreter&, std::string_view, TurtleHost&);

/// The executor for *language*, or `nullptr` when it has not been ported yet.
///
/// Returning null rather than throwing keeps "the bridge still runs this one"
/// a question the caller can ask, instead of an exception used for control flow.
[[nodiscard]] Executor executor_for(Language language) noexcept;

}  // namespace timewarp::lang
