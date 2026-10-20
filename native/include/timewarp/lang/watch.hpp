// SPDX-License-Identifier: MIT
//
// Watch expressions: arithmetic over the variables a paused program has.
//
// A watch is deliberately *not* a second interpreter. It is the same
// `timewarp_expr` evaluator a program's arithmetic uses, given the bindings the
// pause reported, so `X * 2` means exactly what it means in the program that
// owns X. A binding that is text is visible by its own name and as `NAME$`, which
// is how the evaluator spells it everywhere else.
//
// Qt-free, so the panel's values are testable without a window.

#pragma once

#include <map>
#include <string>
#include <string_view>
#include <vector>

#include "timewarp/lang/value.hpp"

namespace timewarp::lang {

struct WatchResult {
    std::string expression;
    /// What the panel shows: a rendered number, a text value, or "?" on failure.
    std::string value;
    bool ok = false;
    /// Why it failed, for the cell's tooltip. Empty when `ok`.
    std::string error;
};

/// Evaluates one expression against `variables`.
///
/// A bare variable name yields that variable's rendered value, text included;
/// anything else is arithmetic through the expression evaluator and therefore a
/// number. An unknown name is an error rather than zero: a watch that silently
/// reads 0 for a typo is worse than one that says it could not find the name.
[[nodiscard]] WatchResult evaluate_watch(std::string_view expression,
                                         const std::map<std::string, Value>& variables);

/// Evaluates `expressions` in order. Each result carries its own failure, so one
/// bad watch does not blank the rest.
[[nodiscard]] std::vector<WatchResult> evaluate_watches(
    const std::vector<std::string>& expressions,
    const std::map<std::string, Value>& variables);

}  // namespace timewarp::lang
