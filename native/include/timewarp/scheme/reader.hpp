// SPDX-License-Identifier: MIT
//
// Iterative tokenizer and parser.
//
// The reference's `_parse()` recursed once per nesting level via `parse_expr()`,
// so reading deeply nested source exhausted the native stack before evaluation
// began.  Here an explicit frame stack holds the lists under construction.

#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "timewarp/scheme/source_map.hpp"
#include "timewarp/scheme/value.hpp"

namespace timewarp::scheme {

/// A token together with the 1-based line it started on.
struct SourceToken {
    std::string text;
    std::uint32_t line = 1;
};

/// Splits source into tokens, matching the reference's `_tokenize()`.
///
/// Two quirks are preserved deliberately: a string token keeps its surrounding
/// quotes (so `atom()` can tell strings from symbols), and `|a b|` yields the
/// inner text without the bars.
[[nodiscard]] std::vector<std::string> tokenize(std::string_view source);

/// As `tokenize`, but also reports the line each token started on.
///
/// The reference carried no positions at all, so this is an addition rather than
/// a port; `tokenize` remains the faithful one.
[[nodiscard]] std::vector<SourceToken> tokenize_with_lines(
    std::string_view source);

/// Converts one token into a value, matching the reference's `_atom()`.
[[nodiscard]] Value atom(Arena& arena, std::string_view token);

/// Parses a whole program into top-level forms.
[[nodiscard]] std::vector<Value> parse_program(Arena& arena,
                                              std::string_view source);

/// Parses a whole program, recording the span of every compound form.
///
/// When `source_map` is null this behaves exactly like the two-argument form, so
/// a normal run pays nothing for the debugging surface.
[[nodiscard]] std::vector<Value> parse_program(Arena& arena,
                                              std::string_view source,
                                              SourceMap* source_map);

/// Parses exactly one expression; raises if the source holds none or several.
[[nodiscard]] Value parse_one(Arena& arena, std::string_view source);

/// Parses a token as a Python `int` would, returning nothing when it fails.
[[nodiscard]] bool parse_integer_token(std::string_view text,
                                       std::int64_t& out);

/// Parses a token as a Python `float` would, returning nothing when it fails.
[[nodiscard]] bool parse_real_token(std::string_view text, double& out);

}  // namespace timewarp::scheme
