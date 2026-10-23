// SPDX-License-Identifier: MIT
//
// What the BASIC statement files share.
//
// `basic_statements.cpp` was 2,066 lines: the dispatch, printing, variables, control
// flow, the named blocks and the error handlers in one translation unit. The helpers
// below were an anonymous-style preamble in it, and the split needs them in more than
// one file, so they have declarations here and definitions in
// `basic_statements_helpers.cpp`.
//
// Internal to the BASIC library: nothing outside `src/lang/basic_*.cpp` includes it.

#pragma once

#include "basic_internal.hpp"

#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace timewarp::lang::basic {

bool is_all_digits(std::string_view text);
/// The reference's `is_logo_line`: a statement worth handing to Logo.
bool is_logo_line(std::string_view command);
bool division_by_zero(std::string_view message);
std::string strip_quotes(std::string_view text);
bool has_string_concat(std::string_view expression);
bool mentions_string_function(std::string_view upper_expression);
std::optional<std::string> string_operand(Machine& machine, std::string_view expression);
int try_string_comparison(Machine& machine, std::string_view condition);
std::string substitute_undefined(std::string_view expression, std::string_view name);
std::optional<long> parse_integer(std::string_view text);
std::string join(const std::vector<std::string>& values, std::string_view separator);
bool starts_with(std::string_view text, std::string_view prefix);
std::string tail(std::string_view text, std::size_t from);

/// A statement argument read as a whole number, for the statements that take counts
/// and sizes: `SCREEN 80, 25` either has two numbers or it is an error.
std::optional<long> evaluate_integer(Machine& machine, std::string_view text);
/// `text` split on `separator` with each field trimmed, which is how every argument
/// list in BASIC is read.
std::vector<std::string> split_trimmed(std::string_view text, char separator);
/// A coordinate on the raster. The statements carry doubles; a pixel is a place.
int pixel(double value);

}  // namespace timewarp::lang::basic
