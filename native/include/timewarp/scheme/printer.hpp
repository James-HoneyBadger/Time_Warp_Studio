// SPDX-License-Identifier: MIT
//
// Iterative renderer for Scheme values.
//
// The reference's `_write_str()` recursed once per nesting level, so a deeply
// nested list could exhaust the native stack *while printing a value it had
// already successfully computed*.  This version walks an explicit work stack.

#pragma once

#include <string>

#include "timewarp/scheme/value.hpp"

namespace timewarp::scheme {

/// Appends `value` rendered the way `write` does (strings quoted).
void write_value(std::string& out, const Value& value);

/// Appends `value` rendered the way `display` does.
///
/// Only a *top-level* string is printed unquoted.  Strings nested inside a list
/// or vector stay quoted, because the reference's `_display_str()` delegates to
/// `_write_str()` for anything that is not a string at the top level -- so
/// `(display (list "a"))` prints `("a")`.  Verified against the reference.
void display_value(std::string& out, const Value& value);

[[nodiscard]] std::string write_to_string(const Value& value);
[[nodiscard]] std::string display_to_string(const Value& value);

/// Formats a real the way the reference's `_default_float()` does: an integral
/// real is rendered as an integer (`2.0` prints as `2`), everything else in
/// shortest round-trip form.
void append_real(std::string& out, double number);

}  // namespace timewarp::scheme
