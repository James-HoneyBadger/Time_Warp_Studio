// SPDX-License-Identifier: MIT
//
// ASCII classification/comparison primitives shared by the lexer and the
// evaluator.
//
// The Python reference relied on ``str.isalpha`` / ``str.isalnum`` /
// ``str.upper``, which are Unicode-aware.  The engine only ever produces
// ``[A-Za-z_][A-Za-z0-9_]*`` identifiers, so ASCII-only classification is
// sufficient -- and it is locale-independent, branch-cheap, and usable in
// constant expressions (which the keyword tables rely on).

#pragma once

#include <cstddef>
#include <string_view>

namespace timewarp::expr::detail {

[[nodiscard]] constexpr bool is_digit(char c) noexcept {
    return c >= '0' && c <= '9';
}

[[nodiscard]] constexpr bool is_alpha(char c) noexcept {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

[[nodiscard]] constexpr bool is_alnum(char c) noexcept {
    return is_alpha(c) || is_digit(c);
}

[[nodiscard]] constexpr bool is_space(char c) noexcept {
    return c == ' ' || c == '\t' || c == '\n';
}

/// ASCII upper-casing.  Non-letters (including UTF-8 continuation bytes) are
/// passed through untouched.
[[nodiscard]] constexpr char ascii_upper(char c) noexcept {
    return (c >= 'a' && c <= 'z') ? static_cast<char>(c - 0x20) : c;
}

[[nodiscard]] constexpr bool is_hex_digit(char c) noexcept {
    return is_digit(c) || (ascii_upper(c) >= 'A' && ascii_upper(c) <= 'F');
}

[[nodiscard]] constexpr bool is_octal_digit(char c) noexcept {
    return c >= '0' && c <= '7';
}

[[nodiscard]] constexpr unsigned digit_value(char c) noexcept {
    return is_digit(c) ? static_cast<unsigned>(c - '0')
                       : static_cast<unsigned>(ascii_upper(c) - 'A') + 10U;
}

/// Case-insensitive ASCII equality.
[[nodiscard]] constexpr bool iequals(std::string_view lhs,
                                     std::string_view rhs) noexcept {
    if (lhs.size() != rhs.size()) {
        return false;
    }
    for (std::size_t i = 0; i < lhs.size(); ++i) {
        if (ascii_upper(lhs[i]) != ascii_upper(rhs[i])) {
            return false;
        }
    }
    return true;
}

/// BASIC variable type suffix: ``% & ! # $``.
[[nodiscard]] constexpr bool is_type_suffix(char c) noexcept {
    return c == '%' || c == '&' || c == '!' || c == '#' || c == '$';
}

}  // namespace timewarp::expr::detail
