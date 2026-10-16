// SPDX-License-Identifier: MIT
//
// Expression tokenizer / lexer.
//
// Port of ``ExpressionEvaluator._tokenize`` from
// ``time_warp/utils/expression_evaluator.py``.
//
// The lexer is a single-pass, hand-written scanner.  It is:
//   * allocation-free (tokens borrow from the source; operator spellings are
//     static literals),
//   * re-entrant and stateless (all state is local to ``tokenize``),
//   * O(n) with no backtracking, and
//   * free of locale dependence (ASCII classification only).
//
// Lexical grammar (uppercase = literal, ``*`` = repetition):
//
//   NUMBER     := DIGIT+ | DIGIT* '.' DIGIT*
//               | '&' [Hh] HEXDIGIT+          -- BASIC &HFF
//               | '0' [Xx] HEXDIGIT+          -- 0xFF      (see note below)
//               | '0' [Oo] OCTDIGIT+          -- 0o17
//   IDENT      := (ALPHA | '_') (ALNUM | '_')*
//   VARIABLE   := IDENT SUFFIX?
//   FUNCTION   := IDENT SUFFIX? '('            -- suffix is consumed, ignored
//   KEYWORD    := (AND|OR|NOT|MOD|XOR|DIV|REM) SUFFIX?
//   OP2        := '**' | '//' | '<<' | '>>' | '/\' | '\/'
//   OP1        := '+' | '-' | '*' | '/' | '%' | '^' | '\' | '~' | '|' | '&'
//   COMPARISON := ('<' | '>' | '=' | '!') ('>' | '=')?
//
// NOTE on ``0x`` / ``0o``: in the Python reference the ``0x``/``0o`` branch was
// placed *after* the decimal branch and was therefore unreachable -- ``0x10``
// lexed as the number ``0`` followed by the variable ``X10``.  This port
// restores the documented intent by testing the radix prefixes first.  See
// native/README.md ("Deliberate divergences").

#pragma once

#include <cstddef>
#include <string_view>
#include <vector>

#include "timewarp/expr/token.hpp"

namespace timewarp::expr {

/// Tokenizes arithmetic expressions for the Time Warp engine.
///
/// Stateless and trivially copyable: hold one instance and call ``tokenize``
/// from anywhere.  Instances are safe to share between threads.
class Lexer {
public:
    /// Upper bound on tokens per expression, mirroring ``MAX_TOKENS``.
    static constexpr std::size_t kDefaultMaxTokens = 1000;

    constexpr Lexer() noexcept = default;

    explicit constexpr Lexer(std::size_t max_tokens) noexcept
        : max_tokens_(max_tokens) {}

    [[nodiscard]] constexpr std::size_t max_tokens() const noexcept {
        return max_tokens_;
    }

    /// Splits ``source`` into tokens.
    ///
    /// @throws LexError on an unknown character, a malformed numeric literal,
    ///         or when more than ``max_tokens()`` tokens are produced.
    ///
    /// The returned tokens borrow from ``source``, which must outlive them.
    [[nodiscard]] std::vector<Token> tokenize(std::string_view source) const;

private:
    std::size_t max_tokens_{kDefaultMaxTokens};
};

}  // namespace timewarp::expr
