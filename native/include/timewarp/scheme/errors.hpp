// SPDX-License-Identifier: MIT
//
// Scheme-level diagnostics.
//
// The message text is deliberately part of the contract: `execute_scheme()`
// renders failures as "❌ Scheme error: <message>", and that string is what the
// IDE shows the learner.  Messages therefore match the Python reference
// verbatim wherever the reference produced a `SchemeError`.

#pragma once

#include <cstdint>
#include <stdexcept>
#include <string>

namespace timewarp::scheme {

/// Every Scheme-level failure.  Mirrors the reference's `SchemeError`.
class SchemeError : public std::runtime_error {
public:
    explicit SchemeError(const std::string& message)
        : std::runtime_error{message} {}
};

/// Malformed source text (unbalanced parens, unterminated string).
///
/// Split out from `SchemeError` so the reader can be unit tested in isolation
/// without evaluating anything; the reference folded both into one type.
class ReaderError : public SchemeError {
public:
    explicit ReaderError(const std::string& message) : SchemeError{message} {}
};

/// The host's step allowance ran out.
///
/// The reference left this to the host: it called `interpreter.tick(512)` and
/// relied on that call to raise.  Here the host returns `false` and the
/// machine unwinds its own state, so the abort is observable and testable.
class BudgetExhausted : public SchemeError {
public:
    explicit BudgetExhausted(std::uint64_t steps)
        : SchemeError{"step budget exhausted after " + std::to_string(steps) +
                      " steps"} {}
};

}  // namespace timewarp::scheme
