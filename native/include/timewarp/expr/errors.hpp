// SPDX-License-Identifier: MIT
//
// Diagnostic types for the Time Warp expression engine.
//
// The Python reference signalled nearly every failure with a bare
// ``ValueError`` (plus a bespoke ``DivisionByZeroError``).  The C++ port keeps
// one catchable base type but splits it by *phase*, which is what a caller
// actually needs to react to:
//
//   Python reference                     C++ equivalent
//   ----------------------------------   ------------------------------
//   ValueError("Unexpected character")   LexError
//   ValueError("Invalid expression")     ParseError   (program shape)
//   ValueError("Mismatched parentheses") ParseError
//   ValueError("Undefined variable")     EvalError    (runtime value)
//   ValueError("Unknown function")       EvalError
//   DivisionByZeroError                  DivisionByZeroError
//
// Hosts that used ``except ValueError`` should catch ``ExprError``.

#pragma once

#include <stdexcept>
#include <string>

namespace timewarp::expr {

/// Base class of every diagnostic raised by the expression engine.
class ExprError : public std::runtime_error {
public:
    explicit ExprError(const std::string& message) : std::runtime_error(message) {}
};

/// Raised by the tokenizer for malformed input (bad literals, stray bytes).
class LexError : public ExprError {
public:
    using ExprError::ExprError;
};

/// Raised when a token stream is structurally invalid (paren mismatch, RPN
/// shape).  These indicate a bug in the caller's program or in the parser,
/// not a bad runtime value.
class ParseError : public ExprError {
public:
    using ExprError::ExprError;
};

/// Raised while executing the compiled RPN program: undefined variable,
/// unknown function, out-of-range index, wrong operand kinds.
class EvalError : public ExprError {
public:
    using ExprError::ExprError;
};

/// Raised when a divisor (or a modulo/shift base) is zero.
///
/// Deliberately a distinct type: the BASIC and COMAL executors rely on being
/// able to distinguish "this expression divided by zero" from "this expression
/// did not parse", in order to emit the right user-facing message.
class DivisionByZeroError : public EvalError {
public:
    explicit DivisionByZeroError(const std::string& detail = "")
        : EvalError("Division by zero" + detail) {}
};

}  // namespace timewarp::expr
