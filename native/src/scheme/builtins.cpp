// SPDX-License-Identifier: MIT
//
// Builtin procedures: the name table, and the dispatch that implements them.
//
// The reference built its global environment by closing over Python lambdas.
// That is not available here, so each builtin has an id and a `switch` case.
// The up-front cost is verbosity; the payoff is that a missing implementation
// is a compiler warning rather than a `KeyError` at run time, and no call goes
// through a `std::function` indirection.
//
// Builtins that must re-enter the evaluator (map, for-each, filter, foldl,
// apply) push a continuation frame and set the machine to `Eval` instead of
// returning a value.  Those are the only ones that are not a pure function of
// their arguments.

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <string_view>
#include <vector>

#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/printer.hpp"
#include "timewarp/scheme/reader.hpp"

namespace timewarp::scheme {
namespace {

struct BuiltinEntry {
    std::string_view name;
    PrimitiveId id;
};

/// Every globally bound builtin.
///
/// `string=? ` with a trailing space is not a typo: the reference registered
/// that name as well as the correct one, and dropping it would break any
/// program that accidentally relied on it.
constexpr BuiltinEntry kBuiltins[] = {
    // arithmetic
    {"+", PrimitiveId::Add}, {"-", PrimitiveId::Sub}, {"*", PrimitiveId::Mul},
    {"/", PrimitiveId::Div}, {"abs", PrimitiveId::Abs}, {"min", PrimitiveId::Min},
    {"max", PrimitiveId::Max}, {"quotient", PrimitiveId::Quotient},
    {"remainder", PrimitiveId::Remainder}, {"modulo", PrimitiveId::Modulo},
    {"expt", PrimitiveId::Expt}, {"sqrt", PrimitiveId::Sqrt},
    {"floor", PrimitiveId::FloorF}, {"ceiling", PrimitiveId::CeilF},
    {"round", PrimitiveId::RoundF}, {"truncate", PrimitiveId::Truncate},
    {"exact->inexact", PrimitiveId::ExactToInexact},
    {"inexact->exact", PrimitiveId::InexactToExact},
    // comparison
    {"=", PrimitiveId::NumEq}, {"<", PrimitiveId::NumLt},
    {">", PrimitiveId::NumGt}, {"<=", PrimitiveId::NumLe},
    {">=", PrimitiveId::NumGe},
    // predicates
    {"not", PrimitiveId::Not}, {"eq?", PrimitiveId::Eq},
    {"eqv?", PrimitiveId::Eqv}, {"equal?", PrimitiveId::Equal},
    {"null?", PrimitiveId::IsNull}, {"pair?", PrimitiveId::IsPair},
    {"list?", PrimitiveId::IsList}, {"number?", PrimitiveId::IsNumber},
    {"integer?", PrimitiveId::IsInteger}, {"boolean?", PrimitiveId::IsBoolean},
    {"string?", PrimitiveId::IsString}, {"symbol?", PrimitiveId::IsSymbol},
    {"procedure?", PrimitiveId::IsProcedure}, {"vector?", PrimitiveId::IsVector},
    {"zero?", PrimitiveId::IsZero}, {"positive?", PrimitiveId::IsPositive},
    {"negative?", PrimitiveId::IsNegative}, {"odd?", PrimitiveId::IsOdd},
    {"even?", PrimitiveId::IsEven},
    // lists
    {"cons", PrimitiveId::Cons}, {"car", PrimitiveId::Car},
    {"cdr", PrimitiveId::Cdr}, {"caar", PrimitiveId::Caar},
    {"cadr", PrimitiveId::Cadr}, {"cdar", PrimitiveId::Cdar},
    {"cddr", PrimitiveId::Cddr}, {"list", PrimitiveId::List},
    {"length", PrimitiveId::Length}, {"append", PrimitiveId::Append},
    {"reverse", PrimitiveId::Reverse}, {"list-ref", PrimitiveId::ListRef},
    {"list-tail", PrimitiveId::ListTail}, {"member", PrimitiveId::Member},
    {"memq", PrimitiveId::Memq}, {"assoc", PrimitiveId::Assoc},
    // vectors
    {"vector", PrimitiveId::Vector}, {"make-vector", PrimitiveId::MakeVector},
    {"vector-ref", PrimitiveId::VectorRef},
    {"vector-length", PrimitiveId::VectorLength},
    {"vector->list", PrimitiveId::VectorToList},
    {"list->vector", PrimitiveId::ListToVector},
    // output
    // The database is a host service, like the turtle: the language asks and the
    // embedding application answers.
    {"sql", PrimitiveId::Sql},
    {"display", PrimitiveId::Display}, {"write", PrimitiveId::Write},
    {"newline", PrimitiveId::Newline},
    // strings and symbols
    {"number->string", PrimitiveId::NumberToString},
    {"string->number", PrimitiveId::StringToNumber},
    {"symbol->string", PrimitiveId::SymbolToString},
    {"string->symbol", PrimitiveId::StringToSymbol},
    {"string-length", PrimitiveId::StringLength},
    {"string-append", PrimitiveId::StringAppend},
    {"string-upcase", PrimitiveId::StringUpcase},
    {"string-downcase", PrimitiveId::StringDowncase},
    {"substring", PrimitiveId::Substring},
    {"string->list", PrimitiveId::StringToList},
    {"string=? ", PrimitiveId::StringEq}, {"string=?", PrimitiveId::StringEq},
    {"string<?", PrimitiveId::StringLt}, {"string-ci=?", PrimitiveId::StringCiEq},
    // control and higher-order
    {"error", PrimitiveId::Error}, {"apply", PrimitiveId::Apply},
    {"map", PrimitiveId::Map}, {"for-each", PrimitiveId::ForEach},
    {"filter", PrimitiveId::Filter}, {"foldl", PrimitiveId::Foldl},
    // turtle graphics
    {"forward", PrimitiveId::Forward}, {"fd", PrimitiveId::Forward},
    {"backward", PrimitiveId::Backward}, {"back", PrimitiveId::Backward},
    {"bk", PrimitiveId::Backward}, {"right", PrimitiveId::Right},
    {"rt", PrimitiveId::Right}, {"left", PrimitiveId::Left},
    {"lt", PrimitiveId::Left}, {"goto", PrimitiveId::Goto},
    {"setpos", PrimitiveId::Goto}, {"setheading", PrimitiveId::SetHeading},
    {"pensize", PrimitiveId::PenSize}, {"width", PrimitiveId::PenSize},
    {"color", PrimitiveId::Color}, {"penup", PrimitiveId::PenUp},
    {"pu", PrimitiveId::PenUp}, {"pendown", PrimitiveId::PenDown},
    {"pd", PrimitiveId::PenDown}, {"home", PrimitiveId::Home},
    {"clear-canvas", PrimitiveId::ClearCanvas}, {"circle", PrimitiveId::Circle},
    {"show-turtle", PrimitiveId::ShowTurtle},
    {"hide-turtle", PrimitiveId::HideTurtle},
};

// -- argument checking -------------------------------------------------------
//
// The reference relied on Python's own argument binding, so calling a builtin
// with the wrong number of arguments raised an *uncaught* `TypeError` that
// escaped `execute_scheme()`.  (Measured: three of the probe cases do exactly
// that.)  Here a wrong arity is a proper Scheme error.  See the README.

void require_exactly(const std::vector<Value>& args, std::size_t count,
                     std::string_view who) {
    if (args.size() != count) {
        throw SchemeError{std::string{who} + ": expected " +
                          std::to_string(count) + " argument(s), got " +
                          std::to_string(args.size())};
    }
}

void require_between(const std::vector<Value>& args, std::size_t low,
                     std::size_t high, std::string_view who) {
    if (args.size() < low || args.size() > high) {
        throw SchemeError{std::string{who} + ": expected between " +
                          std::to_string(low) + " and " +
                          std::to_string(high) + " arguments, got " +
                          std::to_string(args.size())};
    }
}

void require_at_least(const std::vector<Value>& args, std::size_t low,
                      std::string_view who) {
    if (args.size() < low) {
        throw SchemeError{std::string{who} + ": expected at least " +
                          std::to_string(low) + " argument(s), got " +
                          std::to_string(args.size())};
    }
}

// -- numeric helpers ---------------------------------------------------------

[[nodiscard]] bool to_int64(double value, std::int64_t& out) {
    constexpr double kLimit = 9223372036854775808.0;  // 2^63
    if (!std::isfinite(value) || std::abs(value) >= kLimit) {
        return false;
    }
    out = static_cast<std::int64_t>(value);
    return true;
}

/// Rounds-toward-zero a real to an integer, exactly as `math.trunc`/`int()` do.
/// Values outside the 64-bit range stay real rather than wrapping.
[[nodiscard]] bool add_overflow(std::int64_t a, std::int64_t b,
                               std::int64_t& out) {
    if ((b > 0 && a > std::numeric_limits<std::int64_t>::max() - b) ||
        (b < 0 && a < std::numeric_limits<std::int64_t>::min() - b)) {
        return true;
    }
    out = a + b;
    return false;
}

[[nodiscard]] bool sub_overflow(std::int64_t a, std::int64_t b,
                               std::int64_t& out) {
    if ((b < 0 && a > std::numeric_limits<std::int64_t>::max() + b) ||
        (b > 0 && a < std::numeric_limits<std::int64_t>::min() + b)) {
        return true;
    }
    out = a - b;
    return false;
}

[[nodiscard]] bool mul_overflow(std::int64_t a, std::int64_t b,
                               std::int64_t& out) {
    // Deliberately does *not* compute `a * b` and then divide back: signed
    // overflow is undefined, and GCC folds the `product / b != a` test away
    // under that assumption, so the naive version silently returns wrapped
    // values.  Magnitudes are compared before multiplying instead.
    if (a == 0 || b == 0) {
        out = 0;
        return false;
    }
    const bool negative = (a < 0) != (b < 0);
    const std::uint64_t magnitude_a =
        a < 0 ? ~static_cast<std::uint64_t>(a) + 1u
              : static_cast<std::uint64_t>(a);
    const std::uint64_t magnitude_b =
        b < 0 ? ~static_cast<std::uint64_t>(b) + 1u
              : static_cast<std::uint64_t>(b);

    constexpr std::uint64_t kNegativeLimit = 9223372036854775808ULL;  // 2^63
    constexpr std::uint64_t kPositiveLimit = 9223372036854775807ULL;
    const std::uint64_t limit = negative ? kNegativeLimit : kPositiveLimit;

    if (magnitude_a > limit / magnitude_b) {
        return true;
    }
    const std::uint64_t product = magnitude_a * magnitude_b;
    if (negative) {
        out = product == kNegativeLimit
                  ? std::numeric_limits<std::int64_t>::min()
                  : -static_cast<std::int64_t>(product);
    } else {
        out = static_cast<std::int64_t>(product);
    }
    return false;
}

/// Python's `str()` for the value kinds `string-append` realistically sees.
///
/// Booleans render as `True`/`False` here, *not* as `#t`/`#f`, because the
/// reference composed strings with `str()`.  A compound value falls back to the
/// `write` rendering, which the reference would print in Python list syntax; see
/// the README.
[[nodiscard]] std::string python_str(const Value& value) {
    switch (value.type) {
        case ValueType::Boolean:
            return value.boolean ? "True" : "False";
        case ValueType::Integer:
            return std::to_string(value.integer);
        case ValueType::Real: {
            std::string text;
            append_real(text, value.real);
            return text;
        }
        case ValueType::String:
            return value.string->text;
        case ValueType::Symbol:
            return value.symbol->name;
        case ValueType::Nil:
            return "()";
        case ValueType::Unspecified:
            return "#<unspecified>";
        case ValueType::Pair:
        case ValueType::Vector:
        case ValueType::Procedure:
            return write_to_string(value);
    }
    return {};
}

/// Python's slice-index normalisation: negatives count from the end, and
/// anything past the end clamps.
[[nodiscard]] std::size_t slice_start(std::int64_t index, std::size_t size) {
    if (index < 0) {
        const std::int64_t shifted =
            index + static_cast<std::int64_t>(size);
        return shifted < 0 ? 0 : static_cast<std::size_t>(shifted);
    }
    return static_cast<std::size_t>(index) > size
               ? size
               : static_cast<std::size_t>(index);
}

/// Elements of a proper list, matching `_to_py_list`.
[[nodiscard]] std::vector<Value> list_elements(const Value& list) {
    return to_vector(list);
}

/// `_car` with the reference's exact message.
[[nodiscard]] Value car_of(const Value& value) {
    if (value.type != ValueType::Pair) {
        throw SchemeError{"car: not a pair"};
    }
    return value.pair->car;
}

[[nodiscard]] Value cdr_of(const Value& value) {
    if (value.type != ValueType::Pair) {
        throw SchemeError{"cdr: not a pair"};
    }
    return value.pair->cdr;
}

/// `eq?`: Python's `a is b or a == b or (both symbols with equal text)`.
///
/// Deliberately *not* deep for pairs and vectors, because `Pair` has no
/// `__eq__` in the reference -- `(eq? '(1 2) '(1 2))` is `#f` there.
[[nodiscard]] bool same_object(const Value& left, const Value& right) {
    if (left.type == right.type) {
        switch (left.type) {
            case ValueType::Pair:
                return left.pair == right.pair;
            case ValueType::Vector:
                return left.vector == right.vector;
            case ValueType::Procedure:
                return left.procedure == right.procedure;
            default:
                break;
        }
    }
    return equal_values(left, right);
}

}  // namespace

void Machine::install_builtins() {
    for (const BuiltinEntry& entry : kBuiltins) {
        Procedure* procedure =
            arena_.make_procedure({}, nullptr, unspecified_value(), nullptr,
                                  entry.id, std::string{entry.name});
        define_in(global_, arena_.intern(entry.name), procedure_value(procedure));
    }
}

void Machine::apply_primitive(PrimitiveId id, const std::vector<Value>& args) {
    switch (id) {
        // ---------------------------------------------------------------- +
        case PrimitiveId::Add:
        case PrimitiveId::Mul:
        case PrimitiveId::Sub: {
            const bool is_add = id == PrimitiveId::Add;
            const bool is_mul = id == PrimitiveId::Mul;

            if (is_add || is_mul) {
                if (args.empty()) {
                    deliver_value(integer_value(is_add ? 0 : 1));
                    return;
                }
            } else if (args.empty()) {
                throw SchemeError{"sub: expected at least 1 argument, got 0"};
            }

            Value accumulator =
                require_number(args.front(), is_add ? "add"
                                             : is_mul ? "mul"
                                                      : "sub");
            if (id == PrimitiveId::Sub && args.size() == 1) {
                if (accumulator.type == ValueType::Integer) {
                    if (accumulator.integer ==
                        std::numeric_limits<std::int64_t>::min()) {
                        deliver_value(real_value(-static_cast<double>(
                            accumulator.integer)));
                        return;
                    }
                    deliver_value(integer_value(-accumulator.integer));
                    return;
                }
                deliver_value(real_value(-accumulator.real));
                return;
            }

            for (std::size_t index = 1; index < args.size(); ++index) {
                const Value next = require_number(
                    args[index], is_add ? "add" : is_mul ? "mul" : "sub");

                // Integer arithmetic stays integer, matching Python, with an
                // overflow check that falls back to double instead of wrapping.
                if (accumulator.type == ValueType::Integer &&
                    next.type == ValueType::Integer) {
                    std::int64_t combined = 0;
                    const bool overflow =
                        is_add ? add_overflow(accumulator.integer, next.integer,
                                              combined)
                        : is_mul
                            ? mul_overflow(accumulator.integer, next.integer,
                                           combined)
                            : sub_overflow(accumulator.integer, next.integer,
                                           combined);
                    if (!overflow) {
                        accumulator = integer_value(combined);
                        continue;
                    }
                }
                const double left = as_real(accumulator);
                const double right = as_real(next);
                accumulator =
                    real_value(is_add ? left + right
                                      : is_mul ? left * right : left - right);
            }
            deliver_value(accumulator);
            return;
        }

        case PrimitiveId::Div: {
            require_at_least(args, 1, "/");
            double accumulator = 0.0;
            if (args.size() == 1) {
                accumulator = 1.0;
            } else {
                accumulator = as_real(require_number(args.front(), "/"));
            }
            const std::size_t from = args.size() == 1 ? 0 : 1;
            for (std::size_t index = from; index < args.size(); ++index) {
                const double divisor = as_real(require_number(args[index], "/"));
                if (divisor == 0.0) {
                    throw SchemeError{"division by zero"};
                }
                accumulator /= divisor;
            }
            deliver_value(real_value(accumulator));
            return;
        }

        case PrimitiveId::Abs: {
            require_exactly(args, 1, "abs");
            const Value value = require_number(args[0], "abs");
            if (value.type == ValueType::Integer) {
                if (value.integer == std::numeric_limits<std::int64_t>::min()) {
                    deliver_value(real_value(-static_cast<double>(value.integer)));
                    return;
                }
                deliver_value(integer_value(std::abs(value.integer)));
                return;
            }
            deliver_value(real_value(std::fabs(value.real)));
            return;
        }

        case PrimitiveId::Min:
        case PrimitiveId::Max: {
            const bool want_max = id == PrimitiveId::Max;
            const std::string_view who = want_max ? "max" : "min";
            require_at_least(args, 1, who);
            Value best = require_number(args.front(), who);
            for (std::size_t index = 1; index < args.size(); ++index) {
                const Value candidate = require_number(args[index], who);
                // Strict comparison keeps the *first* extreme, as `min`/`max` do.
                const bool better = want_max ? as_real(candidate) > as_real(best)
                                             : as_real(candidate) < as_real(best);
                if (better) {
                    best = candidate;
                }
            }
            deliver_value(best);
            return;
        }

        case PrimitiveId::Quotient: {
            require_exactly(args, 2, "quotient");
            const double left = as_real(require_number(args[0], "quotient"));
            const double right = as_real(require_number(args[1], "quotient"));
            if (right == 0.0) {
                throw SchemeError{"division by zero"};
            }
            std::int64_t truncated = 0;
            const double result = std::trunc(left / right);
            if (to_int64(result, truncated)) {
                deliver_value(integer_value(truncated));
                return;
            }
            deliver_value(real_value(result));
            return;
        }

        case PrimitiveId::Remainder: {
            require_exactly(args, 2, "remainder");
            const double left = as_real(require_number(args[0], "remainder"));
            const double right = as_real(require_number(args[1], "remainder"));
            // `math.fmod`, which is truncated rather than floored, and always
            // yields a real.
            deliver_value(real_value(std::fmod(left, right)));
            return;
        }

        case PrimitiveId::Modulo: {
            require_exactly(args, 2, "modulo");
            const Value left = require_number(args[0], "modulo");
            const Value right = require_number(args[1], "modulo");
            const double divisor = as_real(right);
            if (divisor == 0.0) {
                throw SchemeError{"division by zero"};
            }
            if (left.type == ValueType::Integer &&
                right.type == ValueType::Integer) {
                std::int64_t remainder = left.integer % right.integer;
                if (remainder != 0 &&
                    ((remainder < 0) != (right.integer < 0))) {
                    remainder += right.integer;
                }
                deliver_value(integer_value(remainder));
                return;
            }
            const double left_real = as_real(left);
            double result = std::fmod(left_real, divisor);
            if (result != 0.0 && ((result < 0.0) != (divisor < 0.0))) {
                result += divisor;
            }
            deliver_value(real_value(result));
            return;
        }

        case PrimitiveId::Expt: {
            require_exactly(args, 2, "expt");
            const Value base = require_number(args[0], "expt");
            const Value exponent = require_number(args[1], "expt");

            if (base.type == ValueType::Integer &&
                exponent.type == ValueType::Integer &&
                exponent.integer >= 0) {
                std::int64_t accumulator = 1;
                std::int64_t factor = base.integer;
                std::int64_t remaining = exponent.integer;
                bool overflow = false;
                while (remaining > 0) {
                    if (remaining & 1) {
                        std::int64_t combined = 0;
                        if (mul_overflow(accumulator, factor, combined)) {
                            overflow = true;
                            break;
                        }
                        accumulator = combined;
                    }
                    remaining >>= 1;
                    if (remaining > 0) {
                        std::int64_t squared = 0;
                        if (mul_overflow(factor, factor, squared)) {
                            overflow = true;
                            break;
                        }
                        factor = squared;
                    }
                }
                if (!overflow) {
                    deliver_value(integer_value(accumulator));
                    return;
                }
            }

            const double left = as_real(base);
            const double right = as_real(exponent);
            if (left == 0.0 && right < 0.0) {
                throw SchemeError{"division by zero"};
            }
            deliver_value(real_value(std::pow(left, right)));
            return;
        }

        case PrimitiveId::Sqrt: {
            require_exactly(args, 1, "sqrt");
            const double value = as_real(require_number(args[0], "sqrt"));
            if (value < 0.0) {
                // The reference let `math.sqrt`'s ValueError escape
                // `execute_scheme()` entirely; this is a proper Scheme error.
                throw SchemeError{"sqrt: math domain error"};
            }
            deliver_value(real_value(std::sqrt(value)));
            return;
        }

        case PrimitiveId::FloorF:
        case PrimitiveId::CeilF:
        case PrimitiveId::RoundF:
        case PrimitiveId::Truncate: {
            const std::string_view who = id == PrimitiveId::FloorF    ? "floor"
                                         : id == PrimitiveId::CeilF  ? "ceiling"
                                         : id == PrimitiveId::RoundF ? "round"
                                                                     : "truncate";
            require_exactly(args, 1, who);
            const double value = as_real(require_number(args[0], who));
            // `round()` is Python's, i.e. half-to-even; the default floating
            // point rounding mode already is `nearbyint`'s.
            const double rounded = id == PrimitiveId::FloorF     ? std::floor(value)
                                   : id == PrimitiveId::CeilF    ? std::ceil(value)
                                   : id == PrimitiveId::RoundF   ? std::nearbyint(value)
                                                                 : std::trunc(value);
            std::int64_t integer = 0;
            if (to_int64(rounded, integer)) {
                deliver_value(integer_value(integer));
                return;
            }
            deliver_value(real_value(rounded));
            return;
        }

        case PrimitiveId::ExactToInexact: {
            require_exactly(args, 1, "exact->inexact");
            deliver_value(real_value(
                as_real(require_number(args[0], "exact->inexact"))));
            return;
        }

        case PrimitiveId::InexactToExact: {
            require_exactly(args, 1, "inexact->exact");
            const double value =
                as_real(require_number(args[0], "inexact->exact"));
            std::int64_t integer = 0;
            if (!to_int64(std::trunc(value), integer)) {
                throw SchemeError{"inexact->exact: value out of range"};
            }
            deliver_value(integer_value(integer));
            return;
        }

        // ------------------------------------------------------- comparison
        case PrimitiveId::NumEq:
        case PrimitiveId::NumLt:
        case PrimitiveId::NumGt:
        case PrimitiveId::NumLe:
        case PrimitiveId::NumGe: {
            const std::string_view who = id == PrimitiveId::NumEq   ? "="
                                         : id == PrimitiveId::NumLt ? "<"
                                         : id == PrimitiveId::NumGt ? ">"
                                         : id == PrimitiveId::NumLe ? "<="
                                                                    : ">=";
            if (args.size() < 2) {
                deliver_value(boolean_value(true));
                return;
            }
            bool satisfied = true;
            for (std::size_t index = 0; index + 1 < args.size(); ++index) {
                const double left = as_real(require_number(args[index], who));
                const double right =
                    as_real(require_number(args[index + 1], who));
                const bool step = id == PrimitiveId::NumEq   ? left == right
                                  : id == PrimitiveId::NumLt ? left < right
                                  : id == PrimitiveId::NumGt ? left > right
                                  : id == PrimitiveId::NumLe ? left <= right
                                                             : left >= right;
                if (!step) {
                    satisfied = false;
                    break;
                }
            }
            deliver_value(boolean_value(satisfied));
            return;
        }

        // -------------------------------------------------------- predicates
        case PrimitiveId::Not: {
            require_exactly(args, 1, "not");
            deliver_value(boolean_value(args[0].type == ValueType::Boolean &&
                                        !args[0].boolean));
            return;
        }
        case PrimitiveId::Eq: {
            require_exactly(args, 2, "eq?");
            deliver_value(boolean_value(same_object(args[0], args[1])));
            return;
        }
        case PrimitiveId::Eqv: {
            require_exactly(args, 2, "eqv?");
            deliver_value(boolean_value(same_object(args[0], args[1])));
            return;
        }
        case PrimitiveId::Equal: {
            require_exactly(args, 2, "equal?");
            deliver_value(boolean_value(equal_values(args[0], args[1])));
            return;
        }
        case PrimitiveId::IsNull:
            require_exactly(args, 1, "null?");
            deliver_value(boolean_value(is_nil(args[0])));
            return;
        case PrimitiveId::IsPair:
            require_exactly(args, 1, "pair?");
            deliver_value(boolean_value(is_pair(args[0])));
            return;
        case PrimitiveId::IsList:
            require_exactly(args, 1, "list?");
            deliver_value(boolean_value(is_proper_list(args[0])));
            return;
        case PrimitiveId::IsNumber:
            require_exactly(args, 1, "number?");
            deliver_value(boolean_value(is_number(args[0])));
            return;
        case PrimitiveId::IsInteger:
            require_exactly(args, 1, "integer?");
            // `integer?` is true only for an exact integer: `(integer? 3.0)`
            // is `#f` in the reference.
            deliver_value(boolean_value(is_integer(args[0])));
            return;
        case PrimitiveId::IsBoolean:
            require_exactly(args, 1, "boolean?");
            deliver_value(
                boolean_value(args[0].type == ValueType::Boolean));
            return;
        case PrimitiveId::IsString:
            require_exactly(args, 1, "string?");
            deliver_value(boolean_value(is_string(args[0])));
            return;
        case PrimitiveId::IsSymbol:
            require_exactly(args, 1, "symbol?");
            deliver_value(boolean_value(is_symbol(args[0])));
            return;
        case PrimitiveId::IsProcedure:
            require_exactly(args, 1, "procedure?");
            deliver_value(
                boolean_value(args[0].type == ValueType::Procedure));
            return;
        case PrimitiveId::IsVector:
            require_exactly(args, 1, "vector?");
            deliver_value(boolean_value(args[0].type == ValueType::Vector));
            return;
        case PrimitiveId::IsZero:
            require_exactly(args, 1, "zero?");
            deliver_value(boolean_value(
                as_real(require_number(args[0], "zero?")) == 0.0));
            return;
        case PrimitiveId::IsPositive:
            require_exactly(args, 1, "positive?");
            deliver_value(boolean_value(
                as_real(require_number(args[0], "positive?")) > 0.0));
            return;
        case PrimitiveId::IsNegative:
            require_exactly(args, 1, "negative?");
            deliver_value(boolean_value(
                as_real(require_number(args[0], "negative?")) < 0.0));
            return;
        case PrimitiveId::IsOdd:
        case PrimitiveId::IsEven: {
            const bool want_even = id == PrimitiveId::IsEven;
            const std::string_view who = want_even ? "even?" : "odd?";
            require_exactly(args, 1, who);
            // The reference truncates to an integer first, so `(odd? 2.5)` is
            // false because `int(2.5)` is 2.
            std::int64_t integer = 0;
            if (!to_int64(std::trunc(as_real(require_number(args[0], who))),
                          integer)) {
                throw SchemeError{std::string{who} + ": value out of range"};
            }
            const bool even = (integer % 2) == 0;
            deliver_value(boolean_value(want_even == even));
            return;
        }

        // ------------------------------------------------------------- lists
        case PrimitiveId::Cons: {
            require_exactly(args, 2, "cons");
            deliver_value(pair_value(arena_.cons(args[0], args[1])));
            return;
        }
        case PrimitiveId::Car:
            require_exactly(args, 1, "car");
            deliver_value(car_of(args[0]));
            return;
        case PrimitiveId::Cdr:
            require_exactly(args, 1, "cdr");
            deliver_value(cdr_of(args[0]));
            return;
        case PrimitiveId::Caar:
            require_exactly(args, 1, "caar");
            deliver_value(car_of(car_of(args[0])));
            return;
        case PrimitiveId::Cadr:
            require_exactly(args, 1, "cadr");
            deliver_value(car_of(cdr_of(args[0])));
            return;
        case PrimitiveId::Cdar:
            require_exactly(args, 1, "cdar");
            deliver_value(cdr_of(car_of(args[0])));
            return;
        case PrimitiveId::Cddr:
            require_exactly(args, 1, "cddr");
            deliver_value(cdr_of(cdr_of(args[0])));
            return;
        case PrimitiveId::List:
            deliver_value(list_from(arena_, args));
            return;
        case PrimitiveId::Length: {
            require_exactly(args, 1, "length");
            const std::vector<Value> items = list_elements(args[0]);
            deliver_value(
                integer_value(static_cast<std::int64_t>(items.size())));
            return;
        }
        case PrimitiveId::Append: {
            if (args.empty()) {
                deliver_value(nil_value());
                return;
            }
            std::vector<Value> combined;
            for (std::size_t index = 0; index + 1 < args.size(); ++index) {
                const std::vector<Value> part = list_elements(args[index]);
                combined.insert(combined.end(), part.begin(), part.end());
            }
            // The final argument is used as the tail, so it need not be a list.
            deliver_value(list_from(arena_, combined, args.back()));
            return;
        }
        case PrimitiveId::Reverse: {
            require_exactly(args, 1, "reverse");
            std::vector<Value> items = list_elements(args[0]);
            std::reverse(items.begin(), items.end());
            deliver_value(list_from(arena_, items));
            return;
        }
        case PrimitiveId::ListRef: {
            require_exactly(args, 2, "list-ref");
            const std::vector<Value> items = list_elements(args[0]);
            const double position =
                as_real(require_number(args[1], "list-ref"));
            std::int64_t index = 0;
            if (!to_int64(std::trunc(position), index)) {
                throw SchemeError{"list-ref: index out of range"};
            }
            if (index < 0) {
                index += static_cast<std::int64_t>(items.size());
            }
            // The reference let Python's IndexError escape `execute_scheme()`.
            if (index < 0 ||
                index >= static_cast<std::int64_t>(items.size())) {
                throw SchemeError{"list-ref: index out of range"};
            }
            deliver_value(items[static_cast<std::size_t>(index)]);
            return;
        }
        case PrimitiveId::ListTail: {
            require_exactly(args, 2, "list-tail");
            const std::vector<Value> items = list_elements(args[0]);
            const double position =
                as_real(require_number(args[1], "list-tail"));
            std::int64_t index = 0;
            if (!to_int64(std::trunc(position), index)) {
                index = static_cast<std::int64_t>(items.size());
            }
            const std::size_t from = slice_start(index, items.size());
            std::vector<Value> tail(items.begin() +
                                        static_cast<std::ptrdiff_t>(from),
                                    items.end());
            deliver_value(list_from(arena_, tail));
            return;
        }
        case PrimitiveId::Member:
        case PrimitiveId::Memq: {
            const std::string_view who =
                id == PrimitiveId::Member ? "member" : "memq";
            require_exactly(args, 2, who);
            const Value needle = args[0];
            for (Value cursor = args[1]; cursor.type == ValueType::Pair;
                 cursor = cursor.pair->cdr) {
                if (equal_values(cursor.pair->car, needle)) {
                    deliver_value(cursor);
                    return;
                }
            }
            deliver_value(boolean_value(false));
            return;
        }
        case PrimitiveId::Assoc: {
            require_exactly(args, 2, "assoc");
            const Value needle = args[0];
            for (Value cursor = args[1]; cursor.type == ValueType::Pair;
                 cursor = cursor.pair->cdr) {
                const Value entry = cursor.pair->car;
                if (entry.type == ValueType::Pair &&
                    equal_values(entry.pair->car, needle)) {
                    deliver_value(entry);
                    return;
                }
            }
            deliver_value(boolean_value(false));
            return;
        }

        // ----------------------------------------------------------- vectors
        case PrimitiveId::Vector:
            deliver_value(vector_value(arena_.make_vector(args)));
            return;
        case PrimitiveId::MakeVector: {
            require_between(args, 1, 2, "make-vector");
            const double count = as_real(require_number(args[0], "make-vector"));
            std::int64_t length = 0;
            if (!to_int64(std::trunc(count), length)) {
                throw SchemeError{"make-vector: length out of range"};
            }
            const Value fill =
                args.size() > 1 ? args[1] : integer_value(0);
            std::vector<Value> items;
            if (length > 0) {
                items.assign(static_cast<std::size_t>(length), fill);
            }
            deliver_value(vector_value(arena_.make_vector(std::move(items))));
            return;
        }
        case PrimitiveId::VectorRef: {
            require_exactly(args, 2, "vector-ref");
            if (args[0].type != ValueType::Vector) {
                throw SchemeError{"vector-ref: expected a vector, got " +
                                  std::string{type_name(args[0])}};
            }
            const std::vector<Value>& items = args[0].vector->items;
            const double position =
                as_real(require_number(args[1], "vector-ref"));
            std::int64_t index = 0;
            if (!to_int64(std::trunc(position), index)) {
                throw SchemeError{"vector-ref: index out of range"};
            }
            if (index < 0) {
                index += static_cast<std::int64_t>(items.size());
            }
            if (index < 0 ||
                index >= static_cast<std::int64_t>(items.size())) {
                throw SchemeError{"vector-ref: index out of range"};
            }
            deliver_value(items[static_cast<std::size_t>(index)]);
            return;
        }
        case PrimitiveId::VectorLength: {
            require_exactly(args, 1, "vector-length");
            if (args[0].type != ValueType::Vector) {
                throw SchemeError{"vector-length: expected a vector, got " +
                                  std::string{type_name(args[0])}};
            }
            deliver_value(integer_value(
                static_cast<std::int64_t>(args[0].vector->items.size())));
            return;
        }
        case PrimitiveId::VectorToList: {
            require_exactly(args, 1, "vector->list");
            if (args[0].type != ValueType::Vector) {
                throw SchemeError{"vector->list: expected a vector, got " +
                                  std::string{type_name(args[0])}};
            }
            deliver_value(list_from(arena_, args[0].vector->items));
            return;
        }
        case PrimitiveId::ListToVector: {
            require_exactly(args, 1, "list->vector");
            deliver_value(vector_value(
                arena_.make_vector(list_elements(args[0]))));
            return;
        }

        // ------------------------------------------------------------ output
        case PrimitiveId::Sql: {
            require_exactly(args, 1, "sql");
            if (args[0].type != ValueType::String) {
                throw SchemeError{"sql: expected a string statement"};
            }
            append_output(host_.sql(args[0].string->text));
            deliver_value(unspecified_value());
            return;
        }
        case PrimitiveId::Display: {
            require_exactly(args, 1, "display");
            append_output(display_to_string(args[0]));
            deliver_value(unspecified_value());
            return;
        }
        case PrimitiveId::Write: {
            require_exactly(args, 1, "write");
            append_output(write_to_string(args[0]));
            deliver_value(unspecified_value());
            return;
        }
        case PrimitiveId::Newline:
            require_exactly(args, 0, "newline");
            append_output("\n");
            deliver_value(unspecified_value());
            return;

        // -------------------------------------------------- strings, symbols
        case PrimitiveId::NumberToString: {
            require_exactly(args, 1, "number->string");
            deliver_value(string_value(
                arena_.make_string(write_to_string(args[0]))));
            return;
        }
        case PrimitiveId::StringToNumber: {
            require_between(args, 1, 2, "string->number");
            // The reference accepted a radix and then ignored it.
            const std::string text = python_str(args[0]);
            std::int64_t integer = 0;
            if (parse_integer_token(text, integer)) {
                deliver_value(integer_value(integer));
                return;
            }
            double real = 0.0;
            if (parse_real_token(text, real)) {
                deliver_value(real_value(real));
                return;
            }
            deliver_value(boolean_value(false));
            return;
        }
        case PrimitiveId::SymbolToString: {
            require_exactly(args, 1, "symbol->string");
            deliver_value(string_value(arena_.make_string(python_str(args[0]))));
            return;
        }
        case PrimitiveId::StringToSymbol: {
            require_exactly(args, 1, "string->symbol");
            deliver_value(symbol_value(arena_.intern(python_str(args[0]))));
            return;
        }
        case PrimitiveId::StringLength: {
            require_exactly(args, 1, "string-length");
            // Works on symbols too, because `Symbol` subclasses `str`.
            const std::string text = python_str(args[0]);
            deliver_value(
                integer_value(static_cast<std::int64_t>(text.size())));
            return;
        }
        case PrimitiveId::StringAppend: {
            std::string combined;
            for (const Value& value : args) {
                combined += python_str(value);
            }
            deliver_value(string_value(arena_.make_string(combined)));
            return;
        }
        case PrimitiveId::StringUpcase:
        case PrimitiveId::StringDowncase: {
            const bool up = id == PrimitiveId::StringUpcase;
            const std::string_view who = up ? "string-upcase"
                                            : "string-downcase";
            require_exactly(args, 1, who);
            std::string text = python_str(args[0]);
            // ASCII-only case mapping; Python's is Unicode-aware.  See README.
            std::transform(text.begin(), text.end(), text.begin(),
                           [up](unsigned char ch) {
                               return static_cast<char>(
                                   up ? std::toupper(ch) : std::tolower(ch));
                           });
            deliver_value(string_value(arena_.make_string(text)));
            return;
        }
        case PrimitiveId::Substring: {
            require_between(args, 2, 3, "substring");
            const std::string text = python_str(args[0]);
            const std::int64_t start =
                static_cast<std::int64_t>(as_real(require_number(args[1],
                                                                 "substring")));
            const std::size_t from = slice_start(start, text.size());

            std::size_t to = text.size();
            if (args.size() > 2) {
                const std::int64_t end = static_cast<std::int64_t>(
                    as_real(require_number(args[2], "substring")));
                // Python clamps a slice end both ways.
                if (end >= 0) {
                    to = static_cast<std::size_t>(end) > text.size()
                             ? text.size()
                             : static_cast<std::size_t>(end);
                } else {
                    const std::int64_t shifted =
                        end + static_cast<std::int64_t>(text.size());
                    to = shifted < 0 ? 0
                                     : static_cast<std::size_t>(shifted);
                }
            }
            if (to < from) {
                to = from;
            }
            deliver_value(
                string_value(arena_.make_string(text.substr(from, to - from))));
            return;
        }
        case PrimitiveId::StringToList: {
            require_exactly(args, 1, "string->list");
            const std::string text = python_str(args[0]);
            std::vector<Value> characters;
            characters.reserve(text.size());
            for (const char ch : text) {
                characters.push_back(
                    string_value(arena_.make_string(std::string(1, ch))));
            }
            deliver_value(list_from(arena_, characters));
            return;
        }
        case PrimitiveId::StringEq:
        case PrimitiveId::StringLt:
        case PrimitiveId::StringCiEq: {
            // Variadic with no arity check, matching the reference: the chain
            // is vacuously true for fewer than two arguments.
            bool satisfied = true;
            for (std::size_t index = 0; index + 1 < args.size(); ++index) {
                std::string left = python_str(args[index]);
                std::string right = python_str(args[index + 1]);
                if (id == PrimitiveId::StringCiEq) {
                    const auto lower = [](std::string text) {
                        std::transform(
                            text.begin(), text.end(), text.begin(),
                            [](unsigned char ch) {
                                return static_cast<char>(std::tolower(ch));
                            });
                        return text;
                    };
                    left = lower(left);
                    right = lower(right);
                }
                const bool step = id == PrimitiveId::StringLt ? left < right
                                                              : left == right;
                if (!step) {
                    satisfied = false;
                    break;
                }
            }
            deliver_value(boolean_value(satisfied));
            return;
        }

        // ------------------------------------------------- higher-order bits
        case PrimitiveId::Error: {
            std::string message;
            for (std::size_t index = 0; index < args.size(); ++index) {
                if (index > 0) {
                    message += ' ';
                }
                message += display_to_string(args[index]);
            }
            throw SchemeError{message};
        }

        case PrimitiveId::Apply: {
            require_at_least(args, 1, "apply");
            std::vector<Value> call_args;
            if (args.size() > 1) {
                const std::vector<Value> tail = list_elements(args.back());
                call_args.insert(call_args.end(), args.begin() + 1,
                                 args.end() - 1);
                call_args.insert(call_args.end(), tail.begin(), tail.end());
            }
            start_apply(args[0], std::move(call_args));
            return;
        }

        case PrimitiveId::Map:
        case PrimitiveId::ForEach: {
            const bool collect = id == PrimitiveId::Map;
            const std::string_view who = collect ? "map" : "for-each";
            require_at_least(args, 2, who);

            MapFrame frame;
            frame.proc = args[0];
            frame.collect = collect;
            frame.count = std::numeric_limits<std::size_t>::max();
            for (std::size_t index = 1; index < args.size(); ++index) {
                frame.columns.push_back(list_elements(args[index]));
                frame.count = std::min(frame.count, frame.columns.back().size());
            }

            if (frame.count == 0) {
                deliver_value(collect ? nil_value() : unspecified_value());
                return;
            }

            std::vector<Value> row;
            row.reserve(frame.columns.size());
            for (const std::vector<Value>& column : frame.columns) {
                row.push_back(column.front());
            }
            // `index` names the row whose result is outstanding, so row 0 is
            // the one just dispatched.
            frame.index = 0;
            frames_.push_back(std::move(frame));
            note_depth();
            start_apply(args[0], std::move(row));
            return;
        }

        case PrimitiveId::Filter: {
            require_exactly(args, 2, "filter");
            FilterFrame frame;
            frame.proc = args[0];
            frame.source = list_elements(args[1]);
            if (frame.source.empty()) {
                deliver_value(nil_value());
                return;
            }
            std::vector<Value> first;
            first.push_back(frame.source.front());
            // Element 0 is the one just dispatched.
            frame.index = 0;
            frames_.push_back(std::move(frame));
            note_depth();
            start_apply(args[0], std::move(first));
            return;
        }

        case PrimitiveId::Foldl: {
            require_exactly(args, 3, "foldl");
            FoldFrame frame;
            frame.proc = args[0];
            frame.acc = args[1];
            frame.source = list_elements(args[2]);
            if (frame.source.empty()) {
                deliver_value(frame.acc);
                return;
            }
            std::vector<Value> first;
            first.push_back(frame.acc);
            first.push_back(frame.source.front());
            frame.index = 1;
            frames_.push_back(std::move(frame));
            note_depth();
            start_apply(args[0], std::move(first));
            return;
        }

        // ---------------------------------------------------- turtle graphics
        case PrimitiveId::Forward:
        case PrimitiveId::Backward:
        case PrimitiveId::Right:
        case PrimitiveId::Left:
        case PrimitiveId::SetHeading:
        case PrimitiveId::PenSize:
        case PrimitiveId::Circle: {
            const std::string_view who = id == PrimitiveId::Forward    ? "forward"
                                         : id == PrimitiveId::Backward ? "backward"
                                         : id == PrimitiveId::Right    ? "right"
                                         : id == PrimitiveId::Left     ? "left"
                                         : id == PrimitiveId::SetHeading
                                             ? "setheading"
                                         : id == PrimitiveId::PenSize ? "pensize"
                                                                      : "circle";
            require_exactly(args, 1, who);
            const double amount = as_real(require_number(args[0], who));
            switch (id) {
                case PrimitiveId::Forward: host_.forward(amount); break;
                case PrimitiveId::Backward: host_.back(amount); break;
                case PrimitiveId::Right: host_.right(amount); break;
                case PrimitiveId::Left: host_.left(amount); break;
                case PrimitiveId::SetHeading: host_.set_heading(amount); break;
                case PrimitiveId::PenSize: host_.set_pen_width(amount); break;
                default: host_.circle(amount); break;
            }
            deliver_value(unspecified_value());
            return;
        }

        case PrimitiveId::Goto: {
            require_exactly(args, 2, "goto");
            host_.goto_xy(as_real(require_number(args[0], "goto")),
                          as_real(require_number(args[1], "goto")));
            deliver_value(unspecified_value());
            return;
        }

        case PrimitiveId::Color: {
            require_exactly(args, 3, "color");
            const auto channel = [this](const Value& value) {
                return static_cast<int>(
                    std::trunc(as_real(require_number(value, "color"))));
            };
            host_.set_color(channel(args[0]), channel(args[1]), channel(args[2]));
            deliver_value(unspecified_value());
            return;
        }

        case PrimitiveId::PenUp:
        case PrimitiveId::PenDown:
        case PrimitiveId::Home:
        case PrimitiveId::ClearCanvas:
        case PrimitiveId::ShowTurtle:
        case PrimitiveId::HideTurtle: {
            require_exactly(args, 0, "turtle command");
            switch (id) {
                case PrimitiveId::PenUp: host_.pen_up(); break;
                case PrimitiveId::PenDown: host_.pen_down(); break;
                case PrimitiveId::Home: host_.home(); break;
                case PrimitiveId::ClearCanvas: host_.clear(); break;
                case PrimitiveId::ShowTurtle: host_.show_turtle(); break;
                default: host_.hide_turtle(); break;
            }
            deliver_value(unspecified_value());
            return;
        }

        case PrimitiveId::None:
            break;
    }

    throw SchemeError{"internal error: builtin without an implementation"};
}

}  // namespace timewarp::scheme
