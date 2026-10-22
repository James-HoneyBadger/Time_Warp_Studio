// SPDX-License-Identifier: MIT
#include "timewarp/expr/evaluator.hpp"

#include "timewarp/core/charconv.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <system_error>
#include <utility>
#include <variant>

#include "timewarp/expr/detail/ascii.hpp"
#include "timewarp/expr/errors.hpp"

namespace timewarp::expr {
namespace {

using detail::ascii_upper;
using detail::is_type_suffix;

// ---------------------------------------------------------------------------
// Values on the RPN stack
//
// The reference stack held either a ``float`` or a ``str`` (strings appear via
// ``$``-suffixed variables so that ``LEN(A$)`` works).  ``std::variant`` is the
// right model here: the stack is small, short-lived and not a hot allocation
// site, and the alternative (a tagged double + string_view pair) buys nothing
// for the extra bookkeeping.
// ---------------------------------------------------------------------------

using Value = std::variant<double, std::string_view>;

[[nodiscard]] Value pop_value(std::vector<Value>& stack) {
    if (stack.empty()) {
        throw EvalError("Invalid expression");
    }
    Value value = stack.back();
    stack.pop_back();
    return value;
}

/// Extracts the numeric payload, reporting the operator that wanted it.
[[nodiscard]] double require_number(const Value& value, std::string_view who) {
    if (const auto* number = std::get_if<double>(&value)) {
        return *number;
    }
    throw EvalError("operator " + std::string{who} + " requires numeric operands");
}

[[nodiscard]] double pop_number(std::vector<Value>& stack, std::string_view who) {
    return require_number(pop_value(stack), who);
}

// ---------------------------------------------------------------------------
// Integers
// ---------------------------------------------------------------------------

/// Truncates toward zero, mirroring Python's ``int()`` on a float.
///
/// Python has arbitrary-precision integers, so ``int(1e300)`` succeeds; the
/// native port works in 64 bits and must reject what it cannot represent
/// instead of tripping signed-conversion undefined behaviour.
[[nodiscard]] long long checked_to_int(double value) {
    if (!std::isfinite(value)) {
        throw EvalError("integer operand is not finite");
    }
    // 2^63 is exactly representable as a double and is one past LLONG_MAX.
    constexpr double kLimit = 9223372036854775808.0;
    if (value >= kLimit || value < -kLimit) {
        throw EvalError("integer operand out of 64-bit range");
    }
    return static_cast<long long>(std::trunc(value));
}

// ---------------------------------------------------------------------------
// Operator precedence
// ---------------------------------------------------------------------------

constexpr int kUnknownPrecedence = std::numeric_limits<int>::min();

/// Precedence, mirroring the reference table exactly.
///
///   unary (NOT, u+, u-, u~)  4
///   ^                        3
///   * / // % \ MOD REM       2
///   + - << >>                1
///   comparisons              0
///   AND                     -1
///   OR XOR                  -2
[[nodiscard]] constexpr int precedence_of(std::string_view op) noexcept {
    if (op == "u+" || op == "u-" || op == "u~" || op == "NOT") {
        return 4;
    }
    if (op == "^") {
        return 3;
    }
    if (op == "*" || op == "/" || op == "//" || op == "%" || op == "\\" ||
        op == "MOD" || op == "REM") {
        return 2;
    }
    if (op == "+" || op == "-" || op == "<<" || op == ">>") {
        return 1;
    }
    if (op == "<" || op == ">" || op == "<=" || op == ">=" || op == "==" ||
        op == "!=") {
        return 0;
    }
    if (op == "AND") {
        return -1;
    }
    if (op == "OR" || op == "XOR") {
        return -2;
    }
    return kUnknownPrecedence;
}

[[nodiscard]] constexpr bool is_right_associative(std::string_view op) noexcept {
    return op == "^" || op == "u+" || op == "u-" || op == "u~" || op == "NOT";
}

// ---------------------------------------------------------------------------
// Individual operations
// ---------------------------------------------------------------------------

/// ``a << n`` with Python's clamp of the shift count to [0, 64].
///
/// The port clamps to 63: shifting a 64-bit value by 64 is undefined in C++,
/// whereas Python's arbitrary-precision shift simply widens the result.
[[nodiscard]] double shift_left(double lhs, double rhs) {
    const long long value = checked_to_int(lhs);
    const long long count = std::clamp(checked_to_int(rhs), 0LL, 63LL);
    using Unsigned = unsigned long long;
    return static_cast<double>(
        static_cast<long long>(static_cast<Unsigned>(value) << count));
}

[[nodiscard]] double shift_right(double lhs, double rhs) {
    const long long value = checked_to_int(lhs);
    const long long count = std::clamp(checked_to_int(rhs), 0LL, 63LL);
    // C++20 guarantees an arithmetic shift for negative signed values, which is
    // what Python's ``>>`` on ints does (it floors).
    return static_cast<double>(value >> count);
}

[[nodiscard]] double power(double base, double exponent) {
    if (base == 0.0 && exponent < 0.0) {
        // Python raises ZeroDivisionError here.  Letting std::pow return +inf
        // would silently poison BASIC output, so report it as the arithmetic
        // fault it is.
        throw DivisionByZeroError(" (power)");
    }
    if (base < 0.0 && std::floor(exponent) != exponent) {
        // Python produces a complex number, which the numeric engine cannot
        // represent (and the reference then failed converting it back).
        throw EvalError(
            "negative base with fractional exponent has no real result");
    }
    return std::pow(base, exponent);
}

[[nodiscard]] double apply_unary(std::string_view op, const Value& operand) {
    if (op == "NOT") {
        // Logical negation of truthiness: 0 and "" are false.
        if (const auto* text = std::get_if<std::string_view>(&operand)) {
            return text->empty() ? 1.0 : 0.0;
        }
        return std::get<double>(operand) == 0.0 ? 1.0 : 0.0;
    }
    const double value = require_number(operand, op);
    if (op == "u-") {
        return -value;
    }
    if (op == "u~") {
        return static_cast<double>(~checked_to_int(value));
    }
    return value;  // u+
}

[[nodiscard]] double apply_binary(std::string_view op, const Value& lhs,
                                  const Value& rhs) {
    const auto* lhs_text = std::get_if<std::string_view>(&lhs);
    const auto* rhs_text = std::get_if<std::string_view>(&rhs);

    if (lhs_text != nullptr || rhs_text != nullptr) {
        // Relational comparison of two strings is meaningful (Python compared
        // them directly); every other operator is a type error.
        if (lhs_text != nullptr && rhs_text != nullptr) {
            const int ordering = lhs_text->compare(*rhs_text);
            if (op == "==") return ordering == 0 ? 1.0 : 0.0;
            if (op == "!=") return ordering != 0 ? 1.0 : 0.0;
            if (op == "<") return ordering < 0 ? 1.0 : 0.0;
            if (op == ">") return ordering > 0 ? 1.0 : 0.0;
            if (op == "<=") return ordering <= 0 ? 1.0 : 0.0;
            if (op == ">=") return ordering >= 0 ? 1.0 : 0.0;
        }
        throw EvalError("operator " + std::string{op} +
                        " requires numeric operands");
    }

    const double lhs_value = std::get<double>(lhs);
    const double rhs_value = std::get<double>(rhs);

    // Arithmetic -----------------------------------------------------------
    if (op == "+") return lhs_value + rhs_value;
    if (op == "-") return lhs_value - rhs_value;
    if (op == "*") return lhs_value * rhs_value;
    if (op == "/") {
        if (rhs_value == 0.0) throw DivisionByZeroError();
        return lhs_value / rhs_value;
    }
    if (op == "//") {
        if (rhs_value == 0.0) throw DivisionByZeroError(" (integer division)");
        const long long a = checked_to_int(lhs_value);
        const long long b = checked_to_int(rhs_value);
        long long quotient = a / b;
        const long long remainder = a % b;
        // Python's // floors; C++ truncates.  Adjust when the signs differ.
        if (remainder != 0 && ((remainder < 0) != (b < 0))) {
            --quotient;
        }
        return static_cast<double>(quotient);
    }
    if (op == "\\") {
        if (rhs_value == 0.0) throw DivisionByZeroError(" (integer division)");
        // BASIC integer division truncates toward zero (unlike //).
        const long long a = checked_to_int(lhs_value);
        const long long b = checked_to_int(rhs_value);
        return std::trunc(static_cast<double>(a) / static_cast<double>(b));
    }
    if (op == "%" || op == "MOD") {
        if (rhs_value == 0.0) throw DivisionByZeroError(" (MOD)");
        // Floored modulus: the sign follows the *divisor* (Python semantics),
        // whereas std::fmod follows the dividend.  -7 % 3 == 2.
        double result = std::fmod(lhs_value, rhs_value);
        if (result != 0.0 && ((result < 0.0) != (rhs_value < 0.0))) {
            result += rhs_value;
        }
        return result;
    }
    if (op == "REM") {
        if (rhs_value == 0.0) throw DivisionByZeroError(" (REM)");
        // Truncated remainder, i.e. the C/SGN style counterpart of MOD.
        const long long a = checked_to_int(lhs_value);
        const long long b = checked_to_int(rhs_value);
        return static_cast<double>(a - (a / b) * b);
    }

    // Bitwise --------------------------------------------------------------
    if (op == "AND") {
        return static_cast<double>(checked_to_int(lhs_value) &
                                   checked_to_int(rhs_value));
    }
    if (op == "OR") {
        return static_cast<double>(checked_to_int(lhs_value) |
                                   checked_to_int(rhs_value));
    }
    if (op == "XOR") {
        return static_cast<double>(checked_to_int(lhs_value) ^
                                   checked_to_int(rhs_value));
    }
    if (op == "<<") return shift_left(lhs_value, rhs_value);
    if (op == ">>") return shift_right(lhs_value, rhs_value);

    // Power ----------------------------------------------------------------
    if (op == "^") return power(lhs_value, rhs_value);

    // Relational -----------------------------------------------------------
    if (op == "<") return lhs_value < rhs_value ? 1.0 : 0.0;
    if (op == ">") return lhs_value > rhs_value ? 1.0 : 0.0;
    if (op == "<=") return lhs_value <= rhs_value ? 1.0 : 0.0;
    if (op == ">=") return lhs_value >= rhs_value ? 1.0 : 0.0;
    // Approximate equality, as in the reference (BASIC floats rarely compare
    // exactly).
    if (op == "==") return std::abs(lhs_value - rhs_value) < 1e-10 ? 1.0 : 0.0;
    if (op == "!=") return std::abs(lhs_value - rhs_value) >= 1e-10 ? 1.0 : 0.0;

    throw EvalError("Unknown operator: " + std::string{op});
}

// ---------------------------------------------------------------------------
// Built-in functions
//
// Resolved through a sorted, compile-time table + binary search rather than the
// reference's ``Dict[str, Callable]``.  A switch over an enum keeps the call
// sites inlineable and removes the ``std::function`` call-allocation that a
// dictionary of Python callables would otherwise translate into.
// ---------------------------------------------------------------------------

enum class Builtin : std::uint8_t {
    Abs, Acos, Asin, Atan, Atan2, Ceil, Cint, Cos, Cosh, Exp, Fix, Float,
    Floor, Int, Integer, Len, Log, Log10, Log2, Max, Min, Pow, Rand, Random,
    Round, Sgn, Sign, Sin, Sinh, Sqrt, Tan, Tanh, Trunc, Val,
};

struct BuiltinEntry {
    std::string_view name;
    Builtin id;
};

/// Sorted by name so ``lower_bound`` can be used.
inline constexpr BuiltinEntry kBuiltins[] = {
    {"ABS", Builtin::Abs},         {"ACOS", Builtin::Acos},
    {"ASIN", Builtin::Asin},       {"ATAN", Builtin::Atan},
    {"ATAN2", Builtin::Atan2},     {"CEIL", Builtin::Ceil},
    {"CINT", Builtin::Cint},       {"COS", Builtin::Cos},
    {"COSH", Builtin::Cosh},       {"EXP", Builtin::Exp},
    {"FIX", Builtin::Fix},         {"FLOAT", Builtin::Float},
    {"FLOOR", Builtin::Floor},     {"INT", Builtin::Int},
    {"INTEGER", Builtin::Integer}, {"LEN", Builtin::Len},
    {"LOG", Builtin::Log},         {"LOG10", Builtin::Log10},
    {"LOG2", Builtin::Log2},       {"MAX", Builtin::Max},
    {"MIN", Builtin::Min},         {"POW", Builtin::Pow},
    {"RAND", Builtin::Rand},       {"RANDOM", Builtin::Random},
    {"RND", Builtin::Rand},        {"ROUND", Builtin::Round},
    {"SGN", Builtin::Sgn},         {"SIGN", Builtin::Sign},
    {"SIN", Builtin::Sin},         {"SINH", Builtin::Sinh},
    {"SQR", Builtin::Sqrt},        {"SQRT", Builtin::Sqrt},
    {"TAN", Builtin::Tan},         {"TANH", Builtin::Tanh},
    {"TRUNC", Builtin::Trunc},     {"VAL", Builtin::Val},
};

static_assert(std::is_sorted(std::begin(kBuiltins), std::end(kBuiltins),
                             [](const BuiltinEntry& lhs, const BuiltinEntry& rhs) {
                                 return lhs.name < rhs.name;
                             }),
              "kBuiltins must be sorted by name for binary_search");

[[nodiscard]] const BuiltinEntry* find_builtin(std::string_view name) noexcept {
    const auto* first = std::begin(kBuiltins);
    const auto* last = std::end(kBuiltins);
    const auto* position =
        std::lower_bound(first, last, name,
                         [](const BuiltinEntry& entry, std::string_view key) {
                             return entry.name < key;
                         });
    if (position == last || position->name != name) {
        return nullptr;
    }
    return position;
}

// -- random ----------------------------------------------------------------

[[nodiscard]] std::mt19937_64& random_engine() {
    static thread_local std::mt19937_64 engine{std::random_device{}()};
    return engine;
}

/// Uniform ``[0, 1)``, matching ``random.random()``.
[[nodiscard]] double random_unit() {
    return std::generate_canonical<double, 53>(random_engine());
}

/// Uniform integer in ``[0, bound)``, matching ``random.randrange(bound)``.
[[nodiscard]] double random_below(long long bound) {
    if (bound <= 0) {
        throw EvalError("empty range for RANDOM");
    }
    std::uniform_int_distribution<long long> distribution{0, bound - 1};
    return static_cast<double>(distribution(random_engine()));
}

// -- string helpers --------------------------------------------------------

/// Mirrors ``float(text.strip())``; ``empty_is_zero`` mirrors VAL's fallback.
[[nodiscard]] double parse_numeric_text(std::string_view text, bool empty_is_zero) {
    while (!text.empty() && (text.front() == ' ' || text.front() == '\t' ||
                             text.front() == '\n')) {
        text.remove_prefix(1);
    }
    while (!text.empty() && (text.back() == ' ' || text.back() == '\t' ||
                             text.back() == '\n')) {
        text.remove_suffix(1);
    }
    if (text.empty()) {
        if (empty_is_zero) {
            return 0.0;
        }
        throw EvalError("could not convert empty string to a number");
    }
    double value = 0.0;
    const char* first = text.data();
    const char* last = text.data() + text.size();
    const char* end = timewarp::core::parse_double(first, last, value);
    if (end != last) {
        throw EvalError("cannot convert '" + std::string{text} + "' to a number");
    }
    return value;
}

[[nodiscard]] double value_as_number(const Value& value) {
    if (const auto* number = std::get_if<double>(&value)) {
        return *number;
    }
    return parse_numeric_text(std::get<std::string_view>(value), /*empty_is_zero=*/false);
}

/// Validates the argument domain of a math builtin the way Python's ``math``
/// module does (it raises ``ValueError: math domain error``), so BASIC programs
/// keep surfacing a readable error instead of leaking ``nan`` into the console.
[[nodiscard]] double require_domain(double value, bool strictly_positive,
                                    bool unit_interval) {
    if (strictly_positive && value <= 0.0) {
        throw EvalError("math domain error");
    }
    if (unit_interval && (value < -1.0 || value > 1.0)) {
        throw EvalError("math domain error");
    }
    return value;
}

[[nodiscard]] double apply_unary_builtin(Builtin id, const Value& argument) {
    switch (id) {
        case Builtin::Len:
            if (const auto* text = std::get_if<std::string_view>(&argument)) {
                return static_cast<double>(text->size());
            }
            throw EvalError("LEN requires a string argument");

        case Builtin::Val:
            if (const auto* text = std::get_if<std::string_view>(&argument)) {
                return parse_numeric_text(*text, /*empty_is_zero=*/true);
            }
            return std::get<double>(argument);

        case Builtin::Float:
            return value_as_number(argument);

        case Builtin::Abs:
            return std::fabs(require_number(argument, "ABS"));

        case Builtin::Sin:
            return std::sin(require_number(argument, "SIN"));
        case Builtin::Cos:
            return std::cos(require_number(argument, "COS"));
        case Builtin::Tan:
            return std::tan(require_number(argument, "TAN"));
        case Builtin::Sinh:
            return std::sinh(require_number(argument, "SINH"));
        case Builtin::Cosh:
            return std::cosh(require_number(argument, "COSH"));
        case Builtin::Tanh:
            return std::tanh(require_number(argument, "TANH"));

        case Builtin::Asin:
            return std::asin(require_domain(require_number(argument, "ASIN"),
                                            false, true));
        case Builtin::Acos:
            return std::acos(require_domain(require_number(argument, "ACOS"),
                                            false, true));
        case Builtin::Atan:
            return std::atan(require_number(argument, "ATAN"));

        case Builtin::Sqrt:
            return std::sqrt(require_domain(require_number(argument, "SQRT"),
                                            true, false));
        case Builtin::Exp:
            return std::exp(require_number(argument, "EXP"));

        case Builtin::Log:
            return std::log(require_domain(require_number(argument, "LOG"),
                                           true, false));
        case Builtin::Log10:
            return std::log10(require_domain(require_number(argument, "LOG10"),
                                             true, false));
        case Builtin::Log2:
            return std::log2(require_domain(require_number(argument, "LOG2"),
                                            true, false));

        // Rounding family.  Python's round() is half-to-even, which is exactly
        // what nearbyint does under the default FE_TONEAREST mode.
        case Builtin::Int:
        case Builtin::Floor:
            return std::floor(require_number(argument, "INT"));
        case Builtin::Ceil:
            return std::ceil(require_number(argument, "CEIL"));
        case Builtin::Round:
        case Builtin::Cint:
            return std::nearbyint(require_number(argument, "ROUND"));
        case Builtin::Trunc:
            return std::trunc(require_number(argument, "TRUNC"));
        case Builtin::Integer:
            return std::trunc(require_number(argument, "INTEGER"));
        case Builtin::Fix: {
            const double value = require_number(argument, "FIX");
            return value >= 0.0 ? std::floor(value) : std::ceil(value);
        }
        case Builtin::Sgn: {
            const double value = require_number(argument, "SGN");
            return value > 0.0 ? 1.0 : (value < 0.0 ? -1.0 : 0.0);
        }
        case Builtin::Sign: {
            const double value = require_number(argument, "SIGN");
            return value > 0.0 ? 1.0 : (value < 0.0 ? -1.0 : 0.0);
        }

        case Builtin::Random: {
            const double bound = require_number(argument, "RANDOM");
            // The reference truncated the bound and returned 0 for a
            // non-positive one.
            return bound > 0.0 ? random_below(checked_to_int(bound)) : 0.0;
        }

        // Binary / nullary builtins are intercepted before this point.
        case Builtin::Min:
        case Builtin::Max:
        case Builtin::Pow:
        case Builtin::Atan2:
        case Builtin::Rand:
            break;
    }
    throw EvalError("Unknown function");
}

/// Executes a FUNCTION token, consuming arguments from ``stack``.
void apply_function(const EvalContext& context, std::string_view name,
                    std::vector<Value>& stack) {
    // 1. User arrays shadow everything else (the reference checked arrays
    //    before the builtin table, so an array named MAX wins over MAX()).
    if (!context.arrays.empty()) {
        const auto array = context.arrays.find(name);
        if (array != context.arrays.end()) {
            if (stack.empty()) {
                throw EvalError("Array " + std::string{name} + " requires index");
            }
            const long long index = checked_to_int(pop_number(stack, name));
            const std::span<const double> values = array->second;
            if (index < 0 || static_cast<std::size_t>(index) >= values.size()) {
                throw EvalError("Array index out of bounds: " +
                                std::to_string(index));
            }
            stack.emplace_back(values[static_cast<std::size_t>(index)]);
            return;
        }
    }

    // 1b. Functions the executing language registered (COMAL's FUNC). They are
    //     consulted before the builtin table, because the reference installs
    //     them into its function dictionary and so lets them shadow a builtin.
    if (context.functions) {
        if (auto function = context.functions(name)) {
            if (stack.size() < function->arity) {
                throw EvalError(std::string{name} + " requires " +
                                std::to_string(function->arity) + " arguments");
            }
            std::vector<double> arguments(function->arity);
            for (std::size_t index = function->arity; index-- > 0;) {
                arguments[index] = pop_number(stack, name);
            }
            stack.emplace_back(function->call(arguments));
            return;
        }
    }

    const BuiltinEntry* entry = find_builtin(name);
    if (entry == nullptr) {
        throw EvalError("Unknown function: " + std::string{name});
    }

    switch (entry->id) {
        // 2. Two-argument builtins.
        case Builtin::Min:
        case Builtin::Max:
        case Builtin::Pow:
        case Builtin::Atan2: {
            if (stack.size() < 2) {
                throw EvalError(std::string{name} + " requires 2 arguments");
            }
            const double rhs = pop_number(stack, name);
            const double lhs = pop_number(stack, name);
            double result = 0.0;
            switch (entry->id) {
                case Builtin::Min:   result = std::min(lhs, rhs); break;
                case Builtin::Max:   result = std::max(lhs, rhs); break;
                case Builtin::Atan2: result = std::atan2(lhs, rhs); break;
                default:             result = power(lhs, rhs); break;
            }
            stack.emplace_back(result);
            return;
        }

        // 3. RAND/RND: the reference popped an optional numeric argument
        //    (BASIC spells it RND(1)) and ignored it.
        case Builtin::Rand: {
            if (!stack.empty() && std::holds_alternative<double>(stack.back())) {
                stack.pop_back();
            }
            stack.emplace_back(random_unit());
            return;
        }

        default:
            break;
    }

    if (stack.empty()) {
        throw EvalError(std::string{name} + " requires an argument");
    }
    stack.emplace_back(
        apply_unary_builtin(entry->id, pop_value(stack)));
}

}  // namespace

// ---------------------------------------------------------------------------
// Construction / context
// ---------------------------------------------------------------------------

ExpressionEvaluator::ExpressionEvaluator() = default;

ExpressionEvaluator::ExpressionEvaluator(EvalContext context)
    : context_(std::move(context)) {}

void ExpressionEvaluator::set_variable(std::string_view name, double value) {
    std::array<char, 96> scratch{};
    context_.variables.insert_or_assign(
        std::string{normalize_name(name, scratch)}, value);
}

void ExpressionEvaluator::set_array(std::string_view name,
                                    std::span<const double> values) {
    std::array<char, 96> scratch{};
    context_.arrays.insert_or_assign(std::string{normalize_name(name, scratch)},
                                     values);
}

void ExpressionEvaluator::set_string_variable(std::string_view name,
                                              std::string_view value) {
    std::array<char, 96> scratch{};
    context_.string_variables.insert_or_assign(
        std::string{normalize_name(name, scratch)}, value);
}

void ExpressionEvaluator::set_special_variable_hook(SpecialVariableHook hook) {
    context_.special_variables = std::move(hook);
}

void ExpressionEvaluator::set_function_hook(FunctionHook hook) {
    context_.functions = std::move(hook);
}

void ExpressionEvaluator::set_max_tokens(std::size_t max_tokens) {
    lexer_ = Lexer{max_tokens};
    token_cache_.clear();
}

std::string_view ExpressionEvaluator::normalize_name(
    std::string_view raw, std::span<char> scratch) const {
    if (raw.size() <= scratch.size()) {
        char* out = scratch.data();
        for (std::size_t i = 0; i < raw.size(); ++i) {
            out[i] = ascii_upper(raw[i]);
        }
        return {out, raw.size()};
    }
    // Rare: identifiers longer than the stack buffer use a reusable member
    // buffer rather than allocating.
    long_name_scratch_.resize(raw.size());
    for (std::size_t i = 0; i < raw.size(); ++i) {
        long_name_scratch_[i] = ascii_upper(raw[i]);
    }
    return long_name_scratch_;
}

// ---------------------------------------------------------------------------
// Shunting-yard
// ---------------------------------------------------------------------------

std::vector<Token> ExpressionEvaluator::to_rpn(
    std::span<const Token> tokens) const {
    std::vector<Token> output;
    output.reserve(tokens.size());
    std::vector<Token> operators;

    for (const Token& token : tokens) {
        switch (token.type) {
            case TokenType::Number:
            case TokenType::Variable:
                output.push_back(token);
                break;

            case TokenType::Function:
                operators.push_back(token);
                break;

            case TokenType::Operator:
            case TokenType::Comparison: {
                const std::string_view op = token.text;
                const int op_precedence = precedence_of(op);
                if (op_precedence == kUnknownPrecedence) {
                    throw ParseError("Unknown operator: " + std::string{op});
                }

                while (!operators.empty()) {
                    const Token& top = operators.back();
                    if (top.type == TokenType::LeftParen) {
                        break;
                    }
                    if (top.type == TokenType::Operator ||
                        top.type == TokenType::Comparison) {
                        const std::string_view top_op = top.text;
                        const int top_precedence = precedence_of(top_op);
                        const bool pops =
                            top_precedence > op_precedence ||
                            (top_precedence == op_precedence &&
                             !is_right_associative(op));
                        if (!pops) {
                            break;
                        }
                        output.push_back(operators.back());
                        operators.pop_back();
                    } else if (top.type == TokenType::Function) {
                        // A pending function call binds tighter than any
                        // operator, so flush it.
                        output.push_back(operators.back());
                        operators.pop_back();
                    } else {
                        break;
                    }
                }
                operators.push_back(token);
                break;
            }

            case TokenType::LeftParen:
                operators.push_back(token);
                break;

            case TokenType::RightParen:
                while (!operators.empty() &&
                       operators.back().type != TokenType::LeftParen) {
                    output.push_back(operators.back());
                    operators.pop_back();
                }
                if (operators.empty()) {
                    throw ParseError("Mismatched parentheses");
                }
                operators.pop_back();  // discard '('
                if (!operators.empty() &&
                    operators.back().type == TokenType::Function) {
                    output.push_back(operators.back());
                    operators.pop_back();
                }
                break;

            case TokenType::Comma:
                // Argument separator: unwind to the enclosing '('.
                while (!operators.empty() &&
                       operators.back().type != TokenType::LeftParen) {
                    output.push_back(operators.back());
                    operators.pop_back();
                }
                break;
        }
    }

    while (!operators.empty()) {
        if (operators.back().type == TokenType::LeftParen) {
            throw ParseError("Mismatched parentheses");
        }
        output.push_back(operators.back());
        operators.pop_back();
    }

    return output;
}

// ---------------------------------------------------------------------------
// RPN virtual machine
// ---------------------------------------------------------------------------

double ExpressionEvaluator::evaluate_rpn(std::span<const Token> rpn) const {
    std::vector<Value> stack;
    stack.reserve(rpn.size());

    for (const Token& token : rpn) {
        switch (token.type) {
            case TokenType::Number:
                stack.emplace_back(token.number);
                break;

            case TokenType::Variable: {
                std::array<char, 96> scratch{};
                // The variable spelling includes any BASIC type suffix, so the
                // whole thing is upper-cased in one pass.
                const std::string_view name = normalize_name(token.text, scratch);

                // RND/RAND are dynamic generators rather than stored variables.
                if (name == "RND" || name == "RAND") {
                    stack.emplace_back(random_unit());
                    break;
                }

                // TIMER and friends come from the host through the hook.
                if (name == "TIMER" && context_.special_variables) {
                    if (const auto value = context_.special_variables(name)) {
                        stack.emplace_back(*value);
                        break;
                    }
                }

                std::string_view base = name;
                if (!name.empty() && is_type_suffix(name.back())) {
                    if (name.back() == '$') {
                        // String variable: valid as an operand of LEN() and of
                        // string comparisons; empty when unset.
                        const auto found = context_.string_variables.find(name);
                        stack.emplace_back(found == context_.string_variables.end()
                                               ? std::string_view{}
                                               : found->second);
                        break;
                    }
                    base = name.substr(0, name.size() - 1);
                }

                const auto found = context_.variables.find(base);
                if (found != context_.variables.end()) {
                    stack.emplace_back(found->second);
                    break;
                }

                if (base == "PI") { stack.emplace_back(3.14159265358979323846); break; }
                if (base == "E") { stack.emplace_back(2.71828182845904523536); break; }
                if (base == "TAU") { stack.emplace_back(6.28318530717958647693); break; }
                if (base == "TRUE") { stack.emplace_back(1.0); break; }
                if (base == "FALSE") { stack.emplace_back(0.0); break; }

                if (context_.special_variables) {
                    if (const auto value = context_.special_variables(name)) {
                        stack.emplace_back(*value);
                        break;
                    }
                }

                throw EvalError("Undefined variable: " + std::string{name});
            }

            case TokenType::Operator:
            case TokenType::Comparison: {
                const std::string_view op = token.text;
                if (op == "u+" || op == "u-" || op == "u~" || op == "NOT") {
                    stack.emplace_back(apply_unary(op, pop_value(stack)));
                    break;
                }
                if (stack.size() < 2) {
                    throw EvalError("Invalid expression");
                }
                const Value rhs = pop_value(stack);
                const Value lhs = pop_value(stack);
                stack.emplace_back(apply_binary(op, lhs, rhs));
                break;
            }

            case TokenType::Function: {
                // The array and builtin tables are keyed in upper case and BASIC
                // is case-insensitive, which the two paths disagreed about: the
                // assignment statement upper-cased a name before storing it and
                // the `Variable` branch above upper-cases before reading, but a
                // call arrived here spelled exactly as it was written. So
                // `DIM dat(3)` then `PRINT dat(0)` was "Unknown function: dat"
                // while `PRINT DAT(0)` worked -- and `Examples/basic/
                // math_explorer.bas` is written in lower case.
                std::array<char, 96> scratch{};
                apply_function(context_, normalize_name(token.text, scratch), stack);
                break;
            }

            case TokenType::LeftParen:
            case TokenType::RightParen:
            case TokenType::Comma:
                // Fully consumed by the shunting-yard pass; their presence in
                // an RPN stream means the program was built by hand.
                throw ParseError("Malformed RPN program");
        }
    }

    if (stack.size() != 1) {
        throw EvalError("Invalid expression");
    }
    if (const auto* number = std::get_if<double>(&stack.front())) {
        return *number;
    }
    // The engine is numeric: a bare string result is not a number.
    throw EvalError("expression did not produce a numeric result");
}

// ---------------------------------------------------------------------------
// Combined pipeline
// ---------------------------------------------------------------------------

double ExpressionEvaluator::evaluate(std::string_view expression) {
    const std::vector<Token>* tokens = nullptr;

    const auto cached = token_cache_.find(expression);  // heterogeneous: no alloc
    if (cached != token_cache_.end()) {
        tokens = &cached->second;
    } else {
        if (token_cache_.size() >= max_token_cache_capacity_) {
            token_cache_.clear();
        }
        // Insert first, then lex from the *stored* key: unordered_map is
        // node-based, so the key's buffer never moves and the token views
        // borrow from stable storage.
        const auto [position, inserted] =
            token_cache_.try_emplace(std::string{expression});
        try {
            position->second = lexer_.tokenize(position->first);
        } catch (...) {
            // A failed lex must not be cached: leaving the (empty) entry behind
            // would make the *next* attempt report "Invalid expression" instead
            // of the real syntax error.  The reference only ever cached
            // successful tokenisations.
            token_cache_.erase(position);
            throw;
        }
        tokens = &position->second;
    }

    const std::vector<Token> rpn = to_rpn(*tokens);
    return evaluate_rpn(rpn);
}

}  // namespace timewarp::expr
