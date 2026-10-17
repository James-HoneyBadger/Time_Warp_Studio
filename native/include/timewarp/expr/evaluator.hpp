// SPDX-License-Identifier: MIT
//
// Safe expression evaluator (tokenize -> shunting-yard -> RPN machine).
//
// Port of ``time_warp.utils.expression_evaluator.ExpressionEvaluator``.
//
// Pipeline, mirroring the reference so hosts can cache each stage
// independently:
//
//     source --tokenize--> tokens --to_rpn--> rpn --evaluate_rpn--> double
//
// ``evaluate()`` runs all three with a memoised token cache, which is the fast
// path for the interpreter's hot loop (the same expression is re-evaluated once
// per BASIC statement iteration while variables change underneath it).
//
// Security: there is no eval/exec, no dynamic lookup by name, and no way to
// call arbitrary code.  The operator/function/constant tables are fixed and
// resolved by the engine itself.

#pragma once

#include <array>
#include <cstddef>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "timewarp/expr/lexer.hpp"
#include "timewarp/expr/token.hpp"

namespace timewarp::expr {

// ---------------------------------------------------------------------------
// Transparent string hashing
//
// Lets the variable/array/string tables be probed with the ``std::string_view``
// produced by name normalisation, without materialising a temporary
// ``std::string`` per lookup.
// ---------------------------------------------------------------------------

struct StringHash {
    using is_transparent = void;  ///< opt in to heterogeneous lookup
    [[nodiscard]] std::size_t operator()(std::string_view value) const noexcept {
        return std::hash<std::string_view>{}(value);
    }
};

struct StringEqual {
    using is_transparent = void;
    [[nodiscard]] bool operator()(std::string_view lhs,
                                  std::string_view rhs) const noexcept {
        return lhs == rhs;
    }
};

/// Callable resolving engine-defined pseudo-variables that are not stored in
/// the variable table.
///
/// This replaces the reference implementation's hard-coded
/// ``from ..features.game_support import get_game_state`` for the ``TIMER``
/// variable.  The hook receives the *upper-cased* name and returns
/// ``std::nullopt`` when it does not know the name.
using SpecialVariableHook =
    std::function<std::optional<double>(std::string_view upper_name)>;

/// A function a language executor adds to the evaluator, e.g. COMAL's `FUNC`.
struct UserFunction {
    /// How many arguments to pop from the value stack.
    std::size_t arity = 1;
    /// Called with ``arity`` values, in source order.
    std::function<double(std::span<const double>)> call;
};

/// Resolves a name to a language-defined function, or ``std::nullopt``.
using FunctionHook =
    std::function<std::optional<UserFunction>(std::string_view upper_name)>;

/// Everything the evaluator needs to resolve names at run time.
///
/// All maps are keyed by *upper-case* names, matching the reference, because
/// the engine stores variables canonically upper-cased.  The lookup itself is
/// case-insensitive and allocation-free.
struct EvalContext {
    /// Scalar variables, e.g. ``{"X", 10.0}``.
    std::unordered_map<std::string, double, StringHash, StringEqual> variables;

    /// Arrays addressable as ``NAME(index)``.  Spans are non-owning views, so
    /// the backing storage must outlive the evaluator.
    std::unordered_map<std::string, std::span<const double>, StringHash,
                       StringEqual>
        arrays;

    /// String variables referenced with a ``$`` suffix, e.g. ``{"NAME$", "x"}``.
    std::unordered_map<std::string, std::string_view, StringHash, StringEqual>
        string_variables;

    /// Optional resolver for pseudo-variables (see ``SpecialVariableHook``).
    SpecialVariableHook special_variables;

    /// Optional resolver for language-defined functions.
    FunctionHook functions;
};

/// Evaluates arithmetic/logical expressions to ``double``.
///
/// Thread safety: ``tokenize``/``to_rpn``/``evaluate_rpn`` are const and
/// re-entrant.  ``evaluate`` mutates the token cache and a scratch buffer, so
/// it is *not* thread-safe on a shared instance -- give each thread its own
/// evaluator, which is what the interpreter does anyway.
class ExpressionEvaluator {
public:
    static constexpr std::size_t kDefaultMaxTokens = Lexer::kDefaultMaxTokens;
    static constexpr std::size_t kDefaultTokenCacheCapacity = 1024;

    ExpressionEvaluator();
    explicit ExpressionEvaluator(EvalContext context);

    // -- context mutation ---------------------------------------------------

    /// Sets a scalar variable.  ``name`` is upper-cased, so ``set_variable("x", 1)``
    /// and ``set_variable("X", 1)`` address the same slot.
    void set_variable(std::string_view name, double value);

    /// Binds an array to ``values``; the span must outlive the evaluator.
    void set_array(std::string_view name, std::span<const double> values);

    /// Binds a string variable; ``value`` must outlive the evaluator.
    void set_string_variable(std::string_view name, std::string_view value);

    /// Installs the pseudo-variable resolver.
    void set_special_variable_hook(SpecialVariableHook hook);

    /// Installs the language-defined function resolver.
    void set_function_hook(FunctionHook hook);

    [[nodiscard]] const EvalContext& context() const noexcept { return context_; }

    // -- phased API ---------------------------------------------------------

    /// Lexes ``source``.  The result borrows from ``source``.
    /// @throws LexError
    [[nodiscard]] std::vector<Token> tokenize(std::string_view source) const {
        return lexer_.tokenize(source);
    }

    /// Rewrites an infix token stream into Reverse Polish Notation using the
    /// shunting-yard algorithm.
    /// @throws ParseError on unbalanced parentheses.
    [[nodiscard]] std::vector<Token> to_rpn(
        std::span<const Token> tokens) const;

    /// Executes an RPN program on a value stack.
    /// @throws EvalError, DivisionByZeroError
    [[nodiscard]] double evaluate_rpn(std::span<const Token> rpn) const;

    // -- combined API -------------------------------------------------------

    /// Tokenizes (with memoisation), compiles and evaluates ``expression``.
    ///
    /// @throws LexError, ParseError, EvalError, DivisionByZeroError
    ///
    /// The token cache keys on the *text* of the expression only.  Variable
    /// values are resolved during ``evaluate_rpn``, so mutating a variable and
    /// re-evaluating the same text observes the new value.
    [[nodiscard]] double evaluate(std::string_view expression);

    /// Number of expressions currently memoised.
    [[nodiscard]] std::size_t cached_expression_count() const noexcept {
        return token_cache_.size();
    }

    /// Drops the memoised token streams.
    void clear_token_cache() noexcept { token_cache_.clear(); }

    /// Overrides the token budget (also resets the cache).
    void set_max_tokens(std::size_t max_tokens);

private:
    /// Upper-cases ``raw`` into ``scratch`` if it fits, otherwise into a
    /// reusable member buffer, and returns a view of the result.
    [[nodiscard]] std::string_view normalize_name(
        std::string_view raw, std::span<char> scratch) const;

    EvalContext context_;
    Lexer lexer_;
    /// Memoised token streams.  ``unordered_map`` is node-based, so the key
    /// strings never move; the tokens (which borrow from those keys) therefore
    /// stay valid while an entry exists.
    std::unordered_map<std::string, std::vector<Token>, StringHash, StringEqual>
        token_cache_;
    std::size_t max_token_cache_capacity_{kDefaultTokenCacheCapacity};
    /// Backing store for names longer than the stack scratch buffer.
    mutable std::string long_name_scratch_;
};

}  // namespace timewarp::expr
