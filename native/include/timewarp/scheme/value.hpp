// SPDX-License-Identifier: MIT
//
// Scheme value model, region allocator and lexical environments.
//
// Ported from the reference Scheme implementation in the Python engine
// (native/bridge/time_warp/languages/scheme.py).
//
// Two deliberate departures from the reference's data model, both forced by
// dropping Python:
//
//   * The reference represented a *vector* as a Python `list` and parsed *code*
//     as a distinct Python `list`.  Those two collapsed into each other, which
//     is exactly the bug that broke `case` (it called `_to_py_list()` on parsed
//     code and got nothing).  Here `Vector` is its own type and code is always
//     built from `Pair`s, so the two can no longer be confused.
//
//   * The reference relied on CPython's cycle collector.  A `letrec` closure
//     and its defining environment point at each other, so `shared_ptr` would
//     leak; a region allocator reclaims everything at once instead.

#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "timewarp/scheme/errors.hpp"

namespace timewarp::scheme {

struct Pair;
struct Vector;
struct Procedure;
struct Env;
struct String;
struct Symbol;

// ---------------------------------------------------------------------------
// Builtin identifiers
// ---------------------------------------------------------------------------

/// Identifies a builtin.
///
/// Dispatching through an enum in a `switch` keeps a call free of the
/// per-invocation `std::function` indirection a table of callables would add,
/// and makes "every id has an implementation" a compiler-checkable property.
enum class PrimitiveId : std::uint16_t {
    None = 0,

    // arithmetic
    Add, Sub, Mul, Div, Abs, Min, Max, Quotient, Remainder, Modulo, Expt,
    Sqrt, FloorF, CeilF, RoundF, Truncate, ExactToInexact, InexactToExact,

    // numeric comparison
    NumEq, NumLt, NumGt, NumLe, NumGe,

    // predicates
    Not, Eq, Eqv, Equal, IsNull, IsPair, IsList, IsNumber, IsInteger,
    IsBoolean, IsString, IsSymbol, IsProcedure, IsVector,
    IsZero, IsPositive, IsNegative, IsOdd, IsEven,

    // pairs and lists
    Cons, Car, Cdr, Caar, Cadr, Cdar, Cddr, List, Length, Append, Reverse,
    ListRef, ListTail, Member, Memq, Assoc,

    // vectors
    Vector, MakeVector, VectorRef, VectorLength, VectorToList, ListToVector,

    // output
    Display, Write, Newline,

    // strings and symbols
    NumberToString, StringToNumber, SymbolToString, StringToSymbol,
    StringLength, StringAppend, StringUpcase, StringDowncase, Substring,
    StringToList, StringEq, StringLt, StringCiEq,

    // control and higher-order
    Error, Apply, Map, ForEach, Filter, Foldl,

    // database
    Sql,

    // turtle graphics
    Forward, Backward, Right, Left, Goto, SetHeading, PenSize, Color,
    PenUp, PenDown, Home, ClearCanvas, Circle, ShowTurtle, HideTurtle,
};

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------

enum class ValueType : std::uint8_t {
    Unspecified,
    Nil,
    Boolean,
    Integer,
    Real,
    String,
    Symbol,
    Pair,
    Vector,
    Procedure,
};

/// A Scheme value: a 16-byte tagged union.
///
/// Only the union member named by `type` is ever read.  Heap members are
/// pointers into an `Arena` and are never owned, which keeps `Value` trivially
/// copyable, trivially destructible and free of reference counting -- values
/// are copied constantly (into argument vectors, frame accumulators, pair
/// cars), so this matters more than the union's raw size suggests.
struct Value {
    ValueType type;

    union {
        bool boolean;
        std::int64_t integer;
        double real;
        String* string;
        Symbol* symbol;
        Pair* pair;
        Vector* vector;
        Procedure* procedure;
    };

    Value() : type{ValueType::Unspecified} {}
};

[[nodiscard]] inline Value unspecified_value() { return Value{}; }

[[nodiscard]] inline Value nil_value() {
    Value value;
    value.type = ValueType::Nil;
    return value;
}

[[nodiscard]] inline Value boolean_value(bool flag) {
    Value value;
    value.type = ValueType::Boolean;
    value.boolean = flag;
    return value;
}

[[nodiscard]] inline Value integer_value(std::int64_t number) {
    Value value;
    value.type = ValueType::Integer;
    value.integer = number;
    return value;
}

[[nodiscard]] inline Value real_value(double number) {
    Value value;
    value.type = ValueType::Real;
    value.real = number;
    return value;
}

[[nodiscard]] inline Value string_value(String* text) {
    Value value;
    value.type = ValueType::String;
    value.string = text;
    return value;
}

[[nodiscard]] inline Value symbol_value(Symbol* name) {
    Value value;
    value.type = ValueType::Symbol;
    value.symbol = name;
    return value;
}

[[nodiscard]] inline Value pair_value(Pair* cell) {
    Value value;
    value.type = ValueType::Pair;
    value.pair = cell;
    return value;
}

[[nodiscard]] inline Value vector_value(Vector* items) {
    Value value;
    value.type = ValueType::Vector;
    value.vector = items;
    return value;
}

[[nodiscard]] inline Value procedure_value(Procedure* procedure) {
    Value value;
    value.type = ValueType::Procedure;
    value.procedure = procedure;
    return value;
}

/// Scheme truthiness: *only* `#f` is false.
///
/// The reference implemented this as `value is not False`, so `0`, `()`, `""`
/// and the unspecified value are all truthy.  Verified against the reference.
[[nodiscard]] inline bool is_truthy(const Value& value) {
    return value.type != ValueType::Boolean || value.boolean;
}

[[nodiscard]] inline bool is_nil(const Value& value) {
    return value.type == ValueType::Nil;
}

[[nodiscard]] inline bool is_pair(const Value& value) {
    return value.type == ValueType::Pair;
}

[[nodiscard]] inline bool is_number(const Value& value) {
    return value.type == ValueType::Integer || value.type == ValueType::Real;
}

[[nodiscard]] inline bool is_integer(const Value& value) {
    return value.type == ValueType::Integer;
}

[[nodiscard]] inline bool is_string(const Value& value) {
    return value.type == ValueType::String;
}

[[nodiscard]] inline bool is_symbol(const Value& value) {
    return value.type == ValueType::Symbol;
}

/// True when `value` is the symbol spelled `name`.
///
/// Defined out of line: `Symbol` is opaque here, which is what keeps `Value`
/// free of a complete-type dependency on the objects it points at.
[[nodiscard]] bool is_symbol_named(const Value& value, std::string_view name);

/// Structural equality, matching the reference's `_equal()`.
///
/// Iterative, so comparing deeply nested structures cannot exhaust the native
/// stack.  Note the two Python-level quirks it preserves: `#t` equals `1`, and
/// a symbol equals a string with the same characters.
[[nodiscard]] bool equal_values(const Value& left, const Value& right);

// ---------------------------------------------------------------------------
// Heap objects
// ---------------------------------------------------------------------------

/// A mutable cons cell.
struct Pair {
    Value car;
    Value cdr;
};

/// A vector.  Distinct from a list, unlike the reference's Python `list`.
struct Vector {
    std::vector<Value> items;
};

/// A heap string.  Distinct from `Symbol`, which the reference modelled by
/// subclassing `str` -- the source of several `isinstance` subtleties.
struct String {
    std::string text;
};

/// An interned symbol.
///
/// Interning makes symbol identity a pointer comparison, so `eq?` and variable
/// lookup never compare characters.
struct Symbol {
    std::string name;
};

/// A user-defined or builtin procedure.
struct Procedure {
    std::vector<Symbol*> params;
    Symbol* rest = nullptr;
    Value body;  ///< list of forms; `Unspecified` for builtins
    Env* env = nullptr;
    PrimitiveId prim = PrimitiveId::None;
    std::string name = "lambda";
};

/// A lexical environment frame.
struct Env {
    Env* parent = nullptr;
    std::unordered_map<Symbol*, Value> bindings;
};

// ---------------------------------------------------------------------------
// Type names (for diagnostics)
// ---------------------------------------------------------------------------

/// Mirrors the reference's `_type_name()`, including its quirks.
///
/// Two surprises are reproduced on purpose: `Nil` and a pair both report
/// "list", and the unspecified value reports "object" -- that last one is the
/// reference leaking CPython's anonymous `object()` sentinel.
[[nodiscard]] inline std::string_view type_name(const Value& value) {
    switch (value.type) {
        case ValueType::Boolean:
            return "boolean";
        case ValueType::Integer:
        case ValueType::Real:
            return "number";
        case ValueType::String:
            return "string";
        case ValueType::Symbol:
            return "symbol";
        case ValueType::Procedure:
            return "procedure";
        case ValueType::Nil:
        case ValueType::Pair:
            return "list";
        case ValueType::Vector:
            return "vector";
        case ValueType::Unspecified:
            break;
    }
    return "object";
}

// ---------------------------------------------------------------------------
// Region allocator
// ---------------------------------------------------------------------------

/// Heterogeneous lookup so interning does not copy the candidate name.
struct TransparentStringHash {
    using is_transparent = void;
    [[nodiscard]] std::size_t operator()(std::string_view text) const noexcept {
        return std::hash<std::string_view>{}(text);
    }
    [[nodiscard]] std::size_t operator()(const std::string& text) const
        noexcept {
        return std::hash<std::string_view>{}(text);
    }
};

/// A monotonic allocator that frees every Scheme object at once.
///
/// Nothing is individually freed, so allocation is a pointer bump and cycles
/// (`letrec` recursion, an environment captured by a closure defined in it) are
/// simply not a problem.  Memory is reclaimed when the program ends.
class Arena {
public:
    explicit Arena(std::size_t block_size = 64u * 1024u)
        : block_size_{block_size} {}
    ~Arena() { release(); }

    Arena(const Arena&) = delete;
    Arena& operator=(const Arena&) = delete;
    Arena(Arena&&) = delete;
    Arena& operator=(Arena&&) = delete;

    /// Constructs a `T` in arena storage.
    template <typename T, typename... Args>
    [[nodiscard]] T* create(Args&&... args) {
        void* storage = allocate(sizeof(T), alignof(T));
        T* object = ::new (storage) T(std::forward<Args>(args)...);
        if constexpr (!std::is_trivially_destructible_v<T>) {
            destructors_.emplace_back(object, [](void* pointer) {
                static_cast<T*>(pointer)->~T();
            });
        }
        return object;
    }

    /// Returns the unique symbol for `name`, creating it on first use.
    [[nodiscard]] Symbol* intern(std::string_view name) {
        const auto existing = symbols_.find(name);
        if (existing != symbols_.end()) {
            return existing->second;
        }
        Symbol* symbol = create<Symbol>(Symbol{std::string{name}});
        symbols_.emplace(symbol->name, symbol);
        return symbol;
    }

    [[nodiscard]] String* make_string(std::string_view text) {
        return create<String>(String{std::string{text}});
    }

    [[nodiscard]] Pair* cons(Value car, Value cdr) {
        return create<Pair>(Pair{car, cdr});
    }

    [[nodiscard]] Vector* make_vector(std::vector<Value> items) {
        return create<Vector>(Vector{std::move(items)});
    }

    [[nodiscard]] Env* make_env(Env* parent) {
        Env* env = create<Env>();
        env->parent = parent;
        return env;
    }

    [[nodiscard]] Procedure* make_procedure(std::vector<Symbol*> params,
                                            Symbol* rest, Value body, Env* env,
                                            PrimitiveId prim,
                                            std::string name) {
        Procedure* procedure = create<Procedure>();
        procedure->params = std::move(params);
        procedure->rest = rest;
        procedure->body = body;
        procedure->env = env;
        procedure->prim = prim;
        procedure->name = std::move(name);
        return procedure;
    }

    /// Builds a proper list from `items`.
    [[nodiscard]] Pair* list(std::span<const Value> items, Value tail) {
        Pair* result = nullptr;
        for (auto it = items.rbegin(); it != items.rend(); ++it) {
            result = cons(*it, result == nullptr ? tail : pair_value(result));
        }
        return result;
    }

    [[nodiscard]] std::size_t bytes_allocated() const noexcept {
        return bytes_;
    }
    [[nodiscard]] std::size_t symbol_count() const noexcept {
        return symbols_.size();
    }

    /// Destroys every object and returns all blocks.
    void release() noexcept;

private:
    [[nodiscard]] void* allocate(std::size_t size, std::size_t alignment);

    struct Block {
        std::unique_ptr<std::byte[]> data;
        std::size_t size;
    };

    std::vector<Block> blocks_;
    std::byte* cursor_ = nullptr;
    std::byte* limit_ = nullptr;
    std::size_t block_size_;
    std::size_t bytes_ = 0;
    std::vector<std::pair<void*, void (*)(void*)>> destructors_;
    std::unordered_map<std::string, Symbol*, TransparentStringHash,
                       std::equal_to<>>
        symbols_;
};

inline void* Arena::allocate(std::size_t size, std::size_t alignment) {
    if (cursor_ != nullptr) {
        const auto address = reinterpret_cast<std::uintptr_t>(cursor_);
        const std::size_t padding = (alignment - (address % alignment)) % alignment;
        if (padding + size <= static_cast<std::size_t>(limit_ - cursor_)) {
            cursor_ += padding + size;
            return reinterpret_cast<void*>(address + padding);
        }
    }

    std::size_t block = block_size_;
    const std::size_t needed = size + alignment;
    if (block < needed) {
        block = needed;
    }
    blocks_.push_back(Block{std::make_unique<std::byte[]>(block), block});
    bytes_ += block;

    cursor_ = blocks_.back().data.get();
    limit_ = cursor_ + block;

    const auto address = reinterpret_cast<std::uintptr_t>(cursor_);
    const std::size_t padding = (alignment - (address % alignment)) % alignment;
    cursor_ += padding + size;
    return reinterpret_cast<void*>(address + padding);
}

inline void Arena::release() noexcept {
    // Reverse construction order, so an object is never destroyed before
    // something it owns.
    for (auto it = destructors_.rbegin(); it != destructors_.rend(); ++it) {
        it->second(it->first);
    }
    destructors_.clear();
    blocks_.clear();
    symbols_.clear();
    cursor_ = nullptr;
    limit_ = nullptr;
    bytes_ = 0;
}

// ---------------------------------------------------------------------------
// List helpers
// ---------------------------------------------------------------------------

/// True when `value` is a proper list (a pair chain ending in `Nil`).
///
/// Iterative: the reference already was, and it must be, because a long list
/// would otherwise exhaust the native stack before evaluation even starts.
[[nodiscard]] inline bool is_proper_list(const Value& value) {
    const Value* cursor = &value;
    while (cursor->type == ValueType::Pair) {
        cursor = &cursor->pair->cdr;
    }
    return cursor->type == ValueType::Nil;
}

/// Copies a proper list into `out`.
///
/// Iterative, and raises the reference's exact message when the spine is
/// improper.  This is the workhorse the argument evaluator, the printer and
/// every list builtin run on, so it must not recurse: a list of a million
/// elements is a legitimate Scheme value.
void to_vector(const Value& list, std::vector<Value>& out);

/// Convenience overload that returns a fresh vector.
[[nodiscard]] std::vector<Value> to_vector(const Value& list);

/// Builds a proper list from `items`, reusing the arena's pair allocation.
[[nodiscard]] inline Value list_from(Arena& arena,
                                     std::span<const Value> items,
                                     Value tail = nil_value()) {
    if (items.empty()) {
        return tail;
    }
    return pair_value(arena.list(items, tail));
}

}  // namespace timewarp::scheme
