// SPDX-License-Identifier: MIT

#include "timewarp/scheme/value.hpp"

#include <cstddef>
#include <string>
#include <utility>
#include <vector>

namespace timewarp::scheme {
namespace {

/// Compares two values that are not pairs or vectors.
///
/// Python's `==` is surprisingly loose across types, and all of it is
/// observable from Scheme:
///   * `True == 1` and `False == 0`,
///   * `1 == 1.0`,
///   * `Symbol("a") == "a"`, because `Symbol` subclasses `str`.
[[nodiscard]] bool scalar_equal(const Value& left, const Value& right) {
    if (left.type == right.type) {
        switch (left.type) {
            case ValueType::Boolean:
                return left.boolean == right.boolean;
            case ValueType::Integer:
                return left.integer == right.integer;
            case ValueType::Real:
                return left.real == right.real;
            case ValueType::String:
                return left.string->text == right.string->text;
            case ValueType::Symbol:
                // Interned, so identity is content equality.
                return left.symbol == right.symbol;
            case ValueType::Nil:
            case ValueType::Unspecified:
                return true;
            case ValueType::Procedure:
                return left.procedure == right.procedure;
            case ValueType::Pair:
            case ValueType::Vector:
                break;
        }
        return false;
    }

    if (left.type == ValueType::Symbol && right.type == ValueType::String) {
        return left.symbol->name == right.string->text;
    }
    if (left.type == ValueType::String && right.type == ValueType::Symbol) {
        return left.string->text == right.symbol->name;
    }

    const auto as_real = [](const Value& value, double& out) {
        if (value.type == ValueType::Integer) {
            out = static_cast<double>(value.integer);
            return true;
        }
        if (value.type == ValueType::Real) {
            out = value.real;
            return true;
        }
        if (value.type == ValueType::Boolean) {
            out = value.boolean ? 1.0 : 0.0;
            return true;
        }
        return false;
    };

    double left_real = 0.0;
    double right_real = 0.0;
    if (as_real(left, left_real) && as_real(right, right_real)) {
        return left_real == right_real;
    }
    return false;
}

}  // namespace

bool is_symbol_named(const Value& value, std::string_view name) {
    return value.type == ValueType::Symbol && value.symbol->name == name;
}

bool equal_values(const Value& left, const Value& right) {
    // An explicit work stack replaces the reference's recursion, so comparing
    // two long lists or deeply nested structures cannot blow the native stack.
    std::vector<std::pair<Value, Value>> work;
    work.emplace_back(left, right);

    while (!work.empty()) {
        const std::pair<Value, Value> pair = work.back();
        work.pop_back();
        const Value& a = pair.first;
        const Value& b = pair.second;

        if (a.type == ValueType::Pair && b.type == ValueType::Pair) {
            work.emplace_back(a.pair->car, b.pair->car);
            work.emplace_back(a.pair->cdr, b.pair->cdr);
            continue;
        }
        if (a.type == ValueType::Vector && b.type == ValueType::Vector) {
            if (a.vector->items.size() != b.vector->items.size()) {
                return false;
            }
            for (std::size_t index = 0; index < a.vector->items.size();
                 ++index) {
                work.emplace_back(a.vector->items[index],
                                  b.vector->items[index]);
            }
            continue;
        }
        if (!scalar_equal(a, b)) {
            return false;
        }
    }
    return true;
}

void to_vector(const Value& list, std::vector<Value>& out) {
    out.clear();
    const Value* cursor = &list;
    while (cursor->type == ValueType::Pair) {
        out.push_back(cursor->pair->car);
        cursor = &cursor->pair->cdr;
    }
    if (cursor->type != ValueType::Nil) {
        throw SchemeError{"expected a proper list"};
    }
}

std::vector<Value> to_vector(const Value& list) {
    std::vector<Value> out;
    to_vector(list, out);
    return out;
}

}  // namespace timewarp::scheme
