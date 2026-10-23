// SPDX-License-Identifier: MIT
//
// The structural functions: IOTA, rho, take, drop, rotate, transpose, catenate,
// membership and grade.

#include "apl_internal.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace apl_detail {

// ---------------------------------------------------------------------------
// Structural functions
// ---------------------------------------------------------------------------

Value iota(const Value& right) {
    if (is_array(right)) {
        std::vector<double> shape;
        double product = 1.0;
        for (const Value& item : raze(right)) {
            shape.push_back(number_of(item));
            product *= number_of(item);
        }
        std::vector<Value> flat;
        flat.reserve(static_cast<std::size_t>(product));
        for (double value = 1.0; value <= product; value += 1.0) {
            flat.push_back(Value::numbered(value));
        }
        return from_flat(shape, flat);
    }
    const long long count = static_cast<long long>(number_of(right));
    if (count < 0) {
        throw AplError("IOTA needs a non-negative count");
    }
    std::vector<Value> result;
    result.reserve(static_cast<std::size_t>(count));
    for (long long value = 1; value <= count; ++value) {
        result.push_back(Value::numbered(static_cast<double>(value)));
    }
    return Value::array(std::move(result));
}

Value index_of(const Value& left, const Value& right) {
    const std::vector<Value> haystack = raze(left);
    const auto look = [&haystack](const Value& item) {
        for (std::size_t index = 0; index < haystack.size(); ++index) {
            if (values_equal(haystack[index], item)) {
                return Value::numbered(static_cast<double>(index + 1));
            }
        }
        return Value::numbered(static_cast<double>(haystack.size() + 1));
    };
    return each1(look, right);
}

Value reverse(const Value& right) {
    if (!is_array(right)) {
        return right;
    }
    std::vector<Value> items = right.items;
    std::reverse(items.begin(), items.end());
    return Value::array(std::move(items));
}

Value rotate(const Value& left, const Value& right) {
    if (!is_array(right) || right.items.empty()) {
        return right;
    }
    const long long count = static_cast<long long>(number_of(left));
    const long long size = static_cast<long long>(right.items.size());
    long long shift = count % size;
    if (shift < 0) {
        shift += size;
    }
    std::vector<Value> items = right.items;
    std::rotate(items.begin(), items.begin() + shift, items.end());
    return Value::array(std::move(items));
}

Value take(const Value& left, const Value& right) {
    const long long count = static_cast<long long>(number_of(left));
    std::vector<Value> items = items_of(right);
    if (count >= 0) {
        while (items.size() < static_cast<std::size_t>(count)) {
            items.push_back(Value::numbered(0.0));
        }
        items.resize(static_cast<std::size_t>(count));
        return Value::array(std::move(items));
    }
    const std::size_t wanted = static_cast<std::size_t>(-count);
    if (wanted > items.size()) {
        std::vector<Value> padded(wanted - items.size(), Value::numbered(0.0));
        padded.insert(padded.end(), items.begin(), items.end());
        return Value::array(std::move(padded));
    }
    std::vector<Value> result(items.end() - static_cast<std::ptrdiff_t>(wanted),
                              items.end());
    return Value::array(std::move(result));
}

Value drop(const Value& left, const Value& right) {
    const long long count = static_cast<long long>(number_of(left));
    std::vector<Value> items = items_of(right);
    if (count == 0) {
        return Value::array(std::move(items));
    }
    if (count > 0) {
        const std::size_t start =
            std::min(items.size(), static_cast<std::size_t>(count));
        return Value::array(
            std::vector<Value>(items.begin() + static_cast<std::ptrdiff_t>(start),
                               items.end()));
    }
    const std::size_t keep =
        items.size() > static_cast<std::size_t>(-count)
            ? items.size() - static_cast<std::size_t>(-count)
            : 0;
    return Value::array(
        std::vector<Value>(items.begin(),
                           items.begin() + static_cast<std::ptrdiff_t>(keep)));
}

Value transpose(const Value& right) {
    if (!is_array(right) || right.items.empty() || !is_array(right.items.front())) {
        return right;
    }
    // zip(*rows): the result has one row per *column*, as wide as the shortest
    // input row. Starting from the number of rows transposed only the first two
    // columns of a 2-by-3 table.
    std::size_t width = is_array(right.items.front())
                            ? right.items.front().items.size()
                            : 0;
    for (const Value& row : right.items) {
        if (is_array(row)) {
            width = std::min(width, row.items.size());
        }
    }
    std::vector<Value> rows;
    rows.reserve(width);
    for (std::size_t column = 0; column < width; ++column) {
        std::vector<Value> row;
        row.reserve(right.items.size());
        for (const Value& source : right.items) {
            row.push_back(is_array(source) ? source.items[column] : source);
        }
        rows.push_back(Value::array(std::move(row)));
    }
    return Value::array(std::move(rows));
}

Value catenate(const Value& left, const Value& right) {
    std::vector<Value> items = items_of(left);
    const std::vector<Value> tail = items_of(right);
    items.insert(items.end(), tail.begin(), tail.end());
    return Value::array(std::move(items));
}

Value membership(const Value& left, const Value& right) {
    const std::vector<Value> haystack = raze(left);
    const auto look = [&haystack](const Value& item) {
        for (const Value& candidate : haystack) {
            if (values_equal(candidate, item)) {
                return Value::numbered(1.0);
            }
        }
        return Value::numbered(0.0);
    };
    return each1(look, right);
}

Value grade(const Value& right, bool descending) {
    std::vector<Value> items = items_of(right);
    std::vector<std::size_t> positions(items.size());
    for (std::size_t index = 0; index < positions.size(); ++index) {
        positions[index] = index;
    }
    std::stable_sort(positions.begin(), positions.end(),
                     [&items, descending](std::size_t left, std::size_t right_index) {
                         return descending ? value_less(items[right_index], items[left])
                                           : value_less(items[left], items[right_index]);
                     });
    std::vector<Value> result;
    result.reserve(positions.size());
    for (std::size_t position : positions) {
        result.push_back(Value::numbered(static_cast<double>(position + 1)));
    }
    return Value::array(std::move(result));
}

Value reshape(const Value& left, const Value& right) {
    std::vector<double> shape;
    for (const Value& item : raze(left)) {
        shape.push_back(number_of(item));
    }
    if (shape.empty()) {
        return right;
    }
    const std::vector<Value> flat = raze(right);
    if (flat.empty()) {
        throw AplError("reshape of an empty array");
    }
    return from_flat(shape, flat);
}

}  // namespace apl_detail
}  // namespace timewarp::lang
