// SPDX-License-Identifier: MIT
//
// Values: the type, the operations that make and read arrays, and the scalar
// functions. Everything else is written in terms of these.

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

// Read by the file that defines them, in this order: `display_text` renders each row
// through `row_text` before the rows are joined. The header no longer declares them,
// because nothing outside this file calls them.
std::string number_text(double value);
std::string row_text(const Value& value);

bool is_array(const Value& value) { return value.kind == Value::Kind::Array; }

bool is_number(const Value& value) { return value.kind == Value::Kind::Number; }

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character =
            static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

bool is_space(char character) {
    return std::isspace(static_cast<unsigned char>(character)) != 0;
}

std::string trim(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && is_space(text[start])) {
        ++start;
    }
    while (end > start && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

bool starts_with(std::string_view text, std::string_view prefix) {
    return text.size() >= prefix.size() && text.substr(0, prefix.size()) == prefix;
}

std::string replace_all(std::string text, std::string_view from,
                        std::string_view to) {
    std::size_t position = 0;
    while ((position = text.find(from, position)) != std::string::npos) {
        text.replace(position, from.size(), to);
        position += to.size();
    }
    return text;
}

/// APL's own number spelling: `¯` for negative, no trailing `.0`, and `%g`
/// otherwise -- the same six significant digits the reference prints.
std::string number_text(double value) {
    if (value == 0.0) {
        return "0";
    }
    char buffer[64];
    if (std::isfinite(value) && std::trunc(value) == value &&
        std::fabs(value) < 9.0e15) {
        std::snprintf(buffer, sizeof buffer, "%.0f", value);
    } else {
        std::snprintf(buffer, sizeof buffer, "%g", value);
    }
    return replace_all(std::string{buffer}, "-", "¯");
}

std::string display_text(const Value& value) {
    if (is_array(value)) {
        if (!value.items.empty() &&
            std::all_of(value.items.begin(), value.items.end(), is_array)) {
            std::string result;
            for (const Value& item : value.items) {
                if (!result.empty()) {
                    result.push_back('\n');
                }
                result += row_text(item);
            }
            return result;
        }
        return row_text(value);
    }
    if (value.kind == Value::Kind::Text) {
        return value.text;
    }
    if (value.kind == Value::Kind::Number) {
        return number_text(value.number);
    }
    return "None";
}

std::string row_text(const Value& value) {
    if (!is_array(value)) {
        return display_text(value);
    }
    std::string result;
    for (const Value& item : value.items) {
        if (!result.empty()) {
            result.push_back(' ');
        }
        if (is_array(item)) {
            result += "(" + row_text(item) + ")";
        } else {
            result += display_text(item);
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Arrays
// ---------------------------------------------------------------------------

std::vector<Value> items_of(const Value& value) {
    if (is_array(value)) {
        return value.items;
    }
    return {value};
}

std::vector<Value> raze(const Value& value) {
    if (!is_array(value)) {
        return {value};
    }
    std::vector<Value> result;
    for (const Value& item : value.items) {
        const std::vector<Value> inner = raze(item);
        result.insert(result.end(), inner.begin(), inner.end());
    }
    return result;
}

std::vector<double> shape_of(const Value& value) {
    if (!is_array(value)) {
        return {};
    }
    std::vector<double> shape{static_cast<double>(value.items.size())};
    if (!value.items.empty()) {
        const std::vector<double> inner = shape_of(value.items.front());
        shape.insert(shape.end(), inner.begin(), inner.end());
    }
    return shape;
}

Value from_flat(const std::vector<double>& shape, const std::vector<Value>& flat) {
    if (shape.empty()) {
        if (flat.empty()) {
            throw AplError("not enough elements for the shape");
        }
        return flat.front();
    }
    const std::size_t length = static_cast<std::size_t>(shape.front());
    if (length == 0) {
        return Value::array({});
    }
    if (flat.empty()) {
        throw AplError("not enough elements for the shape");
    }
    if (shape.size() == 1) {
        // Reshape repeats its data cyclically: 3⍴⍳2 is 1 2 1.
        std::vector<Value> result;
        result.reserve(length);
        for (std::size_t index = 0; index < length; ++index) {
            result.push_back(flat[index % flat.size()]);
        }
        return Value::array(std::move(result));
    }
    std::size_t total = 1;
    for (std::size_t index = 1; index < shape.size(); ++index) {
        total *= static_cast<std::size_t>(shape[index]);
    }
    if (total == 0) {
        std::vector<Value> rows;
        rows.reserve(length);
        for (std::size_t index = 0; index < length; ++index) {
            rows.push_back(Value::array({}));
        }
        return Value::array(std::move(rows));
    }
    const std::vector<double> rest(shape.begin() + 1, shape.end());
    std::vector<Value> rows;
    rows.reserve(length);
    for (std::size_t index = 0; index < length; ++index) {
        const std::size_t begin = index * total;
        const std::size_t end = std::min(begin + total, flat.size());
        std::vector<Value> slice;
        if (begin < flat.size()) {
            slice.assign(flat.begin() + static_cast<std::ptrdiff_t>(begin),
                         flat.begin() + static_cast<std::ptrdiff_t>(end));
        }
        rows.push_back(from_flat(rest, slice));
    }
    return Value::array(std::move(rows));
}

/// Pervasive application: at matching positions, keeping the array shape.
///
/// This is what "pervasive" means in APL, and why `1 2 3 + 10` and
/// `1 2 3 + 4 5 6` both work while `1 2 3 + 4 5` is a length error.
Value each(const std::function<Value(const std::vector<Value>&)>& function,
           const std::vector<Value>& values) {
    std::vector<const Value*> arrays;
    for (const Value& value : values) {
        if (is_array(value)) {
            arrays.push_back(&value);
        }
    }
    if (arrays.empty()) {
        return function(values);
    }
    const std::size_t length = arrays.front()->items.size();
    for (const Value* array : arrays) {
        if (array->items.size() != length) {
            std::string message = "length error: ";
            for (std::size_t index = 0; index < arrays.size(); ++index) {
                if (index != 0) {
                    message += " and ";
                }
                message += std::to_string(arrays[index]->items.size());
            }
            throw AplError(message);
        }
    }
    if (length == 0) {
        return Value::array({});
    }
    std::vector<Value> result;
    result.reserve(length);
    for (std::size_t index = 0; index < length; ++index) {
        std::vector<Value> arguments;
        arguments.reserve(values.size());
        for (const Value& value : values) {
            if (is_array(value)) {
                arguments.push_back(value.items[index]);
            } else {
                arguments.push_back(value);
            }
        }
        result.push_back(each(function, arguments));
    }
    return Value::array(std::move(result));
}

Value each1(const std::function<Value(const Value&)>& function, const Value& value) {
    return each(
        [&function](const std::vector<Value>& arguments) {
            return function(arguments.at(0));
        },
        {value});
}

Value each2(const std::function<Value(const Value&, const Value&)>& function,
            const Value& left, const Value& right) {
    return each(
        [&function](const std::vector<Value>& arguments) {
            return function(arguments.at(0), arguments.at(1));
        },
        {left, right});
}

double number_of(const Value& value) {
    if (!is_number(value)) {
        throw AplError("a number belongs here");
    }
    return value.number;
}

bool values_equal(const Value& left, const Value& right) {
    if (left.kind != right.kind) {
        return false;
    }
    switch (left.kind) {
        case Value::Kind::Number:
            return left.number == right.number;
        case Value::Kind::Text:
            return left.text == right.text;
        case Value::Kind::Array:
            if (left.items.size() != right.items.size()) {
                return false;
            }
            for (std::size_t index = 0; index < left.items.size(); ++index) {
                if (!values_equal(left.items[index], right.items[index])) {
                    return false;
                }
            }
            return true;
        case Value::Kind::Nothing:
            return true;
    }
    return false;
}

bool value_less(const Value& left, const Value& right) {
    if (is_number(left) && is_number(right)) {
        return left.number < right.number;
    }
    if (left.kind == Value::Kind::Text && right.kind == Value::Kind::Text) {
        return left.text < right.text;
    }
    throw AplError("cannot compare these values");
}

// ---------------------------------------------------------------------------
// Scalar functions
// ---------------------------------------------------------------------------

Value divide(const Value& left, const Value& right) {
    if (number_of(right) == 0.0) {
        throw AplError("division by zero");
    }
    return Value::numbered(number_of(left) / number_of(right));
}

/// `X | Y` is Y modulo X, and the result takes X's sign, as in APL.
Value residue(const Value& left, const Value& right) {
    const double x = number_of(left);
    const double y = number_of(right);
    if (x == 0.0) {
        return right;
    }
    if (y == 0.0) {
        return Value::numbered(0.0);
    }
    const double quotient =
        x * y > 0.0 ? std::floor(y / x) : std::ceil(y / x);
    return Value::numbered(y - x * quotient);
}

Value apply_canonical(const std::string& glyph, const Value* left,
                      const Value& right);

}  // namespace apl_detail
}  // namespace timewarp::lang
