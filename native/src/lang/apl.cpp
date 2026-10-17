// SPDX-License-Identifier: MIT
//
// APL, ported from `time_warp/languages/apl.py`.
//
// Two features make APL unlike anything else in the studio: there is **no
// operator precedence** -- a line is evaluated right to left -- and most
// functions are **pervasive**, so they work on whole arrays. `1 2 3 + 10` is
// `11 12 13`. The value of the port is that it behaves like the reference,
// which is why every message here is the reference's message.
//
// The subset is bounded and says so: reduce and scan work along the outermost
// axis, there is no rank operator and no system function, and the turtle words
// (FD/BK/RT/LT/PU/PD/HOME/CS plus the read-only TX/TY/THETA) are this
// project's extension rather than APL's.

#include "timewarp/lang/apl.hpp"

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace {

class AplError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------

/// A number, a string, or a (possibly nested) array of them.
struct Value {
    enum class Kind { Number, Text, Array, Nothing };

    Kind kind = Kind::Number;
    double number = 0.0;
    std::string text;
    std::vector<Value> items;

    static Value numbered(double value) {
        Value result;
        result.number = value;
        return result;
    }
    static Value word(std::string value) {
        Value result;
        result.kind = Kind::Text;
        result.text = std::move(value);
        return result;
    }
    static Value array(std::vector<Value> values) {
        Value result;
        result.kind = Kind::Array;
        result.items = std::move(values);
        return result;
    }
    static Value nothing() {
        Value result;
        result.kind = Kind::Nothing;
        return result;
    }
};

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

std::string row_text(const Value& value);

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

// ---------------------------------------------------------------------------
// Function tables
// ---------------------------------------------------------------------------

const std::map<std::string, std::string>& aliases() {
    static const std::map<std::string, std::string> table = {
        {"PLUS", "+"},         {"MINUS", "-"},      {"TIMES", "×"},
        {"DIVIDE", "÷"},       {"POWER", "*"},      {"LOG", "⍟"},
        {"CEILING", "⌈"},      {"FLOOR", "⌊"},      {"MAX", "⌈"},
        {"MIN", "⌊"},          {"ABS", "|"},        {"RESIDUE", "|"},
        {"SIGNUM", "×"},       {"RECIPROCAL", "÷"}, {"RHO", "⍴"},
        {"SHAPE", "⍴"},        {"RESHAPE", "⍴"},    {"IOTA", "⍳"},
        {"INDEXOF", "⍳"},      {"REVERSE", "⌽"},    {"ROTATE", "⌽"},
        {"TAKE", "↑"},         {"DROP", "↓"},       {"TRANSPOSE", "⍉"},
        {"RAVEL", ","},        {"CATENATE", ","},   {"MEMBERSHIP", "∈"},
        {"NOT", "~"},          {"AND", "∧"},        {"OR", "∨"},
        {"NAND", "⍲"},         {"NOR", "⍱"},        {"EQUAL", "="},
        {"NOTEQUAL", "≠"},     {"LESS", "<"},       {"LESSEQ", "≤"},
        {"GREATER", ">"},      {"GREATEREQ", "≥"},  {"GRADEUP", "⍋"},
        {"GRADEDOWN", "⍒"},    {"FORMAT", "⍕"},     {"ROLL", "?"},
        {"REDUCE", "/"},       {"SCAN", "\\"},
    };
    return table;
}

const std::vector<std::string>& structural() {
    static const std::vector<std::string> table = {
        "⍳", "⍴", "⌽", "↑", "↓", "⍉", ",", "∈", "⍋", "⍒", "⍕"};
    return table;
}

const std::vector<std::string>& scalars() {
    static const std::vector<std::string> table = {
        "+", "-", "×", "÷", "*", "⍟", "⌈", "⌊", "|", "=", "≠", "<",
        "≤", ">", "≥", "∧", "∨", "⍲", "⍱", "~", "?"};
    return table;
}

bool in_list(const std::vector<std::string>& list, std::string_view value) {
    return std::find(list.begin(), list.end(), value) != list.end();
}

const std::map<std::string, std::string>& turtle_words() {
    static const std::map<std::string, std::string> table = {
        {"FD", "FD"},   {"FORWARD", "FD"}, {"BK", "BK"},  {"BACK", "BK"},
        {"RT", "RT"},   {"RIGHT", "RT"},   {"LT", "LT"},  {"LEFT", "LT"},
        {"PU", "PU"},   {"PENUP", "PU"},   {"PD", "PD"},  {"PENDOWN", "PD"},
        {"HOME", "HOME"}, {"CS", "CS"},    {"CLEAR", "CS"}};
    return table;
}

std::string canonical(std::string_view token) {
    const std::string shouted = upper(token);
    const auto found = aliases().find(shouted);
    return found == aliases().end() ? shouted : found->second;
}

std::string format_value(const Value& value) { return display_text(value); }

Value apply_canonical(const std::string& glyph, const Value* left,
                      const Value& right) {
    if (in_list(structural(), glyph)) {
        if (glyph == "⍳") {
            return left == nullptr ? iota(right) : index_of(*left, right);
        }
        if (glyph == "⍴") {
            return left == nullptr ? Value::array([&right]() {
                std::vector<Value> shape;
                for (double size : shape_of(right)) {
                    shape.push_back(Value::numbered(size));
                }
                return shape;
            }())
                                   : reshape(*left, right);
        }
        if (glyph == "⌽") {
            return left == nullptr ? reverse(right) : rotate(*left, right);
        }
        if (glyph == "↑") {
            if (left == nullptr) {
                throw AplError("↑ has no monadic form");
            }
            return take(*left, right);
        }
        if (glyph == "↓") {
            if (left == nullptr) {
                throw AplError("↓ has no monadic form");
            }
            return drop(*left, right);
        }
        if (glyph == "⍉") {
            if (left != nullptr) {
                throw AplError("⍉ has no dyadic form");
            }
            return transpose(right);
        }
        if (glyph == ",") {
            return left == nullptr ? Value::array(raze(right)) : catenate(*left, right);
        }
        if (glyph == "∈") {
            if (left == nullptr) {
                throw AplError("∈ has no monadic form");
            }
            return membership(*left, right);
        }
        if (glyph == "⍋") {
            if (left != nullptr) {
                throw AplError("⍋ has no dyadic form");
            }
            return grade(right, false);
        }
        if (glyph == "⍒") {
            if (left != nullptr) {
                throw AplError("⍒ has no dyadic form");
            }
            return grade(right, true);
        }
        if (glyph == "⍕") {
            if (left != nullptr) {
                throw AplError("⍕ has no dyadic form");
            }
            return Value::word(format_value(right));
        }
    }

    if (!in_list(scalars(), glyph)) {
        throw AplError(glyph + " is not a function this build knows");
    }

    const auto monad = [&](const std::function<Value(const Value&)>& function) {
        return each1(function, right);
    };
    const auto dyad = [&](const std::function<Value(const Value&, const Value&)>& function) {
        return each2(function, *left, right);
    };

    if (glyph == "+") {
        if (left == nullptr) {
            throw AplError("+ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) + number_of(b));
        });
    }
    if (glyph == "-") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(-number_of(value));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) - number_of(b));
        });
    }
    if (glyph == "×") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                const double number = number_of(value);
                return Value::numbered(number > 0 ? 1 : (number < 0 ? -1 : 0));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) * number_of(b));
        });
    }
    if (glyph == "÷") {
        if (left == nullptr) {
            if (number_of(right) == 0.0) {
                throw AplError("reciprocal of zero");
            }
            return Value::numbered(1.0 / number_of(right));
        }
        return dyad(divide);
    }
    if (glyph == "*") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::exp(number_of(value)));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::pow(number_of(a), number_of(b)));
        });
    }
    if (glyph == "⍟") {
        if (left == nullptr) {
            if (number_of(right) <= 0.0) {
                throw AplError("log of a non-positive");
            }
            return Value::numbered(std::log(number_of(right)));
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::log(number_of(b)) / std::log(number_of(a)));
        });
    }
    if (glyph == "⌈") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::ceil(number_of(value)));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::max(number_of(a), number_of(b)));
        });
    }
    if (glyph == "⌊") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::floor(number_of(value)));
            });
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(std::min(number_of(a), number_of(b)));
        });
    }
    if (glyph == "|") {
        if (left == nullptr) {
            return monad([](const Value& value) {
                return Value::numbered(std::fabs(number_of(value)));
            });
        }
        return dyad(residue);
    }
    if (glyph == "=") {
        if (left == nullptr) {
            throw AplError("= has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) == number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "≠") {
        if (left == nullptr) {
            throw AplError("≠ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) != number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "<") {
        if (left == nullptr) {
            throw AplError("< has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) < number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "≤") {
        if (left == nullptr) {
            throw AplError("≤ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) <= number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == ">") {
        if (left == nullptr) {
            throw AplError("> has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) > number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "≥") {
        if (left == nullptr) {
            throw AplError("≥ has no monadic form");
        }
        return dyad([](const Value& a, const Value& b) {
            return Value::numbered(number_of(a) >= number_of(b) ? 1.0 : 0.0);
        });
    }
    if (glyph == "∧" || glyph == "∨" || glyph == "⍲" || glyph == "⍱") {
        if (left == nullptr) {
            throw AplError(glyph + " has no monadic form");
        }
        return dyad([glyph](const Value& a, const Value& b) {
            const bool first = number_of(a) != 0.0;
            const bool second = number_of(b) != 0.0;
            bool answer = false;
            if (glyph == "∧") {
                answer = first && second;
            } else if (glyph == "∨") {
                answer = first || second;
            } else if (glyph == "⍲") {
                answer = !(first && second);
            } else {
                answer = !(first || second);
            }
            return Value::numbered(answer ? 1.0 : 0.0);
        });
    }
    if (glyph == "~") {
        if (left != nullptr) {
            throw AplError("~ has no dyadic form");
        }
        return monad([](const Value& value) {
            return Value::numbered(number_of(value) == 0.0 ? 1.0 : 0.0);
        });
    }
    if (glyph == "?") {
        if (left == nullptr) {
            const long long bound = static_cast<long long>(number_of(right));
            if (bound < 1) {
                throw AplError("ROLL needs a positive count");
            }
            return Value::numbered(
                static_cast<double>(1 + std::rand() % bound));
        }
        const long long count = static_cast<long long>(number_of(*left));
        const long long bound = static_cast<long long>(number_of(right));
        if (bound < 1) {
            throw AplError("ROLL needs a positive count");
        }
        std::vector<Value> result;
        result.reserve(static_cast<std::size_t>(std::max(0LL, count)));
        for (long long index = 0; index < count; ++index) {
            result.push_back(Value::numbered(
                static_cast<double>(1 + std::rand() % bound)));
        }
        return Value::array(std::move(result));
    }

    throw AplError(glyph + " is not a function this build knows");
}

// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------

const std::vector<std::string>& glyphs() {
    static const std::vector<std::string> table = {
        "⍳", "⍴", "⌽", "↑", "↓", "⍉", "∈", "∧", "∨", "⍲", "⍱", "≠",
        "≤", "≥", "⍋", "⍒", "⍕", "←", "⍝", "+",  "-",  "×",  "÷",  "*",
        "⍟", "⌈", "⌊", "|",  ",",  "~",  "=",  "<",  ">",  "?",  "/",
        "\\", "(", ")"};
    return table;
}

bool is_digit(char character) {
    return std::isdigit(static_cast<unsigned char>(character)) != 0;
}

bool is_name_start(char character) {
    return std::isalpha(static_cast<unsigned char>(character)) != 0 ||
           character == '_';
}

bool is_name_char(char character) {
    return std::isalnum(static_cast<unsigned char>(character)) != 0 ||
           character == '_';
}

std::vector<std::string> tokenize(std::string_view text) {
    std::vector<std::string> tokens;
    std::size_t index = 0;
    while (index < text.size()) {
        const char character = text[index];
        if (is_space(character)) {
            ++index;
            continue;
        }
        if (character == '\'' || character == '"') {
            const std::size_t closing = text.find(character, index + 1);
            if (closing == std::string_view::npos) {
                throw AplError("unterminated string");
            }
            tokens.push_back(std::string{text.substr(index, closing - index + 1)});
            index = closing + 1;
            continue;
        }
        const bool negative = starts_with(text.substr(index), "¯");
        if (negative || is_digit(character) ||
            (character == '.' && index + 1 < text.size() && is_digit(text[index + 1]))) {
            const std::size_t start = index;
            if (negative) {
                index += 2;  // ¯ is two bytes in UTF-8
            }
            while (index < text.size() &&
                   (is_digit(text[index]) || text[index] == '.')) {
                ++index;
            }
            if (index < text.size() && (text[index] == 'e' || text[index] == 'E')) {
                ++index;
                if (index < text.size() && starts_with(text.substr(index), "¯")) {
                    index += 2;
                }
                while (index < text.size() && is_digit(text[index])) {
                    ++index;
                }
            }
            tokens.push_back(std::string{text.substr(start, index - start)});
            continue;
        }
        if (is_name_start(character)) {
            const std::size_t start = index;
            while (index < text.size() && is_name_char(text[index])) {
                ++index;
            }
            tokens.push_back(upper(text.substr(start, index - start)));
            continue;
        }
        const std::string_view rest = text.substr(index);
        std::string two;
        for (const char* spelling : {">=", "<=", "!=", "=="}) {
            if (starts_with(rest, spelling)) {
                two = spelling;
                break;
            }
        }
        if (!two.empty()) {
            tokens.push_back(two == ">="   ? "≥"
                             : two == "<=" ? "≤"
                             : two == "!=" ? "≠"
                                           : "=");
            index += 2;
            continue;
        }
        bool matched = false;
        for (const std::string& glyph : glyphs()) {
            if (starts_with(rest, glyph)) {
                tokens.push_back(glyph);
                index += glyph.size();
                matched = true;
                break;
            }
        }
        if (matched) {
            continue;
        }
        throw AplError("cannot read '" + std::string(1, character) + "'");
    }
    return tokens;
}

// ---------------------------------------------------------------------------
// Run state
// ---------------------------------------------------------------------------

struct Header {
    std::string name;
    std::vector<std::string> parameters;
    std::string result;
};

struct Function {
    std::vector<std::string> parameters;
    std::vector<std::string> body;
};

struct Definition {
    Header header;
    std::vector<std::string> body;
};

struct AplState {
    std::map<std::string, Value> variables;
    std::map<std::string, Function> functions;
    std::optional<Definition> defining;
    bool quiet = false;
    /// The reference reads `turtle.x` and `turtle.heading`; the host has no
    /// readback, so the pose is mirrored here with the same maths the hosts
    /// use. `TX`, `TY` and `THETA` report it.
    TurtlePose pose;
};

bool is_function(const std::string& token, const AplState* state) {
    const std::string shouted = upper(token);
    if (turtle_words().count(shouted) != 0) {
        return true;
    }
    const std::string plain = canonical(token);
    if (in_list(scalars(), plain) || in_list(structural(), plain)) {
        return true;
    }
    return state != nullptr && state->functions.count(shouted) != 0;
}

bool is_operand(const std::string& token, const AplState* state) {
    if (!token.empty() && (token.front() == '\'' || token.front() == '"')) {
        return true;
    }
    if (starts_with(token, "¯") || token.front() == '.' || is_digit(token.front())) {
        return true;
    }
    if (token == "(") {
        return false;
    }
    if (token == ")") {
        return true;
    }
    if (is_function(token, state)) {
        return false;
    }
    if (in_list(glyphs(), token) || token == "←") {
        return false;
    }
    return true;
}

Value value_of(const std::string& token, const std::map<std::string, Value>& variables) {
    if (!token.empty() && (token.front() == '\'' || token.front() == '"')) {
        return Value::word(token.substr(1, token.size() - 2));
    }
    if (starts_with(token, "¯") || is_digit(token.front()) || token.front() == '.') {
        std::string number = replace_all(token, "¯", "-");
        if (number.find('e') != std::string::npos ||
            number.find('E') != std::string::npos) {
            return Value::numbered(std::strtod(number.c_str(), nullptr));
        }
        if (number.find('.') != std::string::npos) {
            return Value::numbered(std::strtod(number.c_str(), nullptr));
        }
        return Value::numbered(std::strtod(number.c_str(), nullptr));
    }
    const auto found = variables.find(token);
    if (found == variables.end()) {
        return Value::nothing();
    }
    return found->second;
}

// ---------------------------------------------------------------------------
// Evaluation
// ---------------------------------------------------------------------------

Value evaluate(const std::vector<std::string>& tokens, AplState& state,
               TurtleHost* turtle);

std::pair<Value, int> operand(const std::vector<std::string>& tokens, int index,
                              AplState& state, TurtleHost* turtle) {
    const std::string token = tokens[static_cast<std::size_t>(index)];
    if (token == ")") {
        int depth = 0;
        int start = index;
        while (index >= 0) {
            if (tokens[static_cast<std::size_t>(index)] == ")") {
                ++depth;
            } else if (tokens[static_cast<std::size_t>(index)] == "(") {
                --depth;
                if (depth == 0) {
                    break;
                }
            }
            --index;
        }
        if (index < 0) {
            throw AplError("unmatched )");
        }
        const std::vector<std::string> inner(
            tokens.begin() + index + 1,
            tokens.begin() + start);
        return {evaluate(inner, state, turtle), index - 1};
    }
    if (token == "(") {
        throw AplError("unmatched (");
    }
    const Value value = value_of(token, state.variables);
    if (value.kind == Value::Kind::Nothing && !is_function(token, &state) &&
        is_operand(token, &state)) {
        throw AplError(token + " has no value");
    }
    return {value, index - 1};
}

std::pair<Value, int> strand(const std::vector<std::string>& tokens, int index,
                             AplState& state, TurtleHost* turtle) {
    std::vector<Value> items;
    while (index >= 0 &&
           is_operand(tokens[static_cast<std::size_t>(index)], &state)) {
        auto [value, next] = operand(tokens, index, state, turtle);
        items.push_back(value);
        index = next;
    }
    if (items.empty()) {
        throw AplError("an operand is missing");
    }
    if (items.size() == 1) {
        return {items.front(), index};
    }
    std::reverse(items.begin(), items.end());
    return {Value::array(std::move(items)), index};
}

Value apply(const std::string& token, const Value* left, const Value& right,
            AplState& state, TurtleHost* turtle);

Value expression(const std::vector<std::string>& tokens, AplState& state,
                 TurtleHost* turtle) {
    if (tokens.empty()) {
        throw AplError("there is no expression here");
    }
    int index = static_cast<int>(tokens.size()) - 1;
    Value value;
    const std::string last = upper(tokens.back());
    const auto word = turtle_words().find(last);
    const bool niladic = word != turtle_words().end() &&
                         (word->second == "PU" || word->second == "PD" ||
                          word->second == "HOME" || word->second == "CS");
    if (niladic) {
        value = apply(tokens.back(), nullptr, Value::nothing(), state, turtle);
        --index;
    } else {
        auto [first, next] = strand(tokens, index, state, turtle);
        value = first;
        index = next;
    }

    while (index >= 0) {
        const std::string token = tokens[static_cast<std::size_t>(index)];
        if (token == "(") {
            throw AplError("unmatched (");
        }
        if (!is_function(token, &state)) {
            throw AplError(token +
                           " is not a function, but a function belongs here");
        }
        if (index - 1 >= 0 &&
            is_operand(tokens[static_cast<std::size_t>(index - 1)], &state)) {
            auto [left, next] = strand(tokens, index - 1, state, turtle);
            value = apply(token, &left, value, state, turtle);
            index = next;
        } else {
            value = apply(token, nullptr, value, state, turtle);
            --index;
        }
    }
    return value;
}

Value reduce(const std::string& operation, const Value& right) {
    const std::vector<Value> items = items_of(right);
    if (items.empty()) {
        return Value::numbered(0.0);
    }
    Value total = items.front();
    for (std::size_t index = 1; index < items.size(); ++index) {
        total = apply_canonical(operation, &total, items[index]);
    }
    return total;
}

Value scan(const std::string& operation, const Value& right) {
    const std::vector<Value> items = items_of(right);
    if (items.empty()) {
        return Value::array({});
    }
    std::vector<Value> result{items.front()};
    for (std::size_t index = 1; index < items.size(); ++index) {
        result.push_back(apply_canonical(operation, &result.back(), items[index]));
    }
    return Value::array(std::move(result));
}

std::optional<std::tuple<std::string, std::size_t, std::string>> split_operator(
    const std::vector<std::string>& tokens) {
    int depth = 0;
    for (std::size_t index = 0; index < tokens.size(); ++index) {
        if (tokens[index] == "(") {
            ++depth;
            continue;
        }
        if (tokens[index] == ")") {
            --depth;
            continue;
        }
        if (depth != 0) {
            continue;
        }
        const std::string kind = canonical(tokens[index]);
        if (kind != "/" && kind != "\\") {
            continue;
        }
        if (index == 0) {
            throw AplError("an operator needs a function on its left, as in +/");
        }
        return std::make_tuple(canonical(tokens[index - 1]), index, kind);
    }
    return std::nullopt;
}

Value call_user_function(const std::string& name, const Value* left,
                         const Value& right, AplState& state,
                         TurtleHost* turtle);

Value evaluate(const std::vector<std::string>& tokens, AplState& state,
               TurtleHost* turtle) {
    const auto operator_function = split_operator(tokens);
    if (!operator_function.has_value()) {
        return expression(tokens, state, turtle);
    }
    const auto& [glyph, index, kind] = *operator_function;
    if (!in_list(scalars(), glyph)) {
        throw AplError(glyph + " cannot be used with " + kind);
    }
    const std::vector<std::string> rest(tokens.begin() + index + 1, tokens.end());
    const Value right = evaluate(rest, state, turtle);
    return kind == "/" ? reduce(glyph, right) : scan(glyph, right);
}

Value apply(const std::string& token, const Value* left, const Value& right,
            AplState& state, TurtleHost* turtle) {
    const std::string shouted = upper(token);
    const auto word = turtle_words().find(shouted);
    if (word != turtle_words().end()) {
        state.quiet = true;
        const std::string key = word->second;
        if (key == "PU") {
            if (turtle != nullptr) {
                turtle->pen_up();
            }
            return Value::numbered(0.0);
        }
        if (key == "PD") {
            if (turtle != nullptr) {
                turtle->pen_down();
            }
            return Value::numbered(0.0);
        }
        if (key == "HOME") {
            if (turtle != nullptr) {
                turtle->home();
            }
            state.pose.home();
            return Value::numbered(0.0);
        }
        if (key == "CS") {
            if (turtle != nullptr) {
                turtle->clear();
            }
            return Value::numbered(0.0);
        }
        const Value* amount = left != nullptr ? left : &right;
        if (amount->kind == Value::Kind::Nothing) {
            throw AplError(key + " needs a distance or angle");
        }
        const double distance = number_of(*amount);
        if (turtle != nullptr) {
            if (key == "FD") {
                turtle->forward(distance);
            } else if (key == "BK") {
                turtle->back(distance);
            } else if (key == "RT") {
                turtle->right(distance);
            } else {
                turtle->left(distance);
            }
        }
        if (key == "FD") {
            state.pose.move(distance);
        } else if (key == "BK") {
            state.pose.move(-distance);
        } else if (key == "RT") {
            state.pose.turn(distance);
        } else {
            state.pose.turn(-distance);
        }
        return Value::numbered(0.0);
    }

    if (state.functions.count(shouted) != 0) {
        return call_user_function(shouted, left, right, state, turtle);
    }

    const std::string plain = canonical(token);
    if (plain == "/" || plain == "\\" || token == "/" || token == "\\") {
        throw AplError("an operator needs a function on its left, as in +/");
    }
    return apply_canonical(plain, left, right);
}

Value call_user_function(const std::string& name, const Value* left,
                         const Value& right, AplState& state,
                         TurtleHost* turtle) {
    const Function entry = state.functions[name];
    AplState inner;
    inner.variables = state.variables;
    inner.functions = state.functions;
    if (entry.parameters.size() == 1) {
        inner.variables[entry.parameters[0]] = right;
    } else if (entry.parameters.size() == 2) {
        if (left == nullptr) {
            throw AplError(name + " takes one or two arguments");
        }
        inner.variables[entry.parameters[0]] = *left;
        inner.variables[entry.parameters[1]] = right;
    } else {
        throw AplError(name + " takes one or two arguments");
    }

    Value result = Value::nothing();
    for (const std::string& line : entry.body) {
        const auto [value, assigned] = [&]() -> std::pair<Value, bool> {
            const std::string text = trim(line);
            if (text.empty() || starts_with(text, "⍝")) {
                return {Value::nothing(), true};
            }
            const std::vector<std::string> tokens = tokenize(text);
            if (tokens.size() >= 2 && tokens[1] == "←") {
                const std::string target = upper(tokens[0]);
                const std::vector<std::string> rest(tokens.begin() + 2, tokens.end());
                const Value computed = evaluate(rest, inner, turtle);
                inner.variables[target] = computed;
                return {computed, true};
            }
            return {evaluate(tokens, inner, turtle), false};
        }();
        (void)assigned;
        result = value;
    }
    return result;
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

std::pair<Value, bool> statement(const std::string& line, AplState& state,
                                 TurtleHost* turtle) {
    const std::size_t comment = line.find("⍝");
    const std::string text = trim(comment == std::string::npos
                                      ? std::string_view{line}
                                      : std::string_view{line}.substr(0, comment));
    if (text.empty()) {
        return {Value::nothing(), true};
    }
    if (starts_with(text, "∇")) {
        throw AplError("a function definition cannot be nested inside another");
    }

    const std::vector<std::string> tokens = tokenize(text);
    if (tokens.size() >= 2 && tokens[1] == "←") {
        const std::string name = upper(tokens[0]);
        bool valid = !name.empty();
        for (char character : name) {
            if (character != '_' && !std::isalnum(static_cast<unsigned char>(character))) {
                valid = false;
            }
        }
        if (!valid) {
            throw AplError(tokens[0] + " is not a name");
        }
        const std::vector<std::string> rest(tokens.begin() + 2, tokens.end());
        Value value = evaluate(rest, state, turtle);
        state.variables[name] = value;
        return {value, true};
    }
    return {evaluate(tokens, state, turtle), false};
}

Header parse_header(const std::string& text) {
    if (text.empty()) {
        throw AplError("a definition needs a header, as in ∇ R ← DOUBLE X");
    }
    Header header;
    std::string rest = text;
    const std::size_t arrow = rest.find("←");
    if (arrow != std::string::npos) {
        header.result = upper(trim(rest.substr(0, arrow)));
        rest = trim(rest.substr(arrow + std::string("←").size()));
    }
    std::vector<std::string> words;
    std::size_t index = 0;
    while (index < rest.size()) {
        while (index < rest.size() && is_space(rest[index])) {
            ++index;
        }
        const std::size_t start = index;
        while (index < rest.size() && !is_space(rest[index])) {
            ++index;
        }
        if (index > start) {
            words.push_back(rest.substr(start, index - start));
        }
    }
    if (words.empty()) {
        throw AplError("a definition needs a function name");
    }
    if (words.size() == 1) {
        header.name = upper(words[0]);
    } else if (words.size() == 2) {
        header.name = upper(words[0]);
        header.parameters.push_back(upper(words[1]));
    } else if (words.size() == 3) {
        header.name = upper(words[1]);
        header.parameters.push_back(upper(words[0]));
        header.parameters.push_back(upper(words[2]));
    } else {
        throw AplError("one or two arguments is the limit");
    }
    return header;
}

double round6(double value) { return std::round(value * 1e6) / 1e6; }

Value turtle_state_value(double raw) {
    const double rounded = round6(raw);
    return Value::numbered(rounded);
}

}  // namespace

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

std::string execute_apl(Interpreter& interpreter, std::string_view source,
                        TurtleHost& turtle) {
    AplState& state = interpreter.executor_state<AplState>();
    // The turtle's position and heading, under names that cannot collide with a
    // program's own: X, Y and H are ordinary APL variables.
    state.variables["TX"] = turtle_state_value(state.pose.x());
    state.variables["TY"] = turtle_state_value(state.pose.y());
    state.variables["THETA"] = turtle_state_value(state.pose.heading());

    const std::string text = trim(source);

    if (state.defining.has_value()) {
        if (text == "∇") {
            const Definition definition = std::move(*state.defining);
            state.defining.reset();
            if (definition.body.empty()) {
                return "❌ APL: a function needs a body between the ∇ lines\n";
            }
            state.functions[definition.header.name] =
                Function{definition.header.parameters, definition.body};
            return "✅ APL: " + definition.header.name + " defined\n";
        }
        if (starts_with(text, "∇")) {
            state.defining.reset();
            return "❌ APL: a definition was already open\n";
        }
        state.defining->body.push_back(trim(source));
        return "";
    }

    if (starts_with(text, "∇")) {
        try {
            const std::string header_text =
                trim(std::string_view{text}.substr(std::string("∇").size()));
            state.defining = Definition{parse_header(header_text), {}};
        } catch (const AplError& error) {
            return std::string{"❌ APL: "} + error.what() + "\n";
        }
        return "";
    }

    {
        const std::size_t comment = text.find("⍝");
        const std::string code =
            trim(comment == std::string::npos
                     ? std::string_view{text}
                     : std::string_view{text}.substr(0, comment));
        if (code.empty()) {
            return "";
        }
    }

    try {
        auto [value, assigned] = statement(text, state, &turtle);
        if (assigned) {
            return "";
        }
        if (state.quiet) {
            state.quiet = false;
            return "";
        }
        return display_text(value) + "\n";
    } catch (const AplError& error) {
        return std::string{"❌ APL: "} + error.what() + "\n";
    } catch (const std::exception& error) {
        return std::string{"❌ APL: "} + error.what() + "\n";
    }
}

}  // namespace timewarp::lang
