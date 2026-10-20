// SPDX-License-Identifier: MIT

#include "timewarp/scheme/printer.hpp"

#include "timewarp/core/charconv.hpp"

#include <cmath>
#include <cstdint>
#include <string_view>
#include <vector>

namespace timewarp::scheme {
namespace {

/// What the reference prints for the unspecified value.
///
/// It actually prints `str(object())`, i.e. `<object object at 0x7f...>`, which
/// embeds a heap address and therefore differs on every run.  An address is not
/// reproducible information, so the port prints a stable marker instead; this
/// is a documented divergence.
constexpr std::string_view kUnspecifiedText = "#<unspecified>";

/// One unit of deferred work: either a value still to be rendered, or literal
/// text to append.  Every literal points at static storage, so tasks stay
/// trivially copyable and the stack never owns anything.
struct Task {
    enum class Kind : std::uint8_t { Value, Literal };
    Kind kind;
    Value value;
    std::string_view literal;
};

[[nodiscard]] Task value_task(const Value& value) {
    return Task{Task::Kind::Value, value, {}};
}

[[nodiscard]] Task literal_task(std::string_view text) {
    return Task{Task::Kind::Literal, {}, text};
}

void append_quoted(std::string& out, std::string_view text) {
    out.push_back('"');
    for (const char ch : text) {
        if (ch == '\\' || ch == '"') {
            out.push_back('\\');
        }
        out.push_back(ch);
    }
    out.push_back('"');
}

}  // namespace

void append_real(std::string& out, double number) {
    // Integral reals render as integers, matching `_default_float()`, which
    // does `str(int(fmt))`.  Python's `int()` is exact well beyond 64 bits, so
    // the digits are produced directly instead of going through an integer
    // cast: `1e30` must print all 31 digits, not `1e+30`.
    if (std::isfinite(number) && std::trunc(number) == number) {
        constexpr double kInt64Limit = 9223372036854775808.0;  // 2^63
        if (std::abs(number) < kInt64Limit) {
            out += std::to_string(static_cast<std::int64_t>(number));
            return;
        }
        // Zero fractional digits with a fixed format expands the value exactly;
        // 512 bytes covers the widest double (about 310 digits).
        char buffer[512];
        const std::size_t digits =
            timewarp::core::format_fixed_integer(buffer, buffer + sizeof(buffer), number);
        if (digits != 0) {
            out.append(buffer, digits);
            return;
        }
    }

    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof(buffer), number);
    out.append(buffer, length);
}

void write_value(std::string& out, const Value& value) {
    std::vector<Task> work;
    std::vector<Value> chain;

    work.push_back(value_task(value));

    while (!work.empty()) {
        const Task task = work.back();
        work.pop_back();

        if (task.kind == Task::Kind::Literal) {
            out += task.literal;
            continue;
        }

        const Value& current = task.value;
        switch (current.type) {
            case ValueType::Unspecified:
                out += kUnspecifiedText;
                break;
            case ValueType::Nil:
                out += "()";
                break;
            case ValueType::Boolean:
                out += current.boolean ? "#t" : "#f";
                break;
            case ValueType::Integer:
                out += std::to_string(current.integer);
                break;
            case ValueType::Real:
                append_real(out, current.real);
                break;
            case ValueType::Symbol:
                out += current.symbol->name;
                break;
            case ValueType::String:
                append_quoted(out, current.string->text);
                break;
            case ValueType::Procedure:
                out += "#<procedure ";
                out += current.procedure->name;
                out.push_back('>');
                break;
            case ValueType::Pair: {
                // Flatten the spine first so the tasks can be queued in one go.
                // Pushing in reverse makes the pop order the print order.
                chain.clear();
                const Value* cursor = &current;
                while (cursor->type == ValueType::Pair) {
                    chain.push_back(cursor->pair->car);
                    cursor = &cursor->pair->cdr;
                }

                out.push_back('(');
                work.push_back(literal_task(")"));
                if (cursor->type != ValueType::Nil) {
                    work.push_back(value_task(*cursor));
                    work.push_back(literal_task(" . "));
                }
                for (std::size_t index = chain.size(); index-- > 0;) {
                    work.push_back(value_task(chain[index]));
                    if (index > 0) {
                        work.push_back(literal_task(" "));
                    }
                }
                break;
            }
            case ValueType::Vector: {
                const std::vector<Value>& items = current.vector->items;
                out += "#(";
                work.push_back(literal_task(")"));
                for (std::size_t index = items.size(); index-- > 0;) {
                    work.push_back(value_task(items[index]));
                    if (index > 0) {
                        work.push_back(literal_task(" "));
                    }
                }
                break;
            }
        }
    }
}

void display_value(std::string& out, const Value& value) {
    if (value.type == ValueType::String) {
        out += value.string->text;
        return;
    }
    write_value(out, value);
}

std::string write_to_string(const Value& value) {
    std::string out;
    write_value(out, value);
    return out;
}

std::string display_to_string(const Value& value) {
    std::string out;
    display_value(out, value);
    return out;
}

}  // namespace timewarp::scheme
