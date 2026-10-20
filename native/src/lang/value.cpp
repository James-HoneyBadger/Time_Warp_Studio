// SPDX-License-Identifier: MIT

#include "timewarp/lang/value.hpp"

#include "timewarp/core/charconv.hpp"

#include <charconv>
#include <cmath>
#include <cstdio>

namespace timewarp::lang {

std::string Value::render() const {
    if (is_empty()) {
        return "empty";
    }
    if (const auto* text = std::get_if<std::string>(&data_)) {
        return *text;
    }
    const double number = std::get<double>(data_);
    if (std::isnan(number)) {
        return "nan";
    }
    if (std::isinf(number)) {
        return number < 0 ? "-inf" : "inf";
    }
    // An integral value reads as the integer the program wrote. BASIC stores
    // every number as a float; showing `42.0` is a statement about the
    // interpreter, not about the learner's program.
    if (std::trunc(number) == number && std::abs(number) < 1e15) {
        // Negative zero is a real value a program can produce (`0 * SIN x` with
        // x past pi), and the reference prints it with its sign. Rounding it to
        // `0` would be a difference the panels could not explain.
        if (number == 0.0 && std::signbit(number)) {
            return "-0";
        }
        char buffer[32];
        const auto result =
            std::to_chars(buffer, buffer + sizeof buffer, static_cast<long long>(number));
        return std::string(buffer, result.ptr);
    }
    char buffer[64];
    const std::size_t length =
        timewarp::core::format_double(buffer, buffer + sizeof buffer, number);
    return std::string(buffer, length);
}

}  // namespace timewarp::lang
