// SPDX-License-Identifier: MIT
//
// A variable's value, as the engine holds it.
//
// Only the three kinds the ported languages actually need so far: a number, a
// string and "nothing". Arrays and records join this variant when the language
// that needs them is ported -- a type no executor can produce is a type whose
// rendering and comparison rules nobody has tested.

#pragma once

#include <string>
#include <variant>

namespace timewarp::lang {

class Value {
public:
    Value() = default;
    explicit Value(double number) : data_{number} {}
    explicit Value(std::string text) : data_{std::move(text)} {}

    [[nodiscard]] bool is_number() const { return std::holds_alternative<double>(data_); }
    [[nodiscard]] bool is_text() const { return std::holds_alternative<std::string>(data_); }
    [[nodiscard]] bool is_empty() const { return std::holds_alternative<std::monostate>(data_); }

    /// A number's value, or 0 for anything else -- the reference's answer for a
    /// variable a program never assigned. A text value that looks numeric is
    /// *not* converted here: the languages differ on whether that is allowed,
    /// and the executor is where that rule belongs.
    [[nodiscard]] double number() const {
        if (const auto* value = std::get_if<double>(&data_)) {
            return *value;
        }
        return 0.0;
    }

    [[nodiscard]] const std::string& text() const {
        static const std::string kEmpty;
        if (const auto* value = std::get_if<std::string>(&data_)) {
            return *value;
        }
        return kEmpty;
    }

    /// What the Variables panel calls this kind. The words are the learners',
    /// not Python's: these languages have numbers and text.
    [[nodiscard]] const char* type_name() const {
        if (is_number()) {
            return "number";
        }
        if (is_text()) {
            return "text";
        }
        return "empty";
    }

    /// How the value is shown. Integral numbers lose the `.0` an interpreter
    /// stores but a learner never wrote.
    [[nodiscard]] std::string render() const;

private:
    std::variant<std::monostate, double, std::string> data_;
};

}  // namespace timewarp::lang
