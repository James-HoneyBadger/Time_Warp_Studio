// SPDX-License-Identifier: MIT

#include "basic_statements_internal.hpp"
//
// The BASIC statement dispatch, ported from `execute_basic` in
// `time_warp/languages/basic.py`.
//
// The order of the tests is behaviour, not style: `PRINT USING` has to be read
// before `PRINT`, `LINE INPUT` before `INPUT`, and the implicit-`LET` test
// before `COLOR`, or a statement silently becomes a different one. It is copied
// as it stands for that reason.

#include "basic_internal.hpp"

#include "timewarp/io/gamepad.hpp"
#include "timewarp/lang/logo.hpp"

#include <algorithm>
#include <thread>
#include <chrono>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <regex>
#include <string>
#include <utility>

namespace timewarp::lang::basic {

std::string Machine::let(const std::string& args) {
    const std::size_t equals = args.find('=');
    if (equals == std::string::npos) {
        return "❌ LET requires format: variable = expression\n";
    }
    const std::string var_name = upper(trim(args.substr(0, equals)));
    const std::string expr = trim(args.substr(equals + 1));
    const std::string expr_upper = upper(expr);
    if (var_name.empty()) {
        return "❌ LET requires variable name\n";
    }

    // Array element: NAME(index) = value
    if (var_name.find('(') != std::string::npos && var_name.back() == ')') {
        const std::size_t open = var_name.find('(');
        const std::string array_name = var_name.substr(0, open);
        const std::string index_expr = var_name.substr(open + 1, var_name.size() - open - 2);
        long index = 0;
        try {
            index = static_cast<long>(eval(index_expr));
        } catch (const std::exception& error) {
            return "❌ Invalid array index or value: " + std::string{error.what()} + "\n";
        }
        if (!array_name.empty() && array_name.back() == '$') {
            const auto array = state_.string_arrays.find(array_name);
            if (array != state_.string_arrays.end()) {
                std::string value;
                try {
                    Strings strings{interpreter_, state_, this};
                    value = strings.evaluate(expr);
                } catch (const BasicError&) {
                    value = expr;
                }
                if (index < 0 || static_cast<std::size_t>(index) >= array->second.size()) {
                    return "❌ Array index out of bounds: " + std::to_string(index) + "\n";
                }
                array->second[static_cast<std::size_t>(index)] = value;
                return {};
            }
        }
        try {
            const double value = eval(expr);
            const auto array = interpreter_.arrays.find(array_name);
            if (array == interpreter_.arrays.end()) {
                return "❌ Array not defined: " + array_name + "\n";
            }
            if (index < 0 || static_cast<std::size_t>(index) >= array->second.size()) {
                return "❌ Array index out of bounds: " + std::to_string(index) + "\n";
            }
            array->second[static_cast<std::size_t>(index)] = value;
            return {};
        } catch (const std::exception& error) {
            return "❌ Invalid array index or value: " + std::string{error.what()} + "\n";
        }
    }

    // Record field: VAR.FIELD = value
    if (var_name.find('.') != std::string::npos) {
        const std::size_t dot = var_name.find('.');
        const std::string object = var_name.substr(0, dot);
        const std::string field = var_name.substr(dot + 1);
        const auto record = state_.records.find(object);
        if (record == state_.records.end()) {
            return "❌ " + object + " is not a user-defined type instance\n";
        }
        if (!field.empty() && field.back() == '$') {
            record->second[field] = Value{strip_quotes(expr)};
        } else {
            double value = 0.0;
            try {
                value = eval(expr);
            } catch (const std::exception&) {
                value = 0.0;
            }
            record->second[field] = Value{value};
        }
        return {};
    }

    try {
        validate_name(var_name);
    } catch (const BasicError& error) {
        return "❌ " + std::string{error.what()} + "\n";
    }

    if (!var_name.empty() && var_name.back() == '$') {
        const bool simple_literal =
            expr.size() >= 2 && expr.front() == '"' && expr.back() == '"' &&
            expr.find('"', 1) == expr.size() - 1;
        if (simple_literal) {
            assign(var_name, expr.substr(1, expr.size() - 2));
        } else if (expr_upper == "INKEY$") {
            assign(var_name, key_text());
        } else if (expr_upper == "TIME$") {
            assign(var_name, clock_time_text());
        } else if (expr_upper == "DATE$") {
            assign(var_name, clock_date_text());
        } else {
            try {
                Strings strings{interpreter_, state_, this};
                assign(var_name, strings.evaluate(expr));
            } catch (const BasicError&) {
                assign(var_name, expr);
            }
        }
        return {};
    }

    if (expr_upper == "TIMER") {
        assign(var_name, clock_timer_seconds());
        return {};
    }

    if (!expr.empty() && expr.front() == '"') {
        try {
            Strings strings{interpreter_, state_, this};
            assign(var_name, strings.evaluate(expr));
        } catch (const BasicError&) {
            assign(var_name, strip_quotes(expr));
        }
        return {};
    }

    try {
        assign(var_name, eval(expr));
    } catch (const std::exception& thrown) {
        const std::string message = thrown.what();
        if (division_by_zero(message)) {
            if (divert_on_error()) {
                return {};
            }
            throw;
        }
        // A string function that returns a number (`LEN`, `INSTR`, `VAL`).
        try {
            Strings strings{interpreter_, state_, this};
            const std::string text = strings.evaluate(expr);
            double value = 0.0;
            if (parse_float(text, value)) {
                assign(var_name, value);
            } else {
                assign(var_name, text);
            }
        } catch (const BasicError&) {
            return "❌ Error in LET: " + message + " (expr: '" + expr + "')\n";
        }
    }
    return {};
}

std::string Machine::input(const std::string& args) {
    const std::string name = trim(args);
    std::string prompt = "? ";
    std::string var_name = upper(name);
    const std::size_t quote = args.find('"');
    if (quote != std::string::npos) {
        const std::size_t closing = args.find('"', quote + 1);
        if (closing != std::string::npos) {
            std::size_t after = closing + 1;
            if (after < args.size() && (args[after] == ';' || args[after] == ',')) {
                ++after;
            }
            while (after < args.size() && is_space(args[after])) {
                ++after;
            }
            if (after < args.size()) {
                prompt = args.substr(quote + 1, closing - quote - 1) + " ";
                var_name = upper(trim(args.substr(after)));
            }
        }
    }
    if (var_name.empty()) {
        return "❌ INPUT requires variable name\n";
    }
    try {
        validate_name(var_name);
    } catch (const BasicError& error) {
        return "❌ " + std::string{error.what()} + "\n";
    }
    const bool numeric = var_name.back() != '$';
    interpreter_.request_input(prompt, var_name, numeric);
    return {};
}

// -- control flow ----------------------------------------------------------

std::string Machine::conditional(const std::string& args) {
    const std::string args_upper = upper(args);
    std::size_t then_pos = args_upper.find(" THEN ");
    if (then_pos == std::string::npos) {
        const std::string trimmed = rstrip(args_upper);
        if (trimmed.size() >= 4 &&
            trimmed.compare(trimmed.size() - 4, 4, "THEN") == 0) {
            const std::size_t found = trimmed.rfind("THEN");
            then_pos = found == 0 ? std::string::npos : found - 1;
        }
    }
    if (then_pos == std::string::npos) {
        return "❌ IF requires THEN keyword\n";
    }
    const std::string condition = trim(args.substr(0, then_pos + 1));
    const std::string then_part =
        then_pos + 6 < args.size() ? trim(args.substr(then_pos + 6)) : std::string{};

    bool condition_true = false;
    const int verdict = try_string_comparison(*this, condition);
    if (verdict >= 0) {
        condition_true = verdict == 1;
    } else {
        try {
            condition_true = std::fabs(eval(condition)) > 0.0001;
        } catch (const std::exception& error) {
            const std::string message = error.what();
            if (division_by_zero(message)) {
                if (divert_on_error()) {
                    return {};
                }
                throw;
            }
            return "❌ Error in IF condition: " + message + "\n";
        }
    }

    if (then_part.empty()) {
        state_.ifs.push_back(IfFrame{!condition_true, 0});
        return {};
    }

    std::string else_part;
    std::string body = then_part;
    const std::string body_upper = upper(then_part);
    const std::size_t else_pos = body_upper.find(" ELSE ");
    if (else_pos != std::string::npos) {
        else_part = trim(then_part.substr(else_pos + 6));
        body = trim(then_part.substr(0, else_pos));
    }

    if (condition_true && !body.empty()) {
        if (is_all_digits(body)) {
            return goto_line(body);
        }
        return execute(body);
    }
    if (!condition_true && !else_part.empty()) {
        if (is_all_digits(else_part)) {
            return goto_line(else_part);
        }
        return execute(else_part);
    }
    return {};
}

std::string Machine::dim(const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        const std::string part = trim(raw);
        static const std::regex as_pattern{
            R"(^([A-Za-z_][A-Za-z0-9_]*\$?)\s+AS\s+([A-Za-z_][A-Za-z0-9_]*)$)",
            std::regex::ECMAScript | std::regex::icase};
        std::smatch match;
        if (std::regex_match(part, match, as_pattern)) {
            const std::string name = upper(match[1].str());
            const std::string type = upper(match[2].str());
            const auto fields = state_.types.find(type);
            state_.records[name] = fields == state_.types.end()
                                       ? std::map<std::string, Value>{}
                                       : fields->second;
            continue;
        }
        if (part.find('(') == std::string::npos || part.back() != ')') {
            return "❌ Invalid DIM syntax: " + part + "\n";
        }
        const std::string name = upper(trim(part.substr(0, part.find('('))));
        const std::string size_expr =
            part.substr(part.find('(') + 1, part.size() - part.find('(') - 2);
        try {
            validate_name(name);
            double size_value = 0.0;
            if (!parse_float(trim(size_expr), size_value)) {
                return "❌ Expected numeric value for array dimension, got '" +
                       trim(size_expr) + "'\n";
            }
            const auto size = static_cast<long>(std::trunc(eval(trim(size_expr))));
            if (size < 0) {
                return "❌ Array dimension must be non-negative: " + std::to_string(size) +
                       "\n";
            }
            if (size > 65535) {
                return "❌ Array too large: " + std::to_string(size) + " (max 65535)\n";
            }
            const auto length = static_cast<std::size_t>(size) + 1;
            if (!name.empty() && name.back() == '$') {
                state_.string_arrays[name] = std::vector<std::string>(length);
            } else {
                interpreter_.arrays[name] = std::vector<double>(length, 0.0);
            }
        } catch (const BasicError& error) {
            return "❌ " + std::string{error.what()} + "\n";
        } catch (const std::exception& error) {
            return "❌ Error in DIM " + name + ": " + std::string{error.what()} + "\n";
        }
    }
    return {};
}

std::string Machine::type_definition(const std::string& args) {
    const std::string name = upper(trim(args));
    if (name.empty()) {
        return "❌ TYPE requires a name\n";
    }
    std::map<std::string, Value> fields;
    const auto& lines = interpreter_.program_lines();
    for (std::size_t scan = interpreter_.current_line + 1; scan < lines.size(); ++scan) {
        const std::string text = trim(lines[scan].text);
        const std::string code = upper(text);
        if (code == "END TYPE" || code == "ENDTYPE") {
            interpreter_.jump_to(scan);
            break;
        }
        static const std::regex field_pattern{
            R"(^\s*([A-Za-z_][A-Za-z0-9_]*\$?)\s+AS\s+([A-Za-z_][A-Za-z0-9_]*)\s*$)",
            std::regex::ECMAScript};
        std::smatch match;
        if (std::regex_match(text, match, field_pattern)) {
            const std::string field = upper(match[1].str());
            const std::string type = upper(match[2].str());
            fields[field] = type == "STRING" ? Value{std::string{}} : Value{0.0};
        }
    }
    state_.types[name] = std::move(fields);
    return {};
}

std::string Machine::define_types(const std::string& type, const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        std::string part = upper(trim(raw));
        if (part.empty()) {
            continue;
        }
        const std::size_t dash = part.find('-');
        if (dash != std::string::npos) {
            const char first = part.front();
            const char last = part.back();
            for (char letter = first; letter <= last; ++letter) {
                if (letter >= 'A' && letter <= 'Z') {
                    state_.defaults[letter] = type;
                }
            }
        } else if (part.front() >= 'A' && part.front() <= 'Z') {
            state_.defaults[part.front()] = type;
        }
    }
    return {};
}

std::string Machine::constant(const std::string& args) {
    const std::size_t equals = args.find('=');
    if (equals == std::string::npos) {
        return "❌ CONST syntax error: expected '='\n";
    }
    const std::string name = upper(trim(args.substr(0, equals)));
    const std::string value_text = trim(args.substr(equals + 1));
    if (name.empty()) {
        return "❌ CONST requires a name\n";
    }
    if (!name.empty() && name.back() == '$') {
        if (value_text.size() >= 2 && value_text.front() == '"' && value_text.back() == '"') {
            interpreter_.set_text(name, value_text.substr(1, value_text.size() - 2));
        } else {
            interpreter_.set_text(name, interpreter_.interpolate_text(value_text));
        }
        return {};
    }
    try {
        interpreter_.set_number(name, eval(value_text));
    } catch (const std::exception&) {
        return "❌ CONST: cannot evaluate '" + value_text + "'\n";
    }
    return {};
}

std::string Machine::swap(const std::string& args) {
    const std::size_t comma = args.find(',');
    if (comma == std::string::npos) {
        return "❌ SWAP requires two variable names separated by a comma\n";
    }
    const std::string first = upper(trim(args.substr(0, comma)));
    const std::string second = upper(trim(args.substr(comma + 1)));
    const auto numeric = [this](const std::string& name) {
        const auto found = interpreter_.variables().find(name);
        return found != interpreter_.variables().end() && found->second.is_number();
    };
    const auto textual = [this](const std::string& name) {
        const auto found = interpreter_.variables().find(name);
        return found != interpreter_.variables().end() && found->second.is_text();
    };
    if (numeric(first) || numeric(second)) {
        const double a = number_of(first).value_or(0.0);
        const double b = number_of(second).value_or(0.0);
        interpreter_.set_number(first, b);
        interpreter_.set_number(second, a);
        return {};
    }
    if (textual(first) || textual(second)) {
        const std::string a = text_of(first);
        const std::string b = text_of(second);
        interpreter_.set_text(first, b);
        interpreter_.set_text(second, a);
        return {};
    }
    interpreter_.set_number(first, 0.0);
    interpreter_.set_number(second, 0.0);
    return {};
}

std::string Machine::erase(const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        const std::string name = upper(trim(raw));
        if (name.empty()) {
            continue;
        }
        const auto strings = state_.string_arrays.find(name);
        if (strings != state_.string_arrays.end()) {
            strings->second.assign(strings->second.size(), std::string{});
            continue;
        }
        const auto numbers = interpreter_.arrays.find(name);
        if (numbers != interpreter_.arrays.end()) {
            numbers->second.assign(numbers->second.size(), 0.0);
        }
    }
    return {};
}

std::string Machine::read_data(const std::string& args) {
    for (const std::string& raw : split_plain(args, ',')) {
        const std::string name = upper(trim(raw));
        if (state_.data_pointer >= state_.data.size()) {
            return "❌ Out of DATA\n";
        }
        const std::string value = state_.data[state_.data_pointer];
        ++state_.data_pointer;
        double number = 0.0;
        if (parse_float(value, number)) {
            assign(name, number);
        } else {
            assign(name, value);
        }
    }
    return {};
}

std::string Machine::restore() {
    state_.data_pointer = 0;
    return {};
}

}  // namespace timewarp::lang::basic
