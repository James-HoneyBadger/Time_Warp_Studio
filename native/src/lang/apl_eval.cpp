// SPDX-License-Identifier: MIT
//
// Evaluation: operands, strands, the reduce and scan operators, user functions, and
// the statements that assign, define and print.

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

}  // namespace apl_detail
}  // namespace timewarp::lang
