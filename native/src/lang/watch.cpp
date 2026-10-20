// SPDX-License-Identifier: MIT

#include "timewarp/lang/watch.hpp"

#include <algorithm>
#include <cctype>
#include <exception>

#include "timewarp/expr/evaluator.hpp"

namespace timewarp::lang {

namespace {

std::string trimmed(std::string_view text) {
    std::size_t begin = 0;
    while (begin < text.size() &&
           std::isspace(static_cast<unsigned char>(text[begin])) != 0) {
        ++begin;
    }
    std::size_t end = text.size();
    while (end > begin &&
           std::isspace(static_cast<unsigned char>(text[end - 1])) != 0) {
        --end;
    }
    return std::string{text.substr(begin, end - begin)};
}

std::string upper(std::string_view text) {
    std::string result;
    result.reserve(text.size());
    for (const char character : text) {
        result.push_back(static_cast<char>(
            std::toupper(static_cast<unsigned char>(character))));
    }
    return result;
}

/// The variable whose name is `expression`, case-insensitively, or null.
const Value* bare_variable(std::string_view expression,
                           const std::map<std::string, Value>& variables) {
    const std::string wanted = upper(expression);
    for (const auto& [name, value] : variables) {
        if (upper(name) == wanted) {
            return &value;
        }
    }
    return nullptr;
}

}  // namespace

WatchResult evaluate_watch(std::string_view expression,
                           const std::map<std::string, Value>& variables) {
    WatchResult result;
    result.expression = trimmed(expression);
    if (result.expression.empty()) {
        result.value = "?";
        result.error = "No expression";
        return result;
    }

    if (const Value* direct = bare_variable(result.expression, variables);
        direct != nullptr) {
        result.value = direct->render();
        result.ok = true;
        return result;
    }

    expr::ExpressionEvaluator evaluator;
    for (const auto& [name, value] : variables) {
        if (value.is_number()) {
            evaluator.set_variable(name, value.number());
        } else if (value.is_text()) {
            // The evaluator addresses text as `NAME$`. A variable whose stored
            // name already ends in `$` is not given a second one.
            const std::string key = name.empty() || name.back() == '$' ? name : name + '$';
            evaluator.set_string_variable(key, value.text());
        }
    }

    try {
        const double value = evaluator.evaluate(result.expression);
        // Rendered through Value so `10 / 2` reads `5`, the way a program's own
        // output would, rather than `5.0`.
        result.value = Value{value}.render();
        result.ok = true;
    } catch (const std::exception& thrown) {
        result.value = "?";
        result.error = thrown.what();
    }
    return result;
}

std::vector<WatchResult> evaluate_watches(const std::vector<std::string>& expressions,
                                          const std::map<std::string, Value>& variables) {
    std::vector<WatchResult> results;
    results.reserve(expressions.size());
    for (const std::string& expression : expressions) {
        results.push_back(evaluate_watch(expression, variables));
    }
    return results;
}

}  // namespace timewarp::lang
