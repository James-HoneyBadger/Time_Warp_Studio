// SPDX-License-Identifier: MIT
//
// Expression probe.
//
// Reads one expression per line from stdin and writes:
//
//     <expression>\tOK <result>
//     <expression>\tERR <message>
//
// It was built to be diffed against the Python reference one expression at a time,
// and that half is gone with the reference. What is left is the quickest way to ask
// the evaluator a question by hand -- `echo '2+3*X' | native/build/expr_cli` -- with
// `evaluator_tests` as the gate. Blank lines and lines starting with '#' are ignored.

#include <iomanip>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "timewarp/expr/evaluator.hpp"

namespace {

constexpr double kTable[] = {0.0, 10.0, 20.0, 30.0};

timewarp::expr::ExpressionEvaluator make_evaluator() {
    timewarp::expr::EvalContext context;
    context.variables = {{"X", 10.0}, {"A", 4.0}, {"B", 0.0}};
    context.arrays = {{"TAB", kTable}};
    context.string_variables = {{"NAME$", "Time Warp"},
                                {"OTHER$", "Time Warp"}};
    context.special_variables = [](std::string_view name)
        -> std::optional<double> {
        if (name == "TIMER") {
            return 42.0;
        }
        return std::nullopt;
    };
    return timewarp::expr::ExpressionEvaluator{std::move(context)};
}

}  // namespace

int main() {
    auto evaluator = make_evaluator();

    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty() || line.front() == '#') {
            continue;
        }
        std::cout << line << '\t';
        try {
            // Evaluate *before* touching the stream: `std::cout << "OK " <<
            // evaluate(...)` would flush the "OK " prefix as part of the same
            // left-to-right chain, so a throwing evaluation would leave a
            // stray "OK " in front of the error line.
            const double value = evaluator.evaluate(line);
            std::cout << "OK " << std::setprecision(17) << value << '\n';
        } catch (const std::exception& error) {
            std::cout << "ERR " << error.what() << '\n';
        }
    }
    return 0;
}
