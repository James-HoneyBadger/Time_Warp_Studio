// SPDX-License-Identifier: MIT
//
// Differential-test CLI.
//
// Reads one expression per line from stdin and writes:
//
//     <expression>\tOK <result>
//     <expression>\tERR <message>
//
// It is wired to exactly the same context the Python reference is given by
// tools/differential_check.py, so the two implementations can be diffed over a
// corpus.  Blank lines and lines starting with '#' are ignored.

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
