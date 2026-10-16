// SPDX-License-Identifier: MIT
//
// Micro-benchmark for the expression engine.
//
// Runs a fixed, representative corpus two ways:
//
//   hot   - the token cache is left enabled, which is how the interpreter
//           evaluates the same statement over and over inside a loop
//   cold  - the cache is cleared before every evaluation, isolating
//           tokenizer + shunting-yard throughput
//
// The corpus deliberately mixes evaluation shapes (arithmetic chain, call,
// comparison, variable lookup) so the number is not a best case for one path.

#include <chrono>
#include <cstdio>
#include <optional>
#include <string_view>

#include "timewarp/expr/evaluator.hpp"

namespace {

constexpr double kTable[] = {0.0, 10.0, 20.0, 30.0};

inline constexpr std::string_view kCorpus[] = {
    "2 + 3 * X",
    "(X + 1) * (X - 1) / 2",
    "SQRT(144) + ABS(0 - 7)",
    "X > 5 AND A < 9",
    "TAB(2) + LEN(NAME$)",
    "MIN(X, A) ^ 2 - 7 MOD 3",
    "NOT B OR X >= 10",
    "INT(X / 3) << 2",
};

constexpr int kIterations = 20000;
constexpr int kWarmupIterations = 500;

timewarp::expr::ExpressionEvaluator make_evaluator() {
    timewarp::expr::EvalContext context;
    context.variables = {{"X", 10.0}, {"A", 4.0}, {"B", 0.0}};
    context.arrays = {{"TAB", kTable}};
    context.string_variables = {{"NAME$", "Time Warp"}};
    context.special_variables = [](std::string_view name)
        -> std::optional<double> {
        if (name == "TIMER") {
            return 42.0;
        }
        return std::nullopt;
    };
    return timewarp::expr::ExpressionEvaluator{std::move(context)};
}

[[nodiscard]] double evaluate_all(timewarp::expr::ExpressionEvaluator& evaluator,
                                  bool clear_cache) {
    double sink = 0.0;
    for (const std::string_view expression : kCorpus) {
        if (clear_cache) {
            evaluator.clear_token_cache();
        }
        try {
            sink += evaluator.evaluate(expression);
        } catch (const std::exception&) {
            // Benchmarks ignore diagnostics; the suites cover correctness.
        }
    }
    return sink;
}

void report(const char* label, double sink, double nanoseconds) {
    const double count = static_cast<double>(kIterations) *
                         static_cast<double>(std::size(kCorpus));
    std::printf("%-5s %10.1f ms total  %8.1f ns/eval  (%lld evaluations, sink %.3f)\n",
                label, nanoseconds / 1e6, nanoseconds / count,
                static_cast<long long>(count), sink);
}

}  // namespace

int main() {
    auto evaluator = make_evaluator();
    for (int i = 0; i < kWarmupIterations; ++i) {
        (void)evaluate_all(evaluator, /*clear_cache=*/false);
    }

    {
        const auto start = std::chrono::steady_clock::now();
        const double sink = [&] {
            double accumulated = 0.0;
            for (int i = 0; i < kIterations; ++i) {
                accumulated += evaluate_all(evaluator, /*clear_cache=*/false);
            }
            return accumulated;
        }();
        const auto elapsed = std::chrono::steady_clock::now() - start;
        report("hot", sink,
               std::chrono::duration<double, std::nano>(elapsed).count());
    }

    {
        const auto start = std::chrono::steady_clock::now();
        const double sink = [&] {
            double accumulated = 0.0;
            for (int i = 0; i < kIterations; ++i) {
                accumulated += evaluate_all(evaluator, /*clear_cache=*/true);
            }
            return accumulated;
        }();
        const auto elapsed = std::chrono::steady_clock::now() - start;
        report("cold", sink,
               std::chrono::duration<double, std::nano>(elapsed).count());
    }

    return 0;
}
