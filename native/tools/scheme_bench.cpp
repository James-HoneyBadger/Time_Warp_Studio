// SPDX-License-Identifier: MIT
//
// Measures what the port exists to fix: how deep and how fast the evaluator can
// go.  The reference aborts at roughly 200 Scheme calls, so every workload here
// is one it cannot run at all.
//
//   ./native/build/scheme_bench

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <string>

#include "timewarp/scheme/host.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/value.hpp"

namespace {

using Clock = std::chrono::steady_clock;

struct Measurement {
    double milliseconds = 0.0;
    std::size_t peak_frames = 0;
    std::uint64_t steps = 0;
    std::string output;
};

Measurement measure(const std::string& program) {
    timewarp::scheme::Arena arena;
    timewarp::scheme::NullHost host;
    timewarp::scheme::Machine machine{arena, host};

    const auto start = Clock::now();
    Measurement result;
    result.output = machine.execute(program);
    result.milliseconds =
        std::chrono::duration<double, std::milli>(Clock::now() - start).count();
    result.peak_frames = machine.peak_frame_depth();
    result.steps = machine.steps();
    return result;
}

void report(const char* label, const Measurement& measurement,
            std::uint64_t count, const char* unit) {
    const double per_call =
        measurement.milliseconds * 1e6 / static_cast<double>(count);
    std::printf("%-26s %9.1f ms %8.1f ns/%-8s %10llu steps  peak frames %zu\n",
                label, measurement.milliseconds, per_call, unit,
                static_cast<unsigned long long>(measurement.steps),
                measurement.peak_frames);
}

/// A tail-recursive loop needs constant machine depth -- this is the number the
/// whole design exists to keep flat.
void report_tail_calls(std::uint64_t count) {
    const std::string program =
        "(define (loop n) (if (= n 0) 'done (loop (- n 1))))"
        "(display (loop " +
        std::to_string(count) + "))";
    const Measurement measurement = measure(program);
    report("tail-call loop", measurement, count, "call");
}

void report_non_tail(std::uint64_t count) {
    const std::string program =
        "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1)))))"
        "(display (sum " +
        std::to_string(count) + "))";
    const Measurement measurement = measure(program);
    report("non-tail recursion", measurement, count, "level");
}

void report_build(std::uint64_t count) {
    const std::string program =
        "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons n acc))))"
        "(display (length (mk " +
        std::to_string(count) + " '())))";
    const Measurement measurement = measure(program);
    report("list construction", measurement, count, "element");
}

void report_print(std::uint64_t count) {
    const std::string program =
        "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons n acc))))"
        "(display (mk " +
        std::to_string(count) + " '()))";
    const Measurement measurement = measure(program);
    report("printing a long list", measurement, count, "element");
    std::printf("%-26s %9zu bytes of output\n", "", measurement.output.size());
}

void report_nested(std::uint64_t depth) {
    const std::string program =
        "(define (nest n x) (if (= n 0) x (nest (- n 1) (list x))))"
        "(display (equal? (nest " +
        std::to_string(depth) + " 'a) (nest " + std::to_string(depth) + " 'a)))";
    const Measurement measurement = measure(program);
    report("deep structural equal?", measurement, depth, "level");
}

/// A rough measure of raw evaluation throughput on ordinary code.
void report_throughput(std::uint64_t iterations) {
    const std::string program =
        "(define (work n acc) "
        "  (if (= n 0) acc "
        "      (work (- n 1) (+ acc (* 2 3) (- 7 2) (if (> n 1) 1 0)))))"
        "(display (work " +
        std::to_string(iterations) + " 0))";
    const Measurement measurement = measure(program);
    report("arithmetic loop", measurement, iterations, "iteration");
}

}  // namespace

int main() {
    std::printf("Scheme evaluator (explicit continuation stack, no native recursion)\n");
    std::printf("reference implementation limit: ~200 Scheme calls\n\n");

    report_tail_calls(1000000);
    report_non_tail(200000);
    report_build(200000);
    report_print(200000);
    report_nested(200000);
    report_throughput(200000);

    std::printf("\n");
    return 0;
}
