// SPDX-License-Identifier: MIT
//
// Runs a complete Scheme program read from stdin and writes its output to
// stdout, in exactly the form `execute_scheme()` produces -- including the
// "❌ Scheme error: ..." prefix -- so tools/scheme_differential_check.py can
// compare the port against the Python reference byte for byte.
//
// Exits non-zero only for host failures (e.g. unreadable stdin).  A Scheme
// error is a successful run of a failing program.

#include <cstdint>
#include <iostream>
#include <iterator>
#include <string>

#include "timewarp/scheme/host.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/scheme/value.hpp"

int main(int argc, char** argv) {
    std::uint64_t step_limit = 0;
    if (argc > 1) {
        try {
            step_limit = static_cast<std::uint64_t>(std::stoull(argv[1]));
        } catch (const std::exception&) {
            std::cerr << "usage: scheme_cli [step-limit]\n";
            return 2;
        }
    }

    const std::string source{std::istreambuf_iterator<char>(std::cin),
                             std::istreambuf_iterator<char>()};

    timewarp::scheme::Arena arena;
    timewarp::scheme::NullHost host;
    timewarp::scheme::Machine machine{arena, host};
    if (step_limit != 0) {
        machine.set_step_limit(step_limit);
    }

    std::cout << machine.execute(source);
    return 0;
}
