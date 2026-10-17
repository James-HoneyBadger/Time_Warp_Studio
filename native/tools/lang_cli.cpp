// SPDX-License-Identifier: MIT
//
// Runs one native language program and prints what it did, so
// `tools/lang_differential_check.py` can compare the C++ engine with the Python
// reference line by line.
//
//   lang_cli <language-key>  < program
//
// Output, one record per line:
//
//   OUT\t<line>                 program output, in order
//   DRAW\t<op> <args...>       one drawing call
//   VAR\t<name>\t<type>\t<value>  a variable the run left behind
//
// Input is answered with an empty string, matching a one-shot client of the
// bridge (the worker's `release_input`). Programs whose output depends on a
// particular answer are not part of the parity corpus.

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/language.hpp"

#include "trace_host.hpp"

#include <cstdio>
#include <iostream>
#include <iterator>
#include <string>

int main(int argc, char** argv) {
    if (argc == 2 && std::string{argv[1]} == "--ported") {
        // The parity harness asks which languages have a native executor, so a
        // port is registered in one place (`executor_for`) and not in a list on
        // each side that can drift.
        for (const timewarp::lang::Language language :
             timewarp::lang::all_languages()) {
            if (timewarp::lang::executor_for(language) != nullptr) {
                std::printf("%s\n",
                            std::string{timewarp::lang::key(language)}.c_str());
            }
        }
        return 0;
    }
    if (argc != 2) {
        std::fprintf(stderr, "usage: %s <language-key>  < program\n", argv[0]);
        return 2;
    }
    const std::optional<timewarp::lang::Language> language =
        timewarp::lang::language_from_key(argv[1]);
    if (!language.has_value()) {
        std::fprintf(stderr, "unknown language: %s\n", argv[1]);
        return 2;
    }

    const std::string source{std::istreambuf_iterator<char>{std::cin},
                             std::istreambuf_iterator<char>{}};

    timewarp::lang::TraceHost host;
    timewarp::lang::Interpreter interpreter{*language, host};
    interpreter.set_output_callback(
        [](const std::string& line) { std::printf("OUT\t%s\n", line.c_str()); });
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });
    interpreter.load_program(source);
    interpreter.run();

    for (const auto& [name, value] : interpreter.variables()) {
        std::printf("VAR\t%s\t%s\t%s\n", name.c_str(), value.type_name(),
                    value.render().c_str());
    }
    return 0;
}
