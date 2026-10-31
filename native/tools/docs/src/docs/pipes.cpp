// SPDX-License-Identifier: MIT
//
// The one child process this project starts. See pipes.hpp for why it exists.
//
// `_POSIX_C_SOURCE` has to be the first thing in this translation unit: a strict
// `-std=c++20` defines `__STRICT_ANSI__`, and glibc then keeps `popen` out of
// <cstdio> unless a feature-test macro asks for it. Doing it here rather than in a
// header is what keeps the definition ahead of every include.
#if !defined(_POSIX_C_SOURCE)
#define _POSIX_C_SOURCE 200809L
#endif

#include "docs/pipes.hpp"

#include <cstdio>
#include <iterator>

namespace docs {
namespace {

std::FILE* open_pipe(const char* command) { return popen(command, "r"); }
int close_pipe(std::FILE* pipe) { return pclose(pipe); }

}  // namespace

CommandResult run_command(std::string_view command) {
    CommandResult result;
    const std::string text(command);
    std::FILE* pipe = open_pipe(text.c_str());
    if (pipe == nullptr) {
        return result;
    }
    result.started = true;

    char buffer[4096];
    while (std::fgets(buffer, static_cast<int>(sizeof buffer), pipe) != nullptr) {
        result.output += buffer;
    }
    result.status = close_pipe(pipe);
    return result;
}

}  // namespace docs
