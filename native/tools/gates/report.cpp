// SPDX-License-Identifier: MIT
//
// Where a gate writes what it found, and whether it was asked to be quiet.
//
// Every tool in `tools/` speaks the command-line vocabulary in
// `timewarp/tools/cli.hpp`; this is the part of it a gate needs in the middle of its
// own printing. Two facts, set once by `main` before a command runs:
//
//   * `--quiet` -- the closings (a tally, a count of rows) are not printed. A
//     failure is always printed, because a quiet run that hides a failure is a lie.
//   * `--json` -- the prose moves to stderr, so stdout carries the machine report
//     and nothing else. A script reading the report does not have to skip prose, and
//     a person reading the log still sees what happened.

#include "gates.hpp"

#include <cstdio>

namespace timewarp::gates {
namespace {

bool g_quiet = false;
bool g_prose_to_stderr = false;

}  // namespace

FILE* out() {
    return g_prose_to_stderr ? stderr : stdout;
}

bool quiet() {
    return g_quiet;
}

void setQuiet(bool quiet) {
    g_quiet = quiet;
}

void setProseToStderr(bool to_stderr) {
    g_prose_to_stderr = to_stderr;
}

}  // namespace timewarp::gates
