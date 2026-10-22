// SPDX-License-Identifier: MIT
//
// `time_warp_docs` -- the documentation gates, as one executable.
//
//   time_warp_docs links   [FILE ...]
//   time_warp_docs claims  [--self-test]
//   time_warp_docs version [--write]
//   time_warp_docs md      [--explain] [FILE ...]
//
// The command line is `timewarp::cli`'s, which every tool in `native/tools/` speaks:
// `--root DIR`, `--json PATH`, `--quiet` and `--help` in any position, and status 2
// for a command line that cannot be used. The Python these replace found the
// repository from `Path(__file__).resolve()`; `--root` is the replacement,
// defaulting to the working directory, which is where CI and the Makefile run them
// from.
//
// Why it is a project of its own: the documentation job has to run on a runner with
// no Qt and no virtual environment, and it has to run in seconds. This links nothing.
//
// Exit status: 0 the check passed, 1 it failed, 2 the command line or an input file
// did not allow a check.

#include <array>
#include <iostream>
#include <span>
#include <string_view>

#include "timewarp/tools/cli.hpp"

#include "docs/claims.hpp"
#include "docs/links.hpp"
#include "docs/md.hpp"
#include "docs/version.hpp"

namespace {

using timewarp::cli::Invocation;

int runLinks(const Invocation& invocation) {
    return docs::run_links_command(invocation);
}

int runClaims(const Invocation& invocation) {
    return docs::run_claims_command(invocation);
}

int runVersion(const Invocation& invocation) {
    return docs::run_version_command(invocation);
}

int runMd(const Invocation& invocation) {
    return docs::md::run_md_command(invocation);
}

constexpr std::array<timewarp::cli::Command, 4> kCommands{{
    {"links", "every relative link and #anchor in the guides resolves", runLinks},
    {"claims", "every path and name a guide mentions exists", runClaims},
    {"version", "every hand-written copy of VERSION agrees with it", runVersion},
    {"md", "every tracked Markdown file satisfies the house rules", runMd},
}};

constexpr std::string_view kSynopsis{"<command> [FILE ...]"};

}  // namespace

int main(int argc, char** argv) {
    const Invocation invocation = timewarp::cli::parse("time_warp_docs", argc, argv);
    if (invocation.help || !invocation.ok() || invocation.command == "help") {
        return timewarp::cli::dispatch(invocation, kSynopsis, kCommands);
    }
    int status = 0;
    {
        // The report is the only thing on stdout; what the check prints as it goes
        // is for a person reading the log.
        const timewarp::cli::ProseToStderr prose{invocation.wants_json()};
        status = timewarp::cli::dispatch(invocation, kSynopsis, kCommands);
    }
    if (!invocation.wants_json()) {
        return status;
    }
    timewarp::cli::Report report{"time_warp_docs", invocation};
    report.check(invocation.command, status == 0, status == 0 ? "" : "the check said no");
    return report.finish();
}
