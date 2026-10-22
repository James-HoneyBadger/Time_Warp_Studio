// SPDX-License-Identifier: MIT
//
// `time_warp_docs` -- the documentation gates, as one executable.
//
//   time_warp_docs links   [--root DIR] [FILE ...]
//   time_warp_docs claims  [--root DIR] [--quiet] [--self-test]
//   time_warp_docs version [--root DIR] [--write]
//   time_warp_docs md      [--root DIR] [--quiet] [FILE ...]
//
// The Python they replace found the repository from `Path(__file__).resolve()`;
// `--root` is the replacement, defaulting to the working directory, which is where
// CI and the Makefile run them from. That is the only difference in behaviour: the
// lines printed and the exit status are the same, so a reader who has seen the
// Python output recognises this one.
//
// Why it exists at all: the documentation job has to run on a runner with no Qt and
// no virtual environment, and it has to run in seconds. This project links nothing.
//
// Exit status: 0 the check passed, 1 it failed, 2 the command line or an input file
// did not allow a check.

#include <cstdio>
#include <iostream>
#include <string_view>

#include "docs/claims.hpp"
#include "docs/links.hpp"
#include "docs/md.hpp"
#include "docs/version.hpp"

namespace {

int usage(std::ostream& stream, int status) {
    stream << "usage: time_warp_docs links   [--root DIR] [FILE ...]\n"
              "       time_warp_docs claims  [--root DIR] [--quiet] [--self-test]\n"
              "       time_warp_docs version [--root DIR] [--write]\n"
              "       time_warp_docs md      [--root DIR] [--quiet] [FILE ...]\n"
              "\n"
              "  links    every relative link and #anchor in the guides resolves\n"
              "  claims   every path, retired name and make target a guide names exists\n"
              "  version  every hand-written copy of VERSION agrees with it\n"
              "  md       every tracked Markdown file satisfies the house rules\n"
              "\n"
              "--root defaults to the working directory.\n";
    return status;
}

}  // namespace

int main(int argc, char** argv) {
    if (argc < 2) {
        return usage(std::cerr, 2);
    }
    const std::string_view command = argv[1];
    if (command == "--help" || command == "-h" || command == "help") {
        return usage(std::cout, 0);
    }
    if (command == "links") {
        return docs::run_links_command(argc, argv);
    }
    if (command == "claims") {
        return docs::run_claims_command(argc, argv);
    }
    if (command == "version") {
        return docs::run_version_command(argc, argv);
    }
    if (command == "md") {
        return docs::md::run_md_command(argc, argv);
    }
    std::cerr << "time_warp_docs: unknown command: " << command << '\n';
    return usage(std::cerr, 2);
}
