// SPDX-License-Identifier: MIT
//
// `time_warp_api`: the Studio's automation surface as a process.
//
// One JSON-RPC 2.0 request per line on stdin, one response per line on stdout.
// No window, no Qt event loop, no port. Run it from a script, a test, or an
// editor plugin; `docs/API.md` lists every method.

#include <cstdio>
#include <cstring>
#include <iostream>

#include "timewarp/api/server.hpp"

int main(int argc, char** argv) {
    for (int index = 1; index < argc; ++index) {
        if (std::strcmp(argv[index], "--help") == 0 ||
            std::strcmp(argv[index], "-h") == 0) {
            std::printf(
                "Usage: %s\n\n"
                "Speaks the Time Warp Studio JSON-RPC API on stdin/stdout.\n"
                "Each line is one request; each response is one line.\n"
                "See docs/API.md for the methods.\n",
                argv[0]);
            return 0;
        }
    }
    timewarp::api::Server server{std::cin, std::cout};
    return server.run();
}
