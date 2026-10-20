// SPDX-License-Identifier: MIT
//
// The Studio's automation API: JSON-RPC 2.0 over newline-delimited JSON.
//
// The surface is deliberately transport-neutral. `time_warp_api` speaks it on
// stdin/stdout, which is enough for a shell pipeline, a test, or an editor
// plugin, and it means the API has no port to collide with, no authentication to
// forget and no network exposure to reason about. A caller writes one request per
// line and reads one response per line.
//
// Everything the IDE can do with a program, a database or a project is here:
//
//   * `language.run` runs one program and returns its output, bindings and SVG.
//   * `repl.*` keeps a session alive between calls, the same way the REPL bar does.
//   * `sql.*` opens a SQLite file, runs SQL and reads the schema.
//   * `project.*` creates and edits a `.twproject` and its groups.
//   * `watch.evaluate` evaluates debugger-style expressions over bindings.
//
// Errors use the JSON-RPC codes (-32700, -32600, -32601, -32602, -32603) and an
// application failure is -32000 with the engine's message. A request without an
// `id` is a notification and gets no response, per the specification.

#pragma once

#include <iosfwd>
#include <memory>
#include <string>

namespace timewarp::api {

class Server {
public:
    /// `in` and `out` must outlive the server. The usual pair is `std::cin` and
    /// `std::cout`.
    Server(std::istream& in, std::ostream& out);
    ~Server();

    Server(const Server&) = delete;
    Server& operator=(const Server&) = delete;

    /// Reads requests until EOF or a `studio.shutdown` call. Returns 0.
    int run();

    /// Handles one already-read line and returns the response line, or an empty
    /// string for a notification. Exposed so a test can drive the server without
    /// a process.
    [[nodiscard]] std::string handleLine(const std::string& line);

private:
    struct State;
    std::unique_ptr<State> state_;
    std::istream& in_;
    std::ostream& out_;
};

}  // namespace timewarp::api
