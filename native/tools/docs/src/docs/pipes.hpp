// SPDX-License-Identifier: MIT
//
// Running one child process and capturing its standard output.
//
// It exists for a single caller: `check_doc_claims.py` shells out to
// `git ls-files '*.md'` so that build trees, the virtual environment and
// node_modules stay out of the run without a second ignore list, and this port has
// to ask git the same question. Python had subprocess.run; C++ has nothing, and
// popen is the smallest thing that answers. Nothing else in this project starts a
// process.

#pragma once

#include <string>
#include <string_view>

namespace docs {

struct CommandResult {
    /// Everything the child wrote to standard output.
    std::string output;
    /// The child's exit status, or -1 when it could not be started.
    int status = -1;
    /// Whether a pipe was opened at all, which tells a failed start from a failed
    /// command.
    bool started = false;
};

/// Runs `command` through the shell and captures its standard output. The child's
/// standard error is left alone: git's complaint about a directory that is not a
/// repository is more useful than a generic message.
[[nodiscard]] CommandResult run_command(std::string_view command);

}  // namespace docs
