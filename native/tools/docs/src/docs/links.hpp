// SPDX-License-Identifier: MIT
//
// `time_warp_docs links`: every relative link and every `#anchor` in the guides.
//
// The C++ counterpart of `native/tools/check_doc_links.py`. The documentation is a
// set of books that link to each other, and a dead link is how a reader discovers
// the map is wrong. This is the cheap half of honesty: every relative path must
// exist, and every `#anchor` must name a heading in the target file, using the same
// slug GitHub builds.
//
// Exit status: 0 every link resolves, 1 something is broken, 2 the command line
// did not name a run.

#pragma once

#include "timewarp/tools/cli.hpp"

namespace docs {

/// Parses `links [--root DIR] [FILE ...]` from `argv[first:]` and runs it.
[[nodiscard]] int run_links_command(const timewarp::cli::Invocation& invocation);

}  // namespace docs
