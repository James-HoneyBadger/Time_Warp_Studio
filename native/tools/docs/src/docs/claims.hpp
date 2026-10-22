// SPDX-License-Identifier: MIT
//
// `time_warp_docs claims`: what the documentation *claims*, not just what it links
// to.
//
// The C++ counterpart of `native/tools/check_doc_claims.py`. A link that resolves
// can still be a false claim: a sentence can name a file that moved, a symbol that
// was deleted, or a `make` target that never existed. Three rules, each aimed at a
// specific way prose goes stale:
//
//   1. a repository path named in backticks must exist;
//   2. a retired name must be discussed in the past tense, unless the file has
//      marked the passage as history;
//   3. a `make <target>` in a fenced command must be a target of the Makefile that
//      would be consulted from where the command says it runs.
//
// It reads `git ls-files '*.md'`, so build trees and vendored trees are excluded
// without a second ignore list, and it is the file list a reviewer sees.
//
// Exit status: 0 no false claims, 1 at least one (or a failed self-test), 2 the
// command line did not name a run.

#pragma once

namespace docs {

/// Parses `claims [--root DIR] [--quiet] [--self-test]` from `argv[first:]` and
/// runs it.
[[nodiscard]] int run_claims_command(int argc, char** argv);

}  // namespace docs
