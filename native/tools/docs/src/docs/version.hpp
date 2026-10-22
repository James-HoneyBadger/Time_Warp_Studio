// SPDX-License-Identifier: MIT
//
// `time_warp_docs version`: check, and optionally apply, the hand-written copies of
// the product version.
//
// The C++ counterpart of `Scripts/version_sync.py`. `VERSION` is the source of
// truth; the Qt application reads it at configure time, so the binary, the window
// title and the About box cannot drift apart. What this looks after are the copies a
// person writes by hand -- the two documentation files that state the version in
// prose -- and the one piece of release paperwork that has to exist and must not be
// rewritten: the changelog section for the version being released.
//
// Without `--write` nothing is modified and any disagreement makes the exit status 1,
// which is what CI uses to refuse a change where the numbers no longer agree.
//
// Exit status: 0 the copies agree and the paperwork is there, 1 they do not, 2 the
// files could not be read or the command line did not name a run.

#pragma once

namespace docs {

/// Parses `version [--root DIR] [--write]` from `argv[first:]` and runs it.
[[nodiscard]] int run_version_command(int argc, char** argv);

}  // namespace docs
