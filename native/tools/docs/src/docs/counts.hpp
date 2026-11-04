// SPDX-License-Identifier: MIT
//
// `native/share/counts.txt` and the gate that reads it. This is the fifth claims
// rule: the numbers a guide states must still be the numbers the tree produces.
//
// A path, a name and a `make` target all leave a corpse when they rot -- the path
// stops resolving. A number leaves nothing: "24 suites" and "23 suites" are both
// grammatical, and the claims rules above are blind to the difference. The data
// file pins each count to the exact phrase each page uses to state it, so a page
// cannot reword or drop the claim while its row stays, and the counts this tool
// can read from the tree are checked against it.

#pragma once

#include <cstddef>
#include <filesystem>

namespace docs {

/// Checks every row of `native/share/counts.txt` against the tree and returns the
/// number of failures; the caller adds the verdict to the claims run.
[[nodiscard]] std::size_t check_counts(const std::filesystem::path& root);

/// Proves the phrase matcher fires in both directions, as `claims --self-test`
/// proves the path and name rules. Returns 0 when every case behaves.
[[nodiscard]] int counts_self_test();

}  // namespace docs
