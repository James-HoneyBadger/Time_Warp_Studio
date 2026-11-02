// SPDX-License-Identifier: MIT
//
// Where a program's file names are allowed to point.
//
// Every language that opens a file resolves the name the same way, against the
// directory the run belongs to: a project's root, or the folder the program was
// opened from. That base is what the IDE sets, and it is also the sandbox -- a name
// that would leave it is refused rather than opened.

#pragma once

#include <optional>
#include <string>
#include <string_view>

namespace timewarp::lang {

/// The absolute path that *filename* names inside *base_dir*, or nothing when the
/// name would leave that directory.
///
/// An empty *base_dir* means the process's working directory, which is what the
/// command-line tools use when no run has named a base. A name is inside the base
/// when it resolves to the base itself or to something below it; the comparison is
/// by path component, so `/tmp/run2` is not inside `/tmp/run`.
[[nodiscard]] std::optional<std::string> confined_path(std::string_view filename,
                                                       std::string_view base_dir);

}  // namespace timewarp::lang
