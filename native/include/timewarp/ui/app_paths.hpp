// SPDX-License-Identifier: MIT
//
// Where user data lives.
//
// One answer to a question the application was starting to answer in several
// places: autosave snapshots, version history, classroom bundles and anything
// else that has to survive a restart but is not a document. Nothing here is a
// *preference* -- preferences are `QSettings`, as they always were -- and nothing
// here is the user's file.
//
// The location is the reference engine's, deliberately: `~/.time_warp`, moved by
// `TIME_WARP_DATA_DIR` (see `native/bridge/time_warp/core/config.py`). The lesson
// progress and the classroom roster are written there by the Python engine, and
// two directories for one application's data would mean a portable install, a
// test run and a teacher's backup each had to know about both.

#pragma once

#include <QString>

namespace timewarp::ui {

/// The directory user data lives in. Not created by this call.
///
/// `TIME_WARP_DATA_DIR` when it is set and not empty (expanded, so `~` works),
/// else `~/.time_warp`. The variable exists so a test can point at a temporary
/// directory and never touch a learner's real progress -- the same reason the
/// Python engine honours it.
[[nodiscard]] QString dataDir();

/// `dataDir()` joined with `leaf`, created if it does not exist.
///
/// Returns an empty string when the directory cannot be created, because a
/// caller that is about to write a snapshot needs to know that before it starts
/// rather than after it has lost one.
[[nodiscard]] QString dataDirFor(const QString& leaf);

/// Where this build's example programs live, or empty when it has none.
///
/// The installed layout is searched first, then the checkout beside the binary,
/// then the working directory. `TIME_WARP_EXAMPLES` overrides all three, so a
/// teacher can point the application at a folder of their own programs.
[[nodiscard]] QString examplesDir();

}  // namespace timewarp::ui
