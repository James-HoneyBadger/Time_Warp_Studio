// SPDX-License-Identifier: MIT
//
// Application identity: who this build is, as far as the operating system and
// QSettings are concerned.
//
// Keeping this in one place matters because the answer has to agree with itself:
// the window title, the settings file, the data directory and any future
// single-instance or desktop-file integration all derive from it.

#pragma once

#include <QString>

namespace timewarp::ui {

/// Applies the organisation name, application name and version to
/// `QCoreApplication`. Call once, before anything constructs a `QSettings`.
///
/// **The settings identity is deliberately *not* the Python IDE's.** That build
/// uses `TimeWarp` / `IDE` (see `core/config.py`), so its file is
/// `~/.config/TimeWarp/IDE.conf`; this one is `~/.config/TimeWarpStudio/TimeWarpStudio.conf`.
///
/// The two schemas are unrelated — Python stores `geometry`, `windowState` and
/// dock state for its own widget tree, this build stores `layout/...` — so sharing
/// a scope would work today only because no key happens to collide. That is the
/// kind of coincidence that turns into silent corruption the moment somebody adds
/// a matching key, and unlike a build error it would surface as a window that
/// restores into a state the user cannot drag their way out of. Separate
/// identities make it structurally impossible.
///
/// Carrying a user's preferences across is a *migration*, not a shared file: read
/// the Python settings once, translate the theme name and recent files, write this
/// build's own keys. That lands with the theme system, which is the first thing
/// with anything worth migrating.
///
/// **User *content* is a different question, and the answer is the opposite.**
/// The Python IDE keeps projects, lesson progress and exports under `~/.time_warp`
/// (`core/config.py: APP_DATA_DIR`). Content belongs to the user rather than to an
/// implementation, so this build uses the same directory: a program written in one
/// build must open in the other. Only settings are private.
void applyApplicationIdentity();

/// The project version, from the repository's `VERSION` file via CMake, so the
/// window title and the packaged version cannot drift apart.
[[nodiscard]] QString version();

}  // namespace timewarp::ui
