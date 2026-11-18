// SPDX-License-Identifier: AGPL-3.0-only
//
// The browser's filesystem, and the one thing a desktop build has no equivalent of: it
// is thrown away when the page reloads unless it has been written to IndexedDB.
//
// Qt for WebAssembly's Emscripten filesystem lives in memory. A document saved into
// `projectRoot()` survives a repaint and disappears on a reload unless `FS.syncfs` is
// called; nothing in the Qt layer calls it, because there is no `fsync` in the browser
// for Qt to translate. This module is that call, in the three places the storage should
// be written: when the page is about to unload, when a document is saved, and every
// thirty seconds, so a tab closed by a crash loses at most half a minute of work.
//
// The mounted volume is the whole home directory, not the projects folder: the IDE's
// own data (`~/.time_warp`) shares it with the documents, and persisting one without
// the other would lose a learner's lesson progress while keeping the program.
//
// A desktop build compiles every function here to a no-op. That is deliberate rather
// than incidental: the save path calls `saveNow()` unconditionally, and a `#ifdef` at
// each call site would be a second thing to keep true.

#pragma once

#include <QString>

namespace timewarp::ui::wasm {

/// Where projects are kept inside the persisted volume.
///
/// `/home/web_user/projects` is under the home directory Emscripten gives the process,
/// so it is on the mounted volume and is the folder the project dialogs start from.
[[nodiscard]] QString projectRoot();

/// Whether this build persists anything at all: true only under Emscripten.
///
/// The user interface asks before it offers a Projects location, so a desktop build
/// does not show a path that only exists in a browser.
[[nodiscard]] bool available();

/// Mounts IndexedDB over the home directory and loads what was there.
///
/// In the browser build, the call yields until population completes, before the window
/// restores documents or projects. A failure is reported in the page status; the
/// application still runs, but browser storage is unavailable.
void mountProjectStorage();

/// Starts the thirty-second flush and the `beforeunload` handler.
///
/// Called once, after the application object exists; the timer is parented to it so the
/// flush stops when the application does.
void startAutoSave();

/// Flushes now: the volume is written to IndexedDB.
///
/// Called when a document is saved and by the automatic timer. The browser build yields
/// until IndexedDB confirms the write; false means the save did not become durable.
/// A desktop build has no browser storage and returns true.
bool saveNow();

}  // namespace timewarp::ui::wasm
