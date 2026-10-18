// SPDX-License-Identifier: MIT
//
// Autosave: snapshots of work that has not been saved, and the history of them.
//
// Ported from `features/autosave_manager.py` (188 lines), which kept a shadow
// copy every two minutes and a `FileVersion` per write. The port keeps the idea
// and is explicit about the two decisions the original left implicit:
//
//  * **Where.** Under the application's data directory (`app_paths.hpp`), not
//    beside the document: a `.bak` appearing next to somebody's homework is a
//    surprise, and a read-only folder must not stop the safety net working.
//  * **What a snapshot is.** The text, the document it came from, and when. Not
//    a diff: a diff is smaller and harder to get right, and the thing this
//    protects is a learner's unsaved buffer, which is small.
//
// The store is plain I/O with no Qt widgets, so it is tested directly rather than
// through a window. `MainWindow` owns the timer and calls it; nothing here knows
// a window exists.

#pragma once

#include <QList>
#include <QString>

namespace timewarp::ui {

/// One saved copy of a document.
struct Snapshot {
    /// The document it came from: an absolute path, or `untitled:<label>` for a
    /// buffer that has never been saved.
    QString document;
    /// The file holding the text, inside the store's directory.
    QString file;
    /// Milliseconds since the epoch, from the caller's clock.
    qint64 timestamp = 0;
    qint64 bytes = 0;
};

/// The snapshot store, and the manifest that lists it.
///
/// A manifest file rather than naming the snapshots after their documents: a
/// document's path can contain anything, and a hash of it cannot be read back
/// when the manifest is the thing that is lost.
class AutosaveStore {
public:
    /// How many snapshots to keep for one document. Old ones are deleted when a
    /// new one is written, so the store has a bound rather than growing with
    /// every autosave interval of every session.
    static constexpr int kKeepPerDocument = 5;

    /// `directory` empty means `<data dir>/autosave`.
    ///
    /// The directory is created on the first write, not here: a user who never
    /// turns autosave on should not find a directory for it.
    explicit AutosaveStore(QString directory = {});

    /// The directory in use, whether or not it exists yet.
    [[nodiscard]] QString directory() const { return directory_; }

    /// Writes `text` as a snapshot of `document`. Returns the snapshot, or an
    /// empty one (with `timestamp == 0`) when the directory could not be written.
    [[nodiscard]] Snapshot save(const QString& document, const QString& text, qint64 now);

    /// The snapshots for `document`, newest first.
    [[nodiscard]] QList<Snapshot> versions(const QString& document) const;

    /// Every snapshot the store holds, newest first.
    [[nodiscard]] QList<Snapshot> all() const;

    /// Snapshots of the document at `path` taken *after* `mtime_ms`, which is
    /// what "this file has work in it the file does not" means: the document was
    /// edited after it was last saved.
    [[nodiscard]] QList<Snapshot> newerThan(const QString& path, qint64 mtime_ms) const;

    /// The text of `snapshot`, or empty when the file has gone.
    [[nodiscard]] QString read(const Snapshot& snapshot) const;

private:
    [[nodiscard]] QString manifestPath() const;
    [[nodiscard]] QList<Snapshot> loadManifest() const;
    bool writeManifest(const QList<Snapshot>& snapshots) const;

    QString directory_;
};

}  // namespace timewarp::ui
