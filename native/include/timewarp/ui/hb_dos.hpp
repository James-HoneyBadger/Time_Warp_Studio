// SPDX-License-Identifier: MIT
//
// HB DOS: the HB-1's disk operating system.
//
// The user-facing reference -- every word, every refusal, the image format --
// is `docs/hb1/hb-dos.md`.
//
// The machine's memory (the PROGRAM page, one buffer per ROM) is volatile; a
// learner who has typed a program wants it back tomorrow. HB DOS is that, modelled
// on the DOS a 1980s home computer shipped with: a volume, a catalogue of named
// files, a lock bit, and the handful of words — CATALOG, SAVE, LOAD, RUN, DELETE,
// RENAME, LOCK, UNLOCK, VERIFY, INIT — that a child learned before they learned to
// program.
//
// It is deliberately not a filesystem abstraction over the host: no directories,
// no extensions in the catalogue, one volume, and a size column counted in the
// 256-byte sectors a 5.25" disk was measured in. What it *is* is one JSON image in
// the application's data directory, so a program saved in a lesson is still there
// in the next lesson, and a teacher can copy the image to another machine.
//
// The class knows nothing about widgets. `RetroComputerWindow` is what turns these
// calls into the words a user types, and the tests drive both.

#pragma once

#include <QList>
#include <QString>

namespace timewarp::ui {

/// One program on the volume.
struct DosFile {
    /// As catalogued: upper case, at most `HbDos::kMaxNameLength` characters.
    QString name;
    /// The ROM it belongs to, as a protocol key (`settingsKey`), so LOAD can
    /// cold-boot the machine into the language that can run it.
    QString language;
    QString source;
    /// LOCKed files cannot be written over or deleted. It is the write-protect
    /// tab of a disk, per file.
    bool locked = false;
};

/// One volume of programs, backed by one image file.
class HbDos {
public:
    /// A 5.25" disk's geometry, kept because the catalogue's size column is the
    /// thing a learner recognises.
    static constexpr int kSectorSize = 256;
    static constexpr int kTotalSectors = 512;
    /// What `CATALOG` prints beside `DISK VOLUME`. A number rather than a name
    /// because that is what the machines this imitates put there.
    static constexpr int kVolumeNumber = 254;
    /// Apple DOS allowed 30 characters. Longer names wrap the catalogue.
    static constexpr int kMaxNameLength = 30;

    /// `<data dir>/disks/HBWARP.dsk`, or empty when the directory cannot be made.
    [[nodiscard]] static QString defaultImagePath();

    /// An empty `image_path` is a machine with no disk: `open` and `commit`
    /// report `NO DISK` rather than pretending a save happened, which is what a
    /// data directory that cannot be created leaves us with. `defaultImagePath`
    /// is what the machine uses.
    explicit HbDos(QString image_path = defaultImagePath());

    [[nodiscard]] QString imagePath() const { return image_path_; }
    [[nodiscard]] QString volumeName() const { return volume_name_; }
    [[nodiscard]] const QList<DosFile>& files() const noexcept { return files_; }
    /// The file called `name` (any case, quotes allowed), or null.
    [[nodiscard]] const DosFile* find(const QString& name) const;
    [[nodiscard]] int usedSectors() const;
    [[nodiscard]] int freeSectors() const;

    /// Reads the image. A missing image is a blank, formatted volume rather than
    /// an error; an unreadable one is an error and leaves the volume empty.
    bool open(QString* error = nullptr);
    /// Writes the image, creating its directory. The volume in memory is
    /// unchanged when this fails.
    bool commit(QString* error = nullptr);

    /// Stores `source` as `name`, replacing an unlocked file of that name.
    bool write(const QString& name, const QString& language, const QString& source,
               QString* error = nullptr);
    bool erase(const QString& name, QString* error = nullptr);
    bool rename(const QString& from, const QString& to, QString* error = nullptr);
    bool setLocked(const QString& name, bool locked, QString* error = nullptr);
    bool read(const QString& name, DosFile* file, QString* error = nullptr) const;

    /// Erases every file, locked ones included, and writes a fresh volume. The
    /// window asks twice before calling this.
    bool format(QString* error = nullptr);

    /// Upper-cases and validates a name, stripping one layer of quotes. Returns an
    /// empty string and sets `error` when the name is not one.
    [[nodiscard]] static QString normaliseName(const QString& raw, QString* error = nullptr);
    [[nodiscard]] static int sectorsFor(const QString& source);

private:
    [[nodiscard]] int indexOf(const QString& name) const;

    QString image_path_;
    QString volume_name_{QStringLiteral("HBWARP")};
    QList<DosFile> files_;
};

}  // namespace timewarp::ui
