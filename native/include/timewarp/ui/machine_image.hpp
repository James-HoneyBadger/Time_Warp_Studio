// SPDX-License-Identifier: MIT
//
// A whole machine, as one file.
//
// The HB-1 keeps its configuration in QSettings, its disk in
// `~/.time_warp/disks`, and one program per ROM in the settings too. That is the
// right shape for a machine you own and the wrong one for a machine you *hand to
// somebody*: a teacher with a lesson machine, or a reader with a `.hb1` beside a
// `.bas`, cannot move three scattered things and call it one.
//
// So `MachineImage` is the whole state -- the ROM it was left in, the screen, the
// programs, the phosphor, the configuration and the HB DOS volume -- written as a
// single versioned JSON document. `RetroComputerWindow::machineImage` and
// `applyMachineImage` are the two halves; `time_warp_player --machine` boots one
// with no IDE around it.
//
// #### Versioning
//
// `format` is the first field read and the first field checked. A file written by a
// newer build is **refused with a sentence** rather than half-read: a machine that
// silently lost the two fields it did not understand would be worse than one that
// said so, and the project's own `dialogs.cpp` sets that precedent for documents.
//
// The class knows nothing about widgets, like `HbDos`. The window owns the pixels.

#pragma once

#include <QJsonObject>
#include <QMap>
#include <QString>

#include "timewarp/ui/hb_dos.hpp"
#include "timewarp/ui/retro_settings.hpp"

namespace timewarp::ui {

/// The whole machine, in a form that survives a file.
struct MachineImage {
    /// The format this build writes and the newest it reads. A later format is a
    /// `load` failure with the numbers in the message.
    static constexpr int kFormat = 1;

    /// What the machine calls itself on the title bar and in the banner. A machine
    /// image is somebody's machine, so it has a name.
    QString name = QStringLiteral("HB-1");
    /// The ROM it was left in, as `settingsKey` returns it (`basic`, `logo`, ...).
    QString language = QStringLiteral("basic");
    /// The screen it was left showing, as `RetroComputerWindow::ScreenMode`.
    int mode = 2;
    /// Each ROM's program, by the same key.
    QMap<QString, QString> programs;
    /// The phosphor, the boot ROM, the text sizes, the glass -- everything the
    /// configuration screen holds.
    RetroComputerSettings config;
    /// The HB DOS volume's *contents*. The path is the machine's own and is not
    /// part of the image: a disk handed to somebody else is a volume, not a file
    /// on this reader's computer.
    QJsonObject volume;

    /// The document this struct is.
    [[nodiscard]] QJsonObject toJson() const;

    /// Reads `root` into `image`. False with a sentence -- never a half-read image
    /// -- for a document that is not a machine, or one from a newer build.
    static bool fromJson(const QJsonObject& root, MachineImage* image, QString* error);

    /// `<data dir>/machines`: where a machine named at the prompt lives. Not
    /// created by this call.
    [[nodiscard]] static QString defaultDirectory();
    /// `name` itself when it looks like a path (it contains a separator or ends in
    /// `.hb1`), otherwise `<defaultDirectory()>/<N>.hb1` with the name upper-cased,
    /// because the machine's own vocabulary shouts.
    [[nodiscard]] static QString resolvePath(const QString& name);

    /// Writes it as indented JSON, creating the directory. Nothing is written when
    /// the document cannot be made.
    [[nodiscard]] bool save(const QString& path, QString* error = nullptr) const;
    /// Reads one. A missing file is an error rather than a blank machine: loading is
    /// a deliberate act, and quietly getting an empty machine would lose the one
    /// that was there.
    static bool load(const QString& path, MachineImage* image,
                     QString* error = nullptr);
};

}  // namespace timewarp::ui
