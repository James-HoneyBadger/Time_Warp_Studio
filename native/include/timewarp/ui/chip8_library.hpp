// SPDX-License-Identifier: MIT
//
// The CHIP-8 ROM library: a folder of programs the user supplies, and the dialog
// that browses it.
//
// Time Warp Studio ships no third-party ROMs. Most CHIP-8 programs that circulate
// online are somebody else's copyright, and several are unlicensed ports of games
// that still are; bundling them would be redistribution the project has no right
// to. The library is the lawful half: the user points it at a folder they have the
// right to use, and the IDE lists it.

#pragma once

#include <QDialog>
#include <QList>
#include <QString>

class QLabel;
class QLineEdit;
class QListWidget;
class QPushButton;

namespace timewarp::ui {

/// One program the library found.
struct Chip8Rom {
    QString path;     ///< Absolute, for loading.
    QString relative; ///< Inside the library folder: the games/ and hires/ grouping.
};

/// Every `.ch8`, `.c8`, `.rom` and `.bin` file under *folder*, recursively, sorted
/// by relative path. A missing or unreadable folder is an empty list rather than
/// an error: a folder that has been moved should read as empty, not as a failure
/// the user has to dismiss.
[[nodiscard]] QList<Chip8Rom> chip8RomsIn(const QString& folder);

/// The folder the library last used, from `QSettings`. Empty until one is chosen;
/// the dialog then starts at the user's home directory.
[[nodiscard]] QString storedChip8RomFolder();
/// Stores the folder. An empty string removes the key.
void storeChip8RomFolder(const QString& folder);

/// Browses the folder: a filter, the list, a button to pick another folder, and --
/// where the build has a network -- the one download this application offers: the
/// public-domain homebrew archive in `chip8_archive.hpp`.
///
/// A dialog rather than a panel because it is used once per program and then
/// dismissed; the CHIP-8 window keeps the folder, not the dialog.
class Chip8LibraryDialog final : public QDialog {
    Q_OBJECT

public:
    explicit Chip8LibraryDialog(const QString& folder, QWidget* parent = nullptr);

    /// The chosen program, or empty when the dialog was cancelled.
    [[nodiscard]] QString selectedPath() const;
    /// The folder the user ended up looking at, for the caller to remember.
    [[nodiscard]] QString folder() const { return folder_; }
    /// How many programs the folder contains, for a test.
    [[nodiscard]] int romCount() const;

private:
    void chooseFolder();
    void reload();
    void acceptSelection();
    /// Confirms, then fetches the catalogue into its own folder and shows it.
    void fetchArchive();
    /// Says, in the dialog, what the fetch is doing. Empty clears the line.
    void setArchiveStatus(const QString& text);

    QString folder_;
    QLabel* folder_label_ = nullptr;
    QLabel* empty_label_ = nullptr;
    QLineEdit* filter_ = nullptr;
    QListWidget* list_ = nullptr;
    QPushButton* open_ = nullptr;
    QPushButton* fetch_ = nullptr;
    QLabel* archive_status_ = nullptr;
};

}  // namespace timewarp::ui
