// SPDX-License-Identifier: MIT
//
// HB DOS: the prompt's half of the volume commands.

#include "retro_computer_internal.hpp"

#include <algorithm>
#include <optional>

#include <QCloseEvent>
#include <QComboBox>
#include <QFileInfo>
#include <QFont>
#include <QFrame>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPalette>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollBar>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSplitter>
#include <QTabWidget>
#include <QTextCursor>
#include <QVBoxLayout>
#include <QWidget>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace timewarp::ui {

using namespace retro_detail;

void RetroComputerWindow::diskAccessed() {
    if (voice_ != nullptr) {
        voice_->play(MachineVoice::Event::Disk);
    }
}

void RetroComputerWindow::listDisk() {
    diskAccessed();
    text_tabs_->setCurrentWidget(tube_);
    appendConsole(QStringLiteral("DISK VOLUME %1   %2   %3 SECTORS")
                      .arg(HbDos::kVolumeNumber)
                      .arg(dos_.volumeName())
                      .arg(HbDos::kTotalSectors));
    appendConsole(QString());
    if (dos_.files().isEmpty()) {
        appendConsole(QStringLiteral(" NO FILES"));
    } else {
        for (const DosFile& file : dos_.files()) {
            appendConsole(QStringLiteral("%1%2 %3  %4")
                              .arg(file.locked ? QLatin1Char('*') : QLatin1Char(' '))
                              .arg(dosTypeName(file.language).leftJustified(10))
                              .arg(HbDos::sectorsFor(file.source), 3, 10,
                                   QLatin1Char('0'))
                              .arg(file.name));
        }
    }
    appendConsole(QString());
    appendConsole(QStringLiteral("%1 FILES   %2 SECTORS USED   %3 FREE")
                      .arg(dos_.files().size())
                      .arg(dos_.usedSectors())
                      .arg(dos_.freeSectors()));
}

void RetroComputerWindow::showDiskStatus() {
    text_tabs_->setCurrentWidget(tube_);
    appendConsole(QStringLiteral("HB DOS 1.0   VOLUME %1").arg(dos_.volumeName()));
    appendConsole(QStringLiteral("%1 SECTORS   %2 USED   %3 FREE")
                      .arg(HbDos::kTotalSectors)
                      .arg(dos_.usedSectors())
                      .arg(dos_.freeSectors()));
    appendConsole(QStringLiteral("%1 FILES").arg(dos_.files().size()));
    appendConsole(QStringLiteral("IMAGE %1").arg(dos_.imagePath()));
}

void RetroComputerWindow::saveToDisk(const QString& name) {
    diskAccessed();
    if (name.isEmpty()) {
        appendConsole(QStringLiteral("?FILENAME NEEDED"));
        return;
    }
    if (program_ == nullptr || program_->toPlainText().trimmed().isEmpty()) {
        appendConsole(QStringLiteral("?NO PROGRAM"));
        return;
    }

    const QString source = program_->toPlainText();
    QString error;
    if (!dos_.write(name, settingsKey(language_), source, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 SAVED   %2 SECTORS")
                      .arg(HbDos::normaliseName(name))
                      .arg(HbDos::sectorsFor(source)));
    updateStatus();
}

void RetroComputerWindow::loadFromDisk(const QString& name) {
    diskAccessed();
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    const Language language = languageFromKey(file.language);
    if (language != Language::Unknown && language != language_) {
        // The file knows which ROM can run it, so loading it can cold-boot.
        setLanguage(language);
    }
    loadProgram(file.source);
    text_tabs_->setCurrentWidget(tube_);
    appendConsole(QStringLiteral("%1 LOADED").arg(file.name));
    updateStatus();
}

void RetroComputerWindow::runFromDisk(const QString& name) {
    diskAccessed();
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    const Language language = languageFromKey(file.language);
    if (language != Language::Unknown && language != language_) {
        setLanguage(language);
    }
    loadProgram(file.source);
    runProgram();
}

void RetroComputerWindow::eraseFromDisk(const QString& name) {
    diskAccessed();
    if (name.isEmpty()) {
        appendConsole(QStringLiteral("?FILENAME NEEDED"));
        return;
    }
    const QString erased = HbDos::normaliseName(name);
    QString error;
    if (!dos_.erase(name, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 DELETED").arg(erased));
    updateStatus();
}

void RetroComputerWindow::renameOnDisk(const QString& from, const QString& to) {
    diskAccessed();
    if (from.isEmpty() || to.isEmpty()) {
        appendConsole(QStringLiteral("?RENAME \"OLD\",\"NEW\""));
        return;
    }
    QString error;
    if (!dos_.rename(from, to, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 RENAMED TO %2")
                      .arg(HbDos::normaliseName(from), HbDos::normaliseName(to)));
}

void RetroComputerWindow::lockOnDisk(const QString& name, bool locked) {
    if (name.isEmpty()) {
        appendConsole(QStringLiteral("?FILENAME NEEDED"));
        return;
    }
    QString error;
    if (!dos_.setLocked(name, locked, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 %2")
                      .arg(HbDos::normaliseName(name),
                           locked ? tr("LOCKED") : tr("UNLOCKED")));
}

void RetroComputerWindow::verifyOnDisk(const QString& name) {
    diskAccessed();
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    // VERIFY compares the disk with memory, as it did on the machines this
    // imitates: load a file, edit it, and VERIFY is how you find out.
    if (program_ != nullptr && program_->toPlainText() == file.source) {
        appendConsole(QStringLiteral("%1 VERIFIED").arg(file.name));
    } else {
        appendConsole(QStringLiteral("?%1 DIFFERS FROM MEMORY").arg(file.name));
    }
}

void RetroComputerWindow::typeFromDisk(const QString& name) {
    diskAccessed();
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    text_tabs_->setCurrentWidget(tube_);
    appendConsole(QStringLiteral("%1  %2  %3 SECTORS")
                      .arg(file.name, dosTypeName(file.language))
                      .arg(HbDos::sectorsFor(file.source), 3, 10, QLatin1Char('0')));
    const QStringList lines = file.source.split(QLatin1Char('\n'));
    for (const QString& line : lines) {
        appendConsole(line);
    }
}

void RetroComputerWindow::formatDisk() {
    diskAccessed();
    QString error;
    if (!dos_.format(&error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("HB DOS 1.0   VOLUME %1").arg(dos_.volumeName()));
    appendConsole(QStringLiteral("ALL FILES ERASED"));
    updateStatus();
}

}  // namespace timewarp::ui
