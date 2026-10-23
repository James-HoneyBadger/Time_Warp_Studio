// SPDX-License-Identifier: MIT
//
// The HB-1: the ROM socket, the boot screen, the combined text and
// graphics display, the prompt, the two execution paths behind it, and HB DOS.
//
// These tests are deliberately about behaviour a user can see: the banner names
// the selected ROM, the prompt changes with it, a numbered line lands in the
// program in numeric order, `RUN` prints and draws, and each ROM remembers the
// program it was left holding. A screenshot can show the machine is green; only
// these can show it is a computer.

#include <cstdio>

#include <QAction>
#include <QApplication>
#include <QCheckBox>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QElapsedTimer>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSettings>
#include <QSlider>
#include <QTabWidget>
#include <QTextCursor>
#include <QThread>

#include "check.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/hb_memory.hpp"
#include "timewarp/ui/hb_clock.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/retro_computer.hpp"
#include "timewarp/ui/retro_settings.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

// The cases themselves are in `retro/*.cpp`, one file per part of the machine; this
// header is what they share: the settings wipe, the event-loop pump and the windows
// they build.

namespace tw::retro {


using timewarp::ui::BootBanner;
using timewarp::ui::DosFile;
using timewarp::ui::HbDos;
using timewarp::ui::HbMemory;
using timewarp::ui::HbTube;
using timewarp::ui::MachineVoice;
using timewarp::ui::CodeEditor;
using timewarp::ui::Language;
using timewarp::ui::MainWindow;
using timewarp::ui::PhosphorTint;
using timewarp::ui::RetroComputerSettings;
using timewarp::ui::RetroComputerSettingsDialog;
using timewarp::ui::RetroComputerWindow;
using timewarp::ui::retroPalette;
using timewarp::ui::TurtleCanvas;
using ScreenMode = RetroComputerWindow::ScreenMode;

template <class Predicate>
bool spin_until(Predicate predicate, int milliseconds = 5000) {
    QElapsedTimer clock;
    clock.start();
    while (clock.elapsed() < milliseconds) {
        QCoreApplication::processEvents(QEventLoop::AllEvents, 20);
        if (predicate()) {
            return true;
        }
        QThread::msleep(5);
    }
    QCoreApplication::processEvents(QEventLoop::AllEvents, 20);
    return predicate();
}

inline void clearMachineSettings() {
    QSettings settings;
    settings.remove(QStringLiteral("retroComputer"));
    settings.sync();
    // The disk is state too: a test that starts by wiping the machine must not
    // inherit a file the previous test saved.
    const QString image = timewarp::ui::HbDos::defaultImagePath();
    if (!image.isEmpty()) {
        QFile::remove(image);
    }
}

inline QStringList nonEmptyLines(const QString& text) {
    QStringList lines;
    for (const QString& line : text.split(QLatin1Char('\n'))) {
        if (!line.trimmed().isEmpty()) {
            lines << line;
        }
    }
    return lines;
}

/// The configuration screen, opened and accepted the way a person would.
inline RetroComputerSettingsDialog* openSettingsFor(RetroComputerWindow& machine) {
    auto* dialog = machine.openSettings();
    TW_CHECK(dialog != nullptr);
    return dialog;
}

inline void acceptSettings(RetroComputerSettingsDialog* dialog) {
    auto* buttons = dialog->findChild<QDialogButtonBox*>(
        QStringLiteral("retroSettingsButtons"));
    TW_CHECK(buttons != nullptr);
    if (buttons != nullptr) {
        buttons->button(QDialogButtonBox::Ok)->click();
    }
}

}  // namespace tw::retro
