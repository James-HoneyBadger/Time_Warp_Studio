// SPDX-License-Identifier: MIT
//
// The Time Warp: the ROM socket, the boot screen, the combined text and
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
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/retro_computer.hpp"
#include "timewarp/ui/retro_settings.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace {

using timewarp::ui::BootBanner;
using timewarp::ui::DosFile;
using timewarp::ui::HbDos;
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

void clearMachineSettings() {
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

QStringList nonEmptyLines(const QString& text) {
    QStringList lines;
    for (const QString& line : text.split(QLatin1Char('\n'))) {
        if (!line.trimmed().isEmpty()) {
            lines << line;
        }
    }
    return lines;
}

void test_boots_into_basic() {
    clearMachineSettings();
    RetroComputerWindow machine;

    TW_CHECK_EQ(machine.language(), Language::Basic);
    TW_CHECK(machine.languageSelector() != nullptr);
    TW_CHECK_EQ(machine.languageSelector()->count(),
                timewarp::ui::allLanguages().size());
    TW_CHECK_EQ(machine.languageSelector()->currentData().toInt(),
                static_cast<int>(Language::Basic));
    TW_CHECK_EQ(machine.screenMode(), ScreenMode::Split);
    TW_CHECK(machine.programEditor() != nullptr);
    TW_CHECK(machine.consoleView() != nullptr);
    TW_CHECK(machine.graphicsCanvas() != nullptr);
    TW_CHECK_EQ(machine.promptLabel()->text(), QStringLiteral("READY."));
    TW_CHECK(machine.windowTitle().contains(QStringLiteral("Time Warp")));

    const QString boot = machine.consoleView()->toPlainText();
    TW_CHECK(boot.contains(QStringLiteral("*** TIME WARP ***")));
    TW_CHECK(boot.contains(QStringLiteral("BASIC V2.0")));
    TW_CHECK(boot.contains(QStringLiteral("64K RAM")));
    TW_CHECK(boot.contains(QStringLiteral("HB DOS 1.0")));
    TW_CHECK(boot.contains(QStringLiteral("HBWARP")));
    TW_CHECK(boot.contains(QStringLiteral("READY.")));

    TW_CHECK_EQ(machine.textTabs()->count(), 2);
    TW_CHECK_EQ(machine.textTabs()->tabText(0), QStringLiteral("PROGRAM"));
    TW_CHECK_EQ(machine.textTabs()->tabText(1), QStringLiteral("OUTPUT"));
    TW_CHECK(!machine.graphicsCanvas()->gridVisible());
}

void test_rom_switch_reboots_and_keeps_memory() {
    clearMachineSettings();
    RetroComputerWindow machine;

    machine.setLanguage(Language::Logo);
    TW_CHECK_EQ(machine.language(), Language::Logo);
    TW_CHECK_EQ(machine.promptLabel()->text(), QStringLiteral("?"));
    TW_CHECK(machine.windowTitle().contains(QStringLiteral("Logo")));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("Logo V1.0 ROM")));

    machine.loadProgram(QStringLiteral("FORWARD 100"));
    TW_CHECK_EQ(machine.program(), QStringLiteral("FORWARD 100"));

    machine.setLanguage(Language::Scheme);
    TW_CHECK_EQ(machine.promptLabel()->text(), QStringLiteral(">"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("Scheme V84 ROM")));
    TW_CHECK(machine.program().trimmed().isEmpty());

    machine.setLanguage(Language::Logo);
    TW_CHECK_EQ(machine.program(), QStringLiteral("FORWARD 100"));
}

void test_every_rom_boots() {
    clearMachineSettings();
    RetroComputerWindow machine;
    for (const Language language : timewarp::ui::allLanguages()) {
        machine.setLanguage(language);
        TW_CHECK_EQ(machine.language(), language);
        TW_CHECK(!machine.promptLabel()->text().isEmpty());
        TW_CHECK(machine.consoleView()->toPlainText().contains(
            timewarp::ui::displayName(language)));
        TW_CHECK(machine.consoleView()->toPlainText().contains(
            QStringLiteral("READY.")));
    }
}

void test_basic_run_prints_and_supports_question_mark() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"ONE\"\n20 ? \"TWO\""));

    bool finished = false;
    QObject::connect(&machine, &RetroComputerWindow::runFinished, &machine,
                     [&finished] { finished = true; });
    machine.runProgram();
    TW_CHECK(spin_until([&finished] { return finished; }));
    const QString screen = machine.consoleView()->toPlainText();
    TW_CHECK(screen.contains(QStringLiteral("ONE")));
    TW_CHECK(screen.contains(QStringLiteral("TWO")));
}

void test_screen_modes_show_text_and_graphics() {
    clearMachineSettings();
    RetroComputerWindow machine;

    machine.setScreenMode(ScreenMode::Text);
    TW_CHECK_EQ(machine.screenMode(), ScreenMode::Text);
    TW_CHECK(!machine.textTabs()->isHidden());
    TW_CHECK(machine.graphicsCanvas()->parentWidget()->isHidden());
    TW_CHECK_EQ(machine.modeSelector()->currentData().toInt(),
                static_cast<int>(ScreenMode::Text));

    machine.setScreenMode(ScreenMode::Graphics);
    TW_CHECK(machine.textTabs()->isHidden());
    TW_CHECK(!machine.graphicsCanvas()->parentWidget()->isHidden());

    machine.setScreenMode(ScreenMode::Split);
    TW_CHECK(!machine.textTabs()->isHidden());
    TW_CHECK(!machine.graphicsCanvas()->parentWidget()->isHidden());
}

void test_basic_direct_mode_edits_the_program() {
    clearMachineSettings();
    RetroComputerWindow machine;

    machine.submitCommand(QStringLiteral("20 PRINT \"TWO\""));
    machine.submitCommand(QStringLiteral("10 PRINT \"ONE\""));
    machine.submitCommand(QStringLiteral("15 PRINT \"MID\""));
    TW_CHECK_EQ(nonEmptyLines(machine.program()),
                QStringList({QStringLiteral("10 PRINT \"ONE\""),
                             QStringLiteral("15 PRINT \"MID\""),
                             QStringLiteral("20 PRINT \"TWO\"")}));

    machine.submitCommand(QStringLiteral("15"));
    TW_CHECK_EQ(nonEmptyLines(machine.program()),
                QStringList({QStringLiteral("10 PRINT \"ONE\""),
                             QStringLiteral("20 PRINT \"TWO\"")}));

    machine.submitCommand(QStringLiteral("LIST"));
    const QString listing = machine.consoleView()->toPlainText();
    TW_CHECK(listing.contains(QStringLiteral("PRINT \"ONE\"")));
    TW_CHECK(listing.contains(QStringLiteral("PRINT \"TWO\"")));

    // A line that starts with a digit is not automatically a program line.
    machine.submitCommand(QStringLiteral("PRINT 2 + 2"));
    TW_CHECK(spin_until([&machine] {
        return machine.consoleView()->toPlainText().contains(QStringLiteral("\n4")) ||
               machine.consoleView()->toPlainText().endsWith(QStringLiteral("4"));
    }));
    TW_CHECK_EQ(nonEmptyLines(machine.program()).size(), 2);

    machine.submitCommand(QStringLiteral("?"));
    const QString help = machine.consoleView()->toPlainText();
    TW_CHECK(help.contains(QStringLiteral("run the program")));
    TW_CHECK(help.contains(QStringLiteral("HB DOS 1.0")));
    TW_CHECK(help.contains(QStringLiteral("CATALOG")));
    TW_CHECK(help.contains(QStringLiteral("SAVE")));

    machine.submitCommand(QStringLiteral("NEW"));
    TW_CHECK(machine.program().trimmed().isEmpty());
}

void test_prompt_keeps_repl_bindings() {
    clearMachineSettings();
    RetroComputerWindow machine;

    machine.submitCommand(QStringLiteral("X = 5"));
    machine.submitCommand(QStringLiteral("PRINT X"));
    TW_CHECK(spin_until([&machine] {
        const QString screen = machine.consoleView()->toPlainText();
        return screen.contains(QStringLiteral("PRINT X")) &&
               screen.contains(QStringLiteral("5"));
    }));
}

void test_run_prints_draws_and_switches_text_to_split() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.setLanguage(Language::Logo);
    machine.loadProgram(QStringLiteral("FORWARD 100\nRIGHT 90\nFORWARD 100"));
    machine.setScreenMode(ScreenMode::Text);

    bool finished = false;
    QObject::connect(&machine, &RetroComputerWindow::runFinished, &machine,
                     [&finished] { finished = true; });

    machine.runProgram();
    TW_CHECK(spin_until([&finished] { return finished; }));
    TW_CHECK(machine.graphicsCanvas()->strokeCount() >= 2);
    TW_CHECK_EQ(machine.screenMode(), ScreenMode::Split);
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("* RUN *")));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("READY.")));
}

void test_prompt_history_recalls_the_last_line() {
    clearMachineSettings();
    RetroComputerWindow machine;

    machine.submitCommand(QStringLiteral("PRINT 123"));
    machine.promptInput()->clear();
    machine.promptInput()->setFocus();

    QKeyEvent up{QEvent::KeyPress, Qt::Key_Up, Qt::NoModifier};
    QApplication::sendEvent(machine.promptInput(), &up);
    TW_CHECK_EQ(machine.promptInput()->text(), QStringLiteral("PRINT 123"));

    QKeyEvent down{QEvent::KeyPress, Qt::Key_Down, Qt::NoModifier};
    QApplication::sendEvent(machine.promptInput(), &down);
    TW_CHECK(machine.promptInput()->text().isEmpty());
}

void test_rom_and_program_survive_a_restart() {
    clearMachineSettings();
    {
        RetroComputerWindow machine;
        machine.setLanguage(Language::Scheme);
        machine.loadProgram(QStringLiteral("(define answer 42)"));
    }
    QSettings{}.sync();
    {
        RetroComputerWindow machine;
        TW_CHECK_EQ(machine.language(), Language::Scheme);
        TW_CHECK_EQ(machine.program(), QStringLiteral("(define answer 42)"));
    }
}

void test_main_window_owns_the_machine_action() {
    clearMachineSettings();
    MainWindow window;
    QAction* action = window.findChild<QAction*>(QStringLiteral("retroComputerAction"));
    TW_CHECK(action != nullptr);
    if (action == nullptr) {
        return;
    }
    action->trigger();
    auto* machine = window.findChild<RetroComputerWindow*>();
    TW_CHECK(machine != nullptr);
    if (machine != nullptr) {
        TW_CHECK(machine->isVisible());
        machine->close();
    }
}


// ---------------------------------------------------------------------------
// The configuration screen
// ---------------------------------------------------------------------------

RetroComputerSettingsDialog* openSettingsFor(RetroComputerWindow& machine) {
    auto* dialog = machine.openSettings();
    TW_CHECK(dialog != nullptr);
    return dialog;
}

void acceptSettings(RetroComputerSettingsDialog* dialog) {
    auto* buttons = dialog->findChild<QDialogButtonBox*>(
        QStringLiteral("retroSettingsButtons"));
    TW_CHECK(buttons != nullptr);
    if (buttons != nullptr) {
        buttons->button(QDialogButtonBox::Ok)->click();
    }
}

void test_config_button_opens_the_settings_screen() {
    clearMachineSettings();
    RetroComputerWindow machine;

    TW_CHECK(machine.configButton() != nullptr);
    TW_CHECK_EQ(machine.configButton()->text(), QStringLiteral("CONFIG"));

    RetroComputerSettingsDialog* dialog = openSettingsFor(machine);
    if (dialog == nullptr) {
        return;
    }
    TW_CHECK(dialog->isVisible());
    const auto* pages = dialog->findChild<QTabWidget*>(QStringLiteral("retroSettingsPages"));
    TW_CHECK(pages != nullptr);
    if (pages != nullptr) {
        TW_CHECK_EQ(pages->count(), 3);
        TW_CHECK_EQ(pages->tabText(0), QStringLiteral("Machine"));
        TW_CHECK_EQ(pages->tabText(1), QStringLiteral("Screen"));
        TW_CHECK_EQ(pages->tabText(2), QStringLiteral("Glass"));
    }
    // Every setting the dialog edits has a control: a page that looks complete
    // and quietly writes nothing is the failure this catches.
    TW_CHECK(dialog->findChild<QComboBox*>(QStringLiteral("retroBootLanguage")) != nullptr);
    TW_CHECK(dialog->findChild<QComboBox*>(QStringLiteral("retroBootScreen")) != nullptr);
    TW_CHECK(dialog->findChild<QComboBox*>(QStringLiteral("retroBootBanner")) != nullptr);
    TW_CHECK(dialog->findChild<QComboBox*>(QStringLiteral("retroHistoryDepth")) != nullptr);
    TW_CHECK(dialog->findChild<QCheckBox*>(QStringLiteral("retroRememberPrograms")) != nullptr);
    TW_CHECK(dialog->findChild<QCheckBox*>(QStringLiteral("retroSwitchToSplit")) != nullptr);
    TW_CHECK(dialog->findChild<QComboBox*>(QStringLiteral("retroPhosphor")) != nullptr);
    TW_CHECK(dialog->findChild<QSlider*>(QStringLiteral("retroScreenTextSize")) != nullptr);
    TW_CHECK(dialog->findChild<QSlider*>(QStringLiteral("retroEditorTextSize")) != nullptr);
    TW_CHECK(dialog->findChild<QSlider*>(QStringLiteral("retroCrtScanlines")) != nullptr);
    TW_CHECK(dialog->findChild<QSlider*>(QStringLiteral("retroCrtTint")) != nullptr);
    dialog->reject();
}

void test_config_prompt_command_opens_the_settings_screen() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("CONFIG"));
    TW_CHECK(spin_until([&machine] {
        return machine.findChild<RetroComputerSettingsDialog*>() != nullptr;
    }));
    auto* dialog = machine.findChild<RetroComputerSettingsDialog*>();
    TW_CHECK(dialog != nullptr);
    if (dialog != nullptr) {
        dialog->reject();
    }
}

void test_settings_preview_apply_and_persist() {
    clearMachineSettings();
    {
        RetroComputerWindow machine;

        RetroComputerSettingsDialog* dialog = openSettingsFor(machine);
        if (dialog == nullptr) {
            return;
        }
        auto* phosphor = dialog->findChild<QComboBox*>(QStringLiteral("retroPhosphor"));
        auto* screen_size =
            dialog->findChild<QSlider*>(QStringLiteral("retroScreenTextSize"));
        auto* editor_size =
            dialog->findChild<QSlider*>(QStringLiteral("retroEditorTextSize"));
        auto* scanlines = dialog->findChild<QSlider*>(QStringLiteral("retroCrtScanlines"));
        TW_CHECK(phosphor != nullptr && screen_size != nullptr && editor_size != nullptr &&
                 scanlines != nullptr);
        if (phosphor == nullptr || screen_size == nullptr || editor_size == nullptr ||
            scanlines == nullptr) {
            dialog->reject();
            return;
        }

        phosphor->setCurrentIndex(static_cast<int>(PhosphorTint::Amber));
        screen_size->setValue(16);
        editor_size->setValue(14);
        scanlines->setValue(0);

        // Preview: the machine has changed before OK, and has not written it yet.
        TW_CHECK(machine.settings().tint == PhosphorTint::Amber);
        TW_CHECK_EQ(machine.settings().screen_text_size, 16);
        TW_CHECK_EQ(machine.settings().editor_text_size, 14);
        TW_CHECK(machine.styleSheet().contains(retroPalette(PhosphorTint::Amber).border.name()));
        TW_CHECK_EQ(machine.programEditor()->font().pointSize(), 14);
        // A preview repaints; it does not write. The stored machine is still the
        // stock one until OK.
        TW_CHECK(RetroComputerSettings::load().tint == PhosphorTint::Green);

        acceptSettings(dialog);
        TW_CHECK(machine.settings().tint == PhosphorTint::Amber);

        // Rejecting after a change must put the previous look back.
        RetroComputerSettingsDialog* second = openSettingsFor(machine);
        if (second != nullptr) {
            auto* tint = second->findChild<QComboBox*>(QStringLiteral("retroPhosphor"));
            if (tint != nullptr) {
                tint->setCurrentIndex(static_cast<int>(PhosphorTint::Ice));
                TW_CHECK(machine.settings().tint == PhosphorTint::Ice);
            }
            second->reject();
            TW_CHECK(machine.settings().tint == PhosphorTint::Amber);
        }
    }

    QSettings{}.sync();
    RetroComputerWindow reopened;
    TW_CHECK(reopened.settings().tint == PhosphorTint::Amber);
    TW_CHECK_EQ(reopened.settings().screen_text_size, 16);
    TW_CHECK_EQ(reopened.settings().editor_text_size, 14);
    TW_CHECK_EQ(reopened.settings().crt.scanlines, 0);
    TW_CHECK(reopened.styleSheet().contains(retroPalette(PhosphorTint::Amber).border.name()));
}

void test_boot_settings_decide_the_next_power_on() {
    clearMachineSettings();
    {
        RetroComputerWindow machine;
        RetroComputerSettings configured;
        configured.boot_language = static_cast<int>(Language::Logo);
        configured.boot_mode = static_cast<int>(ScreenMode::Text);
        configured.banner = BootBanner::Short;
        machine.setSettings(configured);
        // Choosing a boot ROM is not a command to switch to it now.
        TW_CHECK_EQ(machine.language(), Language::Basic);
    }
    QSettings{}.sync();
    {
        RetroComputerWindow machine;
        TW_CHECK_EQ(machine.language(), Language::Logo);
        TW_CHECK_EQ(machine.screenMode(), ScreenMode::Text);
        const QString boot = machine.consoleView()->toPlainText();
        TW_CHECK(boot.contains(QStringLiteral("*** TIME WARP ***")));
        TW_CHECK(boot.contains(QStringLiteral("Logo")));
        TW_CHECK(!boot.contains(QStringLiteral("1982")));
        TW_CHECK(boot.contains(QStringLiteral("READY.")));
    }
    // Off leaves the screen empty, so nothing is printed on power-on at all.
    {
        RetroComputerWindow machine;
        RetroComputerSettings quiet;
        quiet.banner = BootBanner::Off;
        machine.setSettings(quiet);
        machine.reboot();
        TW_CHECK(machine.consoleView()->toPlainText().trimmed().isEmpty());
    }
}

void test_remember_programs_can_be_switched_off() {
    clearMachineSettings();
    {
        RetroComputerWindow machine;
        machine.loadProgram(QStringLiteral("10 PRINT \"KEPT\""));
        RetroComputerSettings forgetful;
        forgetful.remember_programs = false;
        machine.setSettings(forgetful);
    }
    QSettings{}.sync();
    {
        RetroComputerWindow machine;
        TW_CHECK(machine.program().trimmed().isEmpty());
    }
    // And back on: what is typed now survives, because the setting was written
    // with the programs rather than beside them.
    {
        RetroComputerWindow machine;
        RetroComputerSettings mindful;
        mindful.remember_programs = true;
        machine.setSettings(mindful);
        machine.loadProgram(QStringLiteral("20 PRINT \"KEPT\""));
    }
    QSettings{}.sync();
    {
        RetroComputerWindow machine;
        TW_CHECK_EQ(machine.program(), QStringLiteral("20 PRINT \"KEPT\""));
    }
}

void test_prompt_history_depth_is_honoured() {
    clearMachineSettings();
    RetroComputerWindow machine;
    RetroComputerSettings shallow;
    shallow.history_depth = 2;
    machine.setSettings(shallow);

    machine.submitCommand(QStringLiteral("PRINT 1"));
    machine.submitCommand(QStringLiteral("PRINT 2"));
    machine.submitCommand(QStringLiteral("PRINT 3"));
    machine.promptInput()->clear();
    machine.promptInput()->setFocus();

    QKeyEvent up{QEvent::KeyPress, Qt::Key_Up, Qt::NoModifier};
    QApplication::sendEvent(machine.promptInput(), &up);
    TW_CHECK_EQ(machine.promptInput()->text(), QStringLiteral("PRINT 3"));
    QApplication::sendEvent(machine.promptInput(), &up);
    TW_CHECK_EQ(machine.promptInput()->text(), QStringLiteral("PRINT 2"));
    // Two deep, so the first line has fallen off and Up stays put.
    QApplication::sendEvent(machine.promptInput(), &up);
    TW_CHECK_EQ(machine.promptInput()->text(), QStringLiteral("PRINT 2"));
}

void test_switch_to_split_can_be_turned_off() {
    clearMachineSettings();
    RetroComputerWindow machine;
    RetroComputerSettings stays_put;
    stays_put.switch_to_split = false;
    machine.setSettings(stays_put);

    machine.setLanguage(Language::Logo);
    machine.loadProgram(QStringLiteral("FORWARD 100"));
    machine.setScreenMode(ScreenMode::Text);

    bool finished = false;
    QObject::connect(&machine, &RetroComputerWindow::runFinished, &machine,
                     [&finished] { finished = true; });
    machine.runProgram();
    TW_CHECK(spin_until([&finished] { return finished; }));
    TW_CHECK(machine.graphicsCanvas()->strokeCount() > 0);
    TW_CHECK_EQ(machine.screenMode(), ScreenMode::Text);
}

void test_restore_defaults_returns_a_stock_machine() {
    clearMachineSettings();
    RetroComputerWindow machine;
    RetroComputerSettings configured;
    configured.tint = PhosphorTint::Amber;
    configured.screen_text_size = 20;
    configured.boot_language = static_cast<int>(Language::Scheme);
    configured.history_depth = 3;
    machine.setSettings(configured);

    RetroComputerSettingsDialog* dialog = openSettingsFor(machine);
    if (dialog == nullptr) {
        return;
    }
    dialog->restoreDefaults();
    TW_CHECK(dialog->settings().tint == PhosphorTint::Green);
    TW_CHECK_EQ(dialog->settings().boot_language, RetroComputerSettings::kLastUsed);
    TW_CHECK_EQ(dialog->settings().history_depth, 100);
    TW_CHECK_EQ(dialog->settings().screen_text_size, 11);
    // The preview follows Restore Defaults, so what is on screen is what the
    // dialog now says, not what it said when it opened.
    TW_CHECK(machine.settings().tint == PhosphorTint::Green);
    TW_CHECK_EQ(machine.settings().screen_text_size, 11);
    // The number beside a slider is written by the slider's own signal, which
    // Restore Defaults blocks, so the labels have to be refreshed by hand.
    auto* screen_value =
        dialog->findChild<QLabel*>(QStringLiteral("retroScreenTextSizeValue"));
    TW_CHECK(screen_value != nullptr);
    if (screen_value != nullptr) {
        TW_CHECK_EQ(screen_value->text(), QStringLiteral("11 pt"));
    }
    dialog->reject();
}


// ---------------------------------------------------------------------------
// HB DOS
// ---------------------------------------------------------------------------

void test_hb_dos_saves_catalogues_loads_and_runs() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"FROM DISK\""));

    machine.submitCommand(QStringLiteral("SAVE \"FIRST\""));
    TW_CHECK(machine.disk().find(QStringLiteral("FIRST")) != nullptr);

    machine.submitCommand(QStringLiteral("CATALOG"));
    const QString catalog = machine.consoleView()->toPlainText();
    TW_CHECK(catalog.contains(QStringLiteral("DISK VOLUME 254")));
    TW_CHECK(catalog.contains(QStringLiteral("HBWARP")));
    TW_CHECK(catalog.contains(QStringLiteral("BASIC")));
    TW_CHECK(catalog.contains(QStringLiteral("001")));  // one sector
    TW_CHECK(catalog.contains(QStringLiteral("FIRST")));
    TW_CHECK(catalog.contains(QStringLiteral("1 FILES")));

    machine.submitCommand(QStringLiteral("NEW"));
    TW_CHECK(machine.program().trimmed().isEmpty());

    machine.submitCommand(QStringLiteral("LOAD \"FIRST\""));
    TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"FROM DISK\""));

    machine.submitCommand(QStringLiteral("RUN \"FIRST\""));
    TW_CHECK(spin_until([&machine] {
        return machine.consoleView()->toPlainText().contains(QStringLiteral("FROM DISK"));
    }));
}

void test_hb_dos_lock_protects_a_file() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"KEEP\""));
    machine.submitCommand(QStringLiteral("SAVE \"KEEP\""));

    machine.submitCommand(QStringLiteral("LOCK \"KEEP\""));
    const DosFile* file = machine.disk().find(QStringLiteral("KEEP"));
    TW_CHECK(file != nullptr);
    TW_CHECK(file != nullptr && file->locked);

    machine.submitCommand(QStringLiteral("DELETE \"KEEP\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("?FILE LOCKED")));
    TW_CHECK(machine.disk().find(QStringLiteral("KEEP")) != nullptr);

    machine.submitCommand(QStringLiteral("CATALOG"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("*BASIC")));

    machine.submitCommand(QStringLiteral("UNLOCK \"KEEP\""));
    TW_CHECK(machine.disk().find(QStringLiteral("KEEP")) != nullptr);
    TW_CHECK(!machine.disk().find(QStringLiteral("KEEP"))->locked);
    machine.submitCommand(QStringLiteral("DELETE \"KEEP\""));
    TW_CHECK(machine.disk().find(QStringLiteral("KEEP")) == nullptr);
}

void test_hb_dos_renames_and_types() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"TYPED\""));

    machine.submitCommand(QStringLiteral("SAVE \"OLD\""));
    machine.submitCommand(QStringLiteral("RENAME \"OLD\",\"NEW\""));
    TW_CHECK(machine.disk().find(QStringLiteral("OLD")) == nullptr);
    TW_CHECK(machine.disk().find(QStringLiteral("NEW")) != nullptr);

    machine.submitCommand(QStringLiteral("TYPE \"NEW\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("10 PRINT \"TYPED\"")));

    // A rename onto a name that is taken is refused, not merged.
    machine.submitCommand(QStringLiteral("SAVE \"OTHER\""));
    machine.submitCommand(QStringLiteral("RENAME \"OTHER\",\"NEW\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("?FILE EXISTS")));
    TW_CHECK(machine.disk().find(QStringLiteral("OTHER")) != nullptr);
}

void test_hb_dos_verify_compares_memory_with_the_disk() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"SAME\""));
    machine.submitCommand(QStringLiteral("SAVE \"CHECK\""));

    machine.submitCommand(QStringLiteral("VERIFY \"CHECK\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("CHECK VERIFIED")));

    machine.loadProgram(QStringLiteral("20 PRINT \"CHANGED\""));
    machine.submitCommand(QStringLiteral("VERIFY \"CHECK\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("?CHECK DIFFERS FROM MEMORY")));
}

void test_hb_dos_init_has_to_be_said_twice() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"DOOMED\""));
    machine.submitCommand(QStringLiteral("SAVE \"DOOMED\""));

    machine.submitCommand(QStringLiteral("INIT"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("TYPE INIT AGAIN TO GO AHEAD")));
    TW_CHECK(machine.disk().find(QStringLiteral("DOOMED")) != nullptr);

    // Any other word is a change of mind: the next INIT warns again.
    machine.submitCommand(QStringLiteral("CATALOG"));
    machine.submitCommand(QStringLiteral("INIT"));
    TW_CHECK(machine.disk().find(QStringLiteral("DOOMED")) != nullptr);

    machine.submitCommand(QStringLiteral("INIT"));
    TW_CHECK(machine.disk().find(QStringLiteral("DOOMED")) == nullptr);
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("ALL FILES ERASED")));
}

void test_hb_dos_survives_a_restart() {
    clearMachineSettings();
    {
        RetroComputerWindow machine;
        machine.loadProgram(QStringLiteral("20 PRINT \"PERSIST\""));
        machine.submitCommand(QStringLiteral("SAVE \"PERSIST\""));
        TW_CHECK(machine.disk().find(QStringLiteral("PERSIST")) != nullptr);
    }
    {
        RetroComputerWindow machine;
        TW_CHECK(machine.disk().find(QStringLiteral("PERSIST")) != nullptr);
        machine.submitCommand(QStringLiteral("CATALOG"));
        TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("PERSIST")));
        TW_CHECK(machine.statusLabel()->text().contains(QStringLiteral("HB DOS 1F")));
    }
}

void test_hb_dos_a_file_belongs_to_its_rom() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.setLanguage(Language::Logo);
    machine.loadProgram(QStringLiteral("FORWARD 100"));
    machine.submitCommand(QStringLiteral("SAVE \"SQUARE\""));

    machine.setLanguage(Language::Basic);
    machine.submitCommand(QStringLiteral("CATALOG"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("LOGO")));

    // LOAD cold-boots the ROM that can run the file.
    machine.submitCommand(QStringLiteral("LOAD \"SQUARE\""));
    TW_CHECK_EQ(machine.language(), Language::Logo);
    TW_CHECK_EQ(machine.program(), QStringLiteral("FORWARD 100"));
}

void test_hb_dos_refuses_bad_names_and_a_full_disk() {
    clearMachineSettings();
    const QString image = QDir::tempPath() + QStringLiteral("/hb-dos-unit.dsk");
    QFile::remove(image);
    {
        HbDos dos{image};
        QString error;
        TW_CHECK_EQ(HbDos::normaliseName(QStringLiteral("  hello  ")),
                    QStringLiteral("HELLO"));
        TW_CHECK_EQ(HbDos::normaliseName(QStringLiteral("\"HELLO\"")),
                    QStringLiteral("HELLO"));
        TW_CHECK(HbDos::normaliseName(QStringLiteral("A,B"), &error).isEmpty());
        TW_CHECK_EQ(error, QStringLiteral("BAD FILE NAME"));
        TW_CHECK(HbDos::normaliseName(QString{}, &error).isEmpty());
        TW_CHECK(HbDos::normaliseName(QString(31, QLatin1Char('X')), &error).isEmpty());

        TW_CHECK_EQ(HbDos::sectorsFor(QStringLiteral("")), 1);
        TW_CHECK_EQ(HbDos::sectorsFor(QString(257, QLatin1Char('A'))), 2);

        // One file of 512 sectors fills the volume exactly, so the next save is
        // refused rather than silently truncated.
        const QString huge(512 * HbDos::kSectorSize, QLatin1Char('A'));
        TW_CHECK(dos.write(QStringLiteral("BIG"), QStringLiteral("basic"), huge, &error));
        TW_CHECK_EQ(dos.freeSectors(), 0);
        TW_CHECK(!dos.write(QStringLiteral("MORE"), QStringLiteral("basic"),
                            QStringLiteral("x"), &error));
        TW_CHECK_EQ(error, QStringLiteral("DISK FULL"));
        TW_CHECK(dos.find(QStringLiteral("BIG")) != nullptr);
        TW_CHECK(dos.find(QStringLiteral("MORE")) == nullptr);
    }
    QFile::remove(image);
}

}  // namespace

int main(int argc, char** argv) {
    qputenv("QT_QPA_PLATFORM", "offscreen");
    // HB DOS keeps its image under the application's data directory, so point
    // that at a scratch folder rather than a learner's real one.
    const QString data_dir = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests");
    QDir{data_dir}.removeRecursively();
    qputenv("TIME_WARP_DATA_DIR", data_dir.toUtf8());
    QApplication app{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("RetroComputerTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    test_boots_into_basic();
    test_rom_switch_reboots_and_keeps_memory();
    test_every_rom_boots();
    test_screen_modes_show_text_and_graphics();
    test_basic_direct_mode_edits_the_program();
    test_prompt_keeps_repl_bindings();
    test_basic_run_prints_and_supports_question_mark();
    test_run_prints_draws_and_switches_text_to_split();
    test_prompt_history_recalls_the_last_line();
    test_rom_and_program_survive_a_restart();
    test_main_window_owns_the_machine_action();
    test_config_button_opens_the_settings_screen();
    test_config_prompt_command_opens_the_settings_screen();
    test_settings_preview_apply_and_persist();
    test_boot_settings_decide_the_next_power_on();
    test_remember_programs_can_be_switched_off();
    test_prompt_history_depth_is_honoured();
    test_switch_to_split_can_be_turned_off();
    test_restore_defaults_returns_a_stock_machine();
    test_hb_dos_saves_catalogues_loads_and_runs();
    test_hb_dos_lock_protects_a_file();
    test_hb_dos_renames_and_types();
    test_hb_dos_verify_compares_memory_with_the_disk();
    test_hb_dos_init_has_to_be_said_twice();
    test_hb_dos_survives_a_restart();
    test_hb_dos_a_file_belongs_to_its_rom();
    test_hb_dos_refuses_bad_names_and_a_full_disk();

    return tw::test::summary("retro_computer_tests");
}
