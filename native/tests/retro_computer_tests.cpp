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

namespace {

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
    TW_CHECK(machine.windowTitle().contains(QStringLiteral("HB-1")));

    const QString boot = machine.consoleView()->toPlainText();
    TW_CHECK(boot.contains(QStringLiteral("*** HB-1 ***")));
    TW_CHECK(boot.contains(QStringLiteral("BASIC V2.0")));
    TW_CHECK(boot.contains(QStringLiteral("64K RAM")));
    TW_CHECK(boot.contains(QStringLiteral("HB DOS 1.0")));
    TW_CHECK(boot.contains(QStringLiteral("HBWARP")));
    TW_CHECK(boot.contains(QStringLiteral("READY.")));

    TW_CHECK_EQ(machine.textTabs()->count(), 2);
    TW_CHECK_EQ(machine.textTabs()->tabText(0), QStringLiteral("PROGRAM"));
    // The OUTPUT page became the tube in 20.1: the machine's screen is memory now.
    TW_CHECK_EQ(machine.textTabs()->tabText(1), QStringLiteral("SCREEN"));
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
    // The menu names the machine, not the studio it lives in.
    TW_CHECK(action->text().contains(QStringLiteral("HB-1")));
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
    TW_CHECK(dialog->windowTitle().contains(QStringLiteral("HB-1")));
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
        TW_CHECK(boot.contains(QStringLiteral("*** HB-1 ***")));
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

// -- the machine's memory ---------------------------------------------------
//
// The 64K is the machine. These assertions are about the map itself: what a byte
// lands on, what the screen is, and that a scroll moves memory rather than a paint
// offset.

void test_memory_map_is_the_machine() {
    timewarp::ui::HbMemory memory;
    // Free RAM answers what was put there, and nothing else does.
    TW_CHECK(memory.poke(0x1000, 0x41));
    TW_CHECK_EQ(static_cast<int>(memory.peek(0x1000)), 0x41);
    TW_CHECK_EQ(static_cast<int>(memory.peek(0x1001)), 0);

    // A cell is a byte in the screen area, and its ink lives in the attribute area.
    memory.setCell(0, 0, 'H');
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kScreenBase)), int{'H'});
    memory.setAttribute(0, 0, 0x0A, 0x01);
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kAttributeBase)), 0xA1);
    TW_CHECK_EQ(static_cast<int>(memory.inkAt(0, 0)), 0x0A);
    TW_CHECK_EQ(static_cast<int>(memory.paperAt(0, 0)), 0x01);

    // ROM refuses a write and keeps its contents; everything below it lands.
    TW_CHECK(!memory.writable(HbMemory::kRomBase));
    TW_CHECK(!memory.poke(HbMemory::kRomBase, 0x55));
    TW_CHECK(memory.writable(HbMemory::kRomBase - 1));
    TW_CHECK(!memory.writable(HbMemory::kSize));
    TW_CHECK(!memory.writable(-1));

    // The I/O page is registers, not storage: the cursor is one of them.
    memory.setCursor(7, 33);
    TW_CHECK_EQ(memory.cursorRow(), 7);
    TW_CHECK_EQ(memory.cursorColumn(), 33);
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kIoBase + HbMemory::kCursorRow)), 7);
    memory.tickFrame();
    TW_CHECK_EQ(memory.frame(), 1);
    memory.setBorder(4);
    TW_CHECK_EQ(static_cast<int>(memory.border()), 4);
}

// A scroll that only moved a paint offset would leave PEEK answering with what used
// to be on the screen -- the exact lie this class exists to end.
void test_scrolling_moves_memory_and_peek_agrees() {
    timewarp::ui::HbMemory memory;
    memory.setCell(0, 0, 'A');
    memory.setCell(1, 0, 'B');
    memory.setCell(HbMemory::kRows - 1, 0, 'Z');
    memory.scrollUp();

    TW_CHECK_EQ(memory.cell(0, 0), std::uint8_t{'B'});
    TW_CHECK_EQ(memory.cell(HbMemory::kRows - 2, 0), std::uint8_t{'Z'});
    TW_CHECK_EQ(memory.cell(HbMemory::kRows - 1, 0), std::uint8_t{' '});
    TW_CHECK_EQ(static_cast<int>(memory.peek(HbMemory::kScreenBase)), int{'B'});
    // And the machine leaves a flag a program can notice the scroll by.
    TW_CHECK(memory.scrolled());
    memory.clearScrolled();
    TW_CHECK(!memory.scrolled());
}

void test_writing_text_wraps_and_scrolls() {
    timewarp::ui::HbMemory memory;
    // 80 columns: the 81st character goes to the next row, at the same ink.
    QString long_line(81, QLatin1Char('x'));
    memory.setInk(0x0C);
    memory.writeText(long_line);
    TW_CHECK_EQ(memory.rowText(0).size(), 80);
    TW_CHECK_EQ(memory.cell(1, 0), std::uint8_t{'x'});
    TW_CHECK_EQ(memory.inkAt(1, 0), 0x0C);
    // The write left the cursor at the start of the following row.
    TW_CHECK_EQ(memory.cursorRow(), 2);
    TW_CHECK_EQ(memory.cursorColumn(), 0);

    // Writing the last row and then ending the line scrolls: the text moves up one
    // and the new bottom row is blank, which is what a teletype does.
    memory.setCursor(HbMemory::kRows - 1, 0);
    memory.writeText(QStringLiteral("LAST"));
    TW_CHECK_EQ(memory.rowText(HbMemory::kRows - 2), QStringLiteral("LAST"));
    TW_CHECK_EQ(memory.rowText(HbMemory::kRows - 1), QString{});
    TW_CHECK(memory.scrolled());

    memory.home();
    TW_CHECK_EQ(memory.cursorRow(), 0);
    TW_CHECK_EQ(memory.cursorColumn(), 0);
}

void test_memory_checksum_changes_when_a_byte_does() {
    timewarp::ui::HbMemory memory;
    const std::uint16_t before = memory.checksum();
    memory.poke(0x2000, 1);
    TW_CHECK(memory.checksum() != before);
    memory.clear();
    TW_CHECK_EQ(memory.checksum(), before);
}

// -- the screen is memory ---------------------------------------------------
//
// The claim of 20.1: a character POKEd into screen memory is on the tube, and
// `LOCATE` moves the cursor `PRINT` then writes at. Before it, POKE wrote a map
// nothing painted and LOCATE validated its arguments and returned.

void test_a_poked_character_is_on_the_screen() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 POKE 32768, 72"));
    machine.submitCommand(QStringLiteral("20 POKE 32769, 73"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));

    // 0x8000 is row 0, column 0 of the tube, and PEEK reads the same bytes back.
    TW_CHECK(machine.memory().screenText().startsWith(QStringLiteral("HI")));
    TW_CHECK_EQ(static_cast<int>(machine.memory().peek(HbMemory::kScreenBase)), int{'H'});
    // Ink and paper are in the attribute page: the cell is not a colour, it is a nibble.
    TW_CHECK(machine.memory().inkAt(0, 0) != 0);
}

void test_locate_positions_the_print_that_follows() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 LOCATE 5, 10"));
    machine.submitCommand(QStringLiteral("20 PRINT \"HERE\""));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));

    // Row 5, column 10 is cell 4*80 + 9 in the screen area.
    const QString row = machine.memory().rowText(4);
    TW_CHECK(row.mid(9, 4) == QStringLiteral("HERE"));
    TW_CHECK_EQ(static_cast<int>(machine.memory().peek(HbMemory::kScreenBase + 4 * 80 + 9)),
                int{'H'});
    // Nothing was written on the row above it: the banner is there, "HERE" is not.
    TW_CHECK(!machine.memory().rowText(3).contains(QStringLiteral("HERE")));
}

void test_cls_clears_screen_memory() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 POKE 32768, 42"));
    machine.submitCommand(QStringLiteral("20 CLS"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    // The POKEd character is gone. The machine still says what it did -- a *log* said
    // "Screen cleared" and a screen that said nothing would be indistinguishable from
    // one that crashed -- but the ink it cleared is not there.
    TW_CHECK(static_cast<int>(machine.memory().peek(HbMemory::kScreenBase)) != 42);
    TW_CHECK(!machine.memory().screenText().contains(QLatin1Char('*')));
}

// A POKE into ROM cannot land, and a program that tries has to be told: the silent
// failure is the one this project treats as the bug.
void test_a_poke_into_rom_is_refused_out_loud() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 POKE 61440, 1"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("is ROM")));
}

// Writing past the bottom of the tube scrolls *memory*: what PEEK answers afterwards
// is what is on the screen.
void test_printing_past_the_bottom_scrolls_the_screen() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 FOR I = 1 TO 30"));
    machine.submitCommand(QStringLiteral("20 PRINT I"));
    machine.submitCommand(QStringLiteral("30 NEXT I"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 6000));
    // Thirty numbers went past a twenty-five row tube, so the banner that was on row
    // zero has scrolled off it -- and every number is still in the transcript, which
    // is what a reader scrolls back through.
    TW_CHECK(machine.memory().rowText(0) != QStringLiteral("*** HB-1 ***"));
    const QString transcript = machine.consoleView()->toPlainText();
    TW_CHECK(transcript.contains(QStringLiteral("29")));
    TW_CHECK(transcript.contains(QStringLiteral("30")));
}

// The graphics characters are the reason a machine with only a text screen could
// draw a picture: code 128 is a solid block, and POKEing it darkens a whole cell.
void test_a_graphics_character_paints_its_cell() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 CLS"));
    machine.submitCommand(QStringLiteral("20 POKE 32768, 128"));
    machine.submitCommand(QStringLiteral("30 POKE 32769, 32"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));

    HbTube* tube = machine.consoleView();
    TW_CHECK(tube != nullptr);
    tube->resize(tube->sizeHint());
    const QImage frame = tube->grab().toImage();
    const QRect block = tube->cellRect(0, 0).adjusted(1, 1, -1, -1);
    const QRect blank = tube->cellRect(0, 1).adjusted(1, 1, -1, -1);
    // The block's middle is ink; the space beside it is paper. A cell of a solid
    // block that came out empty would be a character the tube cannot draw, which is
    // what the sixteen graphics codes exist to prevent.
    TW_CHECK(frame.pixelColor(block.center()) != frame.pixelColor(blank.center()));
    TW_CHECK(frame.pixelColor(block.center()).lightness() >
             frame.pixelColor(blank.center()).lightness());
}

// -- the machine's own voice, its clock and its switch -----------------------
//
// Four things a machine does that a window cannot: it keeps time, it makes a noise
// when nothing asked it to, it counts its own memory at power-on, and it can be
// switched off.

void test_the_machine_sounds_like_a_machine() {
    clearMachineSettings();
    RetroComputerWindow machine;
    MachineVoice* voice = machine.voice();
    TW_CHECK(voice != nullptr);
    // A cold boot chirps.
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Boot), 1);
    // Typing clicks, once per line.
    machine.submitCommand(QStringLiteral("PRINT 1"));
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Key), 1);
    // A disk word rattles.
    machine.submitCommand(QStringLiteral("SAVE \"NOISE\""));
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Disk), 1);
    // And an error buzzes, which is the sound a reader of the period remembers as
    // *bad* -- and it is why the event is its own.
    machine.loadProgram(QStringLiteral("10 NOTACOMMAND"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Error), 1);

    // Every event has a shape a test can see, and no event is silent.
    for (const MachineVoice::Event event :
         {MachineVoice::Event::Boot, MachineVoice::Event::Key, MachineVoice::Event::Disk,
          MachineVoice::Event::Error, MachineVoice::Event::Tape}) {
        TW_CHECK(!MachineVoice::tonesFor(event).empty());
    }
}

// Muting is a promise about sound, not about history: the machine still did the thing.
void test_a_muted_machine_is_silent_but_still_counts() {
    clearMachineSettings();
    RetroComputerWindow machine;
    MachineVoice* voice = machine.voice();
    RetroComputerSettings quiet = machine.settings();
    quiet.voice_muted = true;
    machine.setSettings(quiet);
    TW_CHECK(voice->muted());

    const int sounded_before = voice->sounded();
    const int boot_before = voice->count(MachineVoice::Event::Boot);
    machine.reboot();
    TW_CHECK_EQ(voice->sounded(), sounded_before);
    TW_CHECK_EQ(voice->count(MachineVoice::Event::Boot), boot_before + 1);

    // And again with the volume at zero: audible nowhere, counted everywhere.
    quiet.voice_muted = false;
    quiet.voice_volume = 0;
    machine.setSettings(quiet);
    voice->play(MachineVoice::Event::Key);
    TW_CHECK_EQ(voice->sounded(), sounded_before);
}

// The sound latch is a register: a program pokes a code and the machine plays it on
// the next frame. A machine with no audio still clears it, so a program is not left
// sounding the same note forever.
void test_poking_the_sound_latch_reaches_the_voice() {
    clearMachineSettings();
    RetroComputerWindow machine;
    TW_CHECK(machine.voice() != nullptr);
    const int before = machine.voice()->requested();
    machine.submitCommand(QStringLiteral("10 POKE 40965, 30"));
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    // The clock ticks the latch; advance a frame by hand so the test does not sleep.
    machine.clock()->advance();
    TW_CHECK(machine.voice()->requested() > before);
    TW_CHECK_EQ(static_cast<int>(machine.memory().peek(HbMemory::kIoBase +
                                                      HbMemory::kSoundLatch)), 0);
}

// A self-test that prints "OK" without testing anything is the same lie as a
// hardcoded MEM line. This one walks the RAM and sums the ROM table, and says what
// it found.
void test_the_self_test_counts_the_memory_it_has() {
    clearMachineSettings();
    RetroComputerWindow machine;
    const QString boot = machine.consoleView()->toPlainText();
    TW_CHECK(boot.contains(QStringLiteral("HB-1 SELF TEST")));
    TW_CHECK(boot.contains(QStringLiteral("BYTES FREE")));
    TW_CHECK(boot.contains(QStringLiteral("RAM CHECK OK")));
    TW_CHECK(boot.contains(QStringLiteral("ROM CHECK OK (10 ROMS")));

    // Short and Off are promises about how much the machine says: neither can print a
    // self-test and still keep them.
    RetroComputerSettings short_banner = machine.settings();
    short_banner.banner = BootBanner::Short;
    machine.setSettings(short_banner);
    machine.reboot();
    TW_CHECK(!machine.consoleView()->toPlainText().contains(QStringLiteral("SELF TEST")));
}

void test_the_power_switch_turns_the_machine_off_and_on() {
    clearMachineSettings();
    RetroComputerWindow machine;
    TW_CHECK(machine.powered());
    const int boots = machine.voice()->count(MachineVoice::Event::Boot);

    machine.togglePower();
    TW_CHECK(!machine.powered());
    TW_CHECK(!machine.promptInput()->isEnabled());
    TW_CHECK(!machine.clock()->running());
    // Off is off: a line typed at a switched-off machine does nothing at all.
    const QString before = machine.consoleView()->toPlainText();
    machine.submitCommand(QStringLiteral("PRINT 1"));
    TW_CHECK_EQ(machine.consoleView()->toPlainText(), before);

    machine.togglePower();
    TW_CHECK(machine.powered());
    TW_CHECK(machine.promptInput()->isEnabled());
    TW_CHECK(machine.clock()->running());
    // Powering on is a cold boot, and a cold boot chirps.
    TW_CHECK_EQ(machine.voice()->count(MachineVoice::Event::Boot), boots + 1);
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("SELF TEST")));
}

// The clock is the machine's, and the frame register is how a program reads it.
void test_the_frame_register_counts_the_clock() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.clock()->stop();
    const std::uint16_t start = machine.memory().frame();
    machine.clock()->advance();
    machine.clock()->advance();
    machine.clock()->advance();
    TW_CHECK_EQ(machine.memory().frame(), static_cast<std::uint16_t>(start + 3));
}

// -- the BASIC line editor --------------------------------------------------
//
// A 1982 machine's BASIC is judged by its editor. These are the words the ROM
// had been missing, asserted through the prompt the way a user reaches them.

void test_auto_numbers_the_lines_that_are_typed() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("AUTO 100,10"));
    TW_CHECK(machine.autoNumbering());
    // The number is *in* the prompt, so the typed line already carries it.
    TW_CHECK_EQ(machine.promptInput()->text(), QStringLiteral("100"));
    machine.submitCommand(machine.promptInput()->text() + QStringLiteral(" PRINT \"ONE\""));
    TW_CHECK_EQ(machine.promptInput()->text(), QStringLiteral("110"));
    machine.submitCommand(machine.promptInput()->text() + QStringLiteral(" PRINT \"TWO\""));

    const QStringList lines = nonEmptyLines(machine.program());
    TW_CHECK_EQ(lines.size(), 2);
    TW_CHECK_EQ(lines.at(0), QStringLiteral("100 PRINT \"ONE\""));
    TW_CHECK_EQ(lines.at(1), QStringLiteral("110 PRINT \"TWO\""));

    // An empty line ends it, and the prompt goes back to being empty.
    machine.submitCommand(QString{});
    TW_CHECK(!machine.autoNumbering());
    TW_CHECK(machine.promptInput()->text().isEmpty());

    // With no number given, AUTO continues after the listing.
    machine.submitCommand(QStringLiteral("AUTO"));
    TW_CHECK_EQ(machine.promptInput()->text(), QStringLiteral("120"));
}

// The word with teeth. A renumber that misses a target silently changes what the
// program *does*, so the test runs the program before and after.
void test_renum_renumbers_the_listing_and_every_jump() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 GOTO 40"));
    machine.submitCommand(QStringLiteral("20 PRINT \"NEVER\""));
    machine.submitCommand(QStringLiteral("30 GOSUB 50"));
    machine.submitCommand(QStringLiteral("35 IF 1 THEN 60"));
    machine.submitCommand(QStringLiteral("40 PRINT \"BODY\""));
    machine.submitCommand(QStringLiteral("45 GOTO 70"));
    machine.submitCommand(QStringLiteral("50 PRINT \"SUB\""));
    machine.submitCommand(QStringLiteral("55 RETURN"));
    machine.submitCommand(QStringLiteral("60 PRINT \"THEN-ARM\""));
    machine.submitCommand(QStringLiteral("65 GOTO 70"));
    machine.submitCommand(QStringLiteral("70 PRINT \"END\""));
    machine.submitCommand(QStringLiteral("80 REM GOTO 40 IS NOT A JUMP"));
    machine.submitCommand(QStringLiteral("90 PRINT \"GOTO 40 IS TEXT\""));

    const QString before = machine.program();
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    const QStringList ran_before = nonEmptyLines(machine.consoleView()->toPlainText());

    machine.submitCommand(QStringLiteral("RENUM 100,10"));
    const QStringList after = nonEmptyLines(machine.program());
    TW_CHECK_EQ(after.size(), 13);
    // Every jump follows its target: 10→100, 40→130, 50→140, 60→150, 70→160.
    TW_CHECK(after.contains(QStringLiteral("100 GOTO 140")));
    TW_CHECK(after.contains(QStringLiteral("120 GOSUB 160")));
    TW_CHECK(after.contains(QStringLiteral("130 IF 1 THEN 180")));
    TW_CHECK(after.contains(QStringLiteral("150 GOTO 200")));
    TW_CHECK(after.contains(QStringLiteral("190 GOTO 200")));
    // A comment and a string that look like jumps are prose, byte for byte.
    TW_CHECK(after.contains(QStringLiteral("210 REM GOTO 40 IS NOT A JUMP")));
    TW_CHECK(after.contains(QStringLiteral("220 PRINT \"GOTO 40 IS TEXT\"")));

    // The program still does what it did.
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    const QStringList ran_after = nonEmptyLines(machine.consoleView()->toPlainText());
    for (const QString& line : ran_before) {
        if (line.startsWith(QStringLiteral("BODY")) || line.startsWith(QStringLiteral("SUB")) ||
            line.startsWith(QStringLiteral("END")) ||
            line.startsWith(QStringLiteral("THEN-ARM"))) {
            TW_CHECK(ran_after.contains(line));
        }
    }
    TW_CHECK(before.contains(QStringLiteral("10 GOTO 40")));
}

// A target that names no line is already a runtime error. Renumbering it would
// make it a different one, so it is left alone and counted.
void test_renum_leaves_a_target_it_cannot_resolve_alone() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 GOTO 900"));
    machine.submitCommand(QStringLiteral("20 END"));
    machine.submitCommand(QStringLiteral("RENUM"));
    const QStringList lines = nonEmptyLines(machine.program());
    TW_CHECK_EQ(lines.at(0), QStringLiteral("10 GOTO 900"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("1 TARGET(S) NOT IN THE PROGRAM")));

    // A bad argument is refused and changes nothing.
    machine.submitCommand(QStringLiteral("RENUM 10,0"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(
        QStringLiteral("?SYNTAX ERROR: RENUM")));
    TW_CHECK_EQ(nonEmptyLines(machine.program()).size(), 2);
}

void test_delete_and_list_take_ranges() {
    clearMachineSettings();
    RetroComputerWindow machine;
    for (int line = 10; line <= 50; line += 10) {
        machine.submitCommand(QStringLiteral("%1 PRINT \"%1\"").arg(line));
    }
    machine.submitCommand(QStringLiteral("LIST 20-40"));
    const QString listed = machine.consoleView()->toPlainText();
    TW_CHECK(listed.contains(QStringLiteral("20 PRINT \"20\"")));
    TW_CHECK(listed.contains(QStringLiteral("40 PRINT \"40\"")));
    TW_CHECK(listed.contains(QStringLiteral("10 PRINT \"10\"")));

    machine.submitCommand(QStringLiteral("DELETE 20-40"));
    const QStringList left = nonEmptyLines(machine.program());
    TW_CHECK_EQ(left.size(), 2);
    TW_CHECK_EQ(left.at(0), QStringLiteral("10 PRINT \"10\""));
    TW_CHECK_EQ(left.at(1), QStringLiteral("50 PRINT \"50\""));

    // A range with no lines in it says so rather than doing nothing.
    machine.submitCommand(QStringLiteral("DELETE 60-70"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("?NO LINES IN RANGE")));

    // And HB DOS's delete-a-file is still its own word.
    machine.submitCommand(QStringLiteral("SAVE \"RANGES\""));
    machine.submitCommand(QStringLiteral("DELETE \"RANGES\""));
    TW_CHECK(machine.disk().find(QStringLiteral("RANGES")) == nullptr);
}

void test_new_can_be_undone_once() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 PRINT \"ONE\""));
    machine.submitCommand(QStringLiteral("20 PRINT \"TWO\""));
    machine.submitCommand(QStringLiteral("NEW"));
    TW_CHECK(machine.program().trimmed().isEmpty());
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("TYPE UNDO")));

    machine.submitCommand(QStringLiteral("UNDO"));
    const QStringList restored = nonEmptyLines(machine.program());
    TW_CHECK_EQ(restored.size(), 2);
    TW_CHECK_EQ(restored.at(1), QStringLiteral("20 PRINT \"TWO\""));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("RESTORED 2 LINE(S)")));

    // One deep: a second UNDO has nothing to give back.
    machine.submitCommand(QStringLiteral("UNDO"));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("?NOTHING TO UNDO")));
}

void test_tron_prints_each_line_as_it_runs() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 A = 1"));
    machine.submitCommand(QStringLiteral("20 PRINT \"TRACED\""));
    machine.submitCommand(QStringLiteral("TRON"));
    TW_CHECK(machine.tracing());
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    const QString traced = machine.consoleView()->toPlainText();
    TW_CHECK(traced.contains(QStringLiteral("[10]")));
    TW_CHECK(traced.contains(QStringLiteral("[20]")));
    TW_CHECK(traced.contains(QStringLiteral("TRACED")));

    machine.submitCommand(QStringLiteral("TROFF"));
    TW_CHECK(!machine.tracing());
    machine.submitCommand(QStringLiteral("RUN"));
    TW_CHECK(spin_until([&machine] { return !machine.isRunning(); }, 4000));
    const QString untraced = machine.consoleView()->toPlainText();
    const int last = untraced.lastIndexOf(QStringLiteral("* RUN *"));
    TW_CHECK(last >= 0);
    TW_CHECK(!untraced.mid(last).contains(QStringLiteral("[10]")));
}

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

// A machine is a file: the ROM, the screen, every ROM's program, the phosphor and
// the disk, in one document. The round trip is what proves the image is the machine
// and not a summary of it.
void test_a_machine_survives_a_round_trip_through_a_file() {
    clearMachineSettings();
    const QString path = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests/HV.hb1");
    {
        RetroComputerWindow machine;
        machine.setLanguage(Language::Logo);
        machine.loadProgram(QStringLiteral("FORWARD 100"));
        machine.submitCommand(QStringLiteral("SAVE \"SQUARE\""));
        machine.setLanguage(Language::Basic);
        machine.loadProgram(QStringLiteral("20 PRINT \"HELLO\""));
        machine.setScreenMode(RetroComputerWindow::ScreenMode::Graphics);
        RetroComputerSettings config = machine.settings();
        config.tint = PhosphorTint::Amber;
        config.screen_text_size = 16;
        config.boot_language = static_cast<int>(Language::Pilot);
        machine.setSettings(config);

        QString error;
        TW_CHECK(machine.saveMachine(path, &error));
        TW_CHECK(error.isEmpty());
        TW_CHECK(QFileInfo::exists(path));
    }
    {
        RetroComputerWindow machine;
        QString error;
        TW_CHECK(machine.loadMachine(path, &error));
        TW_CHECK(error.isEmpty());
        // The ROM and the screen it was left in. `mode` is the machine's own enum
        // order -- 0 TEXT, 1 GRAPHICS, 2 SPLIT -- which the first version of the
        // manual had backwards, so it is pinned here rather than remembered.
        TW_CHECK_EQ(machine.language(), Language::Basic);
        TW_CHECK(machine.screenMode() == RetroComputerWindow::ScreenMode::Graphics);
        // The programs, including the one belonging to the ROM it is *not* in.
        TW_CHECK_EQ(machine.program(), QStringLiteral("20 PRINT \"HELLO\""));
        machine.setLanguage(Language::Logo);
        TW_CHECK_EQ(machine.program(), QStringLiteral("FORWARD 100"));
        TW_CHECK_EQ(machine.language(), Language::Logo);
        // The configuration: phosphor, text size and the boot ROM.
        TW_CHECK(machine.settings().tint == PhosphorTint::Amber);
        TW_CHECK_EQ(machine.settings().screen_text_size, 16);
        TW_CHECK_EQ(machine.settings().boot_language, static_cast<int>(Language::Pilot));
        // And the disk, which is a volume rather than a path.
        TW_CHECK(machine.disk().find(QStringLiteral("SQUARE")) != nullptr);
    }
}

// A refusal has to be a refusal: a machine image from a newer build must not be
// half-read, and a machine that failed to load must be the machine that was there.
void test_a_machine_image_refuses_what_it_cannot_read() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.loadProgram(QStringLiteral("10 PRINT \"SAFE\""));

    const QString newer = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests/FUTURE.hb1");
    {
        QFile file{newer};
        TW_CHECK(file.open(QIODevice::WriteOnly | QIODevice::Truncate));
        file.write(R"({"format": 7, "name": "FROM THE FUTURE"})");
    }
    QString error;
    TW_CHECK(!machine.loadMachine(newer, &error));
    TW_CHECK(error.contains(QStringLiteral("format 7")));
    TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"SAFE\""));

    const QString junk = QDir::tempPath() + QStringLiteral("/timewarp-retro-tests/JUNK.hb1");
    {
        QFile file{junk};
        TW_CHECK(file.open(QIODevice::WriteOnly | QIODevice::Truncate));
        file.write("not a machine at all");
    }
    error.clear();
    TW_CHECK(!machine.loadMachine(junk, &error));
    TW_CHECK(error.contains(QStringLiteral("not a machine image")));
    TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"SAFE\""));

    error.clear();
    TW_CHECK(!machine.loadMachine(QDir::tempPath() + QStringLiteral("/nope/NONE.hb1"), &error));
    TW_CHECK(error.contains(QStringLiteral("no machine image at")));
}

// The words, which are how a user reaches an image without a file dialog.
void test_the_machine_words_save_and_load_an_image() {
    clearMachineSettings();
    const QString path = timewarp::ui::MachineImage::resolvePath(QStringLiteral("hv"));
    QFile::remove(path);
    {
        RetroComputerWindow machine;
        machine.loadProgram(QStringLiteral("10 PRINT \"FROM THE MACHINE\""));
        machine.submitCommand(QStringLiteral("SAVE MACHINE \"HV\""));
        TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("MACHINE SAVED")));
        TW_CHECK(QFileInfo::exists(path));
    }
    {
        RetroComputerWindow machine;
        machine.submitCommand(QStringLiteral("LOAD MACHINE \"HV\""));
        TW_CHECK_EQ(machine.program(), QStringLiteral("10 PRINT \"FROM THE MACHINE\""));
    }
    {
        RetroComputerWindow machine;
        machine.submitCommand(QStringLiteral("LOAD MACHINE \"NOSUCH\""));
        TW_CHECK(machine.consoleView()->toPlainText().contains(
            QStringLiteral("NO MACHINE IMAGE")));
    }
    QFile::remove(path);
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
    test_memory_map_is_the_machine();
    test_the_machine_sounds_like_a_machine();
    test_a_muted_machine_is_silent_but_still_counts();
    test_poking_the_sound_latch_reaches_the_voice();
    test_the_self_test_counts_the_memory_it_has();
    test_the_power_switch_turns_the_machine_off_and_on();
    test_the_frame_register_counts_the_clock();
    test_a_poked_character_is_on_the_screen();
    test_a_graphics_character_paints_its_cell();
    test_locate_positions_the_print_that_follows();
    test_cls_clears_screen_memory();
    test_a_poke_into_rom_is_refused_out_loud();
    test_printing_past_the_bottom_scrolls_the_screen();
    test_scrolling_moves_memory_and_peek_agrees();
    test_writing_text_wraps_and_scrolls();
    test_memory_checksum_changes_when_a_byte_does();
    test_auto_numbers_the_lines_that_are_typed();
    test_renum_renumbers_the_listing_and_every_jump();
    test_renum_leaves_a_target_it_cannot_resolve_alone();
    test_delete_and_list_take_ranges();
    test_new_can_be_undone_once();
    test_tron_prints_each_line_as_it_runs();
    test_hb_dos_saves_catalogues_loads_and_runs();
    test_hb_dos_lock_protects_a_file();
    test_hb_dos_renames_and_types();
    test_hb_dos_verify_compares_memory_with_the_disk();
    test_hb_dos_init_has_to_be_said_twice();
    test_hb_dos_survives_a_restart();
    test_hb_dos_a_file_belongs_to_its_rom();
    test_hb_dos_refuses_bad_names_and_a_full_disk();
    test_a_machine_survives_a_round_trip_through_a_file();
    test_a_machine_image_refuses_what_it_cannot_read();
    test_the_machine_words_save_and_load_an_image();

    return tw::test::summary("retro_computer_tests");
}
