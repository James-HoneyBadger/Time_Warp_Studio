// SPDX-License-Identifier: MIT
//
// The configuration screen: what it previews, applies, persists, and what a stock
// machine looks like after `restore defaults`.

#include "retro/support.hpp"

namespace tw::retro {

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

}  // namespace tw::retro
