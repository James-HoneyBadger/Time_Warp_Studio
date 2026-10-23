// SPDX-License-Identifier: MIT
//
// Booting: the banner names the ROM, the prompt changes with it, every ROM boots, and
// the two execution paths behind the prompt do what they say.

#include "retro/support.hpp"

namespace tw::retro {

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

// Forth's `POKE` reaches the machine, not a private array: a program that writes
// the character cells of $8000 has drawn on the tube, which is what makes the ROM
// a language of *this* machine rather than a program running beside it.

void test_forth_poke_reaches_the_tube() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.setLanguage(Language::Forth);
    machine.loadProgram(QStringLiteral("65 32768 POKE 66 32769 POKE 67 32770 POKE"));

    bool finished = false;
    QObject::connect(&machine, &RetroComputerWindow::runFinished, &machine,
                     [&finished] { finished = true; });
    machine.runProgram();
    TW_CHECK(spin_until([&finished] { return finished; }));
    // $8000 is the first character cell, so the three bytes are the start of the
    // tube's top row -- the same row the banner wrote, which is the point: a POKE
    // and a PRINT write the same memory.
    const HbMemory* screen = machine.consoleView()->memory();
    TW_CHECK_EQ(static_cast<int>(screen->cell(0, 0)), 65);
    TW_CHECK_EQ(static_cast<int>(screen->cell(0, 1)), 66);
    TW_CHECK_EQ(static_cast<int>(screen->cell(0, 2)), 67);
    TW_CHECK(screen->rowText(0).startsWith(QStringLiteral("ABC")));
}


void test_forth_poke_refuses_rom() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.setLanguage(Language::Forth);
    // $F000 is the ROM region: the write cannot land, and the machine says so
    // rather than accepting it.
    machine.loadProgram(QStringLiteral("1 61440 POKE"));

    bool finished = false;
    QObject::connect(&machine, &RetroComputerWindow::runFinished, &machine,
                     [&finished] { finished = true; });
    machine.runProgram();
    TW_CHECK(spin_until([&finished] { return finished; }));
    // The refusal is reported through the machine's own output, which is the
    // transcript as well as the screen.
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("ROM")));
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

}  // namespace tw::retro
