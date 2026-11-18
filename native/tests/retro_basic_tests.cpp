// SPDX-License-Identifier: MIT
//
// The BASIC ROM's line editor: AUTO, RENUM with and without a target, LIST and DELETE
// over ranges, NEW's one undo, and TRON printing as it runs.

#include "retro/support.hpp"

namespace tw::retro {

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

}  // namespace tw::retro
