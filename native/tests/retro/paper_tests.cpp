// SPDX-License-Identifier: MIT
//
// The printer: LLIST and LPRINT, and the page they leave behind.
//
// The HB-1 has no printer port and never did; what a 1982 user had was a line
// printer that produced paper. The paper here is a file in the data directory, which
// is the same artefact -- something to keep, read and print elsewhere -- and the
// machine's console says where it went. What the test asserts is the page, not the
// message: a listing that printed nothing and a listing that printed forty lines
// both say "PRINTED".

#include "retro/support.hpp"

namespace tw::retro {

namespace {

QString readPaper(const RetroComputerWindow& machine) {
    if (machine.paperPath().isEmpty()) {
        return {};
    }
    QFile file{machine.paperPath()};
    if (!file.open(QIODevice::ReadOnly)) {
        return {};
    }
    return QString::fromUtf8(file.readAll());
}

}  // namespace

void test_llist_prints_the_program_to_the_paper_tray() {
    clearMachineSettings();
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("10 PRINT \"PAPER\""));
    machine.submitCommand(QStringLiteral("20 GOTO 10"));
    machine.submitCommand(QStringLiteral("LLIST"));

    const QString paper = readPaper(machine);
    TW_CHECK(paper.contains(QStringLiteral("LISTING")));
    TW_CHECK(paper.contains(QStringLiteral("10 PRINT \"PAPER\"")));
    TW_CHECK(paper.contains(QStringLiteral("20 GOTO 10")));
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("PRINTED")));

    // Printing twice appends: a printer that overwrote its own last page would lose
    // a program the user had already taken off the machine.
    machine.submitCommand(QStringLiteral("LLIST"));
    TW_CHECK_EQ(readPaper(machine).count(QStringLiteral("LISTING")), 2);
}

void test_lprint_prints_a_line_and_needs_one() {
    clearMachineSettings();
    RetroComputerWindow machine;
    // Nothing to print is a question, not a blank page.
    machine.submitCommand(QStringLiteral("LPRINT"));
    TW_CHECK(machine.paperPath().isEmpty());
    TW_CHECK(machine.consoleView()->toPlainText().contains(QStringLiteral("LPRINT WHAT")));

    machine.submitCommand(QStringLiteral("LPRINT \"HELLO PAPER\""));
    const QString paper = readPaper(machine);
    TW_CHECK(paper.contains(QStringLiteral("HELLO PAPER")));
}

}  // namespace tw::retro
