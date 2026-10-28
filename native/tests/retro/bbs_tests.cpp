// SPDX-License-Identifier: MIT
//
// The board on the other end of the line: DIAL, the handle, the bulletins, a post
// and a log-off.
//
// What is asserted is the conversation a caller has and the message that survives it.
// The board is a file, so the test can read it back the way the next caller will --
// through a second window -- which is the only proof that "posted" meant anything.

#include "retro/support.hpp"
#include "timewarp/ui/hb_bbs.hpp"

namespace tw::retro {

namespace {

QString consoleOf(const RetroComputerWindow& machine) {
    return machine.consoleView()->toPlainText();
}

}  // namespace

void test_dial_connects_and_asks_for_a_handle() {
    clearMachineSettings();
    QFile::remove(timewarp::ui::HbBbs::defaultBoardPath());
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("DIAL"));

    const QString console = consoleOf(machine);
    TW_CHECK(console.contains(QStringLiteral("ATDT 555-0117")));
    TW_CHECK(console.contains(QStringLiteral("CONNECT 300")));
    TW_CHECK(console.contains(QStringLiteral("WELCOME TO WARP BOARD")));
    TW_CHECK(console.contains(QStringLiteral("ENTER A HANDLE")));
    TW_CHECK(machine.onlineState() == RetroComputerWindow::OnlineState::Handle);
    // The prompt says where the caller is, not which ROM is in the socket.
    TW_CHECK_EQ(machine.promptLabel()->text(), QStringLiteral("BBS>"));

    machine.submitCommand(QStringLiteral("ada"));
    TW_CHECK(machine.onlineState() == RetroComputerWindow::OnlineState::Menu);
    TW_CHECK_EQ(machine.bbsHandle(), QStringLiteral("ADA"));
    TW_CHECK(consoleOf(machine).contains(QStringLiteral("HELLO, ADA")));
}

void test_bulletins_list_and_read() {
    clearMachineSettings();
    QFile::remove(timewarp::ui::HbBbs::defaultBoardPath());
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("DIAL"));
    machine.submitCommand(QStringLiteral("ADA"));

    // A new board seeds one welcome message, because an empty room teaches nothing.
    machine.submitCommand(QStringLiteral("B"));
    TW_CHECK(consoleOf(machine).contains(QStringLiteral("WELCOME TO THE MACHINE")));
    machine.submitCommand(QStringLiteral("R 1"));
    const QString console = consoleOf(machine);
    TW_CHECK(console.contains(QStringLiteral("MESSAGE 1")));
    TW_CHECK(console.contains(QStringLiteral("FROM THE SYSOP")));
    TW_CHECK(console.contains(QStringLiteral("END OF MESSAGE")));
    // A number nobody posted is a refusal, not a blank page.
    machine.submitCommand(QStringLiteral("R 99"));
    TW_CHECK(consoleOf(machine).contains(QStringLiteral("NO MESSAGE 99")));
}

void test_posting_takes_more_than_one_line() {
    clearMachineSettings();
    const QString board = timewarp::ui::HbBbs::defaultBoardPath();
    QFile::remove(board);
    RetroComputerWindow machine;
    machine.submitCommand(QStringLiteral("DIAL"));
    machine.submitCommand(QStringLiteral("ADA"));
    machine.submitCommand(QStringLiteral("P A QUESTION"));
    TW_CHECK(machine.onlineState() == RetroComputerWindow::OnlineState::Posting);
    machine.submitCommand(QStringLiteral("how do you draw a circle in logo?"));
    machine.submitCommand(QString{});
    machine.submitCommand(QStringLiteral("asking for a friend"));
    machine.submitCommand(QStringLiteral("."));
    TW_CHECK(machine.onlineState() == RetroComputerWindow::OnlineState::Menu);
    TW_CHECK(consoleOf(machine).contains(QStringLiteral("POSTED AS MESSAGE 2")));

    // The blank line was a paragraph, not the end: the message has two paragraphs.
    const QString console = consoleOf(machine);
    machine.submitCommand(QStringLiteral("R 2"));
    const QString read = consoleOf(machine).mid(console.size());
    TW_CHECK(read.contains(QStringLiteral("how do you draw a circle in logo?")));
    TW_CHECK(read.contains(QStringLiteral("asking for a friend")));

    // A second window is a second caller: the message has to be on the board, not in
    // the window that wrote it.
    RetroComputerWindow next;
    next.submitCommand(QStringLiteral("DIAL"));
    next.submitCommand(QStringLiteral("B"));
    TW_CHECK(consoleOf(next).contains(QStringLiteral("A QUESTION")));
}

void test_an_empty_message_is_refused_and_bye_comes_home() {
    clearMachineSettings();
    QFile::remove(timewarp::ui::HbBbs::defaultBoardPath());
    RetroComputerWindow machine;
    const QString rom_prompt = machine.promptLabel()->text();
    machine.submitCommand(QStringLiteral("DIAL"));
    machine.submitCommand(QStringLiteral("ADA"));
    machine.submitCommand(QStringLiteral("P"));
    machine.submitCommand(QStringLiteral("."));
    TW_CHECK(consoleOf(machine).contains(QStringLiteral("EMPTY MESSAGE")));
    TW_CHECK(machine.onlineState() == RetroComputerWindow::OnlineState::Menu);

    machine.submitCommand(QStringLiteral("BYE"));
    TW_CHECK(machine.onlineState() == RetroComputerWindow::OnlineState::Off);
    TW_CHECK(consoleOf(machine).contains(QStringLiteral("NO CARRIER")));
    // The prompt goes back to whatever this ROM's prompt is: the board borrowed it
    // for as long as the line was up.
    TW_CHECK_EQ(machine.promptLabel()->text(), rom_prompt);

    // Back at the prompt, the machine's own words work again.
    machine.submitCommand(QStringLiteral("10 PRINT \"HOME\""));
    TW_CHECK(machine.program().contains(QStringLiteral("HOME")));

    // And a second call is greeted by name: the board remembers the handle.
    machine.submitCommand(QStringLiteral("DIAL"));
    TW_CHECK(consoleOf(machine).contains(QStringLiteral("WELCOME BACK, ADA")));
    TW_CHECK(machine.onlineState() == RetroComputerWindow::OnlineState::Menu);
}

void test_the_board_survives_a_corrupt_file() {
    clearMachineSettings();
    const QString board = timewarp::ui::HbBbs::defaultBoardPath();
    QFile::remove(board);
    {
        QDir{}.mkpath(QFileInfo{board}.absolutePath());
        QFile file{board};
        TW_CHECK(file.open(QIODevice::WriteOnly));
        file.write("this is not json");
    }
    // A board that cannot be read is refused and reported rather than silently
    // replaced: overwriting somebody's corrupt file would lose whatever they had.
    timewarp::ui::HbBbs reloaded;
    TW_CHECK(!reloaded.load(board));
    TW_CHECK(!reloaded.error().isEmpty());
    TW_CHECK_EQ(reloaded.count(), 0);
    QFile kept{board};
    TW_CHECK(kept.open(QIODevice::ReadOnly));
    TW_CHECK_EQ(QString::fromUtf8(kept.readAll()).toStdString(), std::string{"this is not json"});
}

}  // namespace tw::retro
