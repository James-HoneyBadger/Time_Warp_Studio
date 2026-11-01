// SPDX-License-Identifier: MIT
//
// The tests `editor_services_tests.cpp` calls: the pty's byte stream and the finish
// notification's wording.
//
// No window and no shell: a byte stream in, a list of edits out, so each rule can
// be checked without a pseudo-terminal to produce it.

#include "editor_services/cases.hpp"

#include <cstdio>

#include <QByteArray>
#include <QDir>
#include <QFile>
#include <QSet>
#include <QString>
#include <QTemporaryDir>

#include <optional>
#include <string>
#include <vector>

#include "check.hpp"
#include "timewarp/ui/desktop_notification.hpp"
#include "timewarp/ui/terminal_text.hpp"

namespace tw::editor_services {

namespace {

using tw::test::fail;
using tw::test::show;

/// The suite's one-line banner for a case, repeated here because this file is a
/// second translation unit of the same binary rather than a suite of its own.
void announce(const std::string& name) { std::printf("-- %s\n", name.c_str()); }

}  // namespace

// ---------------------------------------------------------------------------
// The terminal's byte stream
//
// A pty writes escapes, carriage returns and split characters. The filter that
// turns that into the transcript is a function from bytes to edits, so each rule
// can be checked here rather than by watching a shell.
// ---------------------------------------------------------------------------

/// Everything the filter produced, as one string, with back-edits applied.
[[nodiscard]] std::string filtered(const std::vector<std::string>& reads) {
    timewarp::ui::TerminalText filter;
    QString text;
    for (const std::string& read : reads) {
        for (const timewarp::ui::TerminalOp& op : filter.feed(QByteArray::fromStdString(read))) {
            if (op.kind == timewarp::ui::TerminalOp::Kind::EraseBack) {
                text.chop(op.count);
            } else {
                text += op.text;
            }
        }
    }
    return text.toStdString();
}

void test_terminal_text_strips_escapes() {
    announce("terminal text: escapes");
    // Colour, and the bracket that a prompt puts round it.
    TW_CHECK_EQ(filtered({"\x1b[01;32mok\x1b[0m"}), std::string{"ok"});
    // An OSC window title, terminated by BEL and by ST.
    TW_CHECK_EQ(filtered({"\x1b]0;a title\x07x"}), std::string{"x"});
    TW_CHECK_EQ(filtered({"\x1b]0;a title\x1b\\y"}), std::string{"y"});
    // Bracketed paste, which a modern readline turns on at start-up.
    TW_CHECK_EQ(filtered({"\x1b[?2004h$ \x1b[?2004l"}), std::string{"$ "});
    // A sequence split across two reads is still one sequence.
    TW_CHECK_EQ(filtered({"\x1b[", "31mred"}), std::string{"red"});
    // The bell has no text; the tab does.
    // `\a` rather than `\x07`: a hex escape swallows the following letter.
    TW_CHECK_EQ(filtered({"a\ab\tc"}), std::string{"ab\tc"});
}

void test_terminal_text_applies_carriage_returns() {
    announce("terminal text: carriage returns");
    // The line discipline turns every newline into CRLF; that is one newline.
    TW_CHECK_EQ(filtered({"one\r\ntwo\r\n"}), std::string{"one\ntwo\n"});
    // A progress bar rewrites its line: what it printed last is what is left.
    TW_CHECK_EQ(filtered({"10%", "\r", "55%", "\r", "100%\n"}), std::string{"100%\n"});
    // A carriage return with nothing behind it yet is not a newline.
    timewarp::ui::TerminalText filter;
    static_cast<void>(filter.feed(QByteArray{"half"}));
    TW_CHECK(filter.feed(QByteArray{"\r"}).empty());
    TW_CHECK(filter.awaitingNewline());
    // A backspace takes one character back, and never past the line's start.
    TW_CHECK_EQ(filtered({"ab\x08"}), std::string{"a"});
    TW_CHECK_EQ(filtered({"\x08\x08"}), std::string{});
}

void test_terminal_text_survives_split_characters() {
    announce("terminal text: a character split across reads");
    // `é` is two bytes; a read can end between them, and half a character shows
    // as a replacement glyph.
    const std::string text = "caf\xC3\xA9";
    TW_CHECK_EQ(filtered({text.substr(0, 4), text.substr(4)}), std::string{"caf\xC3\xA9"});
    TW_CHECK_EQ(filtered({"caf\xC3", "\xA9"}), std::string{"caf\xC3\xA9"});
}


void test_the_finish_notification_says_one_thing() {
    announce("notification: what a finished run says");
    TW_CHECK_EQ(timewarp::ui::runNotificationBody(QStringLiteral("mandala.logo"), true,
                                                  QString{}),
                QStringLiteral("mandala.logo finished"));
    TW_CHECK_EQ(timewarp::ui::runNotificationBody(QStringLiteral("mandala.logo"), false,
                                                  QString{}),
                QStringLiteral("mandala.logo stopped"));
    // The first line of the error: a notification is a headline, and the console
    // already carries the whole sentence.
    TW_CHECK_EQ(timewarp::ui::runNotificationBody(
                    QStringLiteral("adventure.bas"), false,
                    QStringLiteral("\u274C line 40: undefined word\nthe two-line detail")),
                QStringLiteral("adventure.bas stopped with an error: \u274C line 40: "
                               "undefined word"));
    // A document with no name still reads as a sentence.
    TW_CHECK_EQ(timewarp::ui::runNotificationBody(QString{}, true, QString{}),
                QStringLiteral("The program finished"));
    // The setting has a home, and the default is to say something.
    TW_CHECK(QString::fromLatin1(timewarp::ui::kSettingNotifyOnFinish) ==
             QStringLiteral("notifications/onFinish"));
}


}  // namespace tw::editor_services
