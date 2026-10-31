// SPDX-License-Identifier: MIT
//
// The tests `editor_services_tests.cpp` calls: the pty's byte stream, the finish
// notification's wording, and the homebrew catalogue.
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
#include "timewarp/ui/chip8_archive.hpp"
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


// ---------------------------------------------------------------------------
// The homebrew catalogue
//
// The catalogue is the legal gate, not the button: every entry was cleared one at
// a time, and the file is what a reviewer reads to check that. These cases assert
// the properties a reviewer would check, plus the ones that keep a downloaded name
// inside the directory it is written to.
// ---------------------------------------------------------------------------

void test_the_archive_catalogue_is_a_cleared_list() {
    announce("archive: the catalogue");
    QString error;
    const std::optional<timewarp::ui::ArchiveCatalogue> catalogue =
        timewarp::ui::loadArchiveCatalogue(QStringLiteral(TIME_WARP_TEST_ARCHIVE), &error);
    TW_CHECK_EQ(error, QString{});
    TW_CHECK(catalogue.has_value());
    if (!catalogue) {
        return;
    }
    TW_CHECK_EQ(catalogue->repository, QStringLiteral("JohnEarnest/chip8Archive"));
    // A pinned commit, or the bytes can change under a running application.
    TW_CHECK_EQ(catalogue->commit.size(), 40);
    TW_CHECK(catalogue->base_url.contains(catalogue->commit));
    TW_CHECK(catalogue->base_url.endsWith(QStringLiteral("/roms/")));
    // The licence is stated once for the source and once per program: the second is
    // what a reviewer reads when they ask about one file.
    TW_CHECK_EQ(catalogue->licence, QStringLiteral("CC0-1.0"));
    TW_CHECK(!catalogue->programs.isEmpty());
    TW_CHECK(catalogue->programs.size() >= 10);

    QSet<QString> files;
    for (const timewarp::ui::ArchiveProgram& program : catalogue->programs) {
        TW_CHECK(program.file.endsWith(QStringLiteral(".ch8")));
        TW_CHECK(!program.title.isEmpty());
        TW_CHECK(!program.authors.isEmpty());
        TW_CHECK_EQ(program.licence, catalogue->licence);
        // A file name is a name, never a path: this is what stops a catalogue entry
        // writing outside the folder it was given.
        TW_CHECK(!program.file.contains(QLatin1Char('/')));
        TW_CHECK(!program.file.contains(QStringLiteral("..")));
        TW_CHECK(!files.contains(program.file));
        files.insert(program.file);

        // The URL a program is fetched from is the source and nothing else.
        const QUrl url = catalogue->urlFor(program);
        TW_CHECK(url.isValid());
        TW_CHECK_EQ(url.toString().toStdString(),
                    (catalogue->base_url + program.file).toStdString());
        TW_CHECK(url.toString().startsWith(QStringLiteral("https://")));
    }

    // A catalogue that is not one is refused rather than half-believed.
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString bad = dir.filePath(QStringLiteral("bad.json"));
    QFile file{bad};
    TW_CHECK(file.open(QIODevice::WriteOnly));
    file.write(R"({"programs":[{"file":"x.ch8","title":"X","licence":"CC0-1.0"}]})");
    file.close();
    TW_CHECK(!timewarp::ui::loadArchiveCatalogue(bad, &error).has_value());
    TW_CHECK(!error.isEmpty());

    // A file name that is a path is skipped, not fetched.
    const QString sneaky = dir.filePath(QStringLiteral("sneaky.json"));
    QFile other{sneaky};
    TW_CHECK(other.open(QIODevice::WriteOnly));
    other.write(R"({"source":{"base_url":"https://example.invalid/roms/","commit":"x"},)"
                R"("programs":[{"file":"../../etc/passwd","title":"no","authors":["a"],)"
                R"("licence":"CC0-1.0"}]})");
    other.close();
    const std::optional<timewarp::ui::ArchiveCatalogue> refused =
        timewarp::ui::loadArchiveCatalogue(sneaky, &error);
    TW_CHECK(!refused.has_value());

    // The fetched programs live in the application's own data directory, never in
    // the repository.
    const QString folder = timewarp::ui::archiveFolder();
    TW_CHECK(folder.endsWith(QStringLiteral("chip8/archive")));
    TW_CHECK(timewarp::ui::archiveFetchingAvailable());
}

}  // namespace tw::editor_services
