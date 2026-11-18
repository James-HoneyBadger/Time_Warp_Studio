// SPDX-License-Identifier: AGPL-3.0-only
//
// The language table and the editor widget: the mapping by position, and the widget
// the rest of these cases type into.

#include "ui_document/support.hpp"

#include "timewarp/ui/chip8_text.hpp"

#include <QTextDocument>

namespace tw::ui_document {

void test_language_mapping() {
    announce("language mapping");
    using timewarp::ui::displayName;
    using timewarp::ui::extensions;
    using timewarp::ui::languageForFile;
    using timewarp::ui::languageFromKey;
    using timewarp::ui::settingsKey;

    // Every extension the Python registry knows, both spellings of each. A file
    // the Python IDE calls BASIC has to be BASIC here too, or the two disagree
    // about what they are running.
    TW_CHECK(languageForFile(QStringLiteral("hello.bas")) == Language::Basic);
    TW_CHECK(languageForFile(QStringLiteral("hello.bsc")) == Language::Basic);
    TW_CHECK(languageForFile(QStringLiteral("hello.pilot")) == Language::Pilot);
    TW_CHECK(languageForFile(QStringLiteral("hello.plt")) == Language::Pilot);
    TW_CHECK(languageForFile(QStringLiteral("hello.logo")) == Language::Logo);
    TW_CHECK(languageForFile(QStringLiteral("hello.lg")) == Language::Logo);
    TW_CHECK(languageForFile(QStringLiteral("hello.comal")) == Language::Comal);
    TW_CHECK(languageForFile(QStringLiteral("hello.cml")) == Language::Comal);
    TW_CHECK(languageForFile(QStringLiteral("hello.rexx")) == Language::Rexx);
    TW_CHECK(languageForFile(QStringLiteral("hello.rex")) == Language::Rexx);
    TW_CHECK(languageForFile(QStringLiteral("hello.scm")) == Language::Scheme);
    TW_CHECK(languageForFile(QStringLiteral("hello.ss")) == Language::Scheme);
    TW_CHECK(languageForFile(QStringLiteral("hello.scheme")) == Language::Scheme);

    // A path, not just a name, and in whatever case the file system offers.
    TW_CHECK(languageForFile(QStringLiteral("/tmp/Examples/logo/Mandala.LOGO")) ==
             Language::Logo);

    // An unknown extension is Unknown, not a guess. Deciding that means BASIC is
    // the caller's job, and `defaultLanguage()` is where that decision lives.
    TW_CHECK(languageForFile(QStringLiteral("notes.txt")) == Language::Unknown);
    TW_CHECK(languageForFile(QStringLiteral("Makefile")) == Language::Unknown);
    TW_CHECK(languageForFile(QString{}) == Language::Unknown);
    TW_CHECK(timewarp::ui::defaultLanguage() == Language::Basic);

    // Display names are not the settings keys: one is read by people, the other
    // is persisted and carried over the wire, and they must not converge.
    TW_CHECK_EQ(displayName(Language::Logo), QStringLiteral("Logo"));
    TW_CHECK_EQ(displayName(Language::Rexx), QStringLiteral("REXX"));
    TW_CHECK_EQ(settingsKey(Language::Rexx), QStringLiteral("rexx"));
    TW_CHECK_EQ(settingsKey(Language::Basic), QStringLiteral("basic"));
    TW_CHECK(languageFromKey(QStringLiteral("scheme")) == Language::Scheme);
    TW_CHECK(languageFromKey(QStringLiteral("Logo")) == Language::Unknown);

    // Round trip: every language survives key -> language -> key.
    for (const Language language : timewarp::ui::allLanguages()) {
        TW_CHECK(languageFromKey(settingsKey(language)) == language);
    }
    // The one place the product's language count is pinned. A language added to
    // the table without being added here is the drift this catches.
    TW_CHECK_EQ(timewarp::ui::allLanguages().size(), 10);
    TW_CHECK_EQ(extensions(Language::Scheme).size(), 3);
    TW_CHECK_EQ(extensions(Language::Logo).size(), 2);
    TW_CHECK_EQ(extensions(Language::Focal).size(), 2);
    TW_CHECK_EQ(extensions(Language::Forth).size(), 2);
    TW_CHECK_EQ(extensions(Language::Apl).size(), 2);
    TW_CHECK(languageForFile(QStringLiteral("square.foc")) == Language::Focal);
    TW_CHECK(languageForFile(QStringLiteral("spiral.apl")) == Language::Apl);
    TW_CHECK(languageForFile(QStringLiteral("shapes.st")) == Language::Smalltalk);
    TW_CHECK(languageForFile(QStringLiteral("notes.smalltalk")) == Language::Smalltalk);

    // The filter has to offer every language, or the file dialog cannot reach it.
    const QString filter = timewarp::ui::openFileFilter();
    for (const Language language : timewarp::ui::allLanguages()) {
        TW_CHECK_EQ(filter.count(displayName(language)), 1);
        for (const QString& extension : extensions(language)) {
            TW_CHECK(filter.contains(QStringLiteral("*.%1").arg(extension)));
        }
    }
}

// A CHIP-8 ROM is bytes the editor has to carry. The two edges of the codec are
// the bytes a `QTextDocument` rewrites on its own -- a carriage return becomes a
// newline and a no-break space becomes a space -- so those are the ones a plain
// Latin-1 decode would lose, silently, on the next save.
void test_a_chip8_rom_round_trips_through_the_editor() {
    announce("a CHIP-8 ROM round-trips through the editor");
    using timewarp::ui::chip8RomExtensions;
    using timewarp::ui::isChip8RomFile;
    using timewarp::ui::romBytesToText;
    using timewarp::ui::romTextToBytes;

    QByteArray rom;
    for (int byte = 0; byte <= 0xFF; ++byte) {
        rom.append(static_cast<char>(byte));
    }
    // Every byte, and again through a real QTextDocument, which is what the editor
    // holds: the document's own normalisation must not reach the program.
    TW_CHECK_EQ(romTextToBytes(romBytesToText(rom)), rom);
    QTextDocument document;
    document.setPlainText(romBytesToText(rom));
    TW_CHECK_EQ(romTextToBytes(document.toPlainText()), rom);

    // A binary ROM opens read-only, because a keystroke in bytes is a corrupt
    // program; the listing in the same extension stays editable source.
    QTemporaryDir dir;
    const QString rom_path = writeFile(
        dir, QStringLiteral("binary.ch8"),
        QByteArray{"\x60\x0B\x00\x00", 4});
    TW_CHECK(!rom_path.isEmpty());
    MainWindow window;
    TW_CHECK(window.openFile(rom_path));
    CodeEditor* opened = window.currentEditor();
    TW_CHECK(opened != nullptr);
    if (opened != nullptr) {
        TW_CHECK(opened->isReadOnly());
        TW_CHECK_EQ(timewarp::ui::romTextToBytes(opened->toPlainText()),
                    (QByteArray{"\x60\x0B\x00\x00", 4}));
    }
    const QString listing_path = writeFile(
        dir, QStringLiteral("listing.ch8"), QByteArray{"; a listing\nstart: HALT\n"});
    TW_CHECK(window.openFile(listing_path));
    CodeEditor* listing = window.currentEditor();
    TW_CHECK(listing != nullptr);
    TW_CHECK(listing == nullptr || !listing->isReadOnly());

    // The suffixes the emulator's library browses for, so one file is one program
    // wherever it is opened from.
    for (const QString& extension : chip8RomExtensions()) {
        TW_CHECK(isChip8RomFile(QStringLiteral("paint.") + extension));
        TW_CHECK(isChip8RomFile(QStringLiteral("PAINT.") + extension.toUpper()));
    }
    TW_CHECK(!isChip8RomFile(QStringLiteral("paint.bas")));

    // The open dialog has to reach them, or the editor cannot open what it runs.
    const QString filter = timewarp::ui::openFileFilter();
    TW_CHECK(filter.contains(QStringLiteral("CHIP-8")));
    TW_CHECK(filter.contains(QStringLiteral("*.ch8")));
}

// ---------------------------------------------------------------------------
// The editor widget
// ---------------------------------------------------------------------------


void test_editor_widget() {
    announce("editor widget");
    CodeEditor editor;
    TW_CHECK_EQ(editor.objectName(), QStringLiteral("codeEditor"));

    // A tab is four spaces wide. The point is not the number but that it is the
    // same number as the Python editor's: a file re-saved by either must not
    // reflow.
    editor.setPlainText(QStringLiteral("x"));
    TW_CHECK(editor.tabStopDistance() > 0.0);

    // The gutter has to fit the largest line number, and it may not jitter as the
    // document grows from 9 lines to 10.
    const int narrow = editor.lineNumberAreaWidth();
    editor.setPlainText(QStringLiteral("line\n").repeated(200));
    TW_CHECK(editor.lineNumberAreaWidth() >= narrow);
    editor.setPlainText(QStringLiteral("one"));
    TW_CHECK(editor.lineNumberAreaWidth() >= 24);

    // Cursor reporting is 1-based, because that is what the status bar and every
    // error message the interpreters produce use.
    editor.setPlainText(QStringLiteral("alpha\nbeta\ngamma"));
    QTextCursor cursor = editor.textCursor();
    cursor.movePosition(QTextCursor::End);
    editor.setTextCursor(cursor);
    TW_CHECK_EQ(editor.currentLine(), 3);
    TW_CHECK_EQ(editor.currentColumn(), 6);

    // Going to a line that does not exist lands on the last one rather than doing
    // nothing: "go to line 9999" of a three-line file has an obvious intent.
    editor.goToLine(99);
    TW_CHECK_EQ(editor.currentLine(), 3);
    editor.goToLine(0);
    TW_CHECK_EQ(editor.currentLine(), 1);
    editor.goToLine(2);
    TW_CHECK_EQ(editor.currentLine(), 2);

    // The position travels on the signal, not only in a getter, because the
    // status bar is updated by a connection rather than by polling.
    int reportedLine = 0;
    int reportedColumn = 0;
    QObject::connect(&editor, &CodeEditor::cursorMoved, &editor,
                     [&reportedLine, &reportedColumn](int line, int column) {
                         reportedLine = line;
                         reportedColumn = column;
                     });
    editor.goToLine(3);
    TW_CHECK_EQ(reportedLine, 3);
    TW_CHECK_EQ(reportedColumn, 1);
}

// ---------------------------------------------------------------------------
// Documents
// ---------------------------------------------------------------------------

}  // namespace tw::ui_document
