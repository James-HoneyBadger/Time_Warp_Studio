// SPDX-License-Identifier: MIT
//
// The language table and the editor widget: the mapping by position, and the widget
// the rest of these cases type into.

#include "ui_document/support.hpp"

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
    TW_CHECK_EQ(timewarp::ui::allLanguages().size(), 11);
    TW_CHECK_EQ(extensions(Language::Scheme).size(), 3);
    TW_CHECK_EQ(extensions(Language::Logo).size(), 2);
    TW_CHECK_EQ(extensions(Language::Focal).size(), 2);
    TW_CHECK_EQ(extensions(Language::Forth).size(), 2);
    TW_CHECK_EQ(extensions(Language::Chip8).size(), 2);
    TW_CHECK_EQ(extensions(Language::Apl).size(), 2);
    TW_CHECK(languageForFile(QStringLiteral("square.foc")) == Language::Focal);
    TW_CHECK(languageForFile(QStringLiteral("font.ch8")) == Language::Chip8);
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
