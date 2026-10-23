// SPDX-License-Identifier: MIT
//
// A document's life: open, raise the tab that is already open, edit, choose a
// language, close, save, and the two files that are read rather than parsed.

#include "ui_document/support.hpp"

namespace tw::ui_document {

// Read by this file only: it arrived here with the cases that need it.

/// The editor in the window's current tab.
[[nodiscard]] CodeEditor* currentEditorOf(const MainWindow& window) {
    return qobject_cast<CodeEditor*>(window.editorTabs()->currentWidget());
}

void test_a_fresh_window_has_one_real_document() {
    announce("fresh window document");
    MainWindow window;

    // The Python calls `new_file()` while building the UI. Without the equivalent
    // the editor pane is an empty box and the tab strip is invisible, which reads
    // as a broken column rather than as an IDE with no document open.
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK(currentEditorOf(window) != nullptr);
    TW_CHECK_EQ(window.editorTabs()->tabText(0), QStringLiteral("untitled"));
    TW_CHECK(window.openFilePaths().isEmpty());
    TW_CHECK(badgeText(window, "languageBadge").contains(QStringLiteral("BASIC")));
    TW_CHECK(window.windowTitle().contains(QStringLiteral("untitled")));

    // A second document is numbered, so the two are distinguishable in the strip.
    CodeEditor* second = window.newFile();
    TW_CHECK_EQ(window.editorTabs()->count(), 2);
    TW_CHECK_EQ(window.editorTabs()->tabText(1), QStringLiteral("untitled 2"));
    TW_CHECK_EQ(window.editorTabs()->currentWidget(), second);
    TW_CHECK_EQ(window.editorTabs()->tabText(0), QStringLiteral("untitled"));
}


void test_opening_a_file() {
    announce("opening a file");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString path = writeFile(dir, QStringLiteral("mandala.logo"),
                                   QByteArray{"FD 100\nRT 90\nFD 100\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));

    // Opening adds a tab rather than replacing the welcome document: what is
    // already in the editor is someone's work, and it is not the file dialog's
    // business to discard it.
    TW_CHECK_EQ(window.editorTabs()->count(), 2);
    CodeEditor* editor = currentEditorOf(window);
    TW_CHECK(editor != nullptr);
    if (editor != nullptr) {
        TW_CHECK_EQ(editor->toPlainText(), QStringLiteral("FD 100\nRT 90\nFD 100\n"));
        // A file just opened is not dirty. A stray modified flag would prompt for a
        // save of something the user has not touched.
        TW_CHECK(!editor->document()->isModified());
    }

    // The tab takes the file's name, the badges take its language, and the
    // breadcrumb takes the path.
    TW_CHECK_EQ(window.editorTabs()->tabText(1), QStringLiteral("mandala.logo"));
    TW_CHECK(badgeText(window, "languageBadge").contains(QStringLiteral("Logo")));
    TW_CHECK(badgeText(window, "breadcrumb").contains(QStringLiteral("mandala.logo")));
    // Bound to a local first: `at()` returns a reference into the temporary list,
    // and comparing that through the check macro binds a reference to an object
    // that is already gone.
    const QStringList open = window.openFilePaths();
    TW_CHECK_EQ(open.size(), 1);
    TW_CHECK_EQ(open.at(0), path);

    // The language selector follows the document, without being told.
    const auto* selector =
        window.findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(selector != nullptr);
    if (selector != nullptr) {
        TW_CHECK_EQ(selector->currentData().toString(), QStringLiteral("logo"));
    }
}


void test_opening_the_same_file_twice_raises_the_first_tab() {
    announce("reopening a file");
    QTemporaryDir dir;
    const QString path = writeFile(dir, QStringLiteral("loops.bas"),
                                   QByteArray{"FOR I = 1 TO 3\nNEXT I\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    const int afterFirst = window.editorTabs()->count();
    CodeEditor* first = currentEditorOf(window);

    // A second copy of one file means the last save wins and the other window's
    // edits disappear, with no undo that can reach them. Raising the existing tab
    // is the only safe answer.
    TW_CHECK(window.openFile(path));
    TW_CHECK_EQ(window.editorTabs()->count(), afterFirst);
    TW_CHECK_EQ(window.editorTabs()->currentWidget(), first);

    // The same file reached by a different spelling of its path is still the same
    // file: `/tmp/x/../x/a.bas` and `/tmp/x/a.bas` are one document.
    const QString indirect =
        dir.path() + QStringLiteral("/./loops.bas");
    TW_CHECK(window.openFile(indirect));
    TW_CHECK_EQ(window.editorTabs()->count(), afterFirst);
}


void test_moving_the_cursor_updates_the_status_bar() {
    announce("cursor position reporting");
    MainWindow window;
    CodeEditor* editor = currentEditorOf(window);
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }
    editor->setPlainText(QStringLiteral("one\ntwo\nthree"));
    editor->goToLine(3);
    TW_CHECK_EQ(badgeText(window, "positionBadge"), QStringLiteral("Ln 3, Col 1"));
    editor->goToLine(1);
    TW_CHECK_EQ(badgeText(window, "positionBadge"), QStringLiteral("Ln 1, Col 1"));
}


void test_editing_marks_the_document_and_the_title() {
    announce("modification markers");
    MainWindow window;
    CodeEditor* editor = currentEditorOf(window);
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }

    TW_CHECK(!window.windowTitle().endsWith(QLatin1Char('*')));
    editor->insertPlainText(QStringLiteral("PRINT 1"));
    TW_CHECK(editor->document()->isModified());

    // Both markers, because they answer different questions: the tab strip says
    // which document is dirty, the title bar says that this window is.
    TW_CHECK(window.editorTabs()->tabText(0).endsWith(QStringLiteral(" \u25CF")));
    TW_CHECK(window.windowTitle().endsWith(QLatin1Char('*')));

    editor->document()->setModified(false);
    TW_CHECK(!window.editorTabs()->tabText(0).endsWith(QStringLiteral(" \u25CF")));
    TW_CHECK(!window.windowTitle().endsWith(QLatin1Char('*')));
}


void test_choosing_a_language_sticks() {
    announce("language override");
    MainWindow window;
    auto* selector = window.findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(selector != nullptr);
    if (selector == nullptr) {
        return;
    }

    // A choice made by hand has to survive the next refresh, which would otherwise
    // derive the language from the document's name and undo it. An unnamed
    // document has no name to derive from, so this is the case that proves the
    // override is stored rather than merely displayed.
    selector->setCurrentIndex(selector->findData(QStringLiteral("rexx")));
    TW_CHECK(badgeText(window, "languageBadge").contains(QStringLiteral("REXX")));

    // Switching documents and back must not move the choice onto the other
    // document, which is what an unguarded `currentIndexChanged` would do.
    window.newFile();
    TW_CHECK(badgeText(window, "languageBadge").contains(QStringLiteral("BASIC")));
    window.editorTabs()->setCurrentIndex(0);
    TW_CHECK(badgeText(window, "languageBadge").contains(QStringLiteral("REXX")));
}


void test_closing_tabs() {
    announce("closing tabs");
    MainWindow window;
    window.newFile();
    window.newFile();
    TW_CHECK_EQ(window.editorTabs()->count(), 3);

    // Nothing is modified, so nothing may be asked about: the unsaved-changes
    // dialog is a decision for the user, not a step in every close.
    TW_CHECK(window.closeCurrentTab());
    TW_CHECK_EQ(window.editorTabs()->count(), 2);
    TW_CHECK(window.closeTab(0));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);

    // Closing the last document leaves a fresh one behind. An editor area of empty
    // grey reads as a window that has broken rather than as an IDE waiting.
    TW_CHECK(window.closeTab(0));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK(currentEditorOf(window) != nullptr);

    // An index that does not exist is refused rather than removing a neighbouring
    // tab, which is what an unchecked `removeTab(-1)` does not do but a clamped one
    // would.
    TW_CHECK(!window.closeTab(99));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
}


void test_saving_a_document() {
    announce("saving");
    QTemporaryDir dir;
    const QString path = writeFile(dir, QStringLiteral("target.logo"), QByteArray{});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    CodeEditor* editor = currentEditorOf(window);
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }

    // Load through the real open path so the document carries a path, then edit
    // and save: this is the round trip a user performs, minus the two dialogs,
    // which cannot be driven from a test without hanging.
    TW_CHECK(window.openFile(path));
    editor = currentEditorOf(window);
    // `insertPlainText` rather than `setPlainText`: replacing the whole document
    // clears the modification flag, so it cannot be used to make one dirty.
    editor->insertPlainText(QStringLiteral("REPEAT 4 [FD 50 RT 90]"));
    TW_CHECK(editor->document()->isModified());
    // Saving the current document needs no dialog because it already has a path.
    TW_CHECK(window.saveCurrentFile());
    TW_CHECK(!editor->document()->isModified());

    QFile written{path};
    TW_CHECK(written.open(QIODevice::ReadOnly));
    TW_CHECK_EQ(QString::fromUtf8(written.readAll()),
                QStringLiteral("REPEAT 4 [FD 50 RT 90]"));
    written.close();

    // The new text is what a re-read of the same file finds, so the write went to
    // the same place the document believes it lives.
    const QStringList open = window.openFilePaths();
    TW_CHECK_EQ(open.at(0), path);
}


void test_a_file_that_cannot_be_read_is_refused() {
    announce("unreadable file");
    MainWindow window;

    // Silent is the mode the command line uses, and the only one a test can drive:
    // the dialog mode is exercised by hand. What matters is that the failure is
    // refused rather than half-done -- and that a script cannot be blocked by a
    // modal box it has nobody to answer.
    TW_CHECK(!window.openFile(QStringLiteral("/nonexistent/directory/thing.bas"),
                              MainWindow::OpenFailure::Silent));
    // Nothing was added, so a failed open cannot leave an empty tab behind.
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK(window.openFilePaths().isEmpty());
    // The reason is on the status bar, where a person launching with a mistyped
    // name will see it.
    TW_CHECK(window.statusBar()->currentMessage().contains(QStringLiteral("thing.bas")));
}


void test_a_byte_order_mark_is_not_part_of_the_program() {
    announce("byte order mark");
    QTemporaryDir dir;
    QByteArray withBom{"\xEF\xBB\xBF"};
    withBom.append("PRINT 1\n");
    const QString path = writeFile(dir, QStringLiteral("bom.bas"), withBom);
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    CodeEditor* editor = currentEditorOf(window);
    TW_CHECK(editor != nullptr);
    if (editor != nullptr) {
        // Left in place the mark is an invisible first character, and the first
        // line then fails to parse with an error pointing at a column that looks
        // empty -- which is a genuinely hard bug to see.
        TW_CHECK_EQ(editor->toPlainText(), QStringLiteral("PRINT 1\n"));
        TW_CHECK(!editor->toPlainText().startsWith(QChar{0xFEFF}));
    }
}

// ---------------------------------------------------------------------------
// Running a program
// ---------------------------------------------------------------------------



void test_a_binary_chip8_rom_opens_read_only() {
    announce("CHIP-8: a binary ROM opens read-only");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    // LD V0, 13; LD V1, 10; HALT. The 0x0D is the byte a QTextDocument would
    // otherwise normalise to 0x0A, so the round-trip assertion below is the one
    // that keeps the editor from changing the program.
    const QByteArray rom{"\x60\x0D\x61\x0A\x00\x00", 6};
    const QString path = scratch.write(QStringLiteral("demo.ch8"), rom);
    TW_CHECK(!path.isEmpty());
    if (path.isEmpty()) {
        return;
    }

    MainWindow window;
    TW_CHECK(window.openFile(path));
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }
    TW_CHECK(editor->isReadOnly());
    TW_CHECK(timewarp::ui::romTextToBytes(editor->toPlainText()) == rom);

    // Reaching HALT means the bytes arrived intact; a corrupted high byte would
    // have assembled as a different instruction or failed to fetch. A native run
    // that finishes leaves the badge at "ready"; only an error or a stop changes
    // it to anything else.
    window.runCurrentDocument();
    TW_CHECK(waitFor(
        [&window] {
            const QString state = badgeText(window, "executionBadge");
            return state != QStringLiteral("running") &&
                   state != QStringLiteral("stopping");
        },
        5000));
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));

    // A text listing with the same extension is still editable source.
    const QString listing = scratch.write(QStringLiteral("listing.ch8"),
                                          QByteArray{"start: CLS\n       HALT\n"});
    MainWindow text_window;
    TW_CHECK(text_window.openFile(listing));
    if (CodeEditor* text_editor = text_window.currentEditor(); text_editor != nullptr) {
        TW_CHECK(!text_editor->isReadOnly());
        TW_CHECK(text_editor->toPlainText().contains(QStringLiteral("CLS")));
    }
}

/// The word under the caret is what F1 asks about, so it has to be the whole
/// word even when the caret is in the middle of it.

void test_restoring_a_copy_opens_a_new_tab() {
    announce("version history: a chosen copy opens a new tab");
    MainWindow window;
    CodeEditor* original = window.currentEditor();
    TW_CHECK(original != nullptr);
    if (original == nullptr) {
        return;
    }
    original->setPlainText(QStringLiteral("10 PRINT \"ORIGINAL\"\n"));
    const int before = window.editorTabs()->count();
    const QString restored_text = QStringLiteral("10 PRINT \"RESTORED\"\n");

    CodeEditor* restored = window.openRestoredCopy(restored_text);
    TW_CHECK(restored != nullptr);
    if (restored == nullptr) {
        return;
    }
    TW_CHECK_EQ(window.editorTabs()->count(), before + 1);
    TW_CHECK(window.currentEditor() == restored);
    TW_CHECK_EQ(restored->toPlainText().toStdString(), restored_text.toStdString());
    // Unsaved, because the text came from the store rather than a file: closing
    // it must ask before discarding the only copy.
    TW_CHECK(restored->document()->isModified());
    // And the document it was restored from still holds what it held.
    TW_CHECK_EQ(original->toPlainText().toStdString(),
                std::string{"10 PRINT \"ORIGINAL\"\n"});
}

}  // namespace tw::ui_document
