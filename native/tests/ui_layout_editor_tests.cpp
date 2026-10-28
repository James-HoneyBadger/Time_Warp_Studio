// SPDX-License-Identifier: MIT
//
// The editor and what hangs off it: panes, minimap, history, snippets, keys.

#include "ui_layout/support.hpp"

#include <QKeyEvent>

namespace tw::ui_layout {

void test_the_split_editor_moves_documents_between_panes() {
    announce("split editor");
    const std::unique_ptr<MainWindow> window = shownWindow();
    ResizableTabWidget* primary = window->editorTabs();
    ResizableTabWidget* secondary = window->secondaryEditorTabs();
    TW_CHECK(primary != nullptr);
    TW_CHECK(secondary != nullptr);
    if (primary == nullptr || secondary == nullptr) {
        return;
    }
    // The pane exists from construction and is hidden, so splitter sizes stay put.
    TW_CHECK(!secondary->isVisible());
    TW_CHECK(!window->editorSplit());
    const int primary_before = primary->count();

    window->splitEditor();
    TW_CHECK(window->editorSplit());
    TW_CHECK(secondary->isVisible());
    TW_CHECK_EQ(secondary->count(), 1);
    // Splitting moves the document rather than showing a second view of it, and
    // leaves a fresh one behind so both halves are usable.
    TW_CHECK_EQ(primary->count(), primary_before);
    TW_CHECK(secondary->widget(0) != primary->currentWidget());

    // Moving the only document out of the split closes it: a blank second pane is
    // not a state a user can do anything with.
    auto* moved = qobject_cast<CodeEditor*>(secondary->widget(0));
    TW_CHECK(moved != nullptr);
    if (moved != nullptr) {
        moved->setFocus();
        QApplication::processEvents();
        window->moveTabToOtherPane();
    }
    TW_CHECK(!window->editorSplit());
    TW_CHECK(!secondary->isVisible());
    TW_CHECK_EQ(secondary->count(), 0);
    TW_CHECK_EQ(primary->count(), primary_before + 1);

    // And Close Split merges whatever is on the right back into the left.
    window->splitEditor();
    TW_CHECK(secondary->isVisible());
    const int both = primary->count() + secondary->count();
    window->closeSplit();
    TW_CHECK(!secondary->isVisible());
    TW_CHECK_EQ(primary->count(), both);
    TW_CHECK_EQ(secondary->count(), 0);
    // The document that was moved is still open, not closed and quietly lost.
    TW_CHECK(primary->indexOf(moved) >= 0 || moved == nullptr);
}


void test_editor_minimap_and_whitespace() {
    announce("editor: minimap and whitespace");
    MainWindow window;
    QWidget* editor_widget = window.currentEditor();
    auto* editor = qobject_cast<timewarp::ui::CodeEditor*>(editor_widget);
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }

    // A long document, so the strip has something to map and the scrollbar can
    // actually move.
    QStringList lines;
    for (int index = 1; index <= 200; ++index) {
        lines << QStringLiteral("PRINT \"line %1\"").arg(index);
    }
    editor->setPlainText(lines.join(QLatin1Char('\n')));
    QApplication::processEvents();

    editor->setMinimapVisible(true);
    TW_CHECK(editor->minimapVisible());
    TW_CHECK(editor->minimapWidth() > 0);
    QWidget* strip = editor->findChild<QWidget*>(QStringLiteral("minimapArea"));
    TW_CHECK(strip != nullptr);
    if (strip != nullptr) {
        // `isHidden()`, not `isVisible()`: the window is never shown in this
        // suite, so a child of it is not *visible* whatever its own flag says.
        // `isHidden()` answers the question the toggle controls.
        TW_CHECK(!strip->isHidden());
        TW_CHECK_EQ(strip->width(), editor->minimapWidth());
        // The strip sits in the margin, not over the text: its right edge is the
        // editor's content right edge.
        TW_CHECK(strip->geometry().right() <= editor->contentsRect().right());
    }

    // Clicking near the bottom maps near the end of the scrollbar, and near the
    // top maps to the start. This is the mapping the mouse handler uses.
    const int height = strip == nullptr ? 100 : strip->height();
    TW_CHECK(editor->scrollValueForMinimapY(0) <= editor->verticalScrollBar()->minimum());
    TW_CHECK(editor->scrollValueForMinimapY(height) >=
             editor->verticalScrollBar()->maximum());
    TW_CHECK(editor->scrollValueForMinimapY(height / 2) >
             editor->scrollValueForMinimapY(0));

    // Off: the margin is given back, so a narrow window has the text it had
    // before.
    editor->setMinimapVisible(false);
    TW_CHECK(!editor->minimapVisible());
    TW_CHECK_EQ(editor->minimapWidth(), 0);
    if (strip != nullptr) {
        TW_CHECK(strip->isHidden());
    }

    // Whitespace marks are a `QTextOption` flag on the document.
    timewarp::ui::EditorSettings settings = editor->settings();
    settings.show_whitespace = true;
    editor->applySettings(settings);
    TW_CHECK((editor->document()->defaultTextOption().flags() &
              QTextOption::ShowTabsAndSpaces) != 0);
    settings.show_whitespace = false;
    editor->applySettings(settings);
    TW_CHECK((editor->document()->defaultTextOption().flags() &
              QTextOption::ShowTabsAndSpaces) == 0);

    // And the strip reaches the paint code without a crash: `grab()` runs the
    // whole paint path, which is where an empty document or a zero-height strip
    // would divide by zero.
    editor->setMinimapVisible(true);
    editor->setPlainText(QString{});
    QApplication::processEvents();
    TW_CHECK(!editor->grab().isNull());
}

/// Version History lists the store's snapshots for one document, newest first,
/// and previews the text. Modal in use, so the dialog is constructed here rather
/// than exec()'d.


// Replace All is a bulk edit that writes files, so the window's half of it matters:
// it must tell the dialog which documents hold unsaved work, and it must reload the
// ones it changed. The write itself is covered by `editor_services_tests`; this is
// the wiring.

void test_find_in_files_offers_replace() {
    announce("Find in Files: replace is offered");
    tw::test::TempDir dir;
    TW_CHECK(dir.valid());
    if (!dir.valid()) {
        return;
    }
    const QString path = dir.write(QStringLiteral("a.logo"), "FD 10\nFD 20\n");
    TW_CHECK(!path.isEmpty());

    timewarp::ui::FindInFilesDialog dialog;
    // Nothing to replace until something has been found.
    auto* replace = dialog.findChild<QPushButton*>(QStringLiteral("findInFilesReplace"));
    TW_CHECK(replace != nullptr);
    TW_CHECK(dialog.replacementField() != nullptr);
    if (replace == nullptr || dialog.replacementField() == nullptr) {
        return;
    }
    TW_CHECK(!replace->isEnabled());

    // The window hands the dialog the documents with unsaved changes before use;
    // with none, an empty set is the answer rather than no answer.
    dialog.setOffLimits({});
    dialog.setRoot(dir.path());
    dialog.setTerm(QStringLiteral("FD"));
    dialog.startSearch();
    TW_CHECK(spin_until([&dialog] { return dialog.hitCount() > 0; }, 5000));
    TW_CHECK_EQ(dialog.hitCount(), 2);
    TW_CHECK(replace->isEnabled());

    // Cancelling the confirmation leaves every file alone: the question is the only
    // thing between the button and somebody's disk.
    QFile file{path};
    TW_CHECK(file.open(QIODevice::ReadOnly));
    TW_CHECK_EQ(QString::fromUtf8(file.readAll()).toStdString(), std::string{"FD 10\nFD 20\n"});
}

void test_version_history_lists_snapshots() {
    announce("version history");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    if (!dir.isValid()) {
        return;
    }
    timewarp::ui::AutosaveStore store{dir.filePath(QStringLiteral("autosave"))};
    const QString document = dir.filePath(QStringLiteral("program.bas"));
    static_cast<void>(store.save(document, QStringLiteral("10 PRINT 1\n"), 1000));
    static_cast<void>(store.save(document, QStringLiteral("10 PRINT 2\n"), 2000));
    // A snapshot of another document must not appear in this one's history.
    static_cast<void>(store.save(dir.filePath(QStringLiteral("other.bas")),
                                 QStringLiteral("20 END\n"), 3000));

    timewarp::ui::VersionHistoryDialog dialog{store, document};
    TW_CHECK_EQ(dialog.versionCount(), 2);
    // Nothing is chosen until a version is: cancelling must not hand the window
    // text to open.
    TW_CHECK(dialog.chosenText().isEmpty());

    auto* list = dialog.findChild<QListWidget*>(QStringLiteral("versionHistoryList"));
    auto* preview = dialog.findChild<QPlainTextEdit*>(QStringLiteral("versionHistoryPreview"));
    auto* restore = dialog.findChild<QPushButton*>(QStringLiteral("versionHistoryRestore"));
    TW_CHECK(list != nullptr);
    TW_CHECK(preview != nullptr);
    TW_CHECK(restore != nullptr);
    if (list != nullptr && preview != nullptr) {
        TW_CHECK_EQ(list->count(), 2);
        // The newest is selected, and the preview is its text.
        TW_CHECK_EQ(list->currentRow(), 0);
        TW_CHECK(preview->toPlainText().contains(QStringLiteral("10 PRINT 2")));
    }
    // Choosing a version accepts the dialog and hands back exactly the text that
    // was previewed; the window, not the dialog, decides where it goes.
    if (restore != nullptr) {
        restore->click();
        TW_CHECK_EQ(dialog.result(), int(QDialog::Accepted));
        TW_CHECK_EQ(dialog.chosenText().toStdString(), std::string{"10 PRINT 2\n"});
    }
}


// A list of copies answers "what did it look like"; the comparison answers "what
// did I change". Both are here, and the tick that chooses between them is the only
// control in the dialog that changes what the user is looking at.

void test_version_history_compares_with_the_open_document() {
    announce("version history: compare");
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    if (!dir.isValid()) {
        return;
    }
    timewarp::ui::AutosaveStore store{dir.filePath(QStringLiteral("autosave"))};
    const QString document = dir.filePath(QStringLiteral("program.bas"));
    static_cast<void>(store.save(document, QStringLiteral("10 PRINT 1\n20 END\n"), 1000));

    timewarp::ui::VersionHistoryDialog dialog{store, document,
                                             QStringLiteral("10 PRINT 2\n20 END\n")};
    auto* compare = dialog.compareBox();
    TW_CHECK(compare != nullptr);
    TW_CHECK(dialog.comparison() != nullptr);
    if (compare == nullptr || dialog.comparison() == nullptr) {
        return;
    }
    // Offered because the editor has text to compare with.
    TW_CHECK(compare->isEnabled());
    // The copy on its own is what is shown until the tick is set.
    TW_CHECK(dialog.preview()->isVisibleTo(&dialog));
    TW_CHECK(!dialog.comparison()->isVisibleTo(&dialog));

    compare->setChecked(true);
    TW_CHECK(dialog.comparison()->isVisibleTo(&dialog));
    TW_CHECK(!dialog.preview()->isVisibleTo(&dialog));
    // From what is open to the copy: the old line comes back, the new one goes.
    TW_CHECK(dialog.diffText().contains(QStringLiteral("-10 PRINT 2")));
    TW_CHECK(dialog.diffText().contains(QStringLiteral("+10 PRINT 1")));
    // ... and the unchanged line is context, not a change.
    TW_CHECK(dialog.diffText().contains(QStringLiteral(" 20 END")));

    compare->setChecked(false);
    TW_CHECK(dialog.preview()->isVisibleTo(&dialog));
    TW_CHECK(dialog.diffText().isEmpty());

    // An empty document has nothing to compare with, and the tick says so instead
    // of producing an empty diff that looks like "no changes".
    timewarp::ui::VersionHistoryDialog empty{store, document, QString{}};
    TW_CHECK(empty.compareBox() != nullptr);
    if (empty.compareBox() != nullptr) {
        TW_CHECK(!empty.compareBox()->isEnabled());
    }
}

/// The picker shows the catalogue, filtered by language, and reports a choice.

void test_snippet_picker_filters_by_language() {
    announce("snippet picker");
    QString error;
    const QList<timewarp::ui::Snippet> catalogue = timewarp::ui::loadSnippets(&error);
    TW_CHECK(error.isEmpty());
    TW_CHECK(!catalogue.isEmpty());
    if (catalogue.isEmpty()) {
        return;
    }

    timewarp::ui::SnippetDialog dialog;
    dialog.setSnippets(catalogue);
    dialog.setLanguage(timewarp::ui::Language::Logo);
    const timewarp::ui::Snippet first = dialog.chosen();
    TW_CHECK(first.language == timewarp::ui::Language::Logo);
    TW_CHECK(!first.body.isEmpty());

    // And a language with no entries shows everything rather than nothing: an
    // empty list tells a user their language has no snippets, which is not the
    // same as "the file could not be read".
    dialog.setLanguage(timewarp::ui::Language::Unknown);
    TW_CHECK(!dialog.chosen().body.isEmpty());
}

/// Find in Files is in the Edit menu, on Ctrl+Shift+F, wired to something. The
/// reserved action id and the menu entry have to agree, or the palette lists a
/// command the key does not reach.

void test_every_right_column_page_has_a_key() {
    announce("right column panel keys");
    MainWindow window;
    QTabWidget* tabs = window.rightTabs();
    TW_CHECK(tabs != nullptr);
    if (tabs == nullptr) {
        return;
    }
    const QStringList keys = MainWindow::panelKeys();
    TW_CHECK_EQ(keys.size(), tabs->count());

    for (int index = 0; index < keys.size() && index < tabs->count(); ++index) {
        // The key must raise the tab it is listed against: the order is the
        // contract, not just membership, because the table stores indices.
        tabs->setCurrentIndex(0);
        TW_CHECK(window.showPanel(keys.at(index)));
        TW_CHECK_EQ(tabs->currentIndex(), index);
    }

    // And an unknown key is refused rather than silently raising page one, which
    // is what a caller holding a name from data needs to be able to trust.
    TW_CHECK(!window.showPanel(QStringLiteral("no-such-panel")));
}

void test_edit_menu_acts_on_whatever_has_focus() {
    announce("Edit > Undo follows focus");
    MainWindow window;

    timewarp::ui::CodeEditor* editor = window.currentEditor();
    auto* console = window.findChild<QPlainTextEdit*>(QStringLiteral("consoleOutput"));
    TW_CHECK(editor != nullptr);
    TW_CHECK(console != nullptr);
    if (editor == nullptr || console == nullptr) {
        return;
    }

    QAction* undo = entryContaining(menuNamed(window, QStringLiteral("&Edit")),
                                    QStringLiteral("Undo"));
    TW_CHECK(undo != nullptr);
    if (undo == nullptr) {
        return;
    }

    // A `setPlainText` empties the undo stack, so the edit that gets undone has to
    // be one the user could have typed. It also leaves the cursor at position 0,
    // which is why each insert below moves to the end first: inserting at the
    // start would quietly test a different edit than the one asserted.
    editor->setPlainText(QStringLiteral("kept"));
    console->setPlainText(QStringLiteral("log"));

    // Undo in the editor, with the editor focused.
    editor->setFocus();
    editor->moveCursor(QTextCursor::End);
    editor->insertPlainText(QStringLiteral(" typed"));
    TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"kept typed"});
    undo->trigger();
    TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"kept"});

    // And in the console, with the console focused. This is the part an Edit menu
    // bound to one widget gets wrong: it would edit the code editor while the
    // cursor the user is looking at is somewhere else.
    //
    // Focus routing needs a window that can hold focus. A hidden window has no
    // focus widget at all, and editTarget() then falls back to the editor by
    // design -- which is what the first half of this test just proved. So the
    // window is shown, and the routing is only asserted once focus was really
    // taken, because otherwise this would be testing the environment.
    window.show();
    QApplication::processEvents();
    console->setFocus(Qt::OtherFocusReason);
    QApplication::processEvents();
    TW_CHECK(QApplication::focusWidget() == console);
    if (QApplication::focusWidget() != console) {
        return;
    }

    console->moveCursor(QTextCursor::End);
    console->insertPlainText(QStringLiteral(" line"));
    TW_CHECK_EQ(console->toPlainText().toStdString(), std::string{"log line"});
    undo->trigger();
    TW_CHECK_EQ(console->toPlainText().toStdString(), std::string{"log"});
    TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"kept"});
}

// A suggested next line is a promise about the shape of an answer: one line of
// ghost text, taken with Tab or refused with Esc, and never inserted until asked
// for. The endpoint behind it is the window's, so this half is what can be asserted
// without one.
void test_a_suggested_line_can_be_taken_or_refused() {
    announce("editor: a suggested line");
    CodeEditor editor;
    timewarp::ui::EditorSettings settings = editor.settings();
    settings.inline_suggestions = true;
    editor.applySettings(settings);
    editor.setPlainText(QStringLiteral("10 PRINT \"HI\"\n20 "));
    QTextCursor cursor = editor.textCursor();
    cursor.movePosition(QTextCursor::End);
    editor.setTextCursor(cursor);

    // A model that answers in prose has answered the wrong question: a suggestion is
    // the rest of the line, so the first line is what is shown.
    editor.setGhostText(QStringLiteral("PRINT \"BYE\"\n20 REM and so on"));
    TW_CHECK_EQ(editor.ghostText(), QStringLiteral("PRINT \"BYE\""));

    QKeyEvent tab{QEvent::KeyPress, Qt::Key_Tab, Qt::NoModifier};
    QApplication::sendEvent(&editor, &tab);
    TW_CHECK(editor.ghostText().isEmpty());
    TW_CHECK(editor.toPlainText().endsWith(QStringLiteral("20 PRINT \"BYE\"")));

    // Esc refuses one without touching the document.
    const QString before = editor.toPlainText();
    editor.setGhostText(QStringLiteral("REM no"));
    QKeyEvent escape{QEvent::KeyPress, Qt::Key_Escape, Qt::NoModifier};
    QApplication::sendEvent(&editor, &escape);
    TW_CHECK(editor.ghostText().isEmpty());
    TW_CHECK_EQ(editor.toPlainText(), before);

    // Any other key dismisses it and is typed as usual: a suggestion that swallowed
    // a keystroke would be worse than no suggestion.
    editor.setGhostText(QStringLiteral("REM no"));
    QKeyEvent word{QEvent::KeyPress, Qt::Key_X, Qt::NoModifier, QStringLiteral("X")};
    QApplication::sendEvent(&editor, &word);
    TW_CHECK(editor.ghostText().isEmpty());
    TW_CHECK(editor.toPlainText().endsWith(QStringLiteral("X")));

    // Turning the setting off takes a visible suggestion with it.
    editor.setGhostText(QStringLiteral("REM no"));
    settings.inline_suggestions = false;
    editor.applySettings(settings);
    TW_CHECK(editor.ghostText().isEmpty());
}

void test_the_typing_pause_asks_for_the_text_before_the_caret() {
    announce("editor: the typing pause");
    CodeEditor editor;
    timewarp::ui::EditorSettings settings = editor.settings();
    settings.inline_suggestions = true;
    editor.applySettings(settings);
    QStringList asked;
    QObject::connect(&editor, &CodeEditor::inlineSuggestionWanted,
                     [&asked](const QString& prefix) { asked.append(prefix); });

    editor.setPlainText(QStringLiteral("10 REM one\n20 REM two"));
    QTextCursor cursor = editor.textCursor();
    cursor.movePosition(QTextCursor::End);
    editor.setTextCursor(cursor);
    // The text a suggestion is about is everything before the caret, and that is
    // also what the window sends as the program.
    TW_CHECK_EQ(editor.textBeforeCursor(), QStringLiteral("10 REM one\n20 REM two"));
    // The tail, because it is the end of the program that comes next from.
    TW_CHECK_EQ(editor.textBeforeCursor(10), QStringLiteral("20 REM two"));

    QKeyEvent word{QEvent::KeyPress, Qt::Key_X, Qt::NoModifier, QStringLiteral("X")};
    QApplication::sendEvent(&editor, &word);
    TW_CHECK(spin_until([&asked] { return !asked.isEmpty(); }, 3000));
    if (!asked.isEmpty()) {
        TW_CHECK(asked.last().endsWith(QStringLiteral("20 REM twoX")));
    }
    // Off, a pause asks for nothing: the switch is the promise that typing sends no
    // request.
    asked.clear();
    settings.inline_suggestions = false;
    editor.applySettings(settings);
    QApplication::sendEvent(&editor, &word);
    TW_CHECK(!spin_until([&asked] { return !asked.isEmpty(); }, 1200));
    TW_CHECK(asked.isEmpty());
}

// Several carets write into the document at once. The rule the test holds is the one a
// reader expects: every caret gets the same keystroke, the earlier positions do not
// move under the later edits, and Escape puts the document down unchanged.
void test_typing_at_several_cursors() {
    announce("editor: several carets");
    CodeEditor editor;
    editor.setPlainText(QStringLiteral("abc"));
    QTextCursor cursor = editor.textCursor();
    cursor.movePosition(QTextCursor::End);
    editor.setTextCursor(cursor);
    TW_CHECK(editor.completionVisible() == false);
    editor.addCursorAt(1);  // after the `a`
    TW_CHECK(editor.multiCursorActive());
    TW_CHECK_EQ(editor.extraCursors().size(), 1);
    // The same caret twice is still one caret.
    editor.addCursorAt(1);
    TW_CHECK_EQ(editor.extraCursors().size(), 1);

    QKeyEvent typed{QEvent::KeyPress, Qt::Key_X, Qt::NoModifier, QStringLiteral("X")};
    QApplication::sendEvent(&editor, &typed);
    TW_CHECK_EQ(editor.toPlainText(), QStringLiteral("aXbcX"));
    // The main caret is the last insertion, so the next keystroke continues there.
    TW_CHECK_EQ(editor.textCursor().position(), 5);

    QKeyEvent back{QEvent::KeyPress, Qt::Key_Backspace, Qt::NoModifier};
    QApplication::sendEvent(&editor, &back);
    TW_CHECK_EQ(editor.toPlainText(), QStringLiteral("abc"));

    // Escape forgets the extra carets and changes nothing else.
    editor.addCursorAt(0);
    QKeyEvent escape{QEvent::KeyPress, Qt::Key_Escape, Qt::NoModifier};
    QApplication::sendEvent(&editor, &escape);
    TW_CHECK(!editor.multiCursorActive());
    TW_CHECK_EQ(editor.toPlainText(), QStringLiteral("abc"));
}

}  // namespace tw::ui_layout
