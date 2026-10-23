// SPDX-License-Identifier: MIT
//
// Editing: find and replace, the snippet picker, run-selection and its history, and
// the two questions answered from the cursor's position.

#include "ui_document/support.hpp"

namespace tw::ui_document {

void test_find_next_selects_and_wraps() {
    announce("find: select, count, wrap");
    MainWindow window;
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    editor->setPlainText(QStringLiteral("alpha beta alpha gamma alpha"));

    TW_CHECK_EQ(editor->matchCount(QStringLiteral("alpha"), true), 3);
    TW_CHECK_EQ(editor->matchCount(QStringLiteral("ALPHA"), false), 3);
    // Case matters when it is asked to: a search that ignored case would find
    // `ALPHA` here and hide a real mistake in a language where case matters.
    TW_CHECK_EQ(editor->matchCount(QStringLiteral("ALPHA"), true), 0);
    TW_CHECK_EQ(editor->matchCount(QStringLiteral(""), true), 0);

    TW_CHECK(editor->findNext(QStringLiteral("alpha"), true));
    TW_CHECK_EQ(editor->textCursor().selectedText(), QStringLiteral("alpha"));
    TW_CHECK_EQ(editor->textCursor().selectionStart(), 0);

    TW_CHECK(editor->findNext(QStringLiteral("alpha"), true));
    TW_CHECK_EQ(editor->textCursor().selectionStart(), 11);

    TW_CHECK(editor->findNext(QStringLiteral("alpha"), true));
    TW_CHECK_EQ(editor->textCursor().selectionStart(), 23);

    // From the last match it wraps to the first, so a search never dead-ends at
    // the bottom of the file with matches above it.
    TW_CHECK(editor->findNext(QStringLiteral("alpha"), true));
    TW_CHECK_EQ(editor->textCursor().selectionStart(), 0);

    // Backwards from the first wraps to the last.
    TW_CHECK(editor->findNext(QStringLiteral("alpha"), true, true));
    TW_CHECK_EQ(editor->textCursor().selectionStart(), 23);

    // A term that is not there moves nothing at all.
    TW_CHECK(!editor->findNext(QStringLiteral("delta"), true));
    TW_CHECK_EQ(editor->textCursor().selectionStart(), 23);
}


void test_replace_all_is_one_step() {
    announce("replace all");
    MainWindow window;
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    editor->setPlainText(QStringLiteral("x = 1\ny = x + x\n"));

    // A replacement containing the search term terminates: each search resumes
    // after what was just inserted.
    TW_CHECK_EQ(editor->replaceAll(QStringLiteral("x"), QStringLiteral("xx"), true), 3);
    TW_CHECK_EQ(editor->toPlainText(), QStringLiteral("xx = 1\ny = xx + xx\n"));

    // One undo step, because replacing a document in twelve pieces means twelve
    // undos to get back to where you were.
    editor->undo();
    TW_CHECK_EQ(editor->toPlainText(), QStringLiteral("x = 1\ny = x + x\n"));

    TW_CHECK_EQ(editor->replaceAll(QStringLiteral("X"), QStringLiteral("z"), false), 3);
    TW_CHECK_EQ(editor->toPlainText(), QStringLiteral("z = 1\ny = z + z\n"));

    TW_CHECK_EQ(editor->replaceAll(QStringLiteral("x"), QStringLiteral("q"), true), 0);

    // Replacing one at a time only replaces when the cursor is on a match.
    TW_CHECK(!editor->replaceCurrent(QStringLiteral("q"), QStringLiteral("w"), true));
    // Park the cursor at the top first: which `z` a single replacement hits has to
    // be a fact about the document, not about where the undo left the cursor.
    editor->moveCursor(QTextCursor::Start);
    TW_CHECK(editor->findNext(QStringLiteral("z"), true));
    TW_CHECK_EQ(editor->textCursor().selectionStart(), 0);
    TW_CHECK(editor->replaceCurrent(QStringLiteral("z"), QStringLiteral("w"), true));
    TW_CHECK_EQ(editor->toPlainText(), QStringLiteral("w = 1\ny = z + z\n"));
}


void test_the_find_bar_drives_the_editor() {
    announce("the find bar");
    MainWindow window;
    FindBar* bar = window.findBar();
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(bar != nullptr);
    TW_CHECK(editor != nullptr);

    // Out of the way until it is asked for, and asking for plain Find does not
    // show a replacement row.
    TW_CHECK(bar->isHidden());
    window.showFindBar(false);
    TW_CHECK(!bar->isHidden());
    TW_CHECK(!bar->replaceRowVisible());

    editor->setPlainText(QStringLiteral("one two three two"));
    bar->setTerm(QStringLiteral("two"));
    TW_CHECK_EQ(bar->statusText(), QStringLiteral("2 matches"));
    bar->findAgain(false);
    TW_CHECK_EQ(editor->textCursor().selectedText(), QStringLiteral("two"));

    // The count follows the document, so it cannot say "2 matches" after one of
    // them was deleted.
    editor->setPlainText(QStringLiteral("one two three"));
    bar->setTerm(QStringLiteral("two"));
    TW_CHECK_EQ(bar->statusText(), QStringLiteral("1 match"));

    bar->setTerm(QStringLiteral("seven"));
    TW_CHECK_EQ(bar->statusText(), QStringLiteral("no matches"));

    // Replace shows the second row, and Replace All reports what it did.
    window.showFindBar(true);
    TW_CHECK(bar->replaceRowVisible());
    bar->setTerm(QStringLiteral("one"));
    TW_CHECK_EQ(bar->statusText(), QStringLiteral("1 match"));
}

// ---------------------------------------------------------------------------
// The recent list, and the session
// ---------------------------------------------------------------------------


void test_a_snippet_is_inserted_at_the_cursor() {
    announce("snippets: insertion");
    MainWindow window;
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }

    // No trailing newline: the snippet supplies the one that ends the line, and a
    // document that already had one would put the caret on an empty third line.
    editor->setPlainText(QStringLiteral("10 PRINT 1\n20 "));
    editor->moveCursor(QTextCursor::End);
    timewarp::ui::Snippet snippet;
    snippet.name = QStringLiteral("End");
    snippet.language = Language::Basic;
    snippet.body = QStringLiteral("END\n");
    window.applySnippet(snippet);
    TW_CHECK(editor->toPlainText().endsWith(QStringLiteral("20 END\n")));

    // A selection is wrapped rather than replaced blindly: `$SELECTION$` sits
    // inside the snippet, which is the only reason to have a selection at all.
    editor->setPlainText(QStringLiteral("HELLO"));
    // The document holds only the word, so the snippets's own `PRINT` is not a
    // second copy of one the document already had.
    QTextCursor select = editor->textCursor();
    select.setPosition(0);
    select.setPosition(5, QTextCursor::KeepAnchor);
    editor->setTextCursor(select);
    timewarp::ui::Snippet wrapper;
    wrapper.name = QStringLiteral("Bracket");
    wrapper.language = Language::Basic;
    wrapper.body = QStringLiteral("PRINT [$SELECTION$]\n$CURSOR$");
    window.applySnippet(wrapper);
    TW_CHECK_EQ(editor->toPlainText().toStdString(), std::string{"PRINT [HELLO]\n"});
    // The caret is where `$CURSOR$` was: the end of the inserted text here.
    TW_CHECK_EQ(editor->textCursor().position(), int(QStringLiteral("PRINT [HELLO]\n").size()));
}


/// Run Selection executes only the selected lines, and Run History records what
/// was run. Both are the removed implementation's, restored.

void test_run_selection_and_history() {
    announce("run selection and run history");
    // The suite's own settings file outlives one run of it, and the history is
    // stored there on purpose. Starting from a known state is the test's job.
    QSettings{}.remove(QStringLiteral("run/history"));
    MainWindow window;
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }

    editor->setPlainText(QStringLiteral("10 PRINT \"FIRST\"\n20 PRINT \"SECOND\"\n30 END\n"));
    // Select line 2 only. `blockNumber` is zero-based; line 2 starts at block 1.
    QTextCursor select{editor->document()->findBlockByNumber(1)};
    select.movePosition(QTextCursor::EndOfBlock, QTextCursor::KeepAnchor);
    editor->setTextCursor(select);

    window.runSelection();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    QPlainTextEdit* console = viewOf(window, "consoleOutput");
    TW_CHECK(console != nullptr);
    if (console != nullptr) {
        // The selected line ran, and the lines around it did not: running the
        // whole document when three lines were highlighted is the failure this
        // action exists to avoid.
        TW_CHECK(console->toPlainText().contains(QStringLiteral("SECOND")));
        TW_CHECK(!console->toPlainText().contains(QStringLiteral("FIRST")));
    }

    // An empty selection is refused with a message rather than silently running
    // everything.
    editor->moveCursor(QTextCursor::End);
    window.runSelection();
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));

    // History: the selection run is recorded, and the refused one is not -- a
    // refused action did not happen, so it must not appear in the list of what
    // did.
    QStringList history = window.runHistory();
    TW_CHECK_EQ(history.size(), 1);
    if (!history.isEmpty()) {
        TW_CHECK(history.first().contains(QStringLiteral("selection:")));
        TW_CHECK(history.first().contains(QStringLiteral("SECOND")));
    }

    // A whole-document run goes in at the top, newest first.
    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    history = window.runHistory();
    TW_CHECK_EQ(history.size(), 2);
    if (!history.isEmpty()) {
        TW_CHECK(!history.first().contains(QStringLiteral("selection:")));
        TW_CHECK(history.first().contains(QStringLiteral("FIRST")));
    }

    // Reset clears the screen and keeps the document, which is the difference
    // between Reset and closing the tab.
    window.resetProgram();
    if (console != nullptr) {
        TW_CHECK(console->toPlainText().isEmpty());
    }
    TW_CHECK_EQ(editor->toPlainText().toStdString(),
                std::string{"10 PRINT \"FIRST\"\n20 PRINT \"SECOND\"\n30 END\n"});
}


/// Autosave writes a copy of a document that has unsaved changes, and only of
/// one that has them. The store is checked directly: what matters is that the
/// bytes are on disk under the data directory, not which widget asked for them.

void test_word_at_cursor() {
    announce("editor: the word under the cursor");
    CodeEditor editor;
    editor.setPlainText(QStringLiteral("PRINT FORWARD 10\n"));

    const auto word_at = [&editor](int position) {
        QTextCursor cursor = editor.textCursor();
        cursor.setPosition(position);
        editor.setTextCursor(cursor);
        return editor.wordAtCursor();
    };

    TW_CHECK_EQ(word_at(8).toStdString(), std::string{"FORWARD"});  // inside
    TW_CHECK_EQ(word_at(6).toStdString(), std::string{"FORWARD"});  // first letter
    TW_CHECK_EQ(word_at(13).toStdString(), std::string{"FORWARD"}); // just past it
    TW_CHECK_EQ(word_at(5).toStdString(), std::string{"PRINT"});    // on the space
    TW_CHECK_EQ(word_at(0).toStdString(), std::string{"PRINT"});

    // Nothing to ask about: a blank line, and a run of punctuation.
    QTextCursor cursor = editor.textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertBlock();
    editor.setTextCursor(cursor);
    TW_CHECK(editor.wordAtCursor().isEmpty());
    cursor.insertText(QStringLiteral("+++"));
    editor.setTextCursor(cursor);
    TW_CHECK(editor.wordAtCursor().isEmpty());
}

/// F1's target is the current document's language: a Logo tab asks the Logo
/// guide, not whichever guide happened to open last.

void test_context_help_follows_the_document_language() {
    announce("context help: the document's language");
    MainWindow window;
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }
    editor->setLanguage(timewarp::ui::Language::Logo);
    TW_CHECK_EQ(window.contextHelpTopic().toStdString(),
                std::string{"tutorials/logo.md"});
    editor->setLanguage(timewarp::ui::Language::Basic);
    TW_CHECK_EQ(window.contextHelpTopic().toStdString(),
                std::string{"tutorials/basic.md"});
    editor->setLanguage(timewarp::ui::Language::Unknown);
    TW_CHECK(window.contextHelpTopic().isEmpty());
}

/// A version chosen in the history becomes a new tab, marked modified, and the
/// document it came from is untouched. The dialog is modal, so the half that
/// matters here -- what the window does with the text -- is called directly.

}  // namespace tw::ui_document
