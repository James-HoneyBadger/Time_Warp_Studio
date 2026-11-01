// SPDX-License-Identifier: MIT
//
// A document's life: open, raise the tab that is already open, edit, choose a
// language, close, save, and the two files that are read rather than parsed.

#include "ui_document/support.hpp"

#include <QClipboard>
#include <QDir>
#include <QMenu>

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
    CodeEditor* initial = currentEditorOf(window);
    TW_CHECK(QMetaObject::invokeMethod(window.editorTabs(), "tabCloseRequested",
                                      Qt::DirectConnection, Q_ARG(int, 0)));
    TW_CHECK_EQ(currentEditorOf(window), initial);
    TW_CHECK_EQ(window.editorTabs()->tabText(0), QStringLiteral("untitled"));
    window.newFile();
    window.newFile();
    TW_CHECK_EQ(window.editorTabs()->count(), 3);

    // Nothing is modified, so nothing may be asked about: the unsaved-changes
    // dialog is a decision for the user, not a step in every close.
    TW_CHECK(window.closeCurrentTab());
    TW_CHECK_EQ(window.editorTabs()->count(), 2);
    TW_CHECK(window.closeTab(0));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);

    CodeEditor* remaining = currentEditorOf(window);
    auto* selector = window.findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(selector != nullptr);
    if (selector == nullptr) {
        return;
    }
    selector->setCurrentIndex(selector->findData(QStringLiteral("logo")));
    const QString title = window.windowTitle();
    const QString breadcrumb = badgeText(window, "breadcrumb");

    // Closing the last blank scratch document keeps its identity and settings.
    TW_CHECK(window.closeTab(0));
    TW_CHECK(window.closeCurrentTab());
    TW_CHECK(QMetaObject::invokeMethod(window.editorTabs(), "tabCloseRequested",
                                      Qt::DirectConnection, Q_ARG(int, 0)));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK_EQ(currentEditorOf(window), remaining);
    TW_CHECK_EQ(window.editorTabs()->tabText(0), QStringLiteral("untitled 2"));
    TW_CHECK_EQ(window.windowTitle(), title);
    TW_CHECK_EQ(badgeText(window, "breadcrumb"), breadcrumb);
    TW_CHECK_EQ(selector->currentData().toString(), QStringLiteral("logo"));

    // An index that does not exist is refused rather than removing a neighbouring
    // tab, which is what an unchecked `removeTab(-1)` does not do but a clamped one
    // would.
    TW_CHECK(!window.closeTab(99));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);

    window.newFile();
    TW_CHECK_EQ(window.editorTabs()->tabText(1), QStringLiteral("untitled 4"));
    TW_CHECK(window.closeCurrentTab());

    remaining->setPlainText(QStringLiteral("PRINT 1"));
    remaining->document()->setModified(false);
    TW_CHECK(window.closeCurrentTab());
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK(currentEditorOf(window) != remaining);
    TW_CHECK_EQ(window.editorTabs()->tabText(0), QStringLiteral("untitled 5"));

    // An empty saved file still closes rather than being kept as a scratch tab.
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    const QString path = writeFile(dir, QStringLiteral("empty.bas"), QByteArray{});
    TW_CHECK(!path.isEmpty());
    TW_CHECK(window.openFile(path));
    TW_CHECK(window.closeTab(0));
    TW_CHECK(window.closeCurrentTab());
    TW_CHECK(window.openFilePaths().isEmpty());
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK_EQ(window.editorTabs()->tabText(0), QStringLiteral("untitled 6"));
}

void test_closing_documents_in_a_split() {
    announce("closing documents in a split");
    MainWindow window;

    // Close Other Tabs keeps the named tab and removes the rest. The context
    // menu's entries emitted signals nothing answered until they were connected,
    // so choosing one did nothing at all.
    window.newFile();
    window.newFile();
    TW_CHECK_EQ(window.editorTabs()->count(), 3);
    TW_CHECK(window.closeOtherTabs(window.editorTabs(), 1));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK_EQ(window.editorTabs()->tabText(0), QStringLiteral("untitled 2"));

    // Close All Tabs ends with a fresh document rather than the grey rectangle the
    // single-tab rule exists to prevent.
    CodeEditor* closed = currentEditorOf(window);
    window.newFile();
    TW_CHECK_EQ(window.editorTabs()->count(), 2);
    TW_CHECK(window.closeAllTabs(window.editorTabs()));
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK(currentEditorOf(window) != closed);

    // On the lone blank scratch document, Close All is the same request as Close:
    // there is nothing else to remove, and the language just chosen is worth
    // keeping.
    auto* selector = window.findChild<QComboBox*>(QStringLiteral("languageSelector"));
    TW_CHECK(selector != nullptr);
    if (selector == nullptr) {
        return;
    }
    CodeEditor* scratch = currentEditorOf(window);
    selector->setCurrentIndex(selector->findData(QStringLiteral("rexx")));
    TW_CHECK(window.closeAllTabs(window.editorTabs()));
    TW_CHECK_EQ(currentEditorOf(window), scratch);
    TW_CHECK_EQ(selector->currentData().toString(), QStringLiteral("rexx"));

    // The split's second pane closes by the same policy, so a modified document
    // there is saved before it goes rather than dropped for being on the right.
    // Its last document takes the split with it: an emptied pane is the grey
    // rectangle, and Move Tab to Other Pane already closes it when it empties.
    window.splitEditor();
    TW_CHECK(window.editorSplit());
    TW_CHECK_EQ(window.secondaryEditorTabs()->count(), 1);
    TW_CHECK(QMetaObject::invokeMethod(window.secondaryEditorTabs(), "tabCloseRequested",
                                       Qt::DirectConnection, Q_ARG(int, 0)));
    TW_CHECK_EQ(window.secondaryEditorTabs()->count(), 0);
    // `isHidden`, not `isVisible`: the window has not been shown, and a child of an
    // unshown window is never "visible" whatever its own flag says.
    TW_CHECK(window.secondaryEditorTabs()->isHidden());
    TW_CHECK(!window.editorSplit());
    // The window is still usable: the primary pane kept its document.
    TW_CHECK_EQ(window.editorTabs()->count(), 1);
    TW_CHECK(currentEditorOf(window) != nullptr);
}


void test_the_split_keeps_its_documents_in_step() {
    announce("the split is a second pane, not a second class");
    MainWindow window;
    CodeEditor* first = currentEditorOf(window);
    TW_CHECK(first != nullptr);
    if (first == nullptr) {
        return;
    }

    window.splitEditor();
    TW_CHECK(window.editorSplit());
    QTabWidget* right = window.secondaryEditorTabs();
    TW_CHECK_EQ(right->count(), 1);
    auto* in_split = qobject_cast<CodeEditor*>(right->widget(0));
    TW_CHECK(in_split == first);
    if (in_split == nullptr) {
        return;
    }

    // An edit in the split marks the split's own tab. The caption was only ever
    // written to the main pane's tab bar, so a modified document on the right
    // showed no unsaved dot -- the marker that says a close will ask.
    in_split->setPlainText(QString{});
    QTextCursor typed = in_split->textCursor();
    typed.insertText(QStringLiteral("10 PRINT \"SPLIT\"\n"));
    TW_CHECK(in_split->document()->isModified());
    TW_CHECK(right->tabText(0).contains(QChar(0x25CF)));

    // Duplicate Tab makes a copy -- text, language, breakpoints -- in the pane the
    // source is in. The menu entry emitted a signal nothing had connected, so
    // choosing it did nothing at all.
    CodeEditor* copy = window.duplicateTab(right, 0);
    TW_CHECK(copy != nullptr);
    if (copy == nullptr) {
        return;
    }
    TW_CHECK(copy != in_split);
    TW_CHECK_EQ(right->count(), 2);
    TW_CHECK(right->indexOf(copy) >= 0);
    TW_CHECK_EQ(copy->toPlainText().toStdString(), in_split->toPlainText().toStdString());
    // Named for what it is a copy of, and marked unsaved: closing it asks, and
    // autosave keeps it.
    TW_CHECK_EQ(right->tabText(right->indexOf(copy)),
                QStringLiteral("untitled copy \u25CF"));
    TW_CHECK(copy->document()->isModified());

    // A tab change inside the split re-describes the window. Nothing listened to
    // the split's tab bar, so the title, badges and outline went on describing the
    // document the pane showed before.
    //
    // Focus routing needs a window that can hold focus: `currentEditor` reads the
    // focused pane, and a hidden window has no focus widget at all. So the window
    // is shown and the assertions made only once focus was really taken.
    window.show();
    QApplication::processEvents();
    in_split->setFocus(Qt::OtherFocusReason);
    QApplication::processEvents();
    if (QApplication::focusWidget() == nullptr) {
        return;
    }
    right->setCurrentWidget(in_split);
    const QString split_crumb = badgeText(window, "breadcrumb");
    right->setCurrentWidget(copy);
    const QString copy_crumb = badgeText(window, "breadcrumb");
    TW_CHECK_EQ(split_crumb, QStringLiteral("untitled"));
    TW_CHECK_EQ(copy_crumb, QStringLiteral("untitled copy"));
}

void test_a_file_open_in_the_split_is_not_opened_twice() {
    announce("a file open in the split is raised, not copied");
    MainWindow window;
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    if (!dir.isValid()) {
        return;
    }
    const QString path = writeFile(dir, QStringLiteral("shared.bas"), QByteArray{"PRINT 1\n"});
    TW_CHECK(!path.isEmpty());
    TW_CHECK(window.openFile(path));

    // Move it to the split, then open it again: the raise rule has to look in both
    // panes, or the second Open loads a second copy of one file and the last save
    // silently wins.
    CodeEditor* opened = currentEditorOf(window);
    TW_CHECK(opened != nullptr);
    if (opened == nullptr) {
        return;
    }
    window.splitEditor();
    QTabWidget* right = window.secondaryEditorTabs();
    TW_CHECK(window.editorSplit());
    TW_CHECK(right->indexOf(opened) >= 0);
    const int tabs_before = right->count();
    const int left_before = window.editorTabs()->count();

    TW_CHECK(window.openFile(path));
    TW_CHECK_EQ(right->count(), tabs_before);
    TW_CHECK_EQ(window.editorTabs()->count(), left_before);
    TW_CHECK_EQ(right->currentWidget(), opened);
    TW_CHECK(window.editorTabs()->indexOf(opened) < 0);

    // A copy of a saved document is named for the file in front of its extension,
    // which is the name Save As then offers.
    const int index = right->indexOf(opened);
    CodeEditor* file_copy = window.duplicateTab(right, index);
    TW_CHECK(file_copy != nullptr);
    if (file_copy == nullptr) {
        return;
    }
    TW_CHECK_EQ(right->tabText(right->indexOf(file_copy)),
                QStringLiteral("shared copy.bas \u25CF"));
    // Untitled, so it is not a second tab on that path.
    TW_CHECK_EQ(window.openFilePaths().size(), 0);
}

void test_the_tab_menu_offers_the_documents_file() {
    announce("tab menu: the document's own file");
    MainWindow window;
    QTabWidget* pane = window.editorTabs();

    // An untitled buffer has no path to copy and no folder to open, so the two
    // entries would be offers to do nothing.
    QMenu blank_menu;
    window.addDocumentActions(pane, 0, &blank_menu);
    TW_CHECK(blank_menu.actions().isEmpty());

    // A saved document gets both. The menu the tab bar shows asks for them through
    // `aboutToShowTabMenu`, which nothing has ever connected -- so the entries the
    // signal exists for have never appeared.
    QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    if (!dir.isValid()) {
        return;
    }
    const QString path = writeFile(dir, QStringLiteral("menu.bas"), QByteArray{"PRINT 1\n"});
    TW_CHECK(!path.isEmpty());
    TW_CHECK(window.openFile(path));

    QMenu menu;
    window.addDocumentActions(pane, pane->currentIndex(), &menu);
    QStringList titles;
    for (QAction* action : menu.actions()) {
        titles << action->text();
        if (action->text() == QStringLiteral("Copy File Path")) {
            action->trigger();
        }
    }
    TW_CHECK(titles.contains(QStringLiteral("Copy File Path")));
    TW_CHECK(titles.contains(QStringLiteral("Open Containing Folder")));
    // The copied text is the path as the platform shows it, which is what a paste
    // into a terminal or a bug report wants.
    TW_CHECK_EQ(QGuiApplication::clipboard()->text().toStdString(),
                QDir::toNativeSeparators(path).toStdString());
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
