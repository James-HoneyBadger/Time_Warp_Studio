// SPDX-License-Identifier: MIT
//
// Tests for the editor and the document lifecycle.
//
// These cover the parts of the IDE that touch the user's files, so the
// interesting cases are the ones where getting it wrong loses work: opening a
// file that is already open, a byte-order mark left in the first line, saving
// over a file that is being written, and the unsaved-changes prompt that decides
// whether a close destroys anything.
//
// Runs offscreen, so it needs no display and belongs in CI alongside the rest.
//
// #### What is deliberately not tested here
//
// Any path that ends in a modal dialog. `QFileDialog` and `QMessageBox` block
// until a person answers them, so a test that triggers one does not fail, it
// hangs -- and a hang in CI is worse than a missing assertion. The unreadable
// file and cancelled-save paths are therefore covered by hand and described in
// the README rather than asserted here. Everything else in the lifecycle is
// exercised through the same entry points the dialogs call.

#include <cmath>
#include <cstdio>
#include <functional>

#include <QApplication>
#include <QComboBox>
#include <QCoreApplication>
#include <QElapsedTimer>
#include <QFile>
#include <QLabel>
#include <QPlainTextEdit>
#include <QSettings>
#include <QStatusBar>
#include <QTabWidget>
#include <QTemporaryDir>
#include <QTextCursor>
#include <QTextDocument>
#include <QThread>

#include "check.hpp"
#include "timewarp/bridge/python_worker.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace {

using timewarp::ui::CodeEditor;
using timewarp::ui::Language;
using timewarp::ui::MainWindow;
using timewarp::ui::TurtleCanvas;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// Spins the event loop until `done` holds, or gives up.
///
/// Needed because a run happens on another thread and reports back through
/// queued signals: without pumping, a test either races the runner or deadlocks
/// waiting for a slot that can only run when it yields.
[[nodiscard]] bool waitFor(const std::function<bool()>& done, int timeout_ms = 15000) {
    QElapsedTimer timer;
    timer.start();
    while (!done() && timer.elapsed() < timeout_ms) {
        QApplication::processEvents(QEventLoop::AllEvents, 10);
        QThread::msleep(1);
    }
    return done();
}

/// The editor in the window's current tab.
[[nodiscard]] CodeEditor* currentEditorOf(const MainWindow& window) {
    return qobject_cast<CodeEditor*>(window.editorTabs()->currentWidget());
}

/// The text of a status-bar badge, by object name.
[[nodiscard]] QString badgeText(const MainWindow& window, const char* name) {
    const auto* label = window.statusBar()->findChild<QLabel*>(QString::fromLatin1(name));
    return label == nullptr ? QString{} : label->text();
}

/// Writes `contents` to `dir/name` and returns the full path.
[[nodiscard]] QString writeFile(const QTemporaryDir& dir, const QString& name,
                                const QByteArray& contents) {
    const QString path = dir.filePath(name);
    QFile file{path};
    if (!file.open(QIODevice::WriteOnly)) {
        return {};
    }
    file.write(contents);
    file.close();
    return path;
}

// ---------------------------------------------------------------------------
// The language table
// ---------------------------------------------------------------------------

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
    TW_CHECK_EQ(timewarp::ui::allLanguages().size(), 9);
    TW_CHECK_EQ(extensions(Language::Scheme).size(), 3);
    TW_CHECK_EQ(extensions(Language::Logo).size(), 2);
    TW_CHECK_EQ(extensions(Language::Focal).size(), 2);
    TW_CHECK_EQ(extensions(Language::Chip8).size(), 2);
    TW_CHECK_EQ(extensions(Language::Apl).size(), 2);
    TW_CHECK(languageForFile(QStringLiteral("square.foc")) == Language::Focal);
    TW_CHECK(languageForFile(QStringLiteral("font.ch8")) == Language::Chip8);
    TW_CHECK(languageForFile(QStringLiteral("spiral.apl")) == Language::Apl);

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

/// The console and the error panel, by object name.
[[nodiscard]] QPlainTextEdit* viewOf(const MainWindow& window, const char* name) {
    return window.findChild<QPlainTextEdit*>(QString::fromLatin1(name));
}

void test_running_scheme_draws_on_the_canvas() {
    announce("running scheme");
    QTemporaryDir dir;
    // Two moves at a right angle, plus output, so one test covers the whole
    // chain: editor text -> runner thread -> turtle host -> queue -> bridge ->
    // canvas, with the program's own printing arriving separately.
    const QString path = writeFile(
        dir, QStringLiteral("draw.scm"),
        QByteArray{"(display \"drawing\")\n(newline)\n(forward 100)\n(right 90)\n"
                   "(forward 100)\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas == nullptr) {
        return;
    }

    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));

    // Geometry, not just "something happened": 100 north then 100 east is the
    // compass convention, and a mirrored or rotated canvas fails here while
    // still producing a picture that looks fine.
    TW_CHECK_EQ(canvas->strokeCount(), 2);
    TW_CHECK(std::abs(canvas->totalStrokeLength() - 200.0) < 1e-6);
    TW_CHECK(std::abs(canvas->turtlePosition().x() - 100.0) < 1e-6);
    TW_CHECK(std::abs(canvas->turtlePosition().y() - 100.0) < 1e-6);
    TW_CHECK(std::abs(canvas->turtleHeading() - 90.0) < 1e-6);

    // The program's output went to the console, not to the status bar.
    QPlainTextEdit* console = viewOf(window, "consoleOutput");
    TW_CHECK(console != nullptr);
    if (console != nullptr) {
        TW_CHECK(console->toPlainText().contains(QStringLiteral("drawing")));
    }

    // Clearing is instant and is not an undoable edit to the picture.
    window.clearCanvas();
    TW_CHECK_EQ(canvas->strokeCount(), 0);
    window.clearOutput();
    TW_CHECK(console == nullptr || console->toPlainText().isEmpty());
}

void test_pen_up_moves_without_drawing() {
    announce("pen up");
    QTemporaryDir dir;
    const QString path = writeFile(
        dir, QStringLiteral("gap.scm"),
        QByteArray{"(penup)\n(forward 50)\n(pendown)\n(forward 50)\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas == nullptr) {
        return;
    }
    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));

    // One stroke, not two: the first move was with the pen up.
    TW_CHECK_EQ(canvas->strokeCount(), 1);
    TW_CHECK(std::abs(canvas->totalStrokeLength() - 50.0) < 1e-6);
    TW_CHECK(std::abs(canvas->turtlePosition().y() - 100.0) < 1e-6);
}

void test_running_basic_prints_through_the_bridge() {
    announce("running basic through the bridge");
    QString reason;
    if (!timewarp::bridge::locateWorker(&reason).isValid()) {
        // No Python means no bridge, which is an environment fact rather than a
        // defect in the window. The reason names every path that was searched.
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    QTemporaryDir dir;
    // BASIC has no C++ interpreter, so this only works if the window routes it to
    // the Python bridge. That is the point: it covers the *window's* half of the
    // wiring -- which process, which console, which badge -- rather than the bridge
    // itself, which `python_bridge_tests` covers directly.
    const QString path = writeFile(dir, QStringLiteral("hello.bas"),
                                   QByteArray{"10 PRINT \"HELLO FROM BASIC\"\n20 END\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));

    // The program's own output, having crossed a process boundary, is in the
    // console the user is looking at.
    QPlainTextEdit* console = viewOf(window, "consoleOutput");
    TW_CHECK(console != nullptr);
    if (console != nullptr) {
        TW_CHECK(console->toPlainText().contains(QStringLiteral("HELLO FROM BASIC")));
    }

    // And nothing was reported as a failure.
    QPlainTextEdit* errors = viewOf(window, "errorsLog");
    TW_CHECK(errors != nullptr);
    if (errors != nullptr) {
        TW_CHECK(errors->toPlainText().isEmpty());
    }
}

void test_running_logo_draws_on_the_canvas() {
    announce("running logo through the bridge");
    QString reason;
    if (!timewarp::bridge::locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    QTemporaryDir dir;
    // The report this test exists for: "I ran a Logo program and none of the
    // graphics showed up". A square, so the assertion can be about more than a
    // count: four strokes of 100 units, ending where it began.
    const QString path = writeFile(dir, QStringLiteral("square.logo"),
                                   QByteArray{"REPEAT 4 [FD 100 RT 90]\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas == nullptr) {
        return;
    }
    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));

    TW_CHECK_EQ(canvas->strokeCount(), 4);
    TW_CHECK(std::abs(canvas->totalStrokeLength() - 400.0) < 1e-6);
    // The compass convention again, this time after a journey through a child
    // process: north first, then east.
    TW_CHECK(std::abs(canvas->turtlePosition().x()) < 1e-6);
    TW_CHECK(std::abs(canvas->turtlePosition().y()) < 1e-6);
    // The reference's own background came with it. This is the assertion that
    // matters most for visibility: the reference's pen is white, so a canvas that
    // kept its own white background would show nothing at all while reporting a
    // successful run and four strokes drawn.
    TW_CHECK_EQ(canvas->background().red(), 10);
    TW_CHECK_EQ(canvas->background().blue(), 20);
}

void test_stopping_when_nothing_runs_is_harmless() {
    announce("stop with no run");
    MainWindow window;
    window.stopRunningDocument();
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));
}

void test_stop_interrupts_a_running_program() {
    announce("stopping a run");
    QTemporaryDir dir;
    // A tail-recursive loop that never returns its own account: the point is a
    // program that would run forever if Stop did nothing. The explicit-stack
    // machine makes this a constant-space spin rather than a stack overflow, so
    // the test is measuring the stop, not the machine's limits.
    const QString path = writeFile(
        dir, QStringLiteral("forever.scm"),
        QByteArray{"(define (loop n) (forward 1) (loop (+ n 1)))\n(loop 0)\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    window.runCurrentDocument();
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("running"));

    // Wait until it has actually drawn something, so the stop is interrupting a
    // program that is demonstrably working rather than racing the thread start.
    TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(waitFor([canvas] { return canvas->strokeCount() > 0; }, 5000));

    window.stopRunningDocument();
    TW_CHECK(waitFor([&window] {
        const QString state = badgeText(window, "executionBadge");
        return state != QStringLiteral("running") && state != QStringLiteral("stopping");
    }));
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("stopped"));

    // And the thread really is gone, not merely quiet: the window must be able
    // to close, which joins it.
    const int drawn = canvas->strokeCount();
    TW_CHECK(drawn > 0);
    TW_CHECK(window.close());
    TW_CHECK_EQ(canvas->strokeCount(), drawn);
}

}  // namespace

int main(int argc, char** argv) {
    // Set before QApplication exists, or Qt tries to open a real display.
    qputenv("QT_QPA_PLATFORM", "offscreen");
    QApplication app{argc, argv};
    // A settings identity of its own, so a test run cannot touch the real one.
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("UiDocumentTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);

    test_language_mapping();
    test_editor_widget();

    test_a_fresh_window_has_one_real_document();
    test_opening_a_file();
    test_opening_the_same_file_twice_raises_the_first_tab();
    test_moving_the_cursor_updates_the_status_bar();
    test_editing_marks_the_document_and_the_title();
    test_choosing_a_language_sticks();
    test_closing_tabs();
    test_saving_a_document();
    test_a_file_that_cannot_be_read_is_refused();
    test_a_byte_order_mark_is_not_part_of_the_program();

    test_running_scheme_draws_on_the_canvas();
    test_pen_up_moves_without_drawing();
    test_running_basic_prints_through_the_bridge();
    test_running_logo_draws_on_the_canvas();
    test_stopping_when_nothing_runs_is_harmless();
    test_stop_interrupts_a_running_program();

    return tw::test::summary("ui_documents");
}
