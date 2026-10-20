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
#include <QFileInfo>
#include <QLabel>
#include <QPlainTextEdit>
#include <QSettings>
#include <QStatusBar>
#include <QTableWidget>
#include <QTabWidget>
#include <QTemporaryDir>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextDocument>
#include <QThread>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/bridge/python_worker.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/autosave.hpp"
#include "timewarp/ui/breakpoints.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace {

using timewarp::ui::CodeEditor;
using timewarp::ui::FindBar;
using timewarp::ui::Language;
using timewarp::ui::MainWindow;
using timewarp::ui::TelemetryPanel;
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
    TW_CHECK_EQ(timewarp::ui::allLanguages().size(), 10);
    TW_CHECK_EQ(extensions(Language::Scheme).size(), 3);
    TW_CHECK_EQ(extensions(Language::Logo).size(), 2);
    TW_CHECK_EQ(extensions(Language::Focal).size(), 2);
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

void test_running_basic_prints_natively() {
    announce("running basic natively");
    QTemporaryDir dir;
    // BASIC was the last port. This covers the window's routing now that
    // `executor_for` answers for it -- editor text -> native runner -> console
    // and badge. (The old version skipped when no Python was found, which hid
    // the routing on exactly the machines that needed it least.)
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
    announce("running logo natively");
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
    // The compass convention again, this time after a journey through the
    // native runner: north first, then east.
    TW_CHECK(std::abs(canvas->turtlePosition().x()) < 1e-6);
    TW_CHECK(std::abs(canvas->turtlePosition().y()) < 1e-6);
    // The engine's own surface came with it. This is the assertion that
    // matters most for visibility: the engine's pen is white, so a canvas
    // that kept its own white background would show nothing at all while
    // reporting a successful run and four strokes drawn.
    //
    // Since the themes landed, the surface a run starts on is the *theme's*
    // canvas paper rather than the reference's fixed colour, so the assertion is
    // that the run and the idle canvas agree with each other and with the theme
    // -- not that they are one particular pair of numbers.
    const QColor paper = timewarp::ui::themes::colour(
        timewarp::ui::themes::byName(window.currentThemeName()).canvas_bg);
    TW_CHECK_EQ(canvas->background(), paper);
    const QColor white{255, 255, 255};
    TW_CHECK(canvas->background() != white);
}

/// The text of one cell in a named table, or an empty string when the row or the
/// cell is not there. Read through the widget rather than the panel on purpose:
/// the table is what a user reads.
[[nodiscard]] QString cellIn(const MainWindow& window, const char* table, int row,
                             int column) {
    QTableWidget* widget = window.findChild<QTableWidget*>(QString::fromLatin1(table));
    if (widget == nullptr || row >= widget->rowCount() || column >= widget->columnCount()) {
        return {};
    }
    const QTableWidgetItem* item = widget->item(row, column);
    return item == nullptr ? QString{} : item->text();
}

/// The row a variables table shows for `name`, or -1.
[[nodiscard]] int rowNamed(const MainWindow& window, const char* table, const QString& name) {
    QTableWidget* widget = window.findChild<QTableWidget*>(QString::fromLatin1(table));
    if (widget == nullptr) {
        return -1;
    }
    for (int row = 0; row < widget->rowCount(); ++row) {
        if (const QTableWidgetItem* item = widget->item(row, 0);
            item != nullptr && item->text() == name) {
            return row;
        }
    }
    return -1;
}

void test_a_scheme_run_fills_the_variables_and_telemetry_panels() {
    announce("a scheme run fills the panels");
    QTemporaryDir dir;
    // Two bindings the machine can describe, one number and one procedure, so the
    // type column is exercised as well as the names.
    const QString path = writeFile(
        dir, QStringLiteral("bind.scm"),
        QByteArray{"(define (area w h) (* w h))\n(define total (area 3 4))\n"
                   "(display total)\n(newline)\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    TW_CHECK(window.variablesPanel() != nullptr);
    TW_CHECK(window.telemetryPanel() != nullptr);
    TW_CHECK(window.debugPanel() != nullptr);
    if (window.variablesPanel() == nullptr || window.telemetryPanel() == nullptr
        || window.debugPanel() == nullptr) {
        return;
    }

    // Before anything runs there is nothing to show. A previous run's numbers
    // sitting in the HUD while a new one starts would read as this run's.
    TW_CHECK_EQ(window.variablesPanel()->rowCount(), 0);
    TW_CHECK_EQ(window.telemetryPanel()->rowCount(), 0);

    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));

    // The variables panel: what the program left behind, with the type the
    // machine knows it by -- `total` is 12, and `area` is a procedure. A run that
    // never paused still gets here, which is the whole point of the final
    // snapshot.
    const int total = rowNamed(window, "variablesTable", QStringLiteral("total"));
    TW_CHECK(total >= 0);
    if (total >= 0) {
        TW_CHECK_EQ(cellIn(window, "variablesTable", total, 1), QStringLiteral("number"));
        TW_CHECK_EQ(cellIn(window, "variablesTable", total, 2), QStringLiteral("12"));
    }
    const int area = rowNamed(window, "variablesTable", QStringLiteral("area"));
    TW_CHECK(area >= 0);
    if (area >= 0) {
        TW_CHECK_EQ(cellIn(window, "variablesTable", area, 1),
                    QStringLiteral("procedure"));
    }

    // The telemetry HUD: a Scheme run reports nothing over the bridge, so every
    // row here is one the IDE measured itself. All four must be present.
    TelemetryPanel* hud = window.telemetryPanel();
    TW_CHECK_EQ(hud->valueOf(QStringLiteral("Language")), QStringLiteral("Scheme"));
    TW_CHECK(!hud->valueOf(QStringLiteral("Outcome")).isEmpty());
    TW_CHECK(!hud->valueOf(QStringLiteral("Wall clock")).isEmpty());
    TW_CHECK(hud->valueOf(QStringLiteral("Canvas ink")).contains(QStringLiteral("segment")));
    TW_CHECK(!hud->valueOf(QStringLiteral("Ink length")).isEmpty());

    // The debug panel is live for Scheme and idle: the run finished, so it must
    // not claim to be paused anywhere.
    TW_CHECK(window.debugPanel()->isAvailable());
    auto* status = window.debugPanel()->findChild<QLabel*>(QStringLiteral("debugStatus"));
    TW_CHECK(status != nullptr);
    if (status != nullptr) {
        TW_CHECK(!status->text().contains(QStringLiteral("at line")));
        TW_CHECK(!status->text().contains(QStringLiteral("running")));
    }
}

void test_a_native_run_fills_the_variables_and_telemetry_panels() {
    announce("a native run fills the panels");
    QTemporaryDir dir;
    // A Logo program with a loop variable and a word of its own, so the engine
    // has something to report on both counts.
    const QString path = writeFile(
        dir, QStringLiteral("named.logo"),
        QByteArray{"MAKE \"NAME \"ada\nREPEAT 4 [FD 50 RT 90]\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    if (window.variablesPanel() == nullptr || window.telemetryPanel() == nullptr ||
        window.debugPanel() == nullptr) {
        TW_CHECK(false);
        return;
    }

    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));

    // `MAKE "NAME "ada` -- a word, which the engine reports beside the numbers.
    const int name = rowNamed(window, "variablesTable", QStringLiteral("NAME"));
    TW_CHECK(name >= 0);
    if (name >= 0) {
        TW_CHECK_EQ(cellIn(window, "variablesTable", name, 1), QStringLiteral("text"));
        TW_CHECK_EQ(cellIn(window, "variablesTable", name, 2), QStringLiteral("ada"));
    }

    // The HUD carries both halves: the engine's counters and the IDE's own. A
    // panel that showed only one of them would be reporting half a run.
    TelemetryPanel* hud = window.telemetryPanel();
    TW_CHECK_EQ(hud->valueOf(QStringLiteral("Language")), QStringLiteral("Logo"));
    TW_CHECK(!hud->valueOf(QStringLiteral("Steps")).isEmpty());
    TW_CHECK(hud->valueOf(QStringLiteral("Steps")).toInt() > 0);
    TW_CHECK(!hud->valueOf(QStringLiteral("Wall clock")).isEmpty());
    TW_CHECK(hud->valueOf(QStringLiteral("Canvas ink")).contains(QStringLiteral("4 segments")));

    // A native run has no pause protocol either, so the debug panel says that
    // rather than offering buttons that would do nothing.
    TW_CHECK(!window.debugPanel()->isAvailable());
}

void test_a_second_run_replaces_the_panels_rather_than_appending() {
    announce("a second run replaces the panels");
    QTemporaryDir dir;
    const QString path = writeFile(
        dir, QStringLiteral("twice.scm"),
        QByteArray{"(define (area w h) (* w h))\n(define total (area 3 4))\n(newline)\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    if (window.variablesPanel() == nullptr) {
        TW_CHECK(false);
        return;
    }

    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    const int first = window.variablesPanel()->rowCount();
    TW_CHECK(first > 0);

    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    // The same program twice is the same table twice. Rows accumulating across
    // runs would look like a program with more variables than it has.
    TW_CHECK_EQ(window.variablesPanel()->rowCount(), first);
}

void test_a_native_run_fills_the_panels() {
    announce("a native focal run fills the panels");
    QTemporaryDir dir;
    // A value, a loop variable and four sides of a square: both panels and the
    // canvas have something real to report.
    const QString path = writeFile(
        dir, QStringLiteral("square.foc"),
        QByteArray{"01.10 S X=7\n01.20 T \"X=\",X\n01.30 FOR I=1,4;FD 50;RT 90\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    TW_CHECK(window.openFile(path));
    if (window.variablesPanel() == nullptr || window.telemetryPanel() == nullptr
        || window.debugPanel() == nullptr) {
        TW_CHECK(false);
        return;
    }

    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    TW_CHECK_EQ(badgeText(window, "executionBadge"), QStringLiteral("ready"));

    // FOCAL ran in this process. Its tables arrive through the native runner,
    // which is what makes the panels agnostic about the engine.
    const int x = rowNamed(window, "variablesTable", QStringLiteral("X"));
    TW_CHECK(x >= 0);
    if (x >= 0) {
        TW_CHECK_EQ(cellIn(window, "variablesTable", x, 1), QStringLiteral("number"));
        TW_CHECK_EQ(cellIn(window, "variablesTable", x, 2), QStringLiteral("7"));
    }
    const int i = rowNamed(window, "variablesTable", QStringLiteral("I"));
    TW_CHECK(i >= 0);
    if (i >= 0) {
        TW_CHECK_EQ(cellIn(window, "variablesTable", i, 2), QStringLiteral("4"));
    }

    TelemetryPanel* hud = window.telemetryPanel();
    TW_CHECK_EQ(hud->valueOf(QStringLiteral("Language")), QStringLiteral("FOCAL"));
    TW_CHECK(!hud->valueOf(QStringLiteral("Steps")).isEmpty());
    TW_CHECK(!hud->valueOf(QStringLiteral("Engine time")).isEmpty());
    TW_CHECK(hud->valueOf(QStringLiteral("Canvas ink")).contains(QStringLiteral("4 segments")));

    // Native, like every language but Scheme: no pause protocol, so the panel
    // says so.
    TW_CHECK(!window.debugPanel()->isAvailable());
}

/// The terminal is opt-in: a build nobody configured has no shell tab at all.
void test_the_terminal_is_off_by_default() {
    announce("terminal: off unless enabled");
    QSettings{}.remove(QStringLiteral("terminal/enabled"));
    MainWindow window;
    TW_CHECK(window.findChild<timewarp::ui::TerminalWidget*>() == nullptr);
    TW_CHECK(!window.terminalEnabled());
}

/// With the setting on, the tab exists, a command runs in the shell, the shell
/// starts in the document's folder, and closing the window ends the process.
void test_the_terminal_runs_commands_when_enabled() {
    announce("terminal: echo hi, then close");
    QSettings{}.setValue(QStringLiteral("terminal/enabled"), true);
    {
        QTemporaryDir dir;
        TW_CHECK(dir.isValid());
        const QString path = writeFile(dir, QStringLiteral("program.bas"),
                                       QByteArray{"10 PRINT 1\n"});
        TW_CHECK(!path.isEmpty());

        MainWindow window;
        TW_CHECK(window.terminalEnabled());
        auto* terminal = window.findChild<timewarp::ui::TerminalWidget*>();
        TW_CHECK(terminal != nullptr);
        if (terminal == nullptr) {
            return;
        }
        QTabWidget* tabs = window.findChild<QTabWidget*>(QStringLiteral("outputSubTabs"));
        TW_CHECK(tabs != nullptr && tabs->indexOf(terminal) >= 0);

        TW_CHECK(window.openFile(path));
        terminal->runCommand(QStringLiteral("echo hi"));
        TW_CHECK(waitFor([terminal] { return terminal->output().contains(QStringLiteral("hi")); }));

        // The shell follows the document, so a build command names the right
        // folder without the user typing a path.
        terminal->runCommand(QStringLiteral("pwd"));
        const QString folder = QFileInfo{dir.path()}.canonicalFilePath();
        TW_CHECK(waitFor([terminal, folder] {
            return terminal->output().contains(folder);
        }));
        TW_CHECK(terminal->isRunning());

        window.close();
        TW_CHECK(!terminal->isRunning());
    }
    QSettings{}.remove(QStringLiteral("terminal/enabled"));
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

// ---------------------------------------------------------------------------
// Find and replace
//
// The searching lives on the editor rather than in the find bar, so it is
// exercised through the editor: the bar is a view over these calls, and a
// failure here is a failure with no window in the stack.
// ---------------------------------------------------------------------------

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

void test_recent_files_are_remembered() {
    announce("recent files");
    const QTemporaryDir dir;
    TW_CHECK(dir.isValid());
    MainWindow window;
    // The list lives in settings, so it starts out holding whatever earlier tests
    // opened -- and whatever the last real session left there.
    window.clearRecentFiles();
    TW_CHECK(window.recentFiles().isEmpty());

    const QString first = writeFile(dir, QStringLiteral("one.bas"), "PRINT 1\n");
    const QString second = writeFile(dir, QStringLiteral("two.logo"), "FORWARD 10\n");
    TW_CHECK(window.openFile(first));
    TW_CHECK(window.openFile(second));

    // Most recent first, and opening a file again moves it rather than adding a
    // second entry for the same file.
    QStringList recent = window.recentFiles();
    TW_CHECK_EQ(recent.size(), 2);
    TW_CHECK_EQ(recent.first(), second);
    TW_CHECK(window.openFile(first));
    recent = window.recentFiles();
    TW_CHECK_EQ(recent.size(), 2);
    TW_CHECK_EQ(recent.first(), first);

    // Capped: a menu of forty paths is a menu nobody reads.
    for (int index = 0; index < 14; ++index) {
        const QString path = writeFile(
            dir, QStringLiteral("f%1.bas").arg(index), "PRINT 1\n");
        TW_CHECK(window.openFile(path));
    }
    TW_CHECK_EQ(window.recentFiles().size(), 10);
}

void test_the_session_comes_back() {
    announce("session restore");
    const QTemporaryDir dir;
    const QString first = writeFile(dir, QStringLiteral("keep.bas"), "PRINT 1\n");
    const QString second = writeFile(dir, QStringLiteral("keep.logo"), "FORWARD 10\n");
    {
        MainWindow window;
        TW_CHECK(window.openFile(first));
        TW_CHECK(window.openFile(second));
        window.saveSession();
    }

    MainWindow restored;
    TW_CHECK_EQ(restored.restoreSession(), 2);
    // The list is bound to a name first: `openFilePaths().first()` is a reference
    // into a temporary, which GCC's -Wdangling-reference is right to refuse.
    const QStringList restored_paths = restored.openFilePaths();
    TW_CHECK_EQ(restored_paths.size(), 2);
    TW_CHECK_EQ(restored_paths.first(), first);

    // An untitled buffer has no name to reopen, so it is not part of a session:
    // a tab restored as "untitled 3" would be a document the user never had.
    MainWindow with_untitled;
    TW_CHECK(with_untitled.openFile(first));
    with_untitled.newFile();
    with_untitled.saveSession();
    MainWindow after;
    TW_CHECK_EQ(after.restoreSession(), 1);

    // A file that has been deleted since is skipped rather than reported. A
    // prompt at startup that cannot be dismissed is worse than a missing tab.
    QFile::remove(second);
    after.saveSession();
    MainWindow again;
    TW_CHECK_EQ(again.restoreSession(), 1);
    TW_CHECK_EQ(again.openFilePaths().size(), 1);
}

/// The data directory is where autosave, version history and classroom bundles
/// will write, so the two things that matter are that the environment variable
/// wins and that an unset one does not write into the working directory.
void test_user_data_lives_where_the_environment_says() {
    announce("user data directory");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }

    scratch.useAsDataDir();
    TW_CHECK_EQ(timewarp::ui::dataDir().toStdString(), scratch.path().toStdString());

    // A named subdirectory is created rather than merely named: the caller is
    // about to write a snapshot into it.
    const QString autosave = timewarp::ui::dataDirFor(QStringLiteral("autosave"));
    TW_CHECK(!autosave.isEmpty());
    TW_CHECK(QFileInfo{autosave}.isDir());
    TW_CHECK_EQ(autosave.toStdString(),
                (scratch.path() + QStringLiteral("/autosave")).toStdString());

    // A leaf that escapes the data directory is refused, because the only
    // reason to pass one is a mistake.
    TW_CHECK(timewarp::ui::dataDirFor(QStringLiteral("../elsewhere")).isEmpty());
    TW_CHECK(timewarp::ui::dataDirFor(QStringLiteral("/etc")).isEmpty());

    // `~` expands the way the Python engine expands it.
    qputenv("TIME_WARP_DATA_DIR", "~/tw-probe");
    TW_CHECK_EQ(timewarp::ui::dataDir().toStdString(),
                (QDir::homePath() + QStringLiteral("/tw-probe")).toStdString());

    // And unset it is the reference engine's directory, not the current one.
    scratch.restoreDataDir();
    TW_CHECK_EQ(timewarp::ui::dataDir().toStdString(),
                (QDir::homePath() + QStringLiteral("/.time_warp")).toStdString());
}


/// Insertion is the half of the snippet feature that can be tested: the picker is
/// modal, so `applySnippet` is where the behaviour lives.
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
void test_autosave_keeps_unsaved_work() {
    announce("autosave: a snapshot of unsaved work");
    tw::test::TempDir scratch;
    TW_CHECK(scratch.valid());
    if (!scratch.valid()) {
        return;
    }
    // Before the window exists: the store resolves its directory when it is
    // constructed.
    scratch.useAsDataDir();

    MainWindow window;
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        scratch.restoreDataDir();
        return;
    }

    // An unmodified document is not snapshotted: it is already on disk or it is
    // an untouched buffer, and either way a copy would push the useful ones out.
    window.autosaveNow();
    timewarp::ui::AutosaveStore store;
    TW_CHECK(store.all().isEmpty());

    // Typed, not `setPlainText`: setting the text clears the modified flag, which
    // is the flag autosave asks about.
    editor->setPlainText(QString{});
    QTextCursor typed = editor->textCursor();
    typed.insertText(QStringLiteral("10 PRINT \"UNSAVED\"\n"));
    TW_CHECK(editor->document()->isModified());
    window.autosaveNow();

    const QList<timewarp::ui::Snapshot> snapshots = store.all();
    TW_CHECK_EQ(snapshots.size(), 1);
    if (!snapshots.isEmpty()) {
        TW_CHECK_EQ(store.read(snapshots.first()).toStdString(),
                    std::string{"10 PRINT \"UNSAVED\"\n"});
        // An untitled buffer is recorded as such: there is no path to name it by.
        TW_CHECK(snapshots.first().document.startsWith(QStringLiteral("untitled:")));
    }

    // The second snapshot of the same buffer supersedes rather than accumulates
    // inside one session's millisecond, and the store stays bounded.
    window.autosaveNow();
    TW_CHECK(store.all().size() <= 2);
    scratch.restoreDataDir();
}

/// A binary ROM and a text listing share `.ch8`. The ROM is decoded through the
/// byte-preserving codec, opened read-only so it cannot be edited into
/// corruption, and run without the assembler seeing it.
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

void test_the_split_survives_a_restart() {
    announce("session restore: the split and which side each document was on");
    const QTemporaryDir dir;
    const QString left = writeFile(dir, QStringLiteral("left.bas"), "PRINT 1\n");
    const QString right = writeFile(dir, QStringLiteral("right.bas"), "PRINT 2\n");
    {
        MainWindow window;
        TW_CHECK(window.openFile(left));
        TW_CHECK(window.openFile(right));
        // Splitting moves the current document -- `right` -- into the second pane
        // and leaves a fresh document behind, so both halves are usable.
        window.splitEditor();
        TW_CHECK(window.editorSplit());
        TW_CHECK_EQ(window.secondaryFilePaths().size(), 1);
        window.saveSession();
    }

    MainWindow restored;
    TW_CHECK_EQ(restored.restoreSession(), 2);
    TW_CHECK(restored.editorSplit());
    // `isHidden`, not `isVisible`: the window has not been shown, and a child of an
    // unshown window is never "visible" whatever its own flag says.
    TW_CHECK(!restored.secondaryEditorTabs()->isHidden());
    const QStringList secondary = restored.secondaryFilePaths();
    TW_CHECK_EQ(secondary.size(), 1);
    if (!secondary.isEmpty()) {
        TW_CHECK_EQ(secondary.first(), right);
    }
    // And the primary still has the document that was on that side.
    const QStringList primary = restored.openFilePaths();
    TW_CHECK_EQ(primary.size(), 1);
    if (!primary.isEmpty()) {
        TW_CHECK_EQ(primary.first(), left);
    }

    // A saved split whose other pane is empty is a blank rectangle, so it is not
    // restored as one.
    QSettings settings;
    settings.setValue(QStringLiteral("session/editorSplit"), true);
    settings.setValue(QStringLiteral("session/secondaryFiles"), QStringList{});
    MainWindow empty_split;
    TW_CHECK_EQ(empty_split.restoreSession(), 1);
    TW_CHECK(!empty_split.editorSplit());
    TW_CHECK(!empty_split.secondaryEditorTabs()->isVisible());
}

void test_a_run_pauses_at_a_breakpoint() {
    announce("running: a breakpoint pauses the run");
    QTemporaryDir dir;
    const QString path = writeFile(dir, QStringLiteral("paused.bas"),
                                   QByteArray{"10 PRINT \"ONE\"\n"
                                              "20 PRINT \"TWO\"\n"
                                              "30 PRINT \"THREE\"\n"
                                              "40 END\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    window.resize(900, 600);
    window.show();
    QApplication::processEvents();
    TW_CHECK(window.openFile(path, MainWindow::OpenFailure::Silent));
    CodeEditor* editor = window.currentEditor();
    TW_CHECK(editor != nullptr);
    if (editor == nullptr) {
        return;
    }
    // Before line 20, which is the second statement.
    editor->setBreakpoints({1});
    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") == QStringLiteral("paused");
    }));

    QPlainTextEdit* console = viewOf(window, "consoleOutput");
    TW_CHECK(console != nullptr);
    if (console != nullptr) {
        // ONE has run; TWO has not, because the pause comes before the statement.
        TW_CHECK(console->toPlainText().contains(QStringLiteral("ONE")));
        TW_CHECK(!console->toPlainText().contains(QStringLiteral("TWO")));
    }

    // Continue finishes the program from the pause.
    window.findChild<QAction*>(QStringLiteral("continueAction"))->trigger();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("paused") &&
               badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    if (console != nullptr) {
        TW_CHECK(console->toPlainText().contains(QStringLiteral("THREE")));
    }
}

void test_breakpoints_toggle_paint_and_persist() {
    announce("breakpoints: toggle, paint and persist");
    tw::test::TempDir dir;
    const QString path = dir.write(QStringLiteral("prog.bas"),
                                   "10 PRINT 1\n20 PRINT 2\n30 PRINT 3\n40 END\n");
    TW_CHECK(!path.isEmpty());
    const QString key = timewarp::ui::breakpoints::keyFor(path);
    QSettings settings;
    settings.remove(key);

    {
        MainWindow window;
        window.resize(900, 600);
        window.show();
        QApplication::processEvents();
        TW_CHECK(window.openFile(path, MainWindow::OpenFailure::Silent));
        CodeEditor* editor = window.currentEditor();
        TW_CHECK(editor != nullptr);
        if (editor == nullptr) {
            return;
        }
        TW_CHECK(editor->breakpoints().isEmpty());

        editor->toggleBreakpoint(0);
        TW_CHECK_EQ(editor->breakpoints().size(), 1);
        TW_CHECK(editor->hasBreakpoint(0));
        // A second toggle on the same line removes exactly that one.
        editor->toggleBreakpoint(0);
        TW_CHECK(editor->breakpoints().isEmpty());
        // Past the last line is refused rather than remembered for a line that
        // does not exist: a breakpoint that can never fire is worse than none.
        editor->toggleBreakpoint(99);
        TW_CHECK(editor->breakpoints().isEmpty());

        editor->toggleBreakpoint(1);
        QApplication::processEvents();
        QWidget* gutter = editor->findChild<QWidget*>(QStringLiteral("lineNumberArea"));
        TW_CHECK(gutter != nullptr);
        if (gutter != nullptr) {
            // "The marker paints" compared against the same gutter with no
            // breakpoints: a pixel count would depend on the font, and comparing an
            // image with itself proves nothing.
            const auto render = [gutter] {
                QImage image{gutter->size(), QImage::Format_ARGB32_Premultiplied};
                image.fill(Qt::transparent);
                gutter->render(&image);
                return image;
            };
            const QImage with_marker = render();
            editor->setBreakpoints({});
            QApplication::processEvents();
            const QImage without_marker = render();
            TW_CHECK(with_marker != without_marker);
            editor->setBreakpoints({1});
        }

        // The gutter's click mapping: line 0 is at the top of the viewport.
        TW_CHECK_EQ(editor->lineAt(2), 0);
    }

    // A new window, from the stored settings alone.
    {
        MainWindow second;
        TW_CHECK(second.openFile(path, MainWindow::OpenFailure::Silent));
        CodeEditor* editor = second.currentEditor();
        TW_CHECK(editor != nullptr);
        if (editor != nullptr) {
            TW_CHECK_EQ(editor->breakpoints().size(), 1);
            TW_CHECK(editor->hasBreakpoint(1));
        }
    }

    // Leave the developer's settings as they were.
    settings.remove(key);
}

void test_a_stale_breakpoint_path_is_dropped() {
    announce("breakpoints: a deleted program takes its breakpoints with it");
    QSettings settings;
    const QString missing = QStringLiteral("/tmp/timewarp-breakpoints-gone-999999.bas");
    TW_CHECK(!QFile::exists(missing));
    const QString key = timewarp::ui::breakpoints::keyFor(missing);
    settings.setValue(key, QStringLiteral("0,1"));

    // Constructing the window is what prunes, so the assertion is about the
    // startup path rather than about a helper nobody calls.
    MainWindow window;
    TW_CHECK(!settings.contains(key));
    TW_CHECK(window.currentEditor() != nullptr);
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
    test_find_next_selects_and_wraps();
    test_replace_all_is_one_step();
    test_the_find_bar_drives_the_editor();
    test_recent_files_are_remembered();
    test_the_session_comes_back();

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
    test_user_data_lives_where_the_environment_says();
    test_a_snippet_is_inserted_at_the_cursor();
    test_run_selection_and_history();
    test_autosave_keeps_unsaved_work();
    test_restoring_a_copy_opens_a_new_tab();
    test_a_binary_chip8_rom_opens_read_only();
    test_word_at_cursor();
    test_context_help_follows_the_document_language();
    test_breakpoints_toggle_paint_and_persist();
    test_a_stale_breakpoint_path_is_dropped();
    test_the_split_survives_a_restart();
    test_a_run_pauses_at_a_breakpoint();

    test_running_scheme_draws_on_the_canvas();
    test_pen_up_moves_without_drawing();
    test_running_basic_prints_natively();
    test_running_logo_draws_on_the_canvas();
    test_a_scheme_run_fills_the_variables_and_telemetry_panels();
    test_a_native_run_fills_the_variables_and_telemetry_panels();
    test_a_second_run_replaces_the_panels_rather_than_appending();
    test_a_native_run_fills_the_panels();
    test_stopping_when_nothing_runs_is_harmless();
    test_stop_interrupts_a_running_program();
    test_the_terminal_is_off_by_default();
    test_the_terminal_runs_commands_when_enabled();

    return tw::test::summary("ui_documents");
}
