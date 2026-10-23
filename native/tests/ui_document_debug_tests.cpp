// SPDX-License-Identifier: MIT
//
// Breakpoints: a paused run, a whole-program run paused the same way, the gutter marks
// that persist, and a stale path that is dropped.

#include "ui_document/support.hpp"

namespace tw::ui_document {

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

/// A whole-program language pauses too: Logo stops before the top-level
/// statement whose line carries the breakpoint, and the Debug tab and canvas
/// agree about where that is.

void test_a_whole_program_run_pauses_at_a_breakpoint() {
    announce("running: logo pauses at a breakpoint");
    QTemporaryDir dir;
    const QString path = writeFile(dir, QStringLiteral("paused.logo"),
                                   QByteArray{"FD 10\nRT 90\nFD 20\n"});
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
    // Zero-based line 1: the RT 90 statement has not happened yet.
    editor->setBreakpoints({1});
    window.runCurrentDocument();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") == QStringLiteral("paused");
    }));

    TurtleCanvas* canvas = window.turtleCanvas();
    TW_CHECK(canvas != nullptr);
    if (canvas != nullptr) {
        TW_CHECK(waitFor([canvas] { return canvas->strokeCount() == 1; }));
        // The second FD has not run, so there is exactly one stroke.
        TW_CHECK_EQ(canvas->strokeCount(), 1);
    }
    auto* status = window.debugPanel()->findChild<QLabel*>(QStringLiteral("debugStatus"));
    TW_CHECK(status != nullptr);
    if (status != nullptr) {
        TW_CHECK(status->text().contains(QStringLiteral("line 2")));
    }

    window.findChild<QAction*>(QStringLiteral("continueAction"))->trigger();
    TW_CHECK(waitFor([&window] {
        return badgeText(window, "executionBadge") != QStringLiteral("paused") &&
               badgeText(window, "executionBadge") != QStringLiteral("running");
    }));
    if (canvas != nullptr) {
        TW_CHECK_EQ(canvas->strokeCount(), 2);
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

}  // namespace tw::ui_document
