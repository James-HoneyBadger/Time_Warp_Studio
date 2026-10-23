// SPDX-License-Identifier: MIT
//
// Running a program: what reaches the canvas, what reaches the panels, the terminal,
// and stopping a run that is still going.

#include "ui_document/support.hpp"

namespace tw::ui_document {

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

// The engine draws through `TurtleHost`, and the IDE's host is
// `NativeCanvasHost`. A sprite restored by `PUT` reaches it as a pixel block:
// the canvas draws shapes, not a framebuffer, so the host has to turn the block
// into a canvas command that carries the pixels.

void test_the_native_host_blits_a_sprite() {
    announce("native sprite blit");
    timewarp::canvas::CanvasCommandQueue queue;
    timewarp::ui::NativeCanvasHost host{queue};

    const std::vector<timewarp::lang::Rgb> pixels{
        {255, 0, 0}, {0, 255, 0}, {0, 0, 255}, {255, 255, 255}};
    host.put_pixels(7, 9, 2, 2, pixels);

    int commands = 0;
    timewarp::canvas::CanvasCommandKind kind =
        timewarp::canvas::CanvasCommandKind::None;
    std::vector<double> payload;
    queue.drain([&](timewarp::canvas::CanvasCommandView view) {
        ++commands;
        kind = view.command.kind;
        payload.assign(view.points.begin(), view.points.end());
        return true;
    });

    TW_CHECK_EQ(commands, 1);
    TW_CHECK(kind == timewarp::canvas::CanvasCommandKind::PutPixels);
    TW_CHECK_EQ(payload.size(), std::size_t{4});
    if (payload.size() == 4) {
        // Row-major, top row first: the top-left pixel is the first value and
        // the bottom-right the last.
        TW_CHECK_EQ(static_cast<std::uint32_t>(payload[0]),
                    timewarp::canvas::make_rgb(255, 0, 0));
        TW_CHECK_EQ(static_cast<std::uint32_t>(payload[3]),
                    timewarp::canvas::make_rgb(255, 255, 255));
    }
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
    const QColor paper_before = canvas->background();
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
    // And the run did not *choose* a paper: the surface it painted on is the one the
    // canvas already had. The assertion this replaced was `background() != white`,
    // which said "the theme's paper is not white" -- true of the default theme and
    // false of Windows 2000, whose paper is exactly white, so a store left on that
    // theme by an interrupted run failed a case that had nothing to do with it. The
    // invariant that matters is that a run and an idle canvas agree.
    TW_CHECK_EQ(canvas->background(), paper_before);
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

    // Logo can stop: at a top-level statement. The panel is live even though
    // this run had no breakpoint to stop at.
    TW_CHECK(window.debugPanel()->isAvailable());
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

    // FOCAL stops at a statement like every line-loop language, so the panel is
    // live rather than explaining itself away.
    TW_CHECK(window.debugPanel()->isAvailable());
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
        // folder without the user typing a path. `pwd` is the POSIX way to ask
        // and `cd` is cmd.exe's; Windows also folds a path's case, so the
        // comparison ignores it there and everywhere below.
        // The shell prints the folder the way it was given it -- backslashes on
        // Windows, and the logical path rather than the resolved one on macOS,
        // where /var is a symlink to /private/var -- so this is the absolute path
        // in the platform's separators, compared without case.
        const QString folder =
            QDir::toNativeSeparators(QFileInfo{dir.path()}.absoluteFilePath());
#ifdef Q_OS_WIN
        terminal->runCommand(QStringLiteral("cd"));
#else
        terminal->runCommand(QStringLiteral("pwd"));
#endif
        TW_CHECK(waitFor([terminal, folder] {
            return terminal->output().contains(folder, Qt::CaseInsensitive);
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

/// A light theme must not draw white on white.
///
/// The reference's pen is white, which is what its dark surface wants. A theme
/// whose paper is light -- Windows 2000, High Contrast Light -- inherits that pen
/// and produces a run that reports four strokes and shows nothing, which is the
/// exact complaint the canvas tests exist to catch. The paper decides the
/// starting ink, so the same program draws black lines on white paper.

/// Puts the theme back when it goes out of scope.
///
/// The theme is a persisted setting, so a case that changes it and then returns early
/// -- an unavailable canvas, a failed assertion is not enough, but a `return` is --
/// leaves the next case, and the next *run* of the suite, on a surface it did not
/// choose. That is how a store ended up on Windows 2000 and failed a run case three
/// cases earlier.
class ThemeRestorer {
public:
    explicit ThemeRestorer(MainWindow& window)
        : window_{window}, previous_{window.currentThemeName()} {}
    ThemeRestorer(const ThemeRestorer&) = delete;
    ThemeRestorer& operator=(const ThemeRestorer&) = delete;
    ~ThemeRestorer() { window_.applyTheme(previous_); }

private:
    MainWindow& window_;
    QString previous_;
};

void test_a_light_theme_draws_dark_ink() {
    announce("a light theme draws dark ink");
    QTemporaryDir dir;
    const QString path = writeFile(dir, QStringLiteral("square.logo"),
                                   QByteArray{"REPEAT 4 [FD 100 RT 90]\n"});
    TW_CHECK(!path.isEmpty());

    MainWindow window;
    const ThemeRestorer restore{window};
    window.applyTheme(QStringLiteral("Windows 2000"));
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
    const QColor ink = canvas->strokeColor(0);
    TW_CHECK(ink.isValid());
    TW_CHECK(ink.lightness() < 128);
    window.close();
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

}  // namespace tw::ui_document
