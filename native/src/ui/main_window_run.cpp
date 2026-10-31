// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QDir>
#include <QFileDialog>
#include <QFileInfo>
#include <QInputDialog>
#include <QLineEdit>
#include <QMouseEvent>
#include <QScrollBar>
#include <QToolButton>
#ifdef TIME_WARP_HAVE_PRINT
#endif

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/ui/app.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/benchmark.hpp"
#include "timewarp/ui/console.hpp"
#include "timewarp/ui/breakpoints.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/classroom_marker.hpp"
#include "timewarp/ui/command_palette.hpp"
#include "timewarp/ui/export.hpp"
#include "timewarp/ui/find_bar.hpp"
#include "timewarp/ui/find_in_files.hpp"
#include "timewarp/ui/formatter.hpp"
#include "timewarp/ui/help_browser.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/preferences.hpp"
#include "timewarp/ui/profile_report.hpp"
#include "timewarp/ui/project_panel.hpp"
#include "timewarp/ui/retro_computer.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sql_panel.hpp"
#include "timewarp/sql/database.hpp"
#include "timewarp/ui/resizable_tab_widget.hpp"
#include "timewarp/ui/scrubber.hpp"
#include "timewarp/ui/snippet_dialog.hpp"
#include "timewarp/ui/snippets.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/tour.hpp"
#include "timewarp/ui/syntax_highlighter.hpp"
#include "timewarp/ui/terminal_widget.hpp"
#include "timewarp/ui/themes.hpp"
#include "timewarp/ui/turtle_canvas.hpp"
#include "timewarp/ui/version_history.hpp"
#include "timewarp/ui/ui_style.hpp"

// Supplied by CMake from the repository's VERSION file, so the window title and
// the packaged version cannot drift apart. The fallback keeps a standalone
// compile of this file working.
#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::ui {

// The settings keys and the small helpers the sections share; they were an
// anonymous namespace in the one-file version, and a `using` is what keeps the call
// sites reading the same.
using namespace detail;

// ---------------------------------------------------------------------------
// Execution
// ---------------------------------------------------------------------------

MainWindow::~MainWindow() {
    // Before any member is destroyed: a runner thread holds a reference to the
    // canvas queue and to its own host, so letting Qt tear those down first is a
    // write into freed memory from a thread that is still drawing.
    stopEngines();
    // And the marking thread owns copies rather than the class, but a `QThread`
    // destroyed while running is fatal either way.
    if (classroom_marker_ != nullptr) {
        classroom_marker_->wait();
    }
}

timewarp::canvas::CanvasCommandQueue& MainWindow::canvasQueue() noexcept {
    return *canvas_queue_;
}

void MainWindow::appendOutput(const QString& text) {
    if (text.isEmpty() || console_view_ == nullptr) {
        return;
    }
    // The log keeps every line, so the errors-only filter can hide one and give it
    // back: a filter that discarded what it hid would make the toggle one-way.
    console_log_.append(text.split(QLatin1Char('\n'), Qt::KeepEmptyParts));
    if (console_errors_only_) {
        refreshConsole();
        return;
    }
    // `moveCursor` + `insertPlainText` rather than `appendPlainText`, which adds
    // its own paragraph break: the interpreter decides where its lines end, and
    // a view that second-guesses it turns "a" and "b" printed separately into
    // two lines in one build and one line in the other.
    QTextCursor cursor = console_view_->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(text);
    console_view_->setTextCursor(cursor);
}

void MainWindow::refreshConsole() {
    if (console_view_ == nullptr) {
        return;
    }
    QString shown;
    bool first = true;
    for (const QString& line : console_log_) {
        if (console_errors_only_ && !isConsoleError(line)) {
            continue;
        }
        if (!first) {
            shown += QLatin1Char('\n');
        }
        shown += line;
        first = false;
    }
    QScrollBar* bar = console_view_->verticalScrollBar();
    const bool at_end = bar != nullptr && bar->value() >= bar->maximum();
    console_view_->setPlainText(shown);
    if (at_end && bar != nullptr) {
        bar->setValue(bar->maximum());
    }
}

void MainWindow::setConsoleErrorsOnly(bool errors_only) {
    console_errors_only_ = errors_only;
    refreshConsole();
    if (console_filter_ != nullptr && console_filter_->isChecked() != errors_only) {
        console_filter_->setChecked(errors_only);
    }
}

bool MainWindow::followConsoleClick(QWidget* view, const QPoint& position) {
    auto* text = qobject_cast<QPlainTextEdit*>(view);
    CodeEditor* editor = currentEditor();
    if (text == nullptr || editor == nullptr) {
        return false;
    }
    const std::optional<int> line =
        consoleLineLink(text->cursorForPosition(position).block().text());
    if (!line.has_value()) {
        return false;
    }
    // The number the console read is the program's own, which is only the position
    // in a language that does not number its lines.
    editor->goToLine(textLineForSourceLine(editor->toPlainText(), *line));
    editor->setFocus();
    setExecutionState(tr("ready"), tr("Line %1").arg(*line));
    return true;
}

bool MainWindow::eventFilter(QObject* watched, QEvent* event) {
    QPlainTextEdit* view = nullptr;
    if (console_view_ != nullptr && watched == console_view_->viewport()) {
        view = console_view_;
    } else if (errors_view_ != nullptr && watched == errors_view_->viewport()) {
        view = errors_view_;
    }
    if (view == nullptr) {
        return QMainWindow::eventFilter(watched, event);
    }
    if (event->type() == QEvent::MouseButtonRelease) {
        auto* mouse = static_cast<QMouseEvent*>(event);
        if (mouse->button() == Qt::LeftButton && followConsoleClick(view, mouse->pos())) {
            return true;
        }
    } else if (event->type() == QEvent::MouseMove) {
        // A line that would take a click says so before the click.
        auto* mouse = static_cast<QMouseEvent*>(event);
        const bool link =
            consoleLineLink(view->cursorForPosition(mouse->pos()).block().text()).has_value();
        if (watched->property("consoleLinkCursor").toBool() != link) {
            watched->setProperty("consoleLinkCursor", link);
            if (auto* widget = qobject_cast<QWidget*>(watched); widget != nullptr) {
                widget->setCursor(link ? Qt::PointingHandCursor : Qt::IBeamCursor);
            }
        }
    }
    return QMainWindow::eventFilter(watched, event);
}

void MainWindow::saveOutputAs() {
    if (console_view_ == nullptr) {
        return;
    }
    const QString text = console_view_->toPlainText();
    if (text.trimmed().isEmpty()) {
        setExecutionState(tr("ready"), tr("There is no output to save"));
        return;
    }
    const QString suggested =
        currentEditor() != nullptr
            ? QFileInfo{nameOf(currentEditor())}.completeBaseName() + QStringLiteral(".txt")
            : QStringLiteral("output.txt");
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Save Output"), suggested, tr("Text files (*.txt);;All files (*)"));
    if (path.isEmpty()) {
        return;
    }
    QFile file{path};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        reportError(tr("\u274C Could not write %1").arg(path));
        return;
    }
    file.write(text.toUtf8());
    if (!text.endsWith(QLatin1Char('\n'))) {
        file.write("\n");
    }
    file.close();
    setExecutionState(tr("ready"), tr("Output saved to %1").arg(path));
}

void MainWindow::reportError(const QString& message) {
    if (message.isEmpty()) {
        return;
    }
    if (errors_view_ != nullptr) {
        QTextCursor cursor = errors_view_->textCursor();
        cursor.movePosition(QTextCursor::End);
        cursor.insertText(message.endsWith(QLatin1Char('\n')) ? message
                                                             : message + QLatin1Char('\n'));
        errors_view_->setTextCursor(cursor);
        // Raised, not merely written. An error the user has to go looking for is
        // how a program that failed looks exactly like a program that drew nothing
        // -- which was a real report about this IDE, and this is the fix for the
        // half of it that is presentation rather than function.
        if (output_sub_tabs_ != nullptr) {
            output_sub_tabs_->setCurrentWidget(errors_view_);
        }
    }
    setExecutionState(tr("error"), message.simplified());
}

void MainWindow::clearOutput() {
    console_log_.clear();
    if (console_view_ != nullptr) {
        console_view_->clear();
    }
    if (errors_view_ != nullptr) {
        errors_view_->clear();
    }
    setExecutionState(tr("ready"), tr("Output cleared"));
}

void MainWindow::clearCanvas() {
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->reset();
    }
    clearTurtleLog();
    setExecutionState(tr("ready"), tr("Canvas cleared"));
}

void MainWindow::applyCanvasCommand(const timewarp::canvas::CanvasCommandView& view) {
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->apply(view);
    }
    if (turtle_log_ == nullptr) {
        return;
    }
    turtle_log_->appendCommand(view);
    if (turtle_inspector_ != nullptr) {
        turtle_inspector_->setCommandCount(turtle_log_->commandCount());
    }
}

void MainWindow::refreshTurtleInspector() {
    if (turtle_inspector_ == nullptr || turtle_canvas_ == nullptr) {
        return;
    }
    turtle_inspector_->showPose(turtle_canvas_->turtlePosition(),
                                turtle_canvas_->turtleHeading(),
                                turtle_canvas_->penIsDown(), turtle_canvas_->penColor(),
                                turtle_canvas_->penWidth());
}

void MainWindow::clearTurtleLog() {
    if (turtle_log_ != nullptr) {
        turtle_log_->clearLog();
    }
    if (turtle_inspector_ != nullptr) {
        turtle_inspector_->clear();
    }
}

bool MainWindow::handleBasicDirectCommand(const QString& command, CodeEditor* editor) {
    if (editor == nullptr) {
        return false;
    }
    const QString trimmed = command.trimmed();
    if (const std::optional<BasicProgramLine> line = parseBasicProgramLine(trimmed);
        line.has_value()) {
        appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
        const QString before = editor->toPlainText();
        QString after;
        if (line->body.trimmed().isEmpty()) {
            after = withoutBasicProgramLine(before, line->number);
            appendOutput(after == before
                             ? tr("\u2139\uFE0F No line %1 to delete\n").arg(line->number)
                             : tr("\u2139\uFE0F Deleted line %1\n").arg(line->number));
        } else {
            after = withBasicProgramLine(before, line->number,
                                         withPrintShorthand(line->body));
            appendOutput(tr("\u2139\uFE0F Line %1 stored in the program\n")
                             .arg(line->number));
        }
        if (after != before) {
            QTextCursor edit{editor->document()};
            edit.beginEditBlock();
            edit.select(QTextCursor::Document);
            edit.insertText(after);
            edit.endEditBlock();
            editor->document()->setModified(true);
            refreshDocumentState();
        }
        setExecutionState(tr("ready"), tr("Line %1 stored").arg(line->number));
        return true;
    }

    if (trimmed.compare(QStringLiteral("LIST"), Qt::CaseInsensitive) == 0) {
        appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
        const QString program = editor->toPlainText();
        if (program.trimmed().isEmpty()) {
            appendOutput(tr("\u2139\uFE0F The program is empty.\n"));
        } else {
            appendOutput(program.endsWith(QLatin1Char('\n')) ? program : program + QLatin1Char('\n'));
        }
        setExecutionState(tr("ready"), tr("Program listed"));
        return true;
    }

    if (trimmed.compare(QStringLiteral("RUN"), Qt::CaseInsensitive) == 0) {
        appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
        if (editor->toPlainText().trimmed().isEmpty()) {
            appendOutput(tr("\u2139\uFE0F The program is empty.\n"));
            setExecutionState(tr("ready"), tr("Program is empty"));
            return true;
        }
        runCurrentDocument();
        return true;
    }

    // NEW clears the program as well as the REPL bindings. The bindings are reset
    // by ReplSession when the command reaches it, so only the document is handled
    // here.
    if (trimmed.compare(QStringLiteral("NEW"), Qt::CaseInsensitive) == 0) {
        stopEngines();
        QTextCursor edit{editor->document()};
        edit.beginEditBlock();
        edit.select(QTextCursor::Document);
        edit.removeSelectedText();
        edit.endEditBlock();
        editor->document()->setModified(true);
        refreshDocumentState();
    }
    return false;
}

void MainWindow::submitReplCommand(const QString& command) {
    const QString trimmed = command.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }
    if (immediate_mode_ == nullptr || canvas_queue_ == nullptr ||
        canvas_bridge_ == nullptr) {
        return;
    }

    CodeEditor* editor = currentEditor();
    const Language ui_language = editor != nullptr ? languageOf(editor) : defaultLanguage();
    const std::optional<timewarp::lang::Language> native = nativeLanguage(ui_language);
    if (!native.has_value()) {
        setExecutionState(tr("ready"), tr("This language has no REPL."));
        return;
    }

    if (ui_language == Language::Basic && handleBasicDirectCommand(trimmed, editor)) {
        return;
    }

    if (repl_runner_ == nullptr || repl_language_ != *native) {
        if (repl_runner_ != nullptr) {
            repl_runner_->requestStop();
            repl_runner_->wait();
            delete repl_runner_;
            repl_runner_ = nullptr;
        }
        repl_runner_ = new ReplRunner{*native, *canvas_queue_, this};
        repl_language_ = *native;
        repl_runner_->setWakeup([this] { canvas_bridge_->wake(); });
        if (const QColor surface = turtle_canvas_ != nullptr ? turtle_canvas_->surfaceColor()
                                                             : QColor{};
            surface.isValid()) {
            repl_runner_->setSurfaceColor(
                timewarp::lang::Rgb{static_cast<std::uint8_t>(surface.red()),
                                    static_cast<std::uint8_t>(surface.green()),
                                    static_cast<std::uint8_t>(surface.blue())});
            repl_runner_->setInkColor(default_ink_for(surface));
        }
        if (sql_database_ != nullptr) {
            const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
            repl_runner_->setSqlCallback([database](std::string_view sql) {
                return database->executeForOutput(sql);
            });
        }
        connect(repl_runner_, &ReplRunner::outputAvailable, this,
                [this](const QString& text) { appendOutput(text); });
        connect(repl_runner_, &ReplRunner::commandFinished, this,
                [this](bool ok, const QString& error) {
                    if (canvas_bridge_ != nullptr) {
                        canvas_bridge_->drainNow();
                    }
                    if (!error.isEmpty()) {
                        reportError(error);
                    }
                    setExecutionState(ok ? tr("ready") : tr("error"),
                                      ok ? tr("REPL ready") : tr("REPL error"));
                });
    }

    appendOutput(tr("REPL \u203A %1\n").arg(trimmed));
    immediate_mode_->setCommand(QString{});
    immediate_mode_->focusInput();
    setExecutionState(tr("running"), tr("REPL: running"));
    repl_runner_->submit(trimmed);
}

void MainWindow::runCurrentDocument() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    const QString source = editor->toPlainText();
    running_selection_ = false;
    noteRun(source);
    runSource(editor, source);
}

QString MainWindow::runFileBase(const CodeEditor* editor) const {
    // A project is a folder of programs that belong together, so a program inside
    // one reads and writes beside the project's other programs -- which is also the
    // only reading of `OPEN "data.txt"` that survives the folder being copied or
    // handed to a learner.
    if (project_.isValid()) {
        return project_.root();
    }
    const QString path = editor != nullptr ? pathOf(editor) : QString{};
    if (!path.isEmpty()) {
        return QFileInfo{path}.absolutePath();
    }
    // Unsaved and unprojected: the process's working directory, which is what a
    // terminal run has always used.
    return {};
}

void MainWindow::runProject() {
    if (!project_.isValid()) {
        setExecutionState(tr("ready"), tr("Open a project first; there is nothing to run."));
        return;
    }
    if (!project_.hasEntry()) {
        setExecutionState(
            tr("ready"),
            tr("This project has no entry point. Right-click a program and choose "
               "\u201cSet as Entry Point\u201d."));
        return;
    }
    const QString entry = project_.entry();
    if (!QFileInfo::exists(entry)) {
        setExecutionState(tr("ready"),
                          tr("The entry point is missing: %1")
                              .arg(QDir::toNativeSeparators(entry)));
        return;
    }
    // Raising the entry is what makes "Run Project" legible: the program the user
    // is watching run is the program in front of them.
    if (!openFile(entry)) {
        return;
    }
    CodeEditor* editor = currentEditor();
    if (editor == nullptr || pathOf(editor) != QFileInfo{entry}.absoluteFilePath()) {
        // Opened, but not the tab in front: a run that executes a document other
        // than the one on screen is the confusion this check exists to prevent.
        setExecutionState(tr("ready"),
                          tr("Could not bring the entry point to the front: %1")
                              .arg(QDir::toNativeSeparators(entry)));
        return;
    }
    runCurrentDocument();
}

void MainWindow::setProjectEntry(const QString& path) {
    if (!project_.isValid() || !project_.setEntry(path)) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
        setExecutionState(tr("ready"),
                          tr("Entry point: %1").arg(project_.displayPath(project_.entry())));
    }
}

void MainWindow::clearProjectEntry() {
    if (!project_.clearEntry()) {
        return;
    }
    if (saveProject()) {
        refreshProjectPanel();
        setExecutionState(tr("ready"), tr("Entry point cleared"));
    }
}

void MainWindow::runSelection() {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    // The selection as the engine sees it. Qt stores paragraph breaks as U+2029
    // in `selectedText()`, and a program that receives those does not parse.
    QString selection = editor->textCursor().selectedText();
    selection.replace(QChar::ParagraphSeparator, QLatin1Char('\n'));
    if (selection.trimmed().isEmpty()) {
        // Refused rather than falling back to the document: running the whole
        // program when the user asked for three lines is a worse surprise than a
        // message saying nothing is selected.
        setExecutionState(tr("ready"), tr("Select the lines to run first."));
        return;
    }
    running_selection_ = true;
    noteRun(selection);
    runSource(editor, selection);
}

void MainWindow::resetProgram() {
    // Stop first: clearing a canvas something is still drawing into is a race the
    // user loses.
    stopEngines();
    canvas_bridge_->drainNow();
    if (turtle_canvas_ != nullptr) {
        turtle_canvas_->reset();
    }
    clearTurtleLog();
    clearOutput();
    clearRunState();
    setExecutionState(tr("ready"), tr("Program reset"));
}

void MainWindow::runSource(CodeEditor* editor, const QString& source) {
    if (editor == nullptr) {
        return;
    }

    // Whichever engine ran last is stopped first, whichever engine is about to
    // run. Two programs drawing into one canvas is not something a user can
    // interpret, and a Scheme run left alive while a Logo one starts would be
    // exactly that.
    stopEngines();

    // A panel showing the previous run's numbers while this one is still starting
    // is a lie that is easy to read as this run's. Emptying them is the honest
    // state, and the HUD has a row that says so.
    clearRunState();
    startRunClock();

    // This run's audio replaces the last one's: File > Export Audio writes what the
    // program being run has sounded, not every note since the window opened.
    if (sound_ != nullptr) {
        sound_->beginRecording();
    }

    // A ported language runs in this process. The bridge is only for the ones
    // that have not been ported, and `executor_for` is the question that decides
    // it -- so adding an executor is all it takes for the IDE to use it.
    if (languageOf(editor) != Language::Scheme) {
        const std::optional<timewarp::lang::Language> native = nativeLanguage(languageOf(editor));
        if (native.has_value() && timewarp::lang::executor_for(*native) != nullptr) {
            runNatively(editor, *native, source);
            return;
        }
        // Unreachable: every language has a native executor, and `language_tests`
        // asserts it. Said rather than silently doing nothing, because "the Run
        // button did nothing" is the report this branch would otherwise produce.
        reportError(tr("This language has no interpreter in this build."));
        return;
    }

    // A clean surface. The previous run's tail is drained *before* the canvas is
    // cleared, so a command still in the queue cannot land on the new picture.
    canvas_bridge_->drainNow();
    turtle_canvas_->reset();
    clearTurtleLog();
    if (console_view_ != nullptr) {
        console_view_->clear();
    }
    if (errors_view_ != nullptr) {
        errors_view_->clear();
    }

    auto* runner = new timewarp::debugger::DebuggerRunner{this};
    runner_ = runner;

    // One host per run: it holds the counters for that run, and the queue it
    // writes into outlives it.
    turtle_host_ = std::make_unique<timewarp::canvas::QueuedTurtleHost>(*canvas_queue_);
    turtle_host_->set_wakeup([this] { canvas_bridge_->wake(); });
    if (sql_database_ != nullptr && sql_database_->isOpen()) {
        const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
        turtle_host_->set_sql_runner([database](std::string_view sql) {
            return database->executeForOutput(sql);
        });
    }
    // No file base here: Scheme has no file words, and a base this runner never
    // reads would be a setting that quietly does nothing. `NativeRunner` takes one.
    runner->set_host(turtle_host_.get());

    connect(runner, &timewarp::debugger::DebuggerRunner::output_available, this,
            [this, runner](const QString& text) {
                // A superseded run must not write into the new run's console. The
                // old runner is deleted before the new one starts, which drops its
                // queued deliveries -- this makes the intent explicit rather than
                // resting on that, because losing it would be silent.
                if (runner_ != runner) {
                    return;
                }
                appendOutput(text);
            });
    connect(runner, &timewarp::debugger::DebuggerRunner::run_finished, this,
            [this, runner](bool completed, const QString& error) {
                if (runner_ != runner) {
                    return;
                }
                finishRun(error, completed);
            });

    // The panel's buttons were already hooked up to "whatever runner exists";
    // these two are the other direction -- the runner telling the panel it has
    // stopped somewhere.
    connect(runner, &timewarp::debugger::DebuggerRunner::paused, this,
            [this, runner](int line, int, const QString& procedure) {
                if (runner_ != runner) {
                    return;
                }
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(true, line, procedure);
                    // Nor is going back offered here: the Scheme machine's state is
                    // an arena and a continuation chain, which is not a value a
                    // snapshot can copy.
                    debug_panel_->setRewindAvailable(
                        false, tr("The Scheme machine cannot be walked back"));
                }
                showRunnerSnapshot();
            });
    connect(runner, &timewarp::debugger::DebuggerRunner::resumed, this,
            [this, runner] {
                if (runner_ != runner) {
                    return;
                }
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(false, 0, QString{});
                }
            });

    // The breakpoints already in the panel are the ones this run honours, which
    // is why the set is applied here rather than only when it changes.
    if (debug_panel_ != nullptr) {
        // `QList` and `QVector` are one type in Qt 6 and two in Qt 5, so the
        // panel's list is converted rather than passed through. The intermediate
        // is named on purpose: `breakpoints()` builds a new list on every call,
        // so asking for it twice would give one iterator to each of two lists.
        const QList<int> lines = debug_panel_->breakpoints();
        runner->set_breakpoints(QVector<int>{lines.begin(), lines.end()});
        runner->set_data_breakpoints(debug_panel_->dataBreakpoints());
    }

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    runner->load_program(source, nameOf(editor));
    runner->start();
}

void MainWindow::stopEngines() {
    // Wait in slices, draining the canvas in between.
    //
    // A runner that is blocked in `CanvasCommandQueue::push` -- the ring is full
    // because nothing has emptied it -- cannot look at the stop flag until it can
    // write again, and the only thing that empties the ring is this thread, which
    // would be parked in `wait()`. That is a deadlock, and whether it is reached
    // is a matter of which thread gets there first: a program that draws fast
    // enough to fill the ring before Stop is pressed deadlocks, and one that does
    // not, stops tidily.
    const auto wait_draining = [this](QThread* thread) {
        while (thread != nullptr && thread->isRunning()) {
            if (thread->wait(50)) {
                return;
            }
            if (canvas_bridge_ != nullptr) {
                canvas_bridge_->drainNow();
            }
        }
    };

    if (repl_runner_ != nullptr) {
        // A REPL command is a small run, and running a document while one is in
        // flight would put two engines on one canvas.
        repl_runner_->requestStop();
        wait_draining(repl_runner_);
        delete repl_runner_;
        repl_runner_ = nullptr;
    }
    if (native_runner_ != nullptr) {
        // Cooperative, then wait: an executor checks the flag between statements.
        native_runner_->request_stop();
        wait_draining(native_runner_.data());
        delete native_runner_.data();
        native_runner_ = nullptr;
    }
    if (runner_ != nullptr) {
        // `wait` rather than a delay: the stop flag is checked per form, so this
        // returns as soon as the interpreter reaches the next one.
        runner_->request_stop();
        wait_draining(runner_.data());
        delete runner_.data();
    }
}

void MainWindow::stopRunningDocument() {
    const bool scheme_running = runner_ != nullptr && runner_->isRunning();
    const bool native_running = native_runner_ != nullptr && native_runner_->isRunning();
    if (!scheme_running && !native_running) {
        setExecutionState(tr("ready"), tr("Nothing is running"));
        return;
    }
    setExecutionState(tr("stopping"), tr("Stopping…"));
    stopEngines();
    setExecutionState(tr("stopped"), tr("Stopped"));
}

void MainWindow::runNatively(CodeEditor* editor, timewarp::lang::Language language,
                             const QString& source) {
    // The same clean surface the other engines get, in the same order.
    canvas_bridge_->drainNow();
    turtle_canvas_->reset();
    clearTurtleLog();
    if (console_view_ != nullptr) {
        console_view_->clear();
    }
    if (errors_view_ != nullptr) {
        errors_view_->clear();
    }

    auto* runner = new NativeRunner{language, *canvas_queue_, this};
    native_runner_ = runner;
    // The run's own directory: a project's root, or the document's folder. See
    // `runFileBase` for why the process's working directory is the last resort.
    runner->set_file_base(runFileBase(editor));
    // The queue has no consumer of its own: a run that fills it waits until the
    // GUI drains it, so every command has to say "look at the queue now".
    runner->set_wakeup([this] { canvas_bridge_->wake(); });
    // Sound: created once, on the first run that might make one. A machine with
    // no player still runs the program -- the language's acknowledgement is the
    // reference's, and `audio_available()` is what tells a program the truth.
    if (sound_ == nullptr) {
        sound_ = new SoundPlayer{this};
    }
    runner->set_sound_player(sound_);
    if (sql_database_ != nullptr && sql_database_->isOpen()) {
        const std::shared_ptr<timewarp::sql::Database> database = sql_database_;
        runner->setSqlCallback([database](std::string_view sql) {
            return database->executeForOutput(sql);
        });
    }
    // The theme's surface, so a program that draws before naming a colour draws
    // on the paper the user's theme chose rather than on the reference's.
    if (const QColor surface =
            turtle_canvas_ != nullptr ? turtle_canvas_->surfaceColor() : QColor{};
        surface.isValid()) {
        runner->set_surface_color(
            timewarp::lang::Rgb{static_cast<std::uint8_t>(surface.red()),
                                static_cast<std::uint8_t>(surface.green()),
                                static_cast<std::uint8_t>(surface.blue())});
        runner->set_ink_color(default_ink_for(surface));
    }
    // Profiling is a per-run choice: the flag travels with this runner, and the
    // report is built from this run's own numbers.
    const bool profiling = profile_action_ != nullptr && profile_action_->isChecked();
    runner->set_profiling(profiling);
    // Likewise the history depth: the panel holds the choice, and this is the run
    // that keeps it.
    if (debug_panel_ != nullptr) {
        runner->set_history(static_cast<std::size_t>(debug_panel_->historyDepth()));
    }
    const QString profile_title = nameOf(editor);
    QStringList profile_source;
    if (profiling) {
        // The engine's statement indices are source-line indices (the loader
        // keeps blanks), so the report can quote the line the index names.
        profile_source = editor->toPlainText().split(QLatin1Char('\n'));
    }
    runner->load_program(source);
    // The editor's breakpoints are zero-based lines. The line-loop languages use
    // them as statement indices, which is why `parse_lines` keeps every line,
    // blanks included; the whole-program executors match them against the source
    // line they are about to run.
    std::set<std::size_t> breakpoints;
    for (const int line : editor->breakpoints()) {
        breakpoints.insert(static_cast<std::size_t>(line));
    }
    runner->set_breakpoints(breakpoints);
    if (debug_panel_ != nullptr) {
        // The panel's set applies to every run, whichever engine drives it. The
        // editor's gutter holds line breakpoints; a data breakpoint has no gutter.
        runner->setDataBreakpoints(debug_panel_->dataBreakpoints());
    }

    connect(runner, &NativeRunner::outputAvailable, this,
            [this, runner](const QString& text) {
                if (native_runner_ != runner) {
                    return;
                }
                // The runner reports lines without terminators, like the engine's
                // own buffer; the console needs the newline the logger added.
                appendOutput(text + QLatin1Char('\n'));
            });
    connect(runner, &NativeRunner::inputRequested, this,
            [this, runner](const QString& prompt) {
                if (native_runner_ != runner) {
                    return;
                }
                // A program that asked a question and stopped is waiting for an
                // answer the IDE cannot invent.
                bool accepted = false;
                const QString answer = QInputDialog::getText(
                    this, tr("The program is waiting for input"), prompt, QLineEdit::Normal,
                    QString{}, &accepted);
                runner->answerInput(accepted ? answer : QString{});
            });
    connect(runner, &NativeRunner::run_finished, this,
            [this, runner, language, profiling, profile_title,
             profile_source](bool completed, const QString& error) {
                if (native_runner_ != runner) {
                    return;
                }
                stopRunClock();
                canvas_bridge_->drainNow();
                turtle_canvas_->fitToContent();

                QList<timewarp::ui::VariableRow> rows;
                for (const NativeRunner::VariableRow& row : runner->variables()) {
                    rows.append({row.name, row.type, row.value});
                }
                last_variables_ = rows;
                if (variables_panel_ != nullptr) {
                    variables_panel_->setRows(rows);
                }
                refreshWatches();
                // A run stopped while paused ends here rather than through
                // `resumed`, so the panel is told the pause is over.
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(false, 0, QString{});
                }
                if (telemetry_panel_ != nullptr) {
                    const std::string_view name = timewarp::lang::display_name(language);
                    QList<timewarp::ui::TelemetryRow> hud;
                    hud.append({tr("Language"),
                                QString::fromLatin1(name.data(), static_cast<int>(name.size()))});
                    hud.append({tr("Outcome"), completed ? tr("finished") : tr("stopped")});
                    hud.append({tr("Steps"), QString::number(runner->steps())});
                    hud.append({tr("Engine time"),
                                tr("%1 ms").arg(runner->elapsedMs(), 0, 'f', 1)});
                    hud.append({tr("Drawing commands"),
                                QString::number(runner->drawingCommands())});
                    telemetry_panel_->setRows(hud + ideTelemetryRows());
                }
                if (!error.isEmpty()) {
                    reportError(error);
                }
                setExecutionState(completed ? tr("ready") : tr("stopped"),
                                  completed ? tr("Finished") : tr("Stopped"));
                announceFinishedRun(completed, error);

                if (profiling) {
                    const auto profile_rows = runner->profile();
                    if (profile_rows.empty()) {
                        // A whole-program language has no statement loop to time;
                        // saying so is better than a report with no rows.
                        const std::string_view name = timewarp::lang::display_name(language);
                        appendOutput(tr("No per-line profile: %1 runs as a whole program.\n")
                                         .arg(QString::fromLatin1(
                                             name.data(), static_cast<int>(name.size()))));
                    } else {
                        QList<ProfileEntry> entries;
                        entries.reserve(static_cast<int>(profile_rows.size()));
                        for (const timewarp::lang::LineProfile& row : profile_rows) {
                            const auto index = static_cast<int>(row.index);
                            entries.append(
                                {row.line,
                                 index >= 0 && index < profile_source.size()
                                     ? profile_source.at(index)
                                     : QString{},
                                 row.hits, row.elapsed_ms});
                        }
                        ProfileReportDialog dialog{profile_title, entries, runner->elapsedMs(),
                                                   this};
                        dialog.exec();
                    }
                }
            });

    connect(runner, &NativeRunner::paused, this, [this, runner](int line) {
        if (native_runner_ != runner) {
            return;
        }
        // The console says where the run is, because the canvas cannot: a learner
        // needs to know which statement has not happened yet, not that something
        // happened somewhere. Line 0 means the program has no source lines (a
        // binary CHIP-8 ROM), so the instruction is named rather than a line.
        setExecutionState(tr("paused"),
                          line > 0 ? tr("Paused before line %1").arg(line)
                                   : tr("Paused before the next instruction"));
        // The Debug tab and the Variables tab follow the pause, so a stopped
        // program is inspectable wherever the user looks.
        if (debug_panel_ != nullptr) {
            debug_panel_->setPaused(true, line, QString{});
            // Going back is offered exactly when it can happen: a paused run whose
            // session was asked to keep history. Scheme is not offered it at all,
            // because that machine's state is not a value to copy.
            const bool rewindable = runner->canRewind();
            debug_panel_->setRewindAvailable(
                rewindable,
                rewindable
                    ? QString{}
                    : tr("History is off, or this language cannot be walked back yet"));
        }
        QList<timewarp::ui::VariableRow> rows;
        for (const NativeRunner::VariableRow& row : runner->variables()) {
            rows.append({row.name, row.type, row.value});
        }
        last_variables_ = rows;
        if (variables_panel_ != nullptr) {
            variables_panel_->setRows(rows);
        }
        if (debug_panel_ != nullptr) {
            // The panel's own variables table is the frame's bindings for
            // Scheme; for a native engine the run's bindings are the best
            // answer, and it is better than an empty table while stopped.
            debug_panel_->setVariables(rows);
        }
        showNativeStack(*runner);
        refreshWatches();
        static_cast<void>(showPanel(QStringLiteral("debug")));
    });
    connect(runner, &NativeRunner::dataBreakpointHit, this,
            [this, runner](const QString& name, int line) {
                if (native_runner_ != runner) {
                    return;
                }
                setExecutionState(tr("paused"), tr("Paused: %1 changed").arg(name));
                if (debug_panel_ != nullptr) {
                    debug_panel_->setPaused(true, line, QString{});
                    debug_panel_->setPauseReason(tr("%1 changed").arg(name));
                }
                QList<timewarp::ui::VariableRow> rows;
                for (const NativeRunner::VariableRow& row : runner->variables()) {
                    rows.append({row.name, row.type, row.value});
                }
                last_variables_ = rows;
                if (variables_panel_ != nullptr) {
                    variables_panel_->setRows(rows);
                }
                if (debug_panel_ != nullptr) {
                    debug_panel_->setVariables(rows);
                }
                showNativeStack(*runner);
                refreshWatches();
                static_cast<void>(showPanel(QStringLiteral("debug")));
            });
    connect(runner, &NativeRunner::resumed, this, [this, runner] {
        if (native_runner_ != runner) {
            return;
        }
        if (debug_panel_ != nullptr) {
            debug_panel_->setPaused(false, 0, QString{});
        }
        setExecutionState(tr("running"), tr("Running…"));
    });

    setExecutionState(tr("running"), tr("Running %1…").arg(nameOf(editor)));
    runner->start();
}

}  // namespace timewarp::ui
