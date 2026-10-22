// SPDX-License-Identifier: MIT

#include "main_window_internal.hpp"

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QClipboard>
#include <QComboBox>
#include <QDateTime>
#include <QDesktopServices>
#include <QDir>
#include <QDirIterator>
#include <QDockWidget>
#include <QFile>
#include <QFileDialog>
#include <QFileInfo>
#include <QFont>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeySequence>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QPageLayout>
#include <QPlainTextEdit>
#ifdef TIME_WARP_HAVE_PRINT
#include <QPrintDialog>
#include <QPrinter>
#endif
#include <QPushButton>
#include <QSaveFile>
#include <QSet>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSlider>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTimer>
#include <QTextDocument>
#include <QToolBar>
#include <QToolButton>
#include <QUrl>
#include <QVBoxLayout>

#include <algorithm>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/ui/app.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/benchmark.hpp"
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
    // `moveCursor` + `insertPlainText` rather than `appendPlainText`, which adds
    // its own paragraph break: the interpreter decides where its lines end, and
    // a view that second-guesses it turns "a" and "b" printed separately into
    // two lines in one build and one line in the other.
    QTextCursor cursor = console_view_->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(text);
    console_view_->setTextCursor(cursor);
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
    // not, stops tidily. Linux never reached it; Windows and macOS did.
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

namespace {

/// The right column's fixed pages: the key `showPanel` accepts and the index of
/// the tab `buildRightColumn` adds. One table, because the failure it prevents is
/// a page that exists and cannot be raised -- from the palette, a function key or
/// a menu -- and that looks exactly like a working page until somebody tries.
struct PanelEntry {
    const char* key;
    int index;
};

constexpr PanelEntry kPanelTable[] = {
    {"output", 0},   {"variables", 1}, {"telemetry", 2},
    {"debug", 3},    {"lessons", 4},   {"classroom", 5},
    {"sql", 6}};

}  // namespace

QStringList MainWindow::panelKeys() {
    QStringList keys;
    for (const PanelEntry& entry : kPanelTable) {
        keys << QString::fromLatin1(entry.key);
    }
    return keys;
}

bool MainWindow::showPanel(const QString& name) {
    const QString wanted = name.trimmed().toLower();
    for (const PanelEntry& entry : kPanelTable) {
        if (wanted == QLatin1String(entry.key)) {
            if (right_tabs_ != nullptr) {
                right_tabs_->setCurrentIndex(entry.index);
            }
            // A one-time hint the first time the lessons page is raised. It
            // refuses while the guided tour is running, which is the one case
            // where two overlays would compete for the same panel.
            if (wanted == QLatin1String("lessons") && tour_ != nullptr) {
                tour_->hint(QStringLiteral("lessons"));
            }
            return true;
        }
    }
    return false;
}

void MainWindow::openLessonCode(const QString& languageKey, const QString& source,
                                const QString& name) {
    if (source.isEmpty()) {
        return;
    }
    CodeEditor* editor = newFile();
    editor->setPlainText(source);
    // The language is recorded as an *override* rather than guessed from the name:
    // a lesson document is called something like `basic_first_steps-1`, which has
    // no extension for the language table to read.
    editor->setProperty(kLanguageOverrideProperty, languageKey);
    editor->setProperty(kUntitledLabelProperty,
                        name.isEmpty() ? tr("lesson") : name);
    updateTabLabel(editor);
}

bool MainWindow::ensureLessons() {
    if (lessons_ != nullptr) {
        return true;
    }
    auto store = std::make_unique<timewarp::lessons::ProgressStore>();
    QString error;
    if (!store->load_default(&error)) {
        if (lesson_panel_ != nullptr) {
            lesson_panel_->setFailure(tr("the lessons"), error);
        }
        return false;
    }
    lessons_ = std::move(store);
    return true;
}

void MainWindow::requestLessonCurriculum() {
    if (!ensureLessons()) {
        return;
    }
    QList<timewarp::ui::LessonSummary> summaries;
    for (const timewarp::lessons::Lesson& lesson : lessons_->lessons()) {
        timewarp::ui::LessonSummary summary;
        summary.id = QString::fromStdString(lesson.id);
        summary.title = QString::fromStdString(lesson.title);
        summary.language = QString::fromStdString(std::string{
            timewarp::lang::display_name(lesson.language)});
        summary.languageKey =
            QString::fromStdString(std::string{timewarp::lang::key(lesson.language)});
        summary.difficulty = QString::fromStdString(lesson.difficulty);
        summary.description = QString::fromStdString(lesson.description);
        for (const std::string& objective : lesson.objectives) {
            summary.objectives << QString::fromStdString(objective);
        }
        for (const std::string& prerequisite : lesson.prerequisites) {
            summary.prerequisites << QString::fromStdString(prerequisite);
        }
        for (const timewarp::lessons::Checkpoint& checkpoint : lesson.checkpoints) {
            timewarp::ui::LessonCheckpoint row;
            row.title = QString::fromStdString(checkpoint.title);
            row.description = QString::fromStdString(checkpoint.description);
            row.starter = QString::fromStdString(checkpoint.starter_code);
            row.expectation = checkpoint.expect_error.has_value()
                                  ? QStringLiteral("error")
                                  : (checkpoint.expect_turtle.has_value()
                                         ? QStringLiteral("turtle")
                                         : QStringLiteral("lines"));
            for (const std::string& hint : checkpoint.hints) {
                row.hints << QString::fromStdString(hint);
            }
            summary.checkpoints.append(row);
        }
        summaries.append(summary);
    }
    if (lesson_panel_ != nullptr) {
        lesson_panel_->setLessons(summaries);
        if (presentation_font_ > 0) {
            lesson_panel_->setPresentationFont(presentation_font_);
        }
    }
    requestLessonProgress();
}

void MainWindow::requestLessonProgress() {
    if (!ensureLessons() || lesson_panel_ == nullptr) {
        return;
    }
    QList<timewarp::ui::LessonProgress> rows;
    for (const timewarp::lessons::Lesson& lesson : lessons_->lessons()) {
        timewarp::ui::LessonProgress row;
        row.id = QString::fromStdString(lesson.id);
        row.title = QString::fromStdString(lesson.title);
        row.language = QString::fromStdString(std::string{
            timewarp::lang::display_name(lesson.language)});
        row.difficulty = QString::fromStdString(lesson.difficulty);
        switch (lessons_->status(lesson.id)) {
            case timewarp::lessons::LessonStatus::NotStarted:
                row.status = QStringLiteral("not_started");
                break;
            case timewarp::lessons::LessonStatus::InProgress:
                row.status = QStringLiteral("in_progress");
                break;
            case timewarp::lessons::LessonStatus::Completed:
                row.status = QStringLiteral("completed");
                break;
        }
        row.checkpoint = lessons_->current_index(lesson.id);
        row.checkpoints = timewarp::core::intLength(lesson.checkpoints.size());
        row.hintsUsed = lessons_->hints_used(lesson.id);
        rows.append(row);
    }
    const auto [finished, total] = lessons_->completed_count();
    lesson_panel_->setProgress(rows, finished, total);
}

void MainWindow::requestLessonCheck(const QString& lesson, int checkpoint) {
    CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        if (lesson_panel_ != nullptr) {
            lesson_panel_->setFailure(tr("the check"),
                                      tr("open a document with your program in it first"));
        }
        return;
    }
    if (!ensureLessons()) {
        return;
    }
    // In process, through the same library `time_warp_teach` runs. The check is
    // the learner's program against the checkpoint; a run that takes a while is
    // the program's own budget, not a request round trip.
    const timewarp::lessons::VerifyResult result = lessons_->check(
        lesson.toStdString(), checkpoint, editor->toPlainText().toStdString());
    const auto [finished, total] = lessons_->completed_count();
    const bool completed =
        lessons_->status(lesson.toStdString()) == timewarp::lessons::LessonStatus::Completed;
    if (lesson_panel_ != nullptr) {
        lesson_panel_->setChecked(lesson, checkpoint, result.passed,
                                  QString::fromStdString(result.message), completed,
                                  finished, total);
    }
}

void MainWindow::requestLessonHint(const QString& lesson, int checkpoint) {
    if (!ensureLessons() || lesson_panel_ == nullptr) {
        return;
    }
    const timewarp::lessons::Lesson* found =
        timewarp::lessons::find_lesson(lessons_->lessons(), lesson.toStdString());
    if (found == nullptr) {
        return;
    }
    // The panel asks for the next hint it has not shown: the count in the
    // progress file is what makes "hint 2 of 3" survive closing the window.
    const int already = lessons_->hints_used(found->id);
    const std::string text = lessons_->hint(found->id, checkpoint, already);
    const int total = timewarp::core::intLength(
        found->checkpoints[static_cast<std::size_t>(checkpoint)].hints.size());
    lesson_panel_->setHint(QString::fromStdString(text),
                           std::min(already + 1, std::max(1, total)), total);
}

void MainWindow::requestLessonSolution(const QString& lesson, int checkpoint) {
    if (!ensureLessons() || lesson_panel_ == nullptr) {
        return;
    }
    const timewarp::lessons::Lesson* found =
        timewarp::lessons::find_lesson(lessons_->lessons(), lesson.toStdString());
    if (found == nullptr || checkpoint < 0 ||
        checkpoint >= timewarp::core::intLength(found->checkpoints.size())) {
        return;
    }
    lesson_panel_->setSolution(QString::fromStdString(
        found->checkpoints[static_cast<std::size_t>(checkpoint)].solution));
}

void MainWindow::requestLessonReset(const QString& lesson) {
    if (!ensureLessons()) {
        return;
    }
    lessons_->reset(lesson.toStdString());
    // The panel shows where the learner is, so the only thing to do after a
    // reset is tell it again.
    requestLessonProgress();
}

bool MainWindow::ensureClassroom() {
    if (classroom_ != nullptr) {
        return true;
    }
    auto room = std::make_unique<timewarp::lessons::Classroom>();
    QString error;
    if (!room->load(&error)) {
        if (classroom_panel_ != nullptr) {
            classroom_panel_->setFailure(QStringLiteral("classroom-roster"), error);
        }
        return false;
    }
    classroom_ = std::move(room);
    return true;
}

void MainWindow::requestClassroom() {
    if (classroom_panel_ == nullptr) {
        return;
    }
    if (!ensureClassroom()) {
        return;
    }
    QList<timewarp::ui::ClassroomStudent> students;
    for (const timewarp::lessons::Student& student : classroom_->students()) {
        timewarp::ui::ClassroomStudent row;
        row.id = student.id;
        row.name = student.name;
        // Reported rather than derived: a learner whose work lives on a shared
        // drive is a fact only the class knows.
        row.directory = classroom_->studentDirectory(student.id);
        students.append(row);
    }
    classroom_panel_->setRoster(students, classroom_->assignment().lessonIds,
                                classroom_->assignment().note);

    // Marking runs each handed-in file through the same verifier a learner's
    // check uses -- on a worker thread, because that runs the learner's program
    // and a runaway one takes its whole budget. The rows that are already on
    // screen stay there until the new ones arrive: blanking the class while a
    // mark runs would show "nobody" as if that were the truth.
    if (!students.isEmpty()) {
        if (classroom_marker_ == nullptr) {
            classroom_marker_ = new ClassroomMarker{this};
            connect(classroom_marker_, &ClassroomMarker::marked, this,
                    [this](const QList<timewarp::ui::ClassroomMark>& rows) {
                        if (classroom_panel_ != nullptr) {
                            classroom_panel_->setMarks(rows);
                        }
                    });
        }
        if (!classroom_marker_->begin(classroom_->markingJobs({}))) {
            // One is already running, and its answer is newer than this request;
            // the rest of this pass still has work to do below.
        }
    } else {
        classroom_panel_->setMarks({});
    }

    // Presentation mode may be entered before this page is ever shown, and the
    // class's own size is what it has to use.
    if (presentation_font_ <= 0) {
        setPresentationSettings(classroom_->presentation().fontSize,
                                classroom_->presentation().fullscreen);
    }
}

void MainWindow::exportClassroomBundle() {
    // The dialog comes first: a teacher who cancels should not have caused a
    // directory walk.
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Save class bundle"),
        QDir{QDir::homePath()}.filePath(QStringLiteral("classroom.zip")),
        tr("Class bundle (*.zip)"));
    if (path.isEmpty() || !ensureClassroom()) {
        return;
    }
    QString error;
    if (!classroom_->createBundle(path, &error)) {
        reportError(error);
        statusBar()->showMessage(tr("The class could not be bundled: %1").arg(error), 8000);
        return;
    }
    setExecutionState(tr("ready"), tr("Wrote %1").arg(QFileInfo{path}.fileName()));
}

void MainWindow::importClassroomBundle() {
    const QString path = QFileDialog::getOpenFileName(this, tr("Open class bundle"),
                                                      QDir::homePath(),
                                                      tr("Class bundle (*.zip)"));
    if (path.isEmpty() || !ensureClassroom()) {
        return;
    }
    // The one question with two genuinely different answers: setting up a new
    // class wants existing files left alone, and refreshing one wants the archive
    // to win. Overwriting a learner's work is the answer that cannot be undone,
    // so it is the one that has to be asked for.
    const QMessageBox::StandardButton answer = QMessageBox::question(
        this, tr("Unpack class bundle"),
        tr("Replace files that are already in the classroom?\n\n"
           "No leaves them alone, which is what setting up a new class wants."),
        QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::No);
    if (answer == QMessageBox::Cancel) {
        return;
    }
    QString error;
    if (!classroom_->importBundle(path, answer == QMessageBox::Yes, &error)) {
        reportError(error);
        statusBar()->showMessage(tr("The bundle could not be unpacked: %1").arg(error), 8000);
        return;
    }
    setExecutionState(tr("ready"),
                      tr("Imported %1 student(s)").arg(classroom_->students().size()));
    // The class on screen is now a different class.
    requestClassroom();
}


void MainWindow::connectDebugPanel() {
    if (debug_panel_ == nullptr) {
        return;
    }
    // These do not connect *to* a runner: one exists only for the length of a
    // run, and is deleted when the run ends. They look it up when a button is
    // pressed, so a press between runs is a no-op rather than a dangling call.
    //
    // A native run is driven here too. The panel is the same panel whichever
    // engine is paused; only the commands differ, because the native runner has
    // one step unit (a statement, a send, a top-level statement or an opcode)
    // rather than Scheme's in/over/out.
    const auto forward = [this](void (timewarp::debugger::DebuggerRunner::*command)()) {
        return [this, command] {
            if (native_runner_ != nullptr && native_runner_->isRunning()) {
                using timewarp::debugger::DebuggerRunner;
                if (command == &DebuggerRunner::request_continue) {
                    native_runner_->request_continue();
                    return;
                }
                if (command == &DebuggerRunner::request_stop) {
                    stopRunningDocument();
                    return;
                }
                // Pause and the three step variants are all "stop at the next
                // boundary" for an engine without a frame stack.
                native_runner_->request_step();
                return;
            }
            if (runner_ != nullptr) {
                (runner_.data()->*command)();
            }
        };
    };
    connect(debug_panel_, &DebugPanel::continueRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_continue));
    connect(debug_panel_, &DebugPanel::pauseRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_pause));
    connect(debug_panel_, &DebugPanel::stepIntoRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_step_into));
    connect(debug_panel_, &DebugPanel::stepOverRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_step_over));
    connect(debug_panel_, &DebugPanel::stepOutRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_step_out));
    connect(debug_panel_, &DebugPanel::stopRequested, this,
            forward(&timewarp::debugger::DebuggerRunner::request_stop));
    connect(debug_panel_, &DebugPanel::breakpointsChanged, this,
            [this](const QList<int>& lines) {
                if (runner_ != nullptr) {
                    runner_->set_breakpoints(QVector<int>{lines.begin(), lines.end()});
                }
                if (native_runner_ != nullptr) {
                    // The panel holds 1-based lines; the editor's gutter set,
                    // which the native runner is given at run start, is
                    // zero-based.
                    std::set<std::size_t> zero_based;
                    for (const int line : lines) {
                        if (line > 0) {
                            zero_based.insert(static_cast<std::size_t>(line - 1));
                        }
                    }
                    native_runner_->set_breakpoints(zero_based);
                }
            });

    // Watches are a session-wide question, so they come back with the next
    // launch. The values do not: a binding from last night's run would be shown
    // as if it were this run's.
    const QSettings settings;
    debug_panel_->setWatches(settings.value(QLatin1String(kSettingWatches)).toStringList());
    debug_panel_->setDataBreakpoints(
        settings.value(QLatin1String(kSettingDataBreakpoints)).toStringList());
    connect(debug_panel_, &DebugPanel::watchesChanged, this,
            [this](const QStringList& expressions) {
                QSettings{}.setValue(QLatin1String(kSettingWatches), expressions);
                refreshWatches();
            });
    connect(debug_panel_, &DebugPanel::dataBreakpointsChanged, this,
            [this](const QStringList& names) {
                QSettings{}.setValue(QLatin1String(kSettingDataBreakpoints), names);
                if (runner_ != nullptr) {
                    runner_->set_data_breakpoints(names);
                }
                if (native_runner_ != nullptr) {
                    native_runner_->setDataBreakpoints(names);
                }
            });
}

QList<timewarp::ui::TelemetryRow> MainWindow::ideTelemetryRows() const {
    QList<timewarp::ui::TelemetryRow> rows;
    if (last_run_ms_ >= 0) {
        rows.append({tr("Wall clock"), tr("%1 ms").arg(last_run_ms_)});
    }
    if (turtle_canvas_ != nullptr) {
        rows.append(
            {tr("Canvas ink"), tr("%1 segments, %2 shapes")
                                   .arg(turtle_canvas_->strokeCount())
                                   .arg(turtle_canvas_->shapeCount())});
        rows.append({tr("Ink length"),
                     tr("%1 units").arg(turtle_canvas_->totalStrokeLength(), 0, 'f', 1)});
    }
    return rows;
}

void MainWindow::refreshWatches() {
    if (debug_panel_ == nullptr) {
        return;
    }
    const QStringList expressions = debug_panel_->watches();
    if (expressions.isEmpty()) {
        debug_panel_->setWatchValues({});
        return;
    }

    std::map<std::string, timewarp::lang::Value> bindings;
    for (const timewarp::ui::VariableRow& row : last_variables_) {
        if (row.type == QLatin1String("text")) {
            bindings[row.name.toStdString()] = timewarp::lang::Value{row.value.toStdString()};
        } else if (row.value == QLatin1String("#t") || row.value == QLatin1String("#f")) {
            // Scheme booleans, which the machine renders this way. A watch of
            // `FLAG AND 1` should mean the same thing as in the program.
            bindings[row.name.toStdString()] =
                timewarp::lang::Value{row.value == QLatin1String("#t") ? 1.0 : 0.0};
        } else {
            bool ok = false;
            const double number = row.value.toDouble(&ok);
            bindings[row.name.toStdString()] = timewarp::lang::Value{ok ? number : 0.0};
        }
    }

    std::vector<std::string> wanted;
    wanted.reserve(static_cast<std::size_t>(expressions.size()));
    for (const QString& expression : expressions) {
        wanted.push_back(expression.toStdString());
    }
    const std::vector<timewarp::lang::WatchResult> results =
        timewarp::lang::evaluate_watches(wanted, bindings);

    QList<DebugPanel::WatchRow> rows;
    rows.reserve(static_cast<int>(results.size()));
    for (const timewarp::lang::WatchResult& result : results) {
        rows.append({QString::fromStdString(result.expression),
                     QString::fromStdString(result.value), result.ok,
                     QString::fromStdString(result.error)});
    }
    debug_panel_->setWatchValues(rows);
}

void MainWindow::showRunnerSnapshot() {
    if (runner_ == nullptr) {
        return;
    }
    const timewarp::debugger::DebugSnapshot snapshot = runner_->snapshot();
    // One vocabulary for both sources: the Scheme machine says "string" where the
    // bridge's inspector says "text", and a column whose word changed with the
    // language would read as two different types.
    const auto kind_word = [](const QString& kind) {
        return kind == QLatin1String("string") ? QStringLiteral("text") : kind;
    };
    QList<timewarp::ui::VariableRow> rows;
    rows.reserve(snapshot.variables.size());
    for (const timewarp::debugger::DebugVariable& entry : snapshot.variables) {
        rows.append({entry.name, kind_word(entry.kind), entry.rendered});
    }
    last_variables_ = rows;
    if (variables_panel_ != nullptr) {
        variables_panel_->setRows(rows);
    }
    if (debug_panel_ != nullptr) {
        QList<DebugPanel::Frame> frames;
        frames.reserve(snapshot.stack.size());
        for (const timewarp::debugger::StackFrameInfo& frame : snapshot.stack) {
            frames.append({frame.procedure, frame.line});
        }
        debug_panel_->setStack(frames);
        debug_panel_->setVariables(rows);
        if (snapshot.cause == timewarp::debugger::PauseCause::DataBreakpoint) {
            debug_panel_->setPauseReason(
                tr("%1 changed").arg(snapshot.changed_variable));
        }
    }
    refreshWatches();
}

void MainWindow::clearRunState() {
    engine_telemetry_.clear();
    last_variables_.clear();
    if (variables_panel_ != nullptr) {
        variables_panel_->setRows({});
    }
    if (telemetry_panel_ != nullptr) {
        telemetry_panel_->setRows({});
    }
    if (debug_panel_ != nullptr) {
        debug_panel_->setPaused(false, 0, QString{});
        debug_panel_->setStack({});
        debug_panel_->setVariables({});
    }
    refreshWatches();
}

void MainWindow::startRunClock() {
    last_run_ms_ = -1;
    run_clock_.start();
}

void MainWindow::stopRunClock() {
    if (!run_clock_.isValid()) {
        return;
    }
    last_run_ms_ = run_clock_.elapsed();
    run_clock_.invalidate();
}

void MainWindow::finishRun(const QString& error, bool completed) {
    stopRunClock();
    // Publish the tail of the drawing: the host stages coalesced state changes,
    // so the last pen colour or width can still be sitting in the queue.
    if (turtle_host_ != nullptr) {
        turtle_host_->flush();
    }
    canvas_bridge_->drainNow();

    // Fit the view only now. Refitting on every command would shrink the picture
    // continuously while it is being drawn, which reads as the canvas fighting
    // back.
    turtle_canvas_->fitToContent();

    // A Scheme run reports nothing through a runner's done-message, so its two
    // tables come from the runner and the IDE. Publishing them here rather than
    // leaving the HUD showing the last run's numbers is the difference between a
    // stale number and an absent one.
    if (debug_panel_ != nullptr) {
        debug_panel_->setPaused(false, 0, QString{});
    }
    showRunnerSnapshot();
    if (telemetry_panel_ != nullptr) {
        QList<timewarp::ui::TelemetryRow> rows;
        rows.append({tr("Language"), tr("Scheme")});
        rows.append({tr("Outcome"),
                     error.isEmpty() ? (completed ? tr("finished") : tr("stopped"))
                                     : tr("stopped with an error")});
        telemetry_panel_->setRows(rows + ideTelemetryRows());
    }

    if (!error.isEmpty()) {
        reportError(error);
        return;
    }
    setExecutionState(completed ? tr("ready") : tr("stopped"),
                      completed ? tr("Finished") : tr("Stopped"));
}

}  // namespace timewarp::ui
