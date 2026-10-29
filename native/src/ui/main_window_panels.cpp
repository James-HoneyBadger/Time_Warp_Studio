// SPDX-License-Identifier: MIT
//
// The window's panels, lessons and classroom: the right column's pages, the lesson
// requests the panels make, and the debug panel's wiring.
//
// It was the second half of `main_window_run.cpp`, which held two subjects: getting a
// program to run, and everything a panel asks for once it has. The split is that line.

#include "main_window_internal.hpp"

#include "timewarp/ui/main_window.hpp"

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/watch.hpp"
#include "timewarp/ui/native_runner.hpp"

#include <QDir>
#include <QFileDialog>
#include <QFileInfo>
#include <QInputDialog>
#include <QMessageBox>
#ifdef TIME_WARP_HAVE_PRINT
#endif
#include <QSettings>

#include <QStatusBar>
#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "timewarp/canvas/host_adapter.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#ifdef TIME_WARP_HAVE_AI
#include "timewarp/ai/client.hpp"
#include "timewarp/ai/config.hpp"
#include "timewarp/ui/ai_panel.hpp"
#include "timewarp/ui/ai_settings.hpp"
#endif
#include "timewarp/ui/app.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/benchmark.hpp"
#include "timewarp/ui/breakpoints.hpp"
#include "timewarp/ui/chip8_window.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/classroom_marker.hpp"
#include "timewarp/ui/command_reference.hpp"
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

// The settings keys and the small helpers the sections share.
using namespace detail;

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
    {"sql", 6},      {"reference", 7},
#ifdef TIME_WARP_HAVE_AI
    // Last because it is the one page that needs something outside this machine,
    // and a build without Qt Network does not have it at all rather than offering
    // a page that can only report that it cannot work.
    {"ai", 8},
#endif
};

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

void MainWindow::handInLesson(const QString& lesson, int checkpoint) {
    CodeEditor* editor = currentEditor();
    if (lesson_panel_ == nullptr) {
        return;
    }
    if (editor == nullptr) {
        lesson_panel_->setFailure(tr("the hand-in"),
                                  tr("open a document with your program in it first"));
        return;
    }
    if (!ensureClassroom() || classroom_ == nullptr) {
        lesson_panel_->setFailure(tr("the hand-in"),
                                  tr("this machine has no class folder to hand work in to"));
        return;
    }
    const std::vector<timewarp::lessons::Student>& students = classroom_->students();
    if (students.empty()) {
        lesson_panel_->setFailure(tr("the hand-in"),
                                  tr("no class list is set up here yet"));
        return;
    }
    // Which learner is this? The window cannot know, so it asks -- with one name in
    // the list when there is only one, which is the common case on a learner's own
    // machine.
    QString student = students.front().id;
    if (students.size() > 1) {
        QStringList names;
        for (const timewarp::lessons::Student& entry : students) {
            names << QStringLiteral("%1 (%2)").arg(entry.name, entry.id);
        }
        bool ok = false;
        const QString chosen = QInputDialog::getItem(
            this, tr("Hand in"), tr("Whose work is this?"), names, 0, false, &ok);
        const int index = names.indexOf(chosen);
        if (!ok || index < 0) {
            return;
        }
        student = students.at(static_cast<std::size_t>(index)).id;
    }

    timewarp::lessons::HandIn result = classroom_->handIn(
        student, lesson, checkpoint, editor->toPlainText(), false);
    if (result.exists) {
        const QMessageBox::StandardButton answer = QMessageBox::question(
            this, tr("Hand in"),
            tr("%1 is already handed in. Replace it with what is open now?")
                .arg(QFileInfo{result.path}.fileName()));
        if (answer == QMessageBox::Yes) {
            result = classroom_->handIn(student, lesson, checkpoint,
                                        editor->toPlainText(), true);
        } else {
            return;
        }
    }
    if (!result.ok) {
        lesson_panel_->setFailure(tr("the hand-in"), result.error);
        return;
    }
    setExecutionState(tr("ready"), tr("Handed in %1").arg(QFileInfo{result.path}.fileName()));
    requestLessonProgress();
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
    // How much a run remembers so it can be walked back. A run setting, so it is
    // persisted and applied when the next run starts rather than mid-run.
    debug_panel_->setHistoryDepth(settings.value(QLatin1String(kSettingHistory), 0).toInt());
    connect(debug_panel_, &DebugPanel::historyChanged, this, [this](int depth) {
        QSettings{}.setValue(QLatin1String(kSettingHistory), depth);
        if (native_runner_ != nullptr) {
            native_runner_->set_history(static_cast<std::size_t>(depth));
        }
    });
    connect(debug_panel_, &DebugPanel::rewindRequested, this, [this](int steps) {
        if (native_runner_ != nullptr && native_runner_->canRewind()) {
            native_runner_->request_rewind(static_cast<std::size_t>(steps));
            // The console is a transcript of the session, not of the run, so the
            // statements a rewind puts back print a second time. Saying where the
            // repeat starts is what keeps that from reading as a bug.
            appendOutput(tr("\u2139\ufe0f Back %n statement(s)\n", "", steps));
        }
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
#ifdef TIME_WARP_HAVE_AI
    if (ai_panel_ != nullptr && ai_panel_->sessionUsage().requests > 0) {
        // The assistant's own cost, beside the run's: what an answer spent is a
        // property of this session, and the HUD is where this window's counters
        // already live.
        const AiPanel::Usage usage = ai_panel_->sessionUsage();
        rows.append({tr("Assistant asks"), QString::number(usage.requests)});
        rows.append({tr("Assistant tokens"), tr("%1 in, %2 out")
                                                 .arg(usage.prompt_tokens)
                                                 .arg(usage.completion_tokens)});
    }
#endif
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

void MainWindow::showNativeStack(const NativeRunner& runner) {
    if (debug_panel_ == nullptr) {
        return;
    }
    QList<DebugPanel::Frame> frames;
    for (const NativeRunner::FrameRow& frame : runner.callStack()) {
        frames.append({frame.procedure, frame.line});
    }
    debug_panel_->setStack(frames);
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



void MainWindow::buildCommandReference() {
    reference_panel_ = new CommandReferencePanel{right_tabs_};
    right_tabs_->addTab(reference_panel_, tr("\U0001F4D6 Reference"));
    // **Try it** opens the row's program in a new document, through the same path a
    // lesson's starting program takes: a language override and an untitled label,
    // so the editor highlights and runs it without a file extension to guess from.
    connect(reference_panel_, &CommandReferencePanel::tryRequested, this,
            [this](const QString& languageKey, const QString& source, const QString& name) {
                openLessonCode(languageKey, source, name);
            });
    connect(reference_panel_, &CommandReferencePanel::statusMessage, this,
            [this](const QString& message) {
                statusBar()->showMessage(message, kStatusMessageMilliseconds);
            });
}

#ifdef TIME_WARP_HAVE_AI

// ---------------------------------------------------------------------------
// The assistant
// ---------------------------------------------------------------------------

void MainWindow::buildAiPanel() {
    // Read once, at construction: the settings file is the source of truth until
    // the user changes it, and a window that re-read it per question would ignore
    // an edit made in the meantime by another copy of the application.
    ai_config_ = timewarp::ai::load();
    ai_panel_ = new AiPanel{right_tabs_};
    ai_panel_->setContextProvider([this] { return aiContext(); });
    ai_panel_->setConfig(ai_config_);
    right_tabs_->addTab(ai_panel_, tr("\U0001F916 Assistant"));

    connect(ai_panel_, &AiPanel::configureRequested, this, &MainWindow::showAiSettings);
    connect(ai_panel_, &AiPanel::insertRequested, this, &MainWindow::insertAssistantCode);
    connect(ai_panel_, &AiPanel::statusMessage, this, [this](const QString& message) {
        statusBar()->showMessage(message, kStatusMessageMilliseconds * 2);
    });
    // The suggested line. Its own client, so a pause in typing cannot cancel an
    // answer the reader is in the middle of.
    inline_client_ = new timewarp::ai::Client{this};
    connect(inline_client_, &timewarp::ai::Client::answered, this,
            [this](const timewarp::ai::Reply& reply) {
                CodeEditor* editor = inline_target_;
                inline_target_ = nullptr;
                if (editor == nullptr || !reply.ok) {
                    return;
                }
                // The caret may have moved while the request was out: the answer is
                // about the text it was asked for, so it is dropped when that text
                // is no longer what precedes the caret.
                if (editor->textBeforeCursor(4000) != inline_prefix_) {
                    inline_prefix_.clear();
                    return;
                }
                inline_prefix_.clear();
                editor->setGhostText(reply.text);
            });
    connect(inline_client_, &timewarp::ai::Client::failed, this,
            [this](const QString&) {
                // Silent on purpose: a suggestion that does not arrive is not an
                // event, and a red sentence under a caret is a worse interruption
                // than no suggestion.
                inline_target_ = nullptr;
                inline_prefix_.clear();
            });
    applyInlineSuggestionSetting();

    connect(ai_panel_, &AiPanel::usageChanged, this, [this] {
        // The HUD's rows are rebuilt from the window's own counters, so a new
        // number only has to ask for that rebuild.
        if (telemetry_panel_ != nullptr) {
            telemetry_panel_->setRows(ideTelemetryRows());
        }
    });
}

timewarp::ai::Context MainWindow::aiContext() const {
    timewarp::ai::Context context;
    const CodeEditor* editor = currentEditor();
    if (editor == nullptr) {
        return context;
    }
    context.language = displayName(languageOf(editor));
    context.file_name = nameOf(editor);
    context.source = editor->toPlainText();
    // A selection is what the user means when there is one: "explain this" pointed
    // at six lines is a question about six lines.
    if (editor->textCursor().hasSelection()) {
        QString selected = editor->textCursor().selectedText();
        // QTextCursor separates paragraphs with U+2029, which a model reads as a
        // character rather than a line break.
        selected.replace(QChar::ParagraphSeparator, QLatin1Char('\n'));
        context.selection = selected;
    }
    // What the last run said, for "find what is wrong with this". Bounded: the
    // console holds every run of the session, and a thousand lines of an earlier
    // program is not evidence about this one.
    if (errors_view_ != nullptr) {
        const QString errors = errors_view_->toPlainText().trimmed();
        if (!errors.isEmpty()) {
            context.last_output += tr("Errors:\n%1").arg(errors.right(2000));
        }
    }
    if (console_view_ != nullptr) {
        const QString output = console_view_->toPlainText().trimmed();
        if (!output.isEmpty()) {
            if (!context.last_output.isEmpty()) {
                context.last_output += QStringLiteral("\n\n");
            }
            context.last_output += tr("Output:\n%1").arg(output.right(4000));
        }
    }
    return context;
}

void MainWindow::insertAssistantCode(const QString& code) {
    if (code.trimmed().isEmpty()) {
        return;
    }
    CodeEditor* editor = currentEditor();
    // No document open is not a refusal: the answer is a program, and a program
    // with nowhere to go would be lost. It opens in a new document, which is what
    // the user would have done next.
    if (editor == nullptr) {
        editor = newFile();
    }
    QTextCursor cursor = editor->textCursor();
    cursor.beginEditBlock();
    if (cursor.hasSelection()) {
        cursor.removeSelectedText();
    }
    cursor.insertText(code);
    cursor.endEditBlock();
    editor->setTextCursor(cursor);
    editor->setFocus();
    setExecutionState(tr("ready"), tr("The answer was inserted into %1").arg(nameOf(editor)));
}

void MainWindow::showAiSettings() {
    auto* dialog = new AiSettingsDialog{ai_config_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    connect(dialog, &AiSettingsDialog::saved, this,
            [this](const timewarp::ai::Config& config) {
                ai_config_ = config;
                timewarp::ai::save(config);
                if (ai_panel_ != nullptr) {
                    ai_panel_->setConfig(config);
                }
                applyInlineSuggestionSetting();
                setExecutionState(tr("ready"),
                                  tr("Assistant settings saved — %1")
                                      .arg(timewarp::ai::summary(config)));
            });
    dialog->open();
}

void MainWindow::showAssistant() {
    // The key is in the table, so the only way this fails is a build with no
    // assistant -- and then there is no panel to focus either.
    static_cast<void>(showPanel(QStringLiteral("ai")));
    if (ai_panel_ == nullptr) {
        return;
    }
    ai_panel_->focusInput();
    if (!ai_panel_->configured()) {
        showAiSettings();
    }
}

void MainWindow::askAssistant(timewarp::ai::Task task) {
    static_cast<void>(showPanel(QStringLiteral("ai")));
    if (ai_panel_ == nullptr) {
        return;
    }
    if (!ai_panel_->configured()) {
        // A menu entry that reports "not configured" and stops is a dead end. The
        // window it needs is one click away, and the question can be asked again
        // with the button that is still in front of the user.
        showAiSettings();
        setExecutionState(tr("ready"),
                          tr("Choose a service and a model, then ask again"));
        return;
    }
    ai_panel_->ask(task);
    ai_panel_->focusInput();
}

void MainWindow::applyInlineSuggestionSetting() {
    if (inline_client_ != nullptr && !ai_config_.inline_suggestions) {
        // Turning it off has to take back a request already on its way, or a line
        // would appear after the switch that asked for none.
        inline_client_->cancel();
        inline_target_ = nullptr;
    }
    editor_settings_.inline_suggestions = ai_config_.inline_suggestions;
    for (QTabWidget* tabs : {editor_tabs_, editor_tabs_secondary_}) {
        if (tabs == nullptr) {
            continue;
        }
        for (int index = 0; index < tabs->count(); ++index) {
            if (auto* editor = qobject_cast<CodeEditor*>(tabs->widget(index));
                editor != nullptr) {
                editor->applySettings(editor_settings_);
            }
        }
    }
}

void MainWindow::requestInlineSuggestion(CodeEditor* editor, const QString& prefix) {
    if (editor == nullptr || inline_client_ == nullptr ||
        !ai_config_.inline_suggestions || prefix.trimmed().isEmpty()) {
        return;
    }
    if (!timewarp::ai::ready(ai_config_, nullptr)) {
        return;
    }
    if (inline_client_->busy()) {
        // The next pause asks again; superseding an answer already on its way would
        // spend a request to arrive at the same place.
        return;
    }
    timewarp::ai::Context context;
    context.language = displayName(languageOf(editor));
    context.file_name = nameOf(editor);
    // The program *up to the caret*: what comes next is the question.
    context.source = prefix;
    inline_target_ = editor;
    inline_prefix_ = prefix;
    inline_client_->send(ai_config_,
                         timewarp::ai::build_prompt(timewarp::ai::Task::Continue, context,
                                                    QString{}));
}

void MainWindow::askAssistantAboutOutput() {
    static_cast<void>(showPanel(QStringLiteral("ai")));
    if (ai_panel_ == nullptr) {
        return;
    }
    // What "this error" means, in the order a reader means it: the lines they
    // selected, then what the run reported as an error, then the last thing the
    // program printed. The Console keeps every run of the session, so the newest is
    // the one worth asking about.
    QString reported;
    if (console_view_ != nullptr) {
        reported = console_view_->textCursor().selectedText().trimmed();
        // QTextCursor separates paragraphs with U+2029, which a model reads as a
        // character rather than a line break.
        reported.replace(QChar::ParagraphSeparator, QLatin1Char('\n'));
    }
    if (reported.isEmpty() && errors_view_ != nullptr) {
        reported = lastNonEmptyLine(errors_view_->toPlainText());
    }
    if (reported.isEmpty() && console_view_ != nullptr) {
        reported = lastNonEmptyLine(console_view_->toPlainText());
    }
    if (reported.isEmpty()) {
        setExecutionState(tr("ready"),
                          tr("Run the program first: there is no output to ask about"));
        return;
    }
    if (!ai_panel_->configured()) {
        showAiSettings();
        setExecutionState(tr("ready"),
                          tr("Choose a service and a model, then ask again"));
        return;
    }
    ai_panel_->ask(timewarp::ai::Task::Diagnose, reported);
    ai_panel_->focusInput();
}

#endif  // TIME_WARP_HAVE_AI

}  // namespace timewarp::ui
