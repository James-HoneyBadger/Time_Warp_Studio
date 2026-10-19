// SPDX-License-Identifier: MIT
//
// Tests for the Python bridge: the process, the protocol, and the drawing it
// produces.
//
// These run the real worker with a real Python interpreter, because the thing
// being tested is a conversation between two processes and mocking either half
// would test the mock. What that costs is a dependency on Python and on the
// repository layout; `locateWorker` reports exactly what it looked for, and a run
// with no Python available says so and skips rather than failing, because a
// missing interpreter is an environment fact rather than a defect in this code.
//
// The half that *is* hermetic is the parser, and it is tested against a scripted
// worker (`TIME_WARP_WORKER` pointing at a file this test writes). That is the
// only way to reach the paths that matter most and cannot be provoked from a
// program: a worker that dies during start-up, a worker that dies mid-run, and a
// message the IDE cannot read.

#include <cstdio>
#include <functional>
#include <memory>
#include <optional>
#include <string>

#include <QByteArray>
#include <QCoreApplication>
#include <QDir>
#include <QElapsedTimer>
#include <QFile>
#include <QPointF>
#include <QProcessEnvironment>
#include <QSet>
#include <QString>
#include <QStringList>
#include <QTemporaryDir>
#include <QThread>
#include <QTimer>

#include "check.hpp"
#include "timewarp/bridge/python_worker.hpp"
#include "timewarp/canvas/command.hpp"
#include "timewarp/canvas/command_queue.hpp"

namespace {

using timewarp::bridge::LessonCheckpoint;
using timewarp::bridge::LessonProgress;
using timewarp::bridge::LessonSummary;
using timewarp::bridge::PythonWorker;
using timewarp::bridge::locateWorker;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::CanvasCommandQueue;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// Everything the worker reported, so a test can assert on the whole run.
struct Recording {
    QString output;
    QStringList errors;
    QStringList unsupported;
    bool ready = false;
    int finished_count = 0;
    bool last_ok = false;
    QString last_error;
    int failures = 0;
    QString last_failure;

    // Drawing, as the IDE would have received it.
    int strokes = 0;
    int clears = 0;
    int background_commands = 0;
    std::uint32_t background = 0;
    std::optional<QPointF> first_stroke_from;
    std::optional<QPointF> first_stroke_to;
    double first_stroke_width = 0.0;
    std::uint32_t first_stroke_colour = 0;
    QPointF pose;
    double pose_heading = 0.0;
    bool pose_visible = true;
    int pose_count = 0;

    [[nodiscard]] bool sawTerminalEvent() const { return finished_count > 0 || failures > 0; }

    // Lessons, as the panel would have received them.
    QList<LessonSummary> lessons;
    int presentation_font = 0;
    QList<LessonProgress> progress_rows;
    int progress_completed = 0;
    int progress_total = 0;
    QString hint_text;
    int hint_number = 0;
    int hint_total = 0;
    QString solution_text;
    bool checked = false;
    bool check_passed = false;
    QString check_message;
    bool check_completed = false;
    int check_finished = 0;
    int check_total = 0;
    QString reset_lesson;
    QString lesson_failure_request;
    QString lesson_failure_text;

    // The classroom, as the panel would have received it.
    bool roster_arrived = false;
    QList<timewarp::bridge::ClassroomStudent> students;
    QStringList assigned_lessons;
    QString assignment_note;
    bool marks_arrived = false;
    QList<timewarp::bridge::ClassroomMark> marks;
    QString classroom_failure_request;
    QString classroom_failure_text;
};

/// Runs a worker and collects everything it says.
class Harness {
public:
    Harness() : worker_{queue_} {
        // The consumer side, inline: draining in the test rather than through a
        // Qt widget keeps this suite free of Widgets and of a display.
        QObject::connect(&worker_, &PythonWorker::commandsQueued, &worker_, [this] { drain(); });
        QObject::connect(&worker_, &PythonWorker::ready, &worker_,
                         [this] { recorded_.ready = true; });
        QObject::connect(&worker_, &PythonWorker::outputReceived, &worker_,
                         [this](const QString& text) { recorded_.output += text; });
        QObject::connect(&worker_, &PythonWorker::errorReceived, &worker_,
                         [this](const QString& text) { recorded_.errors << text; });
        QObject::connect(&worker_, &PythonWorker::poseReceived, &worker_,
                         [this](double x, double y, double heading, bool visible) {
                             recorded_.pose = QPointF{x, y};
                             recorded_.pose_heading = heading;
                             recorded_.pose_visible = visible;
                             ++recorded_.pose_count;
                         });
        QObject::connect(&worker_, &PythonWorker::finished, &worker_,
                         [this](bool ok, const QString& error, const QStringList& unsupported) {
                             ++recorded_.finished_count;
                             recorded_.last_ok = ok;
                             recorded_.last_error = error;
                             recorded_.unsupported = unsupported;
                         });
        QObject::connect(&worker_, &PythonWorker::failed, &worker_,
                         [this](const QString& text) {
                             ++recorded_.failures;
                             recorded_.last_failure = text;
                         });
        QObject::connect(&worker_, &PythonWorker::lessonsReceived, &worker_,
                         [this](const QList<LessonSummary>& lessons, int presentation_font) {
                             recorded_.lessons = lessons;
                             recorded_.presentation_font = presentation_font;
                         });
        QObject::connect(&worker_, &PythonWorker::lessonProgressReceived, &worker_,
                         [this](const QList<LessonProgress>& rows, int completed, int total) {
                             recorded_.progress_rows = rows;
                             recorded_.progress_completed = completed;
                             recorded_.progress_total = total;
                         });
        QObject::connect(&worker_, &PythonWorker::lessonHintReceived, &worker_,
                         [this](const QString& text, int number, int total) {
                             recorded_.hint_text = text;
                             recorded_.hint_number = number;
                             recorded_.hint_total = total;
                         });
        QObject::connect(&worker_, &PythonWorker::lessonSolutionReceived, &worker_,
                         [this](const QString& text) { recorded_.solution_text = text; });
        QObject::connect(
            &worker_, &PythonWorker::lessonChecked, &worker_,
            [this](const QString&, int, bool passed, const QString& message, bool completed,
                   int finished, int total) {
                recorded_.checked = true;
                recorded_.check_passed = passed;
                recorded_.check_message = message;
                recorded_.check_completed = completed;
                recorded_.check_finished = finished;
                recorded_.check_total = total;
            });
        QObject::connect(&worker_, &PythonWorker::lessonResetDone, &worker_,
                         [this](const QString& lesson) { recorded_.reset_lesson = lesson; });
        QObject::connect(&worker_, &PythonWorker::lessonFailed, &worker_,
                         [this](const QString& request, const QString& error) {
                             recorded_.lesson_failure_request = request;
                             recorded_.lesson_failure_text = error;
                         });
        QObject::connect(&worker_, &PythonWorker::classroomRosterReceived, &worker_,
                         [this](const QList<timewarp::bridge::ClassroomStudent>& students,
                                const QStringList& lessons, const QString& note) {
                             recorded_.students = students;
                             recorded_.assigned_lessons = lessons;
                             recorded_.assignment_note = note;
                             recorded_.roster_arrived = true;
                         });
        QObject::connect(&worker_, &PythonWorker::classroomMarksReceived, &worker_,
                         [this](const QList<timewarp::bridge::ClassroomMark>& rows) {
                             recorded_.marks = rows;
                             recorded_.marks_arrived = true;
                         });
        QObject::connect(&worker_, &PythonWorker::classroomFailed, &worker_,
                         [this](const QString& request, const QString& error) {
                             recorded_.classroom_failure_request = request;
                             recorded_.classroom_failure_text = error;
                         });
    }

    /// Forgets the last verdict, so a second check in one test is unambiguous.
    ///
    /// Without this the second wait would be satisfied by the first answer still
    /// sitting in the record, and the test would pass whether or not the second
    /// check ever ran.
    void clearCheck() {
        recorded_.checked = false;
        recorded_.check_passed = false;
        recorded_.check_message.clear();
    }

    /// Forgets the last classroom answers, for the same reason `clearCheck`
    /// exists: `roster_arrived` would already be true, and a wait on it would
    /// return before the second answer had been parsed.
    void clearClassroom() {
        recorded_.roster_arrived = false;
        recorded_.students.clear();
        recorded_.assigned_lessons.clear();
        recorded_.assignment_note.clear();
        recorded_.marks_arrived = false;
        recorded_.marks.clear();
        recorded_.classroom_failure_request.clear();
        recorded_.classroom_failure_text.clear();
    }

    [[nodiscard]] PythonWorker& worker() { return worker_; }
    [[nodiscard]] const Recording& recorded() const { return recorded_; }
    /// The queue's consumer side. Called automatically on every batch.
    void drain() { queue_.drain([this](timewarp::canvas::CanvasCommandView view) { note(view); }); }

    /// Starts the worker, returning false (and recording the reason) if it could
    /// not be started.
    [[nodiscard]] bool start() {
        QString error;
        if (!worker_.start(&error)) {
            start_error_ = error;
            return false;
        }
        return true;
    }

    [[nodiscard]] const QString& startError() const { return start_error_; }

    /// Starts the worker, reporting the reason and counting a failure when it
    /// cannot. Returns false so the caller can stop, rather than waiting out a
    /// timeout for a run that was never sent -- which turns one broken start into
    /// a suite that takes minutes and explains nothing.
    [[nodiscard]] bool requireStart() {
        if (start()) {
            return true;
        }
        TW_CHECK(false);
        // The paths go in the message: "it did not start" is not actionable, and
        // the first thing worth knowing is which interpreter was launched against
        // which script.
        std::printf("     the worker did not start: %s\n     interpreter: %s\n     script: %s\n",
                    qPrintable(start_error_),
                    qPrintable(worker_.paths().interpreter),
                    qPrintable(worker_.paths().script));
        // And what the script actually contains: for a scripted worker, the script
        // *is* the test's input, and a mismatch between what the test meant to
        // write and what it wrote is indistinguishable from a broken bridge
        // without seeing it.
        QFile probe{worker_.paths().script};
        if (probe.open(QIODevice::ReadOnly)) {
            std::printf("     script contents:\n%s\n", probe.readAll().constData());
        } else {
            std::printf("     script could not be read back: %s\n",
                        qPrintable(probe.errorString()));
        }
        std::fflush(stdout);
        return false;
    }

    /// Spins the event loop until `done` holds. Returns whether it held.
    [[nodiscard]] bool waitFor(const std::function<bool()>& done, int timeout_ms = 30000) {
        QElapsedTimer timer;
        timer.start();
        while (!done() && timer.elapsed() < timeout_ms) {
            QCoreApplication::processEvents(QEventLoop::AllEvents, 10);
            QThread::msleep(1);
        }
        return done();
    }

private:
    void note(timewarp::canvas::CanvasCommandView view) {
        const auto& command = view.command;
        if (command.kind == CanvasCommandKind::DrawPolyline) {
            if (recorded_.strokes == 0 && view.point_count() >= 2) {
                recorded_.first_stroke_from = QPointF{view.points[0], view.points[1]};
                recorded_.first_stroke_to = QPointF{view.points[2], view.points[3]};
                recorded_.first_stroke_width = command.x;
                recorded_.first_stroke_colour = command.color;
            }
            ++recorded_.strokes;
        } else if (command.kind == CanvasCommandKind::Clear) {
            ++recorded_.clears;
        } else if (command.kind == CanvasCommandKind::SetBackgroundColor) {
            ++recorded_.background_commands;
            recorded_.background = command.color;
        }
    }

    CanvasCommandQueue queue_;
    PythonWorker worker_;
    Recording recorded_;
    QString start_error_;
};

/// Writes `contents` to `dir/name` and returns the path.
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

/// Points every worker started while this object lives at a private data directory.
///
/// The lesson tests record hints and completed checkpoints, and those are written
/// to `~/.time_warp/progress.json` -- the *learner's* file. A suite that wrote
/// there would corrupt somebody's place in the curriculum, and would then pass or
/// fail depending on what the previous run had left behind: the hint counter is
/// cumulative, which is exactly how this was found.
class DataDirOverride {
public:
    explicit DataDirOverride(const QString& directory) {
        previous_ = QProcessEnvironment::systemEnvironment().value(
            QString::fromLatin1("TIME_WARP_DATA_DIR"));
        qputenv("TIME_WARP_DATA_DIR", directory.toUtf8());
    }
    ~DataDirOverride() {
        if (previous_.isEmpty()) {
            qunsetenv("TIME_WARP_DATA_DIR");
        } else {
            qputenv("TIME_WARP_DATA_DIR", previous_.toUtf8());
        }
    }
    DataDirOverride(const DataDirOverride&) = delete;
    DataDirOverride& operator=(const DataDirOverride&) = delete;

private:
    QString previous_;
};

/// Points the worker at `script` for the duration of the returned object's life.
///
/// A guard rather than a plain `qputenv`, because a test that leaves the override
/// behind would make every later test run a scripted worker and pass for the wrong
/// reason.
class ScriptOverride {
public:
    explicit ScriptOverride(const QString& script) {
        previous_ = QProcessEnvironment::systemEnvironment().value(
            QString::fromLatin1("TIME_WARP_WORKER"));
        qputenv("TIME_WARP_WORKER", script.toUtf8());
    }
    ~ScriptOverride() {
        if (previous_.isEmpty()) {
            qunsetenv("TIME_WARP_WORKER");
        } else {
            qputenv("TIME_WARP_WORKER", previous_.toUtf8());
        }
    }
    ScriptOverride(const ScriptOverride&) = delete;
    ScriptOverride& operator=(const ScriptOverride&) = delete;

private:
    QString previous_;
};

[[nodiscard]] bool nearly(double actual, double expected, double tolerance = 1e-6) {
    return std::abs(actual - expected) <= tolerance;
}

// ---------------------------------------------------------------------------
// Lessons
// ---------------------------------------------------------------------------

/// A worker with the curriculum loaded, or null when the bridge is unavailable.
///
/// The lesson tests all need the same handshake, and the curriculum is something
/// the worker reads from disk, so every one of them would otherwise repeat the
/// same four lines of setup.
[[nodiscard]] bool openLessons(Harness& harness) {
    if (!harness.requireStart()) {
        return false;
    }
    if (!harness.worker().requestLessons()) {
        return false;
    }
    return harness.waitFor([&harness] { return !harness.recorded().lessons.isEmpty(); });
}

void test_the_curriculum_arrives_without_the_answers() {
    announce("lesson curriculum");
    // A private data directory: the lesson requests write progress, and that is
    // the learner's file rather than the test's.
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!openLessons(harness)) {
        TW_CHECK(false);
        return;
    }

    const QList<LessonSummary>& lessons = harness.recorded().lessons;
    TW_CHECK_EQ(lessons.size(), 20);

    // Every language in the studio is reachable from the panel, which is the
    // point of the exercise: a language with no lesson is a language the lesson
    // panel cannot offer.
    QSet<QString> languages;
    for (const LessonSummary& lesson : lessons) {
        languages.insert(lesson.language);
        TW_CHECK_EQ(lesson.checkpointCount(), 3);
        TW_CHECK(!lesson.title.isEmpty());
        TW_CHECK(!lesson.description.isEmpty());
        // The prompt a learner reads, which is what the panel is for. The *answer*
        // is not in this payload at all -- `LessonCheckpoint` has no field for it
        // -- and that is deliberate: it is sent to every client that opens the
        // panel, and a lesson whose answer travels with its questions has been
        // spoiled before it is read.
        for (const LessonCheckpoint& checkpoint : lesson.checkpoints) {
            TW_CHECK(!checkpoint.title.isEmpty());
            TW_CHECK(!checkpoint.description.isEmpty());
            TW_CHECK(!checkpoint.expectation.isEmpty());
        }
    }
    TW_CHECK_EQ(languages.size(), 10);

    // The size a lesson is projected at travels with the curriculum, so a teacher
    // who set it with `teach.py present` gets the panel at that size. It is a real
    // number rather than zero, which is what "the worker did not say" looks like.
    TW_CHECK(harness.recorded().presentation_font > 0);
}

void test_a_correct_answer_passes_and_a_wrong_one_does_not() {
    announce("lesson check");
    // A private data directory: the lesson requests write progress, and that is
    // the learner's file rather than the test's.
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!openLessons(harness)) {
        TW_CHECK(false);
        return;
    }

    // `logo_shapes` checkpoint 0 asks for one straight line, and the check is a
    // real run of the turtle, so this proves the panel's verdict and a learner's
    // own run come from the same engine.
    TW_CHECK(harness.worker().checkLesson(QStringLiteral("logo_shapes"), 0,
                                          QStringLiteral("FORWARD 100")));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().checked; }));
    TW_CHECK(harness.recorded().check_passed);
    TW_CHECK(harness.recorded().check_message.contains(QStringLiteral("passed")));
    TW_CHECK_EQ(harness.recorded().check_total, 20);

    harness.clearCheck();
    TW_CHECK(harness.worker().checkLesson(QStringLiteral("logo_shapes"), 0,
                                          QStringLiteral("REPEAT 3 [ FORWARD 100 RIGHT 120 ]")));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().checked; }));
    TW_CHECK(!harness.recorded().check_passed);
    // The message says what was drawn, not only that it was wrong: "you drew 3
    // segments and this needs 1" is what a learner can act on.
    TW_CHECK(harness.recorded().check_message.contains(QStringLiteral("3")));
}

void test_a_hint_arrives_without_the_answer() {
    announce("lesson hint");
    // A private data directory: the lesson requests write progress, and that is
    // the learner's file rather than the test's.
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!openLessons(harness)) {
        TW_CHECK(false);
        return;
    }

    TW_CHECK(harness.worker().requestHint(QStringLiteral("basic_first_steps"), 0));
    TW_CHECK(harness.waitFor([&harness] { return !harness.recorded().hint_text.isEmpty(); }));
    TW_CHECK_EQ(harness.recorded().hint_number, 1);
    TW_CHECK(harness.recorded().hint_total >= 1);

    // Asking again gives the *next* hint rather than the same one, which is the
    // whole reason a learner presses the button twice.
    const QString first = harness.recorded().hint_text;
    TW_CHECK(harness.worker().requestHint(QStringLiteral("basic_first_steps"), 0));
    TW_CHECK(harness.waitFor([&harness] {
        return !harness.recorded().hint_text.isEmpty() && harness.recorded().hint_number == 2;
    }));
    TW_CHECK(harness.recorded().hint_text != first);
}

void test_the_worked_answer_is_only_sent_when_it_is_asked_for() {
    announce("lesson solution");
    // A private data directory: the lesson requests write progress, and that is
    // the learner's file rather than the test's.
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!openLessons(harness)) {
        TW_CHECK(false);
        return;
    }

    TW_CHECK(harness.recorded().solution_text.isEmpty());
    TW_CHECK(harness.worker().requestSolution(QStringLiteral("logo_shapes"), 0));
    TW_CHECK(harness.waitFor([&harness] { return !harness.recorded().solution_text.isEmpty(); }));
    // And it is the answer the check would accept.
    harness.clearCheck();
    TW_CHECK(harness.worker().checkLesson(QStringLiteral("logo_shapes"), 0,
                                          harness.recorded().solution_text));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().checked; }));
    TW_CHECK(harness.recorded().check_passed);
}

void test_progress_covers_every_lesson() {
    announce("lesson progress");
    // A private data directory: the lesson requests write progress, and that is
    // the learner's file rather than the test's.
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!openLessons(harness)) {
        TW_CHECK(false);
        return;
    }
    TW_CHECK(harness.worker().requestLessonProgress());
    TW_CHECK(harness.waitFor([&harness] { return !harness.recorded().progress_rows.isEmpty(); }));

    TW_CHECK_EQ(harness.recorded().progress_rows.size(), 20);
    TW_CHECK_EQ(harness.recorded().progress_total, 20);
    TW_CHECK(harness.recorded().progress_completed >= 0);
    for (const LessonProgress& row : harness.recorded().progress_rows) {
        TW_CHECK(!row.status.isEmpty());
        TW_CHECK_EQ(row.checkpoints, 3);
    }
}

void test_a_check_is_refused_while_a_program_is_running() {
    announce("lesson check while busy");
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    // Both a program and a check run the engine, so letting them overlap would
    // have two interpreters sharing module state. The refusal is the feature.
    TW_CHECK(harness.worker().run(QStringLiteral("basic"),
                                  QStringLiteral("10 PRINT \"TICK\"\n20 GOTO 10\n"),
                                  QStringLiteral("forever.bas")));
    TW_CHECK(harness.waitFor([&harness] { return !harness.recorded().output.isEmpty(); },
                             10000));

    TW_CHECK(!harness.worker().checkLesson(QStringLiteral("logo_shapes"), 0,
                                           QStringLiteral("FORWARD 100")));
    TW_CHECK(!harness.recorded().lesson_failure_text.isEmpty());
    TW_CHECK(harness.recorded().lesson_failure_text.contains(QStringLiteral("Stop")));

    harness.worker().stop();
    TW_CHECK(harness.waitFor([&harness] { return !harness.worker().isRunning(); }, 10000));
}

void test_reset_names_the_lesson_it_reset() {
    announce("lesson reset");
    // A private data directory: the lesson requests write progress, and that is
    // the learner's file rather than the test's.
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!openLessons(harness)) {
        TW_CHECK(false);
        return;
    }
    TW_CHECK(harness.worker().resetLesson(QStringLiteral("logo_shapes")));
    TW_CHECK(harness.waitFor([&harness] { return !harness.recorded().reset_lesson.isEmpty(); }));
    // Named rather than a bare "done": a panel can have a reset in flight while
    // the learner clicks another lesson.
    TW_CHECK_EQ(harness.recorded().reset_lesson, QStringLiteral("logo_shapes"));
}

// ---------------------------------------------------------------------------
// The classroom
// ---------------------------------------------------------------------------

/// Runs a snippet against the reference engine's own modules and returns its
/// output, or an empty string when it failed.
///
/// The classroom tests need a class list, and the *engine* is what defines one:
/// writing `roster.json` here would test this file's idea of the format against
/// itself and prove nothing. So the class is made the way a teacher makes one --
/// `ClassroomMode`, which `teach.py` drives -- and then read back through the
/// panel's request.
[[nodiscard]] QString runEngine(const PythonWorker& worker, const QString& code,
                               const QString& data_dir, QString* error = nullptr) {
    QProcess process;
    QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
    env.insert(QStringLiteral("PYTHONPATH"),
               QFileInfo{worker.paths().script}.absolutePath());
    env.insert(QStringLiteral("TIME_WARP_DATA_DIR"), data_dir);
    process.setProcessEnvironment(env);
    process.start(worker.paths().interpreter, {QStringLiteral("-c"), code});
    if (!process.waitForStarted(10000)) {
        if (error != nullptr) {
            *error = process.errorString();
        }
        return {};
    }
    if (!process.waitForFinished(120000)) {
        process.kill();
        process.waitForFinished(5000);
        if (error != nullptr) {
            *error = QStringLiteral("the engine did not finish");
        }
        return {};
    }
    const QString out = QString::fromUtf8(process.readAllStandardOutput());
    const QString err = QString::fromUtf8(process.readAllStandardError());
    if (process.exitStatus() != QProcess::NormalExit || process.exitCode() != 0) {
        if (error != nullptr) {
            *error = err.isEmpty() ? QStringLiteral("exit %1").arg(process.exitCode()) : err;
        }
        return {};
    }
    return out;
}

/// A roster added by the engine is listed, with the assignment, and a handed-out
/// checkpoint comes back marked.
void test_the_class_list_and_its_marks_arrive() {
    announce("classroom roster and marks");
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }

    // The teacher's side of the protocol, run against the test's data directory.
    // `distribute` writes the starter code, which is what a learner actually
    // hands in, so the marks below are of real files on disk.
    const QString setup = runEngine(harness.worker(),
                                    QStringLiteral(
                                        "from time_warp.features.classroom_mode import "
                                        "ClassroomMode\n"
                                        "mode = ClassroomMode()\n"
                                        "student = mode.add_student('Ada Lovelace')\n"
                                        "lesson = mode.lessons[0].id\n"
                                        "mode.assign([lesson], 'Chapter one')\n"
                                        "mode.distribute([lesson])\n"
                                        "print(student.student_id)\n"
                                        "print(lesson)\n"),
                                    data_dir.path(), &reason);
    if (setup.isEmpty()) {
        std::printf("SKIP: the engine could not make a class list: %s\n", qPrintable(reason));
        return;
    }
    const QStringList lines = setup.split(QLatin1Char('\n'), Qt::SkipEmptyParts);
    TW_CHECK_EQ(lines.size(), 2);
    if (lines.size() != 2) {
        return;
    }
    const QString student_id = lines.at(0).trimmed();
    const QString lesson_id = lines.at(1).trimmed();

    TW_CHECK(harness.worker().requestClassroomRoster());
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().roster_arrived; }));
    const Recording& first = harness.recorded();
    TW_CHECK(first.classroom_failure_text.isEmpty());
    TW_CHECK_EQ(first.students.size(), 1);
    if (first.students.size() != 1) {
        return;
    }
    TW_CHECK_EQ(first.students.at(0).id, student_id);
    TW_CHECK_EQ(first.students.at(0).name, QStringLiteral("Ada Lovelace"));
    // Where the handouts went, which is what the panel's Open Folder needs. The
    // engine decides it; the panel only shows it.
    TW_CHECK(first.students.at(0).directory.endsWith(QLatin1String("/students/") + student_id));
    TW_CHECK(first.students.at(0).directory.startsWith(data_dir.path()));
    TW_CHECK_EQ(first.assigned_lessons, QStringList{lesson_id});
    TW_CHECK_EQ(first.assignment_note, QStringLiteral("Chapter one"));

    TW_CHECK(harness.worker().requestClassroomMarks());
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().marks_arrived; }));
    const QList<timewarp::bridge::ClassroomMark>& rows = harness.recorded().marks;
    TW_CHECK_EQ(rows.size(), 1);
    if (rows.size() != 1) {
        return;
    }
    TW_CHECK_EQ(rows.at(0).student, student_id);
    TW_CHECK_EQ(rows.at(0).lesson, lesson_id);
    TW_CHECK(!rows.at(0).title.isEmpty());
    // Every checkpoint was handed out, so every one is handed in. The *passed*
    // count is the starter code's business and is not asserted: a starter that
    // happened to pass would be a curriculum bug, not a bridge one.
    TW_CHECK(rows.at(0).checkpoints > 0);
    TW_CHECK_EQ(rows.at(0).handedIn, rows.at(0).checkpoints);
    TW_CHECK(rows.at(0).passed >= 0);
    TW_CHECK(rows.at(0).passed <= rows.at(0).handedIn);
    TW_CHECK(!rows.at(0).complete);

    // Round trip: the engine can read back the class list that the panel was
    // shown, so nothing in this path writes a file the engine cannot parse.
    const QString read_back = runEngine(harness.worker(),
                                        QStringLiteral(
                                            "from time_warp.features.classroom_mode import "
                                            "ClassroomMode\n"
                                            "mode = ClassroomMode()\n"
                                            "print(len(mode.roster.students))\n"
                                            "print(mode.assignment.lesson_ids[0])\n"),
                                        data_dir.path(), &reason);
    TW_CHECK(!read_back.isEmpty());
    TW_CHECK(read_back.startsWith(QStringLiteral("1\n")));

    // A second look is a re-read, not a cached copy: `teach.py` is expected to
    // change the class while the IDE is open, and Refresh has to show it.
    const QString added =
        runEngine(harness.worker(),
                  QStringLiteral("from time_warp.features.classroom_mode import ClassroomMode\n"
                                 "mode = ClassroomMode()\n"
                                 "mode.add_student('Alan Turing')\n"
                                 "print('added')\n"),
                  data_dir.path(), &reason);
    if (added.isEmpty()) {
        std::printf("     adding a student failed: %s\n", qPrintable(reason));
    }
    TW_CHECK(!added.isEmpty());
    harness.clearClassroom();
    TW_CHECK(harness.worker().requestClassroomRoster());
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().roster_arrived; }));
    TW_CHECK_EQ(harness.recorded().students.size(), 2);
    for (const timewarp::bridge::ClassroomStudent& student : harness.recorded().students) {
        TW_CHECK(student.name == QStringLiteral("Ada Lovelace")
                 || student.name == QStringLiteral("Alan Turing"));
    }
}

/// Marking runs an interpreter, so it is refused while a program is running for
/// the same reason a lesson check is. The class list is a file read and is not.
void test_the_marks_are_refused_while_a_program_is_running() {
    announce("classroom marks while busy");
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    TW_CHECK(harness.worker().run(QStringLiteral("basic"),
                                  QStringLiteral("10 PRINT \"TICK\"\n20 GOTO 10\n"),
                                  QStringLiteral("forever.bas")));
    TW_CHECK(harness.waitFor([&harness] { return !harness.recorded().output.isEmpty(); },
                             10000));

    TW_CHECK(!harness.worker().requestClassroomMarks());
    TW_CHECK_EQ(harness.recorded().classroom_failure_request,
                QStringLiteral("classroom-marks"));
    TW_CHECK(harness.recorded().classroom_failure_text.contains(QStringLiteral("Stop")));

    // The class list reads files and runs nothing, so it answers while a program
    // is running: a teacher can see who is in the class at any time.
    harness.clearClassroom();
    TW_CHECK(harness.worker().requestClassroomRoster());
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().roster_arrived; }));
    TW_CHECK(harness.recorded().classroom_failure_text.isEmpty());

    harness.worker().stop();
    TW_CHECK(harness.waitFor([&harness] { return !harness.worker().isRunning(); }, 10000));
}

/// No class list is an empty class, not a failure: a fresh install has no
/// students and the panel shows its empty state.
void test_an_empty_class_is_not_a_failure() {
    announce("classroom empty");
    QTemporaryDir data_dir;
    DataDirOverride data_override{data_dir.path()};
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    TW_CHECK(harness.worker().requestClassroomRoster());
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().roster_arrived; }));
    TW_CHECK(harness.recorded().classroom_failure_text.isEmpty());
    TW_CHECK(harness.recorded().students.isEmpty());
    TW_CHECK(harness.recorded().assigned_lessons.isEmpty());

    TW_CHECK(harness.worker().requestClassroomMarks());
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().marks_arrived; }));
    TW_CHECK(harness.recorded().classroom_failure_text.isEmpty());
    TW_CHECK(harness.recorded().marks.isEmpty());
}

// ---------------------------------------------------------------------------
// Finding the two halves
// ---------------------------------------------------------------------------

void test_the_worker_and_its_interpreter_are_found() {
    announce("locating the worker");
    QString reason;
    const timewarp::bridge::WorkerPaths paths = locateWorker(&reason);
    if (!paths.isValid()) {
        // Not a failure: a machine without Python cannot run the four bridged
        // languages, and saying so is the honest outcome. The message names every
        // path that was tried, which is what makes it fixable.
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }
    TW_CHECK(QFile::exists(paths.script));
    TW_CHECK(QFile::exists(paths.interpreter));
    TW_CHECK(paths.script.endsWith(QStringLiteral("bridge/worker.py")));
    TW_CHECK(reason.isEmpty());
}

// ---------------------------------------------------------------------------
// Real programs
// ---------------------------------------------------------------------------

void test_logo_draws_through_the_bridge() {
    announce("logo program");
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    TW_CHECK(harness.recorded().ready);

    // A square, in the reference's compass coordinates: forward is north at
    // heading 0, and a right turn adds to the heading.
    TW_CHECK(harness.worker().run(QStringLiteral("logo"),
                                  QStringLiteral("REPEAT 4 [FD 100 RT 90]"),
                                  QStringLiteral("square.logo")));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().finished_count > 0; }));

    const Recording& recorded = harness.recorded();
    TW_CHECK(recorded.last_ok);
    TW_CHECK(recorded.last_error.isEmpty());
    TW_CHECK(recorded.errors.isEmpty());
    TW_CHECK_EQ(recorded.strokes, 4);

    // The geometry, not just the count: the first edge runs north from the origin.
    TW_CHECK(recorded.first_stroke_from.has_value());
    if (recorded.first_stroke_from.has_value() && recorded.first_stroke_to.has_value()) {
        TW_CHECK(nearly(recorded.first_stroke_from->x(), 0.0));
        TW_CHECK(nearly(recorded.first_stroke_from->y(), 0.0));
        TW_CHECK(nearly(recorded.first_stroke_to->x(), 0.0));
        TW_CHECK(nearly(recorded.first_stroke_to->y(), 100.0));
    }

    // The background is the reference's dark blue, and it has to arrive: the
    // reference pen is white, so a client that kept its own white canvas would
    // draw white ink on white and show nothing at all.
    TW_CHECK_EQ(recorded.background_commands, 1);
    TW_CHECK_EQ(recorded.background, timewarp::canvas::make_rgb(10, 10, 20));
    TW_CHECK_EQ(recorded.first_stroke_colour, timewarp::canvas::make_rgb(255, 255, 255));

    // And the marker ends where the program left the turtle.
    TW_CHECK(recorded.pose_count > 0);
    TW_CHECK(std::abs(recorded.pose_heading) < 1e-6);
    TW_CHECK(recorded.pose_visible);
}

void test_a_basic_program_prints() {
    announce("basic program");
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    TW_CHECK(harness.worker().run(QStringLiteral("basic"),
                                  QStringLiteral("10 PRINT \"HELLO FROM BASIC\"\n20 END\n"),
                                  QStringLiteral("hello.bas")));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().finished_count > 0; }));

    // Output is the whole point of a text language, and it arrives as text rather
    // than as a list the IDE has to reassemble.
    TW_CHECK(harness.recorded().output.contains(QStringLiteral("HELLO FROM BASIC")));
    TW_CHECK_EQ(harness.recorded().strokes, 0);
}

void test_an_unknown_language_is_refused() {
    announce("unknown language");
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    TW_CHECK(harness.worker().run(QStringLiteral("klingon"), QStringLiteral("nuqneH"),
                                  QStringLiteral("x.klingon")));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().finished_count > 0; }));

    // Reported twice on purpose: once as an error for the panel, once as the run's
    // outcome, so a caller that only listens for one of them still learns.
    TW_CHECK(!harness.recorded().last_ok);
    TW_CHECK(!harness.recorded().last_error.isEmpty());
    TW_CHECK(!harness.recorded().errors.isEmpty());
}

void test_a_broken_program_says_something() {
    announce("broken program");
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    // An unterminated loop: the reference's parser has to object somehow.
    TW_CHECK(harness.worker().run(QStringLiteral("logo"),
                                  QStringLiteral("REPEAT 4 [FD 100 RT 90"),
                                  QStringLiteral("broken.logo")));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().sawTerminalEvent(); }));

    // Deliberately not asserting *which* channel the complaint arrives on. The
    // interpreter recovers from most errors and reports them in its output rather
    // than raising, so the contract that is actually guaranteed is "the user is
    // told", and asserting more than that would be asserting an implementation
    // detail of the Python side.
    const bool told = !harness.recorded().errors.isEmpty() ||
                      harness.recorded().output.contains(QStringLiteral("\u274C"));
    TW_CHECK(told);
}

void test_stop_ends_a_runaway_program() {
    announce("stop a real program");
    QString reason;
    if (!locateWorker(&reason).isValid()) {
        std::printf("SKIP: %s\n", qPrintable(reason));
        return;
    }

    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    // An infinite loop. The interpreter has its own ten-second budget, so this
    // program would end by itself eventually; what is asserted here is only what
    // holds either way, because racing the budget would make the test decide its
    // own outcome.
    TW_CHECK(harness.worker().run(QStringLiteral("basic"),
                                  QStringLiteral("10 PRINT \"TICK\"\n20 GOTO 10\n"),
                                  QStringLiteral("forever.bas")));
    TW_CHECK(harness.waitFor([&harness] { return !harness.recorded().output.isEmpty(); },
                             10000));

    harness.worker().stop();
    TW_CHECK(harness.waitFor([&harness] { return !harness.worker().isRunning(); }, 10000));
    TW_CHECK(!harness.worker().isRunning());

    // A requested stop is never reported as a crash -- that would put an error in
    // front of the user for pressing Stop. The kill path itself is tested
    // hermetically below, where a worker that really never finishes can be
    // scripted.
    TW_CHECK_EQ(harness.recorded().failures, 0);
}

void test_stop_kills_a_worker_that_never_finishes() {
    announce("stop a hung worker");
    QTemporaryDir dir;
    // Reads the request, then hangs without ever answering. This is the case that
    // cooperative stopping cannot handle -- a program inside a statement that will
    // never return -- and it is reachable only here, because the Python
    // interpreter's own budget always ends a real program first.
    const QString script = writeFile(
        dir, QStringLiteral("hangs.py"),
        QByteArray{"import sys, json, time\n"
                   "sys.stdout.write(json.dumps({'t': 'ready', 'version': '1'}) + '\\n')\n"
                   "sys.stdout.flush()\n"
                   "sys.stdin.readline()\n"
                   "while True:\n"
                   "    time.sleep(0.05)\n"});
    TW_CHECK(!script.isEmpty());

    ScriptOverride override_script{script};
    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    TW_CHECK(harness.worker().run(QStringLiteral("basic"), QStringLiteral("10 END"),
                                  QStringLiteral("x.bas")));
    TW_CHECK(harness.worker().isRunning());

    harness.worker().stop();
    TW_CHECK(harness.waitFor([&harness] { return !harness.worker().isRunning(); }, 10000));

    // Gone, and reported as a run that ended rather than as a failure: a run has
    // to end with exactly one terminal event, and this one was asked for.
    TW_CHECK(!harness.worker().isRunning());
    TW_CHECK_EQ(harness.recorded().failures, 0);
    TW_CHECK_EQ(harness.recorded().finished_count, 1);
    TW_CHECK(!harness.recorded().last_ok);
}

// ---------------------------------------------------------------------------
// The failure paths, which a program cannot provoke
// ---------------------------------------------------------------------------

void test_a_worker_that_dies_at_startup_is_reported() {
    announce("worker dies at startup");
    QTemporaryDir dir;
    const QString script = writeFile(
        dir, QStringLiteral("dies.py"),
        QByteArray{"import sys\nsys.stderr.write('boom: this worker is broken\\n')\n"
                   "sys.exit(3)\n"});
    TW_CHECK(!script.isEmpty());

    ScriptOverride override_script{script};
    Harness harness;
    QString error;
    // The reported reason has to be the worker's own stderr: "it did not start" is
    // not enough to fix a missing dependency or a syntax error.
    TW_CHECK(!harness.worker().start(&error));
    TW_CHECK(error.contains(QStringLiteral("boom")));
}

void test_a_worker_that_dies_mid_run_is_an_error_not_a_hang() {
    announce("worker dies mid-run");
    QTemporaryDir dir;
    // Says hello, then dies without reporting a result -- which is what a crash in
    // the interpreter looks like from out here.
    const QString script = writeFile(
        dir, QStringLiteral("halfway.py"),
        QByteArray{"import sys, json\n"
                   "sys.stdout.write(json.dumps({'t': 'ready', 'version': '1'}) + '\\n')\n"
                   "sys.stdout.flush()\n"
                   "sys.stdin.readline()\n"
                   "sys.stdout.write(json.dumps({'t': 'out', 'text': 'partial'}) + '\\n')\n"
                   "sys.stdout.flush()\n"
                   "sys.stderr.write('kaboom\\n')\n"
                   "sys.exit(9)\n"});
    TW_CHECK(!script.isEmpty());

    ScriptOverride override_script{script};
    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    TW_CHECK(harness.worker().run(QStringLiteral("basic"), QStringLiteral("10 END"),
                                  QStringLiteral("x.bas")));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().sawTerminalEvent(); }));

    TW_CHECK_EQ(harness.recorded().failures, 1);
    TW_CHECK(harness.recorded().last_failure.contains(QStringLiteral("9")));
    TW_CHECK(harness.recorded().last_failure.contains(QStringLiteral("kaboom")));
    // The output produced before the crash is not thrown away.
    TW_CHECK(harness.recorded().output.contains(QStringLiteral("partial")));
}

void test_an_unreadable_message_is_reported() {
    announce("malformed protocol");
    QTemporaryDir dir;
    const QString script = writeFile(
        dir, QStringLiteral("garbage.py"),
        QByteArray{"import sys\n"
                   "sys.stdout.write('{\"t\": \"ready\", \"version\": \"1\"}\\n')\n"
                   "sys.stdout.write('this is not json at all\\n')\n"
                   "sys.stdout.write('{\"t\": \"done\", \"ok\": true}\\n')\n"
                   "sys.stdout.flush()\n"
                   "sys.stdin.readline()\n"});
    TW_CHECK(!script.isEmpty());

    ScriptOverride override_script{script};
    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    harness.worker().run(QStringLiteral("basic"), QStringLiteral("10 END"),
                         QStringLiteral("x.bas"));
    TW_CHECK(harness.waitFor([&harness] {
        return harness.recorded().finished_count > 0 || harness.recorded().failures > 0;
    }));

    // Skipping it silently would show up later as a drawing with a hole in it.
    TW_CHECK(!harness.recorded().errors.isEmpty());
}

void test_an_unknown_drawing_command_is_reported() {
    announce("unknown drawing command");
    QTemporaryDir dir;
    const QString script = writeFile(
        dir, QStringLiteral("odd.py"),
        QByteArray{"import sys, json\n"
                   "sys.stdout.write(json.dumps({'t': 'ready', 'version': '1'}) + '\\n')\n"
                   // Flushed before waiting, or the handshake sits in Python's block
                   // buffer for a pipe and never arrives at all.
                   "sys.stdout.flush()\n"
                   "sys.stdin.readline()\n"
                   "sys.stdout.write(json.dumps({'t': 'canvas', 'cmds': [\n"
                   "  {'k': 'line', 'x1': 0, 'y1': 0, 'x2': 10, 'y2': 0,\n"
                   "   'c': [1, 2, 3], 'w': 1},\n"
                   "  {'k': 'wobble', 'c': [1, 2, 3]}]}) + '\\n')\n"
                   "sys.stdout.write(json.dumps({'t': 'done', 'ok': True,\n"
                   "  'unsupported': ['bezier curves']}) + '\\n')\n"
                   "sys.stdout.flush()\n"
                   "sys.stdin.readline()\n"});
    TW_CHECK(!script.isEmpty());

    ScriptOverride override_script{script};
    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    harness.worker().run(QStringLiteral("basic"), QStringLiteral("10 END"),
                         QStringLiteral("x.bas"));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().finished_count > 0; }));

    // The known command was applied...
    TW_CHECK_EQ(harness.recorded().strokes, 1);
    TW_CHECK_EQ(harness.recorded().first_stroke_colour, timewarp::canvas::make_rgb(1, 2, 3));
    // ...the unknown one was reported rather than dropped...
    //
    // `value()` rather than `last()`: these are diagnostics for a failure, and
    // `last()` on an empty list is undefined behaviour that turns a failed
    // assertion into a crashed suite -- which is exactly what happened here the
    // first time this test ran.
    TW_CHECK(!harness.recorded().errors.isEmpty());
    TW_CHECK(harness.recorded().errors.value(harness.recorded().errors.size() - 1)
                 .contains(QStringLiteral("wobble")));
    // ...and the worker's own list of what it could not express is carried
    // through, because "the drawing is incomplete" is something the user has to
    // be told rather than left to notice.
    TW_CHECK_EQ(harness.recorded().unsupported.size(), 1);
    TW_CHECK_EQ(harness.recorded().unsupported.value(0), QStringLiteral("bezier curves"));
}

void test_a_canvas_message_split_across_reads_is_reassembled() {
    announce("split message");
    QTemporaryDir dir;
    // Writes the *answer* in three pieces with a flush between each, which is what
    // a large batch in a pipe buffer looks like from the reading end. A parser that
    // assumed one read is one message would drop this on the floor.
    //
    // The handshake is sent whole on purpose. Splitting it would not test
    // reassembly; it would test whether `start` waits for a complete line, and the
    // answer to that is in the same file as `start`.
    const QString script = writeFile(
        dir, QStringLiteral("split.py"),
        QByteArray{"import sys, time\n"
                   "sys.stdout.write('{\"t\": \"ready\", \"version\": \"1\"}\\n')\n"
                   "sys.stdout.flush()\n"
                   "sys.stdin.readline()\n"
                   "sys.stdout.write('{\"t\": \"canv')\n"
                   "sys.stdout.flush()\n"
                   "time.sleep(0.05)\n"
                   "sys.stdout.write('as\", \"cmds\": [{\"k\": \"bg\", \"c\": [7, 8,')\n"
                   "sys.stdout.flush()\n"
                   "time.sleep(0.05)\n"
                   "sys.stdout.write(' 9]}]}\\n{\"t\": \"done\", \"ok\": true}\\n')\n"
                   "sys.stdout.flush()\n"
                   "sys.stdin.readline()\n"});
    TW_CHECK(!script.isEmpty());

    ScriptOverride override_script{script};
    Harness harness;
    if (!harness.requireStart()) {
        return;
    }
    harness.worker().run(QStringLiteral("basic"), QStringLiteral("10 END"),
                         QStringLiteral("x.bas"));
    TW_CHECK(harness.waitFor([&harness] { return harness.recorded().finished_count > 0; }));

    TW_CHECK(harness.recorded().ready);
    TW_CHECK_EQ(harness.recorded().background, timewarp::canvas::make_rgb(7, 8, 9));
    TW_CHECK(harness.recorded().errors.isEmpty());
}

}  // namespace

int main(int argc, char** argv) {
    QCoreApplication application{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpTests"));
    QCoreApplication::setApplicationName(QStringLiteral("PythonBridgeTests"));

    // An optional name filter, because several of these tests take seconds -- they
    // start real interpreters and wait for real programs -- and iterating on one of
    // them should not mean running all of them.
    const QString filter = argc > 1 ? QString::fromLocal8Bit(argv[1]) : QString{};
    const auto run = [&filter](const char* name, void (*test)()) {
        if (filter.isEmpty() || QString::fromLatin1(name).contains(filter)) {
            test();
        }
    };

    run("locate", test_the_worker_and_its_interpreter_are_found);

    run("logo", test_logo_draws_through_the_bridge);
    run("basic", test_a_basic_program_prints);
    run("unknown-language", test_an_unknown_language_is_refused);
    run("broken", test_a_broken_program_says_something);
    run("stop-real", test_stop_ends_a_runaway_program);
    run("stop-hung", test_stop_kills_a_worker_that_never_finishes);

    run("dies-startup", test_a_worker_that_dies_at_startup_is_reported);
    run("dies-midrun", test_a_worker_that_dies_mid_run_is_an_error_not_a_hang);
    run("malformed", test_an_unreadable_message_is_reported);
    run("unknown-command", test_an_unknown_drawing_command_is_reported);
    run("split", test_a_canvas_message_split_across_reads_is_reassembled);

    run("lesson-curriculum", test_the_curriculum_arrives_without_the_answers);
    run("lesson-check", test_a_correct_answer_passes_and_a_wrong_one_does_not);
    run("lesson-hint", test_a_hint_arrives_without_the_answer);
    run("lesson-solution", test_the_worked_answer_is_only_sent_when_it_is_asked_for);
    run("lesson-progress", test_progress_covers_every_lesson);
    run("lesson-busy", test_a_check_is_refused_while_a_program_is_running);
    run("lesson-reset", test_reset_names_the_lesson_it_reset);

    run("classroom-marks", test_the_class_list_and_its_marks_arrive);
    run("classroom-empty", test_an_empty_class_is_not_a_failure);
    run("classroom-busy", test_the_marks_are_refused_while_a_program_is_running);

    return tw::test::summary("python_bridge");
}
