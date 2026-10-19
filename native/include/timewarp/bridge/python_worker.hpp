// SPDX-License-Identifier: MIT
//
// The IDE's end of the Python bridge: it starts `time_warp_worker.py`, hands it a
// program, and turns the stream of JSON lines it answers with into canvas
// commands and Qt signals.
//
// #### Why a separate process
//
// Five of the six languages have no C++ interpreter yet. The alternative to this
// is reimplementing them, and the Python implementations are complete and tested;
// the alternative *inside* one process is embedding CPython, which turns a
// runaway program into a runaway IDE. Out of process, the worst case is a process
// to kill.
//
// #### What this class is careful about
//
//  * **A worker that dies is an error, not a hang.** If the process exits without
//    a `done` message, that is reported as a failure with the stderr text rather
//    than leaving a run that never finishes.
//  * **A message can be split across reads.** `QProcess::readyRead` says nothing
//    about message boundaries, so partial lines are buffered until their newline
//    arrives.
//  * **A stray `print` cannot corrupt the protocol.** The worker redirects its own
//    stdout for exactly that reason; stderr is captured separately and only ever
//    shown as diagnostics.

#pragma once

#include <cstdint>

#include <QByteArray>
#include <QList>
#include <QMetaType>
#include <QObject>
#include <QString>
#include <QStringList>

class QJsonObject;
class QProcess;

namespace timewarp::canvas {
class CanvasCommandQueue;
}

namespace timewarp::bridge {

/// Where the worker and its interpreter were found.
struct WorkerPaths {
    QString interpreter;
    QString script;

    /// True when both were located and exist.
    [[nodiscard]] bool isValid() const { return !interpreter.isEmpty() && !script.isEmpty(); }
};

/// One checkpoint of a lesson, as the lesson panel shows it.
///
/// Deliberately without the worked answer: this is what every client is sent when
/// it opens the panel, and a lesson whose answer travels with its questions has
/// been spoiled before it is read. `PythonWorker::requestSolution` asks for it.
struct LessonCheckpoint {
    QString title;
    QString description;
    QString starter;      ///< The program the learner begins from.
    QString expectation;  ///< "lines", "turtle" or "error", for the panel's wording.
    QStringList hints;
};

/// A lesson: the learner's view of one piece of the curriculum.
struct LessonSummary {
    QString id;
    QString title;
    QString language;     ///< Display name, e.g. "CHIP-8".
    QString languageKey;  ///< Settings key, e.g. "chip8": what a document records.
    QString difficulty;
    QString description;
    QStringList objectives;
    QStringList prerequisites;
    QList<LessonCheckpoint> checkpoints;

    [[nodiscard]] int checkpointCount() const { return static_cast<int>(checkpoints.size()); }
};

/// Where a learner has got to in one lesson.
struct LessonProgress {
    QString id;
    QString title;
    QString language;
    QString difficulty;
    QString status;       ///< "not_started", "in_progress" or "completed".
    int checkpoint = 0;   ///< The first unfinished checkpoint.
    int checkpoints = 0;  ///< How many the lesson has.
    int hintsUsed = 0;
};

/// One learner on the class list, as `classroom_mode.py` records them.
struct ClassroomStudent {
    QString id;
    QString name;
    /// The folder their work is in. Reported rather than derived here: a student
    /// with a `work_dir` outside the class root is a fact only the engine knows.
    QString directory;
};

/// How a class is shown when it is being taught from the front.
///
/// Read from `presentation.json`, which `teach.py present` writes. `fontSize` is
/// zero when the worker did not say, which is what an older worker looks like.
struct ClassroomPresentation {
    int fontSize = 0;
    bool fullscreen = true;
};

/// One row of the mark sheet: a learner, a lesson, and how many of its checkpoints
/// their handed-in files passed.
struct ClassroomMark {
    QString student;
    QString lesson;
    QString title;
    int handedIn = 0;
    int checkpoints = 0;
    int passed = 0;
    bool complete = false;
};

/// One row of the variables table.
///
/// The engine decides all three fields, including how the value reads: a value
/// rendered in C++ would be a second opinion about a fact the engine already
/// stated, and the two would disagree exactly where it mattered (a BASIC number
/// that is stored as a float).
struct VariableRow {
    QString name;
    QString type;
    QString value;
};

/// One labelled value in the telemetry HUD.
struct TelemetryRow {
    QString label;
    QString value;
};

/// Locates the Python interpreter and the worker script.
///
/// The search order is explicit rather than clever, because "it works on my
/// machine" for a cross-process launcher means "the paths happen to line up
/// here":
///
///   1. `TIME_WARP_PYTHON` and `TIME_WARP_WORKER` override everything.
///   2. The script beside the executable, then two levels up (the build tree
///      layout: `native/build/time_warp_ide` next to `native/bridge/worker.py`),
///      then the working directory.
///   3. The interpreter from the repository's `.venv` next to the script, then
///      `python3` and `python` from `PATH`.
///
/// `reason` is set to a sentence naming what was searched for, for the error
/// panel: a missing bridge has to say which half is missing.
[[nodiscard]] WorkerPaths locateWorker(QString* reason = nullptr);

/// Runs one program at a time in a Python worker process.
///
/// Every method must be called from the thread that owns the object, which is the
/// GUI thread in the application and the test thread in the tests. The
/// interpreter is not consulted for anything: it is a pipe and a parser.
class PythonWorker final : public QObject {
    Q_OBJECT

public:
    /// `queue` receives the drawing. It is pushed to directly rather than through
    /// a signal so a batch costs one call instead of one queued invocation per
    /// command.
    explicit PythonWorker(timewarp::canvas::CanvasCommandQueue& queue,
                          QObject* parent = nullptr);
    ~PythonWorker() override;

    /// Starts the process if it is not already running, and waits for the worker's
    /// `ready` handshake. Returns false and sets `error` when the interpreter or
    /// the script cannot be found, or the process will not start.
    bool start(QString* error);

    /// Sends a program to run. The worker must be started. The source is carried
    /// as a JSON string, so any program text is safe.
    bool run(const QString& language, const QString& source, const QString& name);

    /// Asks the worker to stop, then kills it if it has not exited within a short
    /// grace period. Safe when nothing is running.
    void stop();

    /// Asks for the curriculum. Answers with `lessonsReceived`.
    bool requestLessons();

    /// Asks where the learner has got to. Answers with `lessonProgressReceived`.
    bool requestLessonProgress();

    /// Asks for the next hint for a checkpoint. Answers with `lessonHintReceived`.
    bool requestHint(const QString& lesson, int checkpoint);

    /// Asks for the worked answer to a checkpoint, on purpose.
    ///
    /// A separate request because the answer is not part of the curriculum the
    /// panel is given: a learner who has decided they need it can ask, and one who
    /// has not is not shown it.
    bool requestSolution(const QString& lesson, int checkpoint);

    /// Checks the learner's source against one checkpoint. Answers with
    /// `lessonChecked`.
    ///
    /// The check runs **in the worker's engine**, which is the same engine that
    /// would run the program, so the panel's verdict and the CLI's verdict can
    /// never disagree. Refused while a program is running, because that would be
    /// two interpreters at once.
    bool checkLesson(const QString& lesson, int checkpoint, const QString& source);

    /// Forgets the learner's place in one lesson. Answers with `lessonResetDone`.
    bool resetLesson(const QString& lesson);

    // -- classroom ---------------------------------------------------------

    /// Asks for the class list. Answers with `classroomRosterReceived`.
    ///
    /// The roster file belongs to `classroom_mode.py`, which `teach.py` also
    /// writes; the panel reads it through this request rather than parsing it, so
    /// there is one reader of the format.
    bool requestClassroomRoster();
    /// Asks for every handed-in file, marked. Answers with `classroomMarksReceived`.
    bool requestClassroomMarks();
    /// Asks for the size and window style a class is taught at. Answers with
    /// `classroomPresentationReceived`.
    ///
    /// Its own request rather than a field on the roster: presentation mode is
    /// entered without opening the classroom page, and it should not have to walk
    /// every learner's folder to learn a font size.
    bool requestClassroomPresentation();

    /// True when a lesson request has been sent and no answer has arrived.
    [[nodiscard]] bool isWaitingForLesson() const noexcept { return waiting_for_lesson_; }

    /// True while the process is alive.
    [[nodiscard]] bool isRunning() const;

    /// True while a program has been sent and its result has not arrived.
    ///
    /// Distinct from `isRunning`, which stays true while the worker sits idle
    /// between runs -- deliberately, because a Python start costs about 150ms and
    /// reusing the process across runs is what keeps Run feeling instant. Callers
    /// that mean "a program is executing" have to ask this one, or they will claim
    /// to stop something that is not running and needlessly restart the worker.
    [[nodiscard]] bool isRunningProgram() const noexcept { return running_program_; }

    /// Answers a pending input request.
    void answerInput(const QString& text);

    /// The paths this instance was started with, for diagnostics.
    [[nodiscard]] const WorkerPaths& paths() const noexcept { return paths_; }

signals:
    /// The worker's handshake. A run before this is accepted is a protocol error.
    void ready();
    /// One line of program output, newline included.
    void outputReceived(const QString& text);
    /// A batch of drawing commands has been pushed into the queue. The consumer
    /// drains on this rather than on a timer.
    void commandsQueued();
    /// The turtle moved. Carried as a signal rather than as a canvas command
    /// because painting the *marker* is not something the command stream expresses:
    /// its commands say what to draw, not where the turtle ended up.
    void poseReceived(double x, double y, double heading, bool visible);
    /// The program is waiting for a line of input.
    void inputRequested(const QString& prompt);
    /// A failure the worker reported, for the error panel.
    void errorReceived(const QString& text);
    /// The run ended. `ok` is false for a program error, a stop, or a crash.
    void finished(bool ok, const QString& error, const QStringList& unsupported);
    /// The process could not start, or died mid-run. Distinct from `finished`
    /// because there is no program result to report.
    void failed(const QString& text);

    /// The curriculum arrived, in teaching order.
    ///
    /// `presentationFont` is the size a lesson is projected at, which the teacher
    /// sets through the classroom settings. It travels with the curriculum because
    /// it is a property of how the lessons are *shown*, and a panel that opened at
    /// its own idea of the right size would ignore the teacher who set one.
    void lessonsReceived(const QList<timewarp::bridge::LessonSummary>& lessons,
                         int presentationFont);
    /// The learner's place in every lesson, plus the overall count.
    void lessonProgressReceived(const QList<timewarp::bridge::LessonProgress>& rows,
                                int completed, int total);
    /// One hint. `number` is 1-based, so the panel can say which of how many.
    void lessonHintReceived(const QString& text, int number, int total);
    /// The worked answer to a checkpoint.
    void lessonSolutionReceived(const QString& text);
    /// A checkpoint was checked. `completed` is the particular lesson being
    /// finished; `finished`/`total` are the learner's overall standing.
    void lessonChecked(const QString& lesson, int checkpoint, bool passed,
                       const QString& message, bool completed, int finished, int total);
    /// A lesson's progress was forgotten.
    void lessonResetDone(const QString& lesson);
    /// A lesson request failed. `request` names which, because the panel may have
    /// several in flight and one of them failing is not the others failing.
    void lessonFailed(const QString& request, const QString& error);

    /// The class list, its assignment, and where each learner's folder is.
    void classroomRosterReceived(const QList<timewarp::bridge::ClassroomStudent>& students,
                                 const QStringList& assignedLessons, const QString& note);
    /// The mark sheet: one row per learner per lesson.
    void classroomMarksReceived(const QList<timewarp::bridge::ClassroomMark>& rows);
    /// The size and window style a class is taught at. It rides on both the
    /// curriculum and the class list, because both are read from
    /// `presentation.json`; the last answer wins and they cannot disagree.
    void classroomPresentationReceived(
        const timewarp::bridge::ClassroomPresentation& settings);
    /// A classroom request failed, named as above.
    void classroomFailed(const QString& request, const QString& error);

    /// The finished run's variables, for the Variables tab.
    ///
    /// Emitted before `finished`, so anything reacting to "the run is over" sees
    /// what the run left behind rather than the previous run's table.
    void variablesReported(const QList<timewarp::bridge::VariableRow>& rows);
    /// The finished run's telemetry, for the HUD. Same ordering as above.
    void telemetryReported(const QList<timewarp::bridge::TelemetryRow>& rows);

private slots:
    void onReadyReadStandardOutput();
    void onReadyReadStandardError();
    void onProcessFinished(int exitCode, int status);

private:
    /// Sends one request object. False when the worker is not usable.
    bool sendRequest(const QJsonObject& message);
    /// Handles one complete protocol line.
    void handleMessage(const QByteArray& line);
    /// Applies one entry of a `canvas` batch to the queue.
    void applyCommand(const QJsonObject& command);
    /// Appends to the error stream text, for diagnostics on a crash.
    void noteStandardError(const QString& text);

    timewarp::canvas::CanvasCommandQueue& queue_;
    WorkerPaths paths_;
    QProcess* process_ = nullptr;
    QByteArray pending_stdout_;
    QString standard_error_;
    /// Set when `done` arrives, so a later process exit is not reported as a
    /// crash.
    bool saw_done_ = false;
    /// Set by the `ready` handshake, and checked by `start`: a worker that dies
    /// during start-up must fail there rather than swallow the first program.
    bool saw_ready_ = false;
    /// Set when the user asked for a stop, so the process exit that follows is
    /// reported as a stop rather than as a crash. Cleared when a run begins.
    bool stop_requested_ = false;
    bool running_program_ = false;
    /// Set while a lesson request is outstanding. Lesson requests are answered one
    /// at a time and never overlap a program run, so one flag is enough.
    bool waiting_for_lesson_ = false;
    std::uint64_t commands_pushed_ = 0;
    std::uint64_t batches_ = 0;
};

}  // namespace timewarp::bridge

// The lesson structures travel through signals, so they are declared as metatypes:
// a direct connection would work without this, and a queued one -- which is what a
// recorder in a test uses -- would not, and would fail at run time rather than at
// compile time.
Q_DECLARE_METATYPE(timewarp::bridge::LessonCheckpoint)
Q_DECLARE_METATYPE(timewarp::bridge::LessonSummary)
Q_DECLARE_METATYPE(timewarp::bridge::LessonProgress)
Q_DECLARE_METATYPE(QList<timewarp::bridge::LessonSummary>)
Q_DECLARE_METATYPE(timewarp::bridge::VariableRow)
Q_DECLARE_METATYPE(timewarp::bridge::TelemetryRow)
Q_DECLARE_METATYPE(QList<timewarp::bridge::VariableRow>)
Q_DECLARE_METATYPE(QList<timewarp::bridge::TelemetryRow>)
Q_DECLARE_METATYPE(QList<timewarp::bridge::LessonProgress>)
Q_DECLARE_METATYPE(timewarp::bridge::ClassroomStudent)
Q_DECLARE_METATYPE(timewarp::bridge::ClassroomMark)
Q_DECLARE_METATYPE(timewarp::bridge::ClassroomPresentation)
Q_DECLARE_METATYPE(QList<timewarp::bridge::ClassroomStudent>)
Q_DECLARE_METATYPE(QList<timewarp::bridge::ClassroomMark>)
