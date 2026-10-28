// SPDX-License-Identifier: MIT
//
// The teacher's side of the lessons: a roster, an assignment, marking and bundles.
//
// Everything a class owns is one directory (`~/.time_warp/classroom` by default),
// plain enough to read, zip and hand to somebody else:
//
//     classroom/
//       roster.json                 the students, and where their work lives
//       assignment.json             what was set most recently
//       presentation.json           the size a lesson is projected at
//       students/<student-id>/      one directory per learner
//         basic_first_steps-1.bas   work handed out, and handed back
//         progress.json             that learner's place in the curriculum
//
// A *bundle* is that directory zipped, so a class on a locked-down network can be
// set up from a file rather than a shared drive. This is the same format the
// reference's `teach.py` writes and reads, so a class made before this port still
// opens, and one made here still opens there.
//
// #### Marking is the learner's verifier
//
// `mark()` calls `verify_checkpoint`, which is what the Lessons panel and
// `time_warp_learn` call. A teacher's mark and a learner's green tick cannot
// disagree, because there is one function.
//
// #### What marking deliberately does not touch
//
// Nobody's progress. A teacher collecting a class must not move a learner's place
// in the curriculum -- or write into their own.

#pragma once

#include <string>
#include <vector>

#include <QString>
#include <QStringList>

#include "timewarp/lang/language.hpp"
#include "timewarp/lessons/curriculum.hpp"

namespace timewarp::lessons {

/// One learner, and where their work is kept.
struct Student {
    QString id;
    QString name;
    /// Empty means `<root>/students/<id>`; a shared drive is an absolute path.
    QString workDir;
};

/// What has been set, and when.
struct Assignment {
    QStringList lessonIds;
    QString note;
    QString created;
};

/// One handed-in file and whether it passes its own checkpoint.
struct Submission {
    QString studentId;
    QString lessonId;
    int checkpoint = 0;
    QString path;
    bool passed = false;
    QString message;
};

/// What a learner's hand-in produced: where the file went, whether an earlier one
/// was replaced, and whether one was in the way.
struct HandIn {
    bool ok = false;
    QString path;
    bool replaced = false;
    /// The path already held a hand-in and `replace` was not given, so nothing was
    /// written. Whoever asked can say so and offer to replace it.
    bool exists = false;
    /// A sentence to show when `ok` is false.
    QString error;
};

/// One line per learner per lesson: how much was handed in, how much passed.
struct MarkRow {
    QString student;
    QString lesson;
    QString title;
    int handedIn = 0;
    int checkpoints = 0;
    int passed = 0;
    bool complete = false;
};

/// One file to mark, with everything the verifier needs.
///
/// The point of this type is that marking can run off the GUI thread without
/// touching the class: a learner's file is executed, and a runaway one takes its
/// whole budget, so marking a class on the GUI thread freezes the window.
struct MarkJob {
    QString studentId;
    QString lessonId;
    QString lessonTitle;
    int checkpoint = 0;
    int checkpoints = 0;
    QString path;
    /// Read on the caller's thread: the file may change while a mark is running,
    /// and a mark must be taken from the bytes that were handed in.
    QString source;
    Checkpoint expectation;
    lang::Language language = lang::Language::Basic;
};

/// One line per learner: how far through the curriculum they are.
struct ClassProgressRow {
    QString id;
    QString name;
    int completed = 0;
    int started = 0;
    int lessons = 0;
    int hintsUsed = 0;
};

/// How a lesson is shown when it is taught from the front.
struct PresentationSettings {
    static constexpr int kMinFont = 12;
    static constexpr int kMaxFont = 48;
    static constexpr int kDefaultFont = 18;
    int fontSize = kDefaultFont;
    bool fullscreen = true;
};

class Classroom {
public:
    /// `root` defaults to `$TIME_WARP_DATA_DIR/classroom`, or `~/.time_warp/classroom`.
    explicit Classroom(QString root = {});

    /// `$TIME_WARP_DATA_DIR/classroom`, or `~/.time_warp/classroom`.
    [[nodiscard]] static QString default_root();

    [[nodiscard]] const QString& root() const noexcept { return root_; }

    /// Reads the roster, the assignment and the presentation settings, and the
    /// curriculum they refer to. A missing file is an empty class, not an error:
    /// the first term starts with none of them.
    [[nodiscard]] bool load(QString* error = nullptr);

    [[nodiscard]] const std::vector<Lesson>& lessons() const noexcept { return lessons_; }
    [[nodiscard]] const std::vector<Student>& students() const noexcept { return students_; }
    [[nodiscard]] const Assignment& assignment() const noexcept { return assignment_; }
    [[nodiscard]] const PresentationSettings& presentation() const noexcept {
        return presentation_;
    }

    /// Registers a learner. The id defaults to a slug of the name; a duplicate id
    /// is refused rather than silently merged.
    [[nodiscard]] bool addStudent(const QString& name, const QString& id = {},
                                  const QString& workDir = {}, QString* error = nullptr);
    /// Forgets a learner. Their directory is left alone: removing somebody from a
    /// class list must not delete an afternoon's work.
    bool removeStudent(const QString& id);
    [[nodiscard]] QString studentDirectory(const QString& id) const;

    /// Records what the class has been set, refusing lessons that do not exist.
    [[nodiscard]] bool assign(const QStringList& lessonIds, const QString& note,
                              QString* error = nullptr);

    /// The file one learner works in for one checkpoint.
    [[nodiscard]] QString handoutName(const Lesson& lesson, int index) const;
    [[nodiscard]] QStringList handoutNames(const Lesson& lesson) const;

    /// Writes each assigned lesson's **starter** into every learner's directory.
    /// Existing work is never overwritten, and the returned paths are what was
    /// written.
    [[nodiscard]] QStringList distribute(const QStringList& lessonIds = {},
                                         QString* error = nullptr);

    /// Finds every handed-out file and marks it against its checkpoint. A
    /// checkpoint with no file produces no submission, so "not handed in" and
    /// "handed in and wrong" stay different things.
    ///
    /// `collect` is `markAll(markingJobs(...))`; the two halves exist separately
    /// so the expensive half can run on a worker thread with data it owns.
    [[nodiscard]] QList<Submission> collect(const QStringList& lessonIds = {});

    /// The learner's half: writes what they produced where `collect()` will find it.
    ///
    /// The name is the handout name, because that is the contract the marking side
    /// reads -- a learner who saved their work as `mine.bas` has not handed in a
    /// checkpoint, they have saved a file. `replace` is the learner's decision: an
    /// earlier hand-in is never overwritten by accident, because it may be the
    /// version they meant to keep.
    [[nodiscard]] HandIn handIn(const QString& studentId, const QString& lessonId,
                                int checkpoint, const QString& source,
                                bool replace = false);
    [[nodiscard]] QList<MarkJob> markingJobs(const QStringList& lessonIds = {}) const;
    [[nodiscard]] static QList<Submission> markAll(const QList<MarkJob>& jobs);
    [[nodiscard]] static QList<MarkRow> marksFor(const QList<MarkJob>& jobs,
                                                 const QList<Submission>& submissions);

    /// Rolls collected files up into one line per learner per lesson.
    [[nodiscard]] QList<MarkRow> lessonMarks(const QList<Submission>& submissions) const;

    /// One row per learner: lessons finished, started, and hints used.
    [[nodiscard]] QList<ClassProgressRow> classProgress() const;

    /// Zips the whole class: roster, assignment, presentation and every learner's
    /// work. Learners who have not started are included -- a bundle is also how a
    /// class is *set up* on another machine.
    ///
    /// A learner whose `work_dir` is outside the classroom is listed in the roster
    /// but their files are not in the archive: there is no name inside the zip for
    /// them. Their work is still on the shared drive the roster names.
    [[nodiscard]] bool createBundle(const QString& outputPath, QString* error = nullptr) const;
    /// Unpacks a bundle. `replace` decides whether existing files are overwritten;
    /// setting up a class is additive, refreshing one is authoritative.
    [[nodiscard]] bool importBundle(const QString& bundlePath, bool replace,
                                    QString* error = nullptr);

    void setPresentation(int fontSize, bool fullscreen);

private:
    [[nodiscard]] const Lesson* find(const QString& id) const;
    [[nodiscard]] QString safeName(const QString& text) const;
    [[nodiscard]] bool saveRoster(QString* error = nullptr) const;
    [[nodiscard]] bool saveAssignment(QString* error = nullptr) const;
    [[nodiscard]] bool savePresentation(QString* error = nullptr) const;
    void readAll();

    QString root_;
    std::vector<Lesson> lessons_;
    std::vector<Student> students_;
    Assignment assignment_;
    PresentationSettings presentation_;
};

}  // namespace timewarp::lessons
