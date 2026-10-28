// SPDX-License-Identifier: MIT
//
// Tests for the classroom: the roster, handing work out, taking it in, marking,
// and the bundle that moves a class between machines.
//
// The cases that matter are the promises a teacher relies on:
//   * handing out never overwrites work a learner has started;
//   * "not handed in" and "handed in and wrong" stay different;
//   * marking does not move anybody's progress;
//   * a bundle round-trips, and importing it additively keeps existing files.

#include <cstdio>
#include <string>

#include <QCoreApplication>
#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <vector>

#include "check.hpp"
#include "temp_dir.hpp"
#include "timewarp/lessons/classroom.hpp"

namespace {

using timewarp::lessons::Classroom;
using timewarp::lessons::PresentationSettings;

void announce(const char* name) { std::printf("-- %s\n", name); }

const timewarp::lessons::Lesson* lesson_of(const Classroom& room, const char* id) {
    return timewarp::lessons::find_lesson(room.lessons(), id);
}

void test_a_class_starts_empty_and_assigns_a_real_lesson() {
    announce("roster and assignment");
    tw::test::TempDir directory;
    TW_CHECK(directory.valid());
    if (!directory.valid()) {
        return;
    }
    Classroom room{directory.path()};
    QString error;
    TW_CHECK(room.load(&error));
    TW_CHECK(room.students().empty());

    TW_CHECK(room.addStudent("Ada Lovelace", {}, {}, &error));
    TW_CHECK_EQ(room.students().size(), std::size_t{1});
    TW_CHECK_EQ(room.students().front().id, QStringLiteral("ada-lovelace"));
    // A duplicate id is refused rather than merged: two learners sharing a place
    // in the curriculum is an accident nobody notices until marking.
    TW_CHECK(!room.addStudent("Someone Else", "ada-lovelace", {}, &error));
    TW_CHECK(!error.isEmpty());

    TW_CHECK(room.assign({QStringLiteral("basic_first_steps")}, "week one", &error));
    TW_CHECK_EQ(room.assignment().lessonIds.size(), 1);
    // A lesson that does not exist is refused, not recorded and failed later.
    TW_CHECK(!room.assign({QStringLiteral("no_such_lesson")}, {}, &error));

    TW_CHECK(room.removeStudent("ada-lovelace"));
    TW_CHECK(room.students().empty());
}

void test_handing_out_writes_starters_and_never_overwrites() {
    announce("distribute");
    tw::test::TempDir directory;
    if (!directory.valid()) {
        return;
    }
    Classroom room{directory.path()};
    QString error;
    TW_CHECK(room.load(&error));
    TW_CHECK(room.addStudent("Ada", {}, {}, &error));
    TW_CHECK(room.assign({QStringLiteral("basic_first_steps")}, {}, &error));

    const QStringList first = room.distribute({}, &error);
    const timewarp::lessons::Lesson* lesson = lesson_of(room, "basic_first_steps");
    TW_CHECK(lesson != nullptr);
    if (lesson == nullptr) {
        return;
    }
    TW_CHECK_EQ(first.size(), static_cast<int>(lesson->checkpoints.size()));
    TW_CHECK(first.front().endsWith(QStringLiteral("-1.bas")));

    // The learner edits one file; a second distribution must leave it alone.
    QFile edited{first.front()};
    TW_CHECK(edited.open(QIODevice::WriteOnly | QIODevice::Truncate));
    edited.write("10 REM my own work\n");
    edited.close();
    const QStringList second = room.distribute({}, &error);
    TW_CHECK(second.isEmpty());
    QFile read_back{first.front()};
    TW_CHECK(read_back.open(QIODevice::ReadOnly));
    TW_CHECK_EQ(QString::fromUtf8(read_back.readAll()), QStringLiteral("10 REM my own work\n"));
}

void test_hand_in_writes_where_collect_looks_and_never_overwrites() {
    announce("hand in");
    tw::test::TempDir directory;
    if (!directory.valid()) {
        return;
    }
    Classroom room{directory.path()};
    QString error;
    TW_CHECK(room.load(&error));
    TW_CHECK(room.addStudent("Ada", {}, {}, &error));
    TW_CHECK(room.assign({QStringLiteral("basic_first_steps")}, {}, &error));

    const timewarp::lessons::Lesson* lesson = lesson_of(room, "basic_first_steps");
    TW_CHECK(lesson != nullptr);
    if (lesson == nullptr) {
        return;
    }

    // A learner's own bytes, handed in under the name the marking side reads.
    const auto first = room.handIn(QStringLiteral("ada"), QStringLiteral("basic_first_steps"), 0,
                                   QStringLiteral("10 REM handed in\n"));
    TW_CHECK(first.ok);
    TW_CHECK(first.path.endsWith(room.handoutName(*lesson, 0)));
    TW_CHECK(!first.replaced);

    // It is what `collect` sees, which is the promise that makes it a hand-in
    // rather than a save.
    const QList<timewarp::lessons::Submission> collected = room.collect({});
    bool found = false;
    for (const timewarp::lessons::Submission& submission : collected) {
        if (submission.path == first.path) {
            found = true;
        }
    }
    TW_CHECK(found);

    // A second hand-in is refused, because it may be the version they meant to keep.
    const auto second = room.handIn(QStringLiteral("ada"), QStringLiteral("basic_first_steps"),
                                    0, QStringLiteral("10 REM second try\n"));
    TW_CHECK(!second.ok);
    TW_CHECK(second.exists);
    TW_CHECK(second.path == first.path);
    TW_CHECK(second.error.contains(QStringLiteral("already handed in")));
    QFile unchanged{first.path};
    TW_CHECK(unchanged.open(QIODevice::ReadOnly));
    TW_CHECK_EQ(QString::fromUtf8(unchanged.readAll()), QStringLiteral("10 REM handed in\n"));
    unchanged.close();

    // And replacing is the learner's decision, said out loud.
    const auto replaced = room.handIn(QStringLiteral("ada"), QStringLiteral("basic_first_steps"),
                                      0, QStringLiteral("10 REM second try\n"), true);
    TW_CHECK(replaced.ok);
    TW_CHECK(replaced.replaced);
    QFile again{first.path};
    TW_CHECK(again.open(QIODevice::ReadOnly));
    TW_CHECK_EQ(QString::fromUtf8(again.readAll()), QStringLiteral("10 REM second try\n"));
    again.close();

    // Somebody not in the class has nowhere to hand work in, and is told so.
    const auto stranger = room.handIn(QStringLiteral("nobody"),
                                      QStringLiteral("basic_first_steps"), 0,
                                      QStringLiteral("10 REM\n"));
    TW_CHECK(!stranger.ok);
    TW_CHECK(!stranger.exists);
}

void test_collect_marks_handed_in_work_and_not_the_absence_of_it() {
    announce("collect and mark");
    tw::test::TempDir directory;
    if (!directory.valid()) {
        return;
    }
    Classroom room{directory.path()};
    QString error;
    TW_CHECK(room.load(&error));
    TW_CHECK(room.addStudent("Ada", {}, {}, &error));
    TW_CHECK(room.addStudent("Grace", {}, {}, &error));
    TW_CHECK(room.assign({QStringLiteral("basic_first_steps")}, {}, &error));

    const timewarp::lessons::Lesson* lesson = lesson_of(room, "basic_first_steps");
    if (lesson == nullptr) {
        return;
    }
    // Only Ada is handed a file; Grace hands nothing in, and must not appear.
    const QString ada = room.studentDirectory("ada");
    TW_CHECK(QDir{}.mkpath(ada));
    QFile starter{ada + QStringLiteral("/") + room.handoutName(*lesson, 0)};
    TW_CHECK(starter.open(QIODevice::WriteOnly));
    starter.write(QByteArray::fromStdString(lesson->checkpoints[0].starter_code));
    starter.close();

    QList<timewarp::lessons::Submission> submissions = room.collect({});
    TW_CHECK_EQ(submissions.size(), 1);
    if (!submissions.isEmpty()) {
        TW_CHECK_EQ(submissions.front().studentId, QStringLiteral("ada"));
        TW_CHECK(!submissions.front().passed);  // a starter is not an answer
    }

    // The worked answer passes, and marking it changes nobody's progress.
    QFile solved{ada + QStringLiteral("/") + room.handoutName(*lesson, 0)};
    TW_CHECK(solved.open(QIODevice::WriteOnly | QIODevice::Truncate));
    solved.write(QByteArray::fromStdString(lesson->checkpoints[0].solution));
    solved.close();
    submissions = room.collect({});
    TW_CHECK_EQ(submissions.size(), 1);
    if (!submissions.isEmpty()) {
        TW_CHECK(submissions.front().passed);
    }

    const QList<timewarp::lessons::MarkRow> marks = room.lessonMarks(submissions);
    TW_CHECK_EQ(marks.size(), 1);
    if (!marks.isEmpty()) {
        TW_CHECK_EQ(marks.front().passed, 1);
        TW_CHECK_EQ(marks.front().handedIn, 1);
        TW_CHECK_EQ(marks.front().checkpoints,
                    static_cast<int>(lesson->checkpoints.size()));
        TW_CHECK(!marks.front().complete);
    }
    // No progress file was written: a mark is the teacher's, not the learner's.
    TW_CHECK(!QFileInfo::exists(ada + QStringLiteral("/progress.json")));
}

void test_a_bundle_carries_the_class_to_another_machine() {
    announce("bundle round trip");
    tw::test::TempDir first_dir;
    tw::test::TempDir second_dir;
    if (!first_dir.valid() || !second_dir.valid()) {
        return;
    }
    Classroom room{first_dir.path()};
    QString error;
    TW_CHECK(room.load(&error));
    TW_CHECK(room.addStudent("Ada", {}, {}, &error));
    TW_CHECK(room.assign({QStringLiteral("basic_first_steps")}, "week one", &error));
    TW_CHECK(!room.distribute({}, &error).isEmpty());
    room.setPresentation(24, false);

    const QString bundle = first_dir.path() + QStringLiteral("/class.zip");
    TW_CHECK(room.createBundle(bundle, &error));
    TW_CHECK(QFileInfo{bundle}.size() > 0);

    Classroom other{second_dir.path()};
    TW_CHECK(other.load(&error));
    TW_CHECK(other.importBundle(bundle, false, &error));
    TW_CHECK_EQ(other.students().size(), std::size_t{1});
    TW_CHECK_EQ(other.assignment().lessonIds.size(), 1);
    TW_CHECK_EQ(other.presentation().fontSize, 24);
    TW_CHECK(!other.presentation().fullscreen);

    // The work came with it, so the second machine can collect without handing
    // out again -- all three starters, because that is what was distributed.
    const QList<timewarp::lessons::Submission> submissions = other.collect({});
    TW_CHECK_EQ(submissions.size(), 3);

    // Additive import keeps local work; a replacing import is authoritative.
    Classroom third{second_dir.path()};
    TW_CHECK(third.load(&error));
    const QString path = third.studentDirectory("ada") + QStringLiteral("/notes.txt");
    QFile notes{path};
    TW_CHECK(notes.open(QIODevice::WriteOnly));
    notes.write("local");
    notes.close();
    TW_CHECK(third.importBundle(bundle, false, &error));
    QFile kept{path};
    TW_CHECK(kept.open(QIODevice::ReadOnly));
    TW_CHECK_EQ(QString::fromUtf8(kept.readAll()), QStringLiteral("local"));
}

/// Marking is split into "find the files" and "run them", because the second half
/// runs a learner's program and happens on a worker thread. The halves have to
/// agree with the one-call version, or the thread and the command mark different
/// classes.
void test_marking_jobs_and_marks_agree_with_collect() {
    announce("marking jobs");
    tw::test::TempDir directory;
    if (!directory.valid()) {
        return;
    }
    Classroom room{directory.path()};
    QString error;
    TW_CHECK(room.load(&error));
    TW_CHECK(room.addStudent("Ada", {}, {}, &error));
    TW_CHECK(room.assign({QStringLiteral("basic_first_steps")}, {}, &error));
    const QStringList written = room.distribute({}, &error);
    TW_CHECK(!written.isEmpty());
    if (written.isEmpty()) {
        return;
    }
    // One answer, so the sheet has a pass and a fail in it.
    QFile solved{written.front()};
    TW_CHECK(solved.open(QIODevice::WriteOnly | QIODevice::Truncate));
    solved.write(QByteArray::fromStdString(
        lesson_of(room, "basic_first_steps")->checkpoints[0].solution));
    solved.close();

    const QList<timewarp::lessons::MarkJob> jobs = room.markingJobs({});
    TW_CHECK_EQ(jobs.size(), written.size());
    const QList<timewarp::lessons::Submission> split =
        Classroom::markAll(jobs);
    const QList<timewarp::lessons::Submission> direct = room.collect({});
    TW_CHECK_EQ(split.size(), direct.size());
    TW_CHECK_EQ(Classroom::marksFor(jobs, split).size(),
                room.lessonMarks(direct).size());
    if (!split.isEmpty() && !direct.isEmpty()) {
        TW_CHECK_EQ(split.front().passed, direct.front().passed);
        const auto rows = Classroom::marksFor(jobs, split);
        if (!rows.isEmpty()) {
            TW_CHECK_EQ(rows.front().handedIn, static_cast<int>(jobs.size()));
            TW_CHECK_EQ(rows.front().passed, 1);
        }
    }
}

/// A `work_dir` may be written with a `~`, and it means the home directory.
void test_a_work_directory_expands_a_tilde() {
    announce("work directory");
    tw::test::TempDir directory;
    if (!directory.valid()) {
        return;
    }
    Classroom room{directory.path()};
    QString error;
    TW_CHECK(room.load(&error));
    TW_CHECK(room.addStudent("Ada", {}, QStringLiteral("~/tw-classroom-probe"), &error));
    const QString found = room.studentDirectory(QStringLiteral("ada"));
    TW_CHECK(!found.startsWith(QStringLiteral("~")));
    TW_CHECK(found.startsWith(QDir::homePath()));
}

void test_presentation_size_is_clamped() {
    announce("presentation");
    tw::test::TempDir directory;
    if (!directory.valid()) {
        return;
    }
    Classroom room{directory.path()};
    QString error;
    TW_CHECK(room.load(&error));
    room.setPresentation(2, true);
    TW_CHECK_EQ(room.presentation().fontSize, PresentationSettings::kMinFont);
    room.setPresentation(400, true);
    TW_CHECK_EQ(room.presentation().fontSize, PresentationSettings::kMaxFont);
    // And it survives a reload, which is what a projector across a term needs.
    Classroom again{directory.path()};
    TW_CHECK(again.load(&error));
    TW_CHECK_EQ(again.presentation().fontSize, PresentationSettings::kMaxFont);
}

}  // namespace

int main(int argc, char** argv) {
    // Qt Core only, and only so the curriculum is found beside the build tree:
    // nothing here opens a window.
    QCoreApplication app{argc, argv};
    test_a_class_starts_empty_and_assigns_a_real_lesson();
    test_handing_out_writes_starters_and_never_overwrites();
    test_hand_in_writes_where_collect_looks_and_never_overwrites();
    test_collect_marks_handed_in_work_and_not_the_absence_of_it();
    test_a_bundle_carries_the_class_to_another_machine();
    test_marking_jobs_and_marks_agree_with_collect();
    test_a_work_directory_expands_a_tilde();
    test_presentation_size_is_clamped();
    return tw::test::summary("classroom");
}
