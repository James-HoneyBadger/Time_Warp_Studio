// SPDX-License-Identifier: MIT
//
// Tests for the lessons library.
//
// The important case is not a unit test of the loader: it is the whole curriculum
// through the engine. A lesson whose worked answer does not pass its own
// checkpoint is a curriculum bug that sits in the tree looking correct, because
// data is not exercised by anything unless something exercises it deliberately.
//
// That is what `teach.py validate` was, and this is the same pass in C++ -- which
// is the point of the port: the panel, the teacher's command and this test ask one
// function.

#include <algorithm>
#include <cstddef>
#include <cstdio>
#include <string>
#include <vector>

#include "check.hpp"
#include "timewarp/lang/language.hpp"
#include "temp_dir.hpp"
#include "timewarp/lessons/curriculum.hpp"
#include "timewarp/lessons/progress.hpp"
#include "timewarp/lessons/verify.hpp"

namespace {

using timewarp::lang::Language;
using timewarp::lessons::Checkpoint;
using timewarp::lessons::Lesson;
using timewarp::lessons::LessonStatus;
using timewarp::lessons::ProgressStore;
using timewarp::lessons::printed_lines;
using timewarp::lessons::verify_checkpoint;

void announce(const char* name) { std::printf("-- %s\n", name); }

std::vector<Lesson> curriculum() {
#ifdef TIME_WARP_TEST_LESSONS
    std::vector<Lesson> lessons;
    QString error;
    if (!timewarp::lessons::load_curriculum(QStringLiteral(TIME_WARP_TEST_LESSONS),
                                            lessons, &error)) {
        std::printf("    (%s)\n", error.toStdString().c_str());
    }
    return lessons;
#else
    return {};
#endif
}

void test_the_shipped_curriculum_loads() {
    announce("curriculum loads");
    const std::vector<Lesson> lessons = curriculum();
    TW_CHECK_EQ(lessons.size(), std::size_t{22});
    std::size_t checkpoints = 0;
    for (const Lesson& lesson : lessons) {
        checkpoints += lesson.checkpoints.size();
        TW_CHECK(!lesson.id.empty());
        TW_CHECK(!lesson.checkpoints.empty());
        TW_CHECK(lesson.difficulty == "beginner" || lesson.difficulty == "intermediate" ||
                 lesson.difficulty == "advanced");
    }
    TW_CHECK_EQ(checkpoints, std::size_t{66});
}

void test_every_language_has_a_lesson() {
    announce("every language taught");
    const std::vector<Lesson> lessons = curriculum();
    for (const Language language : timewarp::lang::all_languages()) {
        TW_CHECK(!timewarp::lessons::lessons_for(lessons, language).empty());
    }
}

void test_every_solution_passes_its_own_checkpoint() {
    announce("every solution passes");
    const std::vector<Lesson> lessons = curriculum();
    for (const Lesson& lesson : lessons) {
        for (const Checkpoint& checkpoint : lesson.checkpoints) {
            const auto result =
                verify_checkpoint(checkpoint, lesson.language, checkpoint.solution);
            if (!result.passed) {
                std::printf("    %s / %s: %s\n", lesson.id.c_str(),
                            checkpoint.title.c_str(), result.message.c_str());
            }
            TW_CHECK(result.passed);
        }
    }
}

void test_no_starter_already_passes() {
    announce("starters have work to do");
    const std::vector<Lesson> lessons = curriculum();
    for (const Lesson& lesson : lessons) {
        for (const Checkpoint& checkpoint : lesson.checkpoints) {
            if (checkpoint.starter_is_broken) {
                continue;
            }
            const auto result =
                verify_checkpoint(checkpoint, lesson.language, checkpoint.starter_code);
            if (result.passed) {
                std::printf("    %s / %s: the starter already passes\n",
                            lesson.id.c_str(), checkpoint.title.c_str());
            }
            TW_CHECK(!result.passed);
        }
    }
}

void test_a_wrong_answer_says_so_and_shows_both_sides() {
    announce("wrong answer message");
    const std::vector<Lesson> lessons = curriculum();
    const Lesson* lesson = timewarp::lessons::find_lesson(lessons, "basic_first_steps");
    TW_CHECK(lesson != nullptr);
    if (lesson == nullptr || lesson->checkpoints.empty()) {
        return;
    }
    const Checkpoint& first = lesson->checkpoints.front();
    const auto result = verify_checkpoint(first, lesson->language, "10 PRINT \"nope\"");
    TW_CHECK(!result.passed);
    TW_CHECK(result.message.find("Not yet.") == 0);
    TW_CHECK(result.message.find("Hello, World!") != std::string::npos);
    TW_CHECK(result.message.find("nope") != std::string::npos);
}

void test_an_empty_program_is_not_judged() {
    announce("nothing to check");
    Checkpoint checkpoint;
    checkpoint.title = "x";
    checkpoint.expect_lines = std::vector<std::string>{"anything"};
    const auto result = verify_checkpoint(checkpoint, Language::Basic, "   \n");
    TW_CHECK(!result.passed);
    TW_CHECK_EQ(result.message, std::string{"There is nothing to check yet."});
}

void test_printed_lines_ignores_engine_commentary() {
    announce("printed lines");
    const std::vector<std::string> raw{"🎨 Graphics mode (320x200)", "hello",
                                       "🚀 Running", "world\n", ""};
    const std::vector<std::string> lines = printed_lines(raw);
    TW_CHECK_EQ(lines.size(), std::size_t{2});
    if (lines.size() == 2) {
        TW_CHECK_EQ(lines.at(0), std::string{"hello"});
        TW_CHECK_EQ(lines.at(1), std::string{"world"});
    }
}

void test_a_missing_file_is_an_error_not_an_empty_course() {
    announce("missing curriculum");
    std::vector<Lesson> lessons;
    QString error;
    const bool loaded = timewarp::lessons::load_curriculum(
        QStringLiteral("/nonexistent/lessons.json"), lessons, &error);
    TW_CHECK(!loaded);
    TW_CHECK(!error.isEmpty());
    TW_CHECK(lessons.empty());
}

void test_progress_follows_the_learners_place() {
    announce("progress");
#ifdef TIME_WARP_TEST_LESSONS
    tw::test::TempDir directory;
    TW_CHECK(directory.valid());
    if (!directory.valid()) {
        return;
    }
    ProgressStore store;
    QString error;
    TW_CHECK(store.load(QStringLiteral(TIME_WARP_TEST_LESSONS),
                        directory.path() + QStringLiteral("/progress.json"), &error));
    const std::string id = "basic_first_steps";
    TW_CHECK(store.status(id) == LessonStatus::NotStarted);
    TW_CHECK_EQ(store.current_index(id), 0);

    const Lesson* lesson = timewarp::lessons::find_lesson(store.lessons(), id);
    TW_CHECK(lesson != nullptr);
    if (lesson == nullptr) {
        return;
    }
    // A starter fails and changes nothing.
    const auto missed = store.check(id, 0, lesson->checkpoints[0].starter_code);
    TW_CHECK(!missed.passed);
    TW_CHECK_EQ(store.current_index(id), 0);
    TW_CHECK(store.status(id) == LessonStatus::NotStarted);

    // A pass moves the learner to the next checkpoint.
    const auto passed = store.check(id, 0, lesson->checkpoints[0].solution);
    TW_CHECK(passed.passed);
    TW_CHECK_EQ(store.current_index(id), 1);
    TW_CHECK(store.status(id) == LessonStatus::InProgress);

    // And the place survives being written and read again, which is the whole
    // point of a progress file.
    ProgressStore reopened;
    TW_CHECK(reopened.load(QStringLiteral(TIME_WARP_TEST_LESSONS),
                           directory.path() + QStringLiteral("/progress.json"), &error));
    TW_CHECK_EQ(reopened.current_index(id), 1);

    // Finishing the last checkpoint completes the lesson -- and says so, in the
    // wording a learner is already used to.
    std::string final_message;
    for (int index = 1; index < static_cast<int>(lesson->checkpoints.size()); ++index) {
        const auto result =
            store.check(id, index,
                        lesson->checkpoints[static_cast<std::size_t>(index)].solution);
        TW_CHECK(result.passed);
        final_message = result.message;
    }
    TW_CHECK(store.status(id) == LessonStatus::Completed);
    TW_CHECK(final_message.find("🏆 Lesson complete:") == 0);
    TW_CHECK_EQ(store.completed_count().first, 1);

    // A hint is counted per request and clamped to the last one: a learner who
    // keeps asking keeps getting the last hint rather than a refusal.
    const std::string first = store.hint(id, 0, 0);
    TW_CHECK(!first.empty());
    TW_CHECK_EQ(store.hints_used(id), 1);
    const std::string past_the_end = store.hint(id, 0, 99);
    TW_CHECK(!past_the_end.empty());
    TW_CHECK_EQ(store.hints_used(id), 2);
    // Asking past the end gives the last hint, not a refusal: the panel asks for
    // hint number `hints_used`, so a learner who keeps pressing gets the last one.
    TW_CHECK(past_the_end != first);
    TW_CHECK_EQ(store.hint(id, 0, 1), past_the_end);

    store.reset(id);
    TW_CHECK(store.status(id) == LessonStatus::NotStarted);
    TW_CHECK_EQ(store.current_index(id), 0);
#endif
}

}  // namespace

int main() {
    test_the_shipped_curriculum_loads();
    test_every_language_has_a_lesson();
    test_every_solution_passes_its_own_checkpoint();
    test_no_starter_already_passes();
    test_a_wrong_answer_says_so_and_shows_both_sides();
    test_an_empty_program_is_not_judged();
    test_printed_lines_ignores_engine_commentary();
    test_a_missing_file_is_an_error_not_an_empty_course();
    test_progress_follows_the_learners_place();
    return tw::test::summary("lessons");
}
