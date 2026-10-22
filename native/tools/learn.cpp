// SPDX-License-Identifier: MIT
//
// The learner's command.
//
//   time_warp_learn list
//   time_warp_learn show <lesson>
//   time_warp_learn start <lesson>          (writes the starter to a file)
//   time_warp_learn check <lesson> <file>
//   time_warp_learn hint <lesson>
//   time_warp_learn solution <lesson>
//   time_warp_learn progress
//   time_warp_learn reset <lesson>
//
// The command line is `timewarp::cli`'s: `--root`, `--json PATH`, `--quiet` and
// `--help` in any position, and status 2 for a line it cannot use.
//
// It is the C++ counterpart of `native/tools/learn.py`, and the two give the same
// verdicts because they are the same library: `timewarp_lessons`, which is what the
// IDE's Lessons panel calls too. A learner who prefers a terminal is not a
// second-class client.

#include <array>
#include <cstdio>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <QCoreApplication>
#include <QFile>
#include <QFileInfo>
#include <QString>
#include <QTextStream>

#include "timewarp/lang/language.hpp"
#include "timewarp/tools/cli.hpp"
#include "timewarp/lessons/progress.hpp"
#include "timewarp/lessons/verify.hpp"

namespace {

using timewarp::lessons::Lesson;
using timewarp::lessons::LessonStatus;
using timewarp::lessons::ProgressStore;

QString status_text(LessonStatus status) {
    switch (status) {
        case LessonStatus::NotStarted:
            return QStringLiteral("not started");
        case LessonStatus::InProgress:
            return QStringLiteral("in progress");
        case LessonStatus::Completed:
            return QStringLiteral("completed");
    }
    return {};
}

QString extension_for(timewarp::lang::Language language) {
    // Mirrors `_ASSIGNMENT_EXTENSIONS` in the classroom: a file a learner opens
    // has to have the extension their editor expects.
    switch (language) {
        case timewarp::lang::Language::Basic: return QStringLiteral(".bas");
        case timewarp::lang::Language::Pilot: return QStringLiteral(".pilot");
        case timewarp::lang::Language::Logo: return QStringLiteral(".logo");
        case timewarp::lang::Language::Comal: return QStringLiteral(".comal");
        case timewarp::lang::Language::Rexx: return QStringLiteral(".rexx");
        case timewarp::lang::Language::Scheme: return QStringLiteral(".scm");
        case timewarp::lang::Language::Focal: return QStringLiteral(".foc");
        case timewarp::lang::Language::Chip8: return QStringLiteral(".ch8");
        case timewarp::lang::Language::Apl: return QStringLiteral(".apl");
        case timewarp::lang::Language::Smalltalk: return QStringLiteral(".st");
        case timewarp::lang::Language::Forth: return QStringLiteral(".fs");
    }
    return QStringLiteral(".txt");
}

int list(const ProgressStore& store) {
    int index = 0;
    for (const Lesson& lesson : store.lessons()) {
        ++index;
        std::printf("%2d. %-24s %-10s %-12s %s\n", index, lesson.id.c_str(),
                    lesson.language_name().c_str(),
                    status_text(store.status(lesson.id)).toStdString().c_str(),
                    lesson.title.c_str());
    }
    const auto [finished, total] = store.completed_count();
    std::printf("\n%d of %d lesson(s) finished\n", finished, total);
    return 0;
}

int show(const ProgressStore& store, const Lesson& lesson) {
    std::printf("%s — %s (%s, %s)\n", lesson.id.c_str(), lesson.title.c_str(),
                lesson.language_name().c_str(), lesson.difficulty.c_str());
    if (!lesson.description.empty()) {
        std::printf("%s\n", lesson.description.c_str());
    }
    if (!lesson.objectives.empty()) {
        std::printf("\nBy the end you can:\n");
        for (const std::string& objective : lesson.objectives) {
            std::printf("  • %s\n", objective.c_str());
        }
    }
    const int current = store.current_index(lesson.id);
    std::printf("\nCheckpoints:\n");
    for (std::size_t index = 0; index < lesson.checkpoints.size(); ++index) {
        const bool done = static_cast<int>(index) < current;
        std::printf("  %s %zu. %s\n", done ? "✓" : " ", index + 1,
                    lesson.checkpoints[index].title.c_str());
        if (static_cast<int>(index) == current) {
            std::printf("      %s\n", lesson.checkpoints[index].description.c_str());
        }
    }
    return 0;
}

const Lesson* resolve(const ProgressStore& store, const std::string& id) {
    const Lesson* lesson = timewarp::lessons::find_lesson(store.lessons(), id);
    if (lesson == nullptr) {
        std::printf("❌ no lesson called %s (or the name is ambiguous)\n", id.c_str());
    }
    return lesson;
}

int start(const ProgressStore& store, const Lesson& lesson) {
    const int index = store.current_index(lesson.id);
    if (index >= static_cast<int>(lesson.checkpoints.size())) {
        std::printf("✅ every checkpoint of %s is finished\n", lesson.id.c_str());
        return 0;
    }
    const QString name =
        QString::fromStdString(lesson.id) + QStringLiteral("-") +
        QString::number(index + 1) + extension_for(lesson.language);
    QFile file{name};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        std::printf("❌ cannot write %s: %s\n", name.toStdString().c_str(),
                    file.errorString().toStdString().c_str());
        return 1;
    }
    file.write(QByteArray::fromStdString(
        lesson.checkpoints[static_cast<std::size_t>(index)].starter_code));
    file.close();
    std::printf("wrote %s — checkpoint %d: %s\n", name.toStdString().c_str(), index + 1,
                lesson.checkpoints[static_cast<std::size_t>(index)].title.c_str());
    return 0;
}

int check(ProgressStore& store, const Lesson& lesson, const QString& path) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        std::printf("❌ cannot read %s\n", path.toStdString().c_str());
        return 1;
    }
    const std::string source = file.readAll().toStdString();
    file.close();
    const int index = store.current_index(lesson.id);
    if (index >= static_cast<int>(lesson.checkpoints.size())) {
        std::printf("✅ every checkpoint of %s is finished\n", lesson.id.c_str());
        return 0;
    }
    const auto result = store.check(lesson.id, index, source);
    std::printf("%s\n", result.message.c_str());
    if (result.passed) {
        const int next = store.current_index(lesson.id);
        if (next < static_cast<int>(lesson.checkpoints.size())) {
            std::printf("Next: %s\n",
                        lesson.checkpoints[static_cast<std::size_t>(next)].title.c_str());
        } else {
            std::printf("That was the last checkpoint of %s.\n", lesson.id.c_str());
        }
    }
    return result.passed ? 0 : 1;
}

int runList(const timewarp::cli::Invocation& invocation);
int runShow(const timewarp::cli::Invocation& invocation);
int runStart(const timewarp::cli::Invocation& invocation);
int runCheck(const timewarp::cli::Invocation& invocation);
int runHint(const timewarp::cli::Invocation& invocation);
int runSolution(const timewarp::cli::Invocation& invocation);
int runProgress(const timewarp::cli::Invocation& invocation);
int runReset(const timewarp::cli::Invocation& invocation);

constexpr std::string_view kSynopsis{"<command> [lesson] [file]"};

constexpr std::array<timewarp::cli::Command, 8> kCommands{{
    {"list", "the lessons, and where you are in them", runList},
    {"show", "one lesson: every checkpoint and its objective", runShow},
    {"start", "write the current checkpoint's starter to a file", runStart},
    {"check", "run a file against the current checkpoint", runCheck},
    {"hint", "the next hint for the current checkpoint", runHint},
    {"solution", "the worked answer", runSolution},
    {"progress", "what you have finished", runProgress},
    {"reset", "start a lesson again", runReset},
}};

/// The store, loaded once per command. A command that cannot read it says so and
/// answers 1, which is what the panel would show too.
std::optional<ProgressStore> load_store() {
    ProgressStore store;
    QString error;
    if (!store.load_default(&error)) {
        std::printf("❌ %s\n", error.toStdString().c_str());
        return std::nullopt;
    }
    return store;
}

/// The lesson a command names, or nullptr when it named none and has already been
/// told so.
const Lesson* named(const ProgressStore& store, const timewarp::cli::Invocation& invocation) {
    const std::vector<std::string> words = invocation.positionals();
    if (words.empty()) {
        return nullptr;
    }
    return resolve(store, words.front());
}

int runList(const timewarp::cli::Invocation& invocation) {
    (void)invocation;  // `list` takes no lesson: everything it has is in the store.
    std::optional<ProgressStore> store = load_store();
    return store ? list(*store) : 1;
}

int runProgress(const timewarp::cli::Invocation& invocation) {
    (void)invocation;
    std::optional<ProgressStore> store = load_store();
    if (!store) {
        return 1;
    }
    const auto [finished, total] = store->completed_count();
    std::printf("%d of %d lesson(s) finished\n", finished, total);
    for (const Lesson& lesson : store->lessons()) {
        if (store->status(lesson.id) == LessonStatus::InProgress) {
            std::printf("  in progress: %s (checkpoint %d)\n", lesson.id.c_str(),
                        store->current_index(lesson.id) + 1);
        }
    }
    return 0;
}

int runShow(const timewarp::cli::Invocation& invocation) {
    std::optional<ProgressStore> store = load_store();
    if (!store) {
        return 1;
    }
    const Lesson* lesson = named(*store, invocation);
    return lesson != nullptr ? show(*store, *lesson) : 1;
}

int runStart(const timewarp::cli::Invocation& invocation) {
    std::optional<ProgressStore> store = load_store();
    if (!store) {
        return 1;
    }
    const Lesson* lesson = named(*store, invocation);
    return lesson != nullptr ? start(*store, *lesson) : 1;
}

int runCheck(const timewarp::cli::Invocation& invocation) {
    std::optional<ProgressStore> store = load_store();
    if (!store) {
        return 1;
    }
    const Lesson* lesson = named(*store, invocation);
    if (lesson == nullptr) {
        return 1;
    }
    const std::vector<std::string> words = invocation.positionals();
    if (words.size() < 2) {
        return timewarp::cli::usage_error(invocation, "check needs a lesson and a file",
                                          kSynopsis, kCommands);
    }
    return check(*store, *lesson, QString::fromStdString(words.at(1)));
}

int runHint(const timewarp::cli::Invocation& invocation) {
    std::optional<ProgressStore> store = load_store();
    if (!store) {
        return 1;
    }
    const Lesson* lesson = named(*store, invocation);
    if (lesson == nullptr) {
        return 1;
    }
    const int index = store->current_index(lesson->id);
    const std::string text = store->hint(lesson->id, index, store->hints_used(lesson->id));
    if (text.empty()) {
        std::printf("That is every hint for this checkpoint.\n");
        return 0;
    }
    std::printf("%s\n", text.c_str());
    return 0;
}

int runSolution(const timewarp::cli::Invocation& invocation) {
    std::optional<ProgressStore> store = load_store();
    if (!store) {
        return 1;
    }
    const Lesson* lesson = named(*store, invocation);
    if (lesson == nullptr) {
        return 1;
    }
    const int index = store->current_index(lesson->id);
    if (index >= static_cast<int>(lesson->checkpoints.size())) {
        return 0;
    }
    std::printf("%s\n", lesson->checkpoints[static_cast<std::size_t>(index)].solution.c_str());
    return 0;
}

int runReset(const timewarp::cli::Invocation& invocation) {
    std::optional<ProgressStore> store = load_store();
    if (!store) {
        return 1;
    }
    const Lesson* lesson = named(*store, invocation);
    if (lesson == nullptr) {
        return 1;
    }
    store->reset(lesson->id);
    std::printf("%s starts again at checkpoint 1.\n", lesson->id.c_str());
    return 0;
}

}  // namespace

int main(int argc, char** argv) {
    // Qt Core only, and only so a lesson's default data directory can be found
    // beside the binary: nothing here opens a window.
    QCoreApplication app{argc, argv};
    const timewarp::cli::Invocation invocation =
        timewarp::cli::parse("time_warp_learn", argc, argv);

    int status = 0;
    {
        const timewarp::cli::ProseToStderr prose{invocation.wants_json()};
        status = timewarp::cli::dispatch(invocation, kSynopsis, kCommands);
    }
    if (!invocation.wants_json()) {
        return status;
    }
    timewarp::cli::Report report{"time_warp_learn", invocation};
    report.check(invocation.command, status == 0, status == 0 ? "" : "the command said no");
    return report.finish();
}
