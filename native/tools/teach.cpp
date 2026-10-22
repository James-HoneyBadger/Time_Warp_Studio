// SPDX-License-Identifier: MIT
//
// The teacher's command.
//
//   time_warp_teach validate [curriculum.json]
//   time_warp_teach list [curriculum.json]
//   time_warp_teach roster
//   time_warp_teach bundle <out.zip>
//   time_warp_teach unbundle <in.zip> [--replace]
//
// `validate` is the pass that keeps the curriculum maintainable: every worked
// answer must pass its own checkpoint and every starter must fail it. Without
// that, a lesson whose answer does not work sits in the tree looking correct,
// because data is not exercised by anything unless something exercises it
// deliberately.
//
// It is the C++ counterpart of `native/tools/teach.py`, and the two are the same
// verdict because they are the same function's answer: this one calls
// `timewarp::lessons`, which is what the IDE's panels call too. `bundle` and
// `unbundle` write and read the same zips the reference does, so a class moves
// between them.
//
// `roster` prints the class and each learner's place in the curriculum; it marks
// nothing, because listing a class must not run thirty programs.

#include <cstdio>
#include <string>
#include <vector>

#include <QCoreApplication>
#include <QString>

#include "timewarp/lessons/classroom.hpp"
#include "timewarp/lessons/curriculum.hpp"
#include "timewarp/lessons/verify.hpp"

namespace {

using timewarp::lessons::Lesson;

int usage() {
    std::printf("usage: time_warp_teach <command> [path]\n\n"
                "  validate            every solution passes, every starter fails\n"
                "  list                the lessons, in teaching order\n"
                "  roster              the class, and what it has been set\n"
                "  bundle <out.zip>    zip the classroom, work and all\n"
                "  unbundle <in.zip>   unpack a class bundle (--replace to overwrite)\n");
    return 2;
}

QString curriculum_path(const std::vector<std::string>& arguments) {
    for (const std::string& argument : arguments) {
        if (!argument.empty() && argument.front() != '-') {
            return QString::fromStdString(argument);
        }
    }
    const QString found = timewarp::lessons::default_curriculum_path();
    if (found.isEmpty()) {
        std::printf("the curriculum was not found; set TIME_WARP_LESSONS\n");
    }
    return found;
}

int validate(const std::vector<Lesson>& lessons) {
    int failures = 0;
    std::size_t checkpoints = 0;
    for (const Lesson& lesson : lessons) {
        for (const auto& checkpoint : lesson.checkpoints) {
            ++checkpoints;
            const auto solution = timewarp::lessons::verify_checkpoint(
                checkpoint, lesson.language, checkpoint.solution);
            if (!solution.passed) {
                std::printf("❌ %s / %s: the solution does not pass\n   %s\n",
                            lesson.id.c_str(), checkpoint.title.c_str(),
                            solution.message.c_str());
                ++failures;
            }
            if (checkpoint.starter_is_broken) {
                continue;
            }
            const auto starter = timewarp::lessons::verify_checkpoint(
                checkpoint, lesson.language, checkpoint.starter_code);
            if (starter.passed) {
                std::printf("❌ %s / %s: the starter already passes\n",
                            lesson.id.c_str(), checkpoint.title.c_str());
                ++failures;
            }
        }
    }

    bool every_language = true;
    for (const timewarp::lang::Language language : timewarp::lang::all_languages()) {
        if (timewarp::lessons::lessons_for(lessons, language).empty()) {
            std::printf("❌ %s has no lesson\n",
                        std::string{timewarp::lang::key(language)}.c_str());
            every_language = false;
            ++failures;
        }
    }

    if (failures == 0) {
        std::printf("✅ every solution passes its own checkpoint\n");
        std::printf("✅ no starter already passes, so every checkpoint has work to do\n");
        if (every_language) {
            std::printf("✅ every language has a lesson\n");
        }
        std::printf("%zu lessons, %zu checkpoints\n", lessons.size(), checkpoints);
    }
    return failures == 0 ? 0 : 1;
}

void list(const std::vector<Lesson>& lessons) {
    for (const Lesson& lesson : lessons) {
        std::printf("%-24s %-10s %-12s %zu checkpoint(s)  %s\n", lesson.id.c_str(),
                    lesson.language_name().c_str(), lesson.difficulty.c_str(),
                    lesson.checkpoints.size(), lesson.title.c_str());
    }
}

}  // namespace

int main(int argc, char** argv) {
    // Qt Core only, and only so the default curriculum can be found beside the
    // binary: nothing here opens a window.
    QCoreApplication app{argc, argv};
    if (argc < 2) {
        return usage();
    }
    const std::string command{argv[1]};
    std::vector<std::string> arguments;
    for (int index = 2; index < argc; ++index) {
        arguments.emplace_back(argv[index]);
    }

    // The classroom side first: its path argument is a bundle, not a curriculum,
    // and the class carries its own curriculum in the default location.
    if (command == "roster" || command == "bundle" || command == "unbundle") {
        timewarp::lessons::Classroom room;
        QString error;
        if (!room.load(&error)) {
            std::printf("❌ %s\n", error.toStdString().c_str());
            return 1;
        }
        if (command == "roster") {
            std::printf("%zu student(s); assigned: %s\n", room.students().size(),
                        room.assignment().lessonIds.join(QStringLiteral(", "))
                            .toStdString()
                            .c_str());
            for (const auto& student : room.students()) {
                std::printf("  %-24s %s\n", student.id.toStdString().c_str(),
                            student.name.toStdString().c_str());
            }
            for (const auto& row : room.classProgress()) {
                std::printf("  %-24s %d/%d lesson(s), %d hint(s)\n",
                            row.id.toStdString().c_str(), row.completed, row.lessons,
                            row.hintsUsed);
            }
            return 0;
        }
        QString bundle;
        bool replace = false;
        for (const std::string& argument : arguments) {
            if (argument == "--replace") {
                replace = true;
            } else if (bundle.isEmpty()) {
                bundle = QString::fromStdString(argument);
            }
        }
        if (bundle.isEmpty()) {
            return usage();
        }
        if (command == "bundle") {
            if (!room.createBundle(bundle, &error)) {
                std::printf("❌ %s\n", error.toStdString().c_str());
                return 1;
            }
            std::printf("✅ wrote %s\n", bundle.toStdString().c_str());
            return 0;
        }
        if (!room.importBundle(bundle, replace, &error)) {
            std::printf("❌ %s\n", error.toStdString().c_str());
            return 1;
        }
        std::printf("✅ %zu student(s) in the class\n", room.students().size());
        return 0;
    }

    const QString path = curriculum_path(arguments);
    if (path.isEmpty()) {
        return 1;
    }
    std::vector<Lesson> lessons;
    QString error;
    if (!timewarp::lessons::load_curriculum(path, lessons, &error)) {
        std::printf("❌ %s\n", error.toStdString().c_str());
        return 1;
    }

    if (command == "validate") {
        return validate(lessons);
    }
    if (command == "list") {
        list(lessons);
        return 0;
    }
    return usage();
}
