// SPDX-License-Identifier: MIT
//
// The teacher's command.
//
//   time_warp_teach validate [curriculum.json]
//   time_warp_teach list [curriculum.json]
//   time_warp_teach outline
//   time_warp_teach plans [--language NAME]
//   time_warp_teach plans --write <dir>
//   time_warp_teach answers [--language NAME]
//   time_warp_teach new <id> --language NAME [--title "..."]
//   time_warp_teach roster [add "Name" | remove <id>]
//   time_warp_teach assign <lesson>...
//   time_warp_teach distribute
//   time_warp_teach collect
//   time_warp_teach report
//   time_warp_teach present [<size>] [--windowed]
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

#include <algorithm>
#include <cstdio>
#include <cctype>
#include <cstring>
#include <string>
#include <vector>

#include <QDir>
#include <QFile>
#include <QStringList>

#include <QCoreApplication>
#include <QString>

#include "timewarp/lessons/classroom.hpp"
#include "timewarp/lessons/curriculum.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/lessons/plans.hpp"
#include "timewarp/lessons/verify.hpp"

namespace {

using timewarp::lessons::Lesson;

int usage() {
    std::printf("usage: time_warp_teach <command> [arguments]\n\n"
                "The curriculum:\n"
                "  validate            every solution passes, every starter fails\n"
                "  list                the lessons, in teaching order\n"
                "  outline             the scheme of work, grouped by language\n"
                "  plans               a lesson plan per language, on stdout\n"
                "  plans --write <dir> write them as documents (docs/lessons/plans)\n"
                "  answers             the worked answers, for a teacher's eye\n"
                "  new <id>            scaffold a lesson (--language NAME [--title])\n\n"
                "The class:\n"
                "  roster              the class, and what it has been set\n"
                "  roster add \"Name\"   add a learner (a stable id is derived)\n"
                "  roster remove <id>  take one off the list\n"
                "  assign <lesson>...  set the work\n"
                "  distribute          hand the starter files out\n"
                "  collect             mark what has been handed in\n"
                "  report              where each learner has got to\n"
                "  present [<size>]    the size a lesson is projected at (--windowed)\n"
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

int validate(const std::vector<Lesson>& lessons, bool quiet = false) {
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

    if (failures == 0 && !quiet) {
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

/// Whether `name` means this language, spelled either way a person might write it:
/// the protocol key ("basic", "chip8") or the display name ("BASIC", "CHIP-8"),
/// case-insensitively. The two are different facts stored in different places, and a
/// teacher typing `--language Logo` should not have to know which one this compares.
bool language_matches(timewarp::lang::Language language, const std::string& name) {
    if (name.empty()) {
        return true;
    }
    const auto lower = [](std::string text) {
        for (char& character : text) {
            character = static_cast<char>(std::tolower(static_cast<unsigned char>(character)));
        }
        return text;
    };
    const std::string wanted = lower(name);
    return wanted == lower(std::string{timewarp::lang::key(language)}) ||
           wanted == lower(std::string{timewarp::lang::display_name(language)});
}

/// `outline`: the scheme of work, grouped by language, in curriculum order.
int outline(const std::vector<Lesson>& lessons) {
    const auto groups = timewarp::lessons::group_by_language(lessons);
    if (groups.empty()) {
        std::printf("❌ the curriculum is empty\n");
        return 1;
    }
    std::printf("════════════════════════════════════════════════════════════════════════\n");
    std::printf("Scheme of work\n");
    std::printf("════════════════════════════════════════════════════════════════════════\n");
    for (const auto& group : groups) {
        std::printf("\n%s\n", group.language.c_str());
        for (const Lesson* lesson : group.lessons) {
            std::string needs;
            if (!lesson->prerequisites.empty()) {
                needs = "   (after ";
                for (std::size_t index = 0; index < lesson->prerequisites.size(); ++index) {
                    needs += (index == 0 ? "" : ", ") + lesson->prerequisites[index];
                }
                needs += ")";
            }
            std::printf("  \u00b7 %s — %s, %zu checkpoints%s\n", lesson->title.c_str(),
                        lesson->difficulty.c_str(), lesson->checkpoints.size(),
                        needs.c_str());
            std::printf("    %s\n", lesson->description.c_str());
        }
    }
    std::printf("\nPrint a plan with: time_warp_teach plans --language <NAME>\n");
    return 0;
}

/// `plans`: the plan for one language or all of them, printed or written out.
int plans(const std::vector<Lesson>& lessons, const std::string& language,
          const std::string& directory) {
    const auto groups = timewarp::lessons::group_by_language(lessons);
    if (!directory.empty()) {
        // Writing is what the documentation job checks: it regenerates into
        // `docs/lessons/plans` and refuses a diff, so this command and those files
        // are one fact.
        QDir root{QString::fromStdString(directory)};
        if (!root.exists() && !root.mkpath(QStringLiteral("."))) {
            std::printf("❌ cannot create %s\n", directory.c_str());
            return 1;
        }
        int written = 0;
        for (const auto& group : groups) {
            if (!language_matches(group.lessons.front()->language, language)) {
                continue;
            }
            const std::string name =
                timewarp::lessons::plan_filename(group.lessons.front()->language);
            QFile file{root.filePath(QString::fromStdString(name))};
            if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
                std::printf("❌ cannot write %s\n", name.c_str());
                return 1;
            }
            const std::string text = timewarp::lessons::render_plans(group.language,
                                                                     group.lessons);
            file.write(text.data(), static_cast<qint64>(text.size()));
            file.close();
            std::printf("wrote %s\n", file.fileName().toStdString().c_str());
            ++written;
        }
        if (language.empty()) {
            QFile index{root.filePath(QStringLiteral("README.md"))};
            if (!index.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
                std::printf("❌ cannot write README.md\n");
                return 1;
            }
            const std::string text = timewarp::lessons::render_plans_index(groups);
            index.write(text.data(), static_cast<qint64>(text.size()));
            index.close();
            std::printf("wrote %s\n", index.fileName().toStdString().c_str());
        }
        return written == 0 ? 1 : 0;
    }

    bool printed = false;
    for (const auto& group : groups) {
        if (!language_matches(group.lessons.front()->language, language)) {
            continue;
        }
        std::printf("%s\n", timewarp::lessons::render_plans(group.language, group.lessons)
                                .c_str());
        printed = true;
    }
    if (!printed) {
        std::printf("ℹ️ no lessons for that language\n");
        return 1;
    }
    return 0;
}

/// `answers`: the worked answers, which is what a teacher reads before a class.
int answers(const std::vector<Lesson>& lessons, const std::string& language,
            const std::string& lesson_id) {
    for (const Lesson& lesson : lessons) {
        if (!language_matches(lesson.language, language)) {
            continue;
        }
        if (!lesson_id.empty() && lesson.id != lesson_id) {
            continue;
        }
        if (lesson_id.empty() && language.empty()) {
            continue;
        }
        std::printf("════════════════════════════════════════════════════════════════════════\n");
        std::printf("🏫 Answer key — %s\n", lesson.title.c_str());
        std::printf("════════════════════════════════════════════════════════════════════════\n");
        int position = 0;
        for (const timewarp::lessons::Checkpoint& checkpoint : lesson.checkpoints) {
            ++position;
            std::printf("\n%d. %s  (%s)\n\n", position, checkpoint.title.c_str(),
                        timewarp::lessons::describe_expectation(checkpoint).c_str());
            std::string body = checkpoint.solution;
            while (!body.empty() && body.back() == '\n') {
                body.pop_back();
            }
            std::size_t start = 0;
            while (start <= body.size()) {
                const std::size_t end = body.find('\n', start);
                const std::string line =
                    end == std::string::npos ? body.substr(start) : body.substr(start, end - start);
                std::printf("    %s\n", line.c_str());
                if (end == std::string::npos) {
                    break;
                }
                start = end + 1;
            }
        }
        std::printf("\n");
        return 0;
    }
    std::printf("❌ name a lesson with --lesson, or a language with --language\n");
    return 2;
}

/// `new`: a skeleton in the shape the loader accepts.
///
/// Printed rather than appended, because the shipped curriculum is a real file and
/// a scaffolding command should not be able to damage it by accident. An author who
/// starts from a shape the loader accepts cannot lose an afternoon to a schema
/// mistake.
int new_lesson(const std::string& id, const std::string& language_name,
               const std::string& title) {
    if (id.empty()) {
        std::printf("❌ a lesson needs an id\n");
        return 2;
    }
    std::string key;
    for (const char character : language_name) {
        key += character == '-' ? '_' : static_cast<char>(std::toupper(
                                                  static_cast<unsigned char>(character)));
    }
    const auto language = timewarp::lang::language_from_key(language_name);
    if (!language.has_value()) {
        std::printf("❌ %s is not a language here\n", language_name.c_str());
        std::printf("   Try: ");
        for (const timewarp::lang::Language each : timewarp::lang::all_languages()) {
            std::printf("%s ", std::string{timewarp::lang::display_name(each)}.c_str());
        }
        std::printf("\n");
        return 2;
    }
    const std::string shown = title.empty() ? id : title;
    std::printf("{\n"
                "  \"id\": \"%s\",\n"
                "  \"title\": \"%s\",\n"
                "  \"language\": \"%s\",\n"
                "  \"difficulty\": \"beginner\",\n"
                "  \"description\": \"What this lesson is about, in a sentence.\",\n"
                "  \"objectives\": [\"One thing the learner will be able to do\"],\n"
                "  \"teacher_notes\": \"How to teach it, and what learners usually get "
                "wrong.\",\n"
                "  \"prerequisites\": [],\n"
                "  \"checkpoints\": [\n"
                "    {\n"
                "      \"title\": \"First step\",\n"
                "      \"description\": \"What the finished program must do.\",\n"
                "      \"starter_code\": \"\",\n"
                "      \"solution\": \"\",\n"
                "      \"hints\": [\"A first nudge.\", \"A second, more direct nudge.\"],\n"
                "      \"expect_lines\": [\"\"]\n"
                "    }\n"
                "  ]\n"
                "}\n",
                id.c_str(), shown.c_str(),
                std::string{timewarp::lang::display_name(*language)}.c_str());
    std::printf("\nℹ️ add it to native/share/lessons.json, then run "
                "`time_warp_teach validate`.\n"
                "   It will not validate until you replace the placeholder checkpoint.\n");
    return 0;
}


// ---------------------------------------------------------------------------
// The class
// ---------------------------------------------------------------------------

/// A value in `--flag value` form, or empty.
std::string flag(const std::vector<std::string>& arguments, const std::string& name) {
    for (std::size_t index = 0; index + 1 < arguments.size(); ++index) {
        if (arguments[index] == name) {
            return arguments[index + 1];
        }
    }
    return {};
}

/// The arguments that are not flags and not a flag's value.
std::vector<std::string> plain(const std::vector<std::string>& arguments,
                               const std::vector<std::string>& takes_value) {
    std::vector<std::string> values;
    for (std::size_t index = 0; index < arguments.size(); ++index) {
        const std::string& argument = arguments[index];
        if (argument == "--replace" || argument == "--windowed" || argument == "--all") {
            continue;
        }
        bool consumed = false;
        for (const std::string& name : takes_value) {
            if (argument == name) {
                ++index;
                consumed = true;
                break;
            }
        }
        if (!consumed) {
            values.push_back(argument);
        }
    }
    return values;
}

int roster(timewarp::lessons::Classroom& room, const std::vector<std::string>& arguments) {
    const std::vector<std::string> values = plain(arguments, {});
    if (!values.empty() && values.front() == "add") {
        if (values.size() < 2) {
            std::printf("❌ roster add needs a name\n");
            return 2;
        }
        QString error;
        const QString name = QString::fromStdString(values[1]);
        const QString id = values.size() > 2 ? QString::fromStdString(values[2]) : QString{};
        if (!room.addStudent(name, id, {}, &error)) {
            std::printf("❌ %s\n", error.toStdString().c_str());
            return 1;
        }
        for (const auto& student : room.students()) {
            if (student.name == name) {
                std::printf("✅ added %s as %s\n", student.name.toStdString().c_str(),
                            student.id.toStdString().c_str());
            }
        }
    } else if (!values.empty() && values.front() == "remove") {
        if (values.size() < 2) {
            std::printf("❌ roster remove needs an id\n");
            return 2;
        }
        const QString id = QString::fromStdString(values[1]);
        if (room.removeStudent(id)) {
            std::printf("✅ removed %s\n", values[1].c_str());
            std::printf("ℹ️ their files are still in %s\n",
                        room.studentDirectory(id).toStdString().c_str());
        } else {
            std::printf("ℹ️ no such student\n");
        }
    }

    std::printf("%zu student(s); assigned: %s\n", room.students().size(),
                room.assignment().lessonIds.join(QStringLiteral(", ")).toStdString().c_str());
    if (room.students().empty()) {
        std::printf("ℹ️ the class list is empty. `time_warp_teach roster add \"Ada Lovelace\"`\n");
        return 0;
    }
    for (const auto& student : room.students()) {
        std::printf("  %-24s %s\n", student.id.toStdString().c_str(),
                    student.name.toStdString().c_str());
    }
    return 0;
}

int assign(timewarp::lessons::Classroom& room, const std::vector<std::string>& arguments) {
    const std::string note = flag(arguments, "--note");
    const std::vector<std::string> ids = plain(arguments, {"--note"});
    if (ids.empty()) {
        std::printf("❌ assign needs at least one lesson id\n");
        return 2;
    }
    QStringList list;
    for (const std::string& id : ids) {
        bool known = false;
        for (const Lesson& lesson : room.lessons()) {
            known = known || lesson.id == id;
        }
        if (!known) {
            std::printf("❌ no lesson called %s\n", id.c_str());
            return 2;
        }
        list.append(QString::fromStdString(id));
    }
    QString error;
    if (!room.assign(list, QString::fromStdString(note), &error)) {
        std::printf("❌ %s\n", error.toStdString().c_str());
        return 1;
    }
    std::printf("✅ assigned %zu lesson(s)\n", ids.size());
    if (!note.empty()) {
        std::printf("ℹ️ note: %s\n", note.c_str());
    }
    std::printf("ℹ️ `time_warp_teach distribute` hands the starter files out\n");
    return 0;
}

int distribute(timewarp::lessons::Classroom& room, const std::vector<std::string>& arguments) {
    const std::string lesson = flag(arguments, "--lesson");
    QStringList written;
    if (lesson.empty()) {
        written = room.distribute();
    } else {
        written = room.distribute(QStringList{QString::fromStdString(lesson)});
    }
    if (written.isEmpty()) {
        std::printf("ℹ️ nothing to hand out: every file is already there\n");
        return 0;
    }
    for (const QString& path : written) {
        std::printf("  %s\n", path.toStdString().c_str());
    }
    std::printf("✅ handed out %lld file(s) to %zu student(s)\n",
                static_cast<long long>(written.size()), room.students().size());
    return 0;
}

int collect(timewarp::lessons::Classroom& room, const std::vector<std::string>& arguments) {
    const std::string lesson = flag(arguments, "--lesson");
    const QList<timewarp::lessons::Submission> submissions =
        lesson.empty() ? room.collect()
                       : room.collect(QStringList{QString::fromStdString(lesson)});
    if (submissions.isEmpty()) {
        std::printf("ℹ️ nothing collected: no handed-out files and no work\n");
        return 1;
    }
    const QList<timewarp::lessons::MarkRow> rows = room.lessonMarks(submissions);
    int complete = 0;
    for (const auto& row : rows) {
        if (row.complete) {
            ++complete;
        }
        std::printf("  %s %-12s %-22s %d/%d checkpoint(s) passed\n",
                    row.complete ? "✅" : "…", row.student.toStdString().c_str(),
                    row.lesson.toStdString().c_str(), row.passed, row.checkpoints);
    }
    std::printf("\n%d complete, %lld file(s) collected\n", complete,
                static_cast<long long>(submissions.size()));
    return 0;
}

int report(timewarp::lessons::Classroom& room) {
    // Where each learner has got to, from their own progress files. Distinct from
    // `collect`, which marks handed-in work: "finished" and "handed in" are two
    // questions, and a teacher needs to know which one they asked.
    const QList<timewarp::lessons::ClassProgressRow> rows = room.classProgress();
    if (rows.isEmpty()) {
        std::printf("ℹ️ nobody in the class yet\n");
        return 0;
    }
    std::printf("%-24s %-24s %s\n", "learner", "name", "progress");
    for (const auto& row : rows) {
        std::printf("%-24s %-24s %d/%d lesson(s), %d hint(s)\n", row.id.toStdString().c_str(),
                    row.name.toStdString().c_str(), row.completed, row.lessons,
                    row.hintsUsed);
    }
    return 0;
}

int present(timewarp::lessons::Classroom& room, const std::vector<std::string>& arguments) {
    const std::vector<std::string> values = plain(arguments, {});
    const bool windowed =
        std::find(arguments.begin(), arguments.end(), std::string{"--windowed"}) !=
        arguments.end();
    if (values.empty()) {
        const auto& settings = room.presentation();
        std::printf("font size: %d   fullscreen: %s\n", settings.fontSize,
                    settings.fullscreen ? "yes" : "no");
        std::printf("range: %d..%d\n", timewarp::lessons::PresentationSettings::kMinFont,
                    timewarp::lessons::PresentationSettings::kMaxFont);
        return 0;
    }
    long size = 0;
    if (!std::sscanf(values.front().c_str(), "%ld", &size) || size <= 0) {
        std::printf("❌ %s is not a size\n", values.front().c_str());
        return 2;
    }
    const int font = std::clamp(static_cast<int>(size),
                                timewarp::lessons::PresentationSettings::kMinFont,
                                timewarp::lessons::PresentationSettings::kMaxFont);
    room.setPresentation(font, !windowed);
    QString error;
    if (!room.load(&error)) {
        std::printf("❌ %s\n", error.toStdString().c_str());
        return 1;
    }
    std::printf("✅ presentation size %d, fullscreen %s\n", font, windowed ? "no" : "yes");
    std::printf("ℹ️ the lesson panel in the IDE reads this when you open it\n");
    return 0;
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

    // The class first: its path argument is a bundle or a name, not a curriculum,
    // and the class carries its own curriculum in the default location.
    const bool class_command = command == "roster" || command == "bundle" ||
                               command == "unbundle" || command == "assign" ||
                               command == "distribute" || command == "collect" ||
                               command == "report" || command == "present";
    if (class_command) {
        timewarp::lessons::Classroom room;
        QString error;
        if (!room.load(&error)) {
            std::printf("❌ %s\n", error.toStdString().c_str());
            return 1;
        }
        if (command == "roster") {
            return roster(room, arguments);
        }
        if (command == "assign") {
            return assign(room, arguments);
        }
        if (command == "distribute") {
            return distribute(room, arguments);
        }
        if (command == "collect") {
            return collect(room, arguments);
        }
        if (command == "report") {
            return report(room);
        }
        if (command == "present") {
            return present(room, arguments);
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

    // `new` needs no curriculum: it is the command that puts a lesson into one.
    if (command == "new") {
        const std::vector<std::string> values = plain(arguments, {"--language", "--title"});
        if (values.empty()) {
            std::printf("❌ new needs a lesson id\n");
            return 2;
        }
        return new_lesson(values.front(), flag(arguments, "--language"),
                           flag(arguments, "--title"));
    }

    // Only a bare argument can be the curriculum: `--write dir` and `--language X`
    // name something else, and reading the directory as the curriculum was how
    // `plans --write` first failed.
    const QString path =
        curriculum_path(plain(arguments, {"--write", "--language", "--lang", "--lesson"}));
    if (path.isEmpty()) {
        return 1;
    }
    std::vector<Lesson> lessons;
    QString error;
    if (!timewarp::lessons::load_curriculum(path, lessons, &error)) {
        std::printf("❌ %s\n", error.toStdString().c_str());
        return 1;
    }

    if (command == "outline") {
        return outline(lessons);
    }
    if (command == "plans") {
        const std::string language = flag(arguments, "--language");
        const std::string language_short = flag(arguments, "--lang");
        return plans(lessons, language.empty() ? language_short : language,
                     flag(arguments, "--write"));
    }
    if (command == "answers") {
        return answers(lessons, flag(arguments, "--language"),
                       flag(arguments, "--lesson"));
    }
    if (command == "validate") {
        return validate(lessons,
                        std::find(arguments.begin(), arguments.end(),
                                  std::string{"--quiet"}) != arguments.end());
    }
    if (command == "list") {
        list(lessons);
        return 0;
    }
    return usage();
}
