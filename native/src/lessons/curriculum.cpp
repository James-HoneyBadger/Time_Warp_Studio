// SPDX-License-Identifier: MIT
//
// Reading and checking the curriculum. See `curriculum.hpp` for why the shape is
// refused rather than tolerated.

#include "timewarp/lessons/curriculum.hpp"

#include <algorithm>
#include <cctype>
#include <cstddef>

#include <QCoreApplication>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QJsonValue>

namespace timewarp::lessons {
namespace {

[[nodiscard]] std::string lower_ascii(std::string text) {
    for (char& character : text) {
        character = static_cast<char>(
            std::tolower(static_cast<unsigned char>(character)));
    }
    return text;
}

[[nodiscard]] std::string text_of(const QJsonObject& object, const char* key) {
    return object.value(QLatin1String{key}).toString().toStdString();
}

[[nodiscard]] std::vector<std::string> strings_of(const QJsonObject& object,
                                                  const char* key) {
    std::vector<std::string> values;
    const QJsonValue value = object.value(QLatin1String{key});
    if (!value.isArray()) {
        return values;
    }
    for (const QJsonValue& entry : value.toArray()) {
        values.push_back(entry.toString().toStdString());
    }
    return values;
}

[[nodiscard]] bool optional_int(const QJsonObject& object, const char* key,
                                std::optional<int>& out) {
    const QJsonValue value = object.value(QLatin1String{key});
    if (value.isUndefined()) {
        return true;
    }
    if (!value.isDouble()) {
        return false;
    }
    out = static_cast<int>(value.toDouble());
    return true;
}

/// The language a lesson names. The curriculum says `BASIC`, `CHIP8`,
/// `SMALLTALK` -- the display names, because that is what a lesson document
/// shows.
[[nodiscard]] std::optional<lang::Language> language_named(std::string name) {
    name = lower_ascii(std::move(name));
    std::replace(name.begin(), name.end(), '-', '_');
    return lang::language_from_key(name);
}

[[nodiscard]] bool read_checkpoint(const QJsonObject& raw, const std::string& lesson_id,
                                   Checkpoint& out, QString* error) {
    const auto refuse = [&](const QString& why) {
        if (error != nullptr) {
            *error = QStringLiteral("lesson %1: %2")
                         .arg(QString::fromStdString(lesson_id), why);
        }
        return false;
    };
    const std::string title = text_of(raw, "title");
    if (title.empty()) {
        return refuse(QStringLiteral("a checkpoint has no title"));
    }
    out.title = title;
    out.description = text_of(raw, "description");
    out.starter_code = text_of(raw, "starter_code");
    out.solution = text_of(raw, "solution");
    for (const char* required : {"description", "starter_code", "solution"}) {
        if (text_of(raw, required).empty()) {
            return refuse(QStringLiteral("checkpoint \"%1\" has no %2")
                              .arg(QString::fromStdString(title),
                                   QLatin1String{required}));
        }
    }
    out.hints = strings_of(raw, "hints");
    out.starter_is_broken = raw.value(QStringLiteral("starter_is_broken")).toBool(false);

    const bool has_lines = raw.contains(QStringLiteral("expect_lines"));
    const bool has_turtle = raw.contains(QStringLiteral("expect_turtle"));
    const bool has_error = raw.contains(QStringLiteral("expect_error"));
    const int expectations = (has_lines ? 1 : 0) + (has_turtle ? 1 : 0) + (has_error ? 1 : 0);
    if (expectations != 1) {
        return refuse(QStringLiteral("checkpoint \"%1\" must have exactly one "
                                     "expectation, found %2")
                          .arg(QString::fromStdString(title))
                          .arg(expectations));
    }

    if (has_lines) {
        out.expect_lines = strings_of(raw, "expect_lines");
    } else if (has_error) {
        out.expect_error = text_of(raw, "expect_error");
    } else {
        const QJsonObject spec = raw.value(QStringLiteral("expect_turtle")).toObject();
        TurtleExpectation turtle;
        if (!optional_int(spec, "segments", turtle.segments) ||
            !optional_int(spec, "min_segments", turtle.min_segments) ||
            !optional_int(spec, "shapes", turtle.shapes) ||
            !optional_int(spec, "min_shapes", turtle.min_shapes)) {
            return refuse(QStringLiteral("checkpoint \"%1\" has a non-numeric count")
                              .arg(QString::fromStdString(title)));
        }
        turtle.returns_home = spec.value(QStringLiteral("returns_home")).toBool(false);
        out.expect_turtle = turtle;
    }
    return true;
}

}  // namespace

std::string Lesson::language_name() const { return std::string{lang::key(language)}; }

bool load_curriculum(const QString& path, std::vector<Lesson>& lessons, QString* error) {
    lessons.clear();
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = QStringLiteral("cannot read the curriculum at %1").arg(path);
        }
        return false;
    }
    QJsonParseError parse{};
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parse);
    file.close();
    if (document.isNull()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 is not valid JSON: %2")
                         .arg(path, parse.errorString());
        }
        return false;
    }

    std::vector<std::string> seen;
    for (const QJsonValue& value : document.object().value(QStringLiteral("lessons")).toArray()) {
        const QJsonObject raw = value.toObject();
        Lesson lesson;
        lesson.id = text_of(raw, "id");
        if (lesson.id.empty()) {
            if (error != nullptr) {
                *error = QStringLiteral("a lesson in %1 has no id").arg(path);
            }
            return false;
        }
        if (std::find(seen.begin(), seen.end(), lesson.id) != seen.end()) {
            if (error != nullptr) {
                *error = QStringLiteral("two lessons share the id %1")
                             .arg(QString::fromStdString(lesson.id));
            }
            return false;
        }
        seen.push_back(lesson.id);

        const std::optional<lang::Language> language =
            language_named(text_of(raw, "language"));
        if (!language.has_value()) {
            if (error != nullptr) {
                *error = QStringLiteral("lesson %1: unknown language \"%2\"")
                             .arg(QString::fromStdString(lesson.id),
                                  QString::fromStdString(text_of(raw, "language")));
            }
            return false;
        }
        lesson.language = *language;
        lesson.title = text_of(raw, "title");
        lesson.description = text_of(raw, "description");
        lesson.teacher_notes = text_of(raw, "teacher_notes");
        lesson.objectives = strings_of(raw, "objectives");
        lesson.prerequisites = strings_of(raw, "prerequisites");
        lesson.difficulty = lower_ascii(text_of(raw, "difficulty"));
        if (lesson.difficulty != "beginner" && lesson.difficulty != "intermediate" &&
            lesson.difficulty != "advanced") {
            if (error != nullptr) {
                *error = QStringLiteral("lesson %1: difficulty \"%2\" is not one of "
                                        "beginner, intermediate, advanced")
                             .arg(QString::fromStdString(lesson.id),
                                  QString::fromStdString(lesson.difficulty));
            }
            return false;
        }

        const QJsonArray checkpoints =
            raw.value(QStringLiteral("checkpoints")).toArray();
        for (const QJsonValue& entry : checkpoints) {
            Checkpoint checkpoint;
            if (!read_checkpoint(entry.toObject(), lesson.id, checkpoint, error)) {
                return false;
            }
            lesson.checkpoints.push_back(std::move(checkpoint));
        }
        if (lesson.checkpoints.empty()) {
            if (error != nullptr) {
                *error = QStringLiteral("lesson %1 has no checkpoints")
                             .arg(QString::fromStdString(lesson.id));
            }
            return false;
        }
        lessons.push_back(std::move(lesson));
    }
    if (lessons.empty()) {
        if (error != nullptr) {
            *error = QStringLiteral("the curriculum at %1 is empty").arg(path);
        }
        return false;
    }
    return true;
}

QString default_curriculum_path() {
    const QString override = qEnvironmentVariable("TIME_WARP_LESSONS").trimmed();
    if (!override.isEmpty() && QFileInfo{override}.isFile()) {
        return QFileInfo{override}.absoluteFilePath();
    }
    // Without an application object there is no directory to look beside, which
    // is the case for a library user that passes its own path anyway.
    const QString app_dir = QCoreApplication::instance() != nullptr
                                ? QCoreApplication::applicationDirPath()
                                : QString{};
    if (app_dir.isEmpty()) {
        return {};
    }
    for (const QString& candidate :
         {app_dir + QStringLiteral("/../share/time-warp-studio/lessons.json"),
          app_dir + QStringLiteral("/../share/lessons.json"),
          app_dir + QStringLiteral("/../Resources/share/time-warp-studio/lessons.json"),
          app_dir + QStringLiteral("/../../native/share/lessons.json")}) {
        if (QFileInfo{candidate}.isFile()) {
            return QFileInfo{candidate}.absoluteFilePath();
        }
    }
    return {};
}

std::vector<const Lesson*> lessons_for(const std::vector<Lesson>& lessons,
                                       lang::Language language) {
    std::vector<const Lesson*> chosen;
    for (const Lesson& lesson : lessons) {
        if (lesson.language == language) {
            chosen.push_back(&lesson);
        }
    }
    return chosen;
}

const Lesson* find_lesson(const std::vector<Lesson>& lessons, std::string_view id) {
    for (const Lesson& lesson : lessons) {
        if (lesson.id == id) {
            return &lesson;
        }
    }
    // A prefix is how the CLIs let a teacher type `basic_first` for
    // `basic_first_steps`, which is useful and unambiguous while the prefix is.
    const Lesson* match = nullptr;
    for (const Lesson& lesson : lessons) {
        if (lesson.id.rfind(id, 0) == 0) {
            if (match != nullptr) {
                return nullptr;  // ambiguous: no answer is better than a guess
            }
            match = &lesson;
        }
    }
    return match;
}

}  // namespace timewarp::lessons
