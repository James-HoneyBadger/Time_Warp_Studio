// SPDX-License-Identifier: MIT
//
// The progress file. See `progress.hpp` for the shape and why a corrupt one is
// ignored rather than fatal.

#include "timewarp/lessons/progress.hpp"

#include <algorithm>
#include <cstdio>

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QSaveFile>
#include <QStandardPaths>

namespace timewarp::lessons {
namespace {

QString data_directory() {
    const QString override = qEnvironmentVariable("TIME_WARP_DATA_DIR").trimmed();
    if (!override.isEmpty()) {
        return override;
    }
    const QString home = QStandardPaths::writableLocation(QStandardPaths::HomeLocation);
    return home + QStringLiteral("/.time_warp");
}

}  // namespace

QString ProgressStore::default_progress_path() {
    return data_directory() + QStringLiteral("/progress.json");
}

bool ProgressStore::load(const QString& curriculum_path, const QString& progress_path,
                         QString* error) {
    lessons_.clear();
    progress_.clear();
    path_ = progress_path;
    if (!load_curriculum(curriculum_path, lessons_, error)) {
        return false;
    }
    read_progress();
    return true;
}

bool ProgressStore::load_default(QString* error) {
    const QString curriculum = default_curriculum_path();
    if (curriculum.isEmpty()) {
        if (error != nullptr) {
            *error = QStringLiteral("the curriculum was not found");
        }
        return false;
    }
    return load(curriculum, default_progress_path(), error);
}

std::vector<const Lesson*> ProgressStore::for_language(lang::Language language) const {
    return lessons_for(lessons_, language);
}

const Lesson* ProgressStore::find(const std::string& lesson_id) const {
    return find_lesson(lessons_, lesson_id);
}

void ProgressStore::read_progress() {
    progress_.clear();
    QFile file{path_};
    if (!file.open(QIODevice::ReadOnly)) {
        return;
    }
    QJsonParseError parse{};
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parse);
    file.close();
    if (document.isNull() || !document.isObject()) {
        return;  // a corrupt file is worth less than the lesson it describes
    }
    const QJsonObject lessons =
        document.object().value(QStringLiteral("lessons")).toObject();
    for (const Lesson& lesson : lessons_) {
        const QJsonValue value = lessons.value(QString::fromStdString(lesson.id));
        if (!value.isObject()) {
            continue;
        }
        const QJsonObject entry = value.toObject();
        Entry stored;
        stored.checkpoint = entry.value(QStringLiteral("checkpoint")).toInt(0);
        stored.completed = entry.value(QStringLiteral("completed")).toBool(false);
        stored.hints_used = entry.value(QStringLiteral("hints_used")).toInt(0);
        progress_.emplace_back(lesson.id, stored);
    }
}

bool ProgressStore::save(QString* error) const {
    const QFileInfo info{path_};
    if (!QDir{}.mkpath(info.absolutePath())) {
        if (error != nullptr) {
            *error = QStringLiteral("cannot create %1").arg(info.absolutePath());
        }
        return false;
    }
    QJsonObject lessons;
    for (const auto& [id, entry] : progress_) {
        QJsonObject object;
        object.insert(QStringLiteral("checkpoint"), entry.checkpoint);
        object.insert(QStringLiteral("completed"), entry.completed);
        object.insert(QStringLiteral("hints_used"), entry.hints_used);
        lessons.insert(QString::fromStdString(id), object);
    }
    QJsonObject payload;
    payload.insert(QStringLiteral("version"), 1);
    payload.insert(QStringLiteral("lessons"), lessons);

    // Through `QSaveFile`: a learner's progress is worth more than a tidy log,
    // and a failed write must not leave a half-written file where a good one was.
    QSaveFile file{path_};
    if (!file.open(QIODevice::WriteOnly)) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    file.write(QJsonDocument{payload}.toJson(QJsonDocument::Indented));
    if (!file.commit()) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    return true;
}

int ProgressStore::current_index(const std::string& lesson_id) const {
    const Lesson* lesson = find(lesson_id);
    if (lesson == nullptr) {
        return 0;
    }
    const auto found = std::find_if(progress_.begin(), progress_.end(),
                                    [&](const auto& row) { return row.first == lesson_id; });
    const int index = found == progress_.end() ? 0 : found->second.checkpoint;
    return std::clamp(index, 0, static_cast<int>(lesson->checkpoints.size()));
}

LessonStatus ProgressStore::status(const std::string& lesson_id) const {
    const auto found = std::find_if(progress_.begin(), progress_.end(),
                                    [&](const auto& row) { return row.first == lesson_id; });
    if (found == progress_.end()) {
        return LessonStatus::NotStarted;
    }
    return found->second.completed ? LessonStatus::Completed : LessonStatus::InProgress;
}

int ProgressStore::hints_used(const std::string& lesson_id) const {
    const auto found = std::find_if(progress_.begin(), progress_.end(),
                                    [&](const auto& row) { return row.first == lesson_id; });
    return found == progress_.end() ? 0 : found->second.hints_used;
}

std::pair<int, int> ProgressStore::completed_count() const {
    int done = 0;
    for (const Lesson& lesson : lessons_) {
        if (status(lesson.id) == LessonStatus::Completed) {
            ++done;
        }
    }
    return {done, static_cast<int>(lessons_.size())};
}

VerifyResult ProgressStore::check(const std::string& lesson_id, int index,
                                  const std::string& source) {
    const Lesson* lesson = find(lesson_id);
    if (lesson == nullptr) {
        return VerifyResult{false, "No lesson with the id " + lesson_id + ".", {}};
    }
    if (index < 0 || index >= static_cast<int>(lesson->checkpoints.size())) {
        return VerifyResult{false, "That checkpoint is not part of this lesson.", {}};
    }
    VerifyResult result = verify_checkpoint(lesson->checkpoints[static_cast<std::size_t>(index)],
                                            lesson->language, source);
    if (!result.passed) {
        return result;
    }
    // A pass moves the learner on. The entry is created on first progress, which
    // is what makes "not started" different from "started and passed one".
    Entry entry;
    const auto found = std::find_if(progress_.begin(), progress_.end(),
                                    [&](const auto& row) { return row.first == lesson_id; });
    if (found != progress_.end()) {
        entry = found->second;
    }
    entry.checkpoint = std::max(entry.checkpoint, index + 1);
    entry.completed = entry.checkpoint >= static_cast<int>(lesson->checkpoints.size());
    if (found == progress_.end()) {
        progress_.emplace_back(lesson_id, entry);
    } else {
        found->second = entry;
    }
    // A progress write that fails must not turn a passed checkpoint into a
    // failed one: the verdict is about the learner's program. The next save
    // tries again.
    (void)save();
    return result;
}

std::string ProgressStore::hint(const std::string& lesson_id, int index, int number) {
    const Lesson* lesson = find(lesson_id);
    if (lesson == nullptr || index < 0 ||
        index >= static_cast<int>(lesson->checkpoints.size())) {
        return {};
    }
    const Checkpoint& checkpoint = lesson->checkpoints[static_cast<std::size_t>(index)];
    if (number < 0 || number >= static_cast<int>(checkpoint.hints.size())) {
        return {};
    }
    auto found = std::find_if(progress_.begin(), progress_.end(),
                              [&](const auto& row) { return row.first == lesson_id; });
    if (found == progress_.end()) {
        progress_.emplace_back(lesson_id, Entry{});
        found = std::prev(progress_.end());
    }
    found->second.hints_used = std::max(found->second.hints_used, number + 1);
    (void)save();  // see `check`: the hint is still the answer
    return checkpoint.hints[static_cast<std::size_t>(number)];
}

void ProgressStore::reset(const std::string& lesson_id) {
    progress_.erase(std::remove_if(progress_.begin(), progress_.end(),
                                   [&](const auto& row) { return row.first == lesson_id; }),
                    progress_.end());
    (void)save();  // see `check`
}

}  // namespace timewarp::lessons
