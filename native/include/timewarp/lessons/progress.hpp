// SPDX-License-Identifier: MIT
//
// Where a learner got to.
//
// The curriculum is the same for everybody; this is the one file that is not.
// It holds, per lesson, the checkpoint they are on, whether they finished, and
// how many hints they have opened -- the shape `~/.time_warp/progress.json` has
// had since the lessons existed, so a learner's place survives this port.
//
// #### An unreadable file is reported, not fatal
//
// A progress file that cannot be parsed is worth less than the lesson it
// describes, and refusing to start class because it is corrupt is the worse
// failure. It is ignored (and the next save replaces it) rather than thrown.

#pragma once

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <QString>

#include "timewarp/lessons/curriculum.hpp"
#include "timewarp/lessons/verify.hpp"

namespace timewarp::lessons {

enum class LessonStatus { NotStarted, InProgress, Completed };

/// A curriculum and one learner's place in it.
class ProgressStore {
public:
    /// Loads the curriculum, then the progress file. Returns false (and leaves
    /// `store` unusable) when the curriculum cannot be read.
    [[nodiscard]] bool load(const QString& curriculum_path, const QString& progress_path,
                            QString* error = nullptr);

    /// The same, with the default paths.
    [[nodiscard]] bool load_default(QString* error = nullptr);

    /// `$TIME_WARP_DATA_DIR/progress.json`, or `~/.time_warp/progress.json`.
    [[nodiscard]] static QString default_progress_path();

    [[nodiscard]] const std::vector<Lesson>& lessons() const noexcept { return lessons_; }
    [[nodiscard]] std::vector<const Lesson*> for_language(lang::Language language) const;

    /// The first checkpoint the learner has not finished, clamped to the lesson.
    [[nodiscard]] int current_index(const std::string& lesson_id) const;
    [[nodiscard]] LessonStatus status(const std::string& lesson_id) const;
    [[nodiscard]] int hints_used(const std::string& lesson_id) const;
    /// (lessons completed, lessons in the curriculum)
    [[nodiscard]] std::pair<int, int> completed_count() const;

    /// Checks `source` against checkpoint `index`, and remembers a pass: the
    /// learner moves to the next checkpoint, or the lesson is marked finished.
    [[nodiscard]] VerifyResult check(const std::string& lesson_id, int index,
                                     const std::string& source);

    /// The `number`th hint (0-based), counted as used. An empty string when there
    /// are no more hints, so a caller can say "that is all of them".
    [[nodiscard]] std::string hint(const std::string& lesson_id, int index, int number);

    /// Back to the beginning of one lesson. The curriculum is untouched.
    void reset(const std::string& lesson_id);

    /// Writes the progress file. Called by `check` and `reset`; public because a
    /// caller that changes several lessons wants one write.
    [[nodiscard]] bool save(QString* error = nullptr) const;

private:
    struct Entry {
        int checkpoint = 0;
        bool completed = false;
        int hints_used = 0;
    };

    [[nodiscard]] const Lesson* find(const std::string& lesson_id) const;
    void read_progress();

    std::vector<Lesson> lessons_;
    QString path_;
    std::vector<std::pair<std::string, Entry>> progress_;
};

}  // namespace timewarp::lessons
