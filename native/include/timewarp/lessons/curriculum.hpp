// SPDX-License-Identifier: MIT
//
// The curriculum, as data the application owns.
//
// `native/share/lessons.json` is the whole course: twenty lessons, sixty
// checkpoints, a starter and a worked answer for each. It used to live inside the
// reference engine's package, which meant the IDE's Lessons panel asked Python
// what a learner's program was worth. It is shipped beside the snippets now and
// read here, so the panel's verdict and the teacher's tool are one function's
// answer.
//
// #### Why the shape is checked on load
//
// A checkpoint with no expectation, or with two, cannot be judged: it would pass
// or fail everybody, silently. A lesson with no id, or a duplicate, would share a
// learner's progress with another. Those are refused when the file is read rather
// than when a class is sitting in front of it.

#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <QString>

#include "timewarp/lang/language.hpp"

namespace timewarp::lessons {

/// What a checkpoint wants to see drawn.
///
/// Pen segments and filled shapes are counted separately, and a checkpoint may
/// constrain either, both, or neither -- `returns_home` is about where the turtle
/// finished rather than about how much it drew.
struct TurtleExpectation {
    std::optional<int> segments;
    std::optional<int> min_segments;
    std::optional<int> shapes;
    std::optional<int> min_shapes;
    bool returns_home = false;
};

/// One step of a lesson.
struct Checkpoint {
    std::string title;
    std::string description;
    std::string starter_code;
    std::string solution;
    std::vector<std::string> hints;
    /// Exactly one of these three is present; the loader refuses any other shape.
    std::optional<std::vector<std::string>> expect_lines;
    std::optional<TurtleExpectation> expect_turtle;
    std::optional<std::string> expect_error;
    /// The starter is *meant* to fail (it demonstrates an error), so
    /// `validate` must not demand that it already passes.
    bool starter_is_broken = false;
};

/// A lesson: a language, a difficulty, and its checkpoints in order.
struct Lesson {
    std::string id;
    std::string title;
    std::string description;
    std::string teacher_notes;
    std::vector<std::string> objectives;
    std::vector<std::string> prerequisites;
    std::string difficulty;  ///< beginner | intermediate | advanced
    lang::Language language = lang::Language::Basic;
    std::vector<Checkpoint> checkpoints;

    [[nodiscard]] std::string language_name() const;
};

/// Reads the curriculum. Returns false and fills `error` when the file is missing,
/// malformed, or contains a lesson or checkpoint that could not be judged.
[[nodiscard]] bool load_curriculum(const QString& path, std::vector<Lesson>& lessons,
                                   QString* error = nullptr);

/// Where the curriculum is: `TIME_WARP_LESSONS` if set, then the installed
/// `share/lessons.json`, then the checkout's `native/share/lessons.json`.
[[nodiscard]] QString default_curriculum_path();

/// The extension a handed-out file carries for a language: a learner opens it in
/// their editor, and the editor picks the language from the extension.
[[nodiscard]] std::string_view extension_for(lang::Language language) noexcept;

/// The lessons for one language, in teaching order.
[[nodiscard]] std::vector<const Lesson*> lessons_for(const std::vector<Lesson>& lessons,
                                                     lang::Language language);

/// The lesson with this id, or nullptr.
[[nodiscard]] const Lesson* find_lesson(const std::vector<Lesson>& lessons,
                                        std::string_view id);

}  // namespace timewarp::lessons
