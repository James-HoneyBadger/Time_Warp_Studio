// SPDX-License-Identifier: MIT
//
// The rows a panel is handed.
//
// These used to live beside the bridge worker, because that is what sent them.
// The lesson and classroom pages are now views over `timewarp_lessons`, and the
// run's variables and telemetry come from the engine's own runners -- so the types
// are what they always were underneath: the shape of a table, with no protocol in
// them. Keeping them here is what lets the bridge be deleted without rewriting
// the panels that display them.
//
// Nothing here names a transport, a process or a file format.

#pragma once

#include <QList>
#include <QString>
#include <QStringList>

#include "timewarp/core/qt_compat.hpp"

namespace timewarp::ui {

/// One checkpoint of a lesson, as the lesson panel shows it.
///
/// Deliberately without the worked answer: this is what the panel is given when it
/// opens, and a lesson whose answer travels with its questions has been spoiled
/// before it is read.
struct LessonCheckpoint {
    QString title;
    QString description;
    QString starter;      ///< The program the learner begins from.
    QString expectation;  ///< "lines", "turtle" or "error", for the panel's wording.
    QStringList hints;
};

/// A lesson: the learner's view of one piece of the curriculum.
struct LessonSummary {
    QString id;
    QString title;
    QString language;     ///< Display name, e.g. "CHIP-8".
    QString languageKey;  ///< Settings key, e.g. "chip8": what a document records.
    QString difficulty;
    QString description;
    QStringList objectives;
    QStringList prerequisites;
    QList<LessonCheckpoint> checkpoints;

    [[nodiscard]] int checkpointCount() const {
        return timewarp::core::intLength(checkpoints.size());
    }
};

/// Where a learner has got to in one lesson.
struct LessonProgress {
    QString id;
    QString title;
    QString language;
    QString difficulty;
    QString status;       ///< "not_started", "in_progress" or "completed".
    int checkpoint = 0;   ///< The first unfinished checkpoint.
    int checkpoints = 0;  ///< How many the lesson has.
    int hintsUsed = 0;
};

/// One learner on the class list.
struct ClassroomStudent {
    QString id;
    QString name;
    /// The folder their work is in. Reported rather than derived, because a
    /// learner whose work lives outside the class root is a fact only the class
    /// knows.
    QString directory;
};

/// How a class is shown when it is being taught from the front.
struct ClassroomPresentation {
    int fontSize = 0;
    bool fullscreen = true;
};

/// One row of the mark sheet: a learner, a lesson, and how many of its checkpoints
/// their handed-in files passed.
struct ClassroomMark {
    QString student;
    QString lesson;
    QString title;
    int handedIn = 0;
    int checkpoints = 0;
    int passed = 0;
    bool complete = false;
};

/// One row of the variables table.
///
/// The engine decides all three fields, including how the value reads: a value
/// rendered in C++ would be a second opinion about a fact the engine already
/// stated, and the two would disagree exactly where it mattered (a BASIC number
/// stored as a float).
struct VariableRow {
    QString name;
    QString type;
    QString value;
};

/// One labelled value in the telemetry HUD.
struct TelemetryRow {
    QString label;
    QString value;
};

}  // namespace timewarp::ui
