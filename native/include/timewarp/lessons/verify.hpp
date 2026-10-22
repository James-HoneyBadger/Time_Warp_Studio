// SPDX-License-Identifier: MIT
//
// Judging a learner's program against a checkpoint.
//
// This is the port of `lesson_system.py`'s verifier, and it is deliberately the
// same verifier: the Lessons panel, the `teach` command and the `learn` command
// all ask this function, so a learner's verdict and a teacher's mark cannot
// disagree.
//
// The three expectations are a program's *output*, what the turtle *drew*, and a
// *reported error*. A program that fails to run is reported as itself rather than
// as a wrong answer -- "there is a typo on line 30" is a different thing to tell
// somebody than "not quite yet" -- except where the checkpoint is about
// recognising an error, in which case the failure is the answer.

#pragma once

#include <string>
#include <vector>

#include "timewarp/lang/language.hpp"
#include "timewarp/lessons/curriculum.hpp"

namespace timewarp::lessons {

/// What one run of a program produced.
struct ProgramRun {
    std::vector<std::string> output;
    int segments = 0;
    int shapes = 0;
    double turtle_x = 0.0;
    double turtle_y = 0.0;
};

/// Runs `source` headlessly through the native engine for `language`.
///
/// Nothing is drawn on a canvas: `RecordingHost` is the surface, which is what
/// makes a lesson checkable on a machine with no display. A program that waits for
/// input cannot be checked without a person, so a pending prompt is reported as an
/// error in the output rather than answered.
[[nodiscard]] ProgramRun run_program(lang::Language language, const std::string& source);

/// The outcome of a check.
struct VerifyResult {
    bool passed = false;
    std::string message;
    std::vector<std::string> output;
};

/// Runs `source` and decides whether it satisfies `checkpoint`.
[[nodiscard]] VerifyResult verify_checkpoint(const Checkpoint& checkpoint,
                                             lang::Language language,
                                             const std::string& source);

/// The lines a program printed, without the commentary the interpreters emit
/// alongside it and without a trailing blank line.
///
/// Executors are not consistent about whether an entry carries its newline, and a
/// whole-program language can return several lines in one entry, so a newline is
/// appended to any entry that lacks one. Without that, a language that omits it
/// would have its first two lines glued together and a correct program would fail.
[[nodiscard]] std::vector<std::string> printed_lines(
    const std::vector<std::string>& output);

}  // namespace timewarp::lessons
