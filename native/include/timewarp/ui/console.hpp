// SPDX-License-Identifier: MIT
//
// Reading the console: what a line of output names, and what counts as a failure.
//
// Both questions are asked by the window -- a click on a line that names a source
// line goes there, and the errors-only filter has to decide which lines to keep --
// and both are answered without a widget so a test can drive them. The rules are here
// rather than in `main_window_run.cpp` because the *format* is the engine's: `TRON`
// writes `[12]`, and an error reports `at line 12`. Two places that each guessed at
// the format would disagree the first time the engine changed it.

#pragma once

#include <QString>

#include <optional>

namespace timewarp::ui {

/// The source line `line` names, if it names one.
///
/// The shapes that name a place, all three of which the engine writes:
///
///  * a trace line, which is exactly `[12]`;
///  * the stamp on an error a statement returned, `❌ [line 30] ...`;
///  * an error's own words, `at line 30:` or `line 12:` -- the colon included, so a
///    program that prints "see line 4 of the manual" does not become a link by
///    accident.
[[nodiscard]] std::optional<int> consoleLineLink(const QString& line);

/// Whether `line` is something that went wrong. The engine prefixes a failure with
/// `❌` and a caution with `⚠️`, so this is a check of the convention rather than a
/// guess at the wording.
[[nodiscard]] bool isConsoleError(const QString& line);

}  // namespace timewarp::ui
