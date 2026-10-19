// SPDX-License-Identifier: MIT
//
// Run Both Panes: two programs, measured the same way, compared.
//
// This is a *benchmark*, not two runs in the window. Both programs run headless
// through the same interpreter with a counting turtle host, one after the other,
// and the table says what each produced. That is the only way the numbers can be
// attributed: with two live runners on one canvas, neither the ink nor the
// drawing commands could be divided between them afterwards.
//
// **There is no shared variable bus.** The removed implementation had an option to
// let each program write the other's variables; that made the benchmark a
// demonstration of a mechanism, and it made the comparison unreproducible, which
// is the one thing a benchmark must not be. A polyglot exercise belongs in a
// lesson, where the sharing can be explained.

#pragma once

#include <QList>
#include <QString>

#include "timewarp/ui/language.hpp"

namespace timewarp::ui {

/// One program's result. The fields are what the table prints, so a result that
/// cannot be shown is a field that does not exist.
struct BenchResult {
    QString name;
    QString language;
    bool completed = false;
    /// The program's own output, with the engine's lines joined.
    QString output;
    /// Wall-clock milliseconds spent inside the interpreter.
    double elapsed_ms = 0.0;
    /// Turtle calls that would have drawn on a canvas: pen-down moves, shapes,
    /// text and clears. The number to compare when both programs draw.
    int ink_commands = 0;
    /// Empty when the run completed without an error.
    QString error;
};

/// Runs `source` headless and measures it.
///
/// Synchronous: the interpreter's own budget bounds a runaway program, so the
/// worst case is a wait, not a hang.
[[nodiscard]] BenchResult runHeadless(Language language, const QString& name,
                                      const QString& source);

/// The comparison table, as the console shows it. One row per result.
[[nodiscard]] QString comparisonTable(const QList<BenchResult>& results);

}  // namespace timewarp::ui
