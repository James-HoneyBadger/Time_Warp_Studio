// SPDX-License-Identifier: MIT
//
// What the HB-1 window's translation units share.
//
// `retro_computer.cpp` was 2,472 lines of `RetroComputerWindow` methods with two
// anonymous namespaces of helpers in the middle of them. The class is declared in
// `retro_computer.hpp`, so only the helpers needed a home: they live here, in a
// named `retro_detail` namespace rather than an anonymous one, because several
// translation units call them now.
//
// Internal to the HB-1 window: nothing outside `src/ui/retro_computer*.cpp`
// includes it.

#pragma once

#include "timewarp/ui/retro_computer.hpp"

#include <limits>

#include <QColor>
#include <QMap>
#include <QRegularExpression>
#include <QString>
#include <QStringList>

namespace timewarp::ui::retro_detail {





/// The part of a ROM a user sees and types at.
struct RomInfo {
    const char* version = "V1.0";
    const char* prompt = ">";
    const char* note = "";
};

[[nodiscard]] RomInfo romFor(Language language);

[[nodiscard]] QString modeName(RetroComputerWindow::ScreenMode mode);

/// The first word of a statement, uppercased. The machine's own words are
/// matched on this rather than on the whole line so `CLS` and `cls` agree.
[[nodiscard]] QString headWord(const QString& command);

/// The machine's stylesheet for one palette. Written with tokens and replaced
/// rather than `.arg()`, because a 100-line stylesheet with twenty placeholders is
/// a place where a missing argument silently shifts every colour after it.
[[nodiscard]] QString machineStyleSheet(const RetroPalette& palette);

/// The canvas and the native engines take a surface colour as a plain triplet.
[[nodiscard]] timewarp::lang::Rgb toRgb(const QColor& colour);

/// Everything after the command word, split on commas outside quotes, with the
/// quotes removed: `RENAME "A","B"` and `RENAME A,B` are the same request.
[[nodiscard]] QStringList dosArguments(const QString& command);

/// The first argument of a command, or an empty string: `SAVE "X"` and `SAVE X`
/// are the same request.
[[nodiscard]] QString firstArgument(const QString& command);

/// The name at the end of `SAVE MACHINE "N"` or `SAVEMACHINE "N"`.
///
/// `dosArguments` reads from the second word, which for the two-word spelling is
/// `MACHINE` itself -- so the volume this wrote was called `MACHINE.hb1` and the
/// name that was typed went nowhere.
[[nodiscard]] QString machineArgument(const QString& command);

/// The catalogue's type column: the ROM the file belongs to, in capitals.
[[nodiscard]] QString dosTypeName(const QString& language_key);


/// One line of a listing, matched the same way the machine stores one.
[[nodiscard]] QRegularExpression numberedLine();

/// A line range as the period's machines spelled it: `20`, `20-30`, `20-`, `-30`.
struct LineRange {
    bool valid = false;
    int first = 0;
    int last = std::numeric_limits<int>::max();
};

[[nodiscard]] LineRange parseLineRange(const QString& text);


/// Rewrites the line numbers a statement jumps to, leaving everything else alone.
///
/// String literals are copied verbatim and a comment stops the rewrite, because
/// `PRINT "GOTO 10"` and `REM GOTO 10` are not jumps. A number that names a line
/// the program does not have is counted in `dangling` and left as it is.
[[nodiscard]] QString renumberTargets(const QString& body, const QMap<int, int>& map,
                                      int* dangling);

}  // namespace timewarp::ui::retro_detail
