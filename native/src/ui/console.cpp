// SPDX-License-Identifier: MIT

#include "timewarp/ui/console.hpp"

#include <QRegularExpression>

namespace timewarp::ui {

std::optional<int> consoleLineLink(const QString& line) {
    const QString trimmed = line.trimmed();
    if (trimmed.isEmpty()) {
        return std::nullopt;
    }
    // A trace line, and nothing else on it: `[12]`. Anything looser would turn a
    // program that prints `[1] 2 3` -- an APL vector, say -- into five links.
    static const QRegularExpression trace{QStringLiteral(R"(^\[(\d+)\]$)")};
    const QRegularExpressionMatch traced = trace.match(trimmed);
    if (traced.hasMatch()) {
        return traced.captured(1).toInt();
    }
    // What the loop prefixes to an error a statement returned: `❌ [line 30] ...`.
    // This is the shape most console failures have, so a click has to understand it.
    static const QRegularExpression stamped{QStringLiteral(R"(\[line\s+(\d+)\])")};
    const QRegularExpressionMatch stamp = stamped.match(trimmed);
    if (stamp.hasMatch()) {
        return stamp.captured(1).toInt();
    }
    // An error's own words: `❌ Error at line 30: ...`, `line 12: label not found`.
    // The colon is part of the pattern on purpose: a program that prints "see line 4
    // of the manual" is printing text, not naming a place to go.
    static const QRegularExpression named{
        QStringLiteral(R"(\b(?:at line|line)\s+(\d+)\s*:)")};
    const QRegularExpressionMatch match = named.match(trimmed);
    if (match.hasMatch()) {
        return match.captured(1).toInt();
    }
    return std::nullopt;
}

bool isConsoleError(const QString& line) {
    return line.contains(QString::fromUtf8("\u274C")) ||
           line.contains(QString::fromUtf8("\u26A0\uFE0F"));
}

}  // namespace timewarp::ui
