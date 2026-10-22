// SPDX-License-Identifier: MIT

#include "timewarp/ui/outline.hpp"

#include <QRegularExpression>
#include <QStringList>

namespace timewarp::ui {

namespace {

/// A word that can be a name: a letter or underscore, then letters, digits,
/// underscores and the `-`/`?`/`!` that Logo and Scheme allow in a name.
constexpr auto kName = R"([A-Za-z_][A-Za-z0-9_\-?!]*)";

[[nodiscard]] QString firstMatch(const QString& text, const QRegularExpression& pattern) {
    const QRegularExpressionMatch match = pattern.match(text);
    return match.hasMatch() ? match.captured(1) : QString{};
}

[[nodiscard]] QList<OutlineEntry> logoOutline(const QStringList& lines) {
    QList<OutlineEntry> entries;
    static const QRegularExpression to_pattern{
        QStringLiteral("^\\s*TO\\s+(%1)\\b").arg(QString::fromLatin1(kName)),
        QRegularExpression::CaseInsensitiveOption};
    for (int index = 0; index < lines.size(); ++index) {
        const QString name = firstMatch(lines.at(index), to_pattern);
        if (!name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::Procedure});
        }
    }
    return entries;
}

[[nodiscard]] QList<OutlineEntry> basicOutline(const QStringList& lines) {
    QList<OutlineEntry> entries;
    static const QRegularExpression sub_pattern{
        QStringLiteral("^\\s*(?:\\d+\\s+)?SUB\\s+(%1)\\b").arg(QString::fromLatin1(kName)),
        QRegularExpression::CaseInsensitiveOption};
    static const QRegularExpression function_pattern{
        QStringLiteral("^\\s*(?:\\d+\\s+)?FUNCTION\\s+(%1)\\b").arg(QString::fromLatin1(kName)),
        QRegularExpression::CaseInsensitiveOption};
    // A label is a name followed by a colon, after an optional line number. EXIT
    // SUB and similar keywords are not labels because they are not followed by a
    // colon at that position.
    static const QRegularExpression label_pattern{
        QStringLiteral("^\\s*(?:\\d+\\s+)?(%1)\\s*:").arg(QString::fromLatin1(kName)),
        QRegularExpression::CaseInsensitiveOption};
    for (int index = 0; index < lines.size(); ++index) {
        const QString& line = lines.at(index);
        if (const QString name = firstMatch(line, sub_pattern); !name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::Subroutine});
            continue;
        }
        if (const QString name = firstMatch(line, function_pattern); !name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::Function});
            continue;
        }
        if (const QString name = firstMatch(line, label_pattern); !name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::Label});
        }
    }
    return entries;
}

[[nodiscard]] QList<OutlineEntry> comalOutline(const QStringList& lines) {
    QList<OutlineEntry> entries;
    static const QRegularExpression proc_pattern{
        QStringLiteral("^\\s*(?:\\d+\\s+)?PROC\\s+(%1)\\b").arg(QString::fromLatin1(kName)),
        QRegularExpression::CaseInsensitiveOption};
    for (int index = 0; index < lines.size(); ++index) {
        if (const QString name = firstMatch(lines.at(index), proc_pattern); !name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::Procedure});
        }
    }
    return entries;
}

/// REXX labels sit at column zero; an indented `end:` is inside something else.
[[nodiscard]] QList<OutlineEntry> rexxOutline(const QStringList& lines) {
    QList<OutlineEntry> entries;
    static const QRegularExpression label_pattern{
        QStringLiteral("^(%1)\\s*:").arg(QString::fromLatin1(kName))};
    for (int index = 0; index < lines.size(); ++index) {
        if (const QString name = firstMatch(lines.at(index), label_pattern); !name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::Label});
        }
    }
    return entries;
}

[[nodiscard]] QList<OutlineEntry> pilotOutline(const QStringList& lines) {
    QList<OutlineEntry> entries;
    static const QRegularExpression label_pattern{
        QStringLiteral("^\\*(%1)\\b").arg(QString::fromLatin1(kName))};
    for (int index = 0; index < lines.size(); ++index) {
        if (const QString name = firstMatch(lines.at(index), label_pattern); !name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::Label});
        }
    }
    return entries;
}

[[nodiscard]] QList<OutlineEntry> schemeOutline(const QStringList& lines) {
    QList<OutlineEntry> entries;
    static const QRegularExpression define_pattern{
        QStringLiteral("^\\s*\\(\\s*define\\s+\\(?\\s*(%1)").arg(QString::fromLatin1(kName))};
    for (int index = 0; index < lines.size(); ++index) {
        if (const QString name = firstMatch(lines.at(index), define_pattern); !name.isEmpty()) {
            entries.append(
                OutlineEntry{name, index + 1, OutlineKind::FunctionDefinition});
        }
    }
    return entries;
}

[[nodiscard]] QList<OutlineEntry> aplOutline(const QStringList& lines) {
    QList<OutlineEntry> entries;
    // APL's function definition header: `∇name`, or `∇ name`.
    static const QRegularExpression define_pattern{
        QStringLiteral("^\\s*\u2207\\s*(%1)").arg(QString::fromLatin1(kName))};
    for (int index = 0; index < lines.size(); ++index) {
        if (const QString name = firstMatch(lines.at(index), define_pattern); !name.isEmpty()) {
            entries.append(OutlineEntry{name, index + 1, OutlineKind::FunctionDefinition});
        }
    }
    return entries;
}

}  // namespace

QString outlineKindName(OutlineKind kind) {
    switch (kind) {
        case OutlineKind::Procedure:
            return QStringLiteral("procedure");
        case OutlineKind::Function:
            return QStringLiteral("function");
        case OutlineKind::Subroutine:
            return QStringLiteral("subroutine");
        case OutlineKind::Label:
            return QStringLiteral("label");
        case OutlineKind::FunctionDefinition:
            return QStringLiteral("definition");
    }
    return QStringLiteral("label");
}

QList<OutlineEntry> outlineFor(Language language, const QString& source) {
    const QStringList lines = source.split(QLatin1Char('\n'));
    switch (language) {
        case Language::Logo:
            return logoOutline(lines);
        case Language::Basic:
            return basicOutline(lines);
        case Language::Comal:
            return comalOutline(lines);
        case Language::Rexx:
            return rexxOutline(lines);
        case Language::Pilot:
            return pilotOutline(lines);
        case Language::Scheme:
            return schemeOutline(lines);
        case Language::Apl:
            return aplOutline(lines);
        // FOCAL groups are numbers, CHIP-8 has no named places, Smalltalk's
        // outline would need a real parser, and PILOT's labels are covered above.
        case Language::Focal:
        case Language::Chip8:
        case Language::Smalltalk:
        // A Forth outline is the list of colon definitions, which needs the same
        // tokeniser the interpreter has; until that is shared, no outline.
        case Language::Forth:
        case Language::Unknown:
            break;
    }
    return {};
}

}  // namespace timewarp::ui
