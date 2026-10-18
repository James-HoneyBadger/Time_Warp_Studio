// SPDX-License-Identifier: MIT

#include "timewarp/ui/formatter.hpp"

#include <algorithm>

#include <QChar>
#include <QLatin1String>
#include <QStringList>

namespace timewarp::ui {
namespace {

/// True when `upper` begins with `word` as a whole word: "END IF" and "END"
/// match "END", "ENDIF" does not.
[[nodiscard]] bool startsWithWord(const QString& upper, const char* word) {
    const QLatin1String keyword{word};
    if (!upper.startsWith(keyword)) {
        return false;
    }
    if (upper.size() == keyword.size()) {
        return true;
    }
    const QChar next = upper.at(keyword.size());
    return next == QLatin1Char(' ') || next == QLatin1Char('\t') ||
           next == QLatin1Char(':') || next == QLatin1Char(',');
}

/// True when `word` appears anywhere in `upper` as a whole word.
[[nodiscard]] bool containsWord(const QString& upper, const char* word) {
    const QLatin1String keyword{word};
    int from = 0;
    while (true) {
        const int at = upper.indexOf(keyword, from);
        if (at < 0) {
            return false;
        }
        const int after = at + keyword.size();
        const bool left = at == 0 || !upper.at(at - 1).isLetterOrNumber();
        const bool right = after >= upper.size() || !upper.at(after).isLetterOrNumber();
        if (left && right) {
            return true;
        }
        from = at + 1;
    }
}

/// A line whose structure this build understands: it opens a block for the next
/// line, closes one before itself, or both (an `ELSE` sits between two).
struct Step {
    int dedent = 0;
    int indent = 0;
};

/// BASIC's multi-line `IF` ends with `THEN` and nothing after it. `IF x THEN
/// PRINT 1` is one statement and opens nothing -- the case the original got
/// wrong in the other direction, indenting the line after a single-line `IF`
/// whose `THEN` branch began with a digit.
[[nodiscard]] bool thenOpensBlock(const QString& upper) {
    const int at = upper.lastIndexOf(QLatin1String("THEN"));
    if (at < 0) {
        return false;
    }
    const int after = at + 4;
    if (after >= upper.size()) {
        return true;
    }
    const QString rest = upper.mid(after).trimmed();
    // `THEN 'note` is a comment in BASIC, so it is still a block opener.
    return rest.isEmpty() || rest.startsWith(QLatin1Char('\''));
}

/// Comment lines never carry structure, whatever words they contain: a
/// `REM FOR EVER` would otherwise open a block that never closes.
[[nodiscard]] bool isComment(Language language, const QString& upper) {
    switch (language) {
    case Language::Basic:
        return upper.startsWith(QLatin1Char('\'')) || startsWithWord(upper, "REM");
    case Language::Comal:
        return upper.startsWith(QLatin1String("//")) || upper.startsWith(QLatin1Char('!'));
    case Language::Rexx:
        return upper.startsWith(QLatin1String("/*"));
    case Language::Logo:
        return upper.startsWith(QLatin1Char(';'));
    default:
        return false;
    }
}

[[nodiscard]] Step stepFor(Language language, const QString& upper) {
    Step step;
    switch (language) {
    case Language::Basic:
        if (startsWithWord(upper, "ELSE") || startsWithWord(upper, "CASE")) {
            // ELSE/ELSEIF/CASE sit between the block's two bodies: close the one
            // above, open the one below.
            step.dedent = 1;
            step.indent = 1;
        } else if (startsWithWord(upper, "END") || startsWithWord(upper, "NEXT") ||
                   startsWithWord(upper, "WEND") || startsWithWord(upper, "LOOP")) {
            step.dedent = 1;
        } else if (startsWithWord(upper, "FOR") || startsWithWord(upper, "WHILE") ||
                   startsWithWord(upper, "DO") || startsWithWord(upper, "SUB") ||
                   startsWithWord(upper, "FUNCTION") || startsWithWord(upper, "SELECT")) {
            step.indent = 1;
        } else if (startsWithWord(upper, "IF") && thenOpensBlock(upper)) {
            step.indent = 1;
        }
        break;

    case Language::Comal:
        if (startsWithWord(upper, "ELSE") || startsWithWord(upper, "ELSIF") ||
            startsWithWord(upper, "OTHERWISE") || startsWithWord(upper, "WHEN")) {
            step.dedent = 1;
            step.indent = 1;
        } else if (startsWithWord(upper, "END") || startsWithWord(upper, "ENDIF") ||
                   startsWithWord(upper, "ENDFOR") || startsWithWord(upper, "ENDWHILE") ||
                   startsWithWord(upper, "ENDREPEAT") || startsWithWord(upper, "ENDLOOP") ||
                   startsWithWord(upper, "ENDPROC") || startsWithWord(upper, "ENDCASE") ||
                   startsWithWord(upper, "ENDSELECT") || startsWithWord(upper, "NEXT") ||
                   startsWithWord(upper, "UNTIL")) {
            step.dedent = 1;
        } else if (startsWithWord(upper, "IF")) {
            if (thenOpensBlock(upper)) {
                step.indent = 1;
            }
        } else if (startsWithWord(upper, "FOR") || startsWithWord(upper, "WHILE") ||
                   startsWithWord(upper, "REPEAT") || startsWithWord(upper, "PROC") ||
                   startsWithWord(upper, "LOOP") || startsWithWord(upper, "CASE") ||
                   startsWithWord(upper, "SELECT")) {
            step.indent = 1;
        }
        break;

    case Language::Rexx: {
        if (startsWithWord(upper, "END")) {
            step.dedent = 1;
        } else if (startsWithWord(upper, "ELSE") || startsWithWord(upper, "WHEN") ||
                   startsWithWord(upper, "OTHERWISE")) {
            step.dedent = 1;
            step.indent = 1;
        } else {
            // `DO`, `DO i = 1 TO 5`, `IF x THEN DO`, `SELECT`.
            const bool opens = startsWithWord(upper, "DO") ||
                               upper.endsWith(QLatin1String(" DO")) ||
                               startsWithWord(upper, "SELECT");
            // A block written on one line (`DO i=1 TO 3; SAY i; END`) opens and
            // closes on that line; the later `END` must not close somebody
            // else's block.
            step.indent = (opens && !containsWord(upper, "END")) ? 1 : 0;
        }
        break;
    }

    case Language::Logo: {
        if (startsWithWord(upper, "END")) {
            step.dedent = 1;
        } else if (startsWithWord(upper, "TO")) {
            // A procedure body always follows. The bracket forms do not: their
            // block is the bracket run on the line itself, handled below.
            step.indent = 1;
        }
        break;
    }

    default:
        break;
    }
    return step;
}

/// How many indentation levels a line's brackets open (`[` minus `]`). Logo
/// writes most of its blocks this way, and a balanced pair on one line must open
/// nothing.
[[nodiscard]] int bracketDelta(const QString& upper) {
    int delta = 0;
    for (const QChar c : upper) {
        if (c == QLatin1Char('[')) {
            ++delta;
        } else if (c == QLatin1Char(']')) {
            --delta;
        }
    }
    return delta;
}

/// The line number prefix of a BASIC line ("10 " in "10 FOR I = 1 TO 3"), which
/// is part of the layout and must stay where it is.
///
/// The prefix is the digits **and one separator**, never the whole run: taking
/// the run would re-read an already-indented line as "10" plus five spaces and
/// put the indentation back on top of them, so a second format added a level
/// every time it ran.
[[nodiscard]] int lineNumberPrefixLength(const QString& content) {
    int index = 0;
    while (index < content.size() && content.at(index).isDigit()) {
        ++index;
    }
    if (index == 0) {
        return 0;
    }
    // Digits only count as a line number when one separator follows; `10PRINT`
    // is a number, not a statement.
    if (index < content.size() && (content.at(index) == QLatin1Char(' ') ||
                                   content.at(index) == QLatin1Char('\t'))) {
        return index + 1;
    }
    return 0;
}

/// True for the languages whose blocks this function understands.
[[nodiscard]] bool hasBlocks(Language language) {
    return language == Language::Basic || language == Language::Comal ||
           language == Language::Rexx || language == Language::Logo;
}

}  // namespace

bool canFormat(Language language) { return hasBlocks(language); }

FormatOutcome formatSource(const QString& source, Language language, int indent_width) {
    FormatOutcome outcome;
    outcome.text = source;
    if (!hasBlocks(language)) {
        outcome.message =
            QStringLiteral("%1 has no block structure to re-indent.").arg(displayName(language));
        return outcome;
    }
    const int width = indent_width > 0 ? indent_width : 4;
    const QString unit = QString(width, QLatin1Char(' '));

    QStringList lines = source.split(QLatin1Char('\n'));
    int level = 0;
    int changed = 0;
    QStringList formatted;
    formatted.reserve(lines.size());

    for (QString line : lines) {
        const bool carriage_return = line.endsWith(QLatin1Char('\r'));
        if (carriage_return) {
            line.chop(1);
        }

        const QString stripped = line.trimmed();
        if (stripped.isEmpty()) {
            // A blank line stays blank: indentation on an empty line is
            // trailing whitespace nobody can see and every diff complains about.
            formatted << (carriage_return ? QStringLiteral("\r") : QString{});
            continue;
        }

        // Only the leading whitespace is replaced; the rest of the line is kept
        // exactly, trailing spaces and all. The invariant the tests assert is
        // that stripping both sides gives the same text.
        const int prefix_length =
            (language == Language::Basic) ? lineNumberPrefixLength(line) : 0;
        const QString prefix = prefix_length > 0 ? line.left(prefix_length) : QString{};
        const QString rest = prefix_length > 0 ? line.mid(prefix_length) : line;
        int first = 0;
        while (first < rest.size() && rest.at(first).isSpace()) {
            ++first;
        }
        const QString body = rest.mid(first);
        const QString upper = body.toUpper();

        const bool comment = isComment(language, upper);
        const Step step = comment ? Step{} : stepFor(language, upper);
        // A bracket block closes on its own line: `]` belongs at the level of
        // the line that opened it, so the close is applied *before* the line is
        // written and the open *after*. Doing it the other way round put the
        // bracket one level too deep and made a second format move it again.
        const int brackets = (language == Language::Logo && !comment) ? bracketDelta(upper) : 0;
        const int closes_now = brackets < 0 ? -brackets : 0;
        const int opens_after = brackets > 0 ? brackets : 0;

        level = std::max(0, level - step.dedent - closes_now);
        const QString rebuilt = prefix + unit.repeated(level) + body;
        formatted << (carriage_return ? rebuilt + QLatin1Char('\r') : rebuilt);
        if (rebuilt != line) {
            ++changed;
        }

        level = std::max(0, level + step.indent + opens_after);
    }

    outcome.text = formatted.join(QLatin1Char('\n'));
    outcome.changed_lines = changed;
    if (changed == 0) {
        outcome.message = QStringLiteral("Already formatted.");
    } else {
        outcome.message = changed == 1 ? QStringLiteral("Formatted 1 line.")
                                       : QStringLiteral("Formatted %1 lines.").arg(changed);
    }
    return outcome;
}

}  // namespace timewarp::ui
