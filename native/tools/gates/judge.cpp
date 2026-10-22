// SPDX-License-Identifier: MIT
//
// The two verdicts: did this command do what its row says it should?
//
// One judge reads the program protocol (`time_warp_check --json`) and one reads a
// REPL answer (`repl.eval`), because the two carry their evidence in different
// places: a run has `canvas` messages and a `done` object, a REPL answer has the
// drawing counts inline. The rules are otherwise the same, and they live here
// together so a change to what counts as "nothing happened" cannot be made in one
// harness and forgotten in the other.

#include "gates.hpp"

#include <QJsonObject>
#include <QRegularExpression>

namespace timewarp::gates {

namespace {

/// The project's error convention: interpreter errors come back as output lines
/// prefixed with this, not as protocol `err` messages. A harness that only watched
/// `err` would call every reported error a silent failure.
constexpr QChar kErrorMarker{0x274C};

/// What an optional backend says when it is not installed. A row expecting
/// `optional` accepts the refusal as an acknowledgement: the command ran and told
/// the truth, rather than pretending a speaker moved when there is none.
bool isOptionalRefusal(const QString& detail) {
    static const QRegularExpression refusal{
        QStringLiteral("speech synthesis (?:is )?not available|not installed|"
                       "no .{0,20}backend available"),
        QRegularExpression::CaseInsensitiveOption};
    return refusal.match(detail).hasMatch();
}

/// The first line of `output` that carries the error marker.
QString firstErrorLine(const QString& output) {
    for (const QString& line : output.split(QLatin1Char('\n'))) {
        if (line.contains(kErrorMarker)) {
            return line;
        }
    }
    return {};
}

QString trimmedTo(const QString& text, int limit = 120) {
    const QString trimmed = text.trimmed();
    return trimmed.size() <= limit ? trimmed : trimmed.left(limit);
}

/// The first thing that went wrong, from the two places an engine reports one.
QString failureDetail(const QStringList& errors, const QJsonObject& done,
                      const QString& output) {
    for (const QString& error : errors) {
        if (!error.trimmed().isEmpty()) {
            return error;
        }
    }
    const QString from_done = done.value(QStringLiteral("error")).toString();
    if (!from_done.trimmed().isEmpty()) {
        return from_done;
    }
    return firstErrorLine(output);
}

constexpr int kDetailLimit = 120;

}  // namespace

Verdict judgeProgram(const Row& row, const Outcome& outcome) {
    if (!outcome.malformed.isEmpty()) {
        return {false, QStringLiteral("the worker emitted a line that is not JSON")};
    }
    if (!outcome.finished) {
        return {false, QStringLiteral("no 'done' message: the run did not finish")};
    }

    const QString& output = outcome.output;
    const bool reported = !outcome.errors.isEmpty() ||
                          !outcome.done.value(QStringLiteral("error")).toString().isEmpty() ||
                          output.contains(kErrorMarker);
    const bool silent = output.trimmed().isEmpty() &&
                        outcome.drawings + outcome.actions == 0 && !reported;
    const QString expect = row.expect;

    if (expect == QLatin1String("error")) {
        if (!reported) {
            return {false, QStringLiteral("expected a reported error, got silence")};
        }
        return {true, {}};
    }

    if (expect == QLatin1String("optional")) {
        if (reported) {
            const QString detail = failureDetail(outcome.errors, outcome.done, output);
            if (isOptionalRefusal(detail)) {
                return {true, {}};
            }
            return {false, QStringLiteral("optional backend failed unexpectedly: %1")
                               .arg(trimmedTo(detail, kDetailLimit))};
        }
        if (silent) {
            return {false,
                    QStringLiteral("the command produced nothing: no output, no drawing, "
                                   "no error")};
        }
        return {true, {}};
    }

    if (reported) {
        // A row that expects the command to work cannot pass while the engine
        // reported an error alongside it: a row used to pass on the strength of an
        // error message that happened to contain the right words.
        return {false, QStringLiteral("reported an error while expected to work: %1")
                           .arg(trimmedTo(failureDetail(outcome.errors, outcome.done, output),
                                          kDetailLimit))};
    }

    if (outcome.done.contains(QStringLiteral("ok")) &&
        !outcome.done.value(QStringLiteral("ok")).toBool(true)) {
        return {false, QStringLiteral("failed: %1")
                           .arg(trimmedTo(failureDetail(outcome.errors, outcome.done, output),
                                          kDetailLimit))};
    }

    if (silent) {
        return {false, QStringLiteral("the command produced nothing: no output, no "
                                      "drawing, no error")};
    }

    if (expect == QLatin1String("draws") && outcome.drawings + outcome.actions == 0) {
        return {false, QStringLiteral("expected drawing, got none")};
    }

    if (expect.startsWith(QLatin1String("prints"))) {
        const QString wanted = expect.section(QLatin1Char(':'), 1);
        if (!wanted.isEmpty() && !output.contains(wanted)) {
            return {false, QStringLiteral("expected output containing '%1', got '%2'")
                               .arg(wanted, output.left(80))};
        }
        if (output.trimmed().isEmpty()) {
            return {false, QStringLiteral("expected output, got none")};
        }
    }

    return {true, {}};
}

Verdict judgeRepl(const Row& row, const ReplAnswer& answer) {
    if (answer.protocol_error) {
        return {false, QStringLiteral("protocol error %1").arg(answer.protocol_message)};
    }

    const QString& output = answer.output;
    const bool reported =
        !answer.error.trimmed().isEmpty() || output.contains(kErrorMarker);
    const bool silent =
        output.trimmed().isEmpty() && answer.commands == 0 && answer.actions == 0 && !reported;
    const QString expect = row.expect;

    if (expect == QLatin1String("error")) {
        if (!reported) {
            return {false, QStringLiteral("expected a reported error, got silence")};
        }
        return {true, {}};
    }

    // "any" means any observable result, including a reported error: an optional
    // backend such as speech synthesis is allowed to say it is unavailable.
    if (expect == QLatin1String("any")) {
        if (silent) {
            return {false,
                    QStringLiteral("the command produced nothing: no output, no drawing, "
                                   "no error")};
        }
        return {true, {}};
    }

    if (expect == QLatin1String("optional")) {
        if (silent) {
            return {false,
                    QStringLiteral("the command produced nothing: no output, no drawing, "
                                   "no error")};
        }
        if (reported) {
            const QString detail = answer.error.trimmed().isEmpty()
                                       ? firstErrorLine(output)
                                       : answer.error;
            if (isOptionalRefusal(detail)) {
                return {true, {}};
            }
            return {false, QStringLiteral("optional backend failed unexpectedly: %1")
                               .arg(trimmedTo(detail, kDetailLimit))};
        }
        return {true, {}};
    }

    if (reported) {
        const QString detail = answer.error.trimmed().isEmpty() ? firstErrorLine(output)
                                                               : answer.error;
        return {false, QStringLiteral("reported an error while expected to work: %1")
                           .arg(trimmedTo(detail, kDetailLimit))};
    }

    if (silent) {
        return {false, QStringLiteral("the command produced nothing: no output, no "
                                      "drawing, no error")};
    }

    if (expect == QLatin1String("draws") && answer.commands == 0 && answer.actions == 0) {
        return {false, QStringLiteral("expected drawing, got none")};
    }

    if (expect.startsWith(QLatin1String("prints"))) {
        const QString wanted = expect.section(QLatin1Char(':'), 1);
        if (!wanted.isEmpty() && !output.contains(wanted)) {
            return {false, QStringLiteral("expected output containing '%1', got '%2'")
                               .arg(wanted, output.left(100))};
        }
        if (output.trimmed().isEmpty()) {
            return {false, QStringLiteral("expected output, got none")};
        }
    }

    return {true, {}};
}

}  // namespace timewarp::gates
