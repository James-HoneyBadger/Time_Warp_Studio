// SPDX-License-Identifier: MIT
//
// Running one program through `time_warp_check` and reading its protocol.
//
// The protocol is one JSON object per line: `canvas` with the kinds of drawing it
// received, `out` for a line of output, `err` for the engine's own complaint, and
// `done` at the end. Anything else on stdout is malformed and is reported as such
// rather than skipped, because a corrupted message and a missing feature look the
// same to a harness that ignores what it cannot parse.

#include "gates.hpp"

#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QProcess>

namespace timewarp::gates {

namespace {

/// The kinds a drawing command can have, from `CanvasCommandKind`. Only these
/// count as ink: `SetColor` is a drawing command that draws nothing.
constexpr const char* kDrawnKinds[] = {"line", "poly", "rect", "point", "ellipse", "text"};

}  // namespace

Outcome runProgram(const QString& check, const QString& language, const QString& source,
                   int timeout_ms) {
    Outcome outcome;

    QProcess process;
    process.setProcessChannelMode(QProcess::SeparateChannels);
    process.start(check, {QStringLiteral("--language"), language, QStringLiteral("--json"),
                          QStringLiteral("-")});
    if (!process.waitForStarted(10000)) {
        outcome.fatal = QStringLiteral("time_warp_check would not start: %1")
                            .arg(process.errorString());
        return outcome;
    }
    process.write(source.toUtf8());
    process.closeWriteChannel();
    if (!process.waitForFinished(timeout_ms)) {
        process.kill();
        process.waitForFinished(5000);
        outcome.fatal = QStringLiteral("timed out after %1s").arg(timeout_ms / 1000);
        return outcome;
    }

    const QString standard_out = QString::fromUtf8(process.readAllStandardOutput());
    const QString standard_error = QString::fromUtf8(process.readAllStandardError());
    QString printed;

    const QStringList lines = standard_out.split(QLatin1Char('\n'), Qt::SkipEmptyParts);
    for (const QString& line : lines) {
        if (line.trimmed().isEmpty()) {
            continue;
        }
        QJsonParseError parse_error{};
        const QJsonDocument document = QJsonDocument::fromJson(line.toUtf8(), &parse_error);
        if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
            // Not protocol: something else wrote to stdout. Reported rather than
            // skipped, because a message was corrupted.
            outcome.malformed.append(line.left(200));
            continue;
        }
        const QJsonObject message = document.object();
        const QString kind = message.value(QStringLiteral("t")).toString();
        if (kind == QLatin1String("canvas")) {
            const QJsonObject kinds = message.value(QStringLiteral("kinds")).toObject();
            for (const char* name : kDrawnKinds) {
                outcome.drawings += kinds.value(QLatin1String{name}).toInt();
            }
            outcome.actions += message.value(QStringLiteral("actions")).toInt();
        } else if (kind == QLatin1String("out")) {
            printed += message.value(QStringLiteral("text")).toString();
        } else if (kind == QLatin1String("err")) {
            outcome.errors.append(message.value(QStringLiteral("text")).toString());
        } else if (kind == QLatin1String("done")) {
            outcome.done = message;
            outcome.finished = true;
        }
    }
    // The reference harness joined the lines and ended with a newline, so a row
    // that expects output never has to care whether the last line had one.
    outcome.output = printed.isEmpty() ? QString{} : printed + QLatin1Char('\n');

    if (!outcome.finished) {
        const QStringList detail =
            standard_error.trimmed().split(QLatin1Char('\n'), Qt::SkipEmptyParts);
        outcome.fatal = QStringLiteral("the run produced no result (%1)")
                            .arg(detail.isEmpty() ? QStringLiteral("no diagnostics")
                                                  : detail.last());
    }
    return outcome;
}

}  // namespace timewarp::gates
