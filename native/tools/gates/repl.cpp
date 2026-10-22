// SPDX-License-Identifier: MIT
//
// Every command row again, this time through the REPL API.
//
// The command table proves a command works inside a program. This proves the same
// command works through `repl.eval`, which is a different door: a REPL session keeps
// the language's state between submissions, and a command that only works because a
// program is parsed from the top would fail here. One `time_warp_api` process serves
// every row, because starting one per row would put the cost of the harness into the
// number it reports.
//
// A wedged request cannot be cancelled in-process, so a timeout restarts the server
// and the remaining rows still get their chance. That is the difference between one
// broken row and a run that reports nothing after it.

#include "gates.hpp"

#include <QDir>
#include <QElapsedTimer>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMap>
#include <QProcess>

#include <cstdio>

namespace timewarp::gates {

namespace {

constexpr int kRowTimeoutMs = 30 * 1000;
constexpr int kProgressEvery = 50;
constexpr int kCloseLookLimit = 30;

/// One `time_warp_api` process and one request at a time.
class Api {
public:
    explicit Api(const QString& binary) : binary_{binary} { start(); }

    Api(const Api&) = delete;
    Api& operator=(const Api&) = delete;

    ~Api() { stop(); }

    void start() {
        process_ = std::make_unique<QProcess>();
        process_->setProcessChannelMode(QProcess::ForwardedErrorChannel);
        process_->start(binary_, {});
        (void)process_->waitForStarted(10000);
    }

    void stop() {
        if (!process_) {
            return;
        }
        if (process_->state() != QProcess::NotRunning) {
            process_->terminate();
            if (!process_->waitForFinished(5000)) {
                process_->kill();
                (void)process_->waitForFinished(5000);
            }
        }
        process_.reset();
    }

    void restart() {
        stop();
        start();
    }

    /// Sends one request and reads one reply. `error` is set when the exchange did
    /// not happen at all, which is a different failure from a JSON-RPC error object.
    QJsonObject call(const QString& method, const QJsonObject& params, int timeout_ms,
                     QString* error) {
        if (!process_ || process_->state() == QProcess::NotRunning) {
            *error = QStringLiteral("the API process exited");
            return {};
        }
        QJsonObject request;
        request.insert(QStringLiteral("jsonrpc"), QStringLiteral("2.0"));
        request.insert(QStringLiteral("id"), ++next_id_);
        request.insert(QStringLiteral("method"), method);
        if (!params.isEmpty()) {
            request.insert(QStringLiteral("params"), params);
        }
        QByteArray line = QJsonDocument{request}.toJson(QJsonDocument::Compact);
        line.append('\n');
        process_->write(line);
        if (!process_->waitForBytesWritten(5000)) {
            *error = QStringLiteral("%1: the API would not read the request").arg(method);
            return {};
        }
        return readReply(method, timeout_ms, error);
    }

private:
    QJsonObject readReply(const QString& method, int timeout_ms, QString* error) {
        // A reply is one line. Reading it means waiting until the newline arrives,
        // so a partial line is kept between reads.
        QElapsedTimer clock;
        clock.start();
        for (;;) {
            const int newline = static_cast<int>(buffer_.indexOf('\n'));
            if (newline >= 0) {
                const QByteArray line = buffer_.left(newline);
                buffer_.remove(0, newline + 1);
                if (line.trimmed().isEmpty()) {
                    continue;
                }
                QJsonParseError parse_error{};
                const QJsonDocument document =
                    QJsonDocument::fromJson(line, &parse_error);
                if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
                    *error = QStringLiteral("%1: the API replied with something that is "
                                            "not a JSON object")
                                 .arg(method);
                    return {};
                }
                return document.object();
            }
            const int remaining = timeout_ms - static_cast<int>(clock.elapsed());
            if (remaining <= 0) {
                *error = QStringLiteral("%1: no response within %2s")
                             .arg(method)
                             .arg(timeout_ms / 1000);
                return {};
            }
            if (!process_->waitForReadyRead(remaining)) {
                if (process_->state() == QProcess::NotRunning) {
                    *error = QStringLiteral("%1: the API closed stdout").arg(method);
                } else {
                    *error = QStringLiteral("%1: no response within %2s")
                                 .arg(method)
                                 .arg(timeout_ms / 1000);
                }
                return {};
            }
            buffer_.append(process_->readAllStandardOutput());
        }
    }

    QString binary_;
    std::unique_ptr<QProcess> process_;
    QByteArray buffer_;
    int next_id_ = 0;
};

ReplAnswer answerFrom(const QJsonObject& reply) {
    ReplAnswer answer;
    if (reply.contains(QStringLiteral("error"))) {
        const QJsonObject error = reply.value(QStringLiteral("error")).toObject();
        answer.protocol_error = true;
        answer.protocol_message = QStringLiteral("%1: %2")
                                      .arg(error.value(QStringLiteral("code")).toInt())
                                      .arg(error.value(QStringLiteral("message")).toString());
        return answer;
    }
    const QJsonObject result = reply.value(QStringLiteral("result")).toObject();
    answer.output = result.value(QStringLiteral("output")).toString();
    answer.error = result.value(QStringLiteral("error")).toString();
    const QJsonObject drawing = result.value(QStringLiteral("drawing")).toObject();
    answer.commands = drawing.value(QStringLiteral("commands")).toInt();
    answer.actions = drawing.value(QStringLiteral("actions")).toInt();
    return answer;
}

void removeScratch(const QString& name) {
    if (name.isEmpty()) {
        return;
    }
    const QString path = QDir::current().filePath(name);
    if (QFile::exists(path)) {
        (void)QFile::remove(path);
    }
}

}  // namespace

int runRepl(const QStringList& arguments) {
    QString only_language;
    QString json_path;
    bool verbose = false;
    for (int index = 0; index < arguments.size(); ++index) {
        const QString& argument = arguments.at(index);
        if (argument == QLatin1String("--verbose")) {
            verbose = true;
        } else if (argument == QLatin1String("--language") && index + 1 < arguments.size()) {
            only_language = arguments.at(++index);
        } else if (argument.startsWith(QLatin1String("--language="))) {
            only_language = argument.section(QLatin1Char('='), 1);
        } else if (argument == QLatin1String("--json") && index + 1 < arguments.size()) {
            json_path = arguments.at(++index);
        } else if (argument.startsWith(QLatin1String("--json="))) {
            json_path = argument.section(QLatin1Char('='), 1);
        } else {
            std::fprintf(stderr, "repl: unknown argument %s\n", qPrintable(argument));
            return 2;
        }
    }

    const std::optional<QString> binary = locateBinary(QStringLiteral("time_warp_api"));
    if (!binary.has_value()) {
        std::fprintf(stderr,
                     "time_warp_gates repl: time_warp_api was not built; run `make -C native`\n");
        return 1;
    }

    QString error;
    const std::optional<CommandTable> table = loadCommandTable(&error);
    if (!table.has_value()) {
        std::fprintf(stderr, "%s\n", qPrintable(error));
        return 2;
    }
    const QString wanted = only_language.isEmpty() ? QString{} : languageKey(only_language);
    std::vector<Row> rows;
    for (const Row& row : table->rows) {
        if (wanted.isEmpty() || languageKey(row.language) == wanted) {
            rows.push_back(row);
        }
    }
    if (rows.empty()) {
        std::fprintf(stderr, "no rows for '%s'\n", qPrintable(only_language));
        return 1;
    }

    struct Bucket {
        int passed = 0;
        int failed = 0;
    };
    QMap<QString, Bucket> buckets;
    QJsonArray results;
    std::vector<std::pair<Row, std::pair<QString, ReplAnswer>>> failures;

    QElapsedTimer clock;
    clock.start();

    {
        Api api{*binary};
        int index = 0;
        for (const Row& row : rows) {
            ++index;
            QString create_error;
            QJsonObject params;
            const QString key = row.language.toLower().remove(QLatin1Char('-'));
            params.insert(QStringLiteral("language"), key);
            const QJsonObject created =
                api.call(QStringLiteral("repl.create"), params, kRowTimeoutMs, &create_error);
            QString session =
                created.value(QStringLiteral("result"))
                    .toObject()
                    .value(QStringLiteral("session"))
                    .toString();

            ReplAnswer answer;
            if (!create_error.isEmpty() || session.isEmpty()) {
                if (create_error.isEmpty()) {
                    answer.protocol_error = true;
                    answer.protocol_message =
                        QStringLiteral("repl.create returned no session");
                } else {
                    answer.protocol_error = true;
                    answer.protocol_message = create_error;
                }
                api.restart();
            } else {
                QJsonObject eval_params;
                eval_params.insert(QStringLiteral("session"), session);
                eval_params.insert(QStringLiteral("source"), row.source);
                QString eval_error;
                const QJsonObject reply = api.call(QStringLiteral("repl.eval"), eval_params,
                                                   kRowTimeoutMs, &eval_error);
                if (!eval_error.isEmpty()) {
                    answer.protocol_error = true;
                    answer.protocol_message = eval_error;
                    api.restart();
                } else {
                    answer = answerFrom(reply);
                }

                QJsonObject close_params;
                close_params.insert(QStringLiteral("session"), session);
                QString close_error;
                (void)api.call(QStringLiteral("repl.close"), close_params, 5000, &close_error);
            }

            const Verdict verdict = judgeRepl(row, answer);
            Bucket& bucket = buckets[row.language];
            (verdict.ok ? bucket.passed : bucket.failed) += 1;

            QJsonObject record;
            record.insert(QStringLiteral("language"), row.language);
            record.insert(QStringLiteral("command"), row.command);
            record.insert(QStringLiteral("source"), row.source);
            record.insert(QStringLiteral("expect"), row.expect);
            record.insert(QStringLiteral("ok"), verdict.ok);
            record.insert(QStringLiteral("why"), verdict.why);
            record.insert(QStringLiteral("output"), answer.output);
            record.insert(QStringLiteral("drawing"), answer.commands);
            record.insert(QStringLiteral("actions"), answer.actions);
            results.append(record);

            if (verdict.ok) {
                if (verbose) {
                    std::printf("  ok   %-10s %-28s %s\n", qPrintable(row.language),
                                qPrintable(row.command), qPrintable(row.expect));
                }
            } else {
                failures.emplace_back(row, std::make_pair(verdict.why, answer));
                std::printf("  FAIL %-10s %-28s %s\n", qPrintable(row.language),
                            qPrintable(row.command), qPrintable(verdict.why));
            }
            if (index % kProgressEvery == 0) {
                std::fprintf(stderr, "  ... %d/%d rows\n", index,
                             static_cast<int>(rows.size()));
            }
        }
    }

    removeScratch(table->scratch);
    const double elapsed = static_cast<double>(clock.elapsed()) / 1000.0;

    std::printf("\n%-12s %7s %7s\n", "language", "passed", "failed");
    QStringList languages = buckets.keys();
    languages.sort();
    for (const QString& language : languages) {
        const Bucket& bucket = buckets[language];
        std::printf("%-12s %7d %7d\n", qPrintable(language), bucket.passed, bucket.failed);
    }
    std::printf("rows: %d   elapsed: %.1fs\n", static_cast<int>(rows.size()), elapsed);

    if (!json_path.isEmpty()) {
        QFile file{json_path};
        if (file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
            file.write(QJsonDocument{results}.toJson(QJsonDocument::Indented));
        }
    }

    if (!failures.empty()) {
        std::printf("\n");
        int shown = 0;
        for (const auto& failure : failures) {
            if (shown++ >= kCloseLookLimit) {
                break;
            }
            std::printf("%s / %s: %s\n", qPrintable(failure.first.language),
                        qPrintable(failure.first.command),
                        qPrintable(failure.second.first));
            std::printf("    source: %s\n", qPrintable(failure.first.source));
            std::printf("    output: %s\n",
                        qPrintable(failure.second.second.output.left(160)));
        }
        if (static_cast<int>(failures.size()) > kCloseLookLimit) {
            std::printf("... and %d more\n",
                        static_cast<int>(failures.size()) - kCloseLookLimit);
        }
        return 1;
    }

    std::printf("\nall %d rows behaved through the REPL\n", static_cast<int>(rows.size()));
    return 0;
}

}  // namespace timewarp::gates
