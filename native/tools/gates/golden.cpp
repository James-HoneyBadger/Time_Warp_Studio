// SPDX-License-Identifier: MIT
//
// The ports against the frozen reference corpora.
//
// `native/tests/golden/lang_corpus.json.gz` holds the reference engine's answers to
// the language programs: what each printed, every drawing call in order, and the
// variables it left behind. This compares the native ports with those answers. The
// corpus carries the program text as well as the answer, which is what makes the
// gate survive the engine it was recorded from.
//
// It is honest about being weaker than the differential harnesses it replaced: a
// change that makes a port agree with a *wrong* recording cannot be caught here.
// An entry deliberately changed is edited by hand, in the same commit, with a
// `divergence` note saying why -- and a note that has gone stale is a failure, so a
// divergence cannot quietly become a lie.

#include "gates.hpp"

#include <QCryptographicHash>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMap>
#include <QProcess>
#include <QRegularExpression>

#include <cstdio>
#include <cstdlib>

#include <zlib.h>

namespace timewarp::gates {

namespace {

/// Language keys whose programs read hardware rather than the engine: `STICK` and
/// friends read `/dev/input/js*`, and a plugged-in joystick is not a language
/// difference. The corpus does not contain these rows, and the check skips them
/// again so an entry added by hand cannot be compared against a machine's answer.
struct HardwareMarker {
    const char* marker;
    const char* reason;
};

constexpr HardwareMarker kHardwareMarkers[] = {
    {"JOYINIT", "reads a gamepad"},
    {"STICK(", "reads a gamepad"},
    {"STRIG(", "reads a gamepad"},
};

/// One spelling for a number, so `50` and `50.0` compare equal.
QString normaliseNumbers(const QString& text) {
    static const QRegularExpression number{
        QStringLiteral(R"([+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)")};
    QString result;
    int last = 0;
    auto matches = number.globalMatch(text);
    while (matches.hasNext()) {
        const QRegularExpressionMatch match = matches.next();
        result += text.mid(last, static_cast<int>(match.capturedStart()) - last);
        // `strtod` and `%.12g` are what Python's `float()` and `.12g` do, including
        // the case that matters here: a colour like `#8E9092` reads as the number
        // 8E9092, which overflows to infinity, and the recording caught it as `inf`.
        // Formatting with `QString::number` would keep the original text instead and
        // report a difference that is the harness's, not the port's.
        const QByteArray text_utf8 = match.captured(0).toLatin1();
        char* end = nullptr;
        const double value = std::strtod(text_utf8.constData(), &end);
        if (end != nullptr && end != text_utf8.constData()) {
            char formatted[64];
            std::snprintf(formatted, sizeof formatted, "%.12g", value);
            result += QLatin1String{formatted};
        } else {
            result += match.captured(0);
        }
        last = static_cast<int>(match.capturedEnd());
    }
    result += text.mid(last);
    return result;
}

/// One vocabulary for variables on both sides. The corpus calls `7.0` a `float` and
/// shows `7.0`; the panel calls it a `number` and shows `7`, because the learner
/// wrote `7`. The comparison is on the value and on the word a person reads.
QMap<QString, QPair<QString, QString>> normaliseVariables(const QJsonObject& values) {
    QMap<QString, QPair<QString, QString>> result;
    for (auto it = values.begin(); it != values.end(); ++it) {
        const QJsonArray pair = it.value().toArray();
        const QString kind = pair.at(0).toString();
        const QString value = pair.at(1).toString();
        if (kind == QLatin1String("float") || kind == QLatin1String("number") ||
            kind == QLatin1String("int") || kind == QLatin1String("boolean")) {
            result.insert(it.key(),
                          {QStringLiteral("number"),
                           QString::number(value.toDouble(), 'g', 12)});
        } else if (kind == QLatin1String("str") || kind == QLatin1String("text")) {
            result.insert(it.key(), {QStringLiteral("text"), value});
        } else {
            result.insert(it.key(), {kind, value});
        }
    }
    return result;
}

QString digestOf(const QString& text) {
    return QString::fromLatin1(
        QCryptographicHash::hash(text.toUtf8(), QCryptographicHash::Sha256).toHex());
}

/// Reads a corpus, gzipped or not. A `.gz` file is inflated with zlib directly: Qt's
/// `qUncompress` reads the zlib wrapper, and a gzip member has a different header.
std::optional<QJsonArray> readCorpus(const QString& path, QString* error) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        *error = QStringLiteral("cannot read %1: %2").arg(path, file.errorString());
        return std::nullopt;
    }
    const QByteArray raw = file.readAll();
    QByteArray json = raw;
    if (path.endsWith(QLatin1String(".gz"))) {
        z_stream stream{};
        // `MAX_WBITS + 16` is gzip framing; without it zlib refuses the header.
        if (inflateInit2(&stream, MAX_WBITS + 16) != Z_OK) {
            *error = QStringLiteral("%1: zlib would not start").arg(path);
            return std::nullopt;
        }
        stream.next_in = reinterpret_cast<Bytef*>(const_cast<char*>(raw.constData()));
        stream.avail_in = static_cast<uInt>(raw.size());
        json.clear();
        QByteArray chunk(64 * 1024, Qt::Uninitialized);
        int status = Z_OK;
        while (status == Z_OK) {
            stream.next_out = reinterpret_cast<Bytef*>(chunk.data());
            stream.avail_out = static_cast<uInt>(chunk.size());
            status = inflate(&stream, Z_NO_FLUSH);
            if (status != Z_OK && status != Z_STREAM_END) {
                inflateEnd(&stream);
                *error = QStringLiteral("%1 is not a gzip stream").arg(path);
                return std::nullopt;
            }
            json.append(chunk.constData(),
                        static_cast<int>(chunk.size() - stream.avail_out));
            if (status == Z_STREAM_END) {
                break;
            }
        }
        inflateEnd(&stream);
    }
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(json, &parse_error);
    if (parse_error.error != QJsonParseError::NoError || !document.isArray()) {
        *error = QStringLiteral("%1 is not a JSON list: %2")
                     .arg(path, parse_error.errorString());
        return std::nullopt;
    }
    return document.array();
}

QStringList toStringList(const QJsonArray& values) {
    QStringList list;
    list.reserve(values.size());
    for (const QJsonValue& value : values) {
        list.append(value.toString());
    }
    return list;
}

/// The port's answer, parsed from `lang_cli`'s three record kinds.
struct NativeAnswer {
    QStringList output;
    QStringList drawing;
    QMap<QString, QPair<QString, QString>> variables;
    bool ok = true;
    QString error;
};

NativeAnswer runNative(const QString& cli, const QString& key, const QString& source) {
    NativeAnswer answer;
    QProcess process;
    process.start(cli, {key});
    if (!process.waitForStarted(10000)) {
        answer.ok = false;
        answer.error = QStringLiteral("lang_cli would not start");
        return answer;
    }
    process.write(source.toUtf8());
    process.closeWriteChannel();
    if (!process.waitForFinished(60000)) {
        process.kill();
        process.waitForFinished(5000);
        answer.ok = false;
        answer.error = QStringLiteral("lang_cli timed out");
        return answer;
    }
    if (process.exitCode() != 0) {
        answer.ok = false;
        answer.error = QStringLiteral("lang_cli exited %1: %2")
                           .arg(process.exitCode())
                           .arg(QString::fromUtf8(process.readAllStandardError()).trimmed());
        return answer;
    }
    QJsonObject variables;
    const QString standard_out = QString::fromUtf8(process.readAllStandardOutput());
    for (const QString& line : standard_out.split(QLatin1Char('\n'), Qt::SkipEmptyParts)) {
        const QStringList parts = line.split(QLatin1Char('\t'));
        if (parts.isEmpty()) {
            continue;
        }
        if (parts.at(0) == QLatin1String("OUT")) {
            answer.output.append(normaliseNumbers(parts.value(1)));
        } else if (parts.at(0) == QLatin1String("DRAW")) {
            answer.drawing.append(normaliseNumbers(parts.value(1)));
        } else if (parts.at(0) == QLatin1String("VAR") && parts.size() >= 4) {
            variables.insert(parts.at(1), QJsonArray{parts.at(2), parts.at(3)});
        }
    }
    answer.variables = normaliseVariables(variables);
    return answer;
}

QString quoted(const QStringList& values) {
    QStringList each;
    for (const QString& value : values) {
        each.append(QLatin1Char('\'') + value + QLatin1Char('\''));
    }
    return QLatin1Char('[') + each.join(QStringLiteral(", ")) + QLatin1Char(']');
}

QString quotedVariables(const QMap<QString, QPair<QString, QString>>& values) {
    QStringList each;
    for (auto it = values.begin(); it != values.end(); ++it) {
        each.append(QStringLiteral("'%1': ('%2', '%3')")
                        .arg(it.key(), it.value().first, it.value().second));
    }
    return QLatin1Char('{') + each.join(QStringLiteral(", ")) + QLatin1Char('}');
}

QString hardwareReason(const QString& source) {
    const QString upper = source.toUpper();
    for (const HardwareMarker& marker : kHardwareMarkers) {
        if (upper.contains(QLatin1String{marker.marker})) {
            return QLatin1String{marker.reason};
        }
    }
    return {};
}

int runSchemeHalf(const QString& root, const QString& corpus_path) {
    const std::optional<QString> cli = locateBinary(QStringLiteral("scheme_cli"));
    QString error;
    const std::optional<QJsonArray> corpus = readCorpus(corpus_path, &error);
    if (!corpus.has_value()) {
        std::fprintf(stderr, "%s\n", qPrintable(error));
        return 2;
    }
    if (!cli.has_value()) {
        std::fprintf(stderr,
                     "scheme_cli was not built; run `cmake --build native/build "
                     "--target scheme_cli`\n");
        return 2;
    }
    Q_UNUSED(root);

    int agree = 0;
    int diverged = 0;
    QStringList failures;
    for (const QJsonValue& value : *corpus) {
        const QJsonObject entry = value.toObject();
        const QString program = entry.value(QStringLiteral("program")).toString();
        if (entry.value(QStringLiteral("sha256")).toString() != digestOf(program)) {
            failures.append(
                QStringLiteral("scheme: a recorded program does not match its own digest"));
            continue;
        }
        QProcess process;
        process.start(*cli, {});
        if (!process.waitForStarted(10000)) {
            failures.append(QStringLiteral("scheme: scheme_cli would not start"));
            break;
        }
        process.write(program.toUtf8());
        process.closeWriteChannel();
        if (!process.waitForFinished(120000)) {
            process.kill();
            process.waitForFinished(5000);
            failures.append(QStringLiteral("scheme: a program timed out"));
            continue;
        }
        const QString actual = QString::fromUtf8(process.readAllStandardOutput());
        const QString recorded = entry.value(QStringLiteral("output")).toString();
        const QString note = entry.value(QStringLiteral("divergence")).toString();
        if (!note.isEmpty()) {
            ++diverged;
            if (actual == recorded) {
                failures.append(QStringLiteral("scheme: '%1'\n  documented as a divergence "
                                               "(%2) and it now agrees; remove the note")
                                    .arg(program.trimmed().left(60), note));
            }
            continue;
        }
        if (actual != recorded) {
            failures.append(QStringLiteral("scheme: '%1'\n  recorded: %2\n  port:     %3")
                                .arg(program.trimmed().left(60), recorded.trimmed(),
                                     actual.trimmed()));
            continue;
        }
        ++agree;
    }
    for (const QString& failure : failures) {
        std::printf("DIFFER %s\n", qPrintable(failure));
    }
    const int total = agree + static_cast<int>(failures.size()) + diverged;
    std::printf("scheme: %d agreed, %d differed, %d documented divergence(s) (%d programs)\n",
                agree, static_cast<int>(failures.size()), diverged, total);
    return failures.isEmpty() ? 0 : 1;
}

}  // namespace

int runGolden(const QStringList& arguments) {
    QStringList only_languages;
    bool verbose = false;
    bool scheme = false;
    bool all = false;
    QString corpus_path;
    for (int index = 0; index < arguments.size(); ++index) {
        const QString& argument = arguments.at(index);
        if (argument == QLatin1String("--verbose")) {
            verbose = true;
        } else if (argument == QLatin1String("--scheme")) {
            scheme = true;
        } else if (argument == QLatin1String("--all")) {
            all = true;
        } else if (argument == QLatin1String("--language") && index + 1 < arguments.size()) {
            only_languages.append(arguments.at(++index));
        } else if (argument.startsWith(QLatin1String("--language="))) {
            only_languages.append(argument.section(QLatin1Char('='), 1));
        } else if (argument == QLatin1String("--corpus") && index + 1 < arguments.size()) {
            corpus_path = arguments.at(++index);
        } else if (argument.startsWith(QLatin1String("--corpus="))) {
            corpus_path = argument.section(QLatin1Char('='), 1);
        } else {
            std::fprintf(stderr, "golden: unknown argument %s\n", qPrintable(argument));
            return 2;
        }
    }
    const QString root = repositoryRoot();

    if (scheme || all) {
        const QString path = corpus_path.isEmpty()
                                 ? root + QStringLiteral("/native/tests/golden/scheme_corpus.json.gz")
                                 : corpus_path;
        if (!QFile::exists(path)) {
            std::fprintf(stderr, "no Scheme corpus at %s\n", qPrintable(path));
            return 2;
        }
        const int status = runSchemeHalf(root, path);
        if (status != 0) {
            return status;
        }
        if (scheme && !all) {
            return 0;
        }
    }

    const QString lang_cli = root + QStringLiteral("/native/build/lang_cli");
    if (!QFile::exists(lang_cli)) {
        std::fprintf(stderr,
                     "lang_cli was not built; run `cmake --build native/build "
                     "--target lang_cli`\n");
        return 2;
    }
    const QString path = corpus_path.isEmpty()
                             ? root + QStringLiteral("/native/tests/golden/lang_corpus.json.gz")
                             : corpus_path;
    QString error;
    const std::optional<QJsonArray> corpus = readCorpus(path, &error);
    if (!corpus.has_value()) {
        std::fprintf(stderr, "%s\n", qPrintable(error));
        return 2;
    }

    int agree = 0;
    int diverged = 0;
    int skipped = 0;
    QStringList failures;
    QMap<QString, int> seen;
    for (const QJsonValue& value : *corpus) {
        const QJsonObject entry = value.toObject();
        const QString key = entry.value(QStringLiteral("language")).toString();
        if (!only_languages.isEmpty() && !only_languages.contains(key)) {
            continue;
        }
        seen[key] += 1;
        const QString name = entry.value(QStringLiteral("name")).toString();
        const QString source = entry.value(QStringLiteral("source")).toString();
        const QString reason = hardwareReason(source);
        if (!reason.isEmpty()) {
            std::printf("skip   %s: %s (%s)\n", qPrintable(key), qPrintable(name),
                        qPrintable(reason));
            ++skipped;
            continue;
        }
        if (entry.value(QStringLiteral("source_sha256")).toString() != digestOf(source)) {
            failures.append(QStringLiteral("%1: %2\n  the recorded source does not match its "
                                           "own digest; the entry was edited without "
                                           "re-recording")
                                .arg(key, name));
            continue;
        }
        // The recorded side is compared as it stands: it was normalised once when it
        // was recorded, and normalising both sides would compare two different
        // manglings of the same colour and call it a difference.
        const QStringList want_output =
            toStringList(entry.value(QStringLiteral("output")).toArray());
        const QStringList want_drawing =
            toStringList(entry.value(QStringLiteral("drawing")).toArray());
        const QMap<QString, QPair<QString, QString>> want_variables =
            normaliseVariables(entry.value(QStringLiteral("variables")).toObject());

        const NativeAnswer actual = runNative(lang_cli, key, source);
        if (!actual.ok) {
            failures.append(QStringLiteral("%1: %2\n  %3").arg(key, name, actual.error));
            continue;
        }
        const QString note = entry.value(QStringLiteral("divergence")).toString();
        const bool same = actual.output == want_output && actual.drawing == want_drawing &&
                          actual.variables == want_variables;
        if (!note.isEmpty()) {
            ++diverged;
            if (same) {
                failures.append(QStringLiteral("%1: %2\n  documented as a divergence (%3) "
                                               "and it now agrees; remove the note")
                                    .arg(key, name, note));
            }
            continue;
        }
        if (same) {
            ++agree;
            if (verbose) {
                std::printf("agree  %s: %s\n", qPrintable(key), qPrintable(name));
            }
            continue;
        }

        QStringList problems;
        if (actual.output != want_output) {
            problems.append(QStringLiteral("output:\n  recorded: %1\n  native:   %2")
                                .arg(quoted(want_output), quoted(actual.output)));
        }
        if (actual.drawing != want_drawing) {
            int first = 0;
            while (first < actual.drawing.size() && first < want_drawing.size() &&
                   actual.drawing.at(first) == want_drawing.at(first)) {
                ++first;
            }
            QStringList recorded_slice;
            QStringList native_slice;
            for (int offset = first; offset < first + 3 && offset < want_drawing.size();
                 ++offset) {
                recorded_slice.append(want_drawing.at(offset));
            }
            for (int offset = first; offset < first + 3 && offset < actual.drawing.size();
                 ++offset) {
                native_slice.append(actual.drawing.at(offset));
            }
            problems.append(QStringLiteral("drawing differs at %1: recorded %2 native %3")
                                .arg(first)
                                .arg(quoted(recorded_slice), quoted(native_slice)));
        }
        if (actual.variables != want_variables) {
            problems.append(QStringLiteral("variables:\n  recorded: %1\n  native:   %2")
                                .arg(quotedVariables(want_variables),
                                     quotedVariables(actual.variables)));
        }
        failures.append(QStringLiteral("%1: %2\n  %3")
                            .arg(key, name, problems.join(QStringLiteral("\n  "))));
    }

    // The languages this gate is about, in the order the native build names them.
    // Scheme is not among them: it has a corpus and a half of its own above.
    static const QStringList kPorted{QStringLiteral("basic"), QStringLiteral("pilot"),
                                     QStringLiteral("logo"),  QStringLiteral("comal"),
                                     QStringLiteral("rexx"),  QStringLiteral("focal"),
                                     QStringLiteral("chip8"), QStringLiteral("apl"),
                                     QStringLiteral("smalltalk"), QStringLiteral("forth")};
    const QStringList chosen =
        only_languages.isEmpty() ? kPorted : only_languages;

    // A port with no recorded corpus is not covered by this gate, and saying so is
    // the point: silence would read as agreement.
    for (const QString& key : chosen) {
        if (seen.value(key) == 0) {
            std::printf("note   %s: no recorded corpus -- this gate says nothing "
                        "about it; the command matrix and lang_tests do\n",
                        qPrintable(key));
        }
    }

    for (const QString& failure : failures) {
        std::printf("DIFFER %s\n", qPrintable(failure));
    }
    std::printf("\n%d agreed, %d differed, %d skipped, %d documented divergence(s) (%s)\n",
                agree, static_cast<int>(failures.size()), skipped, diverged,
                qPrintable(chosen.join(QStringLiteral(", "))));
    return failures.isEmpty() ? 0 : 1;
}

}  // namespace timewarp::gates
