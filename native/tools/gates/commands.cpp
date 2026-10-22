// SPDX-License-Identifier: MIT
//
// Every command row, one program each, through the engines.
//
// The question is the finest the project asks: *does each command do something?*
// A command must produce output, or drawing, or a reported error. Silence is the
// failure mode users report as "nothing happens", and it is invisible to every
// other gate: a program that quietly does nothing still exits zero.
//
// The completeness half is the other reason this exists. Against the commands the
// executors actually dispatch on, everything is exercised by a row or waived with a
// reason, so a command that exists and is never tested is a number rather than an
// oversight.

#include "gates.hpp"
#include "inventory.hpp"

#include <QDir>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMap>
#include <QSet>

#include <cstdio>

namespace timewarp::gates {

namespace {

constexpr int kRowTimeoutMs = 20 * 1000;
constexpr int kCloseLookLimit = 20;

/// The label is prose ("IF / ELSE / END IF"), so the command it is filed under is
/// its first word; `also` catches the rest. Compared with case and punctuation
/// removed, so `END` and `END:` are the same label.
QString normaliseLabel(const QString& literal) {
    QString stripped = literal.trimmed();
    while (stripped.startsWith(QLatin1Char(':'))) {
        stripped = stripped.mid(1).trimmed();
    }
    QString collapsed;
    bool in_space = false;
    for (const QChar character : stripped) {
        if (character.isSpace()) {
            if (!collapsed.isEmpty()) {
                in_space = true;
            }
            continue;
        }
        if (in_space) {
            collapsed.append(QLatin1Char(' '));
            in_space = false;
        }
        collapsed.append(character);
    }
    return collapsed.toUpper();
}

QSet<QString> coveredLabels(const std::vector<Row>& rows, const std::vector<Waiver>& waivers) {
    QSet<QString> effective;
    for (const Row& row : rows) {
        const QStringList words = row.command.split(QLatin1Char(' '), Qt::SkipEmptyParts);
        if (!words.isEmpty()) {
            effective.insert(normaliseLabel(words.first()));
        }
        effective.insert(normaliseLabel(row.command));
        for (const QString& extra : row.also) {
            effective.insert(normaliseLabel(extra));
        }
    }
    for (const Waiver& waiver : waivers) {
        effective.insert(normaliseLabel(waiver.command));
    }
    return effective;
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

int runCommands(const QStringList& arguments) {
    QString only_language;
    QString json_path;
    bool verbose = false;
    bool explain = false;
    for (int index = 0; index < arguments.size(); ++index) {
        const QString& argument = arguments.at(index);
        if (argument == QLatin1String("--verbose")) {
            verbose = true;
        } else if (argument == QLatin1String("--explain")) {
            explain = true;
        } else if (argument == QLatin1String("--language") && index + 1 < arguments.size()) {
            only_language = arguments.at(++index);
        } else if (argument.startsWith(QLatin1String("--language="))) {
            only_language = argument.section(QLatin1Char('='), 1);
        } else if (argument == QLatin1String("--json") && index + 1 < arguments.size()) {
            json_path = arguments.at(++index);
        } else if (argument.startsWith(QLatin1String("--json="))) {
            json_path = argument.section(QLatin1Char('='), 1);
        } else {
            std::fprintf(stderr, "commands: unknown argument %s\n", qPrintable(argument));
            return 2;
        }
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

    const std::optional<QString> check = locateBinary(QStringLiteral("time_warp_check"));
    if (!check.has_value()) {
        std::fprintf(stderr,
                     "time_warp_check was not found; build it with "
                     "`cmake --build native/build --target time_warp_check`\n");
        return 1;
    }

    struct Bucket {
        int passed = 0;
        int failed = 0;
    };
    QMap<QString, Bucket> by_language;
    QJsonArray results;
    std::vector<std::pair<Row, std::pair<QString, Outcome>>> failures;

    for (const Row& row : rows) {
        const Outcome outcome =
            runProgram(*check, row.language, row.source, kRowTimeoutMs);
        const Verdict verdict = judgeProgram(row, outcome);
        Bucket& bucket = by_language[row.language];
        (verdict.ok ? bucket.passed : bucket.failed) += 1;

        QJsonObject record;
        record.insert(QStringLiteral("language"), row.language);
        record.insert(QStringLiteral("command"), row.command);
        record.insert(QStringLiteral("source"), row.source);
        record.insert(QStringLiteral("expect"), row.expect);
        record.insert(QStringLiteral("ok"), verdict.ok);
        record.insert(QStringLiteral("why"), verdict.why);
        results.append(record);

        if (verdict.ok) {
            if (verbose) {
                std::fprintf(out(), "  ok   %-6s %-16s %s\n", qPrintable(row.language),
                            qPrintable(row.command), qPrintable(row.expect));
            }
        } else {
            failures.emplace_back(row, std::make_pair(verdict.why, outcome));
            std::fprintf(out(), "  FAIL %-6s %-16s %s\n", qPrintable(row.language),
                        qPrintable(row.command), qPrintable(verdict.why));
        }
        if (explain) {
            // Observed, not assumed: this is how a new row's expectation gets
            // written down instead of guessed.
            QString seen = outcome.output;
            seen.replace(QLatin1Char('\n'), QChar(0x23CE));
            seen = seen.trimmed().left(70);
            QStringList notes;
            if (!seen.isEmpty()) {
                notes.append(QStringLiteral("out='%1'").arg(seen));
            }
            if (outcome.drawings != 0) {
                notes.append(QStringLiteral("draw=%1").arg(outcome.drawings));
            }
            if (!outcome.errors.isEmpty()) {
                notes.append(
                    QStringLiteral("err='%1'").arg(outcome.errors.first().left(40)));
            }
            std::fprintf(out(), "       %-18s expect=%-14s %s\n", qPrintable(row.command),
                        qPrintable(row.expect),
                        qPrintable(notes.isEmpty() ? QStringLiteral("silent")
                                                   : notes.join(QStringLiteral("  "))));
        }
    }

    removeScratch(table->scratch);

    std::fprintf(out(), "\n%-10s %7s %7s\n", "language", "passed", "failed");
    QStringList languages = by_language.keys();
    languages.sort();
    for (const QString& language : languages) {
        const Bucket& bucket = by_language[language];
        std::fprintf(out(), "%-10s %7d %7d\n", qPrintable(language), bucket.passed, bucket.failed);
    }
    std::fprintf(out(), "rows: %d   waived: %d\n", static_cast<int>(rows.size()),
                static_cast<int>(table->waivers.size()));

    // The completeness check. Read the numbers with their scope in mind: the table
    // covers *statements*, and the inventory also sees built-in functions and, for
    // Smalltalk, the selectors its class library registers -- none of which have or
    // want a row, because `lang_tests` exercises them through the programs that
    // call them. The count is printed rather than suppressed: a clean bill from a
    // blind spot is worse than a number that needs a sentence of explanation.
    if (wanted.isEmpty()) {
        const QMap<QString, inventory::Entry> found = inventory::collect();
        const QSet<QString> effective = coveredLabels(table->rows, table->waivers);
        std::fprintf(out(), "\n  Coverage of the *statement* table against what the executors "
                    "dispatch on.\n");
        std::fprintf(out(), "  Functions and library selectors are counted here too, and are "
                    "covered by lang_tests.\n");
        for (auto it = found.cbegin(); it != found.cend(); ++it) {
            QSet<QString> missing;
            for (const QString& name : it.value().implemented()) {
                if (!effective.contains(normaliseLabel(name))) {
                    missing.insert(name);
                }
            }
            std::fprintf(out(), "  %s: %d command-shaped candidate(s) with no row (statements "
                        "only are rows)\n",
                        qPrintable(it.key()), static_cast<int>(missing.size()));
            if (verbose) {
                QStringList names = missing.values();
                names.sort();
                for (const QString& name : names) {
                    std::fprintf(out(), "      %s\n", qPrintable(name));
                }
            }
        }
    }

    if (!json_path.isEmpty()) {
        const QByteArray report = QJsonDocument{results}.toJson(QJsonDocument::Indented);
        if (json_path == QLatin1String("-")) {
            // The shared `--json -` means stdout, and this is the one gate with a
            // per-row report of its own.
            std::fwrite(report.constData(), 1, static_cast<std::size_t>(report.size()), stdout);
        } else {
            QFile file{json_path};
            if (file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
                file.write(report);
            }
        }
    }

    if (!failures.empty()) {
        std::fprintf(out(), "\n");
        int shown = 0;
        for (const auto& failure : failures) {
            if (shown++ >= kCloseLookLimit) {
                break;
            }
            std::fprintf(out(), "%s / %s: %s\n", qPrintable(failure.first.language),
                        qPrintable(failure.first.command),
                        qPrintable(failure.second.first));
            std::fprintf(out(), "    source: %s\n", qPrintable(failure.first.source));
            std::fprintf(out(), "    output: %s\n",
                        qPrintable(failure.second.second.output.left(120)));
        }
        if (static_cast<int>(failures.size()) > kCloseLookLimit) {
            std::fprintf(out(), "... and %d more\n",
                        static_cast<int>(failures.size()) - kCloseLookLimit);
        }
        return 1;
    }

    if (!quiet()) {
        std::fprintf(out(), "\nall %d command rows behaved\n", static_cast<int>(rows.size()));
    }
    return 0;
}

}  // namespace timewarp::gates
