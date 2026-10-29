// SPDX-License-Identifier: MIT
//
// Every example program, through the engines the IDE runs.
//
// This answers "do the examples run". It exists because five languages once ran
// "successfully" and produced no ink at all: a program that exits cleanly and
// draws nothing is the failure users report, and it is invisible to every gate
// that only reads exit codes. So each program is run, and the ones that are
// *supposed* to draw are named and required to draw.

#include "gates.hpp"

#include "timewarp/lang/registry.hpp"

#include <string>
#include <vector>

#include <QDir>
#include <QDirIterator>
#include <QFile>
#include <QFileInfo>
#include <QMap>
#include <QSet>

#include <cstdio>

namespace timewarp::gates {

namespace {

struct LanguageExamples {
    std::string key;
    std::string extension;
};

/// The languages the corpus covers, with the extension their examples use.
///
/// Both halves come from `timewarp/lang/registry.hpp` -- the key and the first
/// extension of each row -- because a second copy of "what extension is CHIP-8" is
/// how the smoke gate and the IDE end up disagreeing about which files are examples.
std::vector<LanguageExamples> language_examples() {
    std::vector<LanguageExamples> languages;
    for (const timewarp::lang::LanguageRow& row : timewarp::lang::kLanguageTable) {
        languages.push_back(LanguageExamples{std::string{row.key},
                                             std::string{row.extensions.front()}});
    }
    return languages;
}

/// Languages expected to draw nothing *as a language*: PILOT is a dialogue
/// language whose examples ask questions and print answers.
const QSet<QString>& textOnly() {
    static const QSet<QString> languages{QStringLiteral("pilot")};
    return languages;
}

/// Examples the harness does not run: they use what the native ports added and the
/// reference never had, so running them here would measure the reference's gaps.
/// They are not skipped silently -- `lang_tests` runs each of them and asserts it
/// draws, which is the same gate on the other side of the port.
const QSet<QString>& nativeOnly() {
    static const QSet<QString> examples{QStringLiteral("basic/star_patrol.bas")};
    return examples;
}

/// The examples that must produce ink. Most examples are text programs -- a budget
/// tracker, a quiz -- and asserting that they draw would be asserting something
/// false. Adding a drawing example means adding it here: a stroke count of zero
/// for a name on this list is a failure.
const QSet<QString>& drawingExamples() {
    static const QSet<QString> examples{
        QStringLiteral("basic/math_explorer.bas"),
        QStringLiteral("apl/sensor_readings.apl"),
        QStringLiteral("basic/gradebook.bas"),
        QStringLiteral("comal/results_table.comal"),
        QStringLiteral("focal/numerics.foc"),
        QStringLiteral("basic/space_simulation.bas"),
        QStringLiteral("basic/observatory.bas"),
        QStringLiteral("logo/community_campus.logo"),
        QStringLiteral("logo/constellation.logo"),
        QStringLiteral("logo/fractal_forest.logo"),
        QStringLiteral("logo/fractal_gallery.logo"),
        QStringLiteral("logo/galaxy.logo"),
        QStringLiteral("logo/geometric_artistry.logo"),
        QStringLiteral("logo/hello.logo"),
        QStringLiteral("logo/mandala.logo"),
        QStringLiteral("logo/spirograph.logo"),
        QStringLiteral("logo/turtle_3d.logo"),
        QStringLiteral("comal/turtle_art.comal"),
        QStringLiteral("rexx/turtle_spiral.rexx"),
        QStringLiteral("scheme/turtle_polygons.scm"),
        QStringLiteral("focal/square.foc"),
        QStringLiteral("chip8/font_and_frames.ch8"),
        QStringLiteral("chip8/hires.ch8"),
        QStringLiteral("chip8/paint.ch8"),
        QStringLiteral("chip8/rewind_lab.ch8"),
        QStringLiteral("chip8/pattern_loom.ch8"),
        QStringLiteral("chip8/waveform.ch8"),
        QStringLiteral("chip8/collision_lab.ch8"),
        QStringLiteral("chip8/font_ledger.ch8"),
        QStringLiteral("chip8/hires_rings.ch8"),
        QStringLiteral("chip8/keypad_probe.ch8"),
        // The HB-1 demos: the tube is memory, so a silent run of one is a broken
        // run even though the program is BASIC.
        QStringLiteral("hb1/tube_writer.bas"),
        QStringLiteral("hb1/ram_probe.bas"),
        QStringLiteral("hb1/frame_clock.bas"),
        QStringLiteral("apl/arrays_and_turtle.apl"),
        QStringLiteral("smalltalk/shapes.st"),
        // The showcases: each draws as well as prints, so a silent run is a broken
        // run for all of them.
        QStringLiteral("apl/showcase.apl"),
        QStringLiteral("basic/showcase.bas"),
        QStringLiteral("chip8/showcase.ch8"),
        QStringLiteral("comal/showcase.comal"),
        QStringLiteral("focal/showcase.foc"),
        QStringLiteral("logo/showcase.logo"),
        QStringLiteral("pilot/showcase.pilot"),
        QStringLiteral("rexx/showcase.rexx"),
        QStringLiteral("scheme/showcase.scm"),
        QStringLiteral("smalltalk/showcase.st"),
        QStringLiteral("forth/showcase.fs"),
        QStringLiteral("forth/turtle.fs"),
    };
    return examples;
}

std::optional<QString> readProgram(const QString& path) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        return std::nullopt;
    }
    return QString::fromUtf8(file.readAll());
}

}  // namespace

int runSmoke(const QStringList& arguments) {
    bool verbose = false;
    int timeout_seconds = 90;
    for (int index = 0; index < arguments.size(); ++index) {
        const QString& argument = arguments.at(index);
        if (argument == QLatin1String("--verbose")) {
            verbose = true;
        } else if (argument == QLatin1String("--timeout") && index + 1 < arguments.size()) {
            timeout_seconds = arguments.at(++index).toInt();
        } else if (argument.startsWith(QLatin1String("--timeout="))) {
            timeout_seconds = argument.section(QLatin1Char('='), 1).toInt();
        } else {
            std::fprintf(stderr, "smoke: unknown argument %s\n", qPrintable(argument));
            return 2;
        }
    }
    if (timeout_seconds <= 0) {
        timeout_seconds = 90;
    }

    const std::optional<QString> check = locateBinary(QStringLiteral("time_warp_check"));
    if (!check.has_value()) {
        std::fprintf(stderr,
                     "time_warp_check was not found; build it with "
                     "`cmake --build native/build --target time_warp_check`\n");
        return 1;
    }

    const QString root = repositoryRoot();
    struct Counts {
        int passed = 0;
        int failed = 0;
        long long strokes = 0;
    };
    QMap<QString, Counts> counts;
    QStringList failures;
    int ran = 0;

    for (const LanguageExamples& language : language_examples()) {
        const QString key = QString::fromStdString(language.key);
        counts.insert(key, Counts{});
        const QString folder = QDir{root}.filePath(QStringLiteral("Examples"));
        QDirIterator iterator{folder,
                              {QStringLiteral("*.") + QString::fromStdString(language.extension)},
                              QDir::Files, QDirIterator::Subdirectories};
        QStringList paths;
        while (iterator.hasNext()) {
            paths.append(iterator.next());
        }
        paths.sort();

        for (const QString& path : paths) {
            // The path below `Examples`, not the bare file name: a language may keep
            // programs in more than one folder (the HB-1 demos are BASIC), and two
            // `showcase.bas` files must not answer to one identifier.
            const QString identifier =
                QFileInfo{path}.absoluteFilePath().mid(folder.size() + 1);
            if (nativeOnly().contains(identifier)) {
                continue;
            }
            ++ran;
            const std::optional<QString> source = readProgram(path);
            if (!source.has_value()) {
                ++counts[key].failed;
                failures.append(identifier + QStringLiteral(": cannot be read"));
                continue;
            }
            const Outcome outcome = runProgram(*check, key, *source, timeout_seconds * 1000);
            counts[key].strokes += outcome.drawings;

            // A program that did not run at all is the first thing to report, and
            // it is a different failure from one that ran and produced no drawing.
            if (!outcome.finished || !outcome.fatal.isEmpty()) {
                ++counts[key].failed;
                failures.append(identifier + QStringLiteral(": ") +
                                (outcome.fatal.isEmpty()
                                     ? outcome.errors.join(QStringLiteral("; "))
                                     : outcome.fatal));
                continue;
            }
            if (!outcome.errors.isEmpty()) {
                ++counts[key].failed;
                failures.append(identifier + QStringLiteral(": ") +
                                outcome.errors.first().trimmed());
                continue;
            }

            // Drawing is expected only where it is real: see `drawingExamples()`.
            const bool must_draw = drawingExamples().contains(identifier);
            if (must_draw && outcome.drawings == 0) {
                ++counts[key].failed;
                failures.append(identifier + QStringLiteral(
                                                ": ran but drew nothing, and it is "
                                                "expected to draw"));
                continue;
            }
            if (!must_draw && textOnly().contains(key) && outcome.drawings != 0) {
                ++counts[key].failed;
                failures.append(identifier +
                                QStringLiteral(": expected no drawing, got %1 strokes")
                                    .arg(outcome.drawings));
                continue;
            }

            ++counts[key].passed;
            if (verbose) {
                std::fprintf(out(), "  ok  %s  (%d strokes)\n", qPrintable(identifier),
                            outcome.drawings);
            }
        }
    }

    std::fprintf(out(), "%-10s %6s %6s %8s\n", "language", "passed", "failed", "strokes");
    for (const LanguageExamples& language : language_examples()) {
        const QString key = QString::fromStdString(language.key);
        const Counts& entry = counts[key];
        std::fprintf(out(), "%-10s %6d %6d %8lld\n", language.key.c_str(), entry.passed,
                    entry.failed, entry.strokes);
    }

    if (!failures.isEmpty()) {
        std::fprintf(stderr, "\n%d failure(s):\n", static_cast<int>(failures.size()));
        for (const QString& failure : failures) {
            std::fprintf(stderr, "  %s\n", qPrintable(failure));
        }
        return 1;
    }
    if (!quiet()) {
        std::fprintf(out(), "\nall %d example programs ran\n", ran);
    }
    return 0;
}

}  // namespace timewarp::gates
