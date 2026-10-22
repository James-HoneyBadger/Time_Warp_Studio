// SPDX-License-Identifier: MIT
//
// The shape of the tree, checked rather than hoped for.
//
// A build file that is one 1,841-line scope, and five translation units over two
// thousand lines, are not style preferences: they are why a change to BASIC's
// `PRINT` recompiled the mixer, and why the warning policy lived at 37 call sites.
// Both were fixed once, by hand, and a hand-made fix grows back.
//
// So this gate sets the budget and names today's overruns. A file may only leave the
// waiver list, never join it: adding a name here is a decision a person makes in a
// diff, which is exactly the friction that should exist.
//
//     structure: 6 allowlisted file(s), none over budget; CMakeLists.txt 116 lines

#include "gates.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QMap>
#include <QProcess>
#include <QStringList>

#include <cstdio>

namespace timewarp::gates {

namespace {

/// A source file's budget. Small enough that reading one is a sitting, large enough
/// that a language executor fits.
constexpr int kSourceLineBudget = 2000;

/// The build file's budget: a schedule of includes and options, not the build itself.
constexpr int kBuildLineBudget = 200;

/// Files over the budget on the day the gate was written, with the reason each is
/// still one file. The list may only shrink; `structure` fails if a name on it is
/// now under budget, so a refactor that fixes one has to say so.
/// `SizeWaiver` rather than `Waiver`: the command table already has a `Waiver`, for
/// a row that cannot be tested, and one name for two ideas is how a reader ends up
/// in the wrong file.
struct SizeWaiver {
    const char* path;
    const char* reason;
};

constexpr SizeWaiver kWaivers[] = {
    {"native/src/ui/main_window.cpp",
     "the window: every section commented in it is a controller waiting to be lifted out"},
    {"native/src/lang/logo.cpp",
     "the Logo executor: lexer, evaluator, primitives and turtle in one file"},
    {"native/src/ui/retro_computer.cpp",
     "the HB-1 machine, which owns its own canvas queue, bridge and runners"},
    {"native/src/lang/basic_statements.cpp",
     "BASIC's statements: one file per family is the split"},
    {"native/src/ui/panels.cpp",
     "the docks: one file per panel is the split"},
    {"native/tests/ui_layout_tests.cpp",
     "85 cases in one file: one file per case group is the split, and the next phase"},
    {"native/tests/lang_tests.cpp",
     "every executor's cases in one file: one file per language is the split"},
};

int countLines(const QString& path) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        return -1;
    }
    int lines = 0;
    const QByteArray content = file.readAll();
    for (const char character : content) {
        if (character == '\n') {
            ++lines;
        }
    }
    return content.isEmpty() ? 0 : lines + (content.endsWith('\n') ? 0 : 1);
}

/// Every tracked source file under `native/`, with its line count.
QMap<QString, int> sourceSizes(const QString& root, const QStringList& files) {
    QMap<QString, int> sizes;
    for (const QString& relative : files) {
        if (!relative.startsWith(QLatin1String("native/src/")) &&
            !relative.startsWith(QLatin1String("native/include/")) &&
            !relative.startsWith(QLatin1String("native/tests/")) &&
            !relative.startsWith(QLatin1String("native/tools/"))) {
            continue;
        }
        if (!relative.endsWith(QLatin1String(".cpp")) && !relative.endsWith(QLatin1String(".hpp")) &&
            !relative.endsWith(QLatin1String(".inc"))) {
            continue;
        }
        sizes.insert(relative, countLines(QDir{root}.filePath(relative)));
    }
    return sizes;
}

QStringList trackedFiles(const QString& root) {
    QProcess git;
    git.setWorkingDirectory(root);
    git.start(QStringLiteral("git"), {QStringLiteral("ls-files")});
    if (!git.waitForFinished(60000) || git.exitCode() != 0) {
        return {};
    }
    QStringList files;
    for (const QString& line :
         QString::fromUtf8(git.readAllStandardOutput()).split(QLatin1Char('\n'),
                                                               Qt::SkipEmptyParts)) {
        files.append(line.trimmed());
    }
    return files;
}

}  // namespace

int runStructure(const QStringList& arguments) {
    Q_UNUSED(arguments);
    const QString root = repositoryRoot();
    const QStringList files = trackedFiles(root);
    if (files.isEmpty()) {
        std::fprintf(stderr, "structure: git ls-files found nothing\n");
        return 2;
    }

    const QMap<QString, int> sizes = sourceSizes(root, files);
    const QMap<QString, QString> waived = [] {
        QMap<QString, QString> map;
        for (const SizeWaiver& waiver : kWaivers) {
            map.insert(QString::fromUtf8(waiver.path), QString::fromUtf8(waiver.reason));
        }
        return map;
    }();

    QStringList failures;
    for (auto it = sizes.begin(); it != sizes.end(); ++it) {
        const QString path = it.key();
        const int lines = it.value();
        if (lines < 0) {
            continue;
        }
        const bool over = lines > kSourceLineBudget;
        const bool listed = waived.contains(path);
        if (over && !listed) {
            failures.append(QStringLiteral("%1 is %2 lines, over the %3-line budget")
                                .arg(path)
                                .arg(lines)
                                .arg(kSourceLineBudget));
        }
        if (!over && listed) {
            failures.append(QStringLiteral("%1 is %2 lines, under budget -- take it off the "
                                           "waiver list")
                                .arg(path)
                                .arg(lines));
        }
    }
    for (auto it = waived.begin(); it != waived.end(); ++it) {
        if (!sizes.contains(it.key())) {
            failures.append(QStringLiteral("%1 is waived but no longer exists").arg(it.key()));
        }
    }

    const QString build = QDir{root}.filePath(QStringLiteral("native/CMakeLists.txt"));
    const int build_lines = countLines(build);
    if (build_lines > kBuildLineBudget) {
        failures.append(QStringLiteral("native/CMakeLists.txt is %1 lines, over the %2-line "
                                       "budget -- a section belongs in cmake/")
                            .arg(build_lines)
                            .arg(kBuildLineBudget));
    }

    if (failures.isEmpty()) {
        std::printf("structure: %d allowlisted file(s), none over budget; CMakeLists.txt %d "
                    "lines, %d source file(s) checked\n",
                    static_cast<int>(waived.size()), build_lines,
                    static_cast<int>(sizes.size()));
        return 0;
    }
    std::printf("structure: %d problem(s):\n", static_cast<int>(failures.size()));
    for (const QString& failure : failures) {
        std::printf("  %s\n", qPrintable(failure));
    }
    std::printf("\nA file grows for one of two reasons: it does more than one thing, or it\n"
                "has not been split yet. The waiver list in tools/gates/structure.cpp names\n"
                "the second kind and may only shrink.\n");
    return 1;
}

}  // namespace timewarp::gates
