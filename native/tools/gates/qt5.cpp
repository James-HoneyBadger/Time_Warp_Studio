// SPDX-License-Identifier: MIT
//
// The second Qt, checked rather than hoped for.
//
// One tree builds both Qts, and a call site that only one of them accepts is the
// failure that gets furthest before it is found: it compiles on Linux and macOS,
// compiles under Qt 6 on Windows, and stops the Windows 7 job -- which is the one
// job that exists to compile the other Qt. Two have shipped that way:
//
//   * `QList<bool> shown(total, false)`, a constructor Qt 5.15 does not have; and
//   * `&QComboBox::currentIndexChanged`, which Qt 5.15 overloads (int and QString)
//     so a bare pointer to it cannot be resolved, and which Qt 6 made single.
//
// The second is the one this gate can see, because it is a name rather than a
// shape. A signal Qt 5.15 overloads has to be named through `QOverload<...>::of`
// or a cast; the bare pointer is what fails. The table below is the set of such
// signals this tree can reach -- not all of Qt, which would be a copy of its
// headers, but the ones a Studio edit could plausibly write.
//
//     qt5: 4 signal(s) named only through QOverload, 348 file(s) checked

#include "gates.hpp"

#include <QDir>
#include <QFile>
#include <QProcess>
#include <QStringList>

#include <array>
#include <cstdio>

namespace timewarp::gates {

namespace {

/// A signal that Qt 5.15 declares twice and Qt 6 declares once.
///
/// The deprecated Qt 5 overloads (`activated(const QString&)`, `buttonClicked(int)`)
/// are still there and still ambiguous, which is the whole problem: a person reading
/// Qt 6's documentation sees one signature and writes the bare pointer.
struct OverloadedSignal {
    const char* owner;
    const char* signal;
};

constexpr std::array<OverloadedSignal, 5> kOverloaded{{
    {"QComboBox", "currentIndexChanged"},
    {"QComboBox", "activated"},
    {"QComboBox", "highlighted"},
    {"QProcess", "finished"},
    {"QButtonGroup", "buttonClicked"},
}};

QStringList trackedFiles(const QString& root) {
    QProcess git;
    git.setWorkingDirectory(root);
    git.start(QStringLiteral("git"), {QStringLiteral("ls-files")});
    if (!git.waitForFinished(60000) || git.exitCode() != 0) {
        return {};
    }
    QStringList files;
    for (const QString& line : QString::fromUtf8(git.readAllStandardOutput())
                                    .split(QLatin1Char('\n'), Qt::SkipEmptyParts)) {
        const QString path = line.trimmed();
        if (!path.startsWith(QLatin1String("native/src/")) &&
            !path.startsWith(QLatin1String("native/include/")) &&
            !path.startsWith(QLatin1String("native/tests/")) &&
            !path.startsWith(QLatin1String("native/tools/"))) {
            continue;
        }
        if (path.endsWith(QLatin1String(".cpp")) || path.endsWith(QLatin1String(".hpp"))) {
            files.append(path);
        }
    }
    return files;
}

/// True when the line resolves the signal rather than naming it bare.
///
/// `QOverload<int>::of(&QComboBox::currentIndexChanged)` is the fix and carries the
/// name; a `static_cast<...>` around the pointer is the other accepted spelling.
bool resolved(const QString& line) {
    return line.contains(QLatin1String("QOverload<")) ||
           line.contains(QLatin1String("qOverload<")) ||
           line.contains(QLatin1String("static_cast<"));
}

}  // namespace

int runQt5(const QStringList& arguments) {
    Q_UNUSED(arguments);
    const QString root = repositoryRoot();
    const QStringList files = trackedFiles(root);
    if (files.isEmpty()) {
        std::fprintf(stderr, "qt5: git ls-files found nothing\n");
        return 2;
    }

    QStringList failures;
    for (const QString& relative : files) {
        QFile file{QDir{root}.filePath(relative)};
        if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            failures.append(QStringLiteral("%1 cannot be read").arg(relative));
            continue;
        }
        int number = 0;
        while (!file.atEnd()) {
            ++number;
            const QString line = QString::fromUtf8(file.readLine());
            if (line.trimmed().startsWith(QLatin1String("//")) || resolved(line)) {
                continue;
            }
            for (const OverloadedSignal& signal : kOverloaded) {
                const QString bare = QStringLiteral("&%1::%2")
                                         .arg(QLatin1String(signal.owner),
                                              QLatin1String(signal.signal));
                if (line.contains(bare)) {
                    failures.append(QStringLiteral("%1:%2 names %3, which Qt 5.15 "
                                                   "overloads -- connect it through "
                                                   "QOverload<int>::of, or through "
                                                   "whichever overload it means")
                                        .arg(relative)
                                        .arg(number)
                                        .arg(bare.mid(1)));
                }
            }
        }
    }

    if (failures.isEmpty()) {
        if (!quiet()) {
            std::fprintf(out(),
                         "qt5: %d signal(s) named only through QOverload, %d file(s) checked\n",
                         static_cast<int>(kOverloaded.size()), static_cast<int>(files.size()));
        }
        return 0;
    }
    std::fprintf(out(), "qt5: %d problem(s):\n", static_cast<int>(failures.size()));
    for (const QString& failure : failures) {
        std::fprintf(out(), "  %s\n", qPrintable(failure));
    }
    std::fprintf(out(), "\nA bare pointer to an overloaded signal resolves under Qt 6 and not\n"
                        "under Qt 5.15, so it fails in the one build that exists to compile\n"
                        "the other Qt. `QOverload<int>::of(...)` says which one is meant and\n"
                        "compiles in both.\n");
    return 1;
}

}  // namespace timewarp::gates
