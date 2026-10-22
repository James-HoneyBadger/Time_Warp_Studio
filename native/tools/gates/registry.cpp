// SPDX-License-Identifier: MIT
//
// One fact, one home: the language table, checked rather than hoped for.
//
// The eleven languages used to be described twice -- a display name, a key and two
// extensions in `src/ui/language.cpp`, and switches over the same names in
// `src/lang/language.cpp` -- and CHIP-8's `.ch8`/`.c8` were written out in four
// files besides. Every copy was plausible and one of them was going to be wrong
// eventually: a language that answers to one extension in the file dialog and
// another in the smoke gate is a file a person cannot open.
//
// They are one table now (`timewarp/lang/registry.hpp`) and this gate keeps it that
// way: a source file that names a language's display name *and* its key is a table
// by another name, and a table is what must not come back.

#include "gates.hpp"

#include "timewarp/lang/registry.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QProcess>
#include <QStringList>
#include <QTextStream>

#include <cstdio>

namespace timewarp::gates {

namespace {

/// Files that are allowed to spell a language out in full:
///
///  * the registry itself, which is the one home;
///  * `src/lang/language.cpp`, which forwards to it -- its only literals are in the
///    executor table, which maps a language to a function and is not a fact about
///    the language;
///  * tests, which write file names like `one.logo` and assert what they mean;
///  * documentation and the JSON data, where a name is prose or a key by protocol.
bool mayNameALanguage(const QString& relative) {
    static const QStringList allowed{
        QStringLiteral("native/include/timewarp/lang/registry.hpp"),
    };
    if (allowed.contains(relative)) {
        return true;
    }
    return relative.startsWith(QLatin1String("native/tests/")) ||
           relative.startsWith(QLatin1String("native/share/")) ||
           relative.startsWith(QLatin1String("docs/")) ||
           relative.startsWith(QLatin1String("Examples/")) ||
           relative.startsWith(QLatin1String("packaging/")) ||
           relative.endsWith(QLatin1String(".md")) ||
           relative.endsWith(QLatin1String(".json")) ||
           relative.endsWith(QLatin1String(".yml"));
}

/// The source files the rule applies to: C++ and the build files, which is where a
/// second table would be written.
bool isCheckedSource(const QString& relative) {
    return relative.startsWith(QLatin1String("native/")) &&
           (relative.endsWith(QLatin1String(".cpp")) ||
            relative.endsWith(QLatin1String(".hpp")) ||
            relative.endsWith(QLatin1String(".inc")) ||
            relative.endsWith(QLatin1String("CMakeLists.txt")));
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

/// The line with its comment removed: a `//` tail, or a line that is entirely a
/// block comment. A row of a table is code; the prose around it is not.
QString withoutComment(const QString& line) {
    const QString trimmed = line.trimmed();
    if (trimmed.startsWith(QLatin1String("//")) || trimmed.startsWith(QLatin1String("/*")) ||
        trimmed.startsWith(QLatin1Char('*'))) {
        return {};
    }
    const qsizetype slash = line.indexOf(QLatin1String("//"));
    return slash >= 0 ? line.left(slash) : line;
}

/// Whether *text* contains *literal* as a whole quoted string: `"logo"` but not
/// `"logo_file"`. Quoted is what a table row or a comparison looks like; an
/// unquoted mention is prose or an identifier.
bool mentions(const QString& text, const QString& literal) {
    return text.contains(QLatin1Char('"') + literal + QLatin1Char('"')) ||
           text.contains(QLatin1Char('\'') + literal + QLatin1Char('\''));
}

}  // namespace

int runRegistry(const QStringList& arguments) {
    Q_UNUSED(arguments);
    const QString root = repositoryRoot();
    const QStringList files = trackedFiles(root);
    if (files.isEmpty()) {
        std::fprintf(stderr, "registry: git ls-files found nothing\n");
        return 2;
    }

    QStringList failures;
    int checked = 0;
    for (const QString& relative : files) {
        if (!isCheckedSource(relative) || mayNameALanguage(relative)) {
            continue;
        }
        QFile file{QDir{root}.filePath(relative)};
        if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            continue;
        }
        const QStringList lines = QString::fromUtf8(file.readAll()).split(QLatin1Char('\n'));
        ++checked;
        for (int number = 0; number < lines.size(); ++number) {
            // A table row is one line naming a language's display name and its key;
            // prose in a comment is not, and neither is a help string that happens to
            // quote both. So the comment is removed first and the rest of the line is
            // asked.
            const QString line = withoutComment(lines.at(number));
            if (line.trimmed().isEmpty()) {
                continue;
            }
            for (const timewarp::lang::LanguageRow& row : timewarp::lang::kLanguageTable) {
                const QString display = QString::fromUtf8(row.display.data(),
                                                          static_cast<int>(row.display.size()));
                const QString key = QString::fromUtf8(row.key.data(),
                                                      static_cast<int>(row.key.size()));
                if (mentions(line, display) && mentions(line, key)) {
                    failures.append(QStringLiteral("%1:%2 names both \"%3\" and \"%4\" -- a "
                                                   "language row by another name")
                                        .arg(relative)
                                        .arg(number + 1)
                                        .arg(display, key));
                }
            }
        }
    }

    if (failures.isEmpty()) {
        std::printf("registry: one table, %d source file(s) checked\n", checked);
        return 0;
    }
    std::printf("registry: %d second copy/copies of the language table:\n",
                static_cast<int>(failures.size()));
    for (const QString& failure : failures) {
        std::printf("  %s\n", qPrintable(failure));
    }
    std::printf("\nA language's display name, key and extensions live in\n"
                "native/include/timewarp/lang/registry.hpp and nowhere else. Ask the table:\n"
                "`language_row`, `language_row_for_key`, `language_row_for_extension`.\n");
    return 1;
}

}  // namespace timewarp::gates
