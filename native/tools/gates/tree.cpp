// SPDX-License-Identifier: MIT
//
// What the tree is made of, checked rather than assumed.
//
// The product had a second language in its own tree for years: four thousand lines
// of Python ran the harnesses, and the reason they were Python was that they were
// written when an engine was Python. That engine is gone, the harnesses are now
// `time_warp_gates`, and the last scripts went with 22.2.0.
//
// A removal that nothing enforces grows back. This walks the tracked files and
// fails if a `.py` appears: not because Python is bad, but because "the product
// needs no interpreter to be built, tested or checked" is a claim in the README and
// a claim needs a gate.
//
// A `.py` is not the only shape the leftovers take. `pytest.ini` outlived the whole
// removal because this gate only matched the extension, and it configured a runner
// for a directory that had been deleted -- so the manifests and caches are named
// here too, and a file that exists only to configure a Python tool is a leftover
// whether or not it ends in `.py`.

#include "gates.hpp"

#include <QDir>
#include <QDirIterator>
#include <QProcess>
#include <QStringList>

#include <cstdio>

namespace timewarp::gates {

namespace {

/// The directories that are output rather than source: a build tree, a virtual
/// environment, anything a packaging run leaves behind. `git ls-files` would
/// exclude them for us, which is why it is asked first.
bool isBuildOutput(const QString& path) {
    static const QStringList names{QStringLiteral("/build/"), QStringLiteral("/build-debug/"),
                                   QStringLiteral("/.venv/"), QStringLiteral("/node_modules/"),
                                   QStringLiteral("/build-docs/")};
    for (const QString& name : names) {
        if (path.contains(name)) {
            return true;
        }
    }
    return path.endsWith(QLatin1String("/build")) || path.contains(QLatin1String("/build/"));
}

/// Files that exist only to configure a Python tool, or to hold its output.
bool isPythonArtefact(const QString& path) {
    const QString name = path.section(QLatin1Char('/'), -1);
    static const QStringList exact{QStringLiteral("pytest.ini"),  QStringLiteral("tox.ini"),
                                   QStringLiteral("setup.py"),    QStringLiteral("setup.cfg"),
                                   QStringLiteral("pyproject.toml"),
                                   QStringLiteral("poetry.lock"), QStringLiteral("Pipfile"),
                                   QStringLiteral("Pipfile.lock"), QStringLiteral("mypy.ini"),
                                   QStringLiteral("ruff.toml"),   QStringLiteral(".ruff.toml"),
                                   QStringLiteral(".pylintrc"),   QStringLiteral("MANIFEST.in")};
    for (const QString& candidate : exact) {
        if (name == candidate) {
            return true;
        }
    }
    if (name.startsWith(QLatin1String("requirements")) &&
        name.endsWith(QLatin1String(".txt"))) {
        return true;
    }
    // A cache directory cannot hold a tracked file, but it can be committed by
    // accident; `git ls-files` is the honest question and this is the other half.
    static const QStringList directories{QStringLiteral("/__pycache__/"),
                                         QStringLiteral("/.pytest_cache/"),
                                         QStringLiteral("/.ruff_cache/"),
                                         QStringLiteral("/.mypy_cache/")};
    for (const QString& directory : directories) {
        if (path.contains(directory)) {
            return true;
        }
    }
    return false;
}

QStringList trackedFiles(const QString& root) {
    QProcess git;
    git.setWorkingDirectory(root);
    git.start(QStringLiteral("git"), {QStringLiteral("ls-files")});
    if (!git.waitForFinished(60000) || git.exitCode() != 0) {
        return {};
    }
    const QString listing = QString::fromUtf8(git.readAllStandardOutput());
    QStringList files;
    for (const QString& line : listing.split(QLatin1Char('\n'), Qt::SkipEmptyParts)) {
        files.append(line.trimmed());
    }
    return files;
}

}  // namespace

int runTree(const QStringList& arguments) {
    Q_UNUSED(arguments);
    const QString root = repositoryRoot();
    QStringList files = trackedFiles(root);
    QString source = QStringLiteral("git");
    if (files.isEmpty()) {
        // A tarball has no index; the walk is the same question asked without git.
        source = QStringLiteral("the tree");
        // Every file, because the question is now about manifests and caches too;
        // the same predicate decides, so the two paths cannot disagree.
        QDirIterator iterator{root, QDir::Files, QDirIterator::Subdirectories};
        while (iterator.hasNext()) {
            const QString path = iterator.next();
            const QString relative = QDir{root}.relativeFilePath(path);
            if (relative.endsWith(QLatin1String(".py")) || isPythonArtefact(relative)) {
                files.append(relative);
            }
        }
    }

    QStringList leftovers;
    for (const QString& file : files) {
        if (isBuildOutput(QLatin1Char('/') + file)) {
            continue;
        }
        if (file.endsWith(QLatin1String(".py")) || isPythonArtefact(file)) {
            leftovers.append(file);
        }
    }

    if (leftovers.isEmpty()) {
        std::printf("tree: no Python in the %s (%d file(s) checked)\n",
                    qPrintable(source), static_cast<int>(files.size()));
        return 0;
    }
    std::printf("tree: %d Python leftover(s) are back:\n", static_cast<int>(leftovers.size()));
    for (const QString& file : leftovers) {
        std::printf("  %s\n", qPrintable(file));
    }
    std::printf("\nThe product is built, tested and checked without an interpreter. If a\n"
                "script is genuinely needed, port it -- the gates in native/tools/gates/\n"
                "and the documentation tools in native/tools/docs/ are the two shapes one\n"
                "takes.\n");
    return 1;
}

}  // namespace timewarp::gates
