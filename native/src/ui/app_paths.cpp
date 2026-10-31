// SPDX-License-Identifier: MIT

#include "timewarp/ui/app_paths.hpp"

#include <QApplication>
#include <QDir>
#include <QFileInfo>

namespace timewarp::ui {

QString dataDir() {
    QString override = qEnvironmentVariable("TIME_WARP_DATA_DIR").trimmed();
    if (!override.isEmpty()) {
        // `~` and `~/…` expand the way the Python side expands them, so a person
        // writing `TIME_WARP_DATA_DIR=~/tw-data` gets a home-relative directory
        // rather than one literally named `~`.
        if (override == QLatin1String("~")) {
            return QDir::homePath();
        }
        if (override.startsWith(QLatin1String("~/"))) {
            override = QDir::homePath() + override.mid(1);
        }
        return QDir::cleanPath(override);
    }
    return QDir::homePath() + QStringLiteral("/.time_warp");
}

QString dataDirFor(const QString& leaf) {
    const QString root = dataDir();
    if (root.isEmpty() || leaf.isEmpty()) {
        return {};
    }
    // A leaf is a directory name, not a path: an absolute one would let a caller
    // write outside the data directory, and nothing here has a reason to.
    if (QFileInfo{leaf}.isAbsolute() || leaf.startsWith(QLatin1String(".."))) {
        return {};
    }
    const QDir directory{root};
    if (!directory.mkpath(leaf)) {
        return {};
    }
    return directory.filePath(leaf);
}

QString examplesDir() {
    const QString from_environment = qEnvironmentVariable("TIME_WARP_EXAMPLES");
    if (!from_environment.isEmpty() && QFileInfo{from_environment}.isDir()) {
        return QFileInfo{from_environment}.absoluteFilePath();
    }
    const QString app_dir = QApplication::applicationDirPath();
    // `bin/time_warp_ide` -> `share/time-warp-studio/examples`, which is the
    // destination CMakeLists.txt installs them to.
    const QString installed =
        app_dir + QStringLiteral("/../share/time-warp-studio/examples");
    if (QFileInfo{installed}.isDir()) {
        return QFileInfo{installed}.absoluteFilePath();
    }
    // A build tree: the binary is `native/build/time_warp_ide` and the programs
    // are beside `native/`. The second path covers a binary in `native/build/bin`.
    for (const QString& upward :
         {QStringLiteral("/../Examples"), QStringLiteral("/../../Examples")}) {
        const QString candidate = app_dir + upward;
        if (QFileInfo{candidate}.isDir()) {
            return QFileInfo{candidate}.absoluteFilePath();
        }
    }
    // Last, the working directory: someone who unpacked the tarball and ran the
    // binary from inside it has the programs right there.
    if (QFileInfo{QStringLiteral("Examples")}.isDir()) {
        return QFileInfo{QStringLiteral("Examples")}.absoluteFilePath();
    }
    return {};
}

}  // namespace timewarp::ui
