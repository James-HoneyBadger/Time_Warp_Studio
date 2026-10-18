// SPDX-License-Identifier: MIT

#include "timewarp/ui/app_paths.hpp"

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

}  // namespace timewarp::ui
