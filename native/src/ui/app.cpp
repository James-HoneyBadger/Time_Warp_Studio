// SPDX-License-Identifier: MIT

#include "timewarp/ui/app.hpp"

#include <QCoreApplication>

// Supplied by CMake from the repository's VERSION file. The fallback keeps a
// standalone compile of this file working.
#ifndef TIME_WARP_VERSION
#define TIME_WARP_VERSION "0.0.0"
#endif

namespace timewarp::ui {

void applyApplicationIdentity() {
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpStudio"));
    QCoreApplication::setApplicationName(QStringLiteral("TimeWarpStudio"));
    QCoreApplication::setApplicationVersion(version());
}

QString version() { return QString::fromUtf8(TIME_WARP_VERSION); }

}  // namespace timewarp::ui
