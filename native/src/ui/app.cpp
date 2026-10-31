// SPDX-License-Identifier: MIT

#include "timewarp/ui/app.hpp"

#include <QCoreApplication>
#include <QGuiApplication>

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

    // The desktop entry this window belongs to. A distribution package names it
    // after the product, and a Flatpak names it after the application id; the
    // sandbox says which by exporting FLATPAK_ID, so the same binary is right in
    // both. Without it the window is not associated with its launcher and the
    // dock shows a second, unnamed icon.
    QString desktop = qEnvironmentVariable("FLATPAK_ID").trimmed();
    if (desktop.isEmpty()) {
        desktop = QStringLiteral("time-warp-studio");
    }
    QGuiApplication::setDesktopFileName(desktop);
}

QString version() { return QString::fromUtf8(TIME_WARP_VERSION); }

}  // namespace timewarp::ui
