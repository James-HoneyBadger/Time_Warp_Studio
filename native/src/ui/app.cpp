// SPDX-License-Identifier: AGPL-3.0-only

#include "timewarp/ui/app.hpp"

#include <QCoreApplication>
#include <QGuiApplication>
#include <QSettings>

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

#ifdef __EMSCRIPTEN__
    // The browser's default is an INI file in the in-memory home directory, which a
    // reload throws away: the theme, the window layout and the list of open documents
    // would reset on every visit. `localStorage` is synchronous, so it needs neither
    // the IndexedDB volume nor a JSPI-enabled build, and the settings are small enough
    // for its five-megabyte allowance. Set here because a format chosen after the first
    // `QSettings` object exists is too late.
    QSettings::setDefaultFormat(QSettings::WebLocalStorageFormat);
#endif

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

void applyApplicationIdentity(const QString& applicationName, const QString& desktopName) {
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpUniverse"));
    QCoreApplication::setApplicationName(applicationName);
    QCoreApplication::setApplicationVersion(version());
    if (!desktopName.isEmpty()) {
        QGuiApplication::setDesktopFileName(desktopName);
    }
}

QString version() { return QString::fromUtf8(TIME_WARP_VERSION); }

}  // namespace timewarp::ui
