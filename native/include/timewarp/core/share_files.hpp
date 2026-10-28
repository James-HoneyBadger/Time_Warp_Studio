// SPDX-License-Identifier: MIT
//
// Where the application's own data files live, by name.
//
// `lessons.json`, `snippets.json` and `command_rows.json` are three files that ship
// beside the binary and are read at run time. Each used to carry its own list of
// candidate paths, and the two lists were the same list: installed share directory,
// beside a build tree, inside a macOS bundle, then the source tree. A third copy for
// a third file is how the three would drift apart, so the search is here once and
// each loader supplies its own name and override variable.
//
// Header-only and Qt Core only, like `registry.hpp`: the lessons library and the
// widget application both read data files, and neither should gain a link
// dependency on the other to do it.

#pragma once

#include <QCoreApplication>
#include <QFileInfo>
#include <QString>

#include <initializer_list>

namespace timewarp::core {

/// The first of these that exists: `NAME`'s installed copy, the copy beside a build
/// tree, the copy inside a macOS bundle, then the checkout's `native/share`.
///
/// Empty when none does, which is what a caller reports rather than working around:
/// "the file is not installed" and "the file is empty" are different problems.
[[nodiscard]] inline QString shareFile(const QString& name) {
    if (name.isEmpty()) {
        return {};
    }
    // Without an application object there is no directory to look beside, which is
    // the case for a library user that passes its own path anyway.
    const QString app_dir = QCoreApplication::instance() != nullptr
                                ? QCoreApplication::applicationDirPath()
                                : QString{};
    if (app_dir.isEmpty()) {
        return {};
    }
    for (const QString& candidate :
         {app_dir + QStringLiteral("/../share/time-warp-studio/") + name,
          app_dir + QStringLiteral("/../share/") + name,
          app_dir + QStringLiteral("/../Resources/share/time-warp-studio/") + name,
          app_dir + QStringLiteral("/../../native/share/") + name}) {
        if (QFileInfo{candidate}.isFile()) {
            return QFileInfo{candidate}.absoluteFilePath();
        }
    }
    return {};
}

/// `shareFile(name)`, unless `variable` names a file, which wins.
///
/// The variables are what a test points at a checkout and a teacher points at a
/// catalogue of their own: `TIME_WARP_LESSONS`, `TIME_WARP_SNIPPETS`,
/// `TIME_WARP_COMMANDS`.
[[nodiscard]] inline QString shareFile(const char* variable, const QString& name) {
    if (variable != nullptr) {
        const QString override = qEnvironmentVariable(variable).trimmed();
        if (!override.isEmpty() && QFileInfo{override}.isFile()) {
            return QFileInfo{override}.absoluteFilePath();
        }
    }
    return shareFile(name);
}

}  // namespace timewarp::core
