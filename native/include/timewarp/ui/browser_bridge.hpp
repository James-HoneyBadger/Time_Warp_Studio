// SPDX-License-Identifier: AGPL-3.0-only
//
// Handing a generated file to the browser's download manager.
//
// On the desktop, an export writes to a path the user picked. A browser page has
// no path the user can see, and a file written into the in-memory filesystem is
// a copy that never leaves the tab. This module is the browser's equivalent of
// that save dialog: it builds a `Blob` from bytes already in memory and clicks a
// download link for it, so the browser downloads the file the way it downloads
// any other.
//
// A desktop build compiles both functions to `false`: the save paths that call
// them check the result, and a false answer is exactly what "there is no browser
// here" means.

#pragma once

#include <QByteArray>
#include <QString>

namespace timewarp::ui {

/// Whether `browserDownload` can do anything: true only under Emscripten.
[[nodiscard]] bool browserDownloadAvailable();

/// Offers `data` as a download named `fileName` with the given MIME type.
/// Returns false on a desktop build or when the browser refused the download.
[[nodiscard]] bool browserDownload(const QByteArray& data, const QString& fileName,
                                   const QString& mimeType);

}  // namespace timewarp::ui
