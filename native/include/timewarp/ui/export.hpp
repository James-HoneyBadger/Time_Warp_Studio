// SPDX-License-Identifier: MIT
//
// Code and session export.
//
// Two files a learner hands to somebody else: a PDF of the program, and one
// self-contained HTML page holding the program, its output and the drawing.
// Both are plain Qt Gui -- `QPdfWriter` ships with Qt, so the PDF needs no new
// dependency and no build-time decision.

#pragma once

#include <QImage>
#include <QString>

namespace timewarp::ui::exporter {

/// Writes *source* as an A4 PDF with line numbers and a header naming the
/// program and its language. `title` is the document's name, not a path.
///
/// Returns false and fills *error* if the file cannot be written. The bytes are
/// deterministic: two calls with the same arguments produce the same file.
[[nodiscard]] bool writeCodePdf(const QString& path, const QString& title,
                                const QString& language, const QString& source,
                                QString* error);

/// Writes one self-contained HTML page: the program (marked up by
/// `QTextDocument::toHtml`), the console output, and *drawing* embedded as a
/// base64 PNG data URI. A null image is reported in the page rather than left
/// as a broken `<img>`.
[[nodiscard]] bool writeSessionHtml(const QString& path, const QString& title,
                                    const QString& language, const QString& source,
                                    const QString& console_output, const QImage& drawing,
                                    QString* error);

}  // namespace timewarp::ui::exporter
