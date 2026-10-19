// SPDX-License-Identifier: MIT
//
// Code and session export.
//
// Three files a learner hands to somebody else: a PDF of the program, one
// self-contained HTML page holding the program, its output and the drawing, and
// a lesson session as Markdown or PDF. All are plain Qt Gui -- `QPdfWriter` ships
// with Qt, so the PDF needs no new dependency and no build-time decision.

#pragma once

#include <QImage>
#include <QList>
#include <QString>
#include <QStringList>

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

/// One checkpoint as a session report shows it: what was asked, and what the
/// learner's program did when it was checked. `checked` is false when the
/// checkpoint was never tried in this session, which is different from "tried and
/// failed" and is reported as such.
struct LessonSessionCheckpoint {
    QString title;
    QString description;
    bool checked = false;
    bool passed = false;
    QString message;
};

/// Everything a lesson-session report contains.
///
/// The panel owns the lesson, its checkpoints and the verdicts; the window adds
/// the learner's program and the console, because those belong to the document
/// and the run rather than to the lesson.
struct LessonSession {
    QString lessonTitle;
    QString lessonDescription;
    QString language;
    QString status;  ///< "not_started", "in_progress" or "completed", as the engine says.
    int current = 0; ///< The checkpoint the learner is on.
    QStringList objectives;
    QList<LessonSessionCheckpoint> checkpoints;
    QString source;
    QString consoleOutput;
};

/// Writes the session as Markdown: the lesson and what it teaches, every
/// checkpoint with its verdict, the learner's program and the console. One file,
/// no assets, so it can be pasted into a report or a class hand-in.
[[nodiscard]] bool writeLessonSessionMarkdown(const QString& path,
                                              const LessonSession& session, QString* error);

/// The same report as an A4 PDF, through the writer `writeCodePdf` uses, so both
/// files say the same thing and neither needs a dependency.
[[nodiscard]] bool writeLessonSessionPdf(const QString& path, const LessonSession& session,
                                         QString* error);

}  // namespace timewarp::ui::exporter
