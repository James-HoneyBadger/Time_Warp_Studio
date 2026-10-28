// SPDX-License-Identifier: MIT
//
// Notebooks: a document that is prose and programs, run one cell at a time.
//
// A lesson handout is text with programs in it, and the desktop has had no way to
// *be* that document -- it could hold the prose (a text file) or the program (an
// editor tab), never both with the answers beside them. A notebook is the two in
// one file: a `.twnb` holding cells, each either prose or a program, saved as JSON
// so a person can read and diff it.
//
// #### What a cell run is
//
// A cell runs the same way a thumbnail does: the engine in this process with a
// recording host and no window, so the answer is the language's answer and not a
// second implementation of it. What a cell prints is shown under it; what it draws
// is painted from the recording. A cell does not share variables with the cell
// above it -- a notebook is a document, not a session -- and the window says so
// where a user would otherwise be surprised by it.

#pragma once

#include <QImage>
#include <QList>
#include <QSize>
#include <QString>

class QIODevice;

namespace timewarp::ui {

struct NotebookCell {
    enum class Kind { Prose, Code };
    Kind kind = Kind::Prose;
    /// The language's protocol key (`basic`, `logo`, ...). Empty for prose.
    QString language;
    QString text;
};

struct Notebook {
    QString title;
    QList<NotebookCell> cells;
};

/// The extension a notebook is saved with, without the dot.
inline constexpr const char* kNotebookExtension = "twnb";

/// Reads a notebook. A file that is not a notebook is an error with a sentence;
/// a cell of an unknown kind is read as prose rather than dropped, because losing
/// a person's words is worse than showing them unformatted.
[[nodiscard]] Notebook readNotebook(QIODevice& device, QString* error);
[[nodiscard]] Notebook readNotebook(const QString& path, QString* error);

/// The notebook as JSON, in the shape `readNotebook` accepts.
[[nodiscard]] QString notebookToJson(const Notebook& notebook);
[[nodiscard]] bool writeNotebook(const QString& path, const Notebook& notebook,
                                 QString* error);

/// What one code cell produced.
struct CellResult {
    bool ok = true;
    /// What the program printed, with the language's own emoji prefixes.
    QString output;
    /// The drawing, or a null image when the cell drew nothing.
    QImage drawing;
};

/// Runs one cell headlessly. `language` is the protocol key; an unknown one is an
/// error sentence rather than a throw.
[[nodiscard]] CellResult runNotebookCell(const QString& language, const QString& source,
                                         const QSize& drawingSize = QSize{320, 200});

}  // namespace timewarp::ui
