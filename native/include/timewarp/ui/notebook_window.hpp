// SPDX-License-Identifier: MIT
//
// The notebook window: prose and programs in one file, run a cell at a time.
//
// A window of its own rather than a document tab, for the same reason the HB-1 and
// the CHIP-8 have theirs: the tab strip holds editors, and every part of the window
// that asks "which document is this?" would have to learn about a second kind of
// thing. A notebook is opened, edited and saved by this window, and the editor is
// not involved.
//
// #### The shape of the thing
//
// Cells are a column in a scroll area: prose is a plain text area, a program is a
// small editor with its language, a Run button, the output below it and the drawing
// if it made one. "Run all" runs each code cell in order. Nothing is shared between
// cells, and the header says so -- a notebook is a document, and a lesson that needs
// a running session can put one program in one cell.

#pragma once

#include <QList>
#include <QString>
#include <QWidget>

#include "timewarp/ui/notebook.hpp"

class QComboBox;
class QLabel;
class QPlainTextEdit;
class QScrollArea;
class QVBoxLayout;

namespace timewarp::ui {

class NotebookWindow final : public QWidget {
    Q_OBJECT

public:
    explicit NotebookWindow(QWidget* parent = nullptr);

    /// Loads a `.twnb`; false and a sentence when it is not one.
    bool openPath(const QString& path, QString* error);
    [[nodiscard]] QString filePath() const { return path_; }
    [[nodiscard]] bool dirty() const noexcept { return dirty_; }

    [[nodiscard]] Notebook notebook() const;
    void setNotebook(const Notebook& notebook);

    /// The cell widgets, in document order. Public for the tests, which drive a run
    /// the way a user does rather than calling the engine directly.
    struct CellWidgets {
        NotebookCell::Kind kind = NotebookCell::Kind::Prose;
        QComboBox* language = nullptr;
        QPlainTextEdit* text = nullptr;
        QPlainTextEdit* output = nullptr;
        QLabel* drawing = nullptr;
    };
    [[nodiscard]] const QList<CellWidgets>& cells() const noexcept { return cells_; }

    /// Adds a cell and returns its widgets.
    CellWidgets addCell(NotebookCell::Kind kind,
                        const QString& language = QStringLiteral("basic"),
                        const QString& text = QString{});
    /// Runs the cell at `index` if it is a program.
    void runCell(int index);
    /// Runs every program cell, top to bottom.
    void runAll();

signals:
    void statusMessage(const QString& message);

private:
    void buildUi();
    void markDirty();
    bool saveTo(const QString& path);
    void saveAs();
    void openDialog();
    void rebuildLanguages(QComboBox* box, const QString& selected) const;

    QString path_;
    bool dirty_ = false;
    QScrollArea* scroll_ = nullptr;
    QWidget* column_ = nullptr;
    QVBoxLayout* cellsLayout_ = nullptr;
    QLabel* hint_ = nullptr;
    QList<CellWidgets> cells_;
};

}  // namespace timewarp::ui
