// SPDX-License-Identifier: MIT

#include "timewarp/ui/notebook_window.hpp"

#include <QComboBox>
#include <QDir>
#include <QFileDialog>
#include <QFileInfo>
#include <QFrame>
#include <QHBoxLayout>
#include <QLabel>
#include <QMessageBox>
#include <QPixmap>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollArea>
#include <QTextEdit>
#include <QVBoxLayout>

#include "timewarp/lang/language.hpp"

namespace timewarp::ui {

namespace {

QPushButton* button(const QString& text, const QString& name, QWidget* parent,
                    const std::function<void()>& action) {
    auto* widget = new QPushButton{text, parent};
    widget->setObjectName(name);
    QObject::connect(widget, &QPushButton::clicked, parent, action);
    return widget;
}

}  // namespace

NotebookWindow::NotebookWindow(QWidget* parent) : QWidget(parent) {
    setObjectName(QStringLiteral("notebookWindow"));
    setWindowTitle(tr("Notebook"));
    resize(760, 720);
    buildUi();
    addCell(NotebookCell::Kind::Prose, {}, tr("# A notebook\n\nProse and programs, side by side."));
    addCell(NotebookCell::Kind::Code, QStringLiteral("basic"),
            QStringLiteral("10 FOR I = 1 TO 3\n20 PRINT \"HELLO \"; I\n30 NEXT I"));
}

void NotebookWindow::buildUi() {
    auto* layout = new QVBoxLayout{this};

    auto* bar = new QHBoxLayout;
    bar->addWidget(button(tr("+ Prose"), QStringLiteral("notebookAddProse"), this,
                          [this] {
                              addCell(NotebookCell::Kind::Prose);
                              markDirty();
                          }));
    bar->addWidget(button(tr("+ Program"), QStringLiteral("notebookAddProgram"), this,
                          [this] {
                              addCell(NotebookCell::Kind::Code);
                              markDirty();
                          }));
    bar->addStretch(1);
    bar->addWidget(button(tr("Run All"), QStringLiteral("notebookRunAll"), this,
                          [this] { runAll(); }));
    bar->addWidget(button(tr("Open\u2026"), QStringLiteral("notebookOpen"), this,
                          [this] { openDialog(); }));
    bar->addWidget(button(tr("Save"), QStringLiteral("notebookSave"), this, [this] {
        if (path_.isEmpty()) {
            saveAs();
        } else {
            (void)saveTo(path_);
        }
    }));
    bar->addWidget(button(tr("Save As\u2026"), QStringLiteral("notebookSaveAs"), this,
                          [this] { saveAs(); }));
    layout->addLayout(bar);

    hint_ = new QLabel{
        tr("Each program runs on its own: it is the engine's answer, and a cell does "
           "not share variables with the one above it."),
        this};
    hint_->setObjectName(QStringLiteral("notebookHint"));
    hint_->setWordWrap(true);
    layout->addWidget(hint_);

    scroll_ = new QScrollArea{this};
    scroll_->setWidgetResizable(true);
    column_ = new QWidget{scroll_};
    cellsLayout_ = new QVBoxLayout{column_};
    cellsLayout_->setAlignment(Qt::AlignTop);
    scroll_->setWidget(column_);
    layout->addWidget(scroll_, 1);
}

void NotebookWindow::rebuildLanguages(QComboBox* box, const QString& selected) const {
    box->clear();
    for (const lang::Language language : lang::all_languages()) {
        const std::string_view key = lang::key(language);
        box->addItem(QString::fromLatin1(key.data(), static_cast<int>(key.size())),
                     QString::fromLatin1(key.data(), static_cast<int>(key.size())));
    }
    const int index = box->findData(selected);
    box->setCurrentIndex(index >= 0 ? index : 0);
}

NotebookWindow::CellWidgets NotebookWindow::addCell(NotebookCell::Kind kind,
                                                    const QString& language,
                                                    const QString& text) {
    auto* frame = new QFrame{column_};
    frame->setFrameShape(QFrame::StyledPanel);
    frame->setObjectName(kind == NotebookCell::Kind::Code
                             ? QStringLiteral("notebookCodeCell")
                             : QStringLiteral("notebookProseCell"));
    auto* layout = new QVBoxLayout{frame};

    CellWidgets widgets;
    widgets.kind = kind;
    widgets.text = new QPlainTextEdit{text, frame};
    widgets.text->setObjectName(QStringLiteral("notebookCellText"));
    widgets.text->setMinimumHeight(kind == NotebookCell::Kind::Code ? 110 : 60);

    if (kind == NotebookCell::Kind::Code) {
        auto* header = new QHBoxLayout;
        widgets.language = new QComboBox{frame};
        widgets.language->setObjectName(QStringLiteral("notebookCellLanguage"));
        rebuildLanguages(widgets.language, language.isEmpty() ? QStringLiteral("basic")
                                                              : language);
        header->addWidget(widgets.language);
        header->addStretch(1);
        auto* run = button(tr("Run"), QStringLiteral("notebookCellRun"), frame, [this, frame] {
            for (int index = 0; index < cells_.size(); ++index) {
                if (cells_.at(index).text == nullptr) {
                    continue;
                }
                if (cells_.at(index).text->parentWidget() == frame) {
                    runCell(index);
                    return;
                }
            }
        });
        header->addWidget(run);
        layout->addLayout(header);

        widgets.output = new QPlainTextEdit{frame};
        widgets.output->setObjectName(QStringLiteral("notebookCellOutput"));
        widgets.output->setReadOnly(true);
        widgets.output->setMaximumHeight(120);
        layout->addWidget(widgets.output);

        widgets.drawing = new QLabel{frame};
        widgets.drawing->setObjectName(QStringLiteral("notebookCellDrawing"));
        widgets.drawing->setAlignment(Qt::AlignCenter);
        widgets.drawing->hide();
        layout->addWidget(widgets.drawing);
    }

    layout->addWidget(widgets.text);
    cellsLayout_->addWidget(frame);
    cells_.append(widgets);

    connect(widgets.text, &QPlainTextEdit::textChanged, this, [this] { markDirty(); });
    if (widgets.language != nullptr) {
        connect(widgets.language, &QComboBox::currentIndexChanged, this,
                [this] { markDirty(); });
    }
    return widgets;
}

void NotebookWindow::runCell(int index) {
    if (index < 0 || index >= cells_.size()) {
        return;
    }
    CellWidgets& cell = cells_[index];
    if (cell.kind != NotebookCell::Kind::Code || cell.output == nullptr) {
        return;
    }
    const QString language = cell.language != nullptr
                                 ? cell.language->currentData().toString()
                                 : QString{};
    const CellResult result = runNotebookCell(language, cell.text->toPlainText());
    cell.output->setPlainText(result.output.isEmpty() ? tr("(no output)") : result.output);
    if (cell.drawing != nullptr) {
        if (result.drawing.isNull()) {
            cell.drawing->hide();
        } else {
            cell.drawing->setPixmap(QPixmap::fromImage(result.drawing));
            cell.drawing->show();
        }
    }
    emit statusMessage(tr("Cell %1 ran").arg(index + 1));
}

void NotebookWindow::runAll() {
    for (int index = 0; index < cells_.size(); ++index) {
        runCell(index);
    }
}

Notebook NotebookWindow::notebook() const {
    Notebook notebook;
    notebook.title = windowTitle();
    for (const CellWidgets& cell : cells_) {
        NotebookCell entry;
        entry.kind = cell.kind;
        entry.text = cell.text != nullptr ? cell.text->toPlainText() : QString{};
        if (cell.kind == NotebookCell::Kind::Code && cell.language != nullptr) {
            entry.language = cell.language->currentData().toString();
        }
        notebook.cells.append(entry);
    }
    return notebook;
}

void NotebookWindow::setNotebook(const Notebook& notebook) {
    for (const CellWidgets& cell : cells_) {
        if (cell.text != nullptr) {
            cell.text->deleteLater();
        }
        if (QWidget* frame = cell.text != nullptr ? cell.text->parentWidget() : nullptr) {
            frame->deleteLater();
        }
    }
    cells_.clear();
    for (const NotebookCell& cell : notebook.cells) {
        addCell(cell.kind, cell.language, cell.text);
    }
    if (!notebook.title.isEmpty()) {
        setWindowTitle(notebook.title);
    }
    dirty_ = false;
}

bool NotebookWindow::openPath(const QString& path, QString* error) {
    const Notebook notebook = readNotebook(path, error);
    if (error != nullptr && !error->isEmpty()) {
        return false;
    }
    setNotebook(notebook);
    path_ = path;
    setWindowTitle(tr("%1 \u2014 Notebook").arg(QFileInfo{path}.fileName()));
    dirty_ = false;
    return true;
}

void NotebookWindow::openDialog() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Open notebook"), QString{},
        tr("Time Warp notebook (*.%1)").arg(QLatin1String{kNotebookExtension}));
    if (path.isEmpty()) {
        return;
    }
    QString error;
    if (!openPath(path, &error)) {
        QMessageBox::warning(this, tr("Could not open"), error);
    }
}

bool NotebookWindow::saveTo(const QString& path) {
    QString error;
    if (!writeNotebook(path, notebook(), &error)) {
        QMessageBox::warning(this, tr("Could not save"), error);
        return false;
    }
    path_ = path;
    setWindowTitle(tr("%1 \u2014 Notebook").arg(QFileInfo{path}.fileName()));
    dirty_ = false;
    emit statusMessage(tr("Saved %1").arg(QFileInfo{path}.fileName()));
    return true;
}

void NotebookWindow::saveAs() {
    const QString path = QFileDialog::getSaveFileName(
        this, tr("Save notebook"), path_,
        tr("Time Warp notebook (*.%1)").arg(QLatin1String{kNotebookExtension}));
    if (!path.isEmpty()) {
        (void)saveTo(path);
    }
}

void NotebookWindow::markDirty() { dirty_ = true; }

}  // namespace timewarp::ui
