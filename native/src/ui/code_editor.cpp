// SPDX-License-Identifier: MIT

#include "timewarp/ui/code_editor.hpp"

#include <algorithm>

#include <QFont>
#include <QFontDatabase>
#include <QFontMetrics>
#include <QPainter>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextDocument>

namespace timewarp::ui {

namespace {

/// Space either side of the digits in the gutter.
constexpr int kGutterPadding = 6;

/// The gutter never shrinks below this, so the numbers do not jiggle while typing
/// on line 9 of a new file.
constexpr int kMinimumGutterWidth = 24;

/// Number of spaces a tab inserts. Matching the Python editor's four matters more
/// than any argument for a different number: a file re-saved by either editor
/// should not reflow.
constexpr int kTabWidthInSpaces = 4;

/// The editor font. The Python editor asks for these in order and takes whatever
/// the system has; so does Qt, which is why the same stack is used.
[[nodiscard]] QFont editorFont() {
    QFont font = QFontDatabase::systemFont(QFontDatabase::FixedFont);
    font.setStyleHint(QFont::Monospace);
    font.setFixedPitch(true);
    font.setPointSize(11);
    return font;
}

}  // namespace

LineNumberArea::LineNumberArea(CodeEditor* editor)
    : QWidget(editor), editor_(editor) {
    setObjectName(QStringLiteral("lineNumberArea"));
}

QSize LineNumberArea::sizeHint() const {
    return QSize(editor_->lineNumberAreaWidth(), 0);
}

void LineNumberArea::paintEvent(QPaintEvent* event) { editor_->paintLineNumbers(event); }

CodeEditor::CodeEditor(QWidget* parent) : QPlainTextEdit(parent) {
    setObjectName(QStringLiteral("codeEditor"));
    setFont(editorFont());
    setTabStopDistance(QFontMetricsF{font()}.horizontalAdvance(QLatin1Char(' ')) *
                       kTabWidthInSpaces);
    setLineWrapMode(QPlainTextEdit::NoWrap);
    setFrameShape(QFrame::NoFrame);

    gutter_ = new LineNumberArea(this);

    connect(this, &QPlainTextEdit::blockCountChanged, this, &CodeEditor::updateGutterWidth);
    connect(this, &QPlainTextEdit::updateRequest, this,
            [this](const QRect& rect, int dy) {
                if (gutter_ == nullptr) {
                    return;
                }
                if (dy != 0) {
                    gutter_->scroll(0, dy);
                } else {
                    gutter_->update(0, rect.y(), gutter_->width(), rect.height());
                }
            });
    // QPlainTextEdit's own signal has no arguments and a different name is not an
    // option for it, so the position is read here and re-emitted with coordinates.
    connect(this, &QPlainTextEdit::cursorPositionChanged, this, [this] {
        highlightCurrentLine();
        emit cursorMoved(currentLine(), currentColumn());
    });

    updateGutterWidth(blockCount());
    highlightCurrentLine();
}

int CodeEditor::lineNumberAreaWidth() const {
    const int digits = QString::number(std::max(1, blockCount())).size();
    return std::max(kMinimumGutterWidth,
                    kGutterPadding + QFontMetrics{font()}.horizontalAdvance(QLatin1Char('9')) *
                                        digits +
                        kGutterPadding);
}

void CodeEditor::paintLineNumbers(QPaintEvent* event) {
    QPainter painter(gutter_);
    painter.fillRect(event->rect(), palette().color(QPalette::AlternateBase));

    QTextBlock block = firstVisibleBlock();
    int blockNumber = block.blockNumber();
    int top = qRound(blockBoundingGeometry(block).translated(contentOffset()).top());
    int bottom = top + qRound(blockBoundingRect(block).height());
    const int currentLineNumber = textCursor().blockNumber();

    while (block.isValid() && top <= event->rect().bottom()) {
        if (block.isVisible() && bottom >= event->rect().top()) {
            const QString number = QString::number(blockNumber + 1);
            const bool isCurrent = blockNumber == currentLineNumber;
            painter.setPen(isCurrent ? palette().color(QPalette::Text)
                                     : palette().color(QPalette::Mid));
            painter.drawText(0, top, gutter_->width() - kGutterPadding,
                             QFontMetrics{font()}.height(), Qt::AlignRight, number);
        }
        block = block.next();
        top = bottom;
        bottom = top + qRound(blockBoundingRect(block).height());
        ++blockNumber;
    }
}

void CodeEditor::goToLine(int line) {
    const int target = std::clamp(line, 1, std::max(1, blockCount()));
    const QTextBlock block = document()->findBlockByNumber(target - 1);
    if (!block.isValid()) {
        return;
    }
    QTextCursor cursor{block};
    cursor.movePosition(QTextCursor::StartOfBlock);
    setTextCursor(cursor);
    centerCursor();
}

int CodeEditor::currentLine() const { return textCursor().blockNumber() + 1; }

int CodeEditor::currentColumn() const {
    return textCursor().positionInBlock() + 1;
}

void CodeEditor::resizeEvent(QResizeEvent* event) {
    QPlainTextEdit::resizeEvent(event);
    const QRect contents = contentsRect();
    gutter_->setGeometry(QRect{contents.left(), contents.top(), lineNumberAreaWidth(),
                               contents.height()});
}

void CodeEditor::updateGutterWidth(int /*blockCount*/) {
    setViewportMargins(lineNumberAreaWidth(), 0, 0, 0);
}

void CodeEditor::highlightCurrentLine() {
    // Nothing is highlighted when the editor cannot be edited: the band reads as
    // "the cursor is here", which is a lie in a read-only view.
    if (isReadOnly()) {
        setExtraSelections({});
        return;
    }
    QTextEdit::ExtraSelection selection;
    selection.format.setBackground(palette().color(QPalette::AlternateBase));
    selection.format.setProperty(QTextFormat::FullWidthSelection, true);
    selection.cursor = textCursor();
    selection.cursor.clearSelection();
    setExtraSelections({selection});
}

}  // namespace timewarp::ui
