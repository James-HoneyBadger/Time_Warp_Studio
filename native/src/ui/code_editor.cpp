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

#include "timewarp/ui/syntax_highlighter.hpp"

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

    // Colouring belongs to the document, not the widget, so it survives a tab
    // being dragged to the other editor pane. The window sets the real language
    // as soon as the document is adopted; this is the default for a scratch
    // buffer that has no name yet.
    highlighter_ = new SyntaxHighlighter(document(), defaultLanguage());

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

void CodeEditor::setLanguage(Language language) {
    if (highlighter_ != nullptr) {
        highlighter_->setLanguage(language);
    }
}

Language CodeEditor::language() const noexcept {
    return highlighter_ == nullptr ? Language::Unknown : highlighter_->language();
}

namespace {

/// The flags `QTextDocument::find` takes for a plain-text search.
[[nodiscard]] QTextDocument::FindFlags findFlags(bool caseSensitive, bool backwards) {
    QTextDocument::FindFlags flags;
    if (caseSensitive) {
        flags |= QTextDocument::FindCaseSensitively;
    }
    if (backwards) {
        flags |= QTextDocument::FindBackward;
    }
    return flags;
}

}  // namespace

bool CodeEditor::findNext(const QString& text, bool caseSensitive, bool backwards) {
    if (text.isEmpty()) {
        return false;
    }
    const QTextDocument::FindFlags flags = findFlags(caseSensitive, backwards);

    QTextCursor found = document()->find(text, textCursor(), flags);
    if (found.isNull()) {
        // Wrap. A search that stops at the end of the file is a search the user
        // has to scroll to continue, and the Python IDE wrapped as well.
        found = backwards ? document()->find(text, document()->characterCount(), flags)
                          : document()->find(text, 0, flags);
    }
    if (found.isNull()) {
        return false;
    }
    setTextCursor(found);
    ensureCursorVisible();
    return true;
}

int CodeEditor::matchCount(const QString& text, bool caseSensitive) const {
    if (text.isEmpty()) {
        return 0;
    }
    const QTextDocument::FindFlags flags = findFlags(caseSensitive, false);
    int count = 0;
    QTextCursor found = document()->find(text, 0, flags);
    while (!found.isNull()) {
        ++count;
        found = document()->find(text, found, flags);
    }
    return count;
}

int CodeEditor::replaceAll(const QString& text, const QString& replacement,
                           bool caseSensitive) {
    if (text.isEmpty()) {
        return 0;
    }
    const QTextDocument::FindFlags flags = findFlags(caseSensitive, false);

    QTextCursor editor{document()};
    editor.beginEditBlock();
    int count = 0;
    QTextCursor found = document()->find(text, 0, flags);
    while (!found.isNull()) {
        found.insertText(replacement);
        ++count;
        // Resume *after* what was just inserted, so `a` -> `aa` terminates.
        found = document()->find(text, found, flags);
    }
    editor.endEditBlock();
    return count;
}

bool CodeEditor::replaceCurrent(const QString& text, const QString& replacement,
                                bool caseSensitive) {
    if (text.isEmpty()) {
        return false;
    }
    QTextCursor cursor = textCursor();
    const QString selected = cursor.selectedText();
    const Qt::CaseSensitivity sensitivity =
        caseSensitive ? Qt::CaseSensitive : Qt::CaseInsensitive;
    if (selected.compare(text, sensitivity) != 0) {
        return false;
    }
    QTextCursor editor{document()};
    editor.beginEditBlock();
    cursor.insertText(replacement);
    editor.endEditBlock();
    return true;
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
