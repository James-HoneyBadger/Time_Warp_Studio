// SPDX-License-Identifier: MIT
//
// The HB-1's tube: the machine's screen, painted from its memory.
//
// ---------------------------------------------------------------------------
// Why a widget and not a text box
// ---------------------------------------------------------------------------
//
// The machine's screen used to be a `QPlainTextEdit` holding the lines it had
// printed. That is a log, not a screen: nothing a program did could move the cursor,
// set a cell's ink, or read back what was there. `LOCATE` validated its arguments and
// returned; `POKE` wrote a map that nothing painted.
//
// This paints `HbMemory` instead. A cell is a byte at `$8000`, its ink and paper are
// a nibble pair at `$9000`, and the cursor is a register at `$A000` -- so the
// statements that address those places now do something a reader can see, and a
// program that scrolls the screen scrolls memory rather than a paint offset.
//
// #### The transcript
//
// A tube is 25 rows and a listing is longer than that, so the widget also keeps the
// rows it has written since the last clear. Those rows are the scrollback (the wheel
// and `Ctrl+PageUp`), and they are what `toPlainText()` returns -- which is what the
// machine's own tests have always read. A 1982 machine had no scrollback; this one
// keeps one because a learner reading a listing needs it, and the manual says so.

#pragma once

#include <QColor>
#include <QString>
#include <QStringList>
#include <QWidget>

#include "timewarp/ui/hb_memory.hpp"

class QFont;

namespace timewarp::ui {

/// The phosphor colours a tube paints with. A monochrome monitor shows every palette
/// number as the same phosphor at a different brightness, which is why `ink` is a
/// ramp rather than sixteen colours: `COLOR 15, 0` is brightest ink on black, and
/// `COLOR 0, 15` is inverse video.
struct TubePalette {
    QColor background;
    QColor border;
    /// Brightest ink, dimmest paper-to-ink step.
    QColor bright;
    /// The dimmest visible ink.
    QColor dim;
};

class HbTube final : public QWidget {
    Q_OBJECT

public:
    explicit HbTube(QWidget* parent = nullptr);

    /// The memory this tube is a view of. Null paints an empty tube, which is what a
    /// widget built before its machine looks like.
    void setMemory(HbMemory* memory);
    [[nodiscard]] HbMemory* memory() const noexcept { return memory_; }

    void setPalette(const TubePalette& palette);
    [[nodiscard]] const TubePalette& palette() const noexcept { return palette_; }

    /// Off is a dark tube that shows nothing: the machine's power switch, not a
    /// blank screen. Text keeps arriving in memory while the tube is dark -- a
    /// powered-off machine's RAM does not know -- but nothing is painted.
    void setPowered(bool on);
    [[nodiscard]] bool powered() const noexcept { return powered_; }

    /// The character cell, in points. The tube's size follows from it.
    void setTextSize(int points);
    [[nodiscard]] int textSize() const noexcept { return text_size_; }

    /// Writes `text` at the cursor -- wrapping and scrolling in memory -- and records
    /// the rows it used in the transcript. Each call ends a line, which is what
    /// `QPlainTextEdit::appendPlainText` did before it.
    void append(const QString& text);

    /// Records a line in the transcript without writing the screen. Program output
    /// takes this path: the engine already wrote it into memory through the machine
    /// bus, in order with the `POKE`s around it, and writing it twice would put it on
    /// the tube twice.
    void remember(const QString& text);

    /// Clears the screen *and* the transcript: what `CLS` means.
    void clear();

    /// Every row written since the last clear, joined by newlines. The machine's tests
    /// read this, and so does a bug report.
    [[nodiscard]] QString toPlainText() const;

    /// The live screen as rows: what the tube is showing right now.
    [[nodiscard]] QString screenText() const;

    /// Rows scrolled back from the live screen. Zero is the live screen.
    void scrollBack(int rows);
    [[nodiscard]] int scrollOffset() const noexcept { return scroll_offset_; }

    /// `text` as the tube's character set can spell it: an engine's emoji prefix
    /// dropped, typographic punctuation transliterated, anything above code 126 as
    /// `?`. Public because the machine bus writes to memory without going through the
    /// widget, and because it is a rule about what a reader sees.
    [[nodiscard]] static QString toCharset(const QString& text);

    [[nodiscard]] QSize sizeHint() const override;
    [[nodiscard]] QSize minimumSizeHint() const override;

    /// The cell rectangle for a row and column, in widget coordinates. Public because
    /// a test samples a cell's pixels rather than trusting the painter.
    [[nodiscard]] QRect cellRect(int row, int column) const;

protected:
    void paintEvent(QPaintEvent* event) override;
    void wheelEvent(QWheelEvent* event) override;
    void keyPressEvent(QKeyEvent* event) override;
    void resizeEvent(QResizeEvent* event) override;

private:
    void recomputeCell();
    [[nodiscard]] QColor inkColour(int nibble) const;
    [[nodiscard]] QColor paperColour(int nibble) const;
    /// One row of characters, with the ink and paper of its cells.
    void paintRow(QPainter& painter, int row, const QString& text, bool fromMemory);
    void trimTranscript();

    HbMemory* memory_ = nullptr;
    TubePalette palette_;
    int text_size_ = 11;
    QFont* font_ = nullptr;
    QSize cell_{8, 16};
    QStringList transcript_;
    int scroll_offset_ = 0;
    bool cursor_visible_ = true;
    bool powered_ = true;
};

}  // namespace timewarp::ui
