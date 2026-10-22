// SPDX-License-Identifier: MIT

#include "timewarp/ui/hb_tube.hpp"

#include <algorithm>

#include <QFont>
#include <QFontMetrics>
#include <QKeyEvent>
#include <QPainter>
#include <QTimer>
#include <QWheelEvent>

namespace timewarp::ui {

namespace {

/// How many rows the transcript keeps. The machine's console kept 4000 blocks; the
/// number is the same so a reader's scrollback did not shrink when the log became a
/// screen.
constexpr int kTranscriptRows = 4000;

/// A phosphor is a brightness ramp, not a colour: a monochrome tube shows every
/// palette number as the same glow at a different level.
[[nodiscard]] QColor mix(const QColor& from, const QColor& to, double amount) {
    const double ratio = std::clamp(amount, 0.0, 1.0);
    return QColor{static_cast<int>(from.red() + (to.red() - from.red()) * ratio),
                  static_cast<int>(from.green() + (to.green() - from.green()) * ratio),
                  static_cast<int>(from.blue() + (to.blue() - from.blue()) * ratio)};
}

/// The sixteen graphics characters at codes 128..143, eight rows of eight bits each.
///
/// A font cannot draw these: they are the blocks and lines an 8-bit machine used to
/// make a picture out of characters -- `POKE 32768, 128` is a solid block, which is
/// the oldest trick in the book and the reason a machine of this kind could draw
/// with nothing but a text screen.
constexpr std::array<std::array<std::uint8_t, 8>, 16> kGraphicsGlyphs{{
    {{0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF}},  // 128 solid block
    {{0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00}},  // 129 top half
    {{0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF}},  // 130 bottom half
    {{0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0}},  // 131 left half
    {{0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F}},  // 132 right half
    {{0xF0, 0xF0, 0xF0, 0xF0, 0x00, 0x00, 0x00, 0x00}},  // 133 top-left quadrant
    {{0x0F, 0x0F, 0x0F, 0x0F, 0x00, 0x00, 0x00, 0x00}},  // 134 top-right
    {{0x00, 0x00, 0x00, 0x00, 0xF0, 0xF0, 0xF0, 0xF0}},  // 135 bottom-left
    {{0x00, 0x00, 0x00, 0x00, 0x0F, 0x0F, 0x0F, 0x0F}},  // 136 bottom-right
    {{0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55}},  // 137 checkerboard
    {{0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}},  // 138 rule, top
    {{0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x00}},  // 139 rule, middle
    {{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF}},  // 140 rule, bottom
    {{0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80}},  // 141 bar, left
    {{0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18}},  // 142 bar, centre
    {{0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01}},  // 143 bar, right
}};

constexpr int kFirstGraphicsCode = 128;
constexpr int kLastGraphicsCode = 143;

}  // namespace

HbTube::HbTube(QWidget* parent) : QWidget(parent) {
    setObjectName(QStringLiteral("hbTube"));
    setAccessibleName(tr("Computer Screen"));
    setFocusPolicy(Qt::StrongFocus);
    setAutoFillBackground(false);
    palette_.background = QColor{8, 8, 10};
    palette_.border = QColor{40, 40, 40};
    palette_.bright = QColor{80, 255, 120};
    palette_.dim = QColor{20, 80, 40};
    recomputeCell();
    // A cursor that blinks is the difference between a picture of a screen and a
    // screen. A second is the period's rate: fast enough to see, slow enough not to
    // distract a reader.
    auto* blink = new QTimer{this};
    connect(blink, &QTimer::timeout, this, [this] {
        cursor_visible_ = !cursor_visible_;
        update();
    });
    blink->start(500);
}

void HbTube::setMemory(HbMemory* memory) {
    memory_ = memory;
    update();
}

void HbTube::setPalette(const TubePalette& palette) {
    palette_ = palette;
    update();
}

void HbTube::setTextSize(int points) {
    text_size_ = std::clamp(points, 6, 32);
    recomputeCell();
    updateGeometry();
    update();
}

void HbTube::recomputeCell() {
    font_ = new QFont{QStringLiteral("monospace"), text_size_};
    font_->setStyleHint(QFont::TypeWriter);
    font_->setFixedPitch(true);
    const QFontMetrics metrics{*font_};
    // A cell is the widest glyph and the line height, so a column never depends on
    // which character is in it: a tube is a grid, and a grid that breathes is not one.
    cell_ = QSize{std::max(6, metrics.horizontalAdvance(QLatin1Char('M'))),
                  std::max(10, metrics.height())};
}

QSize HbTube::sizeHint() const {
    return QSize{cell_.width() * HbMemory::kColumns + 8,
                 cell_.height() * HbMemory::kRows + 8};
}

QSize HbTube::minimumSizeHint() const { return QSize{cell_.width() * 20, cell_.height() * 5}; }

QRect HbTube::cellRect(int row, int column) const {
    return QRect{QPoint{4 + column * cell_.width(), 4 + row * cell_.height()}, cell_};
}

QString HbTube::toCharset(const QString& text) {
    // A tube has 256 character codes and no glyph for an emoji or a typographic
    // dash. A machine of the period had the same problem and solved it the same way:
    // a translation table. The Studio's engines prefix messages with an emoji, so a
    // leading one is dropped rather than shown as `?Error`, and the punctuation that
    // appears in the machine's own banner becomes its ASCII twin.
    QString out;
    out.reserve(text.size());
    bool at_line_start = true;
    for (const QChar character : text) {
        if (character == QLatin1Char('\n')) {
            out.append(character);
            at_line_start = true;
            continue;
        }
        if (at_line_start && character.unicode() > 0x2000) {
            // An engine's emoji prefix: dropped, and the space after it with it.
            continue;
        }
        if (at_line_start && character == QLatin1Char(' ')) {
            // Keep the indent, but a line that began with an emoji has already lost it.
            out.append(character);
            continue;
        }
        at_line_start = false;
        const ushort code = character.unicode();
        switch (code) {
            case 0x00B7:  // ·
                out.append(QLatin1Char('-'));
                break;
            case 0x2014:  // —
                out.append(QLatin1Char('-'));
                break;
            case 0x2019:  // ’
                out.append(QLatin1Char('\''));
                break;
            case 0x201C:  // “
            case 0x201D:  // ”
                out.append(QLatin1Char('"'));
                break;
            default:
                out.append(code > 126 ? QLatin1Char('?') : character);
                break;
        }
    }
    return out;
}

void HbTube::append(const QString& text) {
    const QString body = toCharset(text);
    remember(body);
    if (memory_ != nullptr && !body.isEmpty()) {
        memory_->writeText(body, true);
    }
}

void HbTube::remember(const QString& text) {
    QString body = toCharset(text);
    while (body.endsWith(QLatin1Char('\n')) || body.endsWith(QLatin1Char('\r'))) {
        body.chop(1);
    }
    if (body.isEmpty()) {
        return;
    }
    // The transcript keeps the rows as written, wrapped at the tube's width, so the
    // scrollback and the live screen are the same rows in the same places.
    const QStringList logical = body.split(QLatin1Char('\n'));
    for (const QString& line : logical) {
        QString remaining = line;
        do {
            const QString row = remaining.left(HbMemory::kColumns);
            remaining = remaining.mid(HbMemory::kColumns);
            transcript_.append(row);
        } while (!remaining.isEmpty());
    }
    trimTranscript();
    scroll_offset_ = 0;
    update();
}

void HbTube::trimTranscript() {
    while (transcript_.size() > kTranscriptRows) {
        transcript_.removeFirst();
    }
}

void HbTube::clear() {
    transcript_.clear();
    scroll_offset_ = 0;
    if (memory_ != nullptr) {
        memory_->clear();
    }
    update();
}

QString HbTube::toPlainText() const { return transcript_.join(QLatin1Char('\n')); }

QString HbTube::screenText() const { return memory_ != nullptr ? memory_->screenText() : QString{}; }

void HbTube::scrollBack(int rows) {
    // Only as far as the transcript goes: scrolling into a blank void would look like
    // a machine that lost its output.
    const int max_back =
        std::max(0, static_cast<int>(transcript_.size()) - HbMemory::kRows);
    scroll_offset_ = std::clamp(scroll_offset_ + rows, 0, max_back);
    update();
}

QColor HbTube::inkColour(int nibble) const {
    if (nibble == 0) {
        return palette_.background;
    }
    // 1..7 is the dim half of the ramp, 8..15 the bright half, which is how a
    // sixteen-colour machine's palette behaved on a monochrome tube.
    const double level = nibble >= 8 ? 0.55 + (nibble - 8) * 0.064 : nibble * 0.07;
    return mix(palette_.background, palette_.bright, level);
}

QColor HbTube::paperColour(int nibble) const {
    if (nibble == 0) {
        return palette_.background;
    }
    // Paper that is not black is a glow behind the cell, not a second ink: a
    // monochrome tube can only dim the phosphor between the characters.
    return mix(palette_.background, palette_.dim, std::min(1.0, nibble * 0.09));
}

void HbTube::paintRow(QPainter& painter, int row, const QString& text, bool fromMemory) {
    painter.setFont(*font_);
    int column = 0;
    for (const QChar character : text) {
        if (column >= HbMemory::kColumns) {
            break;
        }
        if (fromMemory && memory_ != nullptr) {
            const int paper = memory_->paperAt(row, column);
            if (paper != 0) {
                painter.fillRect(cellRect(row, column), paperColour(paper));
            }
            painter.setPen(inkColour(memory_->inkAt(row, column)));
        } else {
            painter.setPen(palette_.bright);
        }
        const QRect cell = cellRect(row, column);
        const int code = character.unicode();
        if (fromMemory && code >= kFirstGraphicsCode && code <= kLastGraphicsCode) {
            // A block, a quadrant or a rule: eight rows of eight bits, scaled to the
            // cell. This is how a machine with only a text screen drew a picture.
            const auto& glyph = kGraphicsGlyphs[static_cast<std::size_t>(
                code - kFirstGraphicsCode)];
            const QColor ink = painter.pen().color();
            for (int line = 0; line < 8; ++line) {
                for (int bit = 0; bit < 8; ++bit) {
                    if ((glyph[static_cast<std::size_t>(line)] & (0x80u >> bit)) == 0) {
                        continue;
                    }
                    const int x = cell.left() + bit * cell.width() / 8;
                    const int y = cell.top() + line * cell.height() / 8;
                    const int w = cell.left() + (bit + 1) * cell.width() / 8 - x;
                    const int h = cell.top() + (line + 1) * cell.height() / 8 - y;
                    painter.fillRect(QRect{x, y, std::max(1, w), std::max(1, h)}, ink);
                }
            }
            ++column;
            continue;
        }
        painter.drawText(QRect{cell.left(), cell.top(), cell.width(), cell.height()},
                         Qt::AlignLeft | Qt::AlignVCenter, QString{character});
        ++column;
    }
}

void HbTube::paintEvent(QPaintEvent*) {
    QPainter painter{this};
    painter.fillRect(rect(), palette_.background);
    // The bezel: a screen of the period had a border, and `POKE` can change its
    // colour -- it is register two of the I/O page.
    QColor border = palette_.border;
    if (memory_ != nullptr && memory_->border() != 0) {
        border = inkColour(memory_->border() & 0x0F);
    }
    painter.setPen(border);
    const QRect glass = rect().adjusted(1, 1, -2, -2);
    painter.drawRect(glass);

    painter.setRenderHint(QPainter::TextAntialiasing, false);
    if (memory_ == nullptr) {
        return;
    }
    if (scroll_offset_ > 0) {
        // Scrolled back: paint the transcript rather than the live cells, which is
        // what every emulator of a machine this size has had to do.
        const int first = std::max(
            0, static_cast<int>(transcript_.size()) - HbMemory::kRows - scroll_offset_);
        for (int row = 0; row < HbMemory::kRows; ++row) {
            const int index = first + row;
            if (index < 0 || index >= transcript_.size()) {
                continue;
            }
            paintRow(painter, row, transcript_.at(index), false);
        }
        return;
    }

    for (int row = 0; row < HbMemory::kRows; ++row) {
        QString text;
        text.reserve(HbMemory::kColumns);
        for (int column = 0; column < HbMemory::kColumns; ++column) {
            const std::uint8_t code = memory_->cell(row, column);
            // Every code the memory holds is passed through, including the graphics
            // set: a code the painter turns into a space here is a cell the machine
            // can write and the tube cannot show.
            text.append(code == 0 ? QLatin1Char(' ') : QChar{code});
        }
        paintRow(painter, row, text, true);
    }

    // The cursor, as a block: a cell's worth of phosphor under the character it will
    // overwrite.
    if (cursor_visible_) {
        const QRect cursor =
            cellRect(memory_->cursorRow(), memory_->cursorColumn()).adjusted(0, 0, -1, -1);
        QColor glow = palette_.bright;
        glow.setAlpha(150);
        painter.fillRect(cursor, glow);
    }
}

void HbTube::wheelEvent(QWheelEvent* event) {
    scrollBack(event->angleDelta().y() > 0 ? -3 : 3);
    event->accept();
}

void HbTube::keyPressEvent(QKeyEvent* event) {
    if (event->key() == Qt::Key_PageUp) {
        scrollBack(-HbMemory::kRows / 2);
        event->accept();
        return;
    }
    if (event->key() == Qt::Key_PageDown) {
        scrollBack(HbMemory::kRows / 2);
        event->accept();
        return;
    }
    QWidget::keyPressEvent(event);
}

void HbTube::resizeEvent(QResizeEvent* event) {
    QWidget::resizeEvent(event);
    update();
}

}  // namespace timewarp::ui
