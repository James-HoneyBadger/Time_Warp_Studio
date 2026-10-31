// SPDX-License-Identifier: MIT

#include "timewarp/hb2/video.hpp"

#include <algorithm>
#include <cstddef>
#include <cstring>

namespace timewarp::hb2 {

namespace {

constexpr std::uint64_t kFnvOffset = 0xCBF29CE484222325ULL;
constexpr std::uint64_t kFnvPrime = 0x100000001B3ULL;

std::uint64_t mix(std::uint64_t hash, std::uint8_t byte) {
    return (hash ^ byte) * kFnvPrime;
}

/// The sixteen colours a 1992 machine shipped with, so that `MODE 2` with the DAC
/// untouched is a picture rather than sixteen shades of the same default.
constexpr std::array<Rgb, 16> kDefaultPalette{{
    {0, 0, 0}, {0, 0, 5}, {0, 5, 0}, {0, 5, 5},
    {5, 0, 0}, {5, 0, 5}, {3, 3, 0}, {5, 5, 5},
    {2, 2, 2}, {3, 4, 7}, {3, 7, 3}, {3, 7, 7},
    {7, 3, 3}, {7, 3, 7}, {7, 7, 3}, {7, 7, 7},
}};

}  // namespace

Video::Video()
    : framebuffer_(kHighWidth * kHighHeight / 2, 0),
      cells_(kTextCells, ' '),
      attributes_(kTextCells, 0x07),
      composed_(kLowWidth * kLowHeight, 0) {
    reset();
}

void Video::reset() {
    registers_.fill(0);
    std::fill(framebuffer_.begin(), framebuffer_.end(), std::uint8_t{0});
    std::fill(cells_.begin(), cells_.end(), std::uint8_t{' '});
    std::fill(attributes_.begin(), attributes_.end(), std::uint8_t{0x07});
    std::fill(composed_.begin(), composed_.end(), std::uint8_t{0});
    palette_.fill(Rgb{});
    for (std::size_t index = 0; index < kDefaultPalette.size(); ++index) {
        palette_[index] = kDefaultPalette[index];
    }
    // Above sixteen, a 3-3-2 scatter, so a program that pokes the DAC has somewhere
    // sensible to start from rather than a wall of black.
    for (int index = 16; index < 256; ++index) {
        palette_[static_cast<std::size_t>(index)] = Rgb{
            static_cast<std::uint8_t>((index >> 5) & 7),
            static_cast<std::uint8_t>((index >> 2) & 7),
            static_cast<std::uint8_t>(index & 7)};
    }
    registers_[kModeRegister] = static_cast<std::uint8_t>(Mode::Text);
    registers_[kInk] = 0x0F;
    registers_[kPaper] = 0x00;
    frames_ = 0;
    dirty_ = true;
    collision_ = false;
}

// ---------------------------------------------------------------------------
// Mode, DAC and geometry
// ---------------------------------------------------------------------------

Video::Mode Video::mode() const {
    const std::uint8_t value = registers_[kModeRegister];
    return value <= static_cast<std::uint8_t>(Mode::High) ? static_cast<Mode>(value) : Mode::Text;
}

void Video::setMode(Mode mode) {
    registers_[kModeRegister] = static_cast<std::uint8_t>(mode);
    dirty_ = true;
}

std::uint64_t Video::frames() const { return frames_; }

Rgb Video::dac(int index) const {
    return palette_[static_cast<std::size_t>(index & 0xFF)];
}

void Video::setDac(int index, Rgb colour) {
    const auto clamp = [](std::uint8_t channel) {
        return static_cast<std::uint8_t>(std::min<std::uint8_t>(channel, 7));
    };
    palette_[static_cast<std::size_t>(index & 0xFF)] =
        Rgb{clamp(colour.r), clamp(colour.g), clamp(colour.b)};
}

int Video::width() const {
    switch (mode()) {
    case Mode::Low:
        return kLowWidth;
    case Mode::High:
        return kHighWidth;
    case Mode::Text:
        break;
    }
    return 0;
}

int Video::height() const {
    switch (mode()) {
    case Mode::Low:
        return kLowHeight;
    case Mode::High:
        return kHighHeight;
    case Mode::Text:
        break;
    }
    return 0;
}

// ---------------------------------------------------------------------------
// The text path
// ---------------------------------------------------------------------------

void Video::clearText() {
    std::fill(cells_.begin(), cells_.end(), std::uint8_t{' '});
    std::fill(attributes_.begin(), attributes_.end(), std::uint8_t{0x07});
    registers_[kCursorColumn] = 0;
    registers_[kCursorRow] = 0;
    dirty_ = true;
}

void Video::putCell(int column, int row, std::uint8_t code) {
    if (column < 0 || column >= kTextColumns || row < 0 || row >= kTextRows) {
        return;
    }
    cells_[static_cast<std::size_t>(row * kTextColumns + column)] = code;
}

std::uint8_t Video::cell(int column, int row) const {
    if (column < 0 || column >= kTextColumns || row < 0 || row >= kTextRows) {
        return 0;
    }
    return cells_[static_cast<std::size_t>(row * kTextColumns + column)];
}

void Video::putChar(std::uint8_t code) {
    if (code == '\n') {
        registers_[kCursorColumn] = 0;
        ++registers_[kCursorRow];
    } else if (code == '\r') {
        registers_[kCursorColumn] = 0;
    } else {
        putCell(registers_[kCursorColumn], registers_[kCursorRow], code);
        if (++registers_[kCursorColumn] >= kTextColumns) {
            registers_[kCursorColumn] = 0;
            ++registers_[kCursorRow];
        }
    }
    // A tube that scrolls is a tube whose memory moves: the rows shift up by one,
    // which is what a `LOCATE 1, 25` program expects to be able to do next.
    while (registers_[kCursorRow] >= kTextRows) {
        std::memmove(cells_.data(), cells_.data() + kTextColumns,
                     static_cast<std::size_t>(kTextColumns * (kTextRows - 1)));
        std::memmove(attributes_.data(), attributes_.data() + kTextColumns,
                     static_cast<std::size_t>(kTextColumns * (kTextRows - 1)));
        std::fill(cells_.end() - kTextColumns, cells_.end(), std::uint8_t{' '});
        std::fill(attributes_.end() - kTextColumns, attributes_.end(), std::uint8_t{0x07});
        --registers_[kCursorRow];
    }
}

std::string Video::line(int row) const {
    std::string text;
    text.reserve(kTextColumns);
    for (int column = 0; column < kTextColumns; ++column) {
        const std::uint8_t code = cell(column, row);
        text.push_back(code >= 32 && code < 127 ? static_cast<char>(code) : ' ');
    }
    while (!text.empty() && text.back() == ' ') {
        text.pop_back();
    }
    return text;
}

std::vector<std::string> Video::screenText() const {
    std::vector<std::string> rows;
    rows.reserve(kTextRows);
    for (int row = 0; row < kTextRows; ++row) {
        rows.push_back(line(row));
    }
    return rows;
}

// ---------------------------------------------------------------------------
// The bitmap
// ---------------------------------------------------------------------------

void Video::clearBitmap() {
    std::fill(framebuffer_.begin(), framebuffer_.end(), std::uint8_t{0});
    dirty_ = true;
}

int Video::pixelIndex(int x, int y) const {
    if (x < 0 || y < 0) {
        return 0;
    }
    switch (mode()) {
    case Mode::Low: {
        if (x >= kLowWidth || y >= kLowHeight) {
            return 0;
        }
        return framebuffer_[static_cast<std::size_t>(y * kLowWidth + x)];
    }
    case Mode::High: {
        if (x >= kHighWidth || y >= kHighHeight) {
            return 0;
        }
        const std::size_t at = static_cast<std::size_t>(y * kHighWidth + x);
        const std::uint8_t packed = framebuffer_[at / 2];
        return (at % 2 == 0) ? (packed >> 4) : (packed & 0x0F);
    }
    case Mode::Text:
        break;
    }
    return 0;
}

void Video::putIndex(int x, int y, std::uint8_t colour) {
    if (x < 0 || y < 0) {
        return;
    }
    switch (mode()) {
    case Mode::Low: {
        if (x >= kLowWidth || y >= kLowHeight) {
            return;
        }
        framebuffer_[static_cast<std::size_t>(y * kLowWidth + x)] = colour;
        break;
    }
    case Mode::High: {
        if (x >= kHighWidth || y >= kHighHeight) {
            return;
        }
        const std::size_t at = static_cast<std::size_t>(y * kHighWidth + x);
        const std::uint8_t nibble = static_cast<std::uint8_t>(colour & 0x0F);
        std::uint8_t& packed = framebuffer_[at / 2];
        packed = (at % 2 == 0) ? static_cast<std::uint8_t>((packed & 0x0F) | (nibble << 4))
                               : static_cast<std::uint8_t>((packed & 0xF0) | nibble);
        break;
    }
    case Mode::Text:
        return;
    }
    dirty_ = true;
}

void Video::setPixel(int x, int y, std::uint8_t colour) { putIndex(x, y, colour); }

std::uint8_t Video::pixel(int x, int y) const {
    compose();
    if (mode() == Mode::Text || x < 0 || y < 0 || x >= width() || y >= height()) {
        return 0;
    }
    return composed_[static_cast<std::size_t>(y * width() + x)];
}

void Video::compose() const {
    if (!dirty_) {
        return;
    }
    const int w = width();
    const int h = height();
    composed_.assign(static_cast<std::size_t>(std::max(0, w)) * static_cast<std::size_t>(std::max(0, h)),
                     0);
    collision_ = false;
    if (mode() == Mode::Text) {
        dirty_ = false;
        return;
    }
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            composed_[static_cast<std::size_t>(y * w + x)] = static_cast<std::uint8_t>(pixelIndex(x, y));
        }
    }

    // Sprites last, in index order, so "the higher number is in front" is a rule the
    // manual can state and a program can rely on.
    std::vector<std::uint8_t> covered(composed_.size(), 0);
    for (int sprite = 0; sprite < kSpriteCount; ++sprite) {
        const std::uint32_t base = kSpriteBase + static_cast<std::uint32_t>(sprite) * kSpriteStride;
        if ((registerAt(base) & 0x01U) == 0) {
            continue;
        }
        const int sx = static_cast<int>(register16(base + 1));
        const int sy = static_cast<int>(register16(base + 3));
        const int pattern = registerAt(base + 5) % kPatternCount;
        const std::uint8_t colour = registerAt(base + 6);
        for (int by = 0; by < kSpriteSize; ++by) {
            for (int bx = 0; bx < kSpriteSize; ++bx) {
                const std::uint32_t bits = kPatternBase +
                                           static_cast<std::uint32_t>(pattern * kSpriteBytes) +
                                           static_cast<std::uint32_t>(by * 2 + bx / 8);
                if ((registerAt(bits) & (0x80U >> (bx % 8))) == 0) {
                    continue;
                }
                const int px = sx + bx;
                const int py = sy + by;
                if (px < 0 || py < 0 || px >= w || py >= h) {
                    continue;
                }
                const std::size_t at = static_cast<std::size_t>(py * w + px);
                if (covered[at] != 0) {
                    collision_ = true;
                }
                covered[at] = 1;
                composed_[at] = colour;
            }
        }
    }
    dirty_ = false;
}

std::uint64_t Video::hash() const {
    compose();
    std::uint64_t value = kFnvOffset;
    value = mix(value, registers_[kModeRegister]);
    for (const std::uint8_t byte : composed_) {
        value = mix(value, byte);
    }
    for (const std::uint8_t byte : cells_) {
        value = mix(value, byte);
    }
    for (const std::uint8_t byte : attributes_) {
        value = mix(value, byte);
    }
    return value;
}

// ---------------------------------------------------------------------------
// The blitter
// ---------------------------------------------------------------------------

std::uint32_t Video::fill(int x, int y, int w, int h, std::uint8_t colour) {
    for (int row = 0; row < h; ++row) {
        for (int column = 0; column < w; ++column) {
            putIndex(x + column, y + row, colour);
        }
    }
    const std::uint32_t ticks = 2 + static_cast<std::uint32_t>(std::max(1, w * h) / 8);
    setRegister16(kLastTicksLow, ticks);
    return ticks;
}

std::uint32_t Video::line(int x0, int y0, int x1, int y1, std::uint8_t colour) {
    // A line whose ends are further apart than the screen could ever be is a program
    // mistake; it draws nothing rather than iterating towards a coordinate that is a
    // million pixels away -- the chip refuses, and the refusal costs the same ticks
    // as any other empty operation.
    if (std::abs(x1 - x0) > 8192 || std::abs(y1 - y0) > 8192) {
        setRegister16(kLastTicksLow, 2);
        return 2;
    }
    // Bresenham, with the error term kept in integers so two machines drawing the
    // same line draw the same pixels.
    int x = x0;
    int y = y0;
    const int dx = std::abs(x1 - x0);
    const int dy = -std::abs(y1 - y0);
    const int sx = x0 < x1 ? 1 : -1;
    const int sy = y0 < y1 ? 1 : -1;
    int error = dx + dy;
    int drawn = 0;
    for (;;) {
        putIndex(x, y, colour);
        ++drawn;
        if (x == x1 && y == y1) {
            break;
        }
        const int twice = 2 * error;
        if (twice >= dy) {
            error += dy;
            x += sx;
        }
        if (twice <= dx) {
            error += dx;
            y += sy;
        }
    }
    const std::uint32_t ticks = 2 + static_cast<std::uint32_t>(drawn);
    setRegister16(kLastTicksLow, ticks);
    return ticks;
}

std::uint32_t Video::rectangleXor(int x, int y, int w, int h, std::uint8_t colour) {
    for (int row = 0; row < h; ++row) {
        for (int column = 0; column < w; ++column) {
            const std::uint8_t existing =
                static_cast<std::uint8_t>(pixelIndex(x + column, y + row) ^ colour);
            putIndex(x + column, y + row, existing);
        }
    }
    const std::uint32_t ticks = 2 + static_cast<std::uint32_t>(std::max(1, w * h) / 8);
    setRegister16(kLastTicksLow, ticks);
    return ticks;
}

std::uint32_t Video::copy(int sx, int sy, int w, int h, int dx, int dy) {
    for (int row = 0; row < h; ++row) {
        for (int column = 0; column < w; ++column) {
            putIndex(dx + column, dy + row, static_cast<std::uint8_t>(pixelIndex(sx + column, sy + row)));
        }
    }
    const std::uint32_t ticks = 2 + static_cast<std::uint32_t>(std::max(1, w * h) / 4);
    setRegister16(kLastTicksLow, ticks);
    return ticks;
}

std::uint32_t Video::maskedCopy(int sx, int sy, int w, int h, int dx, int dy) {
    for (int row = 0; row < h; ++row) {
        for (int column = 0; column < w; ++column) {
            const std::uint8_t value = static_cast<std::uint8_t>(pixelIndex(sx + column, sy + row));
            // Zero is transparent, which is the whole of a mask's job: sprites,
            // cursors and text all compose that way.
            if (value == 0) {
                continue;
            }
            putIndex(dx + column, dy + row, value);
        }
    }
    const std::uint32_t ticks = 2 + static_cast<std::uint32_t>(std::max(1, w * h) / 4);
    setRegister16(kLastTicksLow, ticks);
    return ticks;
}

std::uint32_t Video::glyph(int x, int y, std::uint32_t source, std::uint8_t colour) {
    for (int row = 0; row < 8; ++row) {
        const std::uint8_t bits = read(source + static_cast<std::uint32_t>(row));
        for (int column = 0; column < 8; ++column) {
            if ((bits & (0x80U >> column)) != 0) {
                putIndex(x + column, y + row, colour);
            }
        }
    }
    const std::uint32_t ticks = 10;
    setRegister16(kLastTicksLow, ticks);
    return ticks;
}

std::uint32_t Video::executeBlit() {
    const int x0 = static_cast<int>(register16(kX0Low));
    const int y0 = static_cast<int>(register16(kY0Low));
    const int x1 = static_cast<int>(register16(kX1Low));
    const int y1 = static_cast<int>(register16(kY1Low));
    const int dx = static_cast<int>(register16(kDstXLow));
    const int dy = static_cast<int>(register16(kDstYLow));
    const std::uint8_t colour = registerAt(kColour);
    const std::uint32_t source = registerAt(kSource0) |
                                 (static_cast<std::uint32_t>(registerAt(kSource1)) << 8) |
                                 (static_cast<std::uint32_t>(registerAt(kSource2)) << 16);
    switch (registerAt(kBlitOp)) {
    case kFill:
        return fill(x0, y0, x1, y1, colour);
    case kLine:
        return line(x0, y0, x1, y1, colour);
    case kCopy:
        return copy(x0, y0, x1, y1, dx, dy);
    case kMaskedCopy:
        return maskedCopy(x0, y0, x1, y1, dx, dy);
    case kXor:
        return rectangleXor(x0, y0, x1, y1, colour);
    case kGlyph:
        return glyph(x0, y0, source, colour);
    default:
        break;
    }
    setRegister16(kLastTicksLow, 0);
    return 0;
}

// ---------------------------------------------------------------------------
// Sprites
// ---------------------------------------------------------------------------

void Video::setSprite(int index, bool enabled, int x, int y, int pattern, std::uint8_t colour) {
    if (index < 0 || index >= kSpriteCount) {
        return;
    }
    const std::uint32_t base = kSpriteBase + static_cast<std::uint32_t>(index) * kSpriteStride;
    registers_[base] = enabled ? 0x01 : 0x00;
    registers_[base + 1] = static_cast<std::uint8_t>(x & 0xFF);
    registers_[base + 2] = static_cast<std::uint8_t>((x >> 8) & 0xFF);
    registers_[base + 3] = static_cast<std::uint8_t>(y & 0xFF);
    registers_[base + 4] = static_cast<std::uint8_t>((y >> 8) & 0xFF);
    registers_[base + 5] = static_cast<std::uint8_t>(pattern % kPatternCount);
    registers_[base + 6] = colour;
    dirty_ = true;
}

void Video::loadPattern(int pattern, const std::vector<std::uint8_t>& bytes) {
    if (pattern < 0 || pattern >= kPatternCount) {
        return;
    }
    const std::size_t base = kPatternBase + static_cast<std::size_t>(pattern) * kSpriteBytes;
    for (std::size_t index = 0; index < bytes.size() && base + index < registers_.size(); ++index) {
        registers_[base + index] = bytes[index];
    }
    dirty_ = true;
}

bool Video::takeCollision() {
    compose();
    const bool met = collision_;
    collision_ = false;
    return met;
}

std::uint8_t Video::takeSoundLatch() {
    const std::uint8_t note = registers_[kSoundLatch];
    registers_[kSoundLatch] = 0;
    return note;
}

std::vector<std::uint8_t> Video::state() const {
    const std::uint32_t total = kRegisterOffset + kRegisterBytes;
    std::vector<std::uint8_t> bytes(total, 0);
    for (std::uint32_t offset = 0; offset < total; ++offset) {
        bytes[offset] = read(offset);
    }
    return bytes;
}

void Video::setState(const std::vector<std::uint8_t>& bytes) {
    const std::uint32_t total = kRegisterOffset + kRegisterBytes;
    for (std::uint32_t offset = 0; offset < total && offset < bytes.size(); ++offset) {
        write(offset, bytes[offset]);
    }
    dirty_ = true;
}

// ---------------------------------------------------------------------------
// Registers
// ---------------------------------------------------------------------------

std::uint8_t Video::registerAt(std::uint32_t offset) const {
    return offset < registers_.size() ? registers_[offset] : std::uint8_t{0};
}

std::uint32_t Video::register16(std::uint32_t low) const {
    return static_cast<std::uint32_t>(registerAt(low)) |
           (static_cast<std::uint32_t>(registerAt(low + 1)) << 8);
}

void Video::setRegister16(std::uint32_t low, std::uint32_t value) {
    registers_[low] = static_cast<std::uint8_t>(value & 0xFF);
    registers_[low + 1] = static_cast<std::uint8_t>((value >> 8) & 0xFF);
}

void Video::pokeRegister(std::uint32_t offset, std::uint8_t value) {
    switch (offset) {
    case kModeRegister:
        setMode(value <= static_cast<std::uint8_t>(Mode::High) ? static_cast<Mode>(value)
                                                               : Mode::Text);
        return;
    case kBlitGo:
        // A write is the go: the operation runs, and the ticks it took are left in
        // the status register for a program that wants to know what it cost.
        executeBlit();
        return;
    case kCollision:
        collision_ = false;
        return;
    case kPaletteRed:
        palette_[static_cast<std::size_t>(registerAt(kPaletteIndex))].r =
            static_cast<std::uint8_t>(std::min<std::uint8_t>(value, 7));
        return;
    case kPaletteGreen:
        palette_[static_cast<std::size_t>(registerAt(kPaletteIndex))].g =
            static_cast<std::uint8_t>(std::min<std::uint8_t>(value, 7));
        return;
    case kPaletteBlue:
        palette_[static_cast<std::size_t>(registerAt(kPaletteIndex))].b =
            static_cast<std::uint8_t>(std::min<std::uint8_t>(value, 7));
        return;
    case kCursorColumn:
        registers_[offset] = static_cast<std::uint8_t>(std::min<int>(value, kTextColumns - 1));
        return;
    case kCursorRow:
        registers_[offset] = static_cast<std::uint8_t>(std::min<int>(value, kTextRows - 1));
        return;
    default:
        break;
    }
    registers_[offset] = value;
    dirty_ = true;
}

std::uint8_t Video::read(std::uint32_t offset) const {
    if (offset < kCellsOffset) {
        return offset < framebuffer_.size() ? framebuffer_[offset] : std::uint8_t{0};
    }
    if (offset >= kCellsOffset && offset < kCellsOffset + kTextCells) {
        return cells_[offset - kCellsOffset];
    }
    if (offset >= kAttributesOffset && offset < kAttributesOffset + kTextCells) {
        return attributes_[offset - kAttributesOffset];
    }
    if (offset >= kRegisterOffset && offset < kRegisterOffset + kRegisterBytes) {
        const std::uint32_t at = offset - kRegisterOffset;
        return registerAt(at);
    }
    return 0;
}

void Video::write(std::uint32_t offset, std::uint8_t value) {
    if (offset < kCellsOffset) {
        if (offset < framebuffer_.size()) {
            framebuffer_[offset] = value;
            dirty_ = true;
        }
        return;
    }
    if (offset >= kCellsOffset && offset < kCellsOffset + kTextCells) {
        cells_[offset - kCellsOffset] = value;
        return;
    }
    if (offset >= kAttributesOffset && offset < kAttributesOffset + kTextCells) {
        attributes_[offset - kAttributesOffset] = value;
        return;
    }
    if (offset >= kRegisterOffset && offset < kRegisterOffset + kRegisterBytes) {
        pokeRegister(offset - kRegisterOffset, value);
    }
}

void Video::tick() {
    ++frames_;
    setRegister16(kFrameLow, static_cast<std::uint32_t>(frames_ & 0xFFFF));
    dirty_ = true;
}

}  // namespace timewarp::hb2
