// SPDX-License-Identifier: MIT
//
// The HB-2's video chip: one framebuffer, three modes, a DAC, a blitter and sixteen
// sprites.
//
// ---------------------------------------------------------------------------
// Why it is one device and not three
// ---------------------------------------------------------------------------
//
// The 1982 machine's tube painted cells straight out of memory and had no picture
// smaller than a character. This chip keeps that as `MODE 0` -- the same 80 x 25
// cells, at the same address, byte for byte -- and adds the two bitmaps a 1992
// machine has: 320 x 200 with one byte per pixel, so a `POKE` draws, and 640 x 480
// with four bits per pixel and sixteen colours from a 256-entry DAC.
//
// Everything is addressed through the same window the bus routes to, so a program
// reaches the chip with `POKE` and the blitter with the same stores the manual
// prints. The blitter exists because a processor that moves every pixel spends all
// its time doing it: `fill`, `line`, `copy`, `masked copy`, `xor` and `glyph` are
// one store each, and each costs a fixed, counted number of ticks.
//
// Colour does not replace the tube. **HB Mono** is the brightness ramp as it always
// was; **HB Colour** is the DAC. Both are honest views of one framebuffer, and which
// one a monitor shows is the window's decision, not the machine's.

#pragma once

#include <array>
#include <cstdint>
#include <string>
#include <vector>

#include "timewarp/hb2/device.hpp"

namespace timewarp::hb2 {

/// A DAC entry: three bits per channel, which is what a 1992 palette had room for
/// and what makes 256 entries fit in a kilobyte.
struct Rgb {
    std::uint8_t r = 0;
    std::uint8_t g = 0;
    std::uint8_t b = 0;

    friend bool operator==(const Rgb&, const Rgb&) = default;
};

class Video final : public Device {
public:
    /// The three modes. `Text` is the 1982 tube; the other two are the machine's own.
    enum class Mode : std::uint8_t { Text = 0, Low = 1, High = 2 };

    static constexpr int kTextColumns = 80;
    static constexpr int kTextRows = 25;
    static constexpr int kTextCells = kTextColumns * kTextRows;

    static constexpr int kLowWidth = 320;
    static constexpr int kLowHeight = 200;
    static constexpr int kHighWidth = 640;
    static constexpr int kHighHeight = 480;

    static constexpr int kSpriteCount = 16;
    static constexpr int kSpriteSize = 16;
    static constexpr int kSpriteBytes = (kSpriteSize * kSpriteSize) / 8;
    static constexpr int kPatternCount = 16;

    /// Where the chip's windows sit in its own address space. The bus maps a
    /// program's address onto these; `devices.inc` names the program's side.
    static constexpr std::uint32_t kFramebufferOffset = 0x00000;
    static constexpr std::uint32_t kCellsOffset = 0x40000;
    static constexpr std::uint32_t kAttributesOffset = 0x41000;
    static constexpr std::uint32_t kRegisterOffset = 0x42000;
    static constexpr std::uint32_t kRegisterBytes = 0x1000;

    /// Registers, as offsets from `kRegisterOffset`. The first eight are the 1982
    /// machine's I/O page, at the offsets it had them, which is what makes `MODE 0`
    /// free rather than a translation layer.
    enum Register : std::uint32_t {
        kCursorColumn = 0x00,
        kCursorRow = 0x01,
        kBorder = 0x02,
        kInk = 0x03,
        kPaper = 0x04,
        kSoundLatch = 0x05,
        kFrameLow = 0x06,
        kFrameHigh = 0x07,

        kModeRegister = 0x08,
        kStatusLow = 0x09,
        kStatusHigh = 0x0A,

        kX0Low = 0x10,
        kX0High = 0x11,
        kY0Low = 0x12,
        kY0High = 0x13,
        kX1Low = 0x14,
        kX1High = 0x15,
        kY1Low = 0x16,
        kY1High = 0x17,
        kDstXLow = 0x18,
        kDstXHigh = 0x19,
        kDstYLow = 0x1A,
        kDstYHigh = 0x1B,
        kColour = 0x1C,
        kBlitOp = 0x1D,
        kBlitGo = 0x1E,
        kLastTicksLow = 0x1F,
        kLastTicksHigh = 0x20,
        kSource0 = 0x21,
        kSource1 = 0x22,
        kSource2 = 0x23,

        kPaletteIndex = 0x30,
        kPaletteRed = 0x31,
        kPaletteGreen = 0x32,
        kPaletteBlue = 0x33,

        kCollision = 0x48,
        kSpriteBase = 0x50,
        kSpriteStride = 8,
        kPatternBase = 0x800,
    };

    /// The blitter's operations, as written to `kBlitOp`.
    enum BlitOp : std::uint8_t {
        kFill = 1,
        kLine = 2,
        kCopy = 3,
        kMaskedCopy = 4,
        kXor = 5,
        kGlyph = 6,
    };

    Video();

    // -- Device ------------------------------------------------------------
    [[nodiscard]] std::uint8_t read(std::uint32_t offset) const override;
    void write(std::uint32_t offset, std::uint8_t value) override;
    void tick() override;
    void reset() override;

    // -- Mode and the DAC --------------------------------------------------
    [[nodiscard]] Mode mode() const;
    void setMode(Mode mode);
    [[nodiscard]] std::uint64_t frames() const;
    [[nodiscard]] Rgb dac(int index) const;
    void setDac(int index, Rgb colour);
    [[nodiscard]] int width() const;
    [[nodiscard]] int height() const;

    // -- The 1982 text path ------------------------------------------------
    void clearText();
    void putCell(int column, int row, std::uint8_t code);
    [[nodiscard]] std::uint8_t cell(int column, int row) const;
    /// Writes `code` at the cursor and advances it, which is what the machine's
    /// console service does for every character a program prints.
    void putChar(std::uint8_t code);
    [[nodiscard]] std::string line(int row) const;
    [[nodiscard]] std::vector<std::string> screenText() const;

    // -- The bitmap --------------------------------------------------------
    void clearBitmap();
    /// The composed picture: the bitmap with the sprites over it. Zero in text
    /// mode, where there is no bitmap to have pixels in.
    [[nodiscard]] std::uint8_t pixel(int x, int y) const;
    void setPixel(int x, int y, std::uint8_t colour);
    [[nodiscard]] std::uint64_t hash() const;

    // -- The blitter -------------------------------------------------------
    // Each returns the ticks it cost, and leaves them in the status register.
    std::uint32_t fill(int x, int y, int w, int h, std::uint8_t colour);
    std::uint32_t line(int x0, int y0, int x1, int y1, std::uint8_t colour);
    std::uint32_t rectangleXor(int x, int y, int w, int h, std::uint8_t colour);
    std::uint32_t copy(int sx, int sy, int w, int h, int dx, int dy);
    std::uint32_t maskedCopy(int sx, int sy, int w, int h, int dx, int dy);
    std::uint32_t glyph(int x, int y, std::uint32_t source, std::uint8_t colour);
    /// Runs whatever the parameter registers describe, which is what writing
    /// `kBlitGo` does. Returns the ticks, or zero for an operation the chip does
    /// not have.
    std::uint32_t executeBlit();

    // -- Sprites -----------------------------------------------------------
    void setSprite(int index, bool enabled, int x, int y, int pattern, std::uint8_t colour);
    void loadPattern(int pattern, const std::vector<std::uint8_t>& bytes);
    /// Reads the collision register, clearing it: the machine's promise is that a
    /// program that wants to know whether two sprites met asks once.
    bool takeCollision();
    /// Reads the 1982 sound latch, clearing it. The machine drains it into channel
    /// zero on the frame, which is why every existing `SOUND` still sounds.
    std::uint8_t takeSoundLatch();

    /// The whole chip as bytes -- framebuffer, cells, attributes and registers -- for
    /// the machine's snapshot. One call rather than four, so a later device cannot be
    /// left out of a saved image.
    [[nodiscard]] std::vector<std::uint8_t> state() const;
    void setState(const std::vector<std::uint8_t>& bytes);

private:
    [[nodiscard]] std::uint8_t registerAt(std::uint32_t offset) const;
    void pokeRegister(std::uint32_t offset, std::uint8_t value);
    [[nodiscard]] std::uint32_t register16(std::uint32_t low) const;
    void setRegister16(std::uint32_t low, std::uint32_t value);
    void compose() const;
    [[nodiscard]] int pixelIndex(int x, int y) const;
    void putIndex(int x, int y, std::uint8_t colour);
    void markDirty() const noexcept { dirty_ = true; }

    std::array<std::uint8_t, static_cast<std::size_t>(kRegisterBytes)> registers_{};
    std::vector<std::uint8_t> framebuffer_;
    std::vector<std::uint8_t> cells_;
    std::vector<std::uint8_t> attributes_;
    std::array<Rgb, 256> palette_{};
    std::array<std::uint64_t, kSpriteCount> last_tick_frame_{};
    mutable std::vector<std::uint8_t> composed_;
    mutable bool dirty_ = true;
    mutable bool collision_ = false;
    std::uint64_t frames_ = 0;
};

}  // namespace timewarp::hb2
