// SPDX-License-Identifier: MIT
//
// The HB-1's 64K: one address space, and the screen is part of it.
//
// ---------------------------------------------------------------------------
// Why this exists
// ---------------------------------------------------------------------------
//
// Until 20.1 the machine's memory was a `std::map<int,int>` that `POKE` wrote
// into and `PEEK` read back, and **nothing else looked at**. `MEM 64K` on the
// status line was a string literal. That is the one thing a home computer of 1982
// cannot be: a machine whose memory is inert is a costume with a keyboard.
//
// So this is a real 64 KiB, with a documented map:
//
//      $0000 .. $7FFF   free RAM             32768 bytes, for POKE and machine code
//      $8000 .. $8FFF   screen cells         80 x 25 codes, 2000 bytes, row-major
//      $9000 .. $9FFF   colour attributes    one ink/paper pair per cell
//      $A000 .. $A0FF   I/O registers        cursor, border, ink, paper, sound, frame
//      $F000 .. $FFFF   ROM space            reads from the ROM table, writes refused
//
// The ranges between are unmapped and read as zero: a machine built down to a
// price had holes in its map too, and a hole that answers zero is easier to reason
// about than one that answers an error.
//
// The class knows nothing about widgets, exactly like `HbDos`: every operation is
// a plain call, the window turns them into pixels, and the tests drive the same
// methods a program reaches through `POKE`.

#pragma once

#include <array>
#include <cstdint>

#include <QString>

namespace timewarp::ui {

class HbMemory {
public:
    static constexpr int kSize = 0x10000;

    /// Where each region starts. These are the machine's, not an implementation
    /// detail: a program POKEs them.
    static constexpr int kScreenBase = 0x8000;
    static constexpr int kAttributeBase = 0x9000;
    static constexpr int kIoBase = 0xA000;
    static constexpr int kRomBase = 0xF000;

    /// The tube is 80 x 25, which is the text mode this machine already documents
    /// (`SCREEN 0`): 40 columns would wrap the banner and every DOS catalogue line.
    static constexpr int kColumns = 80;
    static constexpr int kRows = 25;
    static constexpr int kCellCount = kColumns * kRows;

    /// Bytes a program may use for its own purposes.
    static constexpr int kUserRamSize = kScreenBase;

    /// Registers in the I/O page, as offsets from `kIoBase`. A POKE here is an
    /// output to a device, which is what makes the page I/O rather than memory.
    enum Register : int {
        kCursorColumn = 0x00,
        kCursorRow = 0x01,
        kBorderColour = 0x02,
        kInk = 0x03,
        kPaper = 0x04,
        /// A non-zero value means "sound this note": the tube reads it, plays, and
        /// writes back zero, so a program does not have to.
        kSoundLatch = 0x05,
        /// The frame counter, low byte then high. It ticks once a frame, which is
        /// the machine's only clock a program can read.
        kFrameLow = 0x06,
        kFrameHigh = 0x07,
        /// Set to 1 by the machine after it scrolls, cleared by the program. It is
        /// how a program notices that the screen moved under it.
        kScrolled = 0x08,
    };

    HbMemory();

    /// Everything to zero, the screen to spaces, the cursor home. A cold boot.
    void clear();

    // -- the address space -------------------------------------------------

    [[nodiscard]] std::uint8_t peek(int address) const;
    /// Writes `value`. False for ROM space, where a write cannot land: a program
    /// that pokes the ROM is told so rather than having its byte vanish.
    bool poke(int address, std::uint8_t value);
    /// True where a `poke` would land.
    [[nodiscard]] bool writable(int address) const noexcept;
    /// A stable sum of the whole space, for the self-test and for tests here.
    [[nodiscard]] std::uint16_t checksum() const;

    // -- the screen --------------------------------------------------------

    [[nodiscard]] int cursorRow() const;
    [[nodiscard]] int cursorColumn() const;
    void setCursor(int row, int column);

    [[nodiscard]] std::uint8_t cell(int row, int column) const;
    void setCell(int row, int column, std::uint8_t code);
    /// The ink and paper nibble pair for one cell.
    void setAttribute(int row, int column, std::uint8_t ink, std::uint8_t paper);
    [[nodiscard]] std::uint8_t inkAt(int row, int column) const;
    [[nodiscard]] std::uint8_t paperAt(int row, int column) const;

    /// Moves every row up one and blanks the last, **in memory**, so the `PEEK` a
    /// program does afterwards sees the same screen the eye does.
    void scrollUp();
    void home();

    /// Writes `text` at the cursor with the current ink and paper, wrapping at the
    /// right edge and scrolling at the bottom. Returns the number of rows used.
    ///
    /// A newline in `text` starts a new row; the caller decides whether the cursor
    /// is left at the end of the text or at the start of the next row, because the
    /// machine's `PRINT` and a prompt echo disagree about that.
    int writeText(const QString& text, bool advanceToNewRow = true);

    /// One row as characters, for a test, an export or a bug report. Codes 32..126
    /// are themselves; the graphics characters above come back as the placeholder
    /// this build uses for them.
    [[nodiscard]] QString rowText(int row) const;

    /// The whole tube as rows joined by newlines, trailing blanks removed.
    [[nodiscard]] QString screenText() const;

    // -- the I/O page ------------------------------------------------------

    [[nodiscard]] std::uint8_t ink() const;
    [[nodiscard]] std::uint8_t paper() const;
    void setInk(std::uint8_t ink);
    void setPaper(std::uint8_t paper);
    [[nodiscard]] std::uint8_t border() const;
    void setBorder(std::uint8_t colour);

    /// The sound latch: a program writes a code and the machine plays it. Reading
    /// the latch clears it, so an event is delivered once.
    [[nodiscard]] std::uint8_t takeSoundLatch();
    [[nodiscard]] std::uint16_t frame() const;
    void tickFrame();
    [[nodiscard]] bool scrolled() const;
    void clearScrolled();

private:
    [[nodiscard]] static bool inRange(int base, int size, int address) noexcept {
        return address >= base && address < base + size;
    }
    [[nodiscard]] std::uint8_t& at(int address) { return bytes_[static_cast<std::size_t>(address)]; }
    [[nodiscard]] std::uint8_t at(int address) const {
        return bytes_[static_cast<std::size_t>(address)];
    }

    std::array<std::uint8_t, kSize> bytes_{};
};

}  // namespace timewarp::ui
