// SPDX-License-Identifier: MIT

#include "timewarp/ui/hb_memory.hpp"

#include <algorithm>

#include <QStringList>

namespace timewarp::ui {

namespace {

constexpr std::uint8_t kSpace = 32;
constexpr int kAttributeBytes = HbMemory::kCellCount;
constexpr int kIoBytes = 0x100;
/// The power-on ink and paper, as nibbles. Light ink on black, which is what a
/// phosphor tube shows.
constexpr std::uint8_t kDefaultInk = 0x0F;
constexpr std::uint8_t kDefaultPaper = 0x00;

}  // namespace

HbMemory::HbMemory() { clear(); }

void HbMemory::clear() {
    bytes_.fill(0);
    // The screen is spaces, not NULs: a cleared tube shows nothing rather than 2000
    // copies of whatever code zero draws.
    std::fill(bytes_.begin() + kScreenBase, bytes_.begin() + kScreenBase + kCellCount,
              kSpace);
    // Every cell starts with the power-on pair in its own attribute byte, so a
    // program that pokes a character and nothing else still gets visible ink.
    for (int index = 0; index < kAttributeBytes; ++index) {
        at(kAttributeBase + index) = static_cast<std::uint8_t>((kDefaultInk << 4) |
                                                               kDefaultPaper);
    }
    setInk(kDefaultInk);
    setPaper(kDefaultPaper);
}

// -- the address space -----------------------------------------------------

bool HbMemory::writable(int address) const noexcept {
    if (address < 0 || address >= kSize) {
        return false;
    }
    return address < kRomBase;
}

std::uint8_t HbMemory::peek(int address) const {
    if (address < 0 || address >= kSize) {
        return 0;
    }
    // The frame counter is a register with a value the machine owns, so a read has
    // to be live rather than whatever a write happened to leave there.
    return at(address);
}

bool HbMemory::poke(int address, std::uint8_t value) {
    if (!writable(address)) {
        return false;
    }
    at(address) = value;
    return true;
}

std::uint16_t HbMemory::checksum() const {
    // FNV-1a, folded to 16 bits. It is a self-test sum, not a cryptographic one:
    // its job is to notice that the machine is not the machine that was tested.
    std::uint32_t hash = 2166136261u;
    for (const std::uint8_t byte : bytes_) {
        hash ^= byte;
        hash *= 16777619u;
    }
    return static_cast<std::uint16_t>((hash ^ (hash >> 16)) & 0xFFFFu);
}

// -- the screen ------------------------------------------------------------

int HbMemory::cursorRow() const { return std::min<int>(at(kIoBase + kCursorRow), kRows - 1); }

int HbMemory::cursorColumn() const {
    return std::min<int>(at(kIoBase + kCursorColumn), kColumns - 1);
}

void HbMemory::setCursor(int row, int column) {
    at(kIoBase + kCursorRow) = static_cast<std::uint8_t>(std::clamp(row, 0, kRows - 1));
    at(kIoBase + kCursorColumn) =
        static_cast<std::uint8_t>(std::clamp(column, 0, kColumns - 1));
}

std::uint8_t HbMemory::cell(int row, int column) const {
    if (row < 0 || row >= kRows || column < 0 || column >= kColumns) {
        return kSpace;
    }
    return at(kScreenBase + row * kColumns + column);
}

void HbMemory::setCell(int row, int column, std::uint8_t code) {
    if (row < 0 || row >= kRows || column < 0 || column >= kColumns) {
        return;
    }
    at(kScreenBase + row * kColumns + column) = code;
}

void HbMemory::setAttribute(int row, int column, std::uint8_t ink, std::uint8_t paper) {
    if (row < 0 || row >= kRows || column < 0 || column >= kColumns) {
        return;
    }
    at(kAttributeBase + row * kColumns + column) = static_cast<std::uint8_t>(
        ((ink & 0x0F) << 4) | (paper & 0x0F));
}

std::uint8_t HbMemory::inkAt(int row, int column) const {
    if (row < 0 || row >= kRows || column < 0 || column >= kColumns) {
        return ink();
    }
    return static_cast<std::uint8_t>(at(kAttributeBase + row * kColumns + column) >> 4);
}

std::uint8_t HbMemory::paperAt(int row, int column) const {
    if (row < 0 || row >= kRows || column < 0 || column >= kColumns) {
        return paper();
    }
    return static_cast<std::uint8_t>(at(kAttributeBase + row * kColumns + column) & 0x0F);
}

void HbMemory::scrollUp() {
    // By memory, not by a paint offset: `PEEK 32768` after a scroll has to answer
    // with what is now on the top row, or the machine's memory and its picture
    // disagree -- which is exactly the lie this class exists to end.
    std::copy(bytes_.begin() + kScreenBase + kColumns,
              bytes_.begin() + kScreenBase + kCellCount, bytes_.begin() + kScreenBase);
    std::fill(bytes_.begin() + kScreenBase + kCellCount - kColumns,
              bytes_.begin() + kScreenBase + kCellCount, kSpace);
    std::copy(bytes_.begin() + kAttributeBase + kColumns,
              bytes_.begin() + kAttributeBase + kAttributeBytes,
              bytes_.begin() + kAttributeBase);
    for (int index = kAttributeBytes - kColumns; index < kAttributeBytes; ++index) {
        at(kAttributeBase + index) =
            static_cast<std::uint8_t>((ink() << 4) | (paper() & 0x0F));
    }
    at(kIoBase + kScrolled) = 1;
}

void HbMemory::home() { setCursor(0, 0); }

int HbMemory::writeText(const QString& text, bool advanceToNewRow) {
    int row = cursorRow();
    int column = cursorColumn();
    const std::uint8_t pen = ink();
    const std::uint8_t background = paper();
    int rowsWritten = 0;
    for (const QChar character : text) {
        if (character == QLatin1Char('\n') || character == QLatin1Char('\r')) {
            if (character == QLatin1Char('\r')) {
                continue;
            }
            column = 0;
            ++row;
            ++rowsWritten;
            if (row >= kRows) {
                scrollUp();
                row = kRows - 1;
            }
            continue;
        }
        if (column >= kColumns) {
            column = 0;
            ++row;
            ++rowsWritten;
            if (row >= kRows) {
                scrollUp();
                row = kRows - 1;
            }
        }
        const int code = character.unicode();
        setCell(row, column, static_cast<std::uint8_t>(code > 0xFF ? '?' : code));
        setAttribute(row, column, pen, background);
        ++column;
    }
    if (advanceToNewRow) {
        column = 0;
        ++row;
        ++rowsWritten;
        if (row >= kRows) {
            scrollUp();
            row = kRows - 1;
        }
    }
    setCursor(row, column);
    return rowsWritten;
}

QString HbMemory::rowText(int row) const {
    QString text;
    text.reserve(kColumns);
    for (int column = 0; column < kColumns; ++column) {
        const std::uint8_t code = cell(row, column);
        if (code >= 32 && code < 127) {
            text.append(QChar{code});
        } else if (code == 0 || code == 32) {
            text.append(QLatin1Char(' '));
        } else {
            // The graphics characters have no ASCII spelling; an export says so
            // with the code's own placeholder rather than inventing a letter.
            text.append(QLatin1Char('#'));
        }
    }
    while (text.endsWith(QLatin1Char(' '))) {
        text.chop(1);
    }
    return text;
}

QString HbMemory::screenText() const {
    QStringList rows;
    rows.reserve(kRows);
    for (int row = 0; row < kRows; ++row) {
        rows.append(rowText(row));
    }
    while (!rows.isEmpty() && rows.last().isEmpty()) {
        rows.removeLast();
    }
    return rows.join(QLatin1Char('\n'));
}

// -- the I/O page ----------------------------------------------------------

std::uint8_t HbMemory::ink() const { return static_cast<std::uint8_t>(at(kIoBase + kInk) & 0x0F); }

std::uint8_t HbMemory::paper() const {
    return static_cast<std::uint8_t>(at(kIoBase + kPaper) & 0x0F);
}

void HbMemory::setInk(std::uint8_t ink) {
    at(kIoBase + kInk) = static_cast<std::uint8_t>(ink & 0x0F);
}

void HbMemory::setPaper(std::uint8_t paper) {
    at(kIoBase + kPaper) = static_cast<std::uint8_t>(paper & 0x0F);
}

std::uint8_t HbMemory::border() const { return at(kIoBase + kBorderColour); }

void HbMemory::setBorder(std::uint8_t colour) { at(kIoBase + kBorderColour) = colour; }

std::uint8_t HbMemory::takeSoundLatch() {
    const std::uint8_t value = at(kIoBase + kSoundLatch);
    at(kIoBase + kSoundLatch) = 0;
    return value;
}

std::uint16_t HbMemory::frame() const {
    return static_cast<std::uint16_t>(at(kIoBase + kFrameLow) |
                                      (at(kIoBase + kFrameHigh) << 8));
}

void HbMemory::tickFrame() {
    const auto next = static_cast<std::uint16_t>(frame() + 1);
    at(kIoBase + kFrameLow) = static_cast<std::uint8_t>(next & 0xFF);
    at(kIoBase + kFrameHigh) = static_cast<std::uint8_t>(next >> 8);
}

bool HbMemory::scrolled() const { return at(kIoBase + kScrolled) != 0; }

void HbMemory::clearScrolled() { at(kIoBase + kScrolled) = 0; }

}  // namespace timewarp::ui
