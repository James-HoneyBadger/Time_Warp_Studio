// SPDX-License-Identifier: MIT
//
// CHIP-8 assembler and machine, ported from `time_warp/languages/chip8.py`.
//
// The two passes matter for the same reason they do in the reference: a jump
// forward to a label cannot be encoded until every label's address is known.
// The machine is byte-for-byte the 1977 one the reference implements, down to
// the choices a later emulator would make differently (Vx shifts in place,
// sprites wrap).

#include "timewarp/lang/chip8.hpp"

#include "timewarp/lang/python_random.hpp"
#include "timewarp/lang/turtle_host.hpp"

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstdint>
#include <optional>
#include <random>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace timewarp::lang {
namespace {

constexpr std::uint8_t kFont[] = {
    0xF0, 0x90, 0x90, 0x90, 0xF0,  // 0
    0x20, 0x60, 0x20, 0x20, 0x70,  // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0,  // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0,  // 3
    0x90, 0x90, 0xF0, 0x10, 0x10,  // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0,  // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0,  // 6
    0xF0, 0x10, 0x20, 0x40, 0x40,  // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0,  // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0,  // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90,  // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0,  // B
    0xF0, 0x80, 0x80, 0x80, 0xF0,  // C
    0xE0, 0x90, 0x90, 0x90, 0xE0,  // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0,  // E
    0xF0, 0x80, 0xF0, 0x80, 0x80,  // F
};

bool is_space(char character) {
    return std::isspace(static_cast<unsigned char>(character)) != 0;
}

std::string trim(std::string_view text) {
    std::size_t start = 0;
    std::size_t end = text.size();
    while (start < end && is_space(text[start])) {
        ++start;
    }
    while (end > start && is_space(text[end - 1])) {
        --end;
    }
    return std::string{text.substr(start, end - start)};
}

std::string upper(std::string_view text) {
    std::string result{text};
    for (char& character : result) {
        character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    return result;
}

std::string hex_upper(unsigned value, int width) {
    char buffer[16];
    const auto result = std::to_chars(buffer, buffer + sizeof buffer, value, 16);
    std::string text{buffer, result.ptr};
    for (char& character : text) {
        character = static_cast<char>(std::toupper(static_cast<unsigned char>(character)));
    }
    while (static_cast<int>(text.size()) < width) {
        text.insert(text.begin(), '0');
    }
    return text;
}

bool starts_with_ci(std::string_view text, std::string_view prefix) {
    if (text.size() < prefix.size()) {
        return false;
    }
    for (std::size_t index = 0; index < prefix.size(); ++index) {
        if (std::tolower(static_cast<unsigned char>(text[index])) !=
            std::tolower(static_cast<unsigned char>(prefix[index]))) {
            return false;
        }
    }
    return true;
}

/// A literal: decimal unless it says otherwise, in either hex spelling.
int parse_number(std::string_view raw) {
    const std::string text = trim(raw);
    bool negative = false;
    std::size_t start = 0;
    if (start < text.size() && (text[start] == '+' || text[start] == '-')) {
        negative = text[start] == '-';
        ++start;
    }
    std::string body = text.substr(start);
    int base = 10;
    if (!body.empty() && (body.back() == 'h' || body.back() == 'H')) {
        body.pop_back();
        base = 16;
    } else if (starts_with_ci(body, "0x")) {
        body = body.substr(2);
        base = 16;
    }
    if (body.empty()) {
        throw Chip8Error("'" + text + "' is not a number");
    }
    int value = 0;
    const auto result = std::from_chars(body.data(), body.data() + body.size(), value, base);
    if (result.ec != std::errc{} || result.ptr != body.data() + body.size()) {
        throw Chip8Error("'" + text + "' is not a number");
    }
    return negative ? -value : value;
}

int parse_byte(std::string_view text) {
    const int value = parse_number(text);
    if (value < 0 || value > 0xFF) {
        throw Chip8Error(trim(text) + " is not a byte");
    }
    return value;
}

int parse_word(std::string_view text) {
    const int value = parse_number(text);
    if (value < 0 || value > 0xFFFF) {
        throw Chip8Error(trim(text) + " is not a 16-bit value");
    }
    return value;
}

/// `V0`-`VF`, and nothing else. Two hex digits after the V is a named register,
/// not a register and an operand.
int parse_register(std::string_view raw) {
    const std::string text = trim(raw);
    if (text.size() != 2 || (text[0] != 'V' && text[0] != 'v') ||
        !std::isxdigit(static_cast<unsigned char>(text[1]))) {
        throw Chip8Error("'" + text + "' is not a register (V0-VF)");
    }
    const int value = std::isdigit(static_cast<unsigned char>(text[1]))
                          ? text[1] - '0'
                          : std::tolower(static_cast<unsigned char>(text[1])) - 'a' + 10;
    return value;
}

bool is_register(std::string_view text) {
    const std::string trimmed = trim(text);
    return trimmed.size() == 2 && (trimmed[0] == 'V' || trimmed[0] == 'v') &&
           std::isxdigit(static_cast<unsigned char>(trimmed[1])) != 0;
}

std::string code_of(std::string_view line) {
    const std::size_t comment = line.find(';');
    return trim(comment == std::string_view::npos ? line : line.substr(0, comment));
}

/// The `name:` prefix of a statement, if it has one.
std::optional<std::pair<std::string, std::size_t>> label_prefix(std::string_view text) {
    if (text.empty() ||
        !(std::isalpha(static_cast<unsigned char>(text[0])) != 0 || text[0] == '_')) {
        return std::nullopt;
    }
    std::size_t index = 1;
    while (index < text.size() &&
           (std::isalnum(static_cast<unsigned char>(text[index])) != 0 || text[index] == '_')) {
        ++index;
    }
    std::size_t after = index;
    while (after < text.size() && is_space(text[after])) {
        ++after;
    }
    if (after >= text.size() || text[after] != ':') {
        return std::nullopt;
    }
    return std::make_pair(std::string{text.substr(0, index)}, after + 1);
}

std::vector<std::string> split_operands(std::string_view text) {
    std::vector<std::string> operands;
    std::string current;
    for (char character : text) {
        if (character == ',' || is_space(character)) {
            if (!current.empty()) {
                operands.push_back(current);
                current.clear();
            }
        } else {
            current.push_back(character);
        }
    }
    if (!current.empty()) {
        operands.push_back(current);
    }
    return operands;
}

int size_of(std::string_view text) {
    std::size_t end = 0;
    while (end < text.size() && !is_space(text[end])) {
        ++end;
    }
    return upper(text.substr(0, end)) == "DB" ? 1 : 2;
}

std::vector<std::uint8_t> encode(std::string_view text,
                                 const std::map<std::string, int>& labels, int number) {
    const std::size_t split = text.find(' ');
    const std::string mnemonic = upper(text.substr(0, split));
    const std::vector<std::string> operands =
        split == std::string_view::npos ? std::vector<std::string>{}
                                        : split_operands(text.substr(split + 1));

    const auto target = [&labels](std::string_view raw) {
        const std::string operand = trim(raw);
        const auto found = labels.find(operand);
        return found == labels.end() ? parse_word(operand) : found->second;
    };
    const auto fail = [&mnemonic, number](std::string_view expectation) {
        return Chip8Error("line " + std::to_string(number) + ": " + mnemonic + " " +
                          std::string{expectation});
    };

    if (mnemonic == "DB") {
        if (operands.size() != 1) {
            throw fail("takes one byte");
        }
        return {static_cast<std::uint8_t>(parse_byte(operands[0]))};
    }
    if (mnemonic == "DW") {
        if (operands.size() != 1) {
            throw fail("takes one word");
        }
        const int value = parse_word(operands[0]);
        return {static_cast<std::uint8_t>(value >> 8), static_cast<std::uint8_t>(value & 0xFF)};
    }

    if (mnemonic == "CLS") {
        return {0x00, 0xE0};
    }
    if (mnemonic == "RET") {
        return {0x00, 0xEE};
    }
    if (mnemonic == "SYS") {
        // 0NNN calls a routine on the RCA 1802 that hosted the original
        // interpreter. Nothing here can run it, so the machine ignores it, as
        // every interpreter since the original has; the assembler can still
        // spell it so a listing that contains one assembles.
        if (operands.size() != 1) {
            throw fail("takes an address");
        }
        const int value = target(operands[0]);
        return {static_cast<std::uint8_t>(value >> 8),
                static_cast<std::uint8_t>(value & 0xFF)};
    }
    if (mnemonic == "HALT") {
        // 0x0000 is unused by the original set, so a program can stop instead
        // of spinning on a jump-to-self to mark the end.
        return {0x00, 0x00};
    }

    if (mnemonic == "JP" && operands.size() == 1) {
        const int value = target(operands[0]);
        return {static_cast<std::uint8_t>(0x10 | (value >> 8)),
                static_cast<std::uint8_t>(value & 0xFF)};
    }
    if (mnemonic == "JP" && operands.size() == 2) {
        // Bnnn carries the address in its twelve bits; V0 is added at run time
        // rather than encoded. Naming another register would assemble to an
        // instruction that ignores it, so that is refused rather than accepted.
        if (parse_register(operands[0]) != 0) {
            throw fail("uses V0 only; Bnnn adds V0");
        }
        const int value = target(operands[1]);
        return {static_cast<std::uint8_t>(0xB0 | (value >> 8)),
                static_cast<std::uint8_t>(value & 0xFF)};
    }
    if (mnemonic == "CALL") {
        if (operands.size() != 1) {
            throw fail("takes an address");
        }
        const int value = target(operands[0]);
        return {static_cast<std::uint8_t>(0x20 | (value >> 8)),
                static_cast<std::uint8_t>(value & 0xFF)};
    }

    if ((mnemonic == "SE" || mnemonic == "SNE") && operands.size() == 2) {
        const int x = parse_register(operands[0]);
        if (is_register(operands[1])) {
            const int y = parse_register(operands[1]);
            const int base = mnemonic == "SE" ? 0x50 : 0x90;
            return {static_cast<std::uint8_t>(base | x),
                    static_cast<std::uint8_t>(y << 4)};
        }
        const int base = mnemonic == "SE" ? 0x30 : 0x40;
        return {static_cast<std::uint8_t>(base | x),
                static_cast<std::uint8_t>(parse_byte(operands[1]))};
    }

    if (mnemonic == "LD" && operands.size() == 2) {
        const std::string left = upper(operands[0]);
        const std::string right = upper(operands[1]);
        if (left == "I") {
            const int value = target(operands[1]);
            return {static_cast<std::uint8_t>(0xA0 | (value >> 8)),
                    static_cast<std::uint8_t>(value & 0xFF)};
        }
        // The conversions and the timers name the *destination* on the left, so
        // `LD F, V0` reads "point I at the glyph for V0".
        if (left == "DT" || left == "ST") {
            const int x = parse_register(operands[1]);
            return {static_cast<std::uint8_t>(0xF0 | x),
                    static_cast<std::uint8_t>(left == "DT" ? 0x15 : 0x18)};
        }
        if (left == "F") {
            const int x = parse_register(operands[1]);
            return {static_cast<std::uint8_t>(0xF0 | x), 0x29};
        }
        if (left == "B") {
            const int x = parse_register(operands[1]);
            return {static_cast<std::uint8_t>(0xF0 | x), 0x33};
        }
        if (left == "[I]") {
            const int x = parse_register(operands[1]);
            return {static_cast<std::uint8_t>(0xF0 | x), 0x55};
        }
        if (right == "[I]") {
            const int x = parse_register(operands[0]);
            return {static_cast<std::uint8_t>(0xF0 | x), 0x65};
        }
        if (right == "DT") {
            const int x = parse_register(operands[0]);
            return {static_cast<std::uint8_t>(0xF0 | x), 0x07};
        }
        if (right == "K") {
            const int x = parse_register(operands[0]);
            return {static_cast<std::uint8_t>(0xF0 | x), 0x0A};
        }
        const int x = parse_register(operands[0]);
        if (is_register(operands[1])) {
            return {static_cast<std::uint8_t>(0x80 | x),
                    static_cast<std::uint8_t>(parse_register(operands[1]) << 4)};
        }
        return {static_cast<std::uint8_t>(0x60 | x),
                static_cast<std::uint8_t>(parse_byte(operands[1]))};
    }

    if (mnemonic == "ADD" && operands.size() == 2) {
        if (upper(operands[0]) == "I") {
            const int x = parse_register(operands[1]);
            return {static_cast<std::uint8_t>(0xF0 | x), 0x1E};
        }
        const int x = parse_register(operands[0]);
        if (is_register(operands[1])) {
            return {static_cast<std::uint8_t>(0x80 | x),
                    static_cast<std::uint8_t>((parse_register(operands[1]) << 4) | 0x4)};
        }
        return {static_cast<std::uint8_t>(0x70 | x),
                static_cast<std::uint8_t>(parse_byte(operands[1]))};
    }

    if ((mnemonic == "OR" || mnemonic == "AND" || mnemonic == "XOR" || mnemonic == "SUB" ||
         mnemonic == "SUBN") &&
        operands.size() == 2) {
        const int x = parse_register(operands[0]);
        const int y = parse_register(operands[1]);
        int low = 0x1;
        if (mnemonic == "AND") {
            low = 0x2;
        } else if (mnemonic == "XOR") {
            low = 0x3;
        } else if (mnemonic == "SUB") {
            low = 0x5;
        } else if (mnemonic == "SUBN") {
            low = 0x7;
        }
        return {static_cast<std::uint8_t>(0x80 | x),
                static_cast<std::uint8_t>((y << 4) | low)};
    }

    if ((mnemonic == "SHR" || mnemonic == "SHL") && !operands.empty()) {
        // The y operand is accepted and ignored: the 1977 machine shifts Vx in
        // place, and a listing written for a later emulator still assembles.
        const int x = parse_register(operands[0]);
        return {static_cast<std::uint8_t>(0x80 | x),
                static_cast<std::uint8_t>(mnemonic == "SHR" ? 0x06 : 0x0E)};
    }

    if (mnemonic == "RND" && operands.size() == 2) {
        const int x = parse_register(operands[0]);
        return {static_cast<std::uint8_t>(0xC0 | x),
                static_cast<std::uint8_t>(parse_byte(operands[1]))};
    }

    if (mnemonic == "DRW" && operands.size() == 3) {
        const int x = parse_register(operands[0]);
        const int y = parse_register(operands[1]);
        const int rows = parse_byte(operands[2]);
        if (rows > 15) {
            throw fail("draws at most 15 rows");
        }
        return {static_cast<std::uint8_t>(0xD0 | x),
                static_cast<std::uint8_t>((y << 4) | rows)};
    }

    if ((mnemonic == "SKP" || mnemonic == "SKNP") && operands.size() == 1) {
        const int x = parse_register(operands[0]);
        return {static_cast<std::uint8_t>(0xE0 | x),
                static_cast<std::uint8_t>(mnemonic == "SKP" ? 0x9E : 0xA1)};
    }

    throw fail("is not an instruction form this assembler knows");
}

unsigned keypad(std::string_view source) {
    unsigned keys = 0;
    std::size_t start = 0;
    while (start <= source.size()) {
        const std::size_t end = source.find('\n', start);
        const std::string line = trim(end == std::string_view::npos
                                          ? source.substr(start)
                                          : source.substr(start, end - start));
        if (line.size() >= 7 && starts_with_ci(line, "; keys")) {
            for (std::size_t index = 6; index < line.size(); ++index) {
                const char character =
                    static_cast<char>(std::toupper(static_cast<unsigned char>(line[index])));
                if (std::isxdigit(static_cast<unsigned char>(character)) != 0) {
                    const int key = std::isdigit(static_cast<unsigned char>(character))
                                        ? character - '0'
                                        : character - 'A' + 10;
                    keys |= 1U << key;
                }
            }
        }
        if (end == std::string_view::npos) {
            break;
        }
        start = end + 1;
    }
    return keys;
}

/// Draws changed pixels onto the host.
///
/// A diff rather than a full repaint: a frame touches a handful of the 2048
/// pixels, and emitting all of them would put a quarter of a million rectangles
/// into the canvas for a ten-second program.
class Chip8Display {
public:
    explicit Chip8Display(TurtleHost& turtle) : turtle_{turtle} {
        turtle_.set_svga_mode(kChip8ScreenWidth * kChip8PixelScale,
                              kChip8ScreenHeight * kChip8PixelScale);
        // CHIP-8 is a palette machine, so the two colours are palette entries.
        turtle_.set_palette_color(kOffIndex, Rgb{16, 16, 20});
        turtle_.set_palette_color(kOnIndex, Rgb{140, 255, 140});
        off_ = turtle_.resolve_color(kOffIndex);
        on_ = turtle_.resolve_color(kOnIndex);
        previous_.assign(static_cast<std::size_t>(kChip8ScreenWidth) * kChip8ScreenHeight, 0);
    }

    void clear() {
        const double width = kChip8ScreenWidth * kChip8PixelScale;
        const double height = kChip8ScreenHeight * kChip8PixelScale;
        turtle_.draw_rect(-width / 2, height / 2, width / 2, -height / 2, off_);
        previous_.assign(static_cast<std::size_t>(kChip8ScreenWidth) * kChip8ScreenHeight, 0);
    }

    void paint(const std::vector<std::uint8_t>& framebuffer) {
        for (std::size_t index = 0; index < framebuffer.size(); ++index) {
            if (framebuffer[index] == previous_[index]) {
                continue;
            }
            previous_[index] = framebuffer[index];
            const Rgb colour = framebuffer[index] != 0 ? on_ : off_;
            rect(static_cast<int>(index) % kChip8ScreenWidth,
                 static_cast<int>(index) / kChip8ScreenWidth, colour);
        }
    }

private:
    static constexpr int kOffIndex = 1;
    static constexpr int kOnIndex = 2;

    /// One CHIP-8 pixel as a filled square. CHIP-8 counts down from the
    /// top-left; the turtle canvas counts up from the middle, which is where
    /// the sign flip on y comes from.
    void rect(int column, int row, Rgb colour) {
        const double width = kChip8ScreenWidth * kChip8PixelScale;
        const double height = kChip8ScreenHeight * kChip8PixelScale;
        const double left = column * kChip8PixelScale - width / 2;
        const double top = height / 2 - row * kChip8PixelScale;
        turtle_.draw_rect(left, top, left + kChip8PixelScale, top - kChip8PixelScale, colour);
    }

    TurtleHost& turtle_;
    Rgb off_{};
    Rgb on_{};
    std::vector<std::uint8_t> previous_;
};

}  // namespace

Chip8Program assemble_chip8(std::string_view source) {
    Chip8Program program;
    program.memory.assign(kChip8MemorySize, 0);
    std::copy(std::begin(kFont), std::end(kFont),
              program.memory.begin() + kChip8FontAddress);

    // A NUL byte is not a thing a listing contains and is in every real ROM, so
    // it is the marker that decides which of the two loaders runs. (The editor and
    // the runner preserve the bytes: see `MainWindow::openFile`.)
    if (source.find('\0') != std::string_view::npos) {
        if (source.size() > static_cast<std::size_t>(kChip8RomLimit)) {
            throw Chip8Error("the ROM is " + std::to_string(source.size()) +
                             " bytes; memory from 0x200 holds " +
                             std::to_string(kChip8RomLimit));
        }
        std::copy(source.begin(), source.end(),
                  program.memory.begin() + kChip8ProgramAddress);
        return program;
    }

    std::vector<std::string> lines;
    std::size_t start = 0;
    while (start <= source.size()) {
        const std::size_t end = source.find('\n', start);
        lines.push_back(std::string{end == std::string_view::npos
                                        ? source.substr(start)
                                        : source.substr(start, end - start)});
        if (end == std::string_view::npos) {
            break;
        }
        start = end + 1;
    }
    // A trailing newline is not a line. `splitlines()` agrees.
    if (!lines.empty() && lines.back().empty() && !source.empty() && source.back() == '\n') {
        lines.pop_back();
    }

    int address = kChip8ProgramAddress;
    for (std::size_t index = 0; index < lines.size(); ++index) {
        std::string text = code_of(lines[index]);
        if (text.empty()) {
            continue;
        }
        if (const auto label = label_prefix(text); label.has_value()) {
            if (program.labels.find(label->first) != program.labels.end()) {
                throw Chip8Error("line " + std::to_string(index + 1) + ": label " +
                                 label->first + " is defined twice");
            }
            program.labels[label->first] = address;
            text = trim(std::string_view{text}.substr(label->second));
            if (text.empty()) {
                continue;
            }
        }
        address += size_of(text);
    }

    if (address > kChip8MemorySize) {
        throw Chip8Error("the program needs " +
                         std::to_string(address - kChip8ProgramAddress) +
                         " bytes and memory ends at 0x1000");
    }

    address = kChip8ProgramAddress;
    for (std::size_t index = 0; index < lines.size(); ++index) {
        std::string text = code_of(lines[index]);
        if (text.empty()) {
            continue;
        }
        if (const auto label = label_prefix(text); label.has_value()) {
            text = trim(std::string_view{text}.substr(label->second));
            if (text.empty()) {
                continue;
            }
        }
        const std::vector<std::uint8_t> emitted =
            encode(text, program.labels, static_cast<int>(index) + 1);
        std::copy(emitted.begin(), emitted.end(), program.memory.begin() + address);
        address += static_cast<int>(emitted.size());
    }

    return program;
}

Chip8Machine::Chip8Machine(std::vector<std::uint8_t> program_memory, int entry, unsigned keys)
    : memory{std::move(program_memory)},
      pc{entry},
      keys_{keys},
      framebuffer_(static_cast<std::size_t>(kChip8ScreenWidth) * kChip8ScreenHeight, 0) {
    // Python's `random` module seeds from the OS on import, so this does too;
    // a fixed seed would make every run of an RND program identical.
    std::random_device device;
    random_.seed_unsigned((static_cast<std::uint64_t>(device()) << 32) | device());
}

std::uint16_t Chip8Machine::fetch() {
    if (pc + 1 >= kChip8MemorySize) {
        throw Chip8Error("the program counter ran past the end of memory");
    }
    const std::uint16_t opcode =
        static_cast<std::uint16_t>((memory[static_cast<std::size_t>(pc)] << 8) |
                                   memory[static_cast<std::size_t>(pc) + 1]);
    pc += 2;
    return opcode;
}

void Chip8Machine::skip() {
    pc += 2;
}

bool Chip8Machine::pixel(int x, int y) const {
    return x >= 0 && x < kChip8ScreenWidth && y >= 0 && y < kChip8ScreenHeight &&
           framebuffer_[static_cast<std::size_t>(y * kChip8ScreenWidth + x)] != 0;
}

void Chip8Machine::set_pixel(int x, int y, bool on) {
    if (x >= 0 && x < kChip8ScreenWidth && y >= 0 && y < kChip8ScreenHeight) {
        framebuffer_[static_cast<std::size_t>(y * kChip8ScreenWidth + x)] = on ? 1 : 0;
    }
}

int Chip8Machine::first_key() const {
    for (int index = 0; index < 16; ++index) {
        if ((keys_ & (1U << index)) != 0U) {
            return index;
        }
    }
    return 0;
}

void Chip8Machine::step() {
    ++cycles_;
    const int opcode = fetch();
    const int high = opcode >> 12;
    const int x = (opcode >> 8) & 0xF;
    const int y = (opcode >> 4) & 0xF;
    const int low_byte = opcode & 0xFF;
    const int low_nibble = opcode & 0xF;

    if (opcode == 0x0000) {
        halted_ = true;
        return;
    }
    if (opcode == 0x00E0) {
        framebuffer_.assign(static_cast<std::size_t>(kChip8ScreenWidth) * kChip8ScreenHeight, 0);
        return;
    }
    if (opcode == 0x00EE) {
        if (stack.empty()) {
            throw Chip8Error("RET with an empty call stack");
        }
        pc = stack.back();
        stack.pop_back();
        return;
    }
    if (high == 0x0) {
        // 0NNN (SYS): an RCA 1802 routine, which this machine cannot execute.
        // Ignoring it is what every interpreter since the original does, and it
        // keeps a ROM that contains one runnable.
        return;
    }

    switch (high) {
        case 0x1:
            pc = opcode & 0xFFF;
            break;
        case 0x2:
            stack.push_back(pc);
            pc = opcode & 0xFFF;
            break;
        case 0x3:
            if (v[static_cast<std::size_t>(x)] == low_byte) {
                skip();
            }
            break;
        case 0x4:
            if (v[static_cast<std::size_t>(x)] != low_byte) {
                skip();
            }
            break;
        case 0x5:
            if (v[static_cast<std::size_t>(x)] == v[static_cast<std::size_t>(y)]) {
                skip();
            }
            break;
        case 0x6:
            v[static_cast<std::size_t>(x)] = static_cast<std::uint8_t>(low_byte);
            break;
        case 0x7:
            v[static_cast<std::size_t>(x)] =
                static_cast<std::uint8_t>((v[static_cast<std::size_t>(x)] + low_byte) & 0xFF);
            break;
        case 0x8:
            arithmetic(x, y, low_nibble);
            break;
        case 0x9:
            if (v[static_cast<std::size_t>(x)] != v[static_cast<std::size_t>(y)]) {
                skip();
            }
            break;
        case 0xA:
            i = opcode & 0xFFF;
            break;
        case 0xB:
            pc = (opcode & 0xFFF) + v[0];
            break;
        case 0xC:
            v[static_cast<std::size_t>(x)] =
                static_cast<std::uint8_t>(random_.randint(0, 255) & low_byte);
            break;
        case 0xD:
            draw(x, y, low_nibble);
            break;
        case 0xE:
            if (low_byte == 0x9E &&
                (keys_ & (1U << v[static_cast<std::size_t>(x)])) != 0U) {
                skip();
            } else if (low_byte == 0xA1 &&
                       (keys_ & (1U << v[static_cast<std::size_t>(x)])) == 0U) {
                skip();
            }
            break;
        case 0xF:
            misc(x, low_byte);
            break;
        default:
            throw Chip8Error("unknown opcode 0x" + hex_upper(opcode, 4) + " at 0x" +
                             hex_upper(pc - 2, 3));
    }
}

void Chip8Machine::arithmetic(int x, int y, int low) {
    auto& vx = v[static_cast<std::size_t>(x)];
    const std::uint8_t vy = v[static_cast<std::size_t>(y)];
    switch (low) {
        case 0x0:
            vx = vy;
            break;
        case 0x1:
            vx = static_cast<std::uint8_t>(vx | vy);
            break;
        case 0x2:
            vx = static_cast<std::uint8_t>(vx & vy);
            break;
        case 0x3:
            vx = static_cast<std::uint8_t>(vx ^ vy);
            break;
        case 0x4: {
            const int total = vx + vy;
            vx = static_cast<std::uint8_t>(total & 0xFF);
            v[0xF] = total > 0xFF ? 1 : 0;
            break;
        }
        case 0x5:
            v[0xF] = vx >= vy ? 1 : 0;
            vx = static_cast<std::uint8_t>((vx - vy) & 0xFF);
            break;
        case 0x6:
            // Vx shifted in place, as the 1977 interpreter does.
            v[0xF] = static_cast<std::uint8_t>(vx & 0x1);
            vx = static_cast<std::uint8_t>(vx >> 1);
            break;
        case 0x7:
            v[0xF] = vy >= vx ? 1 : 0;
            vx = static_cast<std::uint8_t>((vy - vx) & 0xFF);
            break;
        case 0xE:
            v[0xF] = (vx & 0x80) != 0 ? 1 : 0;
            vx = static_cast<std::uint8_t>((vx << 1) & 0xFF);
            break;
        default:
            throw Chip8Error("unknown arithmetic opcode 8xy" + hex_upper(low, 1));
    }
}

void Chip8Machine::draw(int x, int y, int rows) {
    const int start_x = v[static_cast<std::size_t>(x)] % kChip8ScreenWidth;
    const int start_y = v[static_cast<std::size_t>(y)] % kChip8ScreenHeight;
    v[0xF] = 0;
    for (int row = 0; row < rows; ++row) {
        const std::uint8_t bits = memory[static_cast<std::size_t>((i + row) % kChip8MemorySize)];
        for (int column = 0; column < 8; ++column) {
            if ((bits & (0x80 >> column)) == 0) {
                continue;
            }
            const int pixel_x = (start_x + column) % kChip8ScreenWidth;
            const int pixel_y = (start_y + row) % kChip8ScreenHeight;
            if (pixel(pixel_x, pixel_y)) {
                v[0xF] = 1;
                ++collisions_;
            }
            set_pixel(pixel_x, pixel_y, !pixel(pixel_x, pixel_y));
        }
    }
}

void Chip8Machine::misc(int x, int low) {
    auto& vx = v[static_cast<std::size_t>(x)];
    switch (low) {
        case 0x07:
            vx = static_cast<std::uint8_t>(delay_timer);
            break;
        case 0x0A:
            // Blocking on a real key press cannot work here: the keypad is
            // presented by directive. The first held key, or 0.
            vx = static_cast<std::uint8_t>(first_key());
            break;
        case 0x15:
            delay_timer = vx;
            break;
        case 0x18:
            sound_timer = vx;
            break;
        case 0x1E:
            i = (i + vx) & 0xFFF;
            break;
        case 0x29:
            i = kChip8FontAddress + (vx & 0xF) * 5;
            break;
        case 0x33:
            memory[static_cast<std::size_t>(i % kChip8MemorySize)] =
                static_cast<std::uint8_t>(vx / 100);
            memory[static_cast<std::size_t>((i + 1) % kChip8MemorySize)] =
                static_cast<std::uint8_t>((vx / 10) % 10);
            memory[static_cast<std::size_t>((i + 2) % kChip8MemorySize)] =
                static_cast<std::uint8_t>(vx % 10);
            break;
        case 0x55:
            for (int index = 0; index <= x; ++index) {
                memory[static_cast<std::size_t>((i + index) % kChip8MemorySize)] =
                    v[static_cast<std::size_t>(index)];
            }
            // The 1977 interpreter leaves I one past the block; CHIP-48 and
            // SCHIP leave it where it was. This machine is the 1977 one, as the
            // shifts and the sprite wrap are.
            i = (i + x + 1) & 0xFFF;
            break;
        case 0x65:
            for (int index = 0; index <= x; ++index) {
                v[static_cast<std::size_t>(index)] =
                    memory[static_cast<std::size_t>((i + index) % kChip8MemorySize)];
            }
            i = (i + x + 1) & 0xFFF;
            break;
        default:
            throw Chip8Error("unknown opcode Fx" + hex_upper(low, 2));
    }
}

void Chip8Machine::tick_timers() {
    if (delay_timer > 0) {
        --delay_timer;
    }
    if (sound_timer > 0) {
        --sound_timer;
    }
}

std::string execute_chip8(Interpreter& interpreter, std::string_view source, TurtleHost& turtle) {
    (void)interpreter;
    Chip8Program program;
    try {
        program = assemble_chip8(source);
    } catch (const Chip8Error& error) {
        return "❌ CHIP-8: " + std::string{error.what()} + "\n";
    }

    Chip8Machine machine{std::move(program.memory), program.entry, keypad(source)};
    Chip8Display display{turtle};
    display.clear();

    int frames = 0;
    try {
        while (!machine.halted()) {
            if (machine.cycles() >= static_cast<std::uint64_t>(kChip8MaxSteps)) {
                return "❌ CHIP-8: exceeded the cycle limit (1000000 opcodes) without "
                       "reaching HALT\n";
            }
            for (int cycle = 0; cycle < kChip8CyclesPerFrame; ++cycle) {
                if (machine.halted()) {
                    break;
                }
                machine.step();
            }
            machine.tick_timers();
            display.paint(machine.framebuffer());
            ++frames;
            if (frames >= kChip8MaxFrames) {
                // Not an error: a loop that never halts is a legitimate CHIP-8
                // program (an animation, or a game waiting for a key).
                return "ℹ️ CHIP-8: stopped after 600 frames (" +
                       std::to_string(machine.cycles()) +
                       " opcodes); the program had not halted\n";
            }
        }
    } catch (const Chip8Error& error) {
        return "❌ CHIP-8: " + std::string{error.what()} + "\n";
    }
    return {};
}

}  // namespace timewarp::lang
