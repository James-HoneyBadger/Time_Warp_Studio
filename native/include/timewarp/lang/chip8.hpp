// SPDX-License-Identifier: MIT
//
// CHIP-8: a two-pass assembler and the 1977 machine, as one whole-program
// executor.
//
// The manuals are `docs/chip8/assembly.md` (how to write a listing) and
// `docs/chip8/instruction-set.md` (what each opcode does, quirks included).
//
// The reference implementation is `time_warp/languages/chip8.py`, and the
// behaviour here is that file's: the same mnemonics, the same 1977 choices
// (Vx shifts in place, sprites wrap, FX55/FX65 leave I one past the block), the
// same frame model (ten opcodes to a frame, timers tick once per frame) and the
// same limits, reported rather than silently applied.
//
// A source that contains a NUL byte is a binary ROM rather than a listing: it is
// copied to 0x200 unchanged, up to the 3584 bytes that fit before the end of
// memory. The assembler and the loader share `assemble_chip8` because both produce
// the same `Chip8Program`, and a test that runs one should not care which.
//
// Exposed as an assembler and a machine as well as an entry point, because an
// assembled wrong opcode and a wrongly executed right opcode are different
// bugs, and a test that only ran a program end to end would let the first hide
// inside the second.

#pragma once

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/mersenne_random.hpp"

#include <array>
#include <cstdint>
#include <map>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace timewarp::lang {

class Chip8Error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

inline constexpr int kChip8ScreenWidth = 64;
inline constexpr int kChip8ScreenHeight = 32;
/// SuperChip hi-res: exactly twice the original in each direction, so the drawn
/// canvas keeps its 2:1 shape and a hi-res pixel is half the size.
inline constexpr int kChip8HiResWidth = 128;
inline constexpr int kChip8HiResHeight = 64;
inline constexpr int kChip8PixelScale = 8;
inline constexpr int kChip8CyclesPerFrame = 10;
inline constexpr int kChip8MaxFrames = 600;
inline constexpr int kChip8MaxSteps = 1'000'000;
inline constexpr int kChip8MemorySize = 4096;
inline constexpr int kChip8FontAddress = 0x50;
/// Ten bytes per digit: the 8x10 face FX30 points at, right after the 4x5 set.
inline constexpr int kChip8LargeFontAddress = kChip8FontAddress + 16 * 5;
inline constexpr int kChip8ProgramAddress = 0x200;
/// How many bytes a binary ROM may occupy: memory from 0x200 to 0xFFF.
inline constexpr int kChip8RomLimit = kChip8MemorySize - kChip8ProgramAddress;

struct Chip8Program {
    std::vector<std::uint8_t> memory;
    std::map<std::string, int> labels;
    /// The 1-based listing line each opcode was assembled from, by memory
    /// address. Empty for a binary ROM, which has no lines to point at.
    std::map<int, int> source_lines;
    int entry = kChip8ProgramAddress;
};

/// Assembles *source*, or loads it as a binary ROM when it contains a NUL byte.
/// Throws Chip8Error on a malformed listing or a ROM larger than memory.
[[nodiscard]] Chip8Program assemble_chip8(std::string_view source);

/// The machine: registers, memory, timers and framebuffer.
class Chip8Machine {
public:
    Chip8Machine(std::vector<std::uint8_t> memory, int entry, unsigned keys = 0);

    void step();
    void tick_timers();

    /// Makes `LD Vx, K` wait for a key instead of reading the mask once.
    /// The whole-program executor leaves this off, because it has no way to
    /// deliver a later press and the reference reads 0 there.
    void set_interactive(bool interactive) noexcept { interactive_ = interactive; }
    [[nodiscard]] bool interactive() const noexcept { return interactive_; }

    /// The keypad, as the bit set the skip instructions test. A live emulator
    /// calls this on every press and release.
    void set_keys(unsigned keys) noexcept { keys_ = keys; }
    void set_key(int key, bool down) noexcept {
        if (key < 0 || key > 15) {
            return;
        }
        const unsigned bit = 1U << static_cast<unsigned>(key);
        keys_ = down ? (keys_ | bit) : (keys_ & ~bit);
    }
    [[nodiscard]] unsigned keys() const noexcept { return keys_; }
    /// True while `LD Vx, K` is waiting for a press. `step()` completes the
    /// instruction when a key is held.
    [[nodiscard]] bool waiting_for_key() const noexcept { return waiting_for_key_; }

    [[nodiscard]] bool halted() const noexcept { return halted_; }
    [[nodiscard]] std::uint64_t cycles() const noexcept { return cycles_; }
    [[nodiscard]] std::uint64_t collisions() const noexcept { return collisions_; }
    [[nodiscard]] const std::vector<std::uint8_t>& framebuffer() const noexcept {
        return framebuffer_;
    }
    /// The current grid: 64x32 in the original mode, 128x64 in hi-res.
    [[nodiscard]] int screen_width() const noexcept {
        return hi_res_ ? kChip8HiResWidth : kChip8ScreenWidth;
    }
    [[nodiscard]] int screen_height() const noexcept {
        return hi_res_ ? kChip8HiResHeight : kChip8ScreenHeight;
    }
    [[nodiscard]] bool hi_res() const noexcept { return hi_res_; }
    /// SuperChip's RPL flags: FX75 stores registers here and FX85 loads them.
    [[nodiscard]] const std::array<std::uint8_t, 16>& rpl_flags() const noexcept {
        return rpl_;
    }
    /// Registers and memory, for tests that assert what the machine holds. The
    /// reference exposes these directly; so does this.
    std::array<std::uint8_t, 16> v{};
    std::vector<std::uint8_t> memory;
    int i = 0;
    int pc = 0;
    std::vector<int> stack;
    int delay_timer = 0;
    int sound_timer = 0;

private:
    [[nodiscard]] std::uint16_t fetch();
    void skip();
    [[nodiscard]] bool pixel(int x, int y) const;
    void set_pixel(int x, int y, bool on);
    /// The lowest held key, or -1 when none is held.
    [[nodiscard]] int first_key() const;
    void arithmetic(int x, int y, int low);
    void draw(int x, int y, int rows);
    void misc(int x, int low);
    void set_mode(bool hi_res);
    /// Positive rows scroll the display down; the top is cleared.
    void scroll_down(int rows);
    /// Positive columns scroll left, negative right; the gap is cleared.
    void scroll_columns(int columns);

    /// One generator for the whole run, as Python's module-level `random` is:
    /// a fresh seed-0 generator per RND opcode would return the same byte
    /// every time. Seeded from the OS so two runs differ, as the reference's
    /// do.
    MersenneRandom random_;
    unsigned keys_ = 0;
    bool interactive_ = false;
    bool waiting_for_key_ = false;
    int waiting_register_ = 0;
    bool hi_res_ = false;
    std::array<std::uint8_t, 16> rpl_{};
    std::vector<std::uint8_t> framebuffer_;
    bool halted_ = false;
    std::uint64_t cycles_ = 0;
    std::uint64_t collisions_ = 0;
};

/// Assembles and runs a listing, drawing the framebuffer on *turtle*.
///
/// Returns the text the console should show: empty on success, `❌` for an
/// error, `ℹ️` for the frame limit (a program that never halts is legitimate).
[[nodiscard]] std::string execute_chip8(Interpreter& interpreter, std::string_view source,
                                        TurtleHost& turtle);

}  // namespace timewarp::lang
