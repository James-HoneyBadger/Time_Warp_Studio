// SPDX-License-Identifier: MIT
//
// CHIP-8: a two-pass assembler and the 1977 machine, as one whole-program
// executor.
//
// The reference implementation is `time_warp/languages/chip8.py`, and the
// behaviour here is that file's: the same mnemonics, the same 1977 choices
// (Vx shifts in place, sprites wrap), the same frame model (ten opcodes to a
// frame, timers tick once per frame) and the same limits, reported rather than
// silently applied.
//
// Exposed as an assembler and a machine as well as an entry point, because an
// assembled wrong opcode and a wrongly executed right opcode are different
// bugs, and a test that only ran a program end to end would let the first hide
// inside the second.

#pragma once

#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/python_random.hpp"

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
inline constexpr int kChip8PixelScale = 8;
inline constexpr int kChip8CyclesPerFrame = 10;
inline constexpr int kChip8MaxFrames = 600;
inline constexpr int kChip8MaxSteps = 1'000'000;
inline constexpr int kChip8MemorySize = 4096;
inline constexpr int kChip8FontAddress = 0x50;
inline constexpr int kChip8ProgramAddress = 0x200;

struct Chip8Program {
    std::vector<std::uint8_t> memory;
    std::map<std::string, int> labels;
    int entry = kChip8ProgramAddress;
};

/// Assembles *source*. Throws Chip8Error on a malformed listing.
[[nodiscard]] Chip8Program assemble_chip8(std::string_view source);

/// The machine: registers, memory, timers and framebuffer.
class Chip8Machine {
public:
    Chip8Machine(std::vector<std::uint8_t> memory, int entry, unsigned keys = 0);

    void step();
    void tick_timers();

    [[nodiscard]] bool halted() const noexcept { return halted_; }
    [[nodiscard]] std::uint64_t cycles() const noexcept { return cycles_; }
    [[nodiscard]] std::uint64_t collisions() const noexcept { return collisions_; }
    [[nodiscard]] const std::vector<std::uint8_t>& framebuffer() const noexcept {
        return framebuffer_;
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
    [[nodiscard]] int first_key() const;
    void arithmetic(int x, int y, int low);
    void draw(int x, int y, int rows);
    void misc(int x, int low);

    /// One generator for the whole run, as Python's module-level `random` is:
    /// a fresh seed-0 generator per RND opcode would return the same byte
    /// every time. Seeded from the OS so two runs differ, as the reference's
    /// do.
    PythonRandom random_;
    unsigned keys_ = 0;
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
