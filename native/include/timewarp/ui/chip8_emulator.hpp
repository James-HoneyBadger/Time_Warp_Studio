// SPDX-License-Identifier: MIT
//
// A live CHIP-8 machine: wall-clock frames, key state and pause/step.
//
// The manual is `docs/chip8/README.md`; the machine it drives is documented in
// `docs/chip8/instruction-set.md`.
//
// The engine's `execute_chip8` runs a program to completion, which is what the
// parity gates and the lesson checks need. A learner pressing Run on a game needs
// the other model: a fixed number of opcodes per 60 Hz frame, timers that follow
// the clock, and a keypad that can change while the program is running. This
// class owns that loop; `Chip8Window` is only a view of it.

#pragma once

#include "timewarp/lang/chip8.hpp"

#include <QByteArray>
#include <QObject>
#include <QString>
#include <QTimer>

#include <array>
#include <cstdint>
#include <memory>
#include <vector>

namespace timewarp::ui {

inline constexpr int kChip8FramesPerSecond = 60;
/// Ten opcodes a frame is the original pace and the whole-program executor's.
inline constexpr int kChip8DefaultSpeed = 10;
/// Fast enough for a listing that computes, still bounded so the GUI thread
/// cannot be monopolised by one frame.
inline constexpr int kChip8MaximumSpeed = 2000;

/// Everything the window draws, copied in one go so the inspector cannot read a
/// half-executed instruction.
struct Chip8Snapshot {
    std::array<std::uint8_t, 16> v{};
    int i = 0;
    int pc = 0;
    std::vector<int> stack;
    int delay_timer = 0;
    int sound_timer = 0;
    std::uint64_t cycles = 0;
    bool loaded = false;
    bool running = false;
    bool halted = false;
    bool waiting_for_key = false;
    int speed = kChip8DefaultSpeed;
    /// The grid the framebuffer is on: 64x32, or 128x64 in SuperChip hi-res.
    int screen_width = timewarp::lang::kChip8ScreenWidth;
    int screen_height = timewarp::lang::kChip8ScreenHeight;
    std::vector<std::uint8_t> framebuffer;
    /// The whole 4 KiB, for the memory view. A copy per frame is 240 KiB/s,
    /// which is cheaper than keeping a second machine around to read from.
    std::vector<std::uint8_t> memory;
};

class Chip8Emulator final : public QObject {
    Q_OBJECT

public:
    explicit Chip8Emulator(QObject* parent = nullptr);
    ~Chip8Emulator() override;

    /// Reads a `.ch8` file exactly as stored: a listing is assembled, a binary
    /// ROM is copied to 0x200. The file is not decoded as text, so no byte can
    /// be normalised on the way in.
    bool loadFile(const QString& path, QString* error = nullptr);
    /// *bytes* is a listing (assembled) or a binary ROM (contains a NUL).
    bool loadBytes(const QByteArray& bytes, const QString& name, QString* error = nullptr);
    bool loadSource(const QString& source, QString* error = nullptr);

    void start();
    void pause();
    /// Executes one instruction while paused. A no-op while running or halted.
    void step();
    /// Executes one frame's opcodes and ticks the timers, without waiting for
    /// the clock. The tests use it, and it is the deterministic half of the
    /// timer's work.
    void advanceFrame();
    /// Rebuilds the machine from the loaded program. The program is kept, so
    /// Reset is the button a learner wants after a game ends.
    void reset();

    [[nodiscard]] bool loaded() const noexcept { return machine_ != nullptr; }
    [[nodiscard]] bool running() const noexcept { return running_; }
    [[nodiscard]] int speed() const noexcept { return speed_; }
    void setSpeed(int cycles_per_frame);
    [[nodiscard]] QString name() const { return name_; }
    [[nodiscard]] QString path() const { return path_; }
    [[nodiscard]] QString error() const { return error_; }
    /// The bytes the machine was loaded from, so the window's listing editor can
    /// show the program that is running rather than a second copy.
    [[nodiscard]] QByteArray source() const { return source_; }
    [[nodiscard]] bool binary() const noexcept { return binary_; }

    void setKey(int key, bool down);
    void releaseAllKeys();
    [[nodiscard]] unsigned keys() const noexcept;
    [[nodiscard]] Chip8Snapshot snapshot() const;

signals:
    /// After one frame (or a single step), so the window repaints once per tick
    /// rather than once per instruction.
    void frameReady();
    /// Run, pause, halt, wait and speed changes: the controls and the status line.
    void stateChanged();
    void errorOccurred(const QString& message);

private:
    void tick();
    void makeMachine();
    void fail(const QString& message);

    std::unique_ptr<timewarp::lang::Chip8Machine> machine_;
    /// Kept after `makeMachine`, because Reset must not need the file again.
    timewarp::lang::Chip8Program program_;
    QByteArray source_;
    bool binary_ = false;
    QString name_;
    QString path_;
    QString error_;
    QTimer clock_;
    int speed_ = kChip8DefaultSpeed;
    bool running_ = false;
};

}  // namespace timewarp::ui
