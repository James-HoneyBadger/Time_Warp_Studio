// SPDX-License-Identifier: MIT
//
// The HB-2: one object that owns a bus and the devices on it.
//
// ---------------------------------------------------------------------------
// Why one object
// ---------------------------------------------------------------------------
//
// The 1982 machine's state lives in seven files, a settings file and a header comment
// that a compiler has never read. Every capability the Model 92 adds touches three of
// those at once, so nothing is cheap until the machine is one object with a map the
// bus routes with.
//
// This is that object. It boots, it runs code, it ticks sixty times a second, and it
// can be saved and restored whole. The window that will show it owns pixels and
// nothing else; the harness that proves it (`time_warp_gates hb`) owns neither and
// runs the same object.
//
// ---------------------------------------------------------------------------
// The operating system, such as it is
// ---------------------------------------------------------------------------
//
// `INT` is the documented way a program asks the machine for something. The table is
// small on purpose -- a 1992 BIOS was too -- and each service is one line of a
// program: print a character, draw a filled rectangle, draw a line, choose the video
// mode, set a palette entry, draw a sprite, read the clock, write to the serial port,
// exit. Everything a demonstration program does is in that list.

#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "timewarp/hb2/asm.hpp"
#include "timewarp/hb2/audio.hpp"
#include "timewarp/hb2/bus.hpp"
#include "timewarp/hb2/cpu.hpp"
#include "timewarp/hb2/hbos.hpp"
#include "timewarp/hb2/peripherals.hpp"
#include "timewarp/hb2/video.hpp"

namespace timewarp::hb2 {

/// The services `INT` answers, as the manual's table lists them.
enum Service : std::uint16_t {
    kServiceExit = 0,
    kServicePrint = 1,
    kServiceFrame = 2,
    kServiceFill = 3,
    kServiceLine = 4,
    kServiceMode = 5,
    kServicePalette = 6,
    kServiceClear = 7,
    kServicePrintString = 8,
    kServiceSprite = 9,
    kServiceClearText = 10,
    kServiceClock = 11,
    kServiceSerial = 12,
    kServicePattern = 13,
    kServiceNote = 14,
    kServiceWave = 15,
    kServiceKey = 16,
    kServiceKeyStatus = 17,
    kServiceTask = 18,
};

class Machine {
public:
    /// The snapshot's format number. A version-1 image was the 1982 machine's; this
    /// one carries RAM, video, audio and the processor, and refuses what it cannot
    /// read rather than guessing.
    static constexpr int kSnapshotVersion = 2;

    Machine();

    // -- The parts ---------------------------------------------------------
    [[nodiscard]] Bus& bus() noexcept { return bus_; }
    [[nodiscard]] Video& video() noexcept { return video_; }
    [[nodiscard]] Audio& audio() noexcept { return audio_; }
    [[nodiscard]] Input& input() noexcept { return input_; }
    [[nodiscard]] Rtc& rtc() noexcept { return rtc_; }
    [[nodiscard]] Uart& uart() noexcept { return uart_; }
    [[nodiscard]] Rom& rom() noexcept { return rom_; }
    [[nodiscard]] Cpu& cpu() noexcept { return cpu_; }
    [[nodiscard]] Scheduler& scheduler() noexcept { return scheduler_; }
    [[nodiscard]] Desktop& desktop() noexcept { return desktop_; }

    /// Ctrl-Alt-Del: every device to its power-on state, the processor at `entry`.
    void reset(std::uint32_t entry = 0);

    // -- Running -----------------------------------------------------------
    void loadProgram(const std::vector<std::uint8_t>& bytes, std::uint32_t address);
    /// Assembles and loads; returns the assembly so a caller can report its errors.
    Assembly load(const std::string& source, std::uint32_t origin = 0x001000);
    /// Steps until the program halts, faults, or the budget runs out.
    std::uint32_t run(std::uint32_t budget = 1000000);
    /// Runs `frames` frames at 60 Hz, giving the processor `stepsPerFrame`
    /// instructions each and ticking every device on the frame.
    std::uint64_t runFrames(int frames, std::uint32_t stepsPerFrame = 200000);
    /// One 60 Hz frame: devices tick, the clock advances, the sound latch is drained.
    void tickFrame();

    [[nodiscard]] CpuStatus status() const;
    /// A sentence for the harness: `halted`, `running`, or `faulted: …`.
    [[nodiscard]] std::string statusText() const;
    [[nodiscard]] int exitCode() const noexcept { return exit_code_; }

    /// Everything a program has printed, as characters. The text mode has the same
    /// characters in its cells; this is the transcript a harness reads.
    [[nodiscard]] const std::string& console() const noexcept { return console_; }

    // -- HB-OS -------------------------------------------------------------
    /// Starts a listing as a process with a window, and switches the screen to a
    /// framebuffer so the desktop has somewhere to compose. Returns the task id, or
    /// zero when it will not assemble or fit.
    int spawn(const std::string& name, const std::string& source);
    /// Stops every process and clears the desktop.
    void stopAllTasks();
    /// Clears whichever screen is showing: the text cells in `MODE 0`, the bitmap
    /// otherwise. `CLS` and a fresh listing both come through here.
    void resetVideoSurface();

    // -- Snapshot ----------------------------------------------------------
    [[nodiscard]] std::uint64_t hash() const;
    [[nodiscard]] std::vector<std::uint8_t> save();
    [[nodiscard]] bool restore(const std::vector<std::uint8_t>& image);

private:
    bool service(Cpu& cpu, std::uint16_t vector);

    Bus bus_;
    Rom rom_;
    Video video_;
    Audio audio_;
    Input input_;
    Rtc rtc_;
    Uart uart_;
    Cpu cpu_;
    /// One handler for every processor: the console's and each task's. A process and
    /// the prompt ask the same machine for the same services.
    Cpu::InterruptHandler interrupt_handler_;
    Scheduler scheduler_;
    Desktop desktop_;
    std::string console_;
    int exit_code_ = 0;
};

/// The map, as the manual prints it: one line per region of `devices.inc`, in the
/// order the bus consults them.
[[nodiscard]] std::vector<std::string> mapListing();

}  // namespace timewarp::hb2
