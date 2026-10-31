// SPDX-License-Identifier: MIT

#include "timewarp/hb2/rom.hpp"

#include <algorithm>
#include <sstream>

#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/executor.hpp"
#include "timewarp/lang/interpreter.hpp"

namespace timewarp::hb2 {

namespace {

/// The languages the socket holds: the nine engines that are an interpreter over a
/// bus. Scheme is its own machine and CHIP-8 is a different computer with its own
/// window, so neither is a ROM here.
std::vector<lang::Language> executor_languages() {
    return {lang::Language::Basic, lang::Language::Apl,       lang::Language::Logo,
            lang::Language::Pilot, lang::Language::Focal,     lang::Language::Comal,
            lang::Language::Rexx,  lang::Language::Smalltalk, lang::Language::Forth};
}

/// The machine's own error text, without the emoji a language uses: a refusal on this
/// socket is the machine talking.
std::string withoutPrefix(const std::string& line) {
    std::string text = line;
    const std::size_t space = text.find(' ');
    if (space != std::string::npos && space <= 4) {
        text = text.substr(space + 1);
    }
    return text;
}

}  // namespace

NativeRom::NativeRom(Machine& machine, lang::Language language)
    : machine_(machine), language_(language) {}

std::vector<lang::Language> NativeRom::languages() { return executor_languages(); }

std::string NativeRom::sys(std::uint32_t address, std::uint32_t budget) {
    // Machine code runs on the console's processor, which is the one a monitor steps.
    // The run's console is the difference between what it printed and what was there
    // before it, so a program that SYSes twice sees each run's own output.
    const std::string before = machine_.console();
    machine_.cpu().reset(address);
    machine_.run(budget);
    const std::string after = machine_.console();
    std::string printed = after.size() > before.size() ? after.substr(before.size()) : std::string{};
    if (machine_.cpu().status() == CpuStatus::Faulted) {
        printed += "\n" + machine_.statusText();
    }
    return printed;
}

std::string NativeRom::run(const std::string& source, std::uint32_t instruction_budget) {
    error_.clear();
    refusal_.clear();
    if (std::find(executor_languages().begin(), executor_languages().end(), language_) ==
        executor_languages().end()) {
        refusal_ = "the ROM socket has no executor for that language";
        return {};
    }

    timewarp::graphics::RecordingHost host;
    lang::Interpreter interpreter{language_, host};
    interpreter.set_machine_bus(lang::Interpreter::MachineBus{
        .peek = [this](int address) {
            if (address < 0 || static_cast<std::uint32_t>(address) >= addressSpaceSize()) {
                return std::uint8_t{0};
            }
            return machine_.bus().read(static_cast<std::uint32_t>(address));
        },
        .poke = [this](int address, std::uint8_t value) {
            if (address < 0 || static_cast<std::uint32_t>(address) >= addressSpaceSize()) {
                return false;
            }
            const std::uint32_t before = machine_.bus().refusedWrites();
            machine_.bus().write(static_cast<std::uint32_t>(address), value);
            return machine_.bus().refusedWrites() == before;
        },
        .set_cursor = [this](int row, int column) {
            machine_.video().write(Video::kRegisterOffset + Video::kCursorRow,
                                   static_cast<std::uint8_t>(std::clamp(row, 0, 24)));
            machine_.video().write(Video::kRegisterOffset + Video::kCursorColumn,
                                   static_cast<std::uint8_t>(std::clamp(column, 0, 79)));
        },
        .set_ink = [this](int index) {
            machine_.video().write(Video::kRegisterOffset + Video::kInk,
                                   static_cast<std::uint8_t>(index & 0xFF));
        },
        .set_paper = [this](int index) {
            machine_.video().write(Video::kRegisterOffset + Video::kPaper,
                                   static_cast<std::uint8_t>(index & 0xFF));
        },
        .clear_screen = [this] { machine_.resetVideoSurface(); },
        .write_text = [this](const std::string& line) {
            for (const char character : line) {
                machine_.video().putChar(static_cast<std::uint8_t>(character));
            }
            machine_.video().putChar('\n');
        },
        .sys = [this, instruction_budget](int address) {
            // A BASIC `SYS` that lands outside the machine's RAM is a program's
            // mistake, and it says so rather than running whatever is there.
            if (address < 0 || static_cast<std::uint32_t>(address) >= Bus::kRamSize) {
                return std::string{"?SYS outside RAM"};
            }
            const std::string printed = sys(static_cast<std::uint32_t>(address), instruction_budget);
            return printed;
        },
        // The machine's joystick is the one `STICK` reads when the program is on the
        // machine; the axes are 0..255 centred at 128 and the two buttons are
        // full-scale, which is the shape `STICK(n)` has always had.
        .stick = [this](int index) {
            const auto axis = [this](std::uint32_t offset) {
                return static_cast<int>(static_cast<std::int8_t>(
                           machine_.input().read(Input::kJoystickBase + offset))) +
                       128;
            };
            const std::uint8_t buttons = machine_.input().read(Input::kJoystickBase + 2);
            switch (index) {
            case 0:
                return axis(0);
            case 1:
                return axis(1);
            case 2:
                return (buttons & 0x01U) != 0 ? 255 : 0;
            case 3:
                return (buttons & 0x02U) != 0 ? 255 : 0;
            default:
                return 128;
            }
        },
        .strig = [this](int index) {
            const std::uint8_t buttons = machine_.input().read(Input::kJoystickBase + 2);
            if (index == 0) {
                return (buttons & 0x01U) != 0;
            }
            return index == 1 && (buttons & 0x02U) != 0;
        },
        // The machine's map is bigger than a sixteen-bit address: `POKE 1048576, 5` is
        // the first pixel of the framebuffer, not an error.
        .address_limit = static_cast<int>(addressSpaceSize()),
    });

    std::string printed;
    interpreter.set_output_callback([&printed](const std::string& line) {
        printed += line;
        printed.push_back('\n');
    });
    // A program that asks for input on a machine with no keyboard is answered with an
    // empty line, which is what the reference did and what a learner expects: the
    // prompt comes back rather than the run hanging.
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });

    interpreter.load_program(source);
    interpreter.run();
    error_ = withoutPrefix(interpreter.error());
    if (!error_.empty()) {
        printed += "\n\u274C " + error_ + "\n";
    }
    return printed;
}

}  // namespace timewarp::hb2
