// SPDX-License-Identifier: MIT
//
// HB-16, the HB-2's processor: sixteen-bit data, a twenty-four-bit address space,
// eight general registers and a table of cycle counts.
//
// ---------------------------------------------------------------------------
// Why a real instruction set
// ---------------------------------------------------------------------------
//
// The 1982 machine has no processor. Its languages are the native engines behind a
// very good costume, and its "machine code" was sixteen invented opcodes that
// `POKE` wrote into a map nothing executed. That is a costume with a keyboard.
//
// A machine in 1992 had an instruction set, a monitor in ROM and an assembler within
// reach -- that is what an owner learned a computer on, and it is what makes the
// module bay a socket a user can fill rather than a factory decision. So the HB-2
// executes its own code, and the same listing can be assembled, disassembled,
// stepped and watched in registers.
//
// The processor is a value type: it owns registers and nothing else. Memory and the
// I/O page are reached through the bus, so a `STORE` to the video register block is
// the same store a `POKE` would make, and a test can run an instruction without a
// window existing.
//
// The one thing it asks of the machine around it is `INT`: the documented instruction
// for calling the operating system. The machine installs a handler, and everything
// else -- drawing, printing, the clock, exiting -- is a service number rather than
// another instruction.

#pragma once

#include <array>
#include <cstdint>
#include <functional>
#include <string>
#include <vector>

#include "timewarp/hb2/asm.hpp"
#include "timewarp/hb2/bus.hpp"

namespace timewarp::hb2 {

/// Where a run ends up. `Running` means it stopped because the step budget ran out,
/// which is what a scheduler wants to hear, not an error.
enum class CpuStatus { Running, Halted, Faulted };

struct CpuRegisters {
    std::array<std::uint16_t, 8> r{};
    std::uint32_t pc = 0;
    std::uint16_t sp = 0;
    std::uint16_t flags = 0;
    std::uint64_t cycles = 0;
    std::uint64_t instructions = 0;
};

class Cpu {
public:
    /// The stack's top. The stack grows down from here, at the top of the first
    /// 32 KiB of RAM, and a program that overruns it faults rather than writing
    /// through whatever is below.
    static constexpr std::uint16_t kStackTop = 0x7F00;
    /// The lowest address the stack may reach before the processor faults.
    static constexpr std::uint16_t kStackFloor = 0x7E00;

    /// `INT` handler: the machine's operating system. Returning false stops the
    /// program, which is how "exit" is spelled.
    using InterruptHandler = std::function<bool(Cpu&, std::uint16_t)>;

    explicit Cpu(Bus& bus);

    void reset(std::uint32_t entry);
    void setInterruptHandler(InterruptHandler handler);

    /// Executes one instruction. A fault stops the processor and says why; a halt is
    /// the program saying it is done.
    CpuStatus step();
    /// Steps until the budget runs out, the program halts, or it faults. Returns how
    /// many instructions ran.
    std::uint32_t run(std::uint32_t budget = 1000000);

    [[nodiscard]] const CpuRegisters& registers() const noexcept { return registers_; }
    /// Writability for the machine's services: an `INT 1` writes the character it
    /// was handed and must not be able to rewrite the program counter doing it.
    [[nodiscard]] CpuRegisters& registers() noexcept { return registers_; }
    [[nodiscard]] CpuStatus status() const noexcept { return status_; }
    [[nodiscard]] const std::string& fault() const noexcept { return fault_; }
    void halt() noexcept { status_ = CpuStatus::Halted; }
    /// Stops the processor with a sentence, which a scheduler reports and does not
    /// treat as the machine falling over.
    void fail(std::string message);

    /// The flags, as booleans, for a monitor that prints "Z C" rather than a number.
    [[nodiscard]] bool flag(std::uint16_t flag) const noexcept {
        return (registers_.flags & flag) != 0;
    }

private:
    std::uint16_t sourceValue(const Instruction& instruction);
    std::uint32_t effectiveAddress(const Instruction& instruction, bool pointer_in_a = false);
    void setFlags(std::uint32_t result, bool carry, bool overflow);
    void push(std::uint16_t value);
    std::uint16_t pop();
    void jumpTo(std::uint32_t address);
    bool condition(Op op) const;

    Bus& bus_;
    CpuRegisters registers_{};
    CpuStatus status_ = CpuStatus::Running;
    std::string fault_;
    InterruptHandler interrupt_;
};

}  // namespace timewarp::hb2
