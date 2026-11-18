// SPDX-License-Identifier: MIT
//
// The monitor: what a 1992 owner typed to look inside the machine.
//
// A machine in 1992 had an instruction set, a monitor in ROM and an assembler within
// reach, and that is what an owner learned a computer on. This is the monitor: it
// disassembles, edits memory, sets a breakpoint and steps, and it answers in text so
// the same code serves a prompt line, a panel and a shell.
//
// It is a tool *over* the machine rather than a part of it: it reads and writes the
// bus, and it steps whichever processor is in front of it -- the console's, or a
// process's -- without either of them knowing it exists.

#pragma once

#include <cstdint>
#include <set>
#include <string>
#include <vector>

#include "timewarp/hb2/machine.hpp"

namespace timewarp::hb2 {

class Monitor {
public:
    explicit Monitor(Machine& machine);

    /// The register file as a line, which is what a prompt prints and a panel shows.
    [[nodiscard]] std::string registers() const;
    /// `lines` instructions from `address`, with their addresses.
    [[nodiscard]] std::vector<std::string> disassemble(std::uint32_t address, int lines) const;
    /// A hex dump, sixteen bytes a row with the ASCII beside it.
    [[nodiscard]] std::vector<std::string> dump(std::uint32_t address, int rows) const;

    /// One instruction of the console's processor. `what` receives the disassembly of
    /// the instruction that ran.
    bool step(std::string* what = nullptr);
    /// Runs until a breakpoint or the program stops. Returns a sentence: `breakpoint
    /// at $001010`, `halted`, `faulted: …`, or `still running` when the budget ran out.
    [[nodiscard]] std::string runToBreakpoint(std::uint32_t budget = 1000000);

    bool setBreakpoint(std::uint32_t address, bool on = true);
    [[nodiscard]] bool breakpointAt(std::uint32_t address) const;
    [[nodiscard]] std::vector<std::uint32_t> breakpoints() const;

    bool poke(std::uint32_t address, const std::vector<std::uint8_t>& bytes);
    [[nodiscard]] std::vector<std::uint8_t> peek(std::uint32_t address, int count) const;

private:
    [[nodiscard]] Cpu& processor();

    Machine& machine_;
    std::set<std::uint32_t> breakpoints_;
};

}  // namespace timewarp::hb2
