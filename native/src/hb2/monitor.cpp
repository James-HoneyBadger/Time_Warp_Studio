// SPDX-License-Identifier: MIT

#include "timewarp/hb2/monitor.hpp"

#include <algorithm>
#include <cstdio>
#include <string_view>

namespace timewarp::hb2 {

namespace {

std::string hex(std::uint32_t value, int digits) {
    static constexpr char kHex[] = "0123456789ABCDEF";
    std::string text(static_cast<std::size_t>(digits), '0');
    for (int index = digits - 1; index >= 0; --index) {
        text[static_cast<std::size_t>(index)] = kHex[value & 0xF];
        value >>= 4;
    }
    return text;
}

}  // namespace

Monitor::Monitor(Machine& machine) : machine_(machine) {}

Cpu& Monitor::processor() { return machine_.cpu(); }

std::string Monitor::registers() const {
    const CpuRegisters& r = machine_.cpu().registers();
    std::string row = "PC=" + hex(r.pc, 6) + " SP=" + hex(r.sp, 4) + " ";
    for (std::size_t index = 0; index < r.r.size(); ++index) {
        row += "R" + std::to_string(index) + "=" + hex(r.r[index], 4) + " ";
    }
    row += "flags=";
    row += machine_.cpu().flag(kZero) ? "Z" : "-";
    row += machine_.cpu().flag(kCarry) ? "C" : "-";
    row += machine_.cpu().flag(kNegative) ? "N" : "-";
    row += machine_.cpu().flag(kOverflow) ? "V" : "-";
    if (machine_.cpu().status() == CpuStatus::Faulted) {
        row += "  " + machine_.cpu().fault();
    }
    return row;
}

std::vector<std::string> Monitor::disassemble(std::uint32_t address, int lines) const {
    std::vector<std::string> rows;
    std::uint32_t at = address;
    for (int line = 0; line < lines; ++line) {
        std::uint32_t next = at;
        const Instruction instruction = readInstruction(machine_.bus(), at, &next);
        std::string row = "$" + hex(at, 6) + "  ";
        row.push_back(breakpointAt(at) ? '>' : ' ');
        const std::string text = formatInstruction(instruction, at);
        row += text;
        while (row.size() < 40) {
            row.push_back(' ');
        }
        row += std::to_string(instructionBytes(instruction.mode)) + " bytes";
        rows.push_back(row);
        if (next <= at) {
            break;
        }
        at = next;
    }
    return rows;
}

std::vector<std::string> Monitor::dump(std::uint32_t address, int rows) const {
    std::vector<std::string> lines;
    for (int row = 0; row < rows; ++row) {
        const std::uint32_t base = address + static_cast<std::uint32_t>(row * 16);
        std::string line = "$" + hex(base, 6) + "  ";
        std::string ascii;
        for (int column = 0; column < 16; ++column) {
            const std::uint8_t byte = machine_.bus().read(base + static_cast<std::uint32_t>(column));
            line += hex(byte, 2) + " ";
            ascii.push_back(byte >= 32 && byte < 127 ? static_cast<char>(byte) : '.');
        }
        line += " " + ascii;
        lines.push_back(line);
    }
    return lines;
}

bool Monitor::step(std::string* what) {
    Cpu& cpu = processor();
    if (what != nullptr) {
        *what = formatInstruction(readInstruction(machine_.bus(), cpu.registers().pc, nullptr),
                                  cpu.registers().pc);
    }
    cpu.step();
    return cpu.status() == CpuStatus::Running;
}

std::string Monitor::runToBreakpoint(std::uint32_t budget) {
    Cpu& cpu = processor();
    for (std::uint32_t step = 0; step < budget; ++step) {
        if (breakpointAt(cpu.registers().pc)) {
            return "breakpoint at $" + hex(cpu.registers().pc, 6);
        }
        cpu.step();
        switch (cpu.status()) {
        case CpuStatus::Running:
            break;
        case CpuStatus::Halted:
            return "halted";
        case CpuStatus::Faulted:
            return "faulted: " + cpu.fault();
        }
    }
    return "still running";
}

bool Monitor::setBreakpoint(std::uint32_t address, bool on) {
    if (on) {
        breakpoints_.insert(address);
    } else {
        breakpoints_.erase(address);
    }
    return true;
}

bool Monitor::breakpointAt(std::uint32_t address) const {
    return breakpoints_.find(address) != breakpoints_.end();
}

std::vector<std::uint32_t> Monitor::breakpoints() const {
    return std::vector<std::uint32_t>(breakpoints_.begin(), breakpoints_.end());
}

bool Monitor::poke(std::uint32_t address, const std::vector<std::uint8_t>& bytes) {
    for (std::size_t index = 0; index < bytes.size(); ++index) {
        machine_.bus().write(address + static_cast<std::uint32_t>(index), bytes[index]);
    }
    return true;
}

std::vector<std::uint8_t> Monitor::peek(std::uint32_t address, int count) const {
    std::vector<std::uint8_t> bytes;
    for (int index = 0; index < count; ++index) {
        bytes.push_back(machine_.bus().read(address + static_cast<std::uint32_t>(index)));
    }
    return bytes;
}

}  // namespace timewarp::hb2
