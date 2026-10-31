// SPDX-License-Identifier: MIT

#include "timewarp/hb2/cpu.hpp"

#include <algorithm>
#include <utility>

namespace timewarp::hb2 {

namespace {

std::string hexAddress(std::uint32_t value) {
    static constexpr char kHex[] = "0123456789ABCDEF";
    std::string text(6, '0');
    for (int index = 5; index >= 0; --index) {
        text[static_cast<std::size_t>(index)] = kHex[value & 0xF];
        value >>= 4;
    }
    return "$" + text;
}

}  // namespace

Cpu::Cpu(Bus& bus) : bus_(bus) { reset(0); }

void Cpu::reset(std::uint32_t entry) {
    registers_ = CpuRegisters{};
    registers_.pc = entry;
    registers_.sp = kStackTop;
    registers_.flags = kInterruptsEnabled;
    status_ = CpuStatus::Running;
    fault_.clear();
}

void Cpu::setInterruptHandler(InterruptHandler handler) { interrupt_ = std::move(handler); }

void Cpu::fail(std::string message) {
    status_ = CpuStatus::Faulted;
    fault_ = std::move(message);
}

// ---------------------------------------------------------------------------
// Addressing
// ---------------------------------------------------------------------------

std::uint32_t Cpu::effectiveAddress(const Instruction& instruction, bool pointer_in_a) {
    const std::uint8_t pointer = pointer_in_a ? instruction.a : instruction.b;
    switch (instruction.mode) {
    case Mode::RegInd:
        return registers_.r[pointer];
    case Mode::Indexed:
        return (static_cast<std::uint32_t>(registers_.r[pointer]) + instruction.operand) & 0xFFFFFF;
    case Mode::Abs:
        return instruction.operand & 0xFFFFFF;
    case Mode::Reg:
    case Mode::Imm:
    case Mode::Rel:
        break;
    }
    return instruction.operand & 0xFFFFFF;
}

std::uint16_t Cpu::sourceValue(const Instruction& instruction) {
    switch (instruction.mode) {
    case Mode::Reg:
        return registers_.r[instruction.b];
    case Mode::Imm:
        return static_cast<std::uint16_t>(instruction.operand & 0xFFFF);
    case Mode::Abs:
    case Mode::RegInd:
    case Mode::Indexed:
        return bus_.read16(effectiveAddress(instruction, false));
    case Mode::Rel:
        break;
    }
    return 0;
}

// ---------------------------------------------------------------------------
// Flags, stack and branches
// ---------------------------------------------------------------------------

void Cpu::setFlags(std::uint32_t result, bool carry, bool overflow) {
    std::uint16_t flags = static_cast<std::uint16_t>(registers_.flags & kInterruptsEnabled);
    const std::uint16_t value = static_cast<std::uint16_t>(result & 0xFFFF);
    if (value == 0) {
        flags |= kZero;
    }
    if ((value & 0x8000) != 0) {
        flags |= kNegative;
    }
    if (carry) {
        flags |= kCarry;
    }
    if (overflow) {
        flags |= kOverflow;
    }
    registers_.flags = flags;
}

void Cpu::push(std::uint16_t value) {
    if (registers_.sp < kStackFloor) {
        fail("stack overflow at " + hexAddress(registers_.pc));
        return;
    }
    registers_.sp = static_cast<std::uint16_t>(registers_.sp - 2);
    bus_.write16(registers_.sp, value);
}

std::uint16_t Cpu::pop() {
    if (registers_.sp > kStackTop) {
        fail("stack underflow at " + hexAddress(registers_.pc));
        return 0;
    }
    const std::uint16_t value = bus_.read16(registers_.sp);
    registers_.sp = static_cast<std::uint16_t>(registers_.sp + 2);
    return value;
}

bool Cpu::condition(Op op) const {
    const bool zero = flag(kZero);
    const bool carry = flag(kCarry);
    const bool negative = flag(kNegative);
    const bool overflow = flag(kOverflow);
    switch (op) {
    case Op::Jz:
        return zero;
    case Op::Jnz:
        return !zero;
    case Op::Jc:
        return carry;
    case Op::Jnc:
        return !carry;
    case Op::Jn:
        return negative;
    case Op::Jns:
        return !negative;
    case Op::Jlt:
        // Signed comparison: the sign of the difference is only the answer when the
        // subtraction did not itself overflow.
        return negative != overflow;
    case Op::Jgt:
        return !zero && negative == overflow;
    case Op::Jle:
        return zero || negative != overflow;
    case Op::Jge:
        return negative == overflow;
    default:
        return false;
    }
}

// ---------------------------------------------------------------------------
// One instruction
// ---------------------------------------------------------------------------

CpuStatus Cpu::step() {
    if (status_ != CpuStatus::Running) {
        return status_;
    }

    std::uint32_t next = 0;
    const Instruction instruction = readInstruction(bus_, registers_.pc, &next);
    if (!instruction.valid) {
        fail("invalid instruction at " + hexAddress(registers_.pc));
        return status_;
    }
    registers_.pc = next;
    ++registers_.instructions;
    registers_.cycles += cyclesFor(instruction.op, instruction.mode);

    const std::uint8_t a = instruction.a;
    const std::uint8_t b = instruction.b;
    const std::uint16_t source = sourceValue(instruction);

    switch (instruction.op) {
    case Op::Nop:
        break;
    case Op::Halt:
        status_ = CpuStatus::Halted;
        break;
    case Op::Mov:
        registers_.r[a] = source;
        break;
    case Op::Add: {
        const std::uint32_t result = static_cast<std::uint32_t>(registers_.r[a]) + source;
        const bool overflow = ((registers_.r[a] ^ result) & (source ^ result) & 0x8000U) != 0;
        setFlags(result, result > 0xFFFFU, overflow);
        registers_.r[a] = static_cast<std::uint16_t>(result & 0xFFFF);
        break;
    }
    case Op::Sub:
    case Op::Cmp: {
        const std::uint32_t result = static_cast<std::uint32_t>(registers_.r[a]) - source;
        const bool overflow = ((registers_.r[a] ^ source) & (registers_.r[a] ^ result) & 0x8000U) != 0;
        setFlags(result, registers_.r[a] < source, overflow);
        if (instruction.op == Op::Sub) {
            registers_.r[a] = static_cast<std::uint16_t>(result & 0xFFFF);
        }
        break;
    }
    case Op::And:
        registers_.r[a] = static_cast<std::uint16_t>(registers_.r[a] & source);
        setFlags(registers_.r[a], false, false);
        break;
    case Op::Or:
        registers_.r[a] = static_cast<std::uint16_t>(registers_.r[a] | source);
        setFlags(registers_.r[a], false, false);
        break;
    case Op::Xor:
        registers_.r[a] = static_cast<std::uint16_t>(registers_.r[a] ^ source);
        setFlags(registers_.r[a], false, false);
        break;
    case Op::Not:
        registers_.r[b] = static_cast<std::uint16_t>(~registers_.r[b]);
        setFlags(registers_.r[b], false, false);
        break;
    case Op::Test:
        setFlags(static_cast<std::uint16_t>(registers_.r[a] & source), false, false);
        break;
    case Op::Shl:
    case Op::Shr:
    case Op::Sar: {
        const unsigned amount = source & 0x0F;
        bool carry = false;
        if (amount > 0) {
            if (instruction.op == Op::Shl) {
                carry = ((registers_.r[a] >> (16 - amount)) & 0x1) != 0;
                registers_.r[a] = static_cast<std::uint16_t>(registers_.r[a] << amount);
            } else if (instruction.op == Op::Shr) {
                carry = ((registers_.r[a] >> (amount - 1)) & 0x1) != 0;
                registers_.r[a] = static_cast<std::uint16_t>(registers_.r[a] >> amount);
            } else {
                carry = ((registers_.r[a] >> (amount - 1)) & 0x1) != 0;
                const auto signed_value = static_cast<std::int16_t>(registers_.r[a]);
                registers_.r[a] = static_cast<std::uint16_t>(signed_value >> amount);
            }
        }
        setFlags(registers_.r[a], carry, false);
        break;
    }
    case Op::Mul: {
        const std::uint32_t result = static_cast<std::uint32_t>(registers_.r[a]) * source;
        setFlags(result, (result >> 16) != 0, false);
        registers_.r[a] = static_cast<std::uint16_t>(result & 0xFFFF);
        break;
    }
    case Op::Div:
    case Op::Mod:
        if (source == 0) {
            fail("division by zero at " + hexAddress(registers_.pc));
            break;
        }
        if (instruction.op == Op::Div) {
            registers_.r[a] = static_cast<std::uint16_t>(registers_.r[a] / source);
        } else {
            registers_.r[a] = static_cast<std::uint16_t>(registers_.r[a] % source);
        }
        setFlags(registers_.r[a], false, false);
        break;
    case Op::Inc:
        registers_.r[b] = static_cast<std::uint16_t>(registers_.r[b] + 1);
        setFlags(registers_.r[b], registers_.r[b] == 0, false);
        break;
    case Op::Dec:
        registers_.r[b] = static_cast<std::uint16_t>(registers_.r[b] - 1);
        setFlags(registers_.r[b], registers_.r[b] == 0xFFFF, false);
        break;
    case Op::Neg: {
        const std::uint32_t result = 0U - registers_.r[b];
        setFlags(result, registers_.r[b] != 0, registers_.r[b] == 0x8000);
        registers_.r[b] = static_cast<std::uint16_t>(result & 0xFFFF);
        break;
    }
    case Op::Swap:
        std::swap(registers_.r[a], registers_.r[b]);
        break;
    case Op::Load:
        registers_.r[a] = bus_.read16(effectiveAddress(instruction, false));
        break;
    case Op::LoadB:
        registers_.r[a] = bus_.read(effectiveAddress(instruction, false));
        break;
    case Op::Store:
        bus_.write16(effectiveAddress(instruction, true), registers_.r[b]);
        break;
    case Op::StoreB:
        bus_.write(effectiveAddress(instruction, true), static_cast<std::uint8_t>(registers_.r[b] & 0xFF));
        break;
    case Op::Lea:
        // A register is sixteen bits and an address is twenty-four, so `LEA` gives
        // the low half: a program's own data is in the first 64 KiB, and a program
        // that needs the I/O page writes the full address into the instruction.
        registers_.r[a] = static_cast<std::uint16_t>(instruction.operand & 0xFFFF);
        break;
    case Op::Push:
        push(registers_.r[b]);
        break;
    case Op::Pop:
        registers_.r[b] = pop();
        break;
    case Op::Call:
        push(static_cast<std::uint16_t>(registers_.pc & 0xFFFF));
        jumpTo(instruction.operand);
        break;
    case Op::Ret:
        jumpTo(pop());
        break;
    case Op::Jmp:
        jumpTo(instruction.operand);
        break;
    case Op::Jz:
    case Op::Jnz:
    case Op::Jc:
    case Op::Jnc:
    case Op::Jn:
    case Op::Jns:
    case Op::Jlt:
    case Op::Jgt:
    case Op::Jle:
    case Op::Jge:
        if (condition(instruction.op)) {
            // A taken branch is the one instruction whose cost depends on the
            // program rather than on the table, so it is counted here.
            registers_.cycles += 2;
            jumpTo(instruction.operand);
        }
        break;
    case Op::Int:
        if (!interrupt_) {
            fail("no supervisor for INT " + std::to_string(instruction.operand));
            break;
        }
        if (!interrupt_(*this, static_cast<std::uint16_t>(instruction.operand))) {
            status_ = CpuStatus::Halted;
        }
        break;
    case Op::In:
        registers_.r[a] = bus_.read(instruction.operand & 0xFFFFFF);
        break;
    case Op::Out:
        bus_.write(instruction.operand & 0xFFFFFF, static_cast<std::uint8_t>(registers_.r[b] & 0xFF));
        break;
    case Op::Count:
        fail("invalid instruction at " + hexAddress(registers_.pc));
        break;
    }
    return status_;
}

void Cpu::jumpTo(std::uint32_t address) { registers_.pc = address & 0xFFFFFF; }

std::uint32_t Cpu::run(std::uint32_t budget) {
    std::uint32_t ran = 0;
    while (status_ == CpuStatus::Running && ran < budget) {
        step();
        ++ran;
    }
    return ran;
}

}  // namespace timewarp::hb2
