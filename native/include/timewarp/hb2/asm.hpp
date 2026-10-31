// SPDX-License-Identifier: MIT
//
// HB-16: the encoding, shared by the assembler, the disassembler and the processor.
//
// The machine's instruction set is not an implementation detail -- it is what an
// owner of a 1992 computer learned the machine on, and what the manual's instruction
// page documents. So the encoding lives in one place and three things read it: the
// assembler that turns a listing into bytes, the processor that executes them, and
// the disassembler that turns them back into text for the monitor.
//
// ---------------------------------------------------------------------------
// Shape
// ---------------------------------------------------------------------------
//
// 16-bit data, 24-bit address, eight general registers plus a stack pointer, a
// program counter and four flags. Little-endian. An instruction is one to three
// sixteen-bit words:
//
//     word 0   [ opcode:6 ][ A:3 ][ B:3 ][ mode:4 ]
//     word 1,2 operand, in the form the mode asks for
//
// The modes are what makes a small instruction word enough:
//
//     Reg       Rd, Rs        no operand words
//     Imm       Rd, #value    one word, 16-bit
//     Abs       Rd, [addr]    two words, 24-bit
//     RegInd    Rd, [Rs]      no operand words
//     Indexed   Rd, [Rs+disp] one word, signed 8-bit displacement
//     Rel       label         one word, signed 16-bit from the next instruction
//
// A jump's operand is an **absolute target**, not a displacement that a person has
// to compute: the assembler subtracts the next instruction's address, and the
// disassembler adds it back. That is what makes assemble-disassemble-assemble
// return the same bytes, which is asserted rather than hoped for.

#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "timewarp/hb2/bus.hpp"

namespace timewarp::hb2 {

/// The opcodes. `Count` is the table's size, not an instruction.
enum class Op : std::uint8_t {
    Nop = 0,
    Halt,
    Mov,
    Add,
    Sub,
    And,
    Or,
    Xor,
    Not,
    Cmp,
    Shl,
    Shr,
    Sar,
    Mul,
    Div,
    Mod,
    Inc,
    Dec,
    Neg,
    Test,
    Swap,
    Load,
    LoadB,
    Store,
    StoreB,
    Lea,
    Push,
    Pop,
    Call,
    Ret,
    Jmp,
    Jz,
    Jnz,
    Jc,
    Jnc,
    Jn,
    Jns,
    Jlt,
    Jgt,
    Jle,
    Jge,
    Int,
    In,
    Out,
    Count
};

enum class Mode : std::uint8_t { Reg = 0, Imm = 1, Abs = 2, RegInd = 3, Indexed = 4, Rel = 5 };

/// The flags, as bits of the flag register.
enum Flag : std::uint16_t {
    kZero = 0x0001,
    kCarry = 0x0002,
    kNegative = 0x0004,
    kOverflow = 0x0008,
    kInterruptsEnabled = 0x0010,
};

struct Instruction {
    Op op = Op::Nop;
    /// Destination register, or the pointer register of an indirect or indexed
    /// access, or the pointer register of a store.
    std::uint8_t a = 0;
    /// The source register in `Reg` mode.
    std::uint8_t b = 0;
    Mode mode = Mode::Reg;
    /// Immediate, address, displacement or absolute target, depending on the mode.
    std::uint32_t operand = 0;
    bool valid = true;
};

/// The number of sixteen-bit words an instruction in `mode` takes.
[[nodiscard]] int instructionWords(Mode mode);
/// How many bytes, which is what the assembler's address arithmetic needs.
[[nodiscard]] int instructionBytes(Mode mode);
/// The cycles a decoded instruction costs: the table's entry plus what the mode
/// costs. Data, not code, so the manual and the machine cannot disagree.
[[nodiscard]] std::uint32_t cyclesFor(Op op, Mode mode);
[[nodiscard]] std::string_view mnemonic(Op op);
/// `MOV`, `JZ`, `INT` -- the name a listing and the monitor both use.
[[nodiscard]] bool isJump(Op op);
[[nodiscard]] bool isUnary(Op op);

void writeInstruction(const Instruction& instruction, std::uint32_t address,
                      std::vector<std::uint8_t>& out);
[[nodiscard]] Instruction readInstruction(const Bus& bus, std::uint32_t address,
                                          std::uint32_t* next);
/// The listing form of an instruction at `address`, target addresses resolved.
[[nodiscard]] std::string formatInstruction(const Instruction& instruction,
                                            std::uint32_t address);

/// What `assemble` made of a listing.
struct Assembly {
    bool ok = false;
    std::uint32_t origin = 0;
    std::vector<std::uint8_t> bytes;
    std::vector<std::string> errors;
};

/// Assembles a listing. Labels, `ORG`, `DB`, `DW`, `DS` and `EQU` -- the same shape
/// the CHIP-8 assembler has, because a machine's assembler should be the one a
/// reader already knows in this tree.
[[nodiscard]] Assembly assemble(std::string_view source, std::uint32_t defaultOrigin = 0x001000);

}  // namespace timewarp::hb2
