# HB-16

The HB-2's processor: sixteen-bit data, a twenty-four-bit address space, eight
general registers, a stack pointer, a program counter and four flags. Little-endian.
It is the machine's own, not an emulation of anybody's, and the assembler, the
disassembler and the processor all read one encoding -- `native/include/timewarp/hb2/asm.hpp`.

```bash
./native/build/time_warp_gates hb Examples/hb2/blitter.s --ticks 4
```

The harness prints the registers and the cycle count, so a listing can be timed from a
shell.

---

## An instruction

One to three sixteen-bit words:

```text
word 0   [ opcode:6 ][ A:3 ][ B:3 ][ mode:4 ]
word 1,2 operand, in the form the mode asks for
```

| Mode | Written | Operand words |
| --- | --- | --- |
| `Reg` | `Rd, Rs` | none |
| `Imm` | `Rd, #value` | one: 16-bit |
| `Abs` | `Rd, [addr]` | two: address low, then high |
| `RegInd` | `Rd, [Rs]` | none |
| `Indexed` | `Rd, [Rs+disp]` | one: signed 8-bit displacement |
| `Rel` | a jump target | one: signed 16-bit, from the next instruction |

A jump's operand is an **absolute target**, not a displacement a person works out: the
assembler subtracts and the disassembler adds back. That is what makes
assemble-disassemble-assemble return the same bytes, which `hb2_tests` asserts.

`STORE` names its pointer in `A` and its data in `B`; everything else names its
destination in `A` and, when it has one, its pointer in `B`.

---

## The opcodes

Cycles are the base cost. Add **1** for `Imm`, `RegInd` or `Indexed`, **2** for `Abs`,
and **2** more when a branch is taken. The table is data
(`kCycles` in `asm.cpp`), and the manual and the machine print the same numbers.

| Opcode | Name | Cycles | What it does |
| --- | --- | --- | --- |
| 0 | `NOP` | 1 | Nothing |
| 1 | `HALT` | 1 | The program stops, and the machine reports `halted` |
| 2 | `MOV` | 2 | `Rd = source` |
| 3 | `ADD` | 2 | `Rd += source`, setting carry and overflow |
| 4 | `SUB` | 2 | `Rd -= source`, setting borrow and overflow |
| 5 | `AND` | 2 | Bitwise and |
| 6 | `OR` | 2 | Bitwise or |
| 7 | `XOR` | 2 | Bitwise exclusive or |
| 8 | `NOT` | 1 | `Rb = ~Rb` |
| 9 | `CMP` | 2 | Sets the flags from `Rd - source` and stores nothing |
| 10 | `SHL` | 2 | `Rd <<= source & 15`, carry is the last bit out |
| 11 | `SHR` | 2 | Logical shift right |
| 12 | `SAR` | 2 | Arithmetic shift right |
| 13 | `MUL` | 12 | Low sixteen bits of the product; carry is the high half |
| 14 | `DIV` | 20 | Quotient. A zero divisor faults |
| 15 | `MOD` | 20 | Remainder. A zero divisor faults |
| 16 | `INC` | 1 | `Rb += 1` |
| 17 | `DEC` | 1 | `Rb -= 1` |
| 18 | `NEG` | 1 | Two's complement of `Rb` |
| 19 | `TEST` | 2 | Sets the flags from `Ra & source` and stores nothing |
| 20 | `SWAP` | 2 | Exchanges `Ra` and `Rb` |
| 21 | `LOAD` | 4 | `Ra = 16 bits at the operand` |
| 22 | `LOADB` | 3 | `Ra = the byte at the operand` |
| 23 | `STORE` | 4 | `16 bits at the operand = Rb` |
| 24 | `STOREB` | 3 | `the byte at the operand = Rb` |
| 25 | `LEA` | 2 | `Ra = the low sixteen bits of the address` |
| 26 | `PUSH` | 3 | Pushes `Rb` |
| 27 | `POP` | 3 | Pops into `Rb` |
| 28 | `CALL` | 6 | Pushes the return address and jumps |
| 29 | `RET` | 5 | Pops and jumps |
| 30 | `JMP` | 3 | Jumps |
| 31 | `JZ` | 3 | Jumps when zero |
| 32 | `JNZ` | 3 | Jumps when not zero |
| 33 | `JC` | 3 | Jumps when carry |
| 34 | `JNC` | 3 | Jumps when no carry |
| 35 | `JN` | 3 | Jumps when negative |
| 36 | `JNS` | 3 | Jumps when not negative |
| 37 | `JLT` | 3 | Signed less than: negative is not overflow |
| 38 | `JGT` | 3 | Signed greater: not zero and negative equals overflow |
| 39 | `JLE` | 3 | Signed less or equal |
| 40 | `JGE` | 3 | Signed greater or equal |
| 41 | `INT` | 8 | Calls the operating system; see [the manual](README.md) |
| 42 | `IN` | 3 | `Ra = the byte at the port` |
| 43 | `OUT` | 3 | `the byte at the port = Rb` |

Anything else -- which includes `$FFFF`, the fill in ROM -- is an invalid instruction
and a fault that names the address. That is deliberate: a jump into an unwritten page
reports rather than running a field of `NOP`s for ever.

---

## The registers and the flags

`R0` to `R7`, sixteen bits each. The stack pointer starts at `$7F00` and grows down;
below `$7E00` the processor faults rather than eating the program. The program counter
is twenty-four bits, because the bus is.

The flag register holds `Z` (zero), `C` (carry), `N` (negative) and `V` (overflow),
plus an interrupt-enabled bit the machine sets at reset. Signed comparisons use
`N xor V`, which is why `JLT` is spelled that way in the table.

---

## The assembler

Labels, `ORG`, `DB`, `DW`, `DS` and `EQU`, in the manner of the CHIP-8 assembler this
tree already has. Comments start with `;`. Numbers are decimal, `$hex`, or `'c'` for a
character; an expression is sums and differences of those and labels.

```asm
        ORG $1000
start:  MOV R0, #1              ; an immediate
        MOV R1, #count          ; ...of a label's address
        MOV R2, [count]         ; sixteen bits at that address
        MOV R3, [R0]            ; ...at the address in R0
        MOV R4, [R0+2]          ; ...two further on
        STORE [R0], R1          ; and the other way round
        JMP start               ; an absolute target in the listing
count:  DW 42
table:  DB 1, 2, 3
        DS 8                    ; eight zero bytes
text:   DB "HB-2", 0            ; a string is the bytes it spells
value:  EQU $200000             ; a constant, usable above its own line
```

A bad line names itself and its line number: `line 4: STORE needs a bracketed
destination`, not a byte offset.

---

## Disassembly

The monitor's view of the same bytes is `formatInstruction`, and it prints what the
assembler accepts:

```text
$001000  MOV R0, #1
$001004  MOV R2, [$001010]
$00100B  STORE [R0+2], R1
$001010  JMP $001000
```

`hb2_tests` assembles a listing, disassembles it and assembles that, and requires the
same bytes -- the property that makes a disassembly a readable form of the truth
rather than a second opinion about it.
