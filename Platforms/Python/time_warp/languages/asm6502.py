"""MOS 6502 Assembly language executor for Time Warp Studio.

Educational two-pass assembler + cycle-accurate 6502 emulator.

Supported syntax (all standard 6502 assembly conventions):
  - Instructions: full 6502 instruction set (56 mnemonics, all addressing modes)
  - Directives: .ORG / ORG, .BYTE / .DB, .WORD / .DW, .TEXT / .ASCII, EQU / =
  - Labels: colon-terminated (LABEL:) or bare (LABEL EQU $1234)
  - Comments: semicolon to end of line
  - Numbers: $xx (hex), %bbbb (binary), decimal
  - Addressing modes:
      Implied        — BRK, NOP, RTS, ...
      Accumulator    — LSR A, ROL A
      Immediate      — LDA #$FF
      Zero Page      — LDA $10
      Zero Page,X    — LDA $10,X
      Zero Page,Y    — LDA $10,Y
      Absolute       — LDA $2000
      Absolute,X     — LDA $2000,X
      Absolute,Y     — LDA $2000,Y
      (Indirect,X)   — LDA ($10,X)
      (Indirect),Y   — LDA ($10),Y
      Relative       — BEQ LABEL (branch offset)
      Indirect       — JMP ($FFFC)

I/O model (memory-mapped ROM calls intercepted by the emulator):
  JSR $FFD2   — Print character in A to output
  JSR $FFCC   — Print newline
  JSR $FFB8   — Print A as unsigned decimal (0-255)
  JSR $FFB0   — Print A as 2-digit hex
  JSR $FFA0   — Print zero-terminated string; low byte of address in X, high in Y
  JSR $FF90   — Print signed integer in A (-128 to 127)
  Predefined equates (C64 KERNAL style):
    CHROUT = $FFD2
    NEWLINE = $FFCC
    PRINTDEC = $FFB8
    PRINTHEX = $FFB0
    PRINTSTR = $FFA0
    PRINTSDEC = $FF90

Execution:
  - Code assembled at $0200 by default (or .ORG directive)
  - BRK instruction terminates execution
  - Maximum 200,000 cycles (prevents infinite loops)
  - Stack at $01FF-$0100 (standard 6502)
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_asm6502(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Assemble and execute a 6502 program, returning all output."""
    env = Asm6502Environment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_DEFAULT_ORIGIN = 0x0200
_MAX_CYCLES = 200_000
_STACK_BASE = 0x0100
_RAM_SIZE = 0x10000  # 64 KB

# ROM call addresses (intercepted, no actual ROM needed)
_ROM_CHROUT = 0xFFD2     # print char in A
_ROM_NEWLINE = 0xFFCC    # print newline
_ROM_PRINTDEC = 0xFFB8   # print A as unsigned decimal
_ROM_PRINTHEX = 0xFFB0   # print A as 2-digit hex
_ROM_PRINTSTR = 0xFFA0   # print zero-terminated string at X:Y (lo:hi)
_ROM_PRINTSDEC = 0xFF90  # print A as signed decimal
_ROM_ADDRESSES = {
    _ROM_CHROUT, _ROM_NEWLINE, _ROM_PRINTDEC,
    _ROM_PRINTHEX, _ROM_PRINTSTR, _ROM_PRINTSDEC,
}

# Built-in equates
_BUILTINS: dict[str, int] = {
    "CHROUT": _ROM_CHROUT,
    "NEWLINE": _ROM_NEWLINE,
    "PRINTDEC": _ROM_PRINTDEC,
    "PRINTHEX": _ROM_PRINTHEX,
    "PRINTSTR": _ROM_PRINTSTR,
    "PRINTSDEC": _ROM_PRINTSDEC,
}

# Addressing mode constants
IMP = "imp"   # Implied
ACC = "acc"   # Accumulator
IMM = "imm"   # Immediate
ZP = "zp"     # Zero Page
ZPX = "zpx"   # Zero Page, X
ZPY = "zpy"   # Zero Page, Y
ABS = "abs"   # Absolute
ABSX = "absx" # Absolute, X
ABSY = "absy" # Absolute, Y
IND = "ind"   # Indirect
INDX = "indx" # (Indirect, X)
INDY = "indy" # (Indirect), Y
REL = "rel"   # Relative (branches)

# Instruction sizes (bytes) by addressing mode
_MODE_SIZE = {
    IMP: 1, ACC: 1, IMM: 2, ZP: 2, ZPX: 2, ZPY: 2,
    ABS: 3, ABSX: 3, ABSY: 3, IND: 3, INDX: 2, INDY: 2, REL: 2,
}

# ---------------------------------------------------------------------------
# Opcode table: (mnemonic, mode) -> byte
# ---------------------------------------------------------------------------
_OPCODES: dict[tuple[str, str], int] = {
    # LDA
    ("LDA", IMM): 0xA9, ("LDA", ZP): 0xA5, ("LDA", ZPX): 0xB5,
    ("LDA", ABS): 0xAD, ("LDA", ABSX): 0xBD, ("LDA", ABSY): 0xB9,
    ("LDA", INDX): 0xA1, ("LDA", INDY): 0xB1,
    # LDX
    ("LDX", IMM): 0xA2, ("LDX", ZP): 0xA6, ("LDX", ZPY): 0xB6,
    ("LDX", ABS): 0xAE, ("LDX", ABSY): 0xBE,
    # LDY
    ("LDY", IMM): 0xA0, ("LDY", ZP): 0xA4, ("LDY", ZPX): 0xB4,
    ("LDY", ABS): 0xAC, ("LDY", ABSX): 0xBC,
    # STA
    ("STA", ZP): 0x85, ("STA", ZPX): 0x95,
    ("STA", ABS): 0x8D, ("STA", ABSX): 0x9D, ("STA", ABSY): 0x99,
    ("STA", INDX): 0x81, ("STA", INDY): 0x91,
    # STX
    ("STX", ZP): 0x86, ("STX", ZPY): 0x96, ("STX", ABS): 0x8E,
    # STY
    ("STY", ZP): 0x84, ("STY", ZPX): 0x94, ("STY", ABS): 0x8C,
    # Transfer
    ("TAX", IMP): 0xAA, ("TAY", IMP): 0xA8,
    ("TXA", IMP): 0x8A, ("TYA", IMP): 0x98,
    ("TSX", IMP): 0xBA, ("TXS", IMP): 0x9A,
    # Stack
    ("PHA", IMP): 0x48, ("PLA", IMP): 0x68,
    ("PHP", IMP): 0x08, ("PLP", IMP): 0x28,
    # ADC
    ("ADC", IMM): 0x69, ("ADC", ZP): 0x65, ("ADC", ZPX): 0x75,
    ("ADC", ABS): 0x6D, ("ADC", ABSX): 0x7D, ("ADC", ABSY): 0x79,
    ("ADC", INDX): 0x61, ("ADC", INDY): 0x71,
    # SBC
    ("SBC", IMM): 0xE9, ("SBC", ZP): 0xE5, ("SBC", ZPX): 0xF5,
    ("SBC", ABS): 0xED, ("SBC", ABSX): 0xFD, ("SBC", ABSY): 0xF9,
    ("SBC", INDX): 0xE1, ("SBC", INDY): 0xF1,
    # AND
    ("AND", IMM): 0x29, ("AND", ZP): 0x25, ("AND", ZPX): 0x35,
    ("AND", ABS): 0x2D, ("AND", ABSX): 0x3D, ("AND", ABSY): 0x39,
    ("AND", INDX): 0x21, ("AND", INDY): 0x31,
    # ORA
    ("ORA", IMM): 0x09, ("ORA", ZP): 0x05, ("ORA", ZPX): 0x15,
    ("ORA", ABS): 0x0D, ("ORA", ABSX): 0x1D, ("ORA", ABSY): 0x19,
    ("ORA", INDX): 0x01, ("ORA", INDY): 0x11,
    # EOR
    ("EOR", IMM): 0x49, ("EOR", ZP): 0x45, ("EOR", ZPX): 0x55,
    ("EOR", ABS): 0x4D, ("EOR", ABSX): 0x5D, ("EOR", ABSY): 0x59,
    ("EOR", INDX): 0x41, ("EOR", INDY): 0x51,
    # INC/DEC memory
    ("INC", ZP): 0xE6, ("INC", ZPX): 0xF6, ("INC", ABS): 0xEE, ("INC", ABSX): 0xFE,
    ("DEC", ZP): 0xC6, ("DEC", ZPX): 0xD6, ("DEC", ABS): 0xCE, ("DEC", ABSX): 0xDE,
    # INX INY DEX DEY
    ("INX", IMP): 0xE8, ("INY", IMP): 0xC8,
    ("DEX", IMP): 0xCA, ("DEY", IMP): 0x88,
    # ASL
    ("ASL", ACC): 0x0A, ("ASL", ZP): 0x06, ("ASL", ZPX): 0x16,
    ("ASL", ABS): 0x0E, ("ASL", ABSX): 0x1E,
    # LSR
    ("LSR", ACC): 0x4A, ("LSR", ZP): 0x46, ("LSR", ZPX): 0x56,
    ("LSR", ABS): 0x4E, ("LSR", ABSX): 0x5E,
    # ROL
    ("ROL", ACC): 0x2A, ("ROL", ZP): 0x26, ("ROL", ZPX): 0x36,
    ("ROL", ABS): 0x2E, ("ROL", ABSX): 0x3E,
    # ROR
    ("ROR", ACC): 0x6A, ("ROR", ZP): 0x66, ("ROR", ZPX): 0x76,
    ("ROR", ABS): 0x6E, ("ROR", ABSX): 0x7E,
    # BIT
    ("BIT", ZP): 0x24, ("BIT", ABS): 0x2C,
    # CMP
    ("CMP", IMM): 0xC9, ("CMP", ZP): 0xC5, ("CMP", ZPX): 0xD5,
    ("CMP", ABS): 0xCD, ("CMP", ABSX): 0xDD, ("CMP", ABSY): 0xD9,
    ("CMP", INDX): 0xC1, ("CMP", INDY): 0xD1,
    # CPX CPY
    ("CPX", IMM): 0xE0, ("CPX", ZP): 0xE4, ("CPX", ABS): 0xEC,
    ("CPY", IMM): 0xC0, ("CPY", ZP): 0xC4, ("CPY", ABS): 0xCC,
    # Branches
    ("BCC", REL): 0x90, ("BCS", REL): 0xB0,
    ("BEQ", REL): 0xF0, ("BNE", REL): 0xD0,
    ("BMI", REL): 0x30, ("BPL", REL): 0x10,
    ("BVC", REL): 0x50, ("BVS", REL): 0x70,
    # JMP JSR RTS RTI
    ("JMP", ABS): 0x4C, ("JMP", IND): 0x6C,
    ("JSR", ABS): 0x20, ("RTS", IMP): 0x60, ("RTI", IMP): 0x40,
    # Flag ops
    ("CLC", IMP): 0x18, ("SEC", IMP): 0x38,
    ("CLD", IMP): 0xD8, ("SED", IMP): 0xF8,
    ("CLI", IMP): 0x58, ("SEI", IMP): 0x78,
    ("CLV", IMP): 0xB8,
    # Misc
    ("NOP", IMP): 0xEA, ("BRK", IMP): 0x00,
}

# Reverse opcode lookup: byte -> (mnemonic, mode)
_DECODE: dict[int, tuple[str, str]] = {v: k for k, v in _OPCODES.items()}


# ---------------------------------------------------------------------------
# Assembler error
# ---------------------------------------------------------------------------

class AssemblerError(Exception):
    pass


class CpuError(Exception):
    pass


# ---------------------------------------------------------------------------
# Assembler
# ---------------------------------------------------------------------------

@dataclass
class AsmLine:
    """Parsed assembly line."""
    label: Optional[str]
    mnemonic: Optional[str]
    mode: Optional[str]
    operand: int  # resolved operand value (-1 if unresolved label)
    label_ref: Optional[str]  # name of unresolved label reference
    address: int  # assigned PC address
    size: int     # bytes emitted


class Assembler:
    def __init__(self):
        self.symbols: dict[str, int] = dict(_BUILTINS)
        self.lines: list[AsmLine] = []
        self.memory = bytearray(_RAM_SIZE)
        self.origin = _DEFAULT_ORIGIN

    def assemble(self, source: str) -> None:
        raw_lines = source.splitlines()
        parsed = self._first_pass(raw_lines)
        self._second_pass(parsed)

    # ------------------------------------------------------------------
    # First pass: collect labels and sizes, produce partially-assembled list
    # ------------------------------------------------------------------

    def _first_pass(self, raw_lines: list[str]) -> list[AsmLine]:  # noqa: C901
        pc = self.origin
        result: list[AsmLine] = []

        for lineno, raw in enumerate(raw_lines, 1):
            line = raw.strip()
            # Remove comment
            if ";" in line:
                line = line[:line.index(";")].strip()
            if not line:
                continue

            label: Optional[str] = None
            mnemonic: Optional[str] = None

            # Label detection
            if ":" in line.split()[0]:
                parts = line.split(":", 1)
                label = parts[0].strip().upper()
                line = parts[1].strip()

            if not line:
                if label:
                    self.symbols[label] = pc
                continue

            tokens = line.split(None, 1)
            token0 = tokens[0].upper()

            # EQU / = constant
            if len(tokens) >= 2 and token0 not in _MNEMONICS:
                rest = tokens[1].strip()
                if rest.startswith("EQU ") or rest.startswith("= "):
                    val_str = rest.split(None, 1)[1].strip()
                    val = self._parse_num(val_str)
                    self.symbols[token0] = val
                    continue
                # name EQU value
            if token0 in ("EQU", "="):
                continue

            # Directives
            if token0 in (".ORG", "ORG"):
                if label:
                    self.symbols[label] = pc
                val = self._parse_num(tokens[1].strip()) if len(tokens) > 1 else pc
                pc = val
                self.origin = pc
                continue

            if token0 in (".BYTE", ".DB", "DB", "FCB"):
                if label:
                    self.symbols[label] = pc
                values = self._parse_data_bytes(tokens[1] if len(tokens) > 1 else "")
                for b in values:
                    self.memory[pc & 0xFFFF] = b & 0xFF
                    pc += 1
                continue

            if token0 in (".WORD", ".DW", "DW", "FDB"):
                if label:
                    self.symbols[label] = pc
                values = self._parse_data_words(tokens[1] if len(tokens) > 1 else "")
                for w in values:
                    self.memory[pc & 0xFFFF] = w & 0xFF
                    self.memory[(pc + 1) & 0xFFFF] = (w >> 8) & 0xFF
                    pc += 2
                continue

            if token0 in (".TEXT", ".ASCII", "FCC"):
                if label:
                    self.symbols[label] = pc
                text = self._parse_text(tokens[1] if len(tokens) > 1 else "")
                for ch in text:
                    self.memory[pc & 0xFFFF] = ord(ch) & 0xFF
                    pc += 1
                continue

            if token0 in (".ASCIIZ",):
                if label:
                    self.symbols[label] = pc
                text = self._parse_text(tokens[1] if len(tokens) > 1 else "")
                for ch in text:
                    self.memory[pc & 0xFFFF] = ord(ch) & 0xFF
                    pc += 1
                self.memory[pc & 0xFFFF] = 0
                pc += 1
                continue

            # End-of-source marker used by many educational assemblers.
            if token0 in (".END", "END"):
                continue

            # Regular instruction
            if label:
                self.symbols[label] = pc

            mnemonic = token0
            if mnemonic not in _MNEMONICS:
                raise AssemblerError(f"Line {lineno}: Unknown mnemonic '{mnemonic}'")

            operand_str = tokens[1].strip() if len(tokens) > 1 else ""
            mode, operand_val, label_ref = self._parse_operand(mnemonic, operand_str)

            size = _MODE_SIZE[mode]
            asmline = AsmLine(
                label=label,
                mnemonic=mnemonic,
                mode=mode,
                operand=operand_val,
                label_ref=label_ref,
                address=pc,
                size=size,
            )
            result.append(asmline)
            pc += size

        return result

    # ------------------------------------------------------------------
    # Second pass: resolve labels and emit opcodes
    # ------------------------------------------------------------------

    def _second_pass(self, parsed: list[AsmLine]) -> None:
        for asmline in parsed:
            mnemonic = asmline.mnemonic
            if mnemonic is None:
                continue

            # Resolve label reference
            operand = asmline.operand
            mode = asmline.mode
            if asmline.label_ref:
                ref = asmline.label_ref
                # Handle lo/hi byte selectors (<LABEL  >LABEL)
                byte_sel = None
                if ref.startswith("<"):
                    byte_sel = "lo"
                    ref = ref[1:]
                elif ref.startswith(">"):
                    byte_sel = "hi"
                    ref = ref[1:]
                if ref not in self.symbols:
                    raise AssemblerError(f"Undefined label: '{ref}'")
                operand = self.symbols[ref]
                if byte_sel == "lo":
                    operand = operand & 0xFF
                elif byte_sel == "hi":
                    operand = (operand >> 8) & 0xFF
                elif mode == REL:
                    pass  # keep REL, operand is target address
                # Possibly upgrade ZP to ABS if address > 0xFF
                elif mode == ZP and operand > 0xFF:
                    mode = ABS
                    asmline.size = 3

            # For branches, convert target address to offset
            if mode == REL:
                offset = operand - (asmline.address + 2)
                if offset < -128 or offset > 127:
                    raise AssemblerError(
                        f"Branch out of range at ${asmline.address:04X}: offset {offset}"
                    )
                operand = offset & 0xFF

            # Emit opcode
            key = (mnemonic, mode)
            if key not in _OPCODES:
                raise AssemblerError(
                    f"Instruction {mnemonic} does not support addressing mode {mode}"
                )
            opcode = _OPCODES[key]
            addr = asmline.address
            self.memory[addr] = opcode

            size = _MODE_SIZE[mode]
            if size == 2:
                self.memory[addr + 1] = operand & 0xFF
            elif size == 3:
                self.memory[addr + 1] = operand & 0xFF
                self.memory[addr + 2] = (operand >> 8) & 0xFF

        self.lines = parsed

    # ------------------------------------------------------------------
    # Operand parser
    # ------------------------------------------------------------------

    def _parse_operand(  # noqa: C901
        self, mnemonic: str, operand_str: str
    ) -> tuple[str, int, Optional[str]]:
        """Return (mode, value, label_ref) for an operand string."""
        s = operand_str.strip()
        label_ref: Optional[str] = None
        value = 0

        if not s:
            return IMP, 0, None

        # Accumulator
        if s.upper() in ("A", "ACC"):
            return ACC, 0, None

        # Immediate: #$FF, #10, #%1010, #LABEL, #<LABEL (lo), #>LABEL (hi)
        m = re.match(r"^#(.+)$", s)
        if m:
            val_s = m.group(1).strip()
            if val_s.startswith("<") and self._is_label(val_s[1:]):
                label_ref = "<" + val_s[1:].upper()
            elif val_s.startswith(">") and self._is_label(val_s[1:]):
                label_ref = ">" + val_s[1:].upper()
            elif self._is_label(val_s):
                label_ref = val_s.upper()
            else:
                value = self._parse_num(val_s) & 0xFF
            return IMM, value, label_ref

        # (Indirect, X): ($10,X) or ($LABEL,X)
        m = re.match(r"^\((.+),\s*X\)$", s, re.IGNORECASE)
        if m:
            val_s = m.group(1).strip()
            if self._is_label(val_s):
                label_ref = val_s.upper()
            else:
                value = self._parse_num(val_s) & 0xFF
            return INDX, value, label_ref

        # (Indirect), Y: ($10),Y
        m = re.match(r"^\((.+)\),\s*Y$", s, re.IGNORECASE)
        if m:
            val_s = m.group(1).strip()
            if self._is_label(val_s):
                label_ref = val_s.upper()
            else:
                value = self._parse_num(val_s) & 0xFF
            return INDY, value, label_ref

        # Indirect JMP: ($1234)
        m = re.match(r"^\((.+)\)$", s)
        if m:
            val_s = m.group(1).strip()
            if self._is_label(val_s):
                label_ref = val_s.upper()
            else:
                value = self._parse_num(val_s)
            return IND, value, label_ref

        # abs,X or zp,X
        m = re.match(r"^(.+),\s*X$", s, re.IGNORECASE)
        if m:
            val_s = m.group(1).strip()
            if self._is_label(val_s):
                label_ref = val_s.upper()
                mode = ZPX  # will be upgraded to ABSX if address > 0xFF
            else:
                value = self._parse_num(val_s)
                mode = ABSX if value > 0xFF else ZPX
            return mode, value, label_ref

        # abs,Y or zp,Y
        m = re.match(r"^(.+),\s*Y$", s, re.IGNORECASE)
        if m:
            val_s = m.group(1).strip()
            if self._is_label(val_s):
                label_ref = val_s.upper()
                mode = ZPY
            else:
                value = self._parse_num(val_s)
                mode = ABSY if value > 0xFF else ZPY
            return mode, value, label_ref

        # Branch instructions: operand is always a relative label/offset
        if mnemonic in _BRANCH_MNEMONICS:
            if self._is_label(s):
                label_ref = s.upper()
            else:
                value = self._parse_num(s)
                label_ref = None
            return REL, value, label_ref

        # Absolute/Zero Page
        if self._is_label(s):
            label_ref = s.upper()
            return ABS, 0, label_ref  # will be resolved in second pass
        value = self._parse_num(s)
        mode = ZP if value <= 0xFF else ABS
        return mode, value, None

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _parse_num(self, s: str) -> int:
        s = s.strip()
        if s.startswith("'") and s.endswith("'") and len(s) == 3:
            return ord(s[1])
        # Support simple assembler expressions like "'z'+1" and "$10-1".
        if "+" in s or "-" in s:
            parts = re.split(r"([+-])", s)
            if len(parts) >= 3:
                total: Optional[int] = None
                op = "+"
                for part in parts:
                    part = part.strip()
                    if not part:
                        continue
                    if part in ("+", "-"):
                        op = part
                        continue
                    val = self._parse_num(part)
                    if total is None:
                        total = val
                    elif op == "+":
                        total += val
                    else:
                        total -= val
                if total is not None:
                    return total
        if s.startswith("$"):
            return int(s[1:], 16)
        if s.startswith("0x") or s.startswith("0X"):
            return int(s, 16)
        if s.startswith("%"):
            return int(s[1:], 2)
        if s.upper() in self.symbols:
            return self.symbols[s.upper()]
        try:
            return int(s)
        except ValueError:
            raise AssemblerError(f"Cannot parse number: '{s}'")

    def _parse_data_bytes(self, s: str) -> list[int]:
        result = []
        for part in self._split_csv_quoted(s):
            part = part.strip()
            if not part:
                continue
            if part.startswith('"') or part.startswith("'"):
                for ch in part[1:-1]:
                    result.append(ord(ch))
            else:
                result.append(self._parse_num(part) & 0xFF)
        return result

    @staticmethod
    def _split_csv_quoted(s: str) -> list[str]:
        """Split CSV-style data while preserving commas inside quoted strings."""
        parts: list[str] = []
        buf: list[str] = []
        quote: Optional[str] = None
        escape = False

        for ch in s:
            if escape:
                buf.append(ch)
                escape = False
                continue
            if ch == "\\":
                buf.append(ch)
                escape = True
                continue
            if quote:
                buf.append(ch)
                if ch == quote:
                    quote = None
                continue
            if ch in ("\"", "'"):
                quote = ch
                buf.append(ch)
                continue
            if ch == ",":
                parts.append("".join(buf).strip())
                buf = []
                continue
            buf.append(ch)

        if buf:
            parts.append("".join(buf).strip())
        return parts

    def _parse_data_words(self, s: str) -> list[int]:
        return [self._parse_num(p.strip()) & 0xFFFF for p in s.split(",")]

    def _parse_text(self, s: str) -> str:
        s = s.strip()
        if (s.startswith('"') and s.endswith('"')) or (
            s.startswith("'") and s.endswith("'")
        ):
            return (
                s[1:-1]
                .replace("\\n", "\n")
                .replace("\\t", "\t")
                .replace("\\r", "\r")
            )
        return s

    @staticmethod
    def _is_label(s: str) -> bool:
        return bool(re.match(r"^[A-Za-z_][A-Za-z0-9_]*$", s))


# ---------------------------------------------------------------------------
# CPU emulator
# ---------------------------------------------------------------------------


class CPU6502:
    """Cycle-accurate (instruction-level) MOS 6502 emulator."""

    def __init__(self, memory: bytearray, origin: int, output_fn):
        self.memory = memory
        self.output = output_fn
        # Registers
        self.A = 0
        self.X = 0
        self.Y = 0
        self.SP = 0xFF
        self.PC = origin
        # Status flags
        self.N = False  # Negative
        self.V = False  # Overflow
        self.B = False  # Break
        self.D = False  # Decimal (ignored by this emulator)
        self.I = False  # Interrupt disable
        self.Z = False  # Zero
        self.C = False  # Carry
        # Execution tracking
        self.cycles = 0
        self.call_stack: list[int] = []  # for JSR/RTS tracking

    # ------------------------------------------------------------------
    # Memory access
    # ------------------------------------------------------------------

    def _read(self, addr: int) -> int:
        return self.memory[addr & 0xFFFF]

    def _write(self, addr: int, val: int) -> None:
        self.memory[addr & 0xFFFF] = val & 0xFF

    def _read_word(self, addr: int) -> int:
        lo = self._read(addr)
        hi = self._read(addr + 1)
        return lo | (hi << 8)

    def _fetch(self) -> int:
        val = self._read(self.PC)
        self.PC = (self.PC + 1) & 0xFFFF
        return val

    def _fetch_word(self) -> int:
        lo = self._fetch()
        hi = self._fetch()
        return lo | (hi << 8)

    # ------------------------------------------------------------------
    # Stack
    # ------------------------------------------------------------------

    def _push(self, val: int) -> None:
        self._write(_STACK_BASE + self.SP, val & 0xFF)
        self.SP = (self.SP - 1) & 0xFF

    def _pull(self) -> int:
        self.SP = (self.SP + 1) & 0xFF
        return self._read(_STACK_BASE + self.SP)

    def _push_word(self, val: int) -> None:
        self._push((val >> 8) & 0xFF)
        self._push(val & 0xFF)

    def _pull_word(self) -> int:
        lo = self._pull()
        hi = self._pull()
        return lo | (hi << 8)

    # ------------------------------------------------------------------
    # Status register
    # ------------------------------------------------------------------

    def _get_sr(self) -> int:
        return (
            (0x80 if self.N else 0)
            | (0x40 if self.V else 0)
            | 0x20  # unused, always 1
            | (0x10 if self.B else 0)
            | (0x08 if self.D else 0)
            | (0x04 if self.I else 0)
            | (0x02 if self.Z else 0)
            | (0x01 if self.C else 0)
        )

    def _set_sr(self, val: int) -> None:
        self.N = bool(val & 0x80)
        self.V = bool(val & 0x40)
        self.B = bool(val & 0x10)
        self.D = bool(val & 0x08)
        self.I = bool(val & 0x04)
        self.Z = bool(val & 0x02)
        self.C = bool(val & 0x01)

    def _update_nz(self, val: int) -> None:
        self.N = bool(val & 0x80)
        self.Z = val == 0

    # ------------------------------------------------------------------
    # Effective address calculation
    # ------------------------------------------------------------------

    def _ea(self, mode: str, operand: int) -> int:
        if mode == ZP:
            return operand & 0xFF
        if mode == ZPX:
            return (operand + self.X) & 0xFF
        if mode == ZPY:
            return (operand + self.Y) & 0xFF
        if mode == ABS:
            return operand
        if mode == ABSX:
            return (operand + self.X) & 0xFFFF
        if mode == ABSY:
            return (operand + self.Y) & 0xFFFF
        if mode == IND:
            lo = self._read(operand)
            hi = self._read((operand & 0xFF00) | ((operand + 1) & 0xFF))  # 6502 page wrap bug
            return lo | (hi << 8)
        if mode == INDX:
            ptr = (operand + self.X) & 0xFF
            lo = self._read(ptr)
            hi = self._read((ptr + 1) & 0xFF)
            return lo | (hi << 8)
        if mode == INDY:
            lo = self._read(operand & 0xFF)
            hi = self._read((operand + 1) & 0xFF)
            base = lo | (hi << 8)
            return (base + self.Y) & 0xFFFF
        return operand

    def _read_ea(self, mode: str, operand: int) -> int:
        if mode == IMM:
            return operand
        if mode == ACC:
            return self.A
        return self._read(self._ea(mode, operand))

    def _write_ea(self, mode: str, operand: int, val: int) -> None:
        if mode == ACC:
            self.A = val & 0xFF
            return
        self._write(self._ea(mode, operand), val)

    # ------------------------------------------------------------------
    # Main execution loop
    # ------------------------------------------------------------------

    def run(self) -> None:  # noqa: C901
        while self.cycles < _MAX_CYCLES:
            self.cycles += 1
            opcode = self._fetch()
            if opcode == 0x00:  # BRK
                break

            info = _DECODE.get(opcode)
            if info is None:
                # Skip unknown opcodes (treat as NOP)
                continue

            mnemonic, mode = info
            operand = 0
            size = _MODE_SIZE[mode]
            if size == 2:
                operand = self._fetch()
            elif size == 3:
                operand = self._fetch_word()

            self._execute(mnemonic, mode, operand)

        if self.cycles >= _MAX_CYCLES:
            raise CpuError("CPU cycle limit exceeded (infinite loop?)")

    def _execute(self, mnemonic: str, mode: str, operand: int) -> None:  # noqa: C901
        m = mnemonic

        # ----------------------------------------------------------
        # Load/Store
        # ----------------------------------------------------------
        if m == "LDA":
            self.A = self._read_ea(mode, operand)
            self._update_nz(self.A)

        elif m == "LDX":
            self.X = self._read_ea(mode, operand)
            self._update_nz(self.X)

        elif m == "LDY":
            self.Y = self._read_ea(mode, operand)
            self._update_nz(self.Y)

        elif m == "STA":
            self._write_ea(mode, operand, self.A)

        elif m == "STX":
            self._write_ea(mode, operand, self.X)

        elif m == "STY":
            self._write_ea(mode, operand, self.Y)

        # ----------------------------------------------------------
        # Transfers
        # ----------------------------------------------------------
        elif m == "TAX":
            self.X = self.A; self._update_nz(self.X)
        elif m == "TAY":
            self.Y = self.A; self._update_nz(self.Y)
        elif m == "TXA":
            self.A = self.X; self._update_nz(self.A)
        elif m == "TYA":
            self.A = self.Y; self._update_nz(self.A)
        elif m == "TSX":
            self.X = self.SP; self._update_nz(self.X)
        elif m == "TXS":
            self.SP = self.X

        # ----------------------------------------------------------
        # Stack
        # ----------------------------------------------------------
        elif m == "PHA":
            self._push(self.A)
        elif m == "PLA":
            self.A = self._pull(); self._update_nz(self.A)
        elif m == "PHP":
            self._push(self._get_sr() | 0x10)
        elif m == "PLP":
            self._set_sr(self._pull())

        # ----------------------------------------------------------
        # Arithmetic
        # ----------------------------------------------------------
        elif m == "ADC":
            val = self._read_ea(mode, operand)
            result = self.A + val + (1 if self.C else 0)
            self.V = bool(~(self.A ^ val) & (self.A ^ result) & 0x80)
            self.C = result > 0xFF
            self.A = result & 0xFF
            self._update_nz(self.A)

        elif m == "SBC":
            val = self._read_ea(mode, operand)
            result = self.A - val - (0 if self.C else 1)
            self.V = bool((self.A ^ val) & (self.A ^ result) & 0x80)
            self.C = result >= 0
            self.A = result & 0xFF
            self._update_nz(self.A)

        elif m == "INC":
            ea = self._ea(mode, operand)
            val = (self._read(ea) + 1) & 0xFF
            self._write(ea, val); self._update_nz(val)

        elif m == "DEC":
            ea = self._ea(mode, operand)
            val = (self._read(ea) - 1) & 0xFF
            self._write(ea, val); self._update_nz(val)

        elif m == "INX":
            self.X = (self.X + 1) & 0xFF; self._update_nz(self.X)
        elif m == "INY":
            self.Y = (self.Y + 1) & 0xFF; self._update_nz(self.Y)
        elif m == "DEX":
            self.X = (self.X - 1) & 0xFF; self._update_nz(self.X)
        elif m == "DEY":
            self.Y = (self.Y - 1) & 0xFF; self._update_nz(self.Y)

        # ----------------------------------------------------------
        # Bitwise
        # ----------------------------------------------------------
        elif m == "AND":
            self.A &= self._read_ea(mode, operand); self._update_nz(self.A)
        elif m == "ORA":
            self.A |= self._read_ea(mode, operand); self._update_nz(self.A)
        elif m == "EOR":
            self.A ^= self._read_ea(mode, operand); self._update_nz(self.A)

        elif m == "ASL":
            val = self._read_ea(mode, operand)
            self.C = bool(val & 0x80)
            val = (val << 1) & 0xFF
            self._write_ea(mode, operand, val); self._update_nz(val)

        elif m == "LSR":
            val = self._read_ea(mode, operand)
            self.C = bool(val & 0x01)
            val = (val >> 1) & 0xFF
            self._write_ea(mode, operand, val); self._update_nz(val)

        elif m == "ROL":
            val = self._read_ea(mode, operand)
            new_c = bool(val & 0x80)
            val = ((val << 1) | (1 if self.C else 0)) & 0xFF
            self.C = new_c
            self._write_ea(mode, operand, val); self._update_nz(val)

        elif m == "ROR":
            val = self._read_ea(mode, operand)
            new_c = bool(val & 0x01)
            val = ((val >> 1) | (0x80 if self.C else 0)) & 0xFF
            self.C = new_c
            self._write_ea(mode, operand, val); self._update_nz(val)

        elif m == "BIT":
            val = self._read_ea(mode, operand)
            self.N = bool(val & 0x80)
            self.V = bool(val & 0x40)
            self.Z = (self.A & val) == 0

        # ----------------------------------------------------------
        # Compare
        # ----------------------------------------------------------
        elif m == "CMP":
            val = self._read_ea(mode, operand)
            result = self.A - val
            self.C = self.A >= val
            self._update_nz(result & 0xFF)

        elif m == "CPX":
            val = self._read_ea(mode, operand)
            result = self.X - val
            self.C = self.X >= val
            self._update_nz(result & 0xFF)

        elif m == "CPY":
            val = self._read_ea(mode, operand)
            result = self.Y - val
            self.C = self.Y >= val
            self._update_nz(result & 0xFF)

        # ----------------------------------------------------------
        # Branches (REL mode — operand is signed offset)
        # ----------------------------------------------------------
        elif m == "BCC":
            self._branch(not self.C, operand)
        elif m == "BCS":
            self._branch(self.C, operand)
        elif m == "BEQ":
            self._branch(self.Z, operand)
        elif m == "BNE":
            self._branch(not self.Z, operand)
        elif m == "BMI":
            self._branch(self.N, operand)
        elif m == "BPL":
            self._branch(not self.N, operand)
        elif m == "BVC":
            self._branch(not self.V, operand)
        elif m == "BVS":
            self._branch(self.V, operand)

        # ----------------------------------------------------------
        # Jumps / calls
        # ----------------------------------------------------------
        elif m == "JMP":
            target = self._ea(mode, operand)
            self.PC = target

        elif m == "JSR":
            target = self._ea(mode, operand)
            # Check for ROM intercept
            if target in _ROM_ADDRESSES:
                self._rom_call(target)
                return
            # Push return address (PC - 1, i.e., last byte of JSR operand)
            ret = (self.PC - 1) & 0xFFFF
            self._push((ret >> 8) & 0xFF)
            self._push(ret & 0xFF)
            self.PC = target

        elif m == "RTS":
            lo = self._pull()
            hi = self._pull()
            self.PC = ((lo | (hi << 8)) + 1) & 0xFFFF

        elif m == "RTI":
            self._set_sr(self._pull())
            lo = self._pull()
            hi = self._pull()
            self.PC = lo | (hi << 8)

        # ----------------------------------------------------------
        # Flag operations
        # ----------------------------------------------------------
        elif m == "CLC":
            self.C = False
        elif m == "SEC":
            self.C = True
        elif m == "CLD":
            self.D = False
        elif m == "SED":
            self.D = True
        elif m == "CLI":
            self.I = False
        elif m == "SEI":
            self.I = True
        elif m == "CLV":
            self.V = False

        # NOP — nothing
        elif m == "NOP":
            pass

    def _branch(self, cond: bool, raw_offset: int) -> None:
        if cond:
            # Offset is stored as unsigned byte; convert to signed
            offset = raw_offset if raw_offset < 0x80 else raw_offset - 0x100
            self.PC = (self.PC + offset) & 0xFFFF

    def _rom_call(self, addr: int) -> None:
        if addr == _ROM_CHROUT:
            self.output(chr(self.A & 0x7F))
        elif addr == _ROM_NEWLINE:
            self.output("\n")
        elif addr == _ROM_PRINTDEC:
            self.output(str(self.A))
        elif addr == _ROM_PRINTHEX:
            self.output(f"{self.A:02X}")
        elif addr == _ROM_PRINTSTR:
            str_addr = self.X | (self.Y << 8)
            chars = []
            for _ in range(256):
                b = self._read(str_addr)
                if b == 0:
                    break
                chars.append(chr(b & 0x7F))
                str_addr += 1
            self.output("".join(chars))
        elif addr == _ROM_PRINTSDEC:
            signed = self.A if self.A < 0x80 else self.A - 0x100
            self.output(str(signed))


# ---------------------------------------------------------------------------
# Main environment
# ---------------------------------------------------------------------------


class Asm6502Environment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output_parts: list[str] = []

    def _output_fn(self, text: str) -> None:
        self._output_parts.append(text)

    def run(self, source: str) -> str:
        try:
            asm = Assembler()
            asm.assemble(source)
            cpu = CPU6502(asm.memory, asm.origin, self._output_fn)
            cpu.run()
        except AssemblerError as e:
            self._output_parts.append(f"❌ Assembly error: {e}\n")
        except CpuError as e:
            self._output_parts.append(f"❌ CPU error: {e}\n")
        except Exception as e:
            self._output_parts.append(f"❌ Error: {e}\n")

        out = "".join(self._output_parts)
        if out and not out.endswith("\n"):
            out += "\n"
        return out


# ---------------------------------------------------------------------------
# Lookup sets
# ---------------------------------------------------------------------------

_MNEMONICS: frozenset[str] = frozenset(mnemonic for mnemonic, _ in _OPCODES)
_BRANCH_MNEMONICS: frozenset[str] = frozenset(
    ["BCC", "BCS", "BEQ", "BNE", "BMI", "BPL", "BVC", "BVS"]
)
