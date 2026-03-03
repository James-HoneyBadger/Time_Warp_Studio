"""Assembly language executor for Time Warp Studio.

Educational virtual-CPU assembly interpreter.
16 general-purpose registers (R0-R15), 1024-word memory,
a flag register, a call stack, and basic turtle commands.

Instruction set:
  MOV  Rx, Ry/imm     – load register
  ADD  Rx, Ry/imm     – add
  SUB  Rx, Ry/imm     – subtract
  MUL  Rx, Ry/imm     – multiply
  DIV  Rx, Ry/imm     – divide
  MOD  Rx, Ry/imm     – modulo
  AND  Rx, Ry/imm     – bitwise AND
  OR   Rx, Ry/imm     – bitwise OR
  XOR  Rx, Ry/imm     – bitwise XOR
  NOT  Rx              – bitwise NOT
  CMP  Rx, Ry/imm     – compare (sets Z, N flags)
  INC  Rx              – increment
  DEC  Rx              – decrement
  JMP  label           – unconditional jump
  JE   label           – jump if equal (Z=1)
  JNE  label           – jump if not equal (Z=0)
  JG   label           – jump if greater (N=0, Z=0)
  JL   label           – jump if less (N=1)
  JGE  label           – jump if >= (N=0)
  JLE  label           – jump if <= (Z=1 or N=1)
  PUSH Rx              – push register onto stack
  POP  Rx              – pop stack top into register
  CALL label           – call subroutine
  RET                  – return from subroutine
  LOAD Rx, [addr/Ry]  – load from memory
  STORE [addr/Ry], Rx – store to memory
  PRINT Rx/"string"   – output
  INPUT Rx             – read integer
  HALT                 – stop execution
  NOP                  – no operation
  label:               – label definition
  ; comment
"""

from __future__ import annotations

import math as _math
import re
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..core.turtle_state import TurtleState

MAX_STEPS = 500_000


def execute_assembly(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a virtual-CPU assembly program and return all output."""
    cpu = VirtualCPU(interpreter, turtle)
    return cpu.run(source)


class VirtualCPU:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self.regs: list[int] = [0] * 16
        self.memory: list[int] = [0] * 1024
        self.stack: list[int] = []
        self.call_stack: list[int] = []
        self.flags: dict[str, bool] = {
            "Z": False, "N": False, "S": False,  # Zero, Negative, Sign
            "C": False, "O": False, "P": False,  # Carry, Overflow, Parity
        }
        self.labels: dict[str, int] = {}
        self.data_labels: dict[str, int] = {}  # label -> memory address
        self.data_strings: dict[str, str] = {}  # label -> stored string
        self._data_ptr = 0  # next free byte in memory for data
        self.instructions: list[tuple[str, str]] = []  # (opcode, args_str)
        self.ip = 0

    def _emit(self, text: str):
        self._output.append(str(text))

    def run(self, source: str) -> str:
        try:
            self._parse(source)
            self._execute()
        except HaltException:
            pass
        except AsmError as e:
            self._emit(f"❌ Assembly error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    # ------------------------------------------------------------------
    # Parser
    # ------------------------------------------------------------------

    @staticmethod
    def _upper_preserve_strings(line: str) -> str:
        """Uppercase mnemonics/registers but preserve quoted string literals."""
        result: list[str] = []
        in_str = False
        qchar = ""
        for ch in line:
            if in_str:
                result.append(ch)
                if ch == qchar:
                    in_str = False
            elif ch in ('"', "'"):
                in_str = True
                qchar = ch
                result.append(ch)
            else:
                result.append(ch.upper())
        return "".join(result)

    def _parse(self, source: str):
        line_no = 0
        for raw in source.splitlines():
            # Strip comments
            stmt = re.sub(r";.*$", "", raw).strip()
            if not stmt:
                continue
            # Label definition  (label: [instr])
            m = re.match(r"^(\w+)\s*:\s*(.*)$", stmt)
            if m:
                self.labels[m.group(1).upper()] = line_no
                remainder = m.group(2).strip()
                if remainder:
                    op, args = self._split_instr(self._upper_preserve_strings(remainder))
                    self.instructions.append((op, args))
                    line_no += 1
                continue
            # Data definition without colon: name DB/DW/DD/DQ "string",0  or  name DW 42
            dm = re.match(r"^(\w+)\s+(DB|DW|DD|DQ)\b\s*(.*)", stmt, re.IGNORECASE)
            if dm:
                label_name = dm.group(1).upper()
                directive = dm.group(2).upper()
                data_args = dm.group(3).strip()
                self.data_labels[label_name] = self._data_ptr
                self._store_data(label_name, directive, data_args)
                # Also emit a no-op instruction so line numbers stay consistent
                self.instructions.append(("_DATA", ""))
                line_no += 1
                continue
            op, args = self._split_instr(self._upper_preserve_strings(stmt))
            self.instructions.append((op, args))
            line_no += 1

    def _store_data(self, label: str, directive: str, args: str):
        """Parse DB/DW data and store in memory + data_strings."""
        # Collect all data items (strings, numbers)
        text_parts: list[str] = []
        addr = self._data_ptr
        i = 0
        while i < len(args):
            c = args[i]
            if c in ('"', "'"):
                # Quoted string
                end = args.index(c, i + 1)
                s = args[i + 1 : end]
                text_parts.append(s)
                for ch in s:
                    if self._data_ptr < len(self.memory):
                        self.memory[self._data_ptr] = ord(ch)
                        self._data_ptr += 1
                i = end + 1
            elif c == ",":
                i += 1
            elif c.isspace():
                i += 1
            else:
                # Numeric value
                end = i
                while end < len(args) and args[end] not in (",", " ", "\t"):
                    end += 1
                num_str = args[i:end].strip()
                if num_str:
                    try:
                        val = int(num_str, 0)  # handles 0x, 0b, decimal
                    except ValueError:
                        val = 0
                    if self._data_ptr < len(self.memory):
                        self.memory[self._data_ptr] = val
                        self._data_ptr += 1
                i = end
        # Store the combined text for easy PRINTS access
        if text_parts:
            self.data_strings[label] = "".join(text_parts)

    def _split_instr(self, stmt: str) -> tuple[str, str]:
        parts = stmt.split(None, 1)
        op = parts[0]
        args = parts[1] if len(parts) > 1 else ""
        return op, args

    # ------------------------------------------------------------------
    # Executor
    # ------------------------------------------------------------------

    def _execute(self):
        steps = 0
        while 0 <= self.ip < len(self.instructions):
            steps += 1
            if steps > MAX_STEPS:
                self._emit(f"❌ Assembly: execution limit ({MAX_STEPS} steps) exceeded")
                break
            op, args = self.instructions[self.ip]
            self.ip += 1
            self._exec_instr(op, args)

    def _exec_instr(self, op: str, args: str):
        a = [x.strip() for x in args.split(",") if x.strip()] if args.strip() else []

        if op == "NOP":
            pass
        elif op == "HALT":
            raise HaltException()
        elif op == "MOV":
            self._reg_set(a[0], self._val(a[1]))
        elif op == "ADD":
            r = self._reg_get(a[0]) + self._val(a[1])
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "SUB":
            r = self._reg_get(a[0]) - self._val(a[1])
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "MUL":
            r = self._reg_get(a[0]) * self._val(a[1])
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "DIV":
            v = self._val(a[1])
            if v == 0:
                raise AsmError("Division by zero")
            r = self._reg_get(a[0]) // v
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "MOD":
            v = self._val(a[1])
            if v == 0:
                raise AsmError("Modulo by zero")
            r = self._reg_get(a[0]) % v
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "AND":
            r = self._reg_get(a[0]) & self._val(a[1])
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "OR":
            r = self._reg_get(a[0]) | self._val(a[1])
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "XOR":
            r = self._reg_get(a[0]) ^ self._val(a[1])
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "NOT":
            r = ~self._reg_get(a[0])
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "INC":
            r = self._reg_get(a[0]) + 1
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "DEC":
            r = self._reg_get(a[0]) - 1
            self._reg_set(a[0], r)
            self._set_flags(r)
        elif op == "CMP":
            r = self._reg_get(a[0]) - self._val(a[1])
            self._set_flags(r)
        elif op == "PUSH":
            self.stack.append(self._reg_get(a[0]))
        elif op == "POP":
            if not self.stack:
                raise AsmError("Stack underflow")
            self._reg_set(a[0], self.stack.pop())
        elif op == "CALL":
            self.call_stack.append(self.ip)
            self._jump(a[0])
        elif op == "RET":
            if not self.call_stack:
                raise HaltException()
            self.ip = self.call_stack.pop()
        elif op == "LOAD":
            addr = self._mem_addr(a[1])
            self._reg_set(a[0], self.memory[addr])
        elif op == "STORE":
            addr = self._mem_addr(a[0])
            self.memory[addr] = self._reg_get(a[1])
        elif op == "JMP":
            self._jump(a[0])
        elif op == "JE":
            if self.flags["Z"]:
                self._jump(a[0])
        elif op == "JNE":
            if not self.flags["Z"]:
                self._jump(a[0])
        elif op == "JG":
            if not self.flags["N"] and not self.flags["Z"]:
                self._jump(a[0])
        elif op == "JL":
            if self.flags["N"]:
                self._jump(a[0])
        elif op == "JGE":
            if not self.flags["N"]:
                self._jump(a[0])
        elif op == "JLE":
            if self.flags["N"] or self.flags["Z"]:
                self._jump(a[0])
        elif op == "PRINT":
            raw = args.strip()
            if raw.startswith('"') or raw.startswith("'"):
                msg = raw.strip('"\'')
                self._emit(msg)
            else:
                self._emit(str(self._val(raw)))
        elif op == "INPUT":
            raw = ""
            if hasattr(self.interpreter, "request_input"):
                raw = self.interpreter.request_input("asm> ") or "0"
            try:
                self._reg_set(a[0], int(raw))
            except ValueError:
                self._reg_set(a[0], 0)
        # Turtle extension commands
        elif op == "FORWARD" or op == "FWD":
            self.turtle.forward(self._val(a[0]))
        elif op == "BACKWARD" or op == "BCK":
            self.turtle.forward(-self._val(a[0]))
        elif op == "LEFT" or op == "LFT":
            self.turtle.left(self._val(a[0]))
        elif op == "RIGHT" or op == "RGT":
            self.turtle.right(self._val(a[0]))
        elif op == "PEN":
            if a and a[0] == "UP":
                self.turtle.pen_up()
            elif a and a[0] == "DOWN":
                self.turtle.pen_down()

        # ── Shift / Rotate ───────────────────────────────────────────────────
        elif op in ("SHL", "SAL"):
            n = self._val(a[1]) if len(a) > 1 else 1
            r = (self._reg_get(a[0]) << n) & 0xFFFFFFFF
            self._reg_set(a[0], r); self._set_flags(r)
        elif op == "SHR":
            n = self._val(a[1]) if len(a) > 1 else 1
            r = (self._reg_get(a[0]) & 0xFFFFFFFF) >> n
            self._reg_set(a[0], r); self._set_flags(r)
        elif op == "SAR":
            n = self._val(a[1]) if len(a) > 1 else 1
            r = self._reg_get(a[0]) >> n   # Python >> is arithmetic
            self._reg_set(a[0], r); self._set_flags(r)
        elif op == "ROL":
            n = (self._val(a[1]) if len(a) > 1 else 1) & 31
            v = self._reg_get(a[0]) & 0xFFFFFFFF
            r = ((v << n) | (v >> (32 - n))) & 0xFFFFFFFF
            self._reg_set(a[0], r); self._set_flags(r)
        elif op == "ROR":
            n = (self._val(a[1]) if len(a) > 1 else 1) & 31
            v = self._reg_get(a[0]) & 0xFFFFFFFF
            r = ((v >> n) | (v << (32 - n))) & 0xFFFFFFFF
            self._reg_set(a[0], r); self._set_flags(r)
        elif op == "RCL":
            n = (self._val(a[1]) if len(a) > 1 else 1) & 31
            v = self._reg_get(a[0]) & 0xFFFFFFFF
            cf = 1 if self.flags.get("C") else 0
            # 33-bit rotation including carry
            v33 = (v << 1) | cf
            r = ((v33 << n) | (v33 >> (33 - n))) & 0xFFFFFFFF
            self._reg_set(a[0], r); self._set_flags(r)
        elif op == "RCR":
            n = (self._val(a[1]) if len(a) > 1 else 1) & 31
            v = self._reg_get(a[0]) & 0xFFFFFFFF
            cf = 1 if self.flags.get("C") else 0
            v33 = (cf << 32) | v
            r = ((v33 >> n) | (v33 << (33 - n))) & 0xFFFFFFFF
            self._reg_set(a[0], r); self._set_flags(r)

        # ── Arithmetic extensions ────────────────────────────────────────────
        elif op == "NEG":
            r = -self._reg_get(a[0]); self._reg_set(a[0], r); self._set_flags(r)
        elif op in ("IMUL", "SMUL"):
            r = self._reg_get(a[0]) * self._val(a[1])
            self._reg_set(a[0], r); self._set_flags(r)
        elif op in ("IDIV", "SDIV"):
            v = self._val(a[1])
            if v == 0: raise AsmError("Division by zero")
            r = int(self._reg_get(a[0]) / v)
            self._reg_set(a[0], r); self._set_flags(r)
        elif op == "MULH":
            # High 32 bits of 64-bit product
            pr = self._reg_get(a[0]) * self._val(a[1])
            self._reg_set(a[0], (pr >> 32) & 0xFFFFFFFF)
        elif op == "ABS":
            r = abs(self._reg_get(a[0])); self._reg_set(a[0], r)
        elif op == "CBRT":
            import math as _m
            v = self._reg_get(a[0])
            r = int(_m.cbrt(v) if v >= 0 else -_m.cbrt(-v))
            self._reg_set(a[0], r)
        elif op == "SQRT_INT":
            import math as _m
            r = int(_m.sqrt(max(0, self._reg_get(a[0]))))
            self._reg_set(a[0], r)

        # ── Data movement extensions ─────────────────────────────────────────
        elif op == "XCHG":
            va, vb = self._reg_get(a[0]), self._reg_get(a[1])
            self._reg_set(a[0], vb); self._reg_set(a[1], va)
        elif op == "MOVZX":
            # zero-extend byte/word from register or memory
            self._reg_set(a[0], self._val(a[1]) & 0xFF)
        elif op == "MOVSX":
            # sign-extend
            v = self._val(a[1]) & 0xFF
            self._reg_set(a[0], v if v < 128 else v - 256)
        elif op == "LEA":
            # Load effective address: LEA Rx, [addr]
            operand = (a[1] if len(a) > 1 else a[0]).strip()
            m2 = re.match(r"^\[(.+)\]$", operand)
            if m2:
                addr = self._val(m2.group(1).strip())
            else:
                addr = self._val(operand)
            self._reg_set(a[0], addr)
        elif op == "CMOV" or op == "CMOVE":
            if self.flags["Z"]: self._reg_set(a[0], self._val(a[1]))
        elif op == "CMOVNE":
            if not self.flags["Z"]: self._reg_set(a[0], self._val(a[1]))
        elif op == "CMOVG":
            if not self.flags["N"] and not self.flags["Z"]: self._reg_set(a[0], self._val(a[1]))
        elif op == "CMOVL":
            if self.flags["N"]: self._reg_set(a[0], self._val(a[1]))
        elif op in ("MOVQ", "MOVDQU", "MOVDQA"):
            self._reg_set(a[0], self._val(a[1]))

        # ── Additional jump aliases ─────────────────────────────────────────
        elif op in ("JZ",):         # alias for JE
            if self.flags["Z"]: self._jump(a[0])
        elif op in ("JNZ",):        # alias for JNE
            if not self.flags["Z"]: self._jump(a[0])
        elif op in ("JB", "JC", "JNAE"):   # unsigned below / carry
            if self.flags.get("C"): self._jump(a[0])
        elif op in ("JNB", "JNC", "JAE"):  # unsigned not below / no carry
            if not self.flags.get("C"): self._jump(a[0])
        elif op in ("JBE", "JNA"):   # unsigned below or equal
            if self.flags.get("C") or self.flags["Z"]: self._jump(a[0])
        elif op in ("JA", "JNBE"):   # unsigned above
            if not self.flags.get("C") and not self.flags["Z"]: self._jump(a[0])
        elif op in ("JS",):          # sign flag
            if self.flags["N"]: self._jump(a[0])
        elif op in ("JNS",):         # no sign
            if not self.flags["N"]: self._jump(a[0])
        elif op in ("JO",):          # overflow — stub: treat as never
            pass
        elif op in ("JNO",):         # no overflow — stub: always jump
            self._jump(a[0])
        elif op in ("JP", "JPE"):    # parity even — stub
            pass
        elif op in ("JNP", "JPO"):   # parity odd — stub: always jump
            self._jump(a[0])
        elif op in ("JCXZ", "JECXZ", "JRCXZ"):  # jump if CX=0
            if self.regs[0] == 0: self._jump(a[0])

        # ── Bit operations ──────────────────────────────────────────────────
        elif op == "BT":
            # Bit test: sets/clears C flag
            val = self._reg_get(a[0]); bit = self._val(a[1]) & 31
            self.flags["C"] = bool((val >> bit) & 1)
        elif op == "BTS":
            val = self._reg_get(a[0]); bit = self._val(a[1]) & 31
            self.flags["C"] = bool((val >> bit) & 1)
            self._reg_set(a[0], val | (1 << bit))
        elif op == "BTR":
            val = self._reg_get(a[0]); bit = self._val(a[1]) & 31
            self.flags["C"] = bool((val >> bit) & 1)
            self._reg_set(a[0], val & ~(1 << bit))
        elif op == "BTC":
            val = self._reg_get(a[0]); bit = self._val(a[1]) & 31
            self.flags["C"] = bool((val >> bit) & 1)
            self._reg_set(a[0], val ^ (1 << bit))
        elif op == "BSF":
            # Bit scan forward — index of lowest set bit
            val = self._reg_get(a[1]) if len(a) > 1 else self._reg_get(a[0])
            if val == 0: self.flags["Z"] = True
            else:
                self.flags["Z"] = False
                bit = (val & -val).bit_length() - 1
                self._reg_set(a[0], bit)
        elif op == "BSR":
            # Bit scan reverse — index of highest set bit
            val = self._reg_get(a[1]) if len(a) > 1 else self._reg_get(a[0])
            if val == 0: self.flags["Z"] = True
            else:
                self.flags["Z"] = False
                self._reg_set(a[0], val.bit_length() - 1)
        elif op == "POPCNT":
            val = self._reg_get(a[1]) if len(a) > 1 else self._reg_get(a[0])
            self._reg_set(a[0], bin(val & 0xFFFFFFFF).count("1"))
        elif op in ("TZCNT", "LZCNT"):
            val = self._reg_get(a[1]) if len(a) > 1 else self._reg_get(a[0])
            if op == "TZCNT":
                r = (val & -val).bit_length() - 1 if val else 32
            else:
                r = 32 - val.bit_length() if val else 32
            self._reg_set(a[0], r)

        # ── Flag operations ─────────────────────────────────────────────────
        elif op == "TEST":
            r = self._reg_get(a[0]) & self._val(a[1]); self._set_flags(r)
        elif op == "CLFLAGS":
            self.flags = {"Z": False, "N": False, "C": False}
        elif op == "SETFLAGS":
            pass
        elif op == "PUSHF":
            v = (1 if self.flags["Z"] else 0) | (2 if self.flags["N"] else 0) | (4 if self.flags.get("C") else 0)
            self.stack.append(v)
        elif op == "POPF":
            v = self.stack.pop() if self.stack else 0
            self.flags["Z"] = bool(v & 1); self.flags["N"] = bool(v & 2); self.flags["C"] = bool(v & 4)
        elif op == "LAHF":
            v = (1 if self.flags["Z"] else 0) | (2 if self.flags["N"] else 0)
            self._reg_set("R0", v)
        elif op == "SAHF":
            v = self._reg_get("R0"); self.flags["Z"] = bool(v & 1); self.flags["N"] = bool(v & 2)
        elif op in ("STC", "STCF"):
            self.flags["C"] = True
        elif op in ("CLC", "CLCF"):
            self.flags["C"] = False
        elif op in ("CMC", "CMCF"):
            self.flags["C"] = not self.flags.get("C", False)
        elif op == "CLD": pass   # direction flag stub
        elif op == "STD": pass   # direction flag stub
        elif op == "CLI": pass   # interrupt stubs
        elif op == "STI": pass

        # ── Set-byte on condition ────────────────────────────────────────────
        elif op == "SETE"  or op == "SETZ":  self._reg_set(a[0], 1 if self.flags["Z"] else 0)
        elif op == "SETNE" or op == "SETNZ": self._reg_set(a[0], 0 if self.flags["Z"] else 1)
        elif op == "SETG":  self._reg_set(a[0], 1 if (not self.flags["N"] and not self.flags["Z"]) else 0)
        elif op == "SETL":  self._reg_set(a[0], 1 if self.flags["N"] else 0)
        elif op == "SETGE": self._reg_set(a[0], 1 if not self.flags["N"] else 0)
        elif op == "SETLE": self._reg_set(a[0], 1 if (self.flags["N"] or self.flags["Z"]) else 0)
        elif op == "SETS":  self._reg_set(a[0], 1 if self.flags["N"] else 0)
        elif op == "SETNS": self._reg_set(a[0], 0 if self.flags["N"] else 1)

        # ── String/memory block ops ─────────────────────────────────────────
        elif op in ("MOVS", "MOVSB", "MOVSW", "MOVSD"):
            # Move memory[R1] → memory[R0], R1++, R0++, R2--
            src, dst, cnt = self.regs[1], self.regs[0], self.regs[2]
            if 0 <= src < len(self.memory) and 0 <= dst < len(self.memory):
                self.memory[dst] = self.memory[src]
            self.regs[0] += 1; self.regs[1] += 1; self.regs[2] = max(0, cnt - 1)
        elif op in ("STOS", "STOSB", "STOSD"):
            dst = self.regs[0]
            if 0 <= dst < len(self.memory): self.memory[dst] = self.regs[1]
            self.regs[0] += 1; self.regs[2] = max(0, self.regs[2] - 1)
        elif op in ("LODS", "LODSB", "LODSD"):
            src = self.regs[1]
            if 0 <= src < len(self.memory): self.regs[0] = self.memory[src]
            self.regs[1] += 1; self.regs[2] = max(0, self.regs[2] - 1)
        elif op in ("SCAS", "SCASB", "SCASD"):
            # Compare R0 with memory[R1]
            v = self.memory[self.regs[1]] if 0 <= self.regs[1] < len(self.memory) else 0
            self._set_flags(self.regs[0] - v); self.regs[1] += 1; self.regs[2] = max(0, self.regs[2] - 1)
        elif op in ("CMPS", "CMPSB", "CMPSD"):
            v1 = self.memory[self.regs[0]] if 0 <= self.regs[0] < len(self.memory) else 0
            v2 = self.memory[self.regs[1]] if 0 <= self.regs[1] < len(self.memory) else 0
            self._set_flags(v1 - v2); self.regs[0] += 1; self.regs[1] += 1
        elif op == "REP":
            # REP prefix: repeat next instr R2 times (simplified: just note it)
            pass
        elif op in ("REPE", "REPZ", "REPNE", "REPNZ"):
            pass

        # ── Stack extras ────────────────────────────────────────────────────
        elif op == "PUSHA":
            for r in self.regs[:8]: self.stack.append(r)
        elif op == "POPA":
            vals = [self.stack.pop() if self.stack else 0 for _ in range(8)]
            for i, v in enumerate(reversed(vals)): self.regs[i] = v
        elif op == "PUSHAD":
            for r in self.regs: self.stack.append(r)
        elif op == "POPAD":
            vals = [self.stack.pop() if self.stack else 0 for _ in range(16)]
            self.regs[:] = list(reversed(vals))

        # ── Loop instruction ────────────────────────────────────────────────
        elif op == "LOOP":
            self.regs[0] -= 1  # decrement R0 (acting as CX)
            if self.regs[0] != 0: self._jump(a[0])
        elif op == "LOOPE" or op == "LOOPZ":
            self.regs[0] -= 1
            if self.regs[0] != 0 and self.flags["Z"]: self._jump(a[0])
        elif op == "LOOPNE" or op == "LOOPNZ":
            self.regs[0] -= 1
            if self.regs[0] != 0 and not self.flags["Z"]: self._jump(a[0])

        # ── Floating-point (software FPU, f-stack in regs 8-15) ─────────────
        elif op == "FINIT":
            # Clear float registers
            for i in range(8, 16): self.regs[i] = 0
            self._fstack: list[float] = []
        elif op == "FLD":
            if not hasattr(self, "_fstack"): self._fstack = []
            v = float(self._reg_get(a[0]))
            self._fstack.append(v)
        elif op == "FILD":
            if not hasattr(self, "_fstack"): self._fstack = []
            self._fstack.append(float(self._val(a[0])))
        elif op == "FLDPI":
            if not hasattr(self, "_fstack"): self._fstack = []
            self._fstack.append(_math.pi)
        elif op == "FLDL2E":
            if not hasattr(self, "_fstack"): self._fstack = []
            self._fstack.append(_math.log2(_math.e))
        elif op == "FLDLN2":
            if not hasattr(self, "_fstack"): self._fstack = []
            self._fstack.append(_math.log(2))
        elif op == "FLDZ":
            if not hasattr(self, "_fstack"): self._fstack = []
            self._fstack.append(0.0)
        elif op == "FLD1":
            if not hasattr(self, "_fstack"): self._fstack = []
            self._fstack.append(1.0)
        elif op == "FST":
            if hasattr(self, "_fstack") and self._fstack:
                self._reg_set(a[0], int(self._fstack[-1]))
        elif op == "FSTP":
            if hasattr(self, "_fstack") and self._fstack:
                self._reg_set(a[0], int(self._fstack.pop()))
        elif op == "FIST":
            if hasattr(self, "_fstack") and self._fstack:
                self._reg_set(a[0], int(self._fstack[-1]))
        elif op == "FISTP":
            if hasattr(self, "_fstack") and self._fstack:
                self._reg_set(a[0], int(self._fstack.pop()))
        elif op == "FADD":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                b, a2 = self._fstack.pop(), self._fstack.pop(); self._fstack.append(a2 + b)
        elif op == "FSUB":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                b, a2 = self._fstack.pop(), self._fstack.pop(); self._fstack.append(a2 - b)
        elif op == "FSUBR":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                b, a2 = self._fstack.pop(), self._fstack.pop(); self._fstack.append(b - a2)
        elif op == "FMUL":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                b, a2 = self._fstack.pop(), self._fstack.pop(); self._fstack.append(a2 * b)
        elif op == "FDIV":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                b, a2 = self._fstack.pop(), self._fstack.pop()
                self._fstack.append(a2 / b if b else float("nan"))
        elif op == "FDIVR":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                b, a2 = self._fstack.pop(), self._fstack.pop()
                self._fstack.append(b / a2 if a2 else float("nan"))
        elif op == "FSQRT":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(_math.sqrt(abs(self._fstack.pop())))
        elif op == "FSIN":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(_math.sin(self._fstack.pop()))
        elif op == "FCOS":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(_math.cos(self._fstack.pop()))
        elif op == "FTAN":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(_math.tan(self._fstack.pop()))
        elif op == "FSINCOS":
            if hasattr(self, "_fstack") and self._fstack:
                v = self._fstack.pop()
                self._fstack += [_math.sin(v), _math.cos(v)]
        elif op == "FPATAN":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                b, a2 = self._fstack.pop(), self._fstack.pop(); self._fstack.append(_math.atan2(a2, b))
        elif op == "FABS":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(abs(self._fstack.pop()))
        elif op == "FCHS":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(-self._fstack.pop())
        elif op == "FRNDINT":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(float(round(self._fstack.pop())))
        elif op == "FSCALE":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                exp, base = self._fstack.pop(), self._fstack.pop()
                self._fstack.append(base * (2 ** int(exp)))
        elif op == "F2XM1":
            if hasattr(self, "_fstack") and self._fstack:
                self._fstack.append(2 ** self._fstack.pop() - 1)
        elif op == "FYL2X":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                x, y = self._fstack.pop(), self._fstack.pop()
                self._fstack.append(y * _math.log2(x) if x > 0 else float("nan"))
        elif op == "FLDCW":   pass   # control-word stub
        elif op == "FSTCW":   pass
        elif op == "FNSTSW":  pass
        elif op == "FWAIT":   pass
        elif op == "FNOP":    pass
        elif op == "FXCH":    # swap top two float stack items
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                self._fstack[-1], self._fstack[-2] = self._fstack[-2], self._fstack[-1]
        elif op == "FCOMPP" or op == "FCOMP" or op == "FCOM":
            if hasattr(self, "_fstack") and len(self._fstack) >= 2:
                a2, b = self._fstack[-1], self._fstack[-2]
                self.flags["Z"] = a2 == b; self.flags["N"] = a2 < b
        elif op == "FPRINT":  # Time Warp extension
            if hasattr(self, "_fstack") and self._fstack:
                self._emit(str(self._fstack[-1]))

        # ── I/O ─────────────────────────────────────────────────────────────
        elif op in ("IN", "INB"):
            # Read from port (stub — push 0)
            if a: self._reg_set(a[0], 0)
        elif op in ("OUT", "OUTB"):
            # Write to port (stub — print)
            if len(a) >= 2: self._emit(f"PORT[{self._val(a[0])}]={self._val(a[1])}")
        elif op == "SYSCALL":
            # Simplified syscall: R0=syscall number
            sc = self.regs[0]
            if sc == 1:             # print integer in R1
                self._emit(str(self.regs[1]))
            elif sc == 2:           # read integer → R1
                raw = self.interpreter.request_input("asm> ") if hasattr(self.interpreter,"request_input") else "0"
                try: self.regs[1] = int(raw or "0")
                except: self.regs[1] = 0
            elif sc == 3:           # print char in R1
                self._emit(chr(self.regs[1] & 0xFF))
            elif sc == 4:           # halt
                raise HaltException()
        elif op == "SYSENTER": pass
        elif op == "SYSEXIT":  pass
        elif op == "CPUID":
            # Stub: set R0..R3 to fake CPU info
            self.regs[0] = 0x756E6547   # "Genu"
            self.regs[1] = 0x49656E69   # "ineI"
            self.regs[2] = 0x6C65746E   # "ntel"
            self.regs[3] = 1

        # ── Miscellaneous ───────────────────────────────────────────────────
        elif op == "BSWAP":
            # Byte swap 32-bit register
            v = self._reg_get(a[0]) & 0xFFFFFFFF
            r = ((v & 0xFF) << 24) | (((v >> 8) & 0xFF) << 16) | (((v >> 16) & 0xFF) << 8) | ((v >> 24) & 0xFF)
            self._reg_set(a[0], r)
        elif op == "NOOP":
            pass
        elif op in ("ENTER", "LEAVE"):
            pass   # stack-frame stubs
        elif op in ("INT", "INT3", "INTO"):
            if a and a[0] == "3": raise HaltException()  # INT3 = breakpoint
        elif op == "IRET":
            if self.call_stack: self.ip = self.call_stack.pop()
        elif op == "ALIGN":
            pass  # assembler directive
        elif op in ("DB", "DW", "DD", "DQ", "RES", "RESB", "RESW", "RESD"):
            pass  # data definition stubs (handled in _parse for named defs)
        elif op == "_DATA":
            pass  # placeholder for parsed data definitions
        elif op in ("SECTION", ".TEXT", ".DATA", ".BSS", "ORG", "BITS", "CPU", "USE32", "USE64"):
            pass  # assembler directives
        elif op in ("GLOBAL", "EXTERN", "EXPORT"):
            pass  # symbol visibility stubs
        elif op == "PRINTS":
            # Print string at data label: PRINTS msg
            lbl = a[0].upper() if a else ""
            if lbl in self.data_strings:
                self._emit(self.data_strings[lbl])
            elif lbl in self.data_labels:
                # Read from memory until null byte
                addr = self.data_labels[lbl]
                chars = []
                while addr < len(self.memory) and self.memory[addr] != 0:
                    chars.append(chr(self.memory[addr]))
                    addr += 1
                self._emit("".join(chars))
        elif op == "PRINTI":
            self._emit(str(self._val(a[0])))
        elif op == "PRINTR":
            self._emit(str(self._reg_get(a[0])))
        elif op == "NEWLINE":
            self._emit("")
        elif op == "DUMP":
            self._emit(f"Regs: {self.regs!r}")
        elif op == "DUMPF":
            if hasattr(self, "_fstack"): self._emit(f"FStack: {self._fstack!r}")
        else:
            raise AsmError(f"Unknown instruction: {op}")

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    # x86 named register mapping (EAX→R0, EBX→R1, ECX→R2, EDX→R3, etc.)
    _X86_REGS: dict[str, int] = {
        "EAX": 0, "AX": 0, "AL": 0, "AH": 0,
        "EBX": 1, "BX": 1, "BL": 1, "BH": 1,
        "ECX": 2, "CX": 2, "CL": 2, "CH": 2,
        "EDX": 3, "DX": 3, "DL": 3, "DH": 3,
        "ESP": 4, "SP": 4, "EBP": 5, "BP": 5,
        "ESI": 6, "SI": 6, "EDI": 7, "DI": 7,
        "RAX": 0, "RBX": 1, "RCX": 2, "RDX": 3,
        "RSP": 4, "RBP": 5, "RSI": 6, "RDI": 7,
    }

    def _reg_index(self, name: str) -> int:
        up = name.upper()
        if up in self._X86_REGS:
            return self._X86_REGS[up]
        m = re.match(r"^R(\d+)$", up)
        if not m:
            raise AsmError(f"Invalid register: {name}")
        idx = int(m.group(1))
        if not 0 <= idx <= 15:
            raise AsmError(f"Register out of range: {name}")
        return idx

    def _reg_get(self, name: str) -> int:
        return self.regs[self._reg_index(name)]

    def _reg_set(self, name: str, value: int):
        self.regs[self._reg_index(name)] = int(value)

    def _val(self, operand: str) -> int:
        operand = operand.strip()
        up = operand.upper()
        if re.match(r"^R\d+$", up):
            v = self._reg_get(operand)
            return int(v) if isinstance(v, float) else v
        # x86 named registers
        if up in self._X86_REGS:
            v = self._reg_get(operand)
            return int(v) if isinstance(v, float) else v
        if re.match(r"^0X[\dA-F]+$", operand, re.IGNORECASE):
            return int(operand, 16)
        if re.match(r"^0B[01]+$", operand, re.IGNORECASE):
            return int(operand[2:], 2)
        # Octal: 0o prefix
        if re.match(r"^0O[0-7]+$", operand, re.IGNORECASE):
            return int(operand, 8)
        try:
            return int(operand)
        except ValueError:
            pass
        try:
            return float(operand)  # float immediate (e.g. 3.14)
        except ValueError:
            pass
        # Resolve data labels to memory addresses
        up = operand.upper()
        if up in self.data_labels:
            return self.data_labels[up]
        # Resolve code labels to instruction addresses
        if up in self.labels:
            return self.labels[up]
        raise AsmError(f"Invalid immediate value: {operand}")

    def _mem_addr(self, operand: str) -> int:
        operand = operand.strip()
        # [Rx] or [123]
        m = re.match(r"^\[(.+)\]$", operand)
        if m:
            inner = m.group(1).strip()
            addr = self._val(inner)
        else:
            addr = self._val(operand)
        if not 0 <= addr < len(self.memory):
            raise AsmError(f"Memory address out of range: {addr}")
        return addr

    def _set_flags(self, value: int):
        v = int(value) if isinstance(value, float) else value
        self.flags["Z"] = v == 0
        self.flags["N"] = v < 0
        self.flags["S"] = v < 0                                    # sign flag
        self.flags["C"] = (v > 0xFFFFFFFF) or (v < -0x80000000)  # naive carry/borrow
        self.flags["O"] = (v > 0x7FFFFFFF) or (v < -0x80000000)  # overflow
        self.flags["P"] = bin(v & 0xFF).count("1") % 2 == 0       # parity even

    def _jump(self, label: str):
        label = label.upper()
        if label not in self.labels:
            raise AsmError(f"Undefined label: {label}")
        self.ip = self.labels[label]


class HaltException(Exception):
    pass


class AsmError(Exception):
    pass
