"""
Toy x86-style Assembly executor for Time Warp Studio.

Supports a simple register-based virtual machine with R0-R7 (plus EAX/EBX/
ECX/EDX/ESI/EDI aliases), a stack, flat memory, labels, conditional jumps,
subroutine CALL/RET, and PRINT/PRINTS/INPUT for I/O.
"""
from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Register name normalisation
# ---------------------------------------------------------------------------
_X86_ALIASES: Dict[str, str] = {
    "EAX": "R0", "AX": "R0", "AL": "R0",
    "EBX": "R1", "BX": "R1", "BL": "R1",
    "ECX": "R2", "CX": "R2", "CL": "R2",
    "EDX": "R3", "DX": "R3", "DL": "R3",
    "ESI": "R4", "SI": "R4",
    "EDI": "R5", "DI": "R5",
}
_REG_RE = re.compile(r"^R\d+$")


def _norm_reg(name: str) -> str:
    name = name.strip().upper()
    return _X86_ALIASES.get(name, name)


# ---------------------------------------------------------------------------
# Operand resolution
# ---------------------------------------------------------------------------

def _is_reg(token: str) -> bool:
    return _REG_RE.match(token.upper()) is not None or token.upper() in _X86_ALIASES


def _parse_int(s: str) -> Optional[int]:
    s = s.strip()
    try:
        if s.upper().endswith("H"):
            return int(s[:-1], 16)
        if s.startswith("0x") or s.startswith("0X"):
            return int(s, 16)
        return int(s)
    except ValueError:
        return None


class _VM:
    """Minimal register machine."""

    MAX_STEPS = 50_000

    def __init__(self, input_cb):
        self.regs: Dict[str, int] = {f"R{i}": 0 for i in range(8)}
        self.stack: List[Any] = []
        self.ret_stack: List[int] = []
        self.memory: Dict[int, Any] = {}
        self.data: Dict[str, Any] = {}  # data labels from .data section
        self.flags: Dict[str, bool] = {"Z": False, "G": False, "L": False}
        self.output: List[str] = []
        self.input_cb = input_cb

    # ------------------------------------------------------------------
    def get(self, token: str) -> Any:
        token = token.strip()
        upper = token.upper()
        r = _norm_reg(upper)
        if r in self.regs:
            return self.regs[r]
        # string literal
        if (token.startswith('"') and token.endswith('"')) or \
           (token.startswith("'") and token.endswith("'")):
            return token[1:-1]
        # data label
        if token in self.data:
            return self.data[token]
        # numeric
        v = _parse_int(token)
        if v is not None:
            return v
        return token

    def set(self, reg: str, value: Any) -> None:
        r = _norm_reg(reg.upper())
        if r in self.regs:
            self.regs[r] = value if isinstance(value, int) else value

    def run(self, instructions: List[tuple]) -> None:
        """instructions: list of (lineno, label_or_None, opcode, [args])"""
        # Build label -> index map
        label_map: Dict[str, int] = {}
        for idx, (_, label, _op, _args) in enumerate(instructions):
            if label:
                label_map[label.upper()] = idx

        ip = 0
        steps = 0
        while 0 <= ip < len(instructions) and steps < self.MAX_STEPS:
            steps += 1
            _lineno, _label, op, args = instructions[ip]
            ip = self._exec(op, args, ip, label_map)

    # ------------------------------------------------------------------
    def _exec(self, op: str, args: List[str], ip: int, label_map: Dict[str, int]) -> int:
        op = op.upper()
        next_ip = ip + 1

        if op in ("", "NOP"):
            pass

        elif op == "MOV":
            if len(args) >= 2:
                self.set(args[0], self.get(args[1]))

        elif op == "PUSH":
            self.stack.append(self.get(args[0]))

        elif op == "POP":
            if self.stack:
                self.set(args[0], self.stack.pop())

        elif op == "ADD":
            a, b = self.get(args[0]), self.get(args[1])
            result = (a if isinstance(a, int) else 0) + (b if isinstance(b, int) else 0)
            self.set(args[0], result)

        elif op == "SUB":
            a, b = self.get(args[0]), self.get(args[1])
            result = (a if isinstance(a, int) else 0) - (b if isinstance(b, int) else 0)
            self.set(args[0], result)

        elif op == "MUL":
            a, b = self.get(args[0]), self.get(args[1])
            result = (a if isinstance(a, int) else 0) * (b if isinstance(b, int) else 0)
            self.set(args[0], result)

        elif op == "DIV":
            a, b = self.get(args[0]), self.get(args[1])
            bi = b if isinstance(b, int) else 0
            if bi == 0:
                self.output.append("❌ Division by zero")
            else:
                self.set(args[0], (a if isinstance(a, int) else 0) // bi)

        elif op == "AND":
            a, b = self.get(args[0]), self.get(args[1])
            self.set(args[0], (a if isinstance(a, int) else 0) & (b if isinstance(b, int) else 0))

        elif op == "OR":
            a, b = self.get(args[0]), self.get(args[1])
            self.set(args[0], (a if isinstance(a, int) else 0) | (b if isinstance(b, int) else 0))

        elif op == "XOR":
            a, b = self.get(args[0]), self.get(args[1])
            self.set(args[0], (a if isinstance(a, int) else 0) ^ (b if isinstance(b, int) else 0))

        elif op == "NOT":
            a = self.get(args[0])
            self.set(args[0], ~(a if isinstance(a, int) else 0) & 0xFFFFFFFF)

        elif op == "SHL":
            a, b = self.get(args[0]), self.get(args[1])
            self.set(args[0], (a if isinstance(a, int) else 0) << (b if isinstance(b, int) else 0))

        elif op == "SHR":
            a, b = self.get(args[0]), self.get(args[1])
            self.set(args[0], (a if isinstance(a, int) else 0) >> max(0, b if isinstance(b, int) else 0))

        elif op == "CMP":
            a = self.get(args[0]) if args else 0
            b = self.get(args[1]) if len(args) > 1 else 0
            ai = a if isinstance(a, int) else 0
            bi = b if isinstance(b, int) else 0
            self.flags["Z"] = (ai == bi)
            self.flags["G"] = (ai > bi)
            self.flags["L"] = (ai < bi)

        elif op == "JMP":
            target = args[0].upper() if args else ""
            if target in label_map:
                return label_map[target]

        elif op == "JE":
            target = args[0].upper() if args else ""
            if self.flags.get("Z") and target in label_map:
                return label_map[target]

        elif op == "JNE":
            target = args[0].upper() if args else ""
            if not self.flags.get("Z") and target in label_map:
                return label_map[target]

        elif op == "JG":
            target = args[0].upper() if args else ""
            if self.flags.get("G") and target in label_map:
                return label_map[target]

        elif op == "JL":
            target = args[0].upper() if args else ""
            if self.flags.get("L") and target in label_map:
                return label_map[target]

        elif op == "JGE":
            target = args[0].upper() if args else ""
            if (self.flags.get("G") or self.flags.get("Z")) and target in label_map:
                return label_map[target]

        elif op == "JLE":
            target = args[0].upper() if args else ""
            if (self.flags.get("L") or self.flags.get("Z")) and target in label_map:
                return label_map[target]

        elif op == "CALL":
            target = args[0].upper() if args else ""
            if target in label_map:
                self.ret_stack.append(ip + 1)
                return label_map[target]

        elif op == "RET":
            if self.ret_stack:
                return self.ret_stack.pop()

        elif op == "HALT":
            return len(label_map) + 999999  # signal stop

        elif op == "PRINT":
            val = self.get(args[0]) if args else ""
            self.output.append(str(val))

        elif op == "PRINTS":
            # Print a data label value
            label_name = args[0] if args else ""
            val = self.data.get(label_name, self.data.get(label_name.upper(), ""))
            self.output.append(str(val))

        elif op == "INPUT":
            reg = args[0] if args else "R0"
            raw = self.input_cb("? ") if self.input_cb else "0"
            v = _parse_int(raw)
            self.set(reg, v if v is not None else raw)

        elif op in ("STORE", "STR"):
            # STORE [addr], reg
            if len(args) >= 2:
                addr_tok = args[0].strip("[]")
                addr = _parse_int(addr_tok) or 0
                self.memory[addr] = self.get(args[1])

        elif op in ("LOAD", "LDR"):
            # LOAD reg, [addr]
            if len(args) >= 2:
                addr_tok = args[1].strip("[]")
                addr = _parse_int(addr_tok) or 0
                self.set(args[0], self.memory.get(addr, 0))

        else:
            self.output.append(f"❌ Unknown instruction: {op}")

        return next_ip


# ---------------------------------------------------------------------------
# Parser
# ---------------------------------------------------------------------------

def _parse_program(source: str, vm: _VM):
    """Parse source into instruction tuples, populating vm.data as needed."""
    instructions: List[tuple] = []
    in_data = False

    for raw_line in source.splitlines():
        # Strip comments
        semicolon = raw_line.find(";")
        if semicolon != -1:
            raw_line = raw_line[:semicolon]
        line = raw_line.strip()
        if not line:
            continue

        # Section directives
        if re.match(r"^section\s+\.(data|text|bss)$", line, re.IGNORECASE):
            in_data = "data" in line.lower()
            continue

        # Data directives: label DB "string",0
        db_match = re.match(
            r"^(\w+)\s+D[BWQ]\s+['\"](.+?)['\"],?\s*\d*$", line, re.IGNORECASE
        )
        if db_match:
            vm.data[db_match.group(1)] = db_match.group(2)
            if in_data:
                continue

        # Label detection (ends with colon)
        label: Optional[str] = None
        if ":" in line:
            parts = line.split(":", 1)
            candidate = parts[0].strip()
            if re.match(r"^\w+$", candidate):
                label = candidate.upper()
                line = parts[1].strip()

        if not line:
            instructions.append((0, label, "", []))
            continue

        # Tokenise — respect quoted strings
        tokens = re.findall(r'"[^"]*"|\'[^\']*\'|\S+', line)
        opcode = tokens[0].upper() if tokens else ""
        # Normalise operands: split on commas while keeping quoted strings
        raw_args = " ".join(tokens[1:]) if len(tokens) > 1 else ""
        # Split on comma outside quotes
        arg_parts: List[str] = []
        current = ""
        in_quote = False
        q_char = ""
        for ch in raw_args:
            if ch in ('"', "'") and not in_quote:
                in_quote = True
                q_char = ch
                current += ch
            elif ch == q_char and in_quote:
                in_quote = False
                current += ch
            elif ch == "," and not in_quote:
                arg_parts.append(current.strip())
                current = ""
            else:
                current += ch
        if current.strip():
            arg_parts.append(current.strip())

        instructions.append((0, label, opcode, arg_parts))

    return instructions


# ---------------------------------------------------------------------------
# Public executor
# ---------------------------------------------------------------------------

def execute_assembly(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a toy Assembly program and return output text."""
    if not source.strip():
        return ""

    vm = _VM(input_cb=getattr(interpreter, "input_callback", None))
    instructions = _parse_program(source, vm)
    try:
        vm.run(instructions)
    except Exception as exc:  # noqa: BLE001
        vm.output.append(f"❌ Runtime error: {exc}")

    return "\n".join(vm.output) + ("\n" if vm.output else "")
