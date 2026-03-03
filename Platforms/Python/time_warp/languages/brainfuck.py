"""Brainfuck language executor for Time Warp Studio.

Educational Brainfuck interpreter — whole-program execution.
Classic 8-command esoteric language, excellent for teaching Turing completeness.

Commands:
  >  Move data pointer right
  <  Move data pointer left
  +  Increment byte at pointer
  -  Decrement byte at pointer
  .  Output byte at pointer as ASCII char
  ,  Input one byte (reads from interpreter.request_input)
  [  Jump forward past matching ] if byte at pointer is 0
  ]  Jump backward to matching [ if byte at pointer is not 0
"""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..core.turtle_state import TurtleState

TAPE_SIZE = 30_000
MAX_STEPS = 10_000_000


def execute_brainfuck(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a complete Brainfuck program and return all output."""
    # Strip all non-Brainfuck characters
    code = "".join(c for c in source if c in "><+-.,[]")
    if not code:
        return "ℹ️ Empty program"

    # Pre-compute bracket pairs for O(1) jumps
    brackets: dict[int, int] = {}
    stack: list[int] = []
    for ip, ch in enumerate(code):
        if ch == "[":
            stack.append(ip)
        elif ch == "]":
            if not stack:
                return f"❌ Brainfuck: unmatched ']' at position {ip}"
            open_pos = stack.pop()
            brackets[open_pos] = ip
            brackets[ip] = open_pos
    if stack:
        return f"❌ Brainfuck: unmatched '[' at position {stack[-1]}"

    tape = bytearray(TAPE_SIZE)
    dp = 0          # data pointer
    ip = 0          # instruction pointer
    output_chars: list[str] = []
    steps = 0
    input_buffer: list[int] = []

    while ip < len(code):
        steps += 1
        if steps > MAX_STEPS:
            output_chars.append(f"\n❌ Brainfuck: execution limit ({MAX_STEPS} steps) exceeded")
            break

        ch = code[ip]

        if ch == ">":
            dp = (dp + 1) % TAPE_SIZE
        elif ch == "<":
            dp = (dp - 1) % TAPE_SIZE
        elif ch == "+":
            tape[dp] = (tape[dp] + 1) % 256
        elif ch == "-":
            tape[dp] = (tape[dp] - 1) % 256
        elif ch == ".":
            output_chars.append(chr(tape[dp]))
        elif ch == ",":
            if not input_buffer:
                raw = ""
                if hasattr(interpreter, "request_input"):
                    raw = interpreter.request_input("bf> ") or ""
                raw += "\n"
                input_buffer = [ord(c) for c in raw]
            tape[dp] = input_buffer.pop(0) if input_buffer else 0
        elif ch == "[":
            if tape[dp] == 0:
                ip = brackets[ip]
        elif ch == "]":
            if tape[dp] != 0:
                ip = brackets[ip]

        ip += 1

    return "".join(output_chars)
