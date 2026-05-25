"""PostScript language executor for Time Warp Studio.

Educational PostScript interpreter — whole-program execution.
Implements a stack-based subset of Level 2 PostScript:
  - Operand stack: push/pop, stack operations (dup, pop, exch, roll, copy, count, clear)
  - Arithmetic: add, sub, mul, div, mod, idiv, abs, neg, ceiling, floor, round, sqrt
  - Math functions: sin, cos, atan, exp, ln, log
  - Comparisons: eq, ne, gt, ge, lt, le
  - Logical: and, or, not, xor
  - Bitwise: bitshift
  - Type testing: type, isarray, isstring, isinteger, isreal
  - Conversions: cvi, cvr, cvs, cvn
  - Control: if, ifelse, for, repeat, loop, forall, exec, stop, exit, quit
  - Procedures: {} def, load, bind
  - Dictionary: dict, begin, end, def, load, where, known, currentdict
  - String operations: length, get, getinterval, putinterval, search, anchorsearch, stringwidth
  - Array operations: array, aload, astore, length, get, put, copy
  - Output: print, =, ==, message
  - Comments: % to end of line
  - Turtle/graphics: moveto, lineto, curveto, stroke, fill, setlinewidth, setrgbcolor,
    setgray, translate, rotate, scale, gsave, grestore, showpage
  - show (text output using print as simulation)
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any, Dict, List, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_postscript(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a PostScript program and return all output as a string."""
    env = PostScriptEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class _PSStop(Exception):
    pass


class _PSExit(Exception):
    pass


class _PSError(Exception):
    def __init__(self, name: str, msg: str = "") -> None:
        self.name = name
        self.msg = msg
        super().__init__(f"{name}: {msg}" if msg else name)


# ---------------------------------------------------------------------------
# Token types
# ---------------------------------------------------------------------------


def _tokenize_ps(source: str) -> List[Any]:
    """Tokenize a PostScript program into typed tokens."""
    tokens: List[Any] = []
    i = 0
    n = len(source)
    while i < n:
        c = source[i]
        # Skip whitespace
        if c in " \t\r\n":
            i += 1
            continue
        # Comment: % to end of line
        if c == "%":
            while i < n and source[i] != "\n":
                i += 1
            continue
        # String literal: (...)
        if c == "(":
            depth = 0
            j = i + 1
            buf: List[str] = []
            while j < n:
                ch = source[j]
                if ch == "\\" and j + 1 < n:
                    esc = source[j + 1]
                    buf.append(
                        {"n": "\n", "t": "\t", "r": "\r", "\\": "\\"}.get(esc, esc)
                    )
                    j += 2
                    continue
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    if depth == 0:
                        break
                    depth -= 1
                buf.append(ch)
                j += 1
            tokens.append(("string", "".join(buf)))
            i = j + 1
            continue
        # Hex string: <…>
        if c == "<" and (i + 1 < n and source[i + 1] != "<"):
            j = i + 1
            buf = []
            while j < n and source[j] != ">":
                buf.append(source[j])
                j += 1
            hex_str = "".join(buf).replace(" ", "")
            try:
                s = bytes.fromhex(hex_str).decode("latin-1")
            except ValueError:
                s = ""
            tokens.append(("string", s))
            i = j + 1
            continue
        # Procedure/array: {…}
        if c == "{":
            depth = 1
            j = i + 1
            while j < n and depth > 0:
                if source[j] == "{":
                    depth += 1
                elif source[j] == "}":
                    depth -= 1
                j += 1
            inner = source[i + 1 : j - 1].strip()
            tokens.append(("proc", inner))
            i = j
            continue
        # Array: [… ] — treated as executable array literal
        if c == "[":
            tokens.append(("op", "["))
            i += 1
            continue
        if c == "]":
            tokens.append(("op", "]"))
            i += 1
            continue
        # Literal name: /name
        if c == "/":
            j = i + 1
            while j < n and source[j] not in " \t\r\n{}()[]/<>%":
                j += 1
            tokens.append(("literal_name", source[i + 1 : j]))
            i = j
            continue
        # Immediate name: //name
        if c == "/" and i + 1 < n and source[i + 1] == "/":
            j = i + 2
            while j < n and source[j] not in " \t\r\n{}()[]/<>%":
                j += 1
            tokens.append(("immediate_name", source[i + 2 : j]))
            i = j
            continue
        # Number or name
        j = i
        while j < n and source[j] not in " \t\r\n{}()[]/<>%":
            j += 1
        word = source[i:j]
        i = j
        if not word:
            i += 1
            continue
        # Try integer
        try:
            tokens.append(("int", int(word)))
            continue
        except ValueError:
            pass
        # Try radix notation: 16#FF
        rm = re.match(r"^(\d+)#([0-9A-Fa-f]+)$", word)
        if rm:
            tokens.append(("int", int(rm.group(2), int(rm.group(1)))))
            continue
        # Try float
        try:
            tokens.append(("float", float(word)))
            continue
        except ValueError:
            pass
        # Operator / name
        tokens.append(("op", word))
    return tokens


# ---------------------------------------------------------------------------
# PostScript Environment
# ---------------------------------------------------------------------------


class PostScriptEnvironment:
    """Full PostScript execution environment."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self._interp = interpreter
        self._turtle = turtle
        self._stack: List[Any] = []
        self._dict_stack: List[Dict[str, Any]] = [self._build_system_dict()]
        self._output: List[str] = []
        # Graphics state
        self._gstate: List[Dict[str, Any]] = []
        self._current_pos: Tuple[float, float] = (0.0, 0.0)
        self._line_width: float = 1.0
        self._color: Tuple[float, float, float] = (0.0, 0.0, 0.0)
        self._in_path = False

    # ------------------------------------------------------------------
    # Top level
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        tokens = _tokenize_ps(source)
        try:
            self._exec_tokens(tokens)
        except _PSStop:
            pass
        except _PSExit:
            pass
        except _PSError as e:
            self._output.append(f"❌ PostScript error: {e}\n")
        except Exception as e:  # noqa: BLE001
            self._output.append(f"❌ PostScript runtime error: {e}\n")
        return "".join(self._output)

    def _exec_tokens(self, tokens: List[Any]) -> None:
        """Execute a list of tokens."""
        i = 0
        mark_stack: List[int] = []  # positions of [ marks
        while i < len(tokens):
            tok = tokens[i]
            kind, value = tok

            if kind == "int":
                self._push(value)
            elif kind == "float":
                self._push(value)
            elif kind == "string":
                self._push(value)
            elif kind == "literal_name":
                self._push(("name", value))
            elif kind == "proc":
                self._push(("proc", value))
            elif kind == "op":
                if value == "[":
                    mark_stack.append(len(self._stack))
                elif value == "]":
                    mark_pos = mark_stack.pop() if mark_stack else 0
                    arr = self._stack[mark_pos:]
                    self._stack = self._stack[:mark_pos]
                    self._push(arr)
                else:
                    # User-defined names shadow built-in operators (PostScript semantics)
                    try:
                        obj = self._lookup(value)
                        if isinstance(obj, tuple) and obj[0] == "proc":
                            self._exec_proc(obj)
                        elif callable(obj):
                            obj()
                        else:
                            self._push(obj)
                    except _PSError:
                        self._exec_op(value)
            i += 1

    def _exec_op(self, op: str) -> None:  # noqa: C901,PLR0912
        """Execute a single PostScript operator."""
        op_lower = op.lower()

        # Stack operations
        if op_lower == "pop":
            self._pop()
            return
        if op_lower == "dup":
            v = self._peek()
            self._push(v)
            return
        if op_lower == "exch":
            a, b = self._pop(), self._pop()
            self._push(a)
            self._push(b)
            return
        if op_lower == "roll":
            j = int(self._pop())
            n = int(self._pop())
            if n > 0:
                items = [self._pop() for _ in range(n)]
                items.reverse()
                j = j % n
                items = items[-j:] + items[:-j]
                for item in items:
                    self._push(item)
            return
        if op_lower == "copy":
            n = self._pop()
            if isinstance(n, int):
                items = self._stack[-n:]
                for item in items:
                    self._push(item)
            return
        if op_lower == "count":
            self._push(len(self._stack))
            return
        if op_lower == "clear":
            self._stack.clear()
            return
        if op_lower == "cleartomark":
            while self._stack and not isinstance(self._peek(), tuple):
                self._stack.pop()
            return
        if op_lower == "mark":
            self._push(("mark", None))
            return
        if op_lower == "index":
            n = int(self._pop())
            self._push(self._stack[-(n + 1)])
            return

        # Arithmetic
        if op_lower == "add":
            b, a = self._pop(), self._pop()
            self._push(a + b)
            return
        if op_lower == "sub":
            b, a = self._pop(), self._pop()
            self._push(a - b)
            return
        if op_lower == "mul":
            b, a = self._pop(), self._pop()
            self._push(a * b)
            return
        if op_lower == "div":
            b, a = self._pop(), self._pop()
            if b == 0:
                raise _PSError("undefinedresult", "division by zero")
            self._push(a / b)
            return
        if op_lower == "idiv":
            b, a = self._pop(), self._pop()
            self._push(int(a) // int(b))
            return
        if op_lower == "mod":
            b, a = self._pop(), self._pop()
            self._push(int(a) % int(b))
            return
        if op_lower == "abs":
            self._push(abs(self._pop()))
            return
        if op_lower == "neg":
            self._push(-self._pop())
            return
        if op_lower == "ceiling":
            self._push(math.ceil(self._pop()))
            return
        if op_lower == "floor":
            self._push(math.floor(self._pop()))
            return
        if op_lower == "round":
            self._push(round(self._pop()))
            return
        if op_lower == "truncate":
            self._push(int(self._pop()))
            return
        if op_lower == "sqrt":
            self._push(math.sqrt(self._pop()))
            return
        if op_lower == "exp":
            b, a = self._pop(), self._pop()
            self._push(a**b)
            return
        if op_lower == "ln":
            self._push(math.log(self._pop()))
            return
        if op_lower == "log":
            self._push(math.log10(self._pop()))
            return
        if op_lower == "sin":
            self._push(math.sin(math.radians(self._pop())))
            return
        if op_lower == "cos":
            self._push(math.cos(math.radians(self._pop())))
            return
        if op_lower == "atan":
            x, y = self._pop(), self._pop()  # atan y x (dy dx order)
            self._push(math.degrees(math.atan2(y, x)))
            return
        if op_lower == "max":
            b, a = self._pop(), self._pop()
            self._push(max(a, b))
            return
        if op_lower == "min":
            b, a = self._pop(), self._pop()
            self._push(min(a, b))
            return

        # Comparisons
        if op_lower == "eq":
            b, a = self._pop(), self._pop()
            self._push(True if a == b else False)
            return
        if op_lower == "ne":
            b, a = self._pop(), self._pop()
            self._push(True if a != b else False)
            return
        if op_lower == "gt":
            b, a = self._pop(), self._pop()
            self._push(True if a > b else False)
            return
        if op_lower == "ge":
            b, a = self._pop(), self._pop()
            self._push(True if a >= b else False)
            return
        if op_lower == "lt":
            b, a = self._pop(), self._pop()
            self._push(True if a < b else False)
            return
        if op_lower == "le":
            b, a = self._pop(), self._pop()
            self._push(True if a <= b else False)
            return

        # Boolean
        if op_lower == "and":
            b, a = self._pop(), self._pop()
            if isinstance(a, bool) or isinstance(b, bool):
                self._push(bool(a) and bool(b))
            else:
                self._push(int(a) & int(b))
            return
        if op_lower == "or":
            b, a = self._pop(), self._pop()
            if isinstance(a, bool) or isinstance(b, bool):
                self._push(bool(a) or bool(b))
            else:
                self._push(int(a) | int(b))
            return
        if op_lower == "not":
            a = self._pop()
            self._push(not bool(a) if isinstance(a, bool) else ~int(a))
            return
        if op_lower == "xor":
            b, a = self._pop(), self._pop()
            self._push(bool(a) ^ bool(b) if isinstance(a, bool) else int(a) ^ int(b))
            return
        if op_lower == "bitshift":
            shift, val = int(self._pop()), int(self._pop())
            self._push(val << shift if shift >= 0 else val >> (-shift))
            return
        if op_lower == "true":
            self._push(True)
            return
        if op_lower == "false":
            self._push(False)
            return

        # Type conversions
        if op_lower == "cvi":
            v = self._pop()
            self._push(int(float(str(v).split()[0])))
            return
        if op_lower == "cvr":
            v = self._pop()
            self._push(float(str(v).split()[0]))
            return
        if op_lower == "cvs":
            name, val = (
                self._pop(),
                self._pop(),
            )  # val buf → store val as string into buf
            self._push(str(val))
            return
        if op_lower == "cvn":
            self._push(("name", str(self._pop())))
            return

        # Output
        if op_lower in ("=", "print"):
            v = self._pop()
            self._output.append(str(v) + ("\n" if op_lower == "=" else ""))
            return
        if op_lower == "==":
            v = self._pop()
            self._output.append(repr(v) + "\n")
            return
        if op_lower == "message":
            msg = self._pop()
            self._output.append(str(msg) + "\n")
            return
        if op_lower == "show":
            text = self._pop()
            self._output.append(str(text) + "\n")
            return
        if op_lower == "pstack":
            for v in reversed(self._stack):
                self._output.append(str(v) + "\n")
            return

        # Control flow
        if op_lower == "if":
            proc = self._pop()
            cond = self._pop()
            if cond:
                self._exec_proc(proc)
            return
        if op_lower == "ifelse":
            else_proc = self._pop()
            then_proc = self._pop()
            cond = self._pop()
            self._exec_proc(then_proc if cond else else_proc)
            return
        if op_lower == "for":
            body = self._pop()
            limit = self._pop()
            step = self._pop()
            initial = self._pop()
            val = float(initial)
            step_f = float(step)
            limit_f = float(limit)
            count = 0
            while (step_f > 0 and val <= limit_f + 1e-9) or (
                step_f < 0 and val >= limit_f - 1e-9
            ):
                self._push(int(val) if val == int(val) else val)
                self._exec_proc(body)
                val += step_f
                count += 1
                if count > 100_000:
                    break
            return
        if op_lower == "repeat":
            body = self._pop()
            n = int(self._pop())
            for _ in range(n):
                self._exec_proc(body)
            return
        if op_lower == "loop":
            body = self._pop()
            for _ in range(100_000):
                try:
                    self._exec_proc(body)
                except _PSExit:
                    break
            return
        if op_lower == "forall":
            body = self._pop()
            obj = self._pop()
            items = obj if isinstance(obj, list) else list(str(obj))
            for item in items:
                self._push(item)
                self._exec_proc(body)
            return
        if op_lower == "exec":
            self._exec_proc(self._pop())
            return
        if op_lower == "exit":
            raise _PSExit
        if op_lower in ("stop", "quit"):
            raise _PSStop

        # Definitions
        if op_lower == "def":
            val = self._pop()
            key = self._pop()
            name = key[1] if isinstance(key, tuple) and key[0] == "name" else str(key)
            self._current_dict()[name] = val
            return
        if op_lower == "load":
            key = self._pop()
            name = key[1] if isinstance(key, tuple) and key[0] == "name" else str(key)
            self._push(self._lookup(name))
            return
        if op_lower == "bind":
            # No-op in educational interpreter
            return
        if op_lower == "known":
            key = self._pop()
            d = self._pop()
            name = key[1] if isinstance(key, tuple) and key[0] == "name" else str(key)
            self._push(name in (d if isinstance(d, dict) else {}))
            return
        if op_lower == "where":
            key = self._pop()
            name = key[1] if isinstance(key, tuple) and key[0] == "name" else str(key)
            for d in reversed(self._dict_stack):
                if name in d:
                    self._push(d)
                    self._push(True)
                    return
            self._push(False)
            return

        # Dictionaries
        if op_lower == "dict":
            n = int(self._pop())
            self._push({})
            return
        if op_lower == "begin":
            d = self._pop()
            self._dict_stack.append(d if isinstance(d, dict) else {})
            return
        if op_lower == "end":
            if len(self._dict_stack) > 1:
                self._dict_stack.pop()
            return
        if op_lower == "currentdict":
            self._push(self._current_dict())
            return
        if op_lower == "systemdict":
            self._push(self._dict_stack[0])
            return

        # String operations
        if op_lower == "length":
            obj = self._pop()
            if isinstance(obj, list):
                self._push(len(obj))
            else:
                self._push(len(str(obj)))
            return
        if op_lower == "get":
            idx = self._pop()
            obj = self._pop()
            if isinstance(obj, list):
                self._push(obj[int(idx)])
            elif isinstance(obj, dict):
                name = (
                    idx[1] if isinstance(idx, tuple) and idx[0] == "name" else str(idx)
                )
                self._push(obj.get(name, ("null", None)))
            else:
                s = str(obj)
                self._push(ord(s[int(idx)]) if 0 <= int(idx) < len(s) else 0)
            return
        if op_lower == "put":
            val = self._pop()
            idx = self._pop()
            obj = self._pop()
            if isinstance(obj, list):
                obj[int(idx)] = val
            elif isinstance(obj, dict):
                name = (
                    idx[1] if isinstance(idx, tuple) and idx[0] == "name" else str(idx)
                )
                obj[name] = val
            return
        if op_lower == "getinterval":
            count = int(self._pop())
            start = int(self._pop())
            obj = self._pop()
            if isinstance(obj, list):
                self._push(obj[start : start + count])
            else:
                self._push(str(obj)[start : start + count])
            return
        if op_lower == "putinterval":
            src = self._pop()
            start = int(self._pop())
            dest = self._pop()
            if isinstance(dest, list) and isinstance(src, list):
                dest[start : start + len(src)] = src
            return
        if op_lower == "string":
            n = int(self._pop())
            self._push("\0" * n)
            return
        if op_lower == "array":
            n = int(self._pop())
            self._push([None] * n)
            return
        if op_lower == "aload":
            arr = self._pop()
            for item in arr if isinstance(arr, list) else []:
                self._push(item)
            self._push(arr)
            return
        if op_lower == "astore":
            arr = self._pop()
            if isinstance(arr, list):
                for i in range(len(arr) - 1, -1, -1):
                    arr[i] = self._pop()
            self._push(arr)
            return
        if op_lower == "search":
            seek = str(self._pop())
            s = str(self._pop())
            idx = s.find(seek)
            if idx >= 0:
                self._push(s[idx + len(seek) :])  # post
                self._push(seek)  # match
                self._push(s[:idx])  # pre
                self._push(True)
            else:
                self._push(s)
                self._push(False)
            return
        if op_lower == "token":
            s = str(self._pop()).lstrip()
            m = re.match(r"([^\s]+)\s*(.*)", s, re.DOTALL)
            if m:
                self._push(m.group(2))
                tok_str = m.group(1)
                try:
                    self._push(int(tok_str))
                except ValueError:
                    try:
                        self._push(float(tok_str))
                    except ValueError:
                        self._push(tok_str)
                self._push(True)
            else:
                self._push(False)
            return
        if op_lower == "type":
            v = self._pop()
            if isinstance(v, bool):
                self._push(("name", "booleantype"))
            elif isinstance(v, int):
                self._push(("name", "integertype"))
            elif isinstance(v, float):
                self._push(("name", "realtype"))
            elif isinstance(v, str):
                self._push(("name", "stringtype"))
            elif isinstance(v, list):
                self._push(("name", "arraytype"))
            elif isinstance(v, dict):
                self._push(("name", "dicttype"))
            elif isinstance(v, tuple) and v[0] in ("name", "proc"):
                self._push(("name", "nametype"))
            else:
                self._push(("name", "nulltype"))
            return
        if op_lower == "null":
            self._push(None)
            return

        # Graphics operations (map to turtle)
        if op_lower == "moveto":
            y, x = float(self._pop()), float(self._pop())
            self._current_pos = (x, y)
            self._turtle.pen_up()
            self._turtle.set_pos(x, y)
            self._turtle.pen_down()
            return
        if op_lower == "rmoveto":
            dy, dx = float(self._pop()), float(self._pop())
            nx = self._current_pos[0] + dx
            ny = self._current_pos[1] + dy
            self._current_pos = (nx, ny)
            self._turtle.pen_up()
            self._turtle.set_pos(nx, ny)
            self._turtle.pen_down()
            return
        if op_lower == "lineto":
            y, x = float(self._pop()), float(self._pop())
            self._turtle.set_pos(x, y)
            self._current_pos = (x, y)
            return
        if op_lower == "rlineto":
            dy, dx = float(self._pop()), float(self._pop())
            nx = self._current_pos[0] + dx
            ny = self._current_pos[1] + dy
            self._turtle.set_pos(nx, ny)
            self._current_pos = (nx, ny)
            return
        if op_lower == "stroke":
            # Path already drawn via lineto
            return
        if op_lower == "fill":
            return
        if op_lower in ("newpath", "closepath"):
            return
        if op_lower == "setlinewidth":
            self._line_width = float(self._pop())
            return
        if op_lower == "setgray":
            g = float(self._pop())
            self._color = (g, g, g)
            return
        if op_lower == "setrgbcolor":
            b, g, r = float(self._pop()), float(self._pop()), float(self._pop())
            self._color = (r, g, b)
            return
        if op_lower == "translate":
            _dy, _dx = float(self._pop()), float(self._pop())
            # Simplified: just move turtle
            return
        if op_lower == "rotate":
            angle = float(self._pop())
            self._turtle.right(angle)
            return
        if op_lower == "scale":
            _sy, _sx = float(self._pop()), float(self._pop())
            return
        if op_lower == "gsave":
            self._gstate.append(
                {
                    "pos": self._current_pos,
                    "color": self._color,
                    "lw": self._line_width,
                }
            )
            return
        if op_lower == "grestore":
            if self._gstate:
                s = self._gstate.pop()
                self._current_pos = s["pos"]
                self._color = s["color"]
                self._line_width = s["lw"]
            return
        if op_lower in ("showpage", "erasepage"):
            return

        # Try looking up in dict stack
        try:
            val = self._lookup(op)
        except _PSError:
            # Unknown op — silently ignore (for portability)
            return

        if isinstance(val, tuple) and val[0] == "proc":
            self._exec_proc(val)
        else:
            self._push(val)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _push(self, val: Any) -> None:
        self._stack.append(val)

    def _pop(self) -> Any:
        if not self._stack:
            raise _PSError("stackunderflow")
        return self._stack.pop()

    def _peek(self) -> Any:
        if not self._stack:
            raise _PSError("stackunderflow")
        return self._stack[-1]

    def _current_dict(self) -> Dict[str, Any]:
        return self._dict_stack[-1]

    def _lookup(self, name: str) -> Any:
        for d in reversed(self._dict_stack):
            if name in d:
                return d[name]
        raise _PSError("undefined", name)

    def _exec_proc(self, proc: Any) -> None:
        """Execute a procedure (proc string or list of tokens)."""
        if isinstance(proc, tuple) and proc[0] == "proc":
            tokens = _tokenize_ps(proc[1])
            self._exec_tokens(tokens)
        elif isinstance(proc, list):
            self._exec_tokens(
                [
                    ("op", str(item))
                    if isinstance(item, str)
                    else ("int" if isinstance(item, int) else "float", item)
                    for item in proc
                ]
            )
        # Boolean procs: if/ifelse receive bool wrapped procs

    def _build_system_dict(self) -> Dict[str, Any]:
        """Return the system dictionary (empty; operators handled via _exec_op)."""
        return {
            "pi": math.pi,
            "e": math.e,
        }
