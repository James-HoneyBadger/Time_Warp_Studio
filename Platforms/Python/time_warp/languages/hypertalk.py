"""HyperTalk language executor for Time Warp Studio.

Educational HyperTalk interpreter — whole-program execution.
Inspired by Apple HyperCard's HyperTalk scripting language.

Supports:
  put value into var / put value after var / put value before var
  get expression (stores in "it")
  set property to value
  if expr then ... [else ...] end if
  repeat n times / repeat with var = i to n / repeat while/until
  end repeat
  on handlerName ... end handlerName
  answer text  -- display text (appended to output)
  say text     -- print
  ask prompt   -- input (stores in "it")
  global var   -- declare global
  return value
  exit to HyperCard / exit repeat  -- stop
  Arithmetic: +, -, *, /, mod, div, ^, **
  String: char i of x, word i of x, line i of x, item i of x
  Functions: sin, cos, sqrt, abs, round, trunc, random, length, offset
"""

from __future__ import annotations

import math
import random
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_hypertalk(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a HyperTalk program and return all output."""
    env = HyperTalkEnvironment(interpreter, turtle)
    return env.run(source)


class HyperTalkEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._vars: dict[str, Any] = {}
        self._globals: dict[str, Any] = {}
        self._handlers: dict[str, list[str]] = {}
        self._handler_params: dict[str, list[str]] = {}
        self._it: Any = ""

    def _emit(self, text: str):
        self._output.append(str(text))

    def _set_var(self, name: str, value: Any):
        name = name.lower()
        if name in self._globals:
            self._globals[name] = value
        else:
            self._vars[name] = value

    def _get_var(self, name: str) -> Any:
        name = name.lower()
        if name == "it":
            return self._it
        if name in self._vars:
            return self._vars[name]
        if name in self._globals:
            return self._globals[name]
        return name  # unbound returns capitalised name (HyperTalk convention)

    def run(self, source: str) -> str:
        try:
            lines = source.splitlines()
            self._scan_handlers(lines)
            self._exec_lines(lines, 0, len(lines))
        except HTExit:
            pass
        except HTError as e:
            self._emit(f"❌ HyperTalk error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    def _scan_handlers(self, lines: list[str]):
        i = 0
        while i < len(lines):
            m = re.match(r"^\s*on\s+(\w+)(.*)$", lines[i], re.IGNORECASE)
            if m:
                name = m.group(1).lower()
                params = [p.strip() for p in m.group(2).strip().split(",") if p.strip()]
                # Also split space-separated params
                if len(params) == 1 and " " in params[0]:
                    params = params[0].split()
                self._handler_params[name] = params
                body = []
                i += 1
                while i < len(lines) and not re.match(
                    r"^\s*end\s+" + re.escape(name), lines[i], re.IGNORECASE
                ):
                    body.append(lines[i])
                    i += 1
                self._handlers[name] = body
            i += 1

    def _exec_lines(self, lines: list[str], start: int, end: int) -> Any:
        i = start
        while i < end:
            line = lines[i].strip()
            if not line or line.startswith("--"):
                i += 1
                continue

            # Skip on...end handler blocks in main flow
            m = re.match(r"^on\s+(\w+)", line, re.IGNORECASE)
            if m:
                # Skip to matching "end name"
                name = m.group(1).lower()
                i += 1
                while i < end and not re.match(
                    r"^end\s+" + re.escape(name), lines[i].strip(), re.IGNORECASE
                ):
                    i += 1
                i += 1
                continue

            result = self._exec_stmt(line, lines, i, end)
            if isinstance(result, tuple):
                cmd, val = result
                if cmd == "GOTO":
                    i = val
                elif cmd == "RETURN":
                    return val
                elif cmd == "BREAK":
                    return ("BREAK", None)
                else:
                    i += 1
            else:
                i += 1
        return None

    def _exec_stmt(self, stmt: str, lines: list[str], ip: int, block_end: int) -> Any:
        stmt_lower = stmt.lower()

        # PUT value INTO/AFTER/BEFORE var
        m = re.match(
            r"^put\s+(.+?)\s+(into|after|before)\s+(\w+)$", stmt, re.IGNORECASE
        )
        if m:
            val = str(self._eval(m.group(1).strip()))
            op = m.group(2).lower()
            var = m.group(3)
            if op == "into":
                self._set_var(var, val)
            elif op == "after":
                self._set_var(var, str(self._get_var(var)) + val)
            elif op == "before":
                self._set_var(var, val + str(self._get_var(var)))
            return None

        # PUT value (store to "it" and display in message box)
        m = re.match(r"^put\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            val = self._eval(m.group(1).strip())
            self._it = val
            self._emit(str(val))
            return None

        # GET expression
        m = re.match(r"^get\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._it = self._eval(m.group(1).strip())
            return None

        # SET var TO value
        m = re.match(r"^set\s+(\w+)\s+to\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._set_var(m.group(1), self._eval(m.group(2).strip()))
            return None

        # GLOBAL
        m = re.match(r"^global\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            for v in m.group(1).split(","):
                v = v.strip().lower()
                if v not in self._globals:
                    self._globals[v] = ""
            return None

        # ANSWER / SAY
        m = re.match(r"^(?:answer|say)\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            self._emit(str(self._eval(m.group(1).strip())))
            return None

        # ASK
        m = re.match(r"^ask\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            prompt = str(self._eval(m.group(1).strip()))
            if hasattr(self.interpreter, "request_input"):
                self._it = self.interpreter.request_input(prompt)
            else:
                self._it = ""
            return None

        # RETURN
        m = re.match(r"^return\s*(.*)", stmt, re.IGNORECASE)
        if m:
            retval = self._eval(m.group(1).strip()) if m.group(1).strip() else None
            return ("RETURN", retval)

        # EXIT
        if re.match(r"^exit\b", stmt_lower):
            raise HTExit()

        # IF ... THEN [inline or block]
        m = re.match(r"^if\s+(.+?)\s+then\s*(.*)", stmt, re.IGNORECASE)
        if m:
            cond = self._eval_cond(m.group(1).strip())
            inline = m.group(2).strip()
            if inline:
                # Check for else on same line
                else_m = re.match(r"^(.+?)\s+else\s+(.+)$", inline, re.IGNORECASE)
                if else_m:
                    if cond:
                        self._exec_stmt(else_m.group(1).strip(), lines, ip, block_end)
                    else:
                        self._exec_stmt(else_m.group(2).strip(), lines, ip, block_end)
                else:
                    if cond:
                        self._exec_stmt(inline, lines, ip, block_end)
            else:
                # Block if
                then_end, else_range, after = self._scan_if(lines, ip + 1)
                if cond:
                    self._exec_lines(lines, ip + 1, then_end)
                elif else_range:
                    self._exec_lines(lines, else_range[0], else_range[1])
                return ("GOTO", after)
            return None

        # REPEAT n TIMES / WITH / WHILE / UNTIL / forever
        if re.match(r"^repeat\b", stmt_lower):
            return self._exec_repeat(stmt, lines, ip, block_end)

        # Assignment: var = expr or var is expr
        m = re.match(r"^(\w+)\s*[=:]\s*(.+)$", stmt)
        if m and not re.match(
            r"^(if|put|get|set|answer|say|ask|repeat|on|end|global|return|exit)\b",
            stmt_lower,
        ):
            self._set_var(m.group(1), self._eval(m.group(2).strip()))
            return None

        # Call handler
        m = re.match(r"^(\w+)(?:\s+(.*))?$", stmt)
        if m:
            name = m.group(1).lower()
            args_str = m.group(2) or ""
            if name in self._handlers:
                # Bind arguments to handler parameter names
                params = self._handler_params.get(name, [])
                arg_vals = (
                    [a.strip() for a in args_str.split(",")] if args_str.strip() else []
                )
                # If single arg with spaces and multiple params, split by space
                if len(arg_vals) == 1 and len(params) > 1 and " " in arg_vals[0]:
                    arg_vals = arg_vals[0].split()
                for pi, pname in enumerate(params):
                    hval: Any = self._eval(arg_vals[pi]) if pi < len(arg_vals) else None
                    self._set_var(pname, hval)
                self._exec_lines(self._handlers[name], 0, len(self._handlers[name]))
                return None

        return None

    def _scan_if(self, lines: list[str], start: int) -> tuple[int, tuple | None, int]:
        """Return (then_end, else_range or None, after_endif)."""
        i = start
        then_end = start
        else_start = None
        else_end = None
        depth = 0
        while i < len(lines):
            s = lines[i].strip().lower()
            if re.match(r"^if\b", s):
                depth += 1
            elif re.match(r"^end if\b", s) or s == "end if":
                if depth == 0:
                    if else_start is not None:
                        else_end = i
                    else:
                        then_end = i
                    return (
                        then_end,
                        (else_start, else_end) if else_start else None,
                        i + 1,
                    )
                depth -= 1
            elif s == "else" and depth == 0:
                then_end = i
                else_start = i + 1
            i += 1
        return (len(lines), None, len(lines))

    def _exec_repeat(self, stmt: str, lines: list[str], ip: int, block_end: int) -> Any:
        end_i = self._find_end_repeat(lines, ip + 1)
        body_start = ip + 1

        m = re.match(r"^repeat\s+(\d+)\s+times?$", stmt, re.IGNORECASE)
        if m:
            n = int(m.group(1))
            for _ in range(min(n, 100000)):
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
            return ("GOTO", end_i + 1)

        m = re.match(
            r"^repeat\s+with\s+(\w+)\s*=\s*(.+?)\s+to\s+(.+?)(?:\s+by\s+(.+))?$",
            stmt,
            re.IGNORECASE,
        )
        if m:
            var = m.group(1)
            frm = int(float(self._eval(m.group(2).strip())))
            to = int(float(self._eval(m.group(3).strip())))
            step = int(float(self._eval(m.group(4).strip()))) if m.group(4) else 1
            i = frm
            count = 0
            while (step > 0 and i <= to) or (step < 0 and i >= to):
                if count > 100000:
                    break
                self._set_var(var, i)
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                i += step
                count += 1
            return ("GOTO", end_i + 1)

        m = re.match(r"^repeat\s+while\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            cond_str = m.group(1).strip()
            count = 0
            while self._eval_cond(cond_str) and count < 100000:
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                count += 1
            return ("GOTO", end_i + 1)

        m = re.match(r"^repeat\s+until\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            cond_str = m.group(1).strip()
            count = 0
            while not self._eval_cond(cond_str) and count < 100000:
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                count += 1
            return ("GOTO", end_i + 1)

        # repeat forever
        if re.match(r"^repeat\s*$", stmt, re.IGNORECASE) or re.match(
            r"^repeat\s+forever$", stmt, re.IGNORECASE
        ):
            count = 0
            while count < 100000:
                r = self._exec_lines(lines, body_start, end_i)
                if isinstance(r, tuple) and r[0] == "BREAK":
                    break
                count += 1
            return ("GOTO", end_i + 1)

        return ("GOTO", end_i + 1)

    def _find_end_repeat(self, lines: list[str], start: int) -> int:
        depth = 0
        i = start
        while i < len(lines):
            s = lines[i].strip().lower()
            if re.match(r"^repeat\b", s):
                depth += 1
            elif re.match(r"^end\s+repeat\b", s):
                if depth == 0:
                    return i
                depth -= 1
            i += 1
        return i

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return ""
        # String
        if expr.startswith('"') or expr.startswith("'"):
            return expr.strip("\"'")
        # Number
        try:
            return int(expr)
        except ValueError:
            pass
        try:
            return float(expr)
        except ValueError:
            pass
        # Boolean
        if expr.lower() == "true":
            return True
        if expr.lower() == "false":
            return False
        if expr.lower() == "empty":
            return ""
        if expr.lower() == "it":
            return self._it
        if expr.lower() == "return":
            return "\n"
        if expr.lower() == "space":
            return " "
        if expr.lower() == "tab":
            return "\t"
        if expr.lower() == "cr":
            return "\n"

        # Property access: char i of str
        m = re.match(r"^char(?:acter)?\s+(\d+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            idx = int(m.group(1)) - 1
            s = str(self._eval(m.group(2).strip()))
            return s[idx] if 0 <= idx < len(s) else ""

        m = re.match(r"^word\s+(\d+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            idx = int(m.group(1)) - 1
            words = str(self._eval(m.group(2).strip())).split()
            return words[idx] if 0 <= idx < len(words) else ""

        m = re.match(r"^line\s+(\d+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            idx = int(m.group(1)) - 1
            lines_ = str(self._eval(m.group(2).strip())).splitlines()
            return lines_[idx] if 0 <= idx < len(lines_) else ""

        m = re.match(r"^item\s+(\d+)\s+of\s+(.+)$", expr, re.IGNORECASE)
        if m:
            idx = int(m.group(1)) - 1
            items = str(self._eval(m.group(2).strip())).split(",")
            return items[idx].strip() if 0 <= idx < len(items) else ""

        m = re.match(
            r"^the\s+(?:number\s+of\s+)?chars?\s+(?:in|of)\s+(.+)$", expr, re.IGNORECASE
        )
        if m:
            return len(str(self._eval(m.group(1).strip())))

        m = re.match(r"^the\s+length\s+(?:of|in)\s+(.+)$", expr, re.IGNORECASE)
        if m:
            return len(str(self._eval(m.group(1).strip())))

        m = re.match(
            r"^the\s+(?:number\s+of\s+)?words?\s+(?:in|of)\s+(.+)$", expr, re.IGNORECASE
        )
        if m:
            return len(str(self._eval(m.group(1).strip())).split())

        # Function call: name(args)
        m = re.match(r"^(\w+)\s*\((.+)\)$", expr)
        if m:
            result = self._call_fn(m.group(1).lower(), m.group(2))
            if result is not None:
                return result

        # Concatenation: expr & expr
        if "&" in expr:
            parts = expr.split("&", 1)
            return str(self._eval(parts[0].strip())) + str(self._eval(parts[1].strip()))
        if "&&" in expr:
            parts = expr.split("&&", 1)
            return (
                str(self._eval(parts[0].strip()))
                + " "
                + str(self._eval(parts[1].strip()))
            )

        # Arithmetic using Python
        for op in [" is ", " is not ", " mod ", " div ", " contains ", " is in "]:
            if op in expr.lower():
                lhs_s, rhs_s = re.split(op, expr, maxsplit=1, flags=re.IGNORECASE)
                lhs = self._eval(lhs_s.strip())
                rhs = self._eval(rhs_s.strip())
                op_clean = op.strip().lower()
                if op_clean == "is":
                    return lhs == rhs
                if op_clean == "is not":
                    return lhs != rhs
                if op_clean == "mod":
                    return _to_num(lhs) % _to_num(rhs)
                if op_clean == "div":
                    return int(_to_num(lhs) / _to_num(rhs))
                if op_clean == "contains":
                    return str(rhs) in str(lhs)
                if op_clean == "is in":
                    return str(lhs) in str(rhs)

        # Python eval for arithmetic
        def sub_var(m_):
            name = m_.group(0).lower()
            val = self._get_var(name)
            return str(val)

        pyexpr = re.sub(r"[A-Za-z_]\w*", sub_var, expr)
        pyexpr = pyexpr.replace("^", "**")
        try:
            return eval(pyexpr, {"__builtins__": {}})  # noqa: S307
        except Exception:
            pass

        return self._get_var(expr)

    def _eval_cond(self, cond: str) -> bool:
        val = self._eval(cond)
        if isinstance(val, bool):
            return val
        if isinstance(val, (int, float)):
            return val != 0
        if isinstance(val, str):
            return val.lower() not in ("", "false", "0")
        return bool(val)

    def _call_fn(self, name: str, args_str: str) -> Any:
        args = [self._eval(a.strip()) for a in args_str.split(",")]
        a0 = float(args[0]) if args else 0
        fns: dict[str, Any] = {
            "sin": lambda: math.sin(math.radians(a0)),
            "cos": lambda: math.cos(math.radians(a0)),
            "tan": lambda: math.tan(math.radians(a0)),
            "sqrt": lambda: math.sqrt(a0),
            "abs": lambda: abs(a0),
            "round": lambda: round(a0),
            "trunc": lambda: int(a0),
            "length": lambda: len(str(args[0])),
            "random": lambda: random.randint(1, int(a0)) if a0 else random.random(),
            "offset": lambda: (
                str(args[1]).find(str(args[0])) + 1 if len(args) > 1 else 0
            ),
            "max": lambda: max(float(a) for a in args),
            "min": lambda: min(float(a) for a in args),
            "value": lambda: self._eval(str(args[0])),
            "numtochar": lambda: chr(int(a0)),
            "chartonum": lambda: ord(str(args[0])[0]) if args[0] else 0,
        }
        fn = fns.get(name)
        if fn:
            try:
                return fn()
            except Exception:
                return 0
        return None


def _to_num(val: Any) -> float:
    try:
        return float(val)
    except (TypeError, ValueError):
        return 0.0


class HTExit(Exception):
    pass


class HTError(Exception):
    pass
