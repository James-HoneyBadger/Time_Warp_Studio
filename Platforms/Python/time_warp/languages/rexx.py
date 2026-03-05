"""REXX language executor for Time Warp Studio.

Educational REXX interpreter — whole-program execution.
Supports a teaching subset of Classic REXX:
  SAY expr               -- print
  PULL var               -- read input
  var = expr             -- assignment
  IF cond THEN / ELSE / END
  DO ... END             -- simple do group
  DO i = 1 TO n [BY s]  -- numeric loop
  DO WHILE cond / DO UNTIL cond
  LEAVE / ITERATE        -- loop control
  CALL name [args]       -- subroutine call
  name: body RETURN [val]-- subroutine definition
  EXIT [value]           -- exit
  String built-ins: LENGTH, SUBSTR, STRIP, UPPER, LOWER,
                    LEFT, RIGHT, POS, REVERSE, WORD, WORDS,
                    COPIES, SPACE, TRANSLATE
  Numeric built-ins: ABS, MAX, MIN, SIGN, TRUNC, FORMAT
  Logical: NOT (\\), &&, ||
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_rexx(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a REXX program and return all output."""
    env = RexxEnvironment(interpreter, turtle)
    return env.run(source)


def _smart_num(val: Any) -> int | float:
    """Convert to int if whole number, else float."""
    try:
        f = float(val)
        if f == int(f):
            return int(f)
        return f
    except (TypeError, ValueError):
        return 0


class RexxEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._vars: dict[str, Any] = {}
        self._subroutines: dict[str, int] = {}
        self._call_stack: list[tuple[int, dict]] = []
        self._it: Any = ""  # result of GET
        self.lines: list[str] = []

    def _emit(self, text: str):
        self._output.append(str(text))

    def run(self, source: str) -> str:
        try:
            self.lines = [l.rstrip() for l in source.splitlines()]
            self._scan_subroutines()
            self._exec_block(0, len(self.lines))
        except RexxExit:
            pass
        except RexxError as e:
            self._emit(f"❌ REXX error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    def _scan_subroutines(self):
        for i, line in enumerate(self.lines):
            s = line.strip()
            m = re.match(r"^([A-Za-z_]\w*)\s*:\s*$", s)
            if m:
                self._subroutines[m.group(1).upper()] = i + 1

    # ------------------------------------------------------------------
    # Block executor — returns index after block
    # ------------------------------------------------------------------

    def _exec_block(self, start: int, end: int) -> int:
        i = start
        while i < end:
            line = self.lines[i].strip()
            if not line or line.startswith("/*") or line.startswith("--"):
                i += 1
                continue

            # Skip subroutine label lines
            if re.match(r"^[A-Za-z_]\w*\s*:\s*$", line):
                i += 1
                continue

            result = self._exec_stmt(line, i, end)
            if isinstance(result, tuple):
                action, target = result
                if action == "GOTO":
                    i = target
                elif action == "RETURN":
                    return i
                elif action == "LEAVE":
                    return -1  # signal break
                elif action == "ITERATE":
                    return -2  # signal continue
                else:
                    i += 1
            else:
                i += 1
        return end

    def _exec_stmt(self, stmt: str, ip: int, block_end: int) -> Any:
        stmt_upper = stmt.upper()

        # EXIT
        if re.match(r"^EXIT\b", stmt_upper):
            raise RexxExit()

        # SAY
        m = re.match(r"^SAY\s+(.*)", stmt, re.IGNORECASE)
        if m:
            val = self._eval(m.group(1).strip())
            self._emit(str(val))
            return None

        # PULL
        m = re.match(r"^PULL\s+(.*)", stmt, re.IGNORECASE)
        if m:
            val = ""
            if hasattr(self.interpreter, "request_input"):
                val = self.interpreter.request_input("REXX> ") or ""
            for var in m.group(1).split():
                self._set_var(var, val)
            return None

        # PARSE VAR (simplified)
        m = re.match(r"^PARSE\s+VAR\s+(\w+)\s+(.*)", stmt, re.IGNORECASE)
        if m:
            source_var = m.group(1).upper()
            template = m.group(2).strip()
            self._parse_var(self._get_var(source_var), template)
            return None

        # IF cond THEN  or  IF cond THEN stmt [ELSE stmt]
        m = re.match(r"^IF\s+(.+?)\s+THEN\s*(.*)", stmt, re.IGNORECASE)
        if m:
            cond_str = m.group(1).strip()
            rest = m.group(2).strip()
            cond = self._eval_cond(cond_str)
            if rest.upper().startswith("DO") or not rest:
                # Multi-line DO block
                then_end, else_start, else_end = self._scan_if_block(ip + 1)
                if cond:
                    self._exec_block(ip + 1, then_end)
                elif else_start is not None:
                    self._exec_block(else_start, else_end)
                return ("GOTO", else_end if else_end else then_end)
            else:
                # Inline: IF cond THEN stmt [ELSE stmt]
                else_m = re.match(r"^(.+?)\s+ELSE\s+(.+)$", rest, re.IGNORECASE)
                if else_m:
                    if cond:
                        r = self._exec_stmt(else_m.group(1).strip(), ip, block_end)
                    else:
                        r = self._exec_stmt(else_m.group(2).strip(), ip, block_end)
                    if isinstance(r, tuple):
                        return r
                else:
                    if cond:
                        r = self._exec_stmt(rest, ip, block_end)
                        if isinstance(r, tuple):
                            return r
                return None

        # DO WHILE / DO UNTIL / DO i = 1 TO n / DO n / DO ... END
        if re.match(r"^DO\b", stmt_upper):
            return self._exec_do(stmt, ip, block_end)

        # LEAVE
        if stmt_upper == "LEAVE":
            return ("LEAVE", None)

        # ITERATE
        if stmt_upper == "ITERATE":
            return ("ITERATE", None)

        # CALL subroutine
        m = re.match(r"^CALL\s+(\w+)\s*(.*)", stmt, re.IGNORECASE)
        if m:
            self._call_sub(m.group(1).upper(), m.group(2).strip())
            return None

        # RETURN (inside subroutine)
        m = re.match(r"^RETURN\b\s*(.*)", stmt, re.IGNORECASE)
        if m:
            ret_val = self._eval(m.group(1).strip()) if m.group(1).strip() else ""
            self._vars["RESULT"] = ret_val
            return ("RETURN", None)

        # INTERPRET — eval string as REXX code
        m = re.match(r"^INTERPRET\s+(.*)", stmt, re.IGNORECASE)
        if m:
            code = str(self._eval(m.group(1).strip()))
            # Execute single-line interpreted code
            for line in code.splitlines():
                line = line.strip()
                if line:
                    self._exec_stmt(line, ip, block_end)
            return None

        # SIGNAL label — unconditional jump
        m = re.match(r"^SIGNAL\s+(\w+)$", stmt, re.IGNORECASE)
        if m:
            return ("GOTO", int(self._vars.get("_LBL_" + m.group(1).upper(), -1)))

        # ADDRESS environment command
        m = re.match(r"^ADDRESS\s+(\w+)(?:\s+(.+))?$", stmt, re.IGNORECASE)
        if m:
            env = m.group(1).upper()
            cmd = m.group(2) or ""
            if cmd:
                cmdval = str(self._eval(cmd))
                self._emit(f"ℹ️  ADDRESS {env}: {cmdval}")
            return None

        # DROP variable(s)
        m = re.match(r"^DROP\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            for var in m.group(1).split():
                self._vars.pop(var.upper(), None)
            return None

        # NUMERIC DIGITS n
        m = re.match(r"^NUMERIC\s+DIGITS\s+(\d+)$", stmt, re.IGNORECASE)
        if m:
            self._vars["__NUMERIC_DIGITS__"] = int(m.group(1))
            return None

        # NUMERIC FUZZ n
        m = re.match(r"^NUMERIC\s+FUZZ\s+(\d+)$", stmt, re.IGNORECASE)
        if m:
            return None

        # NUMERIC FORM
        m = re.match(r"^NUMERIC\s+FORM\b", stmt, re.IGNORECASE)
        if m:
            return None

        # TRACE ALL/RESULTS/NORMAL/OFF/...
        m = re.match(r"^TRACE\s+", stmt, re.IGNORECASE)
        if m:
            return None

        # PUSH (add to queue)
        m = re.match(r"^PUSH\s*(.*)", stmt, re.IGNORECASE)
        if m:
            if not hasattr(self, "_queue"):
                self._queue: list = []
            self._queue.insert(0, str(self._eval(m.group(1).strip())))
            return None

        # QUEUE (append to queue)
        m = re.match(r"^QUEUE\s+(.*)", stmt, re.IGNORECASE)
        if m:
            if not hasattr(self, "_queue"):
                self._queue = []
            self._queue.append(str(self._eval(m.group(1).strip())))
            return None

        # PARSE PULL — from queue or input
        m = re.match(r"^PARSE\s+PULL\s+(.*)", stmt, re.IGNORECASE)
        if m:
            if hasattr(self, "_queue") and self._queue:
                val = self._queue.pop(0)
            elif hasattr(self.interpreter, "request_input"):
                val = self.interpreter.request_input("PARSE PULL> ") or ""
            else:
                val = ""
            self._parse_var(val, m.group(1).strip())
            return None

        # PARSE ARG / PARSE VALUE ... WITH template
        m = re.match(r"^PARSE\s+ARG\s+(.*)", stmt, re.IGNORECASE)
        if m:
            self._parse_var(self._vars.get("ARG0", ""), m.group(1).strip())
            return None

        m = re.match(r"^PARSE\s+VALUE\s+(.+?)\s+WITH\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            val = str(self._eval(m.group(1).strip()))
            self._parse_var(val, m.group(2).strip())
            return None

        m = re.match(r"^PARSE\s+UPPER\s+VAR\s+(\w+)\s+(.*)", stmt, re.IGNORECASE)
        if m:
            source_var = m.group(1).upper()
            self._parse_var(str(self._get_var(source_var)).upper(), m.group(2).strip())
            return None

        m = re.match(r"^PARSE\s+UPPER\s+(.*)", stmt, re.IGNORECASE)
        if m:
            if hasattr(self.interpreter, "request_input"):
                val = (self.interpreter.request_input("PARSE UPPER> ") or "").upper()
            else:
                val = ""
            self._parse_var(val, m.group(1).strip())
            return None

        m = re.match(r"^PARSE\s+EXTERNAL\s+(.*)", stmt, re.IGNORECASE)
        if m:
            if hasattr(self.interpreter, "request_input"):
                val = self.interpreter.request_input("PARSE EXTERNAL> ") or ""
            else:
                val = ""
            self._parse_var(val, m.group(1).strip())
            return None

        # Stem/compound variable assignment: stem.tail = expr
        m_stem = re.match(r"^(\w+\.\w*(?:\.\w*)*)\s*=\s*(?!=)(.+)$", stmt)
        if m_stem:
            val = self._eval(m_stem.group(2).strip())
            key = m_stem.group(1).upper()
            self._vars[key] = val
            return None

        # Assignment: var = expr  (but not comparisons)
        m = re.match(r"^(\w+)\s*=\s*(?!=)(.+)$", stmt)
        if m:
            val = self._eval(m.group(2).strip())
            self._set_var(m.group(1), val)
            return None

        # Bare function call (e.g., SAY without keyword = assignment to nothing)
        # Try to evaluate as expression with side effects
        return None

    def _exec_do(self, stmt: str, ip: int, block_end: int) -> Any:
        """Execute a DO block. Scan forward for matching END."""
        # Find matching END
        do_end = self._find_do_end(ip + 1)
        body_lines_idx = list(range(ip + 1, do_end))
        upper = stmt.upper()

        # DO i = start TO end [BY step]
        m = re.match(
            r"^DO\s+(\w+)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+BY\s+(.+))?\s*$",
            stmt,
            re.IGNORECASE,
        )
        if m:
            var = m.group(1).upper()
            frm = self._eval(m.group(2).strip())
            to = self._eval(m.group(3).strip())
            step = self._eval(m.group(4).strip()) if m.group(4) else 1
            # Use int when possible
            frm = _smart_num(frm)
            to = _smart_num(to)
            step = _smart_num(step)
            cur = frm
            count = 0
            while (step > 0 and cur <= to) or (step < 0 and cur >= to):
                if count > 100000:
                    break
                self._set_var(var, cur)
                result = self._exec_block(ip + 1, do_end)
                if result == -1:  # LEAVE
                    break
                if result == -2:  # ITERATE
                    cur += step
                    count += 1
                    continue
                cur += step
                count += 1
            return ("GOTO", do_end + 1)

        # DO WHILE cond
        m = re.match(r"^DO\s+WHILE\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            cond_str = m.group(1).strip()
            count = 0
            while self._eval_cond(cond_str):
                if count > 100000:
                    break
                result = self._exec_block(ip + 1, do_end)
                if result == -1:
                    break
                count += 1
            return ("GOTO", do_end + 1)

        # DO UNTIL cond
        m = re.match(r"^DO\s+UNTIL\s+(.+)$", stmt, re.IGNORECASE)
        if m:
            cond_str = m.group(1).strip()
            count = 0
            while True:
                if count > 100000:
                    break
                result = self._exec_block(ip + 1, do_end)
                if result == -1:
                    break
                count += 1
                if self._eval_cond(cond_str):
                    break
            return ("GOTO", do_end + 1)

        # DO n  (repeat n times)
        m = re.match(r"^DO\s+(\d+)\s*$", stmt, re.IGNORECASE)
        if m:
            n = int(self._eval(m.group(1)))
            for _ in range(min(n, 100000)):
                result = self._exec_block(ip + 1, do_end)
                if result == -1:
                    break
            return ("GOTO", do_end + 1)

        # DO FOREVER
        m = re.match(r"^DO\s+FOREVER\s*$", stmt, re.IGNORECASE)
        if m:
            count = 0
            while count < 100000:
                result = self._exec_block(ip + 1, do_end)
                if result == -1:  # LEAVE
                    break
                count += 1
            return ("GOTO", do_end + 1)

        # DO ... END  (simple grouping)
        self._exec_block(ip + 1, do_end)
        return ("GOTO", do_end + 1)

    def _find_do_end(self, start: int) -> int:
        depth = 1
        i = start
        while i < len(self.lines):
            s = self.lines[i].strip().upper()
            if re.match(r"^DO\b", s):
                depth += 1
            elif s == "END":
                depth -= 1
                if depth == 0:
                    return i
            i += 1
        return i

    def _scan_if_block(self, start: int) -> tuple[int, int | None, int | None]:
        """Return (then_end, else_start, else_end) line indices."""
        depth = 0
        i = start
        then_end = start
        else_start = None
        else_end = None
        while i < len(self.lines):
            s = self.lines[i].strip().upper()
            if re.match(r"^DO\b", s):
                depth += 1
            elif s == "END":
                depth -= 1
                if depth < 0:
                    then_end = i
                    break
                if depth == 0:
                    then_end = i + 1
                    break
            elif depth == 0 and s == "ELSE":
                then_end = i
                else_start = i + 1
            i += 1
        if else_start is not None:
            else_end = i + 1
        return then_end, else_start, else_end if else_start else None

    def _call_sub(self, name: str, args_str: str):
        if name not in self._subroutines:
            raise RexxError(f"Unknown subroutine: {name}")
        old_it = self._it
        self._vars["ARG"] = args_str
        # Find end of subroutine
        start = self._subroutines[name]
        self._exec_block(start, len(self.lines))

    def _parse_var(self, source: str, template: str):
        words = source.split()
        for i, name in enumerate(template.split()):
            self._set_var(name, words[i] if i < len(words) else "")

    # ------------------------------------------------------------------
    # Variable access
    # ------------------------------------------------------------------

    def _set_var(self, name: str, value: Any):
        self._vars[name.upper()] = value

    def _get_var(self, name: str) -> Any:
        upper = name.upper()
        # Check direct variable
        if upper in self._vars:
            return self._vars[upper]
        # Stem variable: STEM.TAIL — try resolving tail as variable
        if "." in upper:
            parts = upper.split(".", 1)
            tail_val = self._vars.get(parts[1], parts[1])
            key = parts[0] + "." + str(tail_val)
            if key in self._vars:
                return self._vars[key]
        # Return name as-is (REXX convention: uninitialized vars = their name)
        return upper

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return ""
        # String literal
        if expr.startswith("'") or expr.startswith('"'):
            return expr.strip("'\"")
        # Number
        try:
            return int(expr)
        except ValueError:
            pass
        try:
            return float(expr)
        except ValueError:
            pass
        # Built-in function call
        m = re.match(r"^(\w+)\s*\((.+)\)$", expr, re.IGNORECASE)
        if m:
            result = self._call_builtin(m.group(1).upper(), m.group(2))
            if result is not None:
                return result

        # Arithmetic using Python — try BEFORE concatenation
        def replace_m(m_):
            name = m_.group(0)
            upper = name.upper()
            val = self._vars.get(upper)
            if val is not None:
                return str(val)
            # Try stem variable resolution
            if "." in upper:
                resolved = self._get_var(upper)
                if resolved != upper:
                    return str(resolved)
            return name

        pyexpr = re.sub(r"[A-Za-z_]\w*(?:\.\w+)*", replace_m, expr)
        pyexpr = pyexpr.replace("\\\\", " not ")
        pyexpr = pyexpr.replace("&&", " and ")
        pyexpr = re.sub(r"\\\+", " or ", pyexpr)
        # REXX comparison operators → Python
        pyexpr = pyexpr.replace("\\=", "!=")
        pyexpr = pyexpr.replace("<>", "!=")
        pyexpr = re.sub(r"(?<![<>!=])=(?!=)", "==", pyexpr)  # = → ==
        try:
            val = eval(pyexpr, {"__builtins__": {}, "abs": abs})  # noqa: S307
            return val
        except Exception:
            pass
        # Concatenation: a b or a || b (only if arithmetic failed)
        for sep in [" || ", "||", " "]:
            if sep in expr:
                # Simple two-part
                parts = expr.split(sep, 1)
                if len(parts) == 2:
                    lhs = self._eval(parts[0].strip())
                    rhs = self._eval(parts[1].strip())
                    if sep.strip() == "||" or sep == " ":
                        return str(lhs) + str(rhs)
        # Variable (including stem variables with .)
        return self._get_var(expr)

    def _eval_cond(self, cond: str) -> bool:
        cond = cond.strip()
        val = self._eval(cond)
        if isinstance(val, bool):
            return val
        if isinstance(val, (int, float)):
            return val != 0
        if isinstance(val, str):
            return val.upper() not in ("", "0", "FALSE")
        return bool(val)

    def _call_builtin(self, name: str, args_str: str) -> Any:
        import math as _math
        import random as _random
        import datetime as _datetime

        raw_args_str = args_str
        args = [
            self._eval(a.strip()) for a in args_str.split(",") if a.strip() or args_str
        ]

        def s0() -> str:
            return str(args[0]) if args else ""

        def s1() -> str:
            return str(args[1]) if len(args) > 1 else ""

        def n0() -> float:
            try:
                return float(args[0]) if args else 0
            except (ValueError, TypeError):
                return 0

        def n1() -> float:
            try:
                return float(args[1]) if len(args) > 1 else 0
            except (ValueError, TypeError):
                return 0

        builtins: dict[str, Any] = {
            # -- String functions --
            "LENGTH": lambda: len(s0()),
            "SUBSTR": lambda: (
                (
                    str(args[0])[
                        int(args[1])
                        - 1 : (
                            (int(args[2]) + int(args[1]) - 1) if len(args) > 2 else None
                        )
                    ]
                )
                if args
                else ""
            ),
            "LEFT": lambda: (
                str(args[0])[: int(args[1])].ljust(int(args[1]))
                if len(args) > 1
                else s0()
            ),
            "RIGHT": lambda: (
                str(args[0])[-int(args[1]) :].rjust(int(args[1]))
                if len(args) > 1
                else s0()
            ),
            "STRIP": lambda: (
                s0().strip(str(args[2]))
                if len(args) > 2
                else (
                    s0().lstrip()
                    if len(args) > 1 and str(args[1]).upper() == "L"
                    else (
                        s0().rstrip()
                        if len(args) > 1 and str(args[1]).upper() == "T"
                        else s0().strip()
                    )
                )
            ),
            "UPPER": lambda: s0().upper(),
            "LOWER": lambda: s0().lower(),
            "REVERSE": lambda: s0()[::-1],
            "POS": lambda: str(args[1]).find(str(args[0])) + 1 if len(args) > 1 else 0,
            "LASTPOS": lambda: (
                (str(args[1]).rfind(str(args[0])) + 1) if len(args) > 1 else 0
            ),
            "CHAROUT": lambda: (s0(), "")[1],  # no-op output
            "CHARIN": lambda: "",
            "WORD": lambda: (
                str(args[0]).split()[int(args[1]) - 1]
                if len(args) > 1 and len(str(args[0]).split()) >= int(args[1])
                else ""
            ),
            "WORDS": lambda: len(s0().split()),
            "WORDPOS": lambda: (
                (str(args[1]).split().index(str(args[0])) + 1)
                if len(args) > 1 and str(args[0]) in str(args[1]).split()
                else 0
            ),
            "WORDLENGTH": lambda: (
                len(str(args[0]).split()[int(args[1]) - 1])
                if len(args) > 1 and len(str(args[0]).split()) >= int(args[1])
                else 0
            ),
            "SUBWORD": lambda: (
                " ".join(
                    str(args[0]).split()[
                        int(args[1])
                        - 1 : (
                            (int(args[1]) - 1 + int(args[2])) if len(args) > 2 else None
                        )
                    ]
                )
                if len(args) > 1
                else ""
            ),
            "DELSTR": lambda: (
                (
                    str(args[0])[: int(args[1]) - 1]
                    + str(args[0])[int(args[1]) - 1 + int(args[2]) :]
                )
                if len(args) > 2
                else str(args[0])
            ),
            "DELWORD": lambda: (
                " ".join(
                    w
                    for i, w in enumerate(str(args[0]).split(), 1)
                    if not (
                        int(args[1])
                        <= i
                        < int(args[1]) + (int(args[2]) if len(args) > 2 else 1)
                    )
                )
                if len(args) > 1
                else s0()
            ),
            "INSERT": lambda: (
                (
                    str(args[1])[: int(args[2])]
                    + str(args[0]).ljust(
                        int(args[3]) if len(args) > 3 else len(str(args[0]))
                    )
                    + str(args[1])[int(args[2]) :]
                )
                if len(args) > 2
                else s0()
            ),
            "OVERLAY": lambda: (
                (
                    str(args[1])[: int(args[2]) - 1]
                    + str(args[0]).ljust(
                        int(args[3]) if len(args) > 3 else len(str(args[0]))
                    )[: int(args[3]) if len(args) > 3 else len(str(args[0]))]
                    + str(args[1])[
                        int(args[2])
                        - 1
                        + int(args[3] if len(args) > 3 else len(str(args[0]))) :
                    ]
                )
                if len(args) > 1
                else s0()
            ),
            "COPIES": lambda: str(args[0]) * int(args[1]) if len(args) > 1 else "",
            "SPACE": lambda: (" " * (int(args[1]) if len(args) > 1 else 1)).join(
                s0().split()
            ),
            "CENTER": lambda: (
                str(args[0]).center(int(args[1])) if len(args) > 1 else s0()
            ),
            "CENTRE": lambda: (
                str(args[0]).center(int(args[1])) if len(args) > 1 else s0()
            ),
            "JUSTIFY": lambda: (
                str(args[0]).center(int(args[1])) if len(args) > 1 else s0()
            ),
            "TRANSLATE": lambda: (
                s0().translate(
                    str.maketrans(str(args[2]), str(args[1])[: len(str(args[2]))])
                )
                if len(args) > 2
                else s0().upper()
            ),
            "COMPARE": lambda: (
                (
                    0
                    if str(args[0]) == str(args[1])
                    else next(
                        (
                            i + 1
                            for i, (a, b) in enumerate(zip(str(args[0]), str(args[1])))
                            if a != b
                        ),
                        min(len(str(args[0])), len(str(args[1]))) + 1,
                    )
                )
                if len(args) > 1
                else 0
            ),
            "ABBREV": lambda: (
                (
                    1
                    if str(args[1]).upper().startswith(str(args[0]).upper())
                    and len(str(args[1])) >= (int(args[2]) if len(args) > 2 else 0)
                    else 0
                )
                if len(args) > 1
                else 0
            ),
            "XRANGE": lambda: "".join(
                chr(c)
                for c in range(
                    ord(str(args[0])[0]) if args else 0,
                    ord(str(args[1])[0]) + 1 if len(args) > 1 else 256,
                )
            ),
            "VERIFY": lambda: (
                next(
                    (i + 1 for i, c in enumerate(str(args[0])) if c in str(args[1])), 0
                )
                if len(args) > 1
                else 0
            ),
            "CHANGESTR": lambda: (
                str(args[2]).replace(str(args[0]), str(args[1]))
                if len(args) > 2
                else s0()
            ),
            "COUNTSTR": lambda: (
                str(args[1]).count(str(args[0])) if len(args) > 1 else 0
            ),
            # -- Conversion functions --
            "B2X": lambda: hex(int(s0(), 2))[2:].upper() if s0() else "0",
            "X2B": lambda: bin(int(s0(), 16))[2:].zfill(len(s0()) * 4) if s0() else "0",
            "D2X": lambda: hex(int(n0()))[2:].upper(),
            "X2D": lambda: int(s0(), 16) if s0() else 0,
            "D2C": lambda: chr(int(n0())),
            "C2D": lambda: ord(s0()[0]) if s0() else 0,
            "C2X": lambda: s0().encode().hex().upper(),
            "X2C": lambda: bytes.fromhex(s0()).decode("latin-1"),
            "BITAND": lambda: "".join(chr(ord(a) & ord(b)) for a, b in zip(s0(), s1())),
            "BITOR": lambda: "".join(chr(ord(a) | ord(b)) for a, b in zip(s0(), s1())),
            "BITXOR": lambda: "".join(chr(ord(a) ^ ord(b)) for a, b in zip(s0(), s1())),
            # -- Math functions --
            "ABS": lambda: abs(n0()),
            "MAX": lambda: max(float(a) for a in args),
            "MIN": lambda: min(float(a) for a in args),
            "SIGN": lambda: (1 if n0() > 0 else (-1 if n0() < 0 else 0)),
            "TRUNC": lambda: int(n0()),
            "FLOOR": lambda: _math.floor(n0()),
            "CEILING": lambda: _math.ceil(n0()),
            "SQRT": lambda: _math.sqrt(n0()),
            "EXP": lambda: _math.exp(n0()),
            "LOG": lambda: _math.log(n0()),
            "LOG2": lambda: _math.log2(n0()),
            "LOG10": lambda: _math.log10(n0()),
            "POWER": lambda: n0() ** n1(),
            "MOD": lambda: n0() % n1() if n1() else 0,
            "SIN": lambda: _math.sin(n0()),
            "COS": lambda: _math.cos(n0()),
            "TAN": lambda: _math.tan(n0()),
            "FORMAT": lambda: (f"{n0():.{int(n1())}f}" if len(args) > 1 else str(n0())),
            # -- Type functions --
            "DATATYPE": lambda: ("NUM" if self._is_num(s0()) else "CHAR"),
            "SYMBOL": lambda: ("VAR" if s0().upper() in self._vars else "LIT"),
            "VALUE": lambda: self._get_var(s0()),
            # -- System functions --
            "DATE": lambda: _datetime.date.today().strftime(
                "%d %b %Y"
                if not args or str(args[0]).upper() in ("N", "NORMAL")
                else (
                    "%d/%m/%Y"
                    if str(args[0]).upper() in ("E", "EUROPEAN")
                    else _datetime.date.today().strftime("%Y-%m-%d")
                )
            ),
            "TIME": lambda: _datetime.datetime.now().strftime("%H:%M:%S"),
            "RANDOM": lambda: _random.randint(
                int(n0()) if args else 0, int(n1()) if len(args) > 1 else 999
            ),
            "QUEUED": lambda: 0,
            "CONDITION": lambda: "",
            "ERRORTEXT": lambda: "",
            "SOURCELINE": lambda: len(self._lines) if hasattr(self, "_lines") else 0,
            "USERID": lambda: "REXXUSER",
            "SYSVAR": lambda: "",
            # -- Stream functions --
            "STREAM": lambda: "READY",
            "CHARS": lambda: 0,
            "LINES": lambda: 0,
            "LINEIN": lambda: "",
            "LINEOUT": lambda: 0,
        }
        fn = builtins.get(name)
        if fn:
            try:
                result = fn()
                return result if result is not None else ""
            except Exception:
                return ""
        # SQL() external function: SQL("SELECT ...") → result string
        if name == "SQL":
            return self._call_sql(s0())
        return None

    def _is_num(self, s: str) -> bool:
        """Check if string represents a number."""
        try:
            float(s)
            return True
        except (ValueError, TypeError):
            return False

    def _call_sql(self, query: str) -> str:
        """Execute a T-SQL statement from REXX and return output."""
        try:
            from ..core.sql_engine import SQLSession

            sess = getattr(self.interpreter, "sql_session", None)
            if sess is None:
                sess = SQLSession()
                self.interpreter.sql_session = sess
            result = sess.run_statement(query)
            self._emit(result)
            return result
        except Exception as e:
            msg = f"❌ REXX SQL error: {e}"
            self._emit(msg)
            return msg


class RexxExit(Exception):
    pass


class RexxError(Exception):
    pass
