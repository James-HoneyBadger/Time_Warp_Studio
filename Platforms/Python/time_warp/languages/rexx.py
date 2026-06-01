"""REXX language executor for Time Warp Studio.

Educational REXX interpreter — whole-program execution.
Implements a teaching subset of Classic REXX (IBM, 1979):

  - SAY expression         — print with newline
  - PULL variable          — read input (from interpreter.input_value)
  - var = expression       — variable assignment (case-insensitive names)
  - IF cond THEN / ELSE    — conditionals (block or single-statement)
  - DO ... END             — bare block
  - DO i = start TO end [BY step] ... END  — counted loop
  - DO WHILE cond ... END  — while loop
  - DO UNTIL cond ... END  — until loop
  - DO FOREVER ... END     — infinite (guarded by MAX_LOOPS)
  - LEAVE / ITERATE        — break / continue
  - SELECT / WHEN / OTHERWISE / END  — switch
  - subroutine definitions: name: / PROCEDURE / RETURN
  - CALL name [args]
  - EXIT [value]
  - PARSE VAR / PARSE VALUE ... WITH ...
  - Built-in string functions: LENGTH, SUBSTR, POS, LEFT, RIGHT, STRIP,
    UPPER, LOWER, REVERSE, COPIES, TRANSLATE, OVERLAY, SPACE, WORD,
    WORDS, WORDPOS, DELWORD, SUBWORD, CHANGESTR, COUNTSTR, DATATYPE,
    CENTER/CENTRE, JUSTIFY, DELSTR, INSERT, LASTPOS
  - Built-in numeric functions: ABS, MAX, MIN, TRUNC, FORMAT, SIGN,
    DIGITS, FUZZ, RANDOM, DATE, TIME
  - Arithmetic operators: + - * / % // ** (integer div, remainder, power)
  - Comparison: = \\= > < >= <= == \\== >> << >>= <<= <>
  - Boolean: & | \\  (AND, OR, NOT)
  - String concatenation: (space) || (concat)
  - Turtle graphics: FORWARD, BACKWARD, LEFT, RIGHT, PENUP, PENDOWN, COLOR,
    SETHEADING, HOME
"""

from __future__ import annotations

import math
import re
import random
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_rexx(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete REXX program and return all output as a string."""
    env = RexxEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Control-flow exceptions
# ---------------------------------------------------------------------------


class _RexxLeave(Exception):
    pass


class _RexxIterate(Exception):
    pass


class _RexxReturn(Exception):
    def __init__(self, value: str = "") -> None:
        self.value = value


class _RexxExit(Exception):
    def __init__(self, value: str = "0") -> None:
        self.value = value


class _RexxError(Exception):
    pass


# ---------------------------------------------------------------------------
# Tokeniser helpers
# ---------------------------------------------------------------------------

_COMMENT_RE = re.compile(r"/\*.*?\*/", re.DOTALL)


def _strip_comments(src: str) -> str:
    return _COMMENT_RE.sub(" ", src)


def _parse_clause(src: str) -> str:
    """Remove leading/trailing whitespace and trailing semicolon."""
    return src.strip().rstrip(";").strip()


# ---------------------------------------------------------------------------
# REXX environment
# ---------------------------------------------------------------------------

MAX_LOOPS = 50_000


class RexxEnvironment:
    """State container and executor for a REXX program."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self._interp = interpreter
        self._turtle = turtle
        self._vars: Dict[str, str] = {}          # global variables (UPPER-case keys)
        self._subs: Dict[str, List[str]] = {}    # subroutine label -> body lines
        self._output_buf: str = ""
        self._output: List[str] = []
        self._input_used = False

    # ------------------------------------------------------------------
    # Output
    # ------------------------------------------------------------------

    def _emit(self, text: str) -> None:
        combined = self._output_buf + text
        lines = combined.split("\n")
        for line in lines[:-1]:
            self._output.append(line)
        self._output_buf = lines[-1]

    def _flush(self) -> None:
        if self._output_buf:
            self._output.append(self._output_buf)
            self._output_buf = ""

    # ------------------------------------------------------------------
    # Variable access (REXX vars are always case-insensitive strings)
    # ------------------------------------------------------------------

    def _get(self, name: str) -> str:
        key = name.upper()
        return self._vars.get(key, key)  # uninitialised var = its own name

    def _set(self, name: str, value: str) -> None:
        self._vars[name.upper()] = str(value)

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        source = _strip_comments(source)
        lines = source.splitlines()
        # First pass: collect subroutines
        self._collect_subs(lines)
        # Second pass: execute main body (lines before first label or until END)
        try:
            self._exec_lines(lines, 0, len(lines))
        except (_RexxExit, _RexxReturn):
            pass
        except _RexxError as e:
            self._emit(f"❌ {e}\n")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}\n")
        self._flush()
        result = [ln for ln in self._output]
        return "\n".join(result) + ("\n" if result else "")

    # ------------------------------------------------------------------
    # Subroutine collection (first-pass)
    # ------------------------------------------------------------------

    def _collect_subs(self, lines: List[str]) -> None:
        """Find all label: definitions and store their bodies."""
        i = 0
        while i < len(lines):
            raw = lines[i].strip()
            m = re.match(r"^([A-Za-z_]\w*)\s*:\s*$", raw)
            if m:
                name = m.group(1).upper()
                body: List[str] = []
                i += 1
                depth = 0
                while i < len(lines):
                    rline = lines[i].strip().upper()
                    # Stop at next top-level label
                    if re.match(r"^[A-Za-z_]\w*\s*:\s*$", lines[i].strip()) and depth == 0:
                        break
                    kw = rline.split()[0] if rline.split() else ""
                    if kw in ("DO", "SELECT"):
                        depth += 1
                    elif kw == "END":
                        if depth == 0:
                            body.append(lines[i])
                            i += 1
                            break
                        depth -= 1
                    body.append(lines[i])
                    i += 1
                self._subs[name] = body
                continue
            i += 1

    # ------------------------------------------------------------------
    # Line-range executor
    # ------------------------------------------------------------------

    def _exec_lines(self, lines: List[str], start: int, end: int) -> str:
        """Execute lines[start:end], return last expression value."""
        i = start
        last = ""
        while i < end:
            raw = lines[i].strip()
            if not raw or raw.startswith("/*") or raw.startswith("--"):
                i += 1
                continue
            # Skip label lines (already collected)
            if re.match(r"^[A-Za-z_]\w*\s*:\s*$", raw):
                i += 1
                continue
            kw = raw.split()[0].upper()
            if kw == "END":
                break
            # Multi-line constructs
            if kw == "DO":
                i = self._exec_do(lines, i, end)
                continue
            if kw == "SELECT":
                i = self._exec_select(lines, i, end)
                continue
            if kw == "IF":
                i = self._exec_if(lines, i, end)
                continue
            # Single-line statements
            last = self._exec_stmt(raw)
            i += 1
        return last

    # ------------------------------------------------------------------
    # DO block / loops
    # ------------------------------------------------------------------

    def _exec_do(self, lines: List[str], start: int, end: int) -> int:
        """Handle DO...END. Returns index after END."""
        raw = lines[start].strip()
        upper = raw.upper()

        # Find matching END
        body_start = start + 1
        body_end = self._find_end(lines, start, end)
        body_lines = lines[body_start:body_end]

        if re.match(r"^DO\s+FOREVER\b", upper, re.I):
            self._run_forever(body_lines)
        elif re.match(r"^DO\s+WHILE\b", upper, re.I):
            m = re.match(r"DO\s+WHILE\s+(.+)", raw, re.I)
            cond = m.group(1).strip() if m else "0"
            self._run_while(cond, body_lines)
        elif re.match(r"^DO\s+UNTIL\b", upper, re.I):
            m = re.match(r"DO\s+UNTIL\s+(.+)", raw, re.I)
            cond = m.group(1).strip() if m else "1"
            self._run_until(cond, body_lines)
        elif re.match(r"^DO\s+[A-Za-z_]\w*\s*=", upper):
            m = re.match(
                r"DO\s+([A-Za-z_]\w*)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+BY\s+(.+?))?$",
                raw, re.I
            )
            if m:
                var = m.group(1)
                start_v = self._to_num(self._eval(m.group(2).strip()))
                end_v = self._to_num(self._eval(m.group(3).strip()))
                step_v = self._to_num(self._eval(m.group(4).strip())) if m.group(4) else 1.0
                self._run_counted(var, start_v, end_v, step_v, body_lines)
            else:
                self._exec_lines(body_lines, 0, len(body_lines))
        elif re.match(r"^DO\s*$", upper):
            self._exec_lines(body_lines, 0, len(body_lines))
        else:
            # DO count
            m = re.match(r"DO\s+(.+)", raw, re.I)
            if m:
                count = int(self._to_num(self._eval(m.group(1).strip())))
                self._run_counted("_DO_CTR", 1, count, 1, body_lines)
            else:
                self._exec_lines(body_lines, 0, len(body_lines))

        return body_end + 1

    def _run_forever(self, body: List[str]) -> None:
        count = 0
        while True:
            if count >= MAX_LOOPS:
                raise _RexxError("DO FOREVER: max iterations exceeded")
            count += 1
            try:
                self._exec_lines(body, 0, len(body))
            except _RexxLeave:
                break
            except _RexxIterate:
                continue

    def _run_while(self, cond: str, body: List[str]) -> None:
        count = 0
        while self._is_true(self._eval(cond)):
            if count >= MAX_LOOPS:
                raise _RexxError("DO WHILE: max iterations exceeded")
            count += 1
            try:
                self._exec_lines(body, 0, len(body))
            except _RexxLeave:
                break
            except _RexxIterate:
                continue

    def _run_until(self, cond: str, body: List[str]) -> None:
        count = 0
        while True:
            if count >= MAX_LOOPS:
                raise _RexxError("DO UNTIL: max iterations exceeded")
            count += 1
            try:
                self._exec_lines(body, 0, len(body))
            except _RexxLeave:
                break
            except _RexxIterate:
                pass
            if self._is_true(self._eval(cond)):
                break

    def _run_counted(
        self, var: str, start: float, stop: float, step: float, body: List[str]
    ) -> None:
        if step == 0:
            raise _RexxError("DO loop BY 0")
        count = 0
        val = start
        while (step > 0 and val <= stop) or (step < 0 and val >= stop):
            if count >= MAX_LOOPS:
                raise _RexxError("DO loop: max iterations exceeded")
            count += 1
            self._set(var, _fmt_num(val))
            try:
                self._exec_lines(body, 0, len(body))
            except _RexxLeave:
                break
            except _RexxIterate:
                pass
            val += step

    # ------------------------------------------------------------------
    # SELECT
    # ------------------------------------------------------------------

    def _exec_select(self, lines: List[str], start: int, end: int) -> int:
        body_end = self._find_end(lines, start, end)
        i = start + 1
        matched = False
        otherwise_start: Optional[int] = None
        otherwise_inline: Optional[str] = None

        while i < body_end:
            raw = lines[i].strip()
            upper = raw.upper()
            kw = upper.split()[0] if upper.split() else ""

            if kw == "WHEN":
                m = re.match(r"WHEN\s+(.+?)\s+THEN\s*(.*)", raw, re.I)
                if m:
                    cond = m.group(1).strip()
                    then_part = m.group(2).strip()
                    if not matched and self._is_true(self._eval(cond)):
                        matched = True
                        if then_part:
                            self._exec_stmt(then_part)
                        else:
                            i += 1
                            # collect until next WHEN/OTHERWISE/END
                            j = i
                            depth = 0
                            while j < body_end:
                                rj = lines[j].strip().upper()
                                kj = rj.split()[0] if rj.split() else ""
                                if kj in ("DO", "SELECT"):
                                    depth += 1
                                elif kj == "END":
                                    if depth == 0:
                                        break
                                    depth -= 1
                                elif kj in ("WHEN", "OTHERWISE") and depth == 0:
                                    break
                                j += 1
                            self._exec_lines(lines, i, j)
                            i = j
                            continue
                i += 1
                continue

            if kw == "OTHERWISE":
                rest = raw[9:].strip()  # text after OTHERWISE keyword
                if rest:
                    otherwise_inline = rest
                    otherwise_start = None
                else:
                    otherwise_start = i + 1
                    otherwise_inline = None
                i += 1
                continue

            i += 1

        if not matched:
            if otherwise_inline:
                self._exec_stmt(otherwise_inline)
            elif otherwise_start is not None:
                self._exec_lines(lines, otherwise_start, body_end)

        return body_end + 1

    # ------------------------------------------------------------------
    # IF
    # ------------------------------------------------------------------

    def _exec_if(self, lines: List[str], start: int, end: int) -> int:
        raw = lines[start].strip()
        # Single-line: IF cond THEN stmt [ELSE stmt]
        m = re.match(r"IF\s+(.+?)\s+THEN\s+(.+?)(?:\s+ELSE\s+(.+))?$", raw, re.I)
        if m:
            cond = m.group(1).strip()
            then_stmt = m.group(2).strip()
            else_stmt = m.group(3).strip() if m.group(3) else None
            if self._is_true(self._eval(cond)):
                self._exec_stmt(then_stmt)
            elif else_stmt:
                self._exec_stmt(else_stmt)
            return start + 1

        # Multi-line: IF cond THEN\n  DO...END\n[ELSE\n  DO...END]
        m2 = re.match(r"IF\s+(.+?)\s+THEN\s*$", raw, re.I)
        if not m2:
            m2 = re.match(r"IF\s+(.+)", raw, re.I)
        cond = m2.group(1).strip() if m2 else "0"

        # Next non-blank line should be DO or single statement
        i = start + 1
        while i < end and not lines[i].strip():
            i += 1
        if i >= end:
            return i

        then_raw = lines[i].strip()
        then_upper = then_raw.upper().split()[0] if then_raw.split() else ""
        condition_true = self._is_true(self._eval(cond))

        if then_upper == "DO":
            then_end = self._find_end(lines, i, end)
            if condition_true:
                self._exec_lines(lines, i + 1, then_end)
            i = then_end + 1
        else:
            if condition_true:
                self._exec_stmt(then_raw)
            i += 1

        # Look for ELSE
        while i < end and not lines[i].strip():
            i += 1
        if i < end and lines[i].strip().upper().startswith("ELSE"):
            else_line = lines[i].strip()
            else_rest = re.sub(r"^ELSE\s*", "", else_line, flags=re.I).strip()
            i += 1
            if else_rest.upper() == "DO":
                else_end = self._find_end(lines, i - 1 + (1 if else_rest else 0), end)
                # find the DO after ELSE
                j = i
                while j < end and not lines[j].strip():
                    j += 1
                if j < end and lines[j].strip().upper().startswith("DO"):
                    else_end = self._find_end(lines, j, end)
                    if not condition_true:
                        self._exec_lines(lines, j + 1, else_end)
                    i = else_end + 1
            elif else_rest:
                if not condition_true:
                    self._exec_stmt(else_rest)
            else:
                # ELSE on its own line
                while i < end and not lines[i].strip():
                    i += 1
                if i < end:
                    else_body = lines[i].strip()
                    if else_body.upper().startswith("DO"):
                        else_end = self._find_end(lines, i, end)
                        if not condition_true:
                            self._exec_lines(lines, i + 1, else_end)
                        i = else_end + 1
                    else:
                        if not condition_true:
                            self._exec_stmt(else_body)
                        i += 1
        return i

    # ------------------------------------------------------------------
    # Find matching END for a DO/SELECT at index start
    # ------------------------------------------------------------------

    def _find_end(self, lines: List[str], start: int, limit: int) -> int:
        depth = 0
        for j in range(start, limit):
            kw = lines[j].strip().upper().split()
            if kw and kw[0] in ("DO", "SELECT"):
                depth += 1
            elif kw and kw[0] == "END":
                depth -= 1
                if depth == 0:
                    return j
        return limit - 1

    # ------------------------------------------------------------------
    # Single-statement executor
    # ------------------------------------------------------------------

    def _exec_stmt(self, raw: str) -> str:
        raw = raw.strip().rstrip(";").strip()
        if not raw:
            return ""
        upper = raw.upper()
        kw = upper.split()[0] if upper.split() else ""

        if kw == "SAY":
            rest = raw[3:].strip() if len(raw) > 3 else ""
            val = self._eval(rest) if rest else ""
            self._emit(str(val) + "\n")
            return str(val)

        if kw == "PULL":
            varname = raw[4:].strip().split()[0] if len(raw) > 4 else "RESULT"
            val = self._get_input()
            self._set(varname, val.upper())  # PULL uppercases
            return val

        if kw == "PARSE":
            self._exec_parse(raw)
            return ""

        if kw in ("CALL",):
            return self._exec_call(raw)

        if kw == "RETURN":
            val = self._eval(raw[6:].strip()) if len(raw) > 6 else ""
            raise _RexxReturn(str(val))

        if kw == "EXIT":
            val = self._eval(raw[4:].strip()) if len(raw) > 4 else "0"
            raise _RexxExit(str(val))

        if kw == "LEAVE":
            raise _RexxLeave()

        if kw == "ITERATE":
            raise _RexxIterate()

        if kw == "NOP":
            return ""

        if kw == "DROP":
            for part in raw[4:].strip().split():
                self._vars.pop(part.upper(), None)
            return ""

        if kw == "TRACE":
            return ""  # ignore tracing

        # Turtle commands
        turtle_result = self._try_turtle(raw)
        if turtle_result is not None:
            return ""

        # Assignment: var = expr
        m = re.match(r"^([A-Za-z_]\w*(?:\.[A-Za-z_]\w*)*)\s*=\s*(.*)$", raw, re.DOTALL)
        if m:
            varname = m.group(1)
            val = self._eval(m.group(2).strip())
            self._set(varname, str(val))
            return str(val)

        # Bare expression / function call
        return str(self._eval(raw))

    # ------------------------------------------------------------------
    # PARSE
    # ------------------------------------------------------------------

    def _exec_parse(self, raw: str) -> None:
        m_var = re.match(r"PARSE\s+VAR\s+(\w+)\s+(.*)", raw, re.I)
        if m_var:
            src = self._get(m_var.group(1))
            self._parse_with(src, m_var.group(2).strip())
            return
        m_val = re.match(r"PARSE\s+VALUE\s+(.+?)\s+WITH\s+(.*)", raw, re.I)
        if m_val:
            src = str(self._eval(m_val.group(1).strip()))
            self._parse_with(src, m_val.group(2).strip())
            return
        m_pull = re.match(r"PARSE\s+PULL\s+(.*)", raw, re.I)
        if m_pull:
            src = self._get_input()
            self._parse_with(src, m_pull.group(1).strip())

    def _parse_with(self, src: str, template: str) -> None:
        """Simple REXX template parsing — space-delimited variables."""
        parts = src.split()
        vars_ = template.split()
        for idx, vname in enumerate(vars_):
            if vname == ".":
                continue  # placeholder
            if idx < len(parts):
                self._set(vname, parts[idx])
            elif idx == len(vars_) - 1 and idx < len(parts):
                # Last var gets the rest
                self._set(vname, " ".join(parts[idx:]))
            else:
                self._set(vname, "")

    # ------------------------------------------------------------------
    # CALL
    # ------------------------------------------------------------------

    def _exec_call(self, raw: str) -> str:
        m = re.match(r"CALL\s+(\w+)\s*(.*)", raw, re.I)
        if not m:
            return ""
        name = m.group(1).upper()
        args_str = m.group(2).strip()
        args = [self._eval(a.strip()) for a in self._split_args(args_str)] if args_str else []

        # Check built-ins first (as procedures)
        if name in ("BEEP", "SLEEP"):
            return ""

        # User subroutines
        if name in self._subs:
            return self._call_sub(name, args)

        return ""

    def _call_sub(self, name: str, args: List[str]) -> str:
        """Execute a user-defined subroutine."""
        body = self._subs.get(name, [])
        saved_vars = dict(self._vars)
        # Pass args as ARG(n) and positional
        for idx, arg in enumerate(args):
            self._set(f"ARG{idx + 1}", str(arg))
        self._set("ARG", " ".join(str(a) for a in args))
        try:
            self._exec_lines(body, 0, len(body))
            result = ""
        except _RexxReturn as r:
            result = r.value
        self._vars = saved_vars
        self._set("RESULT", result)
        return result

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval(self, expr: str) -> str:
        expr = expr.strip()
        if not expr:
            return ""

        # String literal (single-quoted)
        if expr.startswith("'") and expr.endswith("'") and len(expr) >= 2:
            return expr[1:-1]
        # Double-quoted string
        if expr.startswith('"') and expr.endswith('"') and len(expr) >= 2:
            return expr[1:-1]

        # Check for function call: NAME(args)
        m_fn = re.match(r"^([A-Za-z_]\w*)\s*\((.*)?\)$", expr, re.DOTALL)
        if m_fn:
            fn_name = m_fn.group(1).upper()
            fn_args_str = m_fn.group(2).strip() if m_fn.group(2) else ""
            fn_args = [self._eval(a.strip()) for a in self._split_args(fn_args_str)] if fn_args_str else []
            result = self._call_builtin(fn_name, fn_args, expr)
            if result is not None:
                return str(result)

        # Boolean NOT: \expr or ¬expr
        if expr.startswith("\\") or expr.startswith("¬"):
            val = self._eval(expr[1:].strip())
            return "0" if self._is_true(val) else "1"

        # Compound expressions — find lowest-precedence operator
        result = self._eval_compound(expr)
        return result

    def _eval_compound(self, expr: str) -> str:
        """Evaluate compound expression respecting operator precedence."""
        expr = expr.strip()
        if not expr:
            return ""

        # Try operators in ascending precedence order
        # Level 1: & | (boolean)
        for op in ("|", "&"):
            pos = self._rfind_op(expr, op)
            if pos >= 0:
                left = self._eval(expr[:pos].strip())
                right = self._eval(expr[pos + len(op):].strip())
                if op == "|":
                    return "1" if (self._is_true(left) or self._is_true(right)) else "0"
                else:
                    return "1" if (self._is_true(left) and self._is_true(right)) else "0"

        # Level 2: comparisons
        for op in ("==", r"\==", ">=", "<=", ">>", "<<", ">>=", "<<=", "<>", "=", r"\=", ">", "<"):
            pos = self._rfind_op(expr, op)
            if pos >= 0:
                left = self._eval(expr[:pos].strip())
                right = self._eval(expr[pos + len(op):].strip())
                return "1" if self._compare(left, right, op) else "0"

        # Level 3: string concatenation ||
        pos = self._rfind_op(expr, "||")
        if pos >= 0:
            left = self._eval(expr[:pos].strip())
            right = self._eval(expr[pos + 2:].strip())
            return str(left) + str(right)

        # Level 4: + -
        for op in ("+", "-"):
            pos = self._rfind_op_arith(expr, op)
            if pos >= 0:
                left = self._eval(expr[:pos].strip())
                right = self._eval(expr[pos + 1:].strip())
                try:
                    lv = self._to_num(left)
                    rv = self._to_num(right)
                    result = lv + rv if op == "+" else lv - rv
                    return _fmt_num(result)
                except (ValueError, TypeError):
                    return left + right if op == "+" else left

        # Level 5: * / % //
        for op in ("//", "%", "*", "/"):
            pos = self._rfind_op(expr, op)
            if pos >= 0:
                left = self._eval(expr[:pos].strip())
                right = self._eval(expr[pos + len(op):].strip())
                try:
                    lv = self._to_num(left)
                    rv = self._to_num(right)
                    if rv == 0:
                        raise _RexxError("Division by zero")
                    if op == "*":
                        return _fmt_num(lv * rv)
                    elif op == "/":
                        return _fmt_num(lv / rv)
                    elif op == "%":
                        return _fmt_num(int(lv / rv))
                    elif op == "//":
                        return _fmt_num(lv - int(lv / rv) * rv)
                except (ValueError, TypeError):
                    return "0"

        # Level 6: ** (right-associative)
        pos = expr.find("**")
        if pos > 0:
            left = self._eval(expr[:pos].strip())
            right = self._eval(expr[pos + 2:].strip())
            try:
                return _fmt_num(self._to_num(left) ** self._to_num(right))
            except Exception:
                return "0"

        # Parentheses
        if expr.startswith("(") and expr.endswith(")"):
            inner = self._extract_parens(expr)
            if inner is not None:
                return self._eval(inner)

        # Unary minus
        if expr.startswith("-"):
            val = self._to_num(self._eval(expr[1:].strip()))
            return _fmt_num(-val)

        # Number literal
        try:
            v = float(expr)
            return _fmt_num(v)
        except ValueError:
            pass

        # Variable
        if re.match(r"^[A-Za-z_]\w*$", expr):
            return self._get(expr)

        # Abuttal concatenation (a b) — space-separated tokens
        parts = expr.split()
        if len(parts) > 1:
            return " ".join(self._eval(p) for p in parts)

        # Bare string
        return expr

    def _extract_parens(self, expr: str) -> Optional[str]:
        """Return inner content of a balanced (…) wrapper, or None."""
        depth = 0
        for i, c in enumerate(expr):
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0:
                    if i == len(expr) - 1:
                        return expr[1:-1]
                    return None
        return None

    def _rfind_op(self, expr: str, op: str) -> int:
        """Find rightmost occurrence of op at depth 0, not inside quotes."""
        depth = 0
        in_sq = in_dq = False
        pos = len(expr) - len(op)
        while pos >= 0:
            c = expr[pos]
            if in_sq:
                if c == "'":
                    in_sq = False
                pos -= 1
                continue
            if in_dq:
                if c == '"':
                    in_dq = False
                pos -= 1
                continue
            if c == "'":
                in_sq = True
                pos -= 1
                continue
            if c == '"':
                in_dq = True
                pos -= 1
                continue
            sub = expr[pos:pos + len(op)]
            # Count brackets from end to current pos
            after = expr[pos:]
            bd = 0
            for ch in after:
                if ch in "([":
                    bd += 1
                elif ch in ")]":
                    bd -= 1
            if bd == 0 and sub == op:
                # Make sure it's not part of a longer operator
                before_c = expr[pos - 1] if pos > 0 else " "
                after_c = expr[pos + len(op)] if pos + len(op) < len(expr) else " "
                # Skip == when looking for =
                if op == "=" and (before_c in "!\\<>=¬" or after_c == "="):
                    pos -= 1
                    continue
                if op == ">" and (after_c in ">="):
                    pos -= 1
                    continue
                if op == "<" and (after_c in "<="):
                    pos -= 1
                    continue
                if op == "|" and (after_c == "|" or before_c == "|"):
                    pos -= 1
                    continue
                if op == "*" and (before_c == "*" or after_c == "*"):
                    pos -= 1
                    continue
                if op == "/" and (before_c == "/" or after_c == "/"):
                    pos -= 1
                    continue
                return pos
            pos -= 1
        return -1

    def _rfind_op_arith(self, expr: str, op: str) -> int:
        """Find rightmost + or - at depth 0, excluding unary and ** context."""
        depth = 0
        in_sq = in_dq = False
        i = len(expr) - 1
        while i >= 1:
            c = expr[i]
            if in_sq:
                if c == "'":
                    in_sq = False
                i -= 1
                continue
            if in_dq:
                if c == '"':
                    in_dq = False
                i -= 1
                continue
            if c == "'":
                in_sq = True
            elif c == '"':
                in_dq = True
            elif c in ")]":
                depth += 1
            elif c in "([":
                depth -= 1
            elif c == op and depth == 0:
                before = expr[i - 1] if i > 0 else " "
                after = expr[i + 1] if i + 1 < len(expr) else " "
                # Skip unary or part of ** ->
                if before in "=*+\\-/<>|&(," and not before.isalnum() and before != ")":
                    i -= 1
                    continue
                return i
            i -= 1
        return -1

    # ------------------------------------------------------------------
    # Comparison
    # ------------------------------------------------------------------

    def _compare(self, left: str, right: str, op: str) -> bool:
        # Numeric comparison if both numeric
        try:
            lv, rv = float(left), float(right)
            is_num = True
        except (ValueError, TypeError):
            is_num = False

        if op == "=" or op == "==":
            return (lv == rv) if is_num else (left.strip() == right.strip())
        if op in (r"\=", r"\==", "<>"):
            return (lv != rv) if is_num else (left.strip() != right.strip())
        if op == ">" or op == ">>":
            return (lv > rv) if is_num else (left > right)
        if op == "<" or op == "<<":
            return (lv < rv) if is_num else (left < right)
        if op in (">=", ">>="):
            return (lv >= rv) if is_num else (left >= right)
        if op in ("<=", "<<="):
            return (lv <= rv) if is_num else (left <= right)
        return False

    # ------------------------------------------------------------------
    # Built-in functions
    # ------------------------------------------------------------------

    def _call_builtin(self, name: str, args: List[str], raw: str) -> Optional[str]:  # noqa: C901
        def a(n: int, default: str = "") -> str:
            return args[n] if n < len(args) else default

        if name == "LENGTH":
            return str(len(a(0)))
        if name == "SUBSTR":
            s = a(0)
            start = max(0, int(self._to_num(a(1, "1"))) - 1)
            length = int(self._to_num(a(2, str(len(s))))) if len(args) > 2 else None
            result = s[start:start + length] if length is not None else s[start:]
            pad = a(3, " ")
            if length is not None and len(result) < length:
                result = result + pad * (length - len(result))
            return result
        if name == "POS":
            needle = a(0)
            haystack = a(1)
            start = int(self._to_num(a(2, "1"))) - 1
            idx = haystack.find(needle, start)
            return str(idx + 1 if idx >= 0 else 0)
        if name == "LASTPOS":
            needle = a(0)
            haystack = a(1)
            idx = haystack.rfind(needle)
            return str(idx + 1 if idx >= 0 else 0)
        if name == "LEFT":
            s = a(0)
            n = int(self._to_num(a(1, "0")))
            pad = a(2, " ")
            return (s[:n] + pad * n)[:n]
        if name == "RIGHT":
            s = a(0)
            n = int(self._to_num(a(1, "0")))
            pad = a(2, " ")
            return (pad * n + s)[-n:] if n else ""
        if name in ("STRIP",):
            s = a(0)
            opt = a(1, "B").upper()
            char = a(2, " ")
            if opt == "B":
                return s.strip(char)
            elif opt == "L":
                return s.lstrip(char)
            elif opt == "T":
                return s.rstrip(char)
            return s.strip(char)
        if name in ("UPPER",):
            return a(0).upper()
        if name in ("LOWER",):
            return a(0).lower()
        if name == "REVERSE":
            return a(0)[::-1]
        if name == "COPIES":
            s = a(0)
            n = int(self._to_num(a(1, "0")))
            return s * n
        if name == "TRANSLATE":
            src = a(0)
            out_tbl = a(1, "")
            in_tbl = a(2, "")
            if in_tbl and out_tbl:
                table = str.maketrans(in_tbl, out_tbl)
                return src.translate(table)
            return src.upper()
        if name in ("CENTER", "CENTRE"):
            s = a(0)
            n = int(self._to_num(a(1, str(len(s)))))
            pad = a(2, " ")
            return s.center(n, pad[0] if pad else " ")
        if name == "SPACE":
            s = a(0)
            n = int(self._to_num(a(1, "1")))
            pad = a(2, " ")
            words = s.split()
            return (pad * n).join(words)
        if name == "WORD":
            s = a(0)
            n = int(self._to_num(a(1, "1"))) - 1
            words = s.split()
            return words[n] if 0 <= n < len(words) else ""
        if name == "WORDS":
            return str(len(a(0).split()))
        if name == "WORDPOS":
            needle = a(0)
            haystack = a(1)
            start = int(self._to_num(a(2, "1"))) - 1
            words = haystack.split()
            for i in range(start, len(words)):
                if words[i] == needle:
                    return str(i + 1)
            return "0"
        if name == "SUBWORD":
            s = a(0)
            start = int(self._to_num(a(1, "1"))) - 1
            n = int(self._to_num(a(2, "0"))) if len(args) > 2 else 0
            words = s.split()
            if n:
                return " ".join(words[start:start + n])
            return " ".join(words[start:])
        if name == "DELWORD":
            s = a(0)
            start = int(self._to_num(a(1, "1"))) - 1
            n = int(self._to_num(a(2, "1")))
            words = s.split()
            del words[start:start + n]
            return " ".join(words)
        if name == "CHANGESTR":
            needle = a(0)
            haystack = a(1)
            replacement = a(2, "")
            return haystack.replace(needle, replacement)
        if name == "COUNTSTR":
            needle = a(0)
            haystack = a(1)
            return str(haystack.count(needle))
        if name == "INSERT":
            ins = a(0)
            tgt = a(1)
            n = int(self._to_num(a(2, "0")))
            return tgt[:n] + ins + tgt[n:]
        if name == "DELSTR":
            s = a(0)
            start = int(self._to_num(a(1, "1"))) - 1
            n = int(self._to_num(a(2, str(len(s))))) if len(args) > 2 else len(s)
            return s[:start] + s[start + n:]
        if name == "OVERLAY":
            new = a(0)
            tgt = a(1)
            n = int(self._to_num(a(2, "1"))) - 1
            pad = a(3, " ")
            while len(tgt) < n:
                tgt += pad
            return tgt[:n] + new + tgt[n + len(new):]
        if name == "DATATYPE":
            s = a(0)
            opt = a(1, "").upper()
            if not opt or opt == "N":
                try:
                    float(s)
                    return "1"
                except ValueError:
                    return "0" if opt else "CHAR"
            if opt == "W":
                try:
                    int(s)
                    return "1"
                except ValueError:
                    return "0"
            if opt == "A":
                return "1" if s.isalpha() else "0"
            if opt == "U":
                return "1" if s.isupper() else "0"
            if opt == "L":
                return "1" if s.islower() else "0"
            return "0"
        if name == "ABS":
            return _fmt_num(abs(self._to_num(a(0))))
        if name == "MAX":
            return _fmt_num(max(self._to_num(x) for x in args))
        if name == "MIN":
            return _fmt_num(min(self._to_num(x) for x in args))
        if name == "TRUNC":
            n = int(self._to_num(a(1, "0")))
            v = self._to_num(a(0))
            if n == 0:
                return str(int(v))
            factor = 10 ** n
            return str(int(v * factor) / factor)
        if name == "FORMAT":
            v = self._to_num(a(0))
            before = int(self._to_num(a(1, "0"))) if len(args) > 1 else 0
            after = int(self._to_num(a(2, "0"))) if len(args) > 2 else 0
            fmt = f"{v:{before}.{after}f}" if (before or after) else _fmt_num(v)
            return fmt
        if name == "SIGN":
            v = self._to_num(a(0))
            return "1" if v > 0 else ("-1" if v < 0 else "0")
        if name == "RANDOM":
            lo = int(self._to_num(a(0, "0")))
            hi = int(self._to_num(a(1, "999")))
            return str(random.randint(lo, hi))
        if name == "SQRT":
            return _fmt_num(math.sqrt(self._to_num(a(0))))
        if name == "DATE":
            import datetime
            return datetime.date.today().strftime("%d %b %Y")
        if name == "TIME":
            import datetime
            return datetime.datetime.now().strftime("%H:%M:%S")
        if name in ("SAY",):
            self._emit(str(a(0)) + "\n")
            return ""
        if name == "SYMBOL":
            k = a(0).upper()
            if k in self._vars:
                return "VAR"
            return "LIT"
        if name == "VALUE":
            return self._get(a(0))
        if name == "ARG":
            n = int(self._to_num(a(0, "1"))) if args else 1
            return self._get(f"ARG{n}")
        if name == "RESULT":
            return self._get("RESULT")
        if name == "C2X":
            return a(0).encode().hex().upper()
        if name == "X2C":
            try:
                return bytes.fromhex(a(0)).decode()
            except Exception:
                return ""
        if name == "D2X":
            try:
                return format(int(self._to_num(a(0))), "X")
            except Exception:
                return "0"
        if name == "X2D":
            try:
                return str(int(a(0), 16))
            except Exception:
                return "0"
        if name == "C2D":
            s = a(0)
            return str(ord(s[0])) if s else "0"
        if name == "D2C":
            try:
                return chr(int(self._to_num(a(0))))
            except Exception:
                return ""
        if name == "VERIFY":
            s = a(0)
            ref = a(1)
            opt = a(2, "N").upper()
            start = int(self._to_num(a(3, "1"))) - 1
            for i, ch in enumerate(s[start:], start + 1):
                if opt == "N" and ch not in ref:
                    return str(i)
                if opt == "M" and ch in ref:
                    return str(i)
            return "0"
        return None

    # ------------------------------------------------------------------
    # Turtle
    # ------------------------------------------------------------------

    def _try_turtle(self, raw: str) -> Optional[bool]:
        t = self._turtle
        upper = raw.upper().split()
        if not upper:
            return None
        kw = upper[0]
        rest = raw.split()[1:] if len(raw.split()) > 1 else []

        def num(i: int, default: float = 0) -> float:
            try:
                return float(self._eval(rest[i]))
            except Exception:
                return default

        if kw in ("FORWARD", "FD"):
            t.forward(num(0))
            return True
        if kw in ("BACKWARD", "BACK", "BK"):
            t.backward(num(0))
            return True
        if kw == "LEFT":
            # Check if numeric (turtle left) vs string comparison
            if rest and re.match(r"^[\d.]+$", rest[0]):
                t.left(num(0))
                return True
        if kw == "RIGHT":
            if rest and re.match(r"^[\d.]+$", rest[0]):
                t.right(num(0))
                return True
        if kw in ("PENUP", "PU"):
            t.pen_up()
            return True
        if kw in ("PENDOWN", "PD"):
            t.pen_down()
            return True
        if kw == "HOME":
            t.home()
            return True
        if kw == "SETHEADING":
            t.set_heading(num(0))
            return True
        if kw == "COLOR":
            r, g, b = int(num(0)), int(num(1)), int(num(2))
            t.set_color(r, g, b)
            return True
        return None

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _split_args(self, s: str) -> List[str]:
        """Split comma-separated args respecting parens and quotes."""
        parts: List[str] = []
        depth = 0
        in_sq = in_dq = False
        buf: List[str] = []
        for c in s:
            if in_sq:
                buf.append(c)
                if c == "'":
                    in_sq = False
            elif in_dq:
                buf.append(c)
                if c == '"':
                    in_dq = False
            elif c == "'":
                in_sq = True
                buf.append(c)
            elif c == '"':
                in_dq = True
                buf.append(c)
            elif c in "([":
                depth += 1
                buf.append(c)
            elif c in ")]":
                depth -= 1
                buf.append(c)
            elif c == "," and depth == 0:
                parts.append("".join(buf).strip())
                buf = []
            else:
                buf.append(c)
        if buf:
            parts.append("".join(buf).strip())
        return parts

    def _to_num(self, s: Any) -> float:
        try:
            return float(str(s).strip())
        except (ValueError, TypeError):
            return 0.0

    def _is_true(self, val: str) -> bool:
        v = str(val).strip().upper()
        if v in ("", "0", "FALSE", "NO"):
            return False
        try:
            return float(v) != 0
        except ValueError:
            return bool(v)

    def _get_input(self) -> str:
        if not self._input_used:
            self._input_used = True
            try:
                return str(self._interp.input_value)
            except Exception:
                return "4"
        return ""


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _fmt_num(v: float) -> str:
    """Format a number: integer if whole, else minimal float."""
    if isinstance(v, bool):
        return "1" if v else "0"
    if v == int(v) and abs(v) < 1e15:
        return str(int(v))
    s = f"{v:.10g}"
    return s
