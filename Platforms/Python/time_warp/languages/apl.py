"""APL language executor for Time Warp Studio.

Educational APL interpreter — whole-program execution.
Implements a teaching subset of APL/J-inspired array language:
  ← assignment
  ⍳ iota (1..n)
  ⍴ shape/reshape
  ⌽ reverse
  ⍉ transpose
  ⌈ ceil / max (dyadic)
  ⌊ floor / min (dyadic)
  + - × ÷ * (arithmetic, scalar-extended)
  / reduce:  +/ 1 2 3 → 6
  ⍋ grade-up (order indices)
  ⍒ grade-down
  ∊ membership (dyadic)
  ≡ match
  ≢ tally (count elements)
  ⎕← output (display)
  ⍝ comment
  :If :ElseIf :Else :EndIf
  :For var :In arr ... :EndFor
  :While cond ... :EndWhile
  :Repeat ... :Until cond
  ↑ take / ↓ drop
  , ravel / catenate
  ⌹ matrix inverse (approximation)
  Numbers and string literals
  Dfns:  {left_arg ⍺ right_arg ⍵ expr}
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..core.turtle_state import TurtleState


def execute_apl(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute an APL program and return all output."""
    env = APLEnvironment(interpreter, turtle)
    return env.run(source)


class APLEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._vars: dict[str, Any] = {}
        self._fns: dict[str, str] = {}  # named dfns

    def _emit(self, text: str):
        self._output.append(str(text))

    def run(self, source: str) -> str:
        try:
            lines = source.splitlines()
            self._exec_lines(lines, 0, len(lines))
        except APLError as e:
            self._emit(f"❌ APL error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    # ------------------------------------------------------------------
    # Line executor
    # ------------------------------------------------------------------

    def _exec_lines(self, lines: list[str], start: int, end: int) -> int:
        i = start
        while i < end:
            line = lines[i].rstrip()
            # Skip comments
            comment_idx = line.find("⍝")
            if comment_idx >= 0:
                line = line[:comment_idx]
            line = line.strip()
            if not line:
                i += 1
                continue

            upper = line.upper()

            # :If / :ElseIf / :Else / :EndIf
            if upper.startswith(":IF "):
                then_end, else_ranges, after = self._scan_if_apl(lines, i)
                cond = self._eval_expr(line[3:].strip())
                if _apl_truthy(cond):
                    self._exec_lines(lines, i + 1, then_end)
                else:
                    executed = False
                    for (ei_cond_str, es, ee) in else_ranges:
                        if ei_cond_str is None:  # :Else block
                            if not executed:
                                self._exec_lines(lines, es, ee)
                            break
                        elif not executed:
                            c = self._eval_expr(ei_cond_str)
                            if _apl_truthy(c):
                                self._exec_lines(lines, es, ee)
                                executed = True
                i = after
                continue

            # :For var :In arr
            m = re.match(r"^:For\s+(\w+)\s+:In\s+(.+)$", line, re.IGNORECASE)
            if m:
                var = m.group(1)
                arr = self._eval_expr(m.group(2).strip())
                for_end = self._find_end_block(lines, i + 1, ":EndFor")
                items = arr if isinstance(arr, list) else [arr]
                for item in items:
                    self._vars[var] = item
                    self._exec_lines(lines, i + 1, for_end)
                i = for_end + 1
                continue

            # :While
            m = re.match(r"^:While\s+(.+)$", line, re.IGNORECASE)
            if m:
                cond_str = m.group(1).strip()
                while_end = self._find_end_block(lines, i + 1, ":EndWhile")
                count = 0
                while _apl_truthy(self._eval_expr(cond_str)) and count < 100000:
                    self._exec_lines(lines, i + 1, while_end)
                    count += 1
                i = while_end + 1
                continue

            # :Repeat ... :Until
            if upper.startswith(":REPEAT"):
                repeat_end = self._find_end_block(lines, i + 1, ":Until")
                count = 0
                while count < 100000:
                    self._exec_lines(lines, i + 1, repeat_end)
                    count += 1
                    until_cond = lines[repeat_end].strip()
                    m = re.match(r"^:Until\s+(.+)$", until_cond, re.IGNORECASE)
                    if m and _apl_truthy(self._eval_expr(m.group(1).strip())):
                        break
                i = repeat_end + 1
                continue

            # Skip block-end markers
            if re.match(r"^:(EndIf|EndFor|EndWhile|Else|ElseIf)\b", line, re.IGNORECASE):
                i += 1
                continue

            # ⎕← expr  (display)
            m = re.match(r"^⎕←\s*(.+)$", line)
            if m:
                val = self._eval_expr(m.group(1).strip())
                self._emit(_apl_format(val))
                i += 1
                continue

            # var ← expr  (assignment)
            m = re.match(r"^(\w+)\s*←\s*(.+)$", line)
            if m:
                val = self._eval_expr(m.group(2).strip())
                self._vars[m.group(1)] = val
                i += 1
                continue

            # Named dfn: name ← { body }
            m = re.match(r"^(\w+)\s*←\s*\{(.+)\}$", line)
            if m:
                self._fns[m.group(1)] = m.group(2).strip()
                i += 1
                continue

            # Bare expression (implicit output if returns value)
            val = self._eval_expr(line)
            if val is not None:
                self._emit(_apl_format(val))
            i += 1
        return end

    def _scan_if_apl(self, lines: list[str], start: int) -> tuple[int, list, int]:
        """Return (then_end, [(elseif_cond|None, start, end)...], after)"""
        depth = 0
        i = start + 1
        then_end = start + 1
        else_ranges = []
        current_else_start = None
        current_cond = None
        while i < len(lines):
            s = lines[i].strip().upper()
            if s.startswith(":IF "):
                depth += 1
            elif s == ":ENDIF" and depth > 0:
                depth -= 1
            elif depth == 0:
                if s.startswith(":ELSEIF "):
                    if current_else_start is not None:
                        else_ranges.append((current_cond, current_else_start, i))
                    else:
                        then_end = i
                    m = re.match(r"^:ElseIf\s+(.+)$", lines[i].strip(), re.IGNORECASE)
                    current_cond = m.group(1) if m else None
                    current_else_start = i + 1
                elif s == ":ELSE":
                    if current_else_start is not None:
                        else_ranges.append((current_cond, current_else_start, i))
                    else:
                        then_end = i
                    current_cond = None
                    current_else_start = i + 1
                elif s == ":ENDIF":
                    if current_else_start is not None:
                        else_ranges.append((current_cond, current_else_start, i))
                    else:
                        then_end = i
                    return then_end, else_ranges, i + 1
            i += 1
        return then_end, else_ranges, i

    def _find_end_block(self, lines: list[str], start: int, end_marker: str) -> int:
        depth = 0
        upper_marker = end_marker.upper()
        i = start
        while i < len(lines):
            s = lines[i].strip().upper()
            # Rough nesting for :If/:For/:While/:Repeat
            if re.match(r"^:(IF|FOR|WHILE|REPEAT)\b", s):
                depth += 1
            if s.startswith(upper_marker.lstrip(":")):
                if depth == 0:
                    return i
                depth -= 1
            i += 1
        return i

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return None

        # Number scalar
        try:
            return int(expr)
        except ValueError:
            pass
        try:
            return float(expr)
        except ValueError:
            pass

        # Negative (APL uses ¯ for high minus)
        if expr.startswith("¯"):
            try:
                return -float(expr[1:])
            except ValueError:
                pass

        # String
        m = re.match(r"^'((?:[^']|'')*)'$", expr)
        if m:
            return m.group(1).replace("''", "'")

        # Boolean
        if expr == "1": return 1
        if expr == "0": return 0

        # Vector literal: numbers separated by spaces
        m = re.match(r"^([\d¯.\s]+)$", expr)
        if m and " " in expr:
            parts = expr.split()
            try:
                return [_apl_num(p) for p in parts]
            except ValueError:
                pass

        # Parenthesised
        if expr.startswith("(") and expr.endswith(")"):
            return self._eval_expr(expr[1:-1].strip())

        # Assignment: name ← expr
        m = re.match(r"^(\w+)\s*←\s*(.+)$", expr)
        if m:
            val = self._eval_expr(m.group(2).strip())
            self._vars[m.group(1)] = val
            return val

        # ⎕← output
        m = re.match(r"^⎕←\s*(.+)$", expr)
        if m:
            val = self._eval_expr(m.group(1).strip())
            self._emit(_apl_format(val))
            return val

        # Dfn call: fn_name args or left_arg fn_name right_arg
        # Operators applied to functions: fn / arr  (reduce)
        # We evaluate right-to-left as in APL

        result = self._eval_apl(expr)
        return result

    def _eval_apl(self, expr: str) -> Any:
        """Evaluate an APL expression right-to-left."""
        expr = expr.strip()
        if not expr:
            return None

        # Number
        try:
            return int(expr)
        except ValueError:
            pass
        try:
            return float(expr)
        except ValueError:
            pass

        if expr.startswith("¯"):
            try:
                return -float(expr[1:])
            except ValueError:
                pass

        # String
        m = re.match(r"^'((?:[^']|'')*)'$", expr)
        if m:
            return m.group(1).replace("''", "'")

        # Parenthesised
        if expr.startswith("(") and expr.endswith(")"):
            inner = expr[1:-1].strip()
            # Could be a vector: (1 2 3)
            if " " in inner and re.match(r"^[¯\d.\s]+$", inner):
                return [_apl_num(p) for p in inner.split()]
            return self._eval_apl(inner)

        # Space-separated vector (must come before variable lookup)
        tokens = _apl_tokenize(expr)
        if len(tokens) > 1:
            return self._eval_tokens(tokens)

        # Variable / function
        if re.match(r"^\w+$", expr):
            if expr in self._vars:
                return self._vars[expr]
            if expr in self._fns:
                return self._call_dfn(self._fns[expr], None, None)
        return expr

    def _eval_tokens(self, tokens: list[str]) -> Any:
        """Evaluate a token list right-to-left (simplified)."""
        # Look for dyadic operator: left op right
        # Handle reduce: fn/ arr  →  reduce fn over arr
        # We find the rightmost operator (lowest precedence)
        # Simple approach: scan left-to-right for the highest-precedence structure

        # Single token
        if len(tokens) == 1:
            return self._eval_apl(tokens[0])

        # Reduce operator: fn/ arr  or  fn⌿ arr
        m = re.match(r"^(.+)[/⌿]$", tokens[0])
        if m and len(tokens) >= 2:
            fn_tok = m.group(1)
            arr = self._eval_tokens(tokens[1:])
            return self._reduce(fn_tok, arr)

        # Scan operator: fn\ arr
        m = re.match(r"^(.+)\\$", tokens[0])
        if m and len(tokens) >= 2:
            fn_tok = m.group(1)
            arr = self._eval_tokens(tokens[1:])
            return self._scan(fn_tok, arr)

        # Monadic APL function followed by array
        if tokens[0] in _MONADIC_FUNS and len(tokens) >= 2:
            arr = self._eval_tokens(tokens[1:])
            return self._apply_monadic(tokens[0], arr)

        # Dyadic: left fn right — find operator
        # Try each known function as dyadic separator
        for i, tok in enumerate(tokens):
            if i == 0 or i == len(tokens) - 1:
                continue
            if tok in _DYADIC_FUNS:
                left = self._eval_tokens(tokens[:i])
                right = self._eval_tokens(tokens[i+1:])
                return self._apply_dyadic(tok, left, right)

        # Numeric vector
        try:
            return [_apl_num(t) for t in tokens]
        except Exception:
            pass

        # Just evaluate each and return last
        result = None
        for t in tokens:
            result = self._eval_apl(t)
        return result

    def _apply_monadic(self, fn: str, arr: Any) -> Any:
        """Apply monadic (unary) APL function."""
        if isinstance(arr, list):
            if fn == "⍳":
                # ⍳ of a vector = indices of array
                if isinstance(arr, list):
                    return list(range(1, len(arr) + 1))
                return list(range(1, int(arr) + 1))
            if fn in ("+", "-", "×", "÷", "*", "⌈", "⌊", "!", "≢", "|"):
                return [self._apply_monadic(fn, x) for x in arr]
            if fn == "⌽": return arr[::-1]
            if fn == "⍉":
                # Transpose: simple 2D case
                if arr and isinstance(arr[0], list):
                    return [list(row) for row in zip(*arr)]
                return arr
            if fn == "⍋": return sorted(range(len(arr)), key=lambda i: arr[i])
            if fn == "⍒": return sorted(range(len(arr)), key=lambda i: -arr[i] if isinstance(arr[i], (int, float)) else arr[i], reverse=False)
            if fn == ",": return arr  # ravel is identity on 1D
            if fn == "≢": return len(arr)
            if fn == "⍴": return [len(arr)]
            if fn == "~": return [0 if x else 1 for x in arr]
        else:
            n = arr
            if fn == "⍳": return list(range(1, int(n) + 1))
            if fn == "+": return n
            if fn == "-": return -n
            if fn == "|": return abs(n)
            if fn == "×": return 0 if n == 0 else (1 if n > 0 else -1)
            if fn == "÷": return 1 / n if n else float("inf")
            if fn == "*": return math.exp(n)
            if fn == "⌈": return math.ceil(n)
            if fn == "⌊": return math.floor(n)
            if fn == "!": return math.factorial(int(n))
            if fn == "≢": return 1
            if fn == "⍴": return []
            if fn == ",": return [n]
            if fn == "~": return 0 if n else 1
        return arr

    def _apply_dyadic(self, fn: str, left: Any, right: Any) -> Any:
        """Apply dyadic (binary) APL function with scalar extension."""
        # Functions that manage their own list arguments — skip generic expansion
        if fn in ("⍴", "∊", ",", "↑", "↓", "⍳"):
            pass  # fall through to per-function handlers below
        elif isinstance(left, list) and isinstance(right, list):
            if len(left) != len(right):
                if len(left) == 1:
                    left = left * len(right)
                elif len(right) == 1:
                    right = right * len(left)
                else:
                    raise APLError(f"Length mismatch: {len(left)} vs {len(right)}")
            return [self._apply_dyadic(fn, l, r) for l, r in zip(left, right)]
        elif isinstance(left, list):
            return [self._apply_dyadic(fn, l, right) for l in left]
        elif isinstance(right, list):
            return [self._apply_dyadic(fn, left, r) for r in right]
        # Scalars
        if fn == "+": return left + right
        if fn == "-": return left - right
        if fn == "×": return left * right
        if fn == "÷": return left / right if right else float("inf")
        if fn == "*": return left ** right
        if fn == "⌈": return max(left, right)
        if fn == "⌊": return min(left, right)
        if fn == "=": return 1 if left == right else 0
        if fn == "≠": return 1 if left != right else 0
        if fn == "<": return 1 if left < right else 0
        if fn == ">": return 1 if left > right else 0
        if fn == "≤": return 1 if left <= right else 0
        if fn == "≥": return 1 if left >= right else 0
        if fn == "|": return right % left if left else right
        if fn == "!":
            from math import comb
            return comb(int(right), int(left))
        if fn == "⍴":
            # Reshape: left is shape, right is data
            shape = left if isinstance(left, list) else [left]
            data = right if isinstance(right, list) else [right]
            total = 1
            for s in shape:
                total *= int(s)
            # Cycle data to fill shape
            result = [data[i % len(data)] for i in range(total)]
            if len(shape) == 1:
                return result
            # Multi-dim reshape (simplified, return flat)
            return result
        if fn == "∊":
            return 1 if left in (right if isinstance(right, list) else [right]) else 0
        if fn == ",":
            l_list = left if isinstance(left, list) else [left]
            r_list = right if isinstance(right, list) else [right]
            return l_list + r_list
        if fn == "↑": return right[:int(left)] if isinstance(right, list) else right
        if fn == "↓": return right[int(left):] if isinstance(right, list) else right
        if fn == "⌹":
            # Simple: scalar inverse
            return 1 / right if right else 0
        return None

    def _reduce(self, fn: str, arr: Any) -> Any:
        """Apply fn/ over arr."""
        if not isinstance(arr, list) or len(arr) == 0:
            return arr
        acc = arr[0]
        for x in arr[1:]:
            acc = self._apply_dyadic(fn, acc, x)
        return acc

    def _scan(self, fn: str, arr: Any) -> Any:
        r"""Apply fn\ (scan) over arr."""
        if not isinstance(arr, list) or len(arr) == 0:
            return arr
        result = [arr[0]]
        acc = arr[0]
        for x in arr[1:]:
            acc = self._apply_dyadic(fn, acc, x)
            result.append(acc)
        return result

    def _call_dfn(self, body: str, alpha: Any, omega: Any) -> Any:
        """Execute a dfn {body} with ⍺ (left) and ⍵ (right) arguments."""
        old_vars = self._vars.copy()
        if alpha is not None:
            self._vars["⍺"] = alpha
        if omega is not None:
            self._vars["⍵"] = omega
        result = self._eval_expr(body)
        self._vars = old_vars
        return result


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_MONADIC_FUNS = set(["⍳", "+", "-", "×", "÷", "*", "⌈", "⌊", "!", "≢", "⌽", "⍉", "⍋", "⍒", ",", "⍴", "~", "|"])
_DYADIC_FUNS = set(["+", "-", "×", "÷", "*", "⌈", "⌊", "=", "≠", "<", ">", "≤", "≥",
                    "|", "!", "⍴", "∊", ",", "↑", "↓", "⌹"])


def _apl_tokenize(expr: str) -> list[str]:
    """Tokenize APL expression into symbols and atoms."""
    tokens = []
    i = 0
    while i < len(expr):
        ch = expr[i]
        if ch == " ":
            i += 1
            continue
        if ch == "'":
            j = i + 1
            s = []
            while j < len(expr):
                if expr[j] == "'" and j + 1 < len(expr) and expr[j+1] == "'":
                    s.append("'")
                    j += 2
                elif expr[j] == "'":
                    j += 1
                    break
                else:
                    s.append(expr[j])
                    j += 1
            tokens.append("'" + "".join(s) + "'")
            i = j
            continue
        if ch == "(" :
            depth = 1
            j = i + 1
            while j < len(expr) and depth > 0:
                if expr[j] == "(": depth += 1
                elif expr[j] == ")": depth -= 1
                j += 1
            tokens.append(expr[i:j])
            i = j
            continue
        # APL symbol
        if ord(ch) > 127:
            # Check for compound: fn/ fn\ fn⌿
            j = i + 1
            while j < len(expr) and expr[j] in "/\\⌿":
                j += 1
            tokens.append(expr[i:j])
            i = j
            continue
        # Number (possibly negative ¯)
        if ch == "¯" or ch.isdigit() or (ch == "." and i + 1 < len(expr) and expr[i+1].isdigit()):
            j = i + 1
            while j < len(expr) and (expr[j].isdigit() or expr[j] == "." or expr[j] == "E" or
                                       (expr[j] == "-" and j > 0 and expr[j-1] == "E")):
                j += 1
            tokens.append(expr[i:j])
            i = j
            continue
        # Identifier
        if ch.isalpha() or ch == "_":
            j = i + 1
            while j < len(expr) and (expr[j].isalnum() or expr[j] == "_"):
                j += 1
            # Check for reduce/scan suffix
            while j < len(expr) and expr[j] in "/\\⌿":
                j += 1
            tokens.append(expr[i:j])
            i = j
            continue
        # Single char operator
        if ch in ("+", "-", "×", "÷", "*", "|", "!", ",", "←", "=", "<", ">"):
            # Check for reduce/scan suffix
            j = i + 1
            tok = ch
            while j < len(expr) and expr[j] in "/\\⌿":
                tok += expr[j]
                j += 1
            tokens.append(tok)
            i = j
            continue
        tokens.append(ch)
        i += 1
    return tokens


def _apl_num(s: str) -> Any:
    s = s.strip()
    if s.startswith("¯"):
        return -_apl_num(s[1:])
    try:
        return int(s)
    except ValueError:
        return float(s)


def _apl_format(val: Any) -> str:
    if isinstance(val, list):
        return " ".join(_apl_format(v) for v in val)
    if isinstance(val, bool):
        return "1" if val else "0"
    if isinstance(val, float):
        if val == int(val):
            return str(int(val))
        return str(val)
    return str(val)


def _apl_truthy(val: Any) -> bool:
    if isinstance(val, bool):
        return val
    if isinstance(val, (int, float)):
        return val != 0
    if isinstance(val, list):
        return len(val) > 0 and any(_apl_truthy(x) for x in val)
    return bool(val)


class APLError(Exception):
    pass
