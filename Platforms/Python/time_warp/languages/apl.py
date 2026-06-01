"""APL language executor for Time Warp Studio.

Educational APL interpreter — whole-program execution.
Implements a teaching subset of APL (Iverson, 1966):

  - ⎕← expr             — print expression
  - var ← expr           — variable assignment (← U+2190)
  - Monadic functions (applied to right argument):
      ⍳n   — iota: integers 1 to n (or 0 to n-1 if ⎕IO=0)
      ⍴A   — shape of A (returns dimension vector)
      ⌈A   — ceiling
      ⌊A   — floor
      |A   — absolute value
      -A   — negate
      ×A   — sign (-1, 0, 1)
      ÷A   — reciprocal
      ⍉A   — transpose
      ⌽A   — reverse
      ⊖A   — reverse first axis
      ⍋A   — grade up (sort ascending indices)
      ⍒A   — grade down (sort descending indices)
      ~A   — logical NOT
      *A   — exponential (e^A)
      ⍟A   — natural log
      ○A   — pi times A
      !A   — factorial
      ⊃A   — first
      ⊂A   — enclose (scalar wrap)
      ≢A   — tally
      ≡A   — depth
  - Dyadic functions:
      A + B    A - B    A × B    A ÷ B    A * B
      A ⌈ B    A ⌊ B    A | B
      A = B    A ≠ B    A < B    A ≤ B    A ≥ B    A > B
      A ∧ B    A ∨ B
      A , B    — catenate
      A ⍴ B    — reshape
      A ↑ B    — take
      A ↓ B    — drop
      A ⌽ B    — rotate
      A ⍟ B    — log base A of B
      A ○ B    — trig (A=1 sin, 2 cos, 3 tan)
      A ⊃ B    — pick
      A ⊂ B    — partitioned enclose
      A ! B    — binomial coefficient
      A ∊ B    — member (epsilon)
      A ⍳ B    — index of
  - Operators (higher-order):
      f/A       — reduce: +/ ×/ ⌈/ etc.
      f\\A      — scan: +\\ etc.
      f⌿A       — reduce first axis
      f¨A       — each
      f∘g       — compose
      A f.g B   — inner product (e.g. +.×)
      A f⍤n B   — rank (simplified)
  - System variables:
      ⎕IO ← 0 or 1     — index origin
      ⎕PP ← n          — print precision
  - Branching: → label / → 0 (return)
  - Labels: LABEL:
  - :If / :Else / :End  — control structures
  - ∇ function definition (dfn-style: f ← {⍺ + ⍵})
  - Dfns: {⍺ ... ⍵}  — ⍺ = left arg, ⍵ = right arg
  - Comments: ⍝ to end of line
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any, Callable, Dict, List, Optional, Tuple, Union

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_apl(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete APL program and return all output as a string."""
    env = APLEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# APL array type
# ---------------------------------------------------------------------------

Scalar = Union[int, float, bool, str]
APLArray = Union[Scalar, List[Any]]


def _is_scalar(v: Any) -> bool:
    return not isinstance(v, list)


def _to_vec(v: Any) -> List[Any]:
    if isinstance(v, list):
        return v
    return [v]


def _shape(v: Any) -> List[int]:
    if not isinstance(v, list):
        return []
    if not v:
        return [0]
    # Nested: check if rows are lists
    if isinstance(v[0], list):
        return [len(v), len(v[0])]
    return [len(v)]


def _rank(v: Any) -> int:
    return len(_shape(v))


def _tally(v: Any) -> int:
    if not isinstance(v, list):
        return 1
    return len(v)


def _flat(v: Any) -> List[Any]:
    if not isinstance(v, list):
        return [v]
    result = []
    for x in v:
        result.extend(_flat(x))
    return result


def _num(v: Any) -> float:
    if isinstance(v, bool):
        return 1.0 if v else 0.0
    if isinstance(v, (int, float)):
        return float(v)
    if isinstance(v, list) and len(v) == 1:
        return _num(v[0])
    if isinstance(v, str):
        if v.startswith("¯"):
            try:
                return -float(v[1:])
            except (ValueError, TypeError):
                return 0.0
        try:
            return float(v)
        except (ValueError, TypeError):
            return 0.0
    try:
        return float(v)
    except (ValueError, TypeError):
        return 0.0


def _fmt_val(v: Any, precision: int = 10) -> str:
    """Format APL value for display."""
    if isinstance(v, bool):
        return "1" if v else "0"
    if isinstance(v, float):
        if v == int(v) and abs(v) < 1e15:
            return str(int(v))
        return f"{v:.{precision}g}"
    if isinstance(v, int):
        return str(v)
    if isinstance(v, str):
        return v
    if isinstance(v, list):
        if not v:
            return ""
        if isinstance(v[0], list):
            # Matrix
            rows = [" ".join(_fmt_val(x) for x in row) for row in v]
            return "\n".join(rows)
        return " ".join(_fmt_val(x) for x in v)
    return str(v)


def _apply_monadic(fn: str, v: Any) -> Any:  # noqa: C901
    """Apply monadic APL function to a value."""
    if isinstance(v, list):
        if v and isinstance(v[0], list):
            return [_apply_monadic(fn, row) for row in v]
        return [_apply_monadic(fn, x) for x in v]
    # Scalar
    n = _num(v)
    if fn == "-":
        return -n
    if fn == "×":
        return 0 if n == 0 else (1 if n > 0 else -1)
    if fn == "÷":
        return 1.0 / n if n != 0 else float("inf")
    if fn == "|":
        return abs(n)
    if fn == "⌈":
        return math.ceil(n)
    if fn == "⌊":
        return math.floor(n)
    if fn == "*":
        return math.e ** n
    if fn == "⍟":
        return math.log(n) if n > 0 else float("nan")
    if fn == "○":
        return math.pi * n
    if fn == "!":
        nn = int(n)
        result = 1
        for k in range(2, nn + 1):
            result *= k
        return result
    if fn == "~":
        return 0 if n else 1
    if fn == "≡":
        return 0  # depth of scalar = 0
    if fn == "≢":
        return 1
    if fn == "⊃":
        return v
    if fn == "⊂":
        return [v]
    return v


def _apply_dyadic(fn: str, left: Any, right: Any) -> Any:  # noqa: C901
    """Apply dyadic APL function element-wise or as array op."""
    # Array broadcasting
    if isinstance(left, list) and isinstance(right, list):
        if len(left) != len(right):
            # Try broadcasting
            if len(left) == 1:
                left = left * len(right)
            elif len(right) == 1:
                right = right * len(left)
        return [_apply_dyadic(fn, l, r) for l, r in zip(left, right)]
    if isinstance(left, list):
        return [_apply_dyadic(fn, l, right) for l in left]
    if isinstance(right, list):
        return [_apply_dyadic(fn, left, r) for r in right]
    # Both scalar
    ln, rn = _num(left), _num(right)
    if fn == "+":
        return ln + rn
    if fn == "-":
        return ln - rn
    if fn == "×":
        return ln * rn
    if fn == "÷":
        if rn == 0:
            return float("inf")
        result = ln / rn
        return int(result) if result == int(result) else result
    if fn == "*":
        return ln ** rn
    if fn == "|":
        return ln % rn if rn != 0 else ln
    if fn == "⌈":
        return max(ln, rn)
    if fn == "⌊":
        return min(ln, rn)
    if fn == "=":
        return 1 if ln == rn else 0
    if fn == "≠":
        return 1 if ln != rn else 0
    if fn == "<":
        return 1 if ln < rn else 0
    if fn == "≤":
        return 1 if ln <= rn else 0
    if fn == ">":
        return 1 if ln > rn else 0
    if fn == "≥":
        return 1 if ln >= rn else 0
    if fn == "∧":
        return 1 if (ln != 0 and rn != 0) else 0
    if fn == "∨":
        return 1 if (ln != 0 or rn != 0) else 0
    if fn == "⍟":
        if ln > 0 and rn > 0:
            return math.log(rn) / math.log(ln)
        return float("nan")
    if fn == "○":
        trig_fns = {
            0: lambda x: math.sqrt(1 - x * x),
            1: math.sin,
            2: math.cos,
            3: math.tan,
            -1: math.asin,
            -2: math.acos,
            -3: math.atan,
        }
        fn_ = trig_fns.get(int(ln))
        return fn_(rn) if fn_ else rn
    if fn == "!":
        # Binomial coefficient
        from math import comb
        return comb(int(rn), int(ln))
    return 0


def _reduce(fn: str, v: Any, io: int = 1) -> Any:
    """f/ reduction."""
    if isinstance(v, list) and isinstance(v[0], list):
        # Matrix: reduce along last axis
        return [_reduce(fn, row) for row in v]
    items = _to_vec(v)
    if not items:
        return 0
    result = items[0]
    for x in items[1:]:
        result = _apply_dyadic(fn, result, x)
    return result


def _scan(fn: str, v: Any) -> List[Any]:
    """f\\ scan."""
    items = _to_vec(v)
    if not items:
        return []
    result = [items[0]]
    for x in items[1:]:
        result.append(_apply_dyadic(fn, result[-1], x))
    return result


def _reshape(dims: List[int], data: Any) -> Any:
    """Shape data into given dimensions."""
    flat = _flat(data)
    total = 1
    for d in dims:
        total *= d
    if not flat:
        flat = [0] * total
    # Cycle data to fill
    expanded = []
    for i in range(total):
        expanded.append(flat[i % len(flat)])
    if len(dims) == 1:
        return expanded
    if len(dims) == 2:
        rows = []
        for i in range(dims[0]):
            rows.append(expanded[i * dims[1]:(i + 1) * dims[1]])
        return rows
    return expanded


def _catenate(left: Any, right: Any) -> List[Any]:
    """A , B — catenate."""
    return _to_vec(left) + _to_vec(right)


def _take(n: int, v: Any, io: int = 1) -> Any:
    items = _to_vec(v)
    if n >= 0:
        result = items[:n]
        if len(result) < n:
            result = result + [0] * (n - len(result))
    else:
        result = items[n:]
        if len(result) < abs(n):
            result = [0] * (abs(n) - len(result)) + result
    return result


def _drop(n: int, v: Any) -> Any:
    items = _to_vec(v)
    if n >= 0:
        return items[n:]
    return items[:len(items) + n]


def _rotate(n: int, v: Any) -> Any:
    items = _to_vec(v)
    if not items:
        return items
    n = n % len(items)
    return items[n:] + items[:n]


def _grade_up(v: Any) -> List[int]:
    items = _to_vec(v)
    indexed = sorted(range(len(items)), key=lambda i: items[i])
    return [i + 1 for i in indexed]


def _grade_down(v: Any) -> List[int]:
    items = _to_vec(v)
    indexed = sorted(range(len(items)), key=lambda i: items[i], reverse=True)
    return [i + 1 for i in indexed]


def _iota(n: int, io: int = 1) -> List[Any]:
    return list(range(io, n + io))


def _index_of(haystack: Any, needle: Any, io: int = 1) -> Any:
    items = _to_vec(haystack)
    if isinstance(needle, list):
        return [_index_of(haystack, x, io) for x in needle]
    try:
        return items.index(needle) + io
    except ValueError:
        return len(items) + io


def _member(left: Any, right: Any) -> Any:
    right_items = _to_vec(right)
    if isinstance(left, list):
        return [1 if x in right_items else 0 for x in left]
    return 1 if left in right_items else 0


def _inner_product(fn1: str, fn2: str, left: Any, right: Any) -> Any:
    """A fn1.fn2 B — inner product (e.g. A +.× B)."""
    lv = _to_vec(left)
    rv = _to_vec(right)
    if not lv or not rv:
        return 0
    pairs = [_apply_dyadic(fn2, l, r) for l, r in zip(lv, rv)]
    return _reduce(fn1, pairs)


def _outer_product(fn: str, left: Any, right: Any) -> Any:
    """A ∘.fn B — outer product."""
    lv = _to_vec(left)
    rv = _to_vec(right)
    return [[_apply_dyadic(fn, l, r) for r in rv] for l in lv]


def _transpose(v: Any) -> Any:
    """Transpose a matrix."""
    if not isinstance(v, list):
        return v
    if not v:
        return v
    if not isinstance(v[0], list):
        # Vector: make column vector
        return [[x] for x in v]
    rows = len(v)
    cols = len(v[0])
    return [[v[r][c] for r in range(rows)] for c in range(cols)]


# ---------------------------------------------------------------------------
# APL Environment
# ---------------------------------------------------------------------------


class APLEnvironment:
    """State container and executor for an APL program."""

    MAX_LOOPS = 50_000

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self._interp = interpreter
        self._turtle = turtle
        self._vars: Dict[str, Any] = {}
        self._fns: Dict[str, Any] = {}   # user-defined functions/dfns
        self._io = 1    # ⎕IO default 1-based
        self._pp = 10   # print precision
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

    def _print_val(self, v: Any) -> None:
        s = _fmt_val(v, self._pp)
        self._emit(s + "\n")

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        lines = self._preprocess(source)
        # First pass: collect named function definitions (∇ style)
        lines = self._collect_fns(lines)
        # Execute
        try:
            self._exec_lines(lines, 0, len(lines))
        except Exception as e:
            self._emit(f"❌ {e}\n")
        self._flush()
        result = self._output
        return "\n".join(result) + ("\n" if result else "")

    def _preprocess(self, source: str) -> List[str]:
        """Strip comments and split into lines."""
        lines = []
        for raw in source.splitlines():
            # Strip APL comment ⍝
            idx = raw.find("⍝")
            if idx >= 0:
                raw = raw[:idx]
            raw = raw.strip()
            if raw:
                lines.append(raw)
        return lines

    def _collect_fns(self, lines: List[str]) -> List[str]:
        """Collect ∇ function definitions; return non-definition lines."""
        remaining: List[str] = []
        i = 0
        while i < len(lines):
            line = lines[i]
            if line.startswith("∇"):
                # ∇ fnname ⍵ or ∇ result ← fnname ⍵ etc.
                # Collect until next ∇
                header = line[1:].strip()
                body: List[str] = []
                i += 1
                while i < len(lines) and not lines[i].startswith("∇"):
                    body.append(lines[i])
                    i += 1
                if i < len(lines):
                    i += 1  # consume closing ∇
                # Parse header
                fn_name = self._parse_fn_header(header)
                if fn_name:
                    self._fns[fn_name.upper()] = ("traditional", header, body)
            else:
                remaining.append(lines[i])
                i += 1
        return remaining

    def _parse_fn_header(self, header: str) -> Optional[str]:
        """Extract function name from header like 'Z←FN A' or 'FN A' or 'Z←A FN B'."""
        # result ← name arg
        m = re.match(r"^(?:\w+\s*←\s*)?(\w+)\s+\w+\s*$", header)
        if m:
            return m.group(1)
        # name
        m = re.match(r"^(\w+)\s*$", header)
        if m:
            return m.group(1)
        return None

    # ------------------------------------------------------------------
    # Line executor
    # ------------------------------------------------------------------

    def _exec_lines(self, lines: List[str], start: int, end: int) -> Any:
        i = start
        last: Any = None
        while i < end:
            line = lines[i].strip()
            if not line:
                i += 1
                continue

            # :If / :Else / :End
            if line.startswith(":"):
                i = self._exec_control(lines, i, end)
                continue

            # Label: WORD:
            if re.match(r"^[A-Za-z_]\w*:$", line):
                i += 1
                continue

            # → n (branch/goto)
            if line.startswith("→"):
                target = line[1:].strip()
                if target == "0":
                    break
                i += 1
                continue

            # ⎕← print
            if line.startswith("⎕←") or line.startswith("⎕ ←"):
                expr = re.sub(r"^⎕\s*←\s*", "", line).strip()
                val = self._eval(expr)
                self._print_val(val)
                last = val
                i += 1
                continue

            # ⎕ IO/PP system variable assignment
            m_sys = re.match(r"^⎕(IO|PP)\s*←\s*(.+)$", line)
            if m_sys:
                val = int(_num(self._eval(m_sys.group(2))))
                if m_sys.group(1) == "IO":
                    self._io = val
                else:
                    self._pp = val
                i += 1
                continue

            # dfn assignment: fn ← {body}
            m_dfn = re.match(r"^(\w+)\s*←\s*\{(.+)\}$", line, re.DOTALL)
            if m_dfn:
                self._fns[m_dfn.group(1).upper()] = ("dfn", m_dfn.group(2))
                i += 1
                continue

            # Assignment: var ← expr
            m_assign = re.match(r"^([A-Za-zÀ-ÿ_][A-Za-z0-9_]*)\s*←\s*(.+)$", line, re.DOTALL)
            if m_assign:
                val = self._eval(m_assign.group(2).strip())
                self._vars[m_assign.group(1).upper()] = val
                last = val
                i += 1
                continue

            # Bare expression (auto-print)
            val = self._eval(line)
            if val is not None:
                self._print_val(val)
                last = val
            i += 1
        return last

    # ------------------------------------------------------------------
    # Control structures
    # ------------------------------------------------------------------

    def _exec_control(self, lines: List[str], start: int, end: int) -> int:
        line = lines[start].strip()
        if line.lower().startswith(":if"):
            cond_expr = line[3:].strip()
            cond = _num(self._eval(cond_expr)) != 0
            # Find :Else and :End
            then_lines: List[str] = []
            else_lines: List[str] = []
            in_else = False
            depth = 1
            i = start + 1
            while i < end:
                l = lines[i].strip().lower()
                if l.startswith(":if"):
                    depth += 1
                    (else_lines if in_else else then_lines).append(lines[i])
                elif l.startswith(":end") and depth == 1:
                    depth -= 1
                    break
                elif l.startswith(":else") and depth == 1:
                    in_else = True
                elif l.startswith(":end"):
                    depth -= 1
                    (else_lines if in_else else then_lines).append(lines[i])
                else:
                    (else_lines if in_else else then_lines).append(lines[i])
                i += 1
            if cond:
                self._exec_lines(then_lines, 0, len(then_lines))
            else:
                self._exec_lines(else_lines, 0, len(else_lines))
            return i + 1
        return start + 1

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval(self, expr: str) -> Any:  # noqa: C901
        expr = expr.strip()
        if not expr:
            return None

        # Parentheses
        if expr.startswith("(") and self._matching_paren(expr) == len(expr) - 1:
            return self._eval(expr[1:-1])

        # String literal
        if expr.startswith("'") and expr.endswith("'") and len(expr) >= 2:
            return expr[1:-1]

        # ⎕ — input
        if expr == "⎕":
            return self._get_input()

        # Numeric vector: 1 2 3 (space-separated numbers at top level)
        toks = self._top_level_tokens(expr)
        if len(toks) > 1 and all(self._is_num_token(t) for t in toks):
            return [_num(t) for t in toks]

        # Single number (including Python-style negative -5)
        if self._is_num_token(expr) or re.match(r"^-[\d.]+$", expr):
            return _num(expr)

        # Try to parse as APL expression (right-to-left)
        return self._eval_apl(expr)

    def _is_num_token(self, t: str) -> bool:
        return bool(re.match(r"^¯?[\d.]+([Ee]¯?[\d]+)?$", t))

    def _top_level_tokens(self, expr: str) -> List[str]:
        """Split on spaces at depth 0."""
        tokens: List[str] = []
        buf: List[str] = []
        depth = 0
        in_sq = False
        for c in expr:
            if in_sq:
                buf.append(c)
                if c == "'":
                    in_sq = False
            elif c == "'":
                in_sq = True
                buf.append(c)
            elif c in "([{":
                depth += 1
                buf.append(c)
            elif c in ")]}":
                depth -= 1
                buf.append(c)
            elif c == " " and depth == 0:
                tok = "".join(buf).strip()
                if tok:
                    tokens.append(tok)
                buf = []
            else:
                buf.append(c)
        tok = "".join(buf).strip()
        if tok:
            tokens.append(tok)
        return tokens

    def _matching_paren(self, expr: str) -> int:
        """Return index of matching ) for opening ( at position 0."""
        depth = 0
        for i, c in enumerate(expr):
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0:
                    return i
        return -1

    def _eval_apl(self, expr: str) -> Any:  # noqa: C901
        """Evaluate APL expression using right-to-left parsing."""
        expr = expr.strip()

        # Negative number literal (APL uses ¯ for negative)
        if re.match(r"^¯[\d.]+$", expr):
            return -float(expr[1:])

        # Single number
        if re.match(r"^[\d.]+$", expr):
            v = float(expr)
            return int(v) if v == int(v) else v

        # String
        if expr.startswith("'") and expr.endswith("'"):
            return expr[1:-1]

        # ⎕IO etc.
        if expr == "⎕IO":
            return self._io
        if expr == "⎕PP":
            return self._pp

        # Variable
        if re.match(r"^[A-Za-zÀ-ÿ_][A-Za-z0-9_]*$", expr):
            key = expr.upper()
            if key in self._vars:
                return self._vars[key]
            if key in self._fns:
                return ("__fn__", key)
            return None

        # Parenthesised
        if expr.startswith("("):
            end = self._matching_paren(expr)
            if end == len(expr) - 1:
                return self._eval(expr[1:-1])

        # Operators: find rightmost operator at depth 0
        # APL evaluates right to left

        # Inner product: A +.× B
        m_ip = self._find_inner_product(expr)
        if m_ip:
            left_str, fn1, fn2, right_str = m_ip
            left = self._eval(left_str)
            right = self._eval(right_str)
            return _inner_product(fn1, fn2, left, right)

        # Outer product: A ∘.f B
        m_op = self._find_outer_product(expr)
        if m_op:
            left_str, fn, right_str = m_op
            left = self._eval(left_str)
            right = self._eval(right_str)
            return _outer_product(fn, left, right)

        # Reduction / scan: f/A or f\A
        m_red = re.match(r"^([+\-×÷*⌈⌊=≠<≤>≥∧∨⍟|,∊])/(.+)$", expr)
        if m_red:
            fn = m_red.group(1)
            arg = self._eval(m_red.group(2).strip())
            return _reduce(fn, arg, self._io)

        m_scan = re.match(r"^([+\-×÷*⌈⌊=≠<≤>≥∧∨⍟|])\\(.+)$", expr)
        if m_scan:
            fn = m_scan.group(1)
            arg = self._eval(m_scan.group(2).strip())
            return _scan(fn, arg)

        # Each: f¨A
        m_each = re.match(r"^([+\-×÷*⌈⌊⌽⍋⍒~|⍟⍉])¨(.+)$", expr)
        if m_each:
            fn = m_each.group(1)
            arg = self._eval(m_each.group(2))
            if isinstance(arg, list):
                return [_apply_monadic(fn, x) for x in arg]
            return _apply_monadic(fn, arg)

        # Find the split point for dyadic vs monadic
        # Try all dyadic operators from left (they appear between left and right args)
        # APL parses right to left: rightmost operator at top level is applied last

        split = self._find_dyadic_op(expr)
        if split:
            left_str, fn, right_str = split
            right = self._eval(right_str)

            if left_str:
                left = self._eval(left_str)
                return self._apply_dyadic_apl(fn, left, right)
            else:
                return self._apply_monadic_apl(fn, right)

        # Numeric vector (space-separated)
        toks = self._top_level_tokens(expr)
        if len(toks) > 1:
            vals = [self._eval(t) for t in toks]
            # If all scalar, return as vector
            if all(_is_scalar(v) for v in vals):
                return vals
            return vals

        # Unknown
        return None

    def _find_dyadic_op(self, expr: str) -> Optional[Tuple[str, str, str]]:
        """Find the main operator in expr; return (left, op, right) or None."""
        # Priority order for finding the main operator (lowest precedence first in APL = rightmost)
        # We scan LEFT to RIGHT and return the first (leftmost) at depth 0
        # because APL is right-to-left and the leftmost operator has lowest precedence

        # Operators (ordered by length desc to avoid partial matches)
        ops = [
            # Special
            "⌿",  # reduce first axis
            # Dyadic
            "∊", "⍳", "⍋", "⍒", "⌽", "⊖", "↑", "↓", ",",
            "⍉",
            # Arithmetic/compare
            "+", "-", "×", "÷", "*", "⌈", "⌊", "|",
            "=", "≠", "<", "≤", ">", "≥",
            "∧", "∨", "~",
            "⍟", "○", "!",
            # Monadic-only (still need to check)
            "⍴", "⊃", "⊂",
        ]

        depth = 0
        in_sq = False
        i = 0
        n = len(expr)

        # Scan left to right, collect first operator at depth 0
        first_op_pos = -1
        first_op = ""
        first_op_len = 0

        while i < n:
            c = expr[i]
            if in_sq:
                if c == "'":
                    in_sq = False
                i += 1
                continue
            if c == "'":
                in_sq = True
                i += 1
                continue
            if c in "([{":
                depth += 1
                i += 1
                continue
            if c in ")]}":
                depth -= 1
                i += 1
                continue
            if depth == 0:
                for op in ops:
                    if expr[i:i + len(op)] == op:
                        if first_op_pos < 0:
                            first_op_pos = i
                            first_op = op
                            first_op_len = len(op)
                        break
            i += 1

        if first_op_pos < 0:
            return None

        left = expr[:first_op_pos].strip()
        right = expr[first_op_pos + first_op_len:].strip()
        return (left, first_op, right)

    def _find_inner_product(self, expr: str) -> Optional[Tuple[str, str, str, str]]:
        """Find A fn1.fn2 B pattern."""
        m = re.search(r"([+\-×÷*⌈⌊∧∨])\.([+\-×÷*⌈⌊∧∨])", expr)
        if m:
            pos = m.start()
            end = m.end()
            left = expr[:pos].strip()
            right = expr[end:].strip()
            if left and right:
                return (left, m.group(1), m.group(2), right)
        return None

    def _find_outer_product(self, expr: str) -> Optional[Tuple[str, str, str]]:
        """Find A ∘.f B pattern."""
        m = re.search(r"∘\.([+\-×÷*⌈⌊=≠<≤>≥∧∨|])", expr)
        if m:
            pos = m.start()
            end = m.end()
            left = expr[:pos].strip()
            right = expr[end:].strip()
            fn = m.group(1)
            if left and right:
                return (left, fn, right)
        return None

    def _apply_monadic_apl(self, fn: str, right: Any) -> Any:  # noqa: C901
        if fn == "⍳":
            n = int(_num(right))
            return _iota(n, self._io)
        if fn == "⍴":
            s = _shape(right)
            return s if s else []
        if fn == "⌽":
            return list(reversed(_to_vec(right)))
        if fn == "⊖":
            v = _to_vec(right)
            if v and isinstance(v[0], list):
                return list(reversed(v))
            return list(reversed(v))
        if fn == "⍋":
            return _grade_up(right)
        if fn == "⍒":
            return _grade_down(right)
        if fn == "⍉":
            return _transpose(right)
        if fn == "⊃":
            if isinstance(right, list):
                return right[0] if right else None
            return right
        if fn == "⊂":
            return [right]
        if fn == "≡":
            def depth(v: Any) -> int:
                if not isinstance(v, list):
                    return 0
                return 1 + max((depth(x) for x in v), default=0)
            return depth(right)
        if fn == "≢":
            return _tally(right)
        # Numeric monadic
        return _apply_monadic(fn, right)

    def _apply_dyadic_apl(self, fn: str, left: Any, right: Any) -> Any:  # noqa: C901
        if fn == "⍴":
            dims = [int(_num(x)) for x in _to_vec(left)]
            return _reshape(dims, right)
        if fn == ",":
            return _catenate(left, right)
        if fn == "↑":
            n = int(_num(left))
            return _take(n, right, self._io)
        if fn == "↓":
            n = int(_num(left))
            return _drop(n, right)
        if fn == "⌽":
            n = int(_num(left))
            return _rotate(n, right)
        if fn == "⍳":
            return _index_of(left, right, self._io)
        if fn == "∊":
            return _member(left, right)
        if fn == "⍋":
            return _grade_up(right)
        if fn == "⍒":
            return _grade_down(right)
        if fn == "⍉":
            return _transpose(right)
        if fn == "⊃":
            # Pick
            if isinstance(right, list):
                idx = int(_num(left)) - self._io
                return right[idx] if 0 <= idx < len(right) else None
            return right
        if fn == "⌿":
            # f⌿A — reduce first axis (same as f/ for vectors)
            return _reduce("+", right)  # default
        # Standard arithmetic/logical
        return _apply_dyadic(fn, left, right)

    def _call_fn(self, name: str, right: Any, left: Any = None) -> Any:
        """Call a user-defined function."""
        key = name.upper()
        if key not in self._fns:
            return None
        fn_def = self._fns[key]
        kind = fn_def[0]

        if kind == "dfn":
            body = fn_def[1]
            saved_vars = dict(self._vars)
            self._vars["⍵"] = right
            self._vars["⍺"] = left if left is not None else None
            # Execute body (dfns evaluate to their last expression)
            lines = [l.strip() for l in body.split("⋄") if l.strip()]
            result: Any = None
            for line in lines:
                result = self._eval(line)
            self._vars = saved_vars
            return result

        if kind == "traditional":
            header = fn_def[1]
            body_lines = fn_def[2]
            saved_vars = dict(self._vars)
            # Parse header to get arg names
            m = re.match(r"^(?:(\w+)\s*←\s*)?(\w+)\s+(\w+)\s*$", header)
            if m:
                self._vars[m.group(3).upper()] = right
            result = self._exec_lines(body_lines, 0, len(body_lines))
            m_result = re.match(r"^(\w+)\s*←", header)
            if m_result:
                result = self._vars.get(m_result.group(1).upper(), result)
            self._vars = saved_vars
            return result

        return None

    def _get_input(self) -> Any:
        if not self._input_used:
            self._input_used = True
            try:
                val = str(self._interp.input_value)
                # Try to parse as number
                try:
                    v = float(val)
                    return int(v) if v == int(v) else v
                except ValueError:
                    return val
            except Exception:
                return 0
        return 0
