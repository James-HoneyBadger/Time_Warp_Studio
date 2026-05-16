"""Erlang language executor for Time Warp Studio.

Educational Erlang interpreter — whole-program execution.
Implements a teaching subset of Erlang:
  - Module and function declarations (-module, -export, -import)
  - Basic types: integers, floats, atoms, strings, lists, tuples, maps
  - Pattern matching (function clauses, case, =)
  - Guards (when)
  - Arithmetic: + - * / div rem
  - Comparison: == /= < > =< >=
  - List operations: hd, tl, length, lists:*, ++, --
  - Tuple operations: element, tuple_size
  - io:format, io:fwrite output
  - Recursion and tail calls
  - Simulated message passing (spawn/send/receive)
  - List comprehensions [Expr || Pat <- List, Guard]
  - String handling
  - Turtle graphics
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_erlang(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete Erlang program and return all output as a string."""
    env = ErlangEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Control flow sentinels
# ---------------------------------------------------------------------------


class _ErlangError(Exception):
    pass


class _PatternMatchError(_ErlangError):
    pass


class _Atom:
    """Erlang atom (lowercase symbol)."""
    _cache: Dict[str, "_Atom"] = {}

    def __new__(cls, name: str) -> "_Atom":
        if name in cls._cache:
            return cls._cache[name]
        inst = super().__new__(cls)
        inst.name = name  # type: ignore[attr-defined]
        cls._cache[name] = inst
        return inst

    def __repr__(self) -> str:
        return self.name  # type: ignore[attr-defined]

    def __str__(self) -> str:
        return self.name  # type: ignore[attr-defined]

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, _Atom):
            return self.name == other.name  # type: ignore[attr-defined]
        return False

    def __hash__(self) -> int:
        return hash(self.name)  # type: ignore[attr-defined]


_ok = _Atom("ok")
_error = _Atom("error")
_true_atom = _Atom("true")
_false_atom = _Atom("false")


class _Tuple:
    """Erlang tuple."""
    def __init__(self, *elements: Any):
        self.elements = tuple(elements)

    def __repr__(self) -> str:
        elems = ", ".join(_erl_repr(e) for e in self.elements)
        return "{" + elems + "}"

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, _Tuple) and self.elements == other.elements

    def __len__(self) -> int:
        return len(self.elements)

    def __getitem__(self, idx: int) -> Any:
        return self.elements[idx]


class _Pid:
    """Simulated Erlang PID."""
    _counter = 0

    def __init__(self, name: str = ""):
        _Pid._counter += 1
        self.pid_id = _Pid._counter
        self.name = name
        self._mailbox: List[Any] = []

    def __repr__(self) -> str:
        return f"<{self.pid_id}.0.0>"


def _erl_repr(val: Any) -> str:
    """Convert a Python value to Erlang representation string."""
    if val is None or val == _Atom("undefined"):
        return "undefined"
    if isinstance(val, bool):
        return "true" if val else "false"
    if isinstance(val, _Atom):
        return str(val)
    if isinstance(val, _Tuple):
        return repr(val)
    if isinstance(val, list):
        return "[" + ", ".join(_erl_repr(x) for x in val) + "]"
    if isinstance(val, dict):
        pairs = ", ".join(f"{_erl_repr(k)} => {_erl_repr(v)}" for k, v in val.items())
        return "#{" + pairs + "}"
    if isinstance(val, _Pid):
        return repr(val)
    if isinstance(val, str):
        return f'"{val}"'
    return str(val)


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class ErlangEnvironment:
    """Tree-walking Erlang interpreter."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: List[str] = []
        self._functions: Dict[Tuple[str, int], List[Tuple[List[str], Optional[str], List[str]]]] = {}
        self._module_name: Optional[str] = None
        self._exported: List[Tuple[str, int]] = []
        self._self_pid = _Pid("self")
        self._pids: Dict[str, _Pid] = {}

    def _emit(self, text: str) -> None:
        self._output.append(text)

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        """Parse module and execute main/0 or start/0."""
        try:
            self._parse(source)
            # Try to call start/0 first, then main/0
            called = False
            for entry in ("start", "main", "go", "run"):
                if (entry, 0) in self._functions:
                    self._call_function(entry, [])
                    called = True
                    break
            if not called:
                # If there are no-arg exported functions, call first one
                for (fname, arity) in self._exported:
                    if arity == 0 and (fname, 0) in self._functions:
                        self._call_function(fname, [])
                        break
        except _ErlangError as e:
            self._emit(f"❌ Erlang error: {e}\n")
        except RecursionError:
            self._emit("❌ Erlang error: too many recursive calls\n")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}\n")
        return "".join(self._output)

    def _strip_comments(self, source: str) -> str:
        """Remove Erlang single-line comments (%...) but not inside strings."""
        result: List[str] = []
        in_string = False
        i = 0
        n = len(source)
        while i < n:
            ch = source[i]
            if ch == '"' and not in_string:
                in_string = True
                result.append(ch)
            elif ch == '"' and in_string:
                in_string = False
                result.append(ch)
            elif ch == '%' and not in_string:
                # Skip to end of line (preserve the newline itself)
                while i < n and source[i] != '\n':
                    i += 1
                continue
            else:
                result.append(ch)
            i += 1
        return ''.join(result)

    # ------------------------------------------------------------------
    # Parser
    # ------------------------------------------------------------------

    def _parse(self, source: str) -> None:
        """Parse Erlang source: extract module, exports, and function defs."""
        # Remove single-line comments (% ... ) but not inside strings
        source = self._strip_comments(source)
        # Split into top-level forms at ". " or ".\n" or ".%"
        forms = self._split_forms(source)

        for form in forms:
            form = form.strip()
            if not form:
                continue
            # -module(name)
            m = re.match(r"-module\s*\(\s*(\w+)\s*\)", form)
            if m:
                self._module_name = m.group(1)
                continue
            # -export([f/a, ...])
            m = re.match(r"-export\s*\(\s*\[([^\]]*)\]\s*\)", form)
            if m:
                for fa in re.findall(r"(\w+)\s*/\s*(\d+)", m.group(1)):
                    self._exported.append((fa[0], int(fa[1])))
                continue
            # -import / -define / other attributes: skip
            if re.match(r"-\w+", form):
                continue
            # Function definition: name(Args...) [when Guard] -> Body.
            self._parse_function(form)

    def _split_forms(self, source: str) -> List[str]:
        """Split Erlang source into top-level forms at unquoted dots."""
        forms = []
        current: List[str] = []
        in_string = False
        i = 0
        while i < len(source):
            ch = source[i]
            if ch == '"' and not in_string:
                in_string = True
                current.append(ch)
            elif ch == '"' and in_string:
                in_string = False
                current.append(ch)
            elif ch == '.' and not in_string:
                # Dot ending a form: followed by whitespace/EOF/comment
                if i + 1 >= len(source) or source[i + 1] in (' ', '\t', '\n', '\r', '%'):
                    forms.append("".join(current).strip())
                    current = []
                else:
                    current.append(ch)
            else:
                current.append(ch)
            i += 1
        if current:
            forms.append("".join(current).strip())
        return forms

    def _parse_function(self, form: str) -> None:
        """Parse a function definition and add clauses."""
        # May contain multiple clauses separated by ;
        clauses = self._split_clauses(form)
        for clause in clauses:
            self._parse_clause(clause)

    def _split_clauses(self, form: str) -> List[str]:
        """Split function clauses (separated by ;).

        Tracks bracket depth AND case/if/begin/receive/try...end keyword depth
        to avoid splitting on ; inside nested block expressions.
        """
        _BLOCK_OPENERS = frozenset({"case", "if", "begin", "receive", "try"})

        clauses: List[str] = []
        current: List[str] = []
        bracket_depth = 0
        kw_depth = 0
        in_string = False
        i = 0
        n = len(form)

        while i < n:
            ch = form[i]

            if ch == '"':
                in_string = not in_string
                current.append(ch)
                i += 1
                continue

            if in_string:
                current.append(ch)
                i += 1
                continue

            # Bracket tracking
            if ch in ('(', '[', '{'):
                bracket_depth += 1
            elif ch in (')', ']', '}'):
                bracket_depth -= 1
            elif ch == ';' and bracket_depth == 0 and kw_depth == 0:
                clauses.append("".join(current).strip())
                current = []
                i += 1
                continue
            elif ch.isalpha() or ch == '_':
                # Extract word token
                j = i
                while j < n and (form[j].isalnum() or form[j] == '_'):
                    j += 1
                word = form[i:j]
                # Only recognise as keyword at word boundary (not mid-identifier)
                prev_is_word = i > 0 and (form[i - 1].isalnum() or form[i - 1] == '_')
                if not prev_is_word:
                    if word == "end":
                        kw_depth = max(0, kw_depth - 1)
                    elif word in _BLOCK_OPENERS:
                        kw_depth += 1
                current.append(form[i:j])
                i = j
                continue

            current.append(ch)
            i += 1

        if current:
            clauses.append("".join(current).strip())
        return clauses

    def _parse_clause(self, clause: str) -> None:
        """Parse one function clause: name(Patterns) [when Guard] -> Body."""
        # Match: funcname(patterns) [when guard] -> body
        m = re.match(
            r"^(\w+)\s*\(([^)]*)\)\s*(?:when\s+(.+?))?\s*->\s*(.+)$",
            clause, re.DOTALL
        )
        if not m:
            return
        fname = m.group(1)
        patterns_str = m.group(2).strip()
        guard_str = m.group(3)
        body_str = m.group(4).strip()

        patterns = self._split_patterns(patterns_str) if patterns_str else []
        arity = len(patterns)
        body_exprs = self._split_body(body_str)

        key = (fname, arity)
        if key not in self._functions:
            self._functions[key] = []
        self._functions[key].append((patterns, guard_str, body_exprs))

    def _split_patterns(self, s: str) -> List[str]:
        """Split comma-separated pattern args."""
        return self._split_commas(s)

    def _eval_guard(self, guard_str: str, bindings: Dict[str, Any]) -> bool:
        """Evaluate an Erlang guard sequence.

        Comma = AND (all conditions must be true).
        Semicolon = OR (any semicolon-separated group with all ANDs true).
        """
        for or_group in guard_str.split(";"):
            or_group = or_group.strip()
            if not or_group:
                continue
            and_conds = self._split_commas(or_group)
            try:
                if all(self._truthy(self._eval_expr(c.strip(), bindings)) for c in and_conds):
                    return True
            except Exception:
                pass
        return False

    def _split_body(self, s: str) -> List[str]:
        """Split body expressions at commas, respecting brackets, strings, and
        keyword blocks (case/if/begin/receive/try/fun...end).

        Unlike _split_commas, this also tracks Erlang keyword block depth so
        that commas inside case branches are not treated as body separators.
        """
        _SIMPLE_OPENERS = frozenset({"case", "if", "begin", "receive", "try"})
        parts: List[str] = []
        current: List[str] = []
        bracket_depth = 0
        kw_depth = 0
        in_string = False
        i = 0
        n = len(s)
        while i < n:
            ch = s[i]
            if ch == '"':
                in_string = not in_string
                current.append(ch)
                i += 1
                continue
            if in_string:
                current.append(ch)
                i += 1
                continue
            # Bracket tracking
            if ch in ('(', '[', '{'):
                bracket_depth += 1
            elif ch in (')', ']', '}'):
                bracket_depth -= 1
            elif ch == ',' and bracket_depth == 0 and kw_depth == 0:
                parts.append("".join(current).strip())
                current = []
                i += 1
                continue
            elif ch.isalpha() or ch == '_':
                # Extract full word token
                j = i
                while j < n and (s[j].isalnum() or s[j] == '_'):
                    j += 1
                word = s[i:j]
                prev_is_word = i > 0 and (s[i - 1].isalnum() or s[i - 1] == '_')
                if not prev_is_word:
                    if word == "end":
                        kw_depth = max(0, kw_depth - 1)
                    elif word in _SIMPLE_OPENERS:
                        kw_depth += 1
                    elif word == "fun":
                        # Only lambda forms (fun(...) -> ... end) need kw_depth;
                        # fun Name/Arity references do not have a matching end.
                        k = j
                        while k < n and s[k] in (' ', '\t', '\n', '\r'):
                            k += 1
                        if k < n and s[k] == '(':
                            kw_depth += 1
                current.append(s[i:j])
                i = j
                continue
            current.append(ch)
            i += 1
        if current:
            parts.append("".join(current).strip())
        return [p for p in parts if p]

    def _split_commas(self, s: str) -> List[str]:
        """Split at commas, respecting brackets, strings, and fun...end blocks."""
        _SIMPLE_OPENERS = frozenset({"case", "if", "begin", "receive", "try"})
        parts: List[str] = []
        current: List[str] = []
        depth = 0
        kw_depth = 0
        in_string = False
        i = 0
        n = len(s)
        while i < n:
            ch = s[i]
            if ch == '"':
                in_string = not in_string
                current.append(ch)
                i += 1
                continue
            if in_string:
                current.append(ch)
                i += 1
                continue
            if ch in ('(', '[', '{'):
                depth += 1
            elif ch in (')', ']', '}'):
                depth -= 1
            elif ch == ',' and depth == 0 and kw_depth == 0:
                parts.append("".join(current).strip())
                current = []
                i += 1
                continue
            elif ch.isalpha() or ch == '_':
                j = i
                while j < n and (s[j].isalnum() or s[j] == '_'):
                    j += 1
                word = s[i:j]
                prev_is_word = i > 0 and (s[i - 1].isalnum() or s[i - 1] == '_')
                if not prev_is_word:
                    if word == "end":
                        kw_depth = max(0, kw_depth - 1)
                    elif word in _SIMPLE_OPENERS:
                        kw_depth += 1
                    elif word == "fun":
                        k = j
                        while k < n and s[k] in (' ', '\t', '\n', '\r'):
                            k += 1
                        if k < n and s[k] == '(':
                            kw_depth += 1
                current.append(s[i:j])
                i = j
                continue
            current.append(ch)
            i += 1
        if current:
            parts.append("".join(current).strip())
        return [p for p in parts if p]

    # ------------------------------------------------------------------
    # Function execution
    # ------------------------------------------------------------------

    def _call_function(self, name: str, args: List[Any], env: Optional[Dict[str, Any]] = None) -> Any:
        """Call an Erlang function by name and args."""
        arity = len(args)
        key = (name, arity)

        if key not in self._functions:
            raise _ErlangError(f"undefined function {name}/{arity}")

        clauses = self._functions[key]
        for patterns, guard_str, body_exprs in clauses:
            bindings: Dict[str, Any] = {}
            if env:
                bindings.update(env)
            try:
                for pat, arg in zip(patterns, args):
                    self._match_pattern(pat.strip(), arg, bindings)
                # Check guard
                if guard_str:
                    if not self._eval_guard(guard_str.strip(), bindings):
                        raise _PatternMatchError("guard failed")
                # Execute body
                result = None
                for expr in body_exprs:
                    result = self._eval_expr(expr.strip(), bindings)
                return result
            except _PatternMatchError:
                continue

        raise _ErlangError(f"no matching clause for {name}/{arity}: {args}")

    # ------------------------------------------------------------------
    # Pattern matching
    # ------------------------------------------------------------------

    def _match_pattern(self, pattern: str, value: Any, bindings: Dict[str, Any]) -> None:
        """Match a value against a pattern, updating bindings. Raises _PatternMatchError on failure."""
        pattern = pattern.strip()

        # Wildcard _
        if pattern == "_":
            return

        # Variable (uppercase start or _Var)
        if re.match(r"^[A-Z_][A-Za-z0-9_]*$", pattern):
            if pattern in bindings:
                if bindings[pattern] != value:
                    raise _PatternMatchError(f"variable {pattern} already bound to {bindings[pattern]}, cannot bind to {value}")
            else:
                bindings[pattern] = value
            return

        # Atom
        if re.match(r"^[a-z][a-zA-Z0-9_@]*$", pattern):
            atom = _Atom(pattern)
            if value != atom and value != pattern:
                raise _PatternMatchError(f"atom {pattern} != {value}")
            return

        # Integer
        if re.match(r"^-?\d+$", pattern):
            if int(pattern) != value:
                raise _PatternMatchError(f"{pattern} != {value}")
            return

        # Float
        if re.match(r"^-?\d+\.\d+$", pattern):
            if float(pattern) != value:
                raise _PatternMatchError(f"{pattern} != {value}")
            return

        # String
        if pattern.startswith('"') and pattern.endswith('"'):
            s = pattern[1:-1]
            if s != value:
                raise _PatternMatchError(f'"{s}" != {value}')
            return

        # Tuple {P1, P2, ...}
        if pattern.startswith("{") and pattern.endswith("}"):
            if not isinstance(value, _Tuple):
                raise _PatternMatchError(f"not a tuple: {value}")
            inner = pattern[1:-1].strip()
            sub_pats = self._split_commas(inner)
            if len(sub_pats) != len(value.elements):
                raise _PatternMatchError(f"tuple size mismatch")
            for sp, sv in zip(sub_pats, value.elements):
                self._match_pattern(sp.strip(), sv, bindings)
            return

        # List [H|T] or [P1, P2, ...]
        if pattern.startswith("[") and pattern.endswith("]"):
            if not isinstance(value, list):
                raise _PatternMatchError(f"not a list: {value}")
            inner = pattern[1:-1].strip()
            if not inner:
                if value:
                    raise _PatternMatchError("expected empty list")
                return
            if "|" in inner:
                head_pat, tail_pat = inner.split("|", 1)
                if not value:
                    raise _PatternMatchError("list is empty, no head")
                self._match_pattern(head_pat.strip(), value[0], bindings)
                self._match_pattern(tail_pat.strip(), value[1:], bindings)
            else:
                sub_pats = self._split_commas(inner)
                if len(sub_pats) != len(value):
                    raise _PatternMatchError(f"list length mismatch")
                for sp, sv in zip(sub_pats, value):
                    self._match_pattern(sp.strip(), sv, bindings)
            return

        # Pattern with = (alias pattern): Pat = Expr
        if "=" in pattern:
            parts = pattern.split("=", 1)
            evaled = self._eval_expr(parts[1].strip(), bindings)
            self._match_pattern(parts[0].strip(), value, bindings)
            if evaled != value:
                raise _PatternMatchError(f"alias mismatch")
            return

        # Fall-through: evaluate as expression and compare
        try:
            evaled = self._eval_expr(pattern, bindings)
            if evaled != value:
                raise _PatternMatchError(f"{evaled} != {value}")
        except Exception:
            raise _PatternMatchError(f"cannot match pattern {pattern}")

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _find_close_paren(self, s: str, open_pos: int) -> int:
        """Find the position of ')' matching '(' at open_pos.

        Respects nesting depth and skips over quoted strings.
        Returns -1 if no matching paren is found.
        """
        depth = 0
        in_string = False
        for i in range(open_pos, len(s)):
            c = s[i]
            if c == '"':
                in_string = not in_string
            if not in_string:
                if c == '(':
                    depth += 1
                elif c == ')':
                    depth -= 1
                    if depth == 0:
                        return i
        return -1

    def _find_op_at_depth0(self, expr: str, op: str) -> int:
        """Find the last occurrence of *op* at bracket/string depth 0, not at position 0.

        Searches right-to-left (last occurrence) for left-associativity.
        Returns the index of the operator or -1 if not found.
        """
        n = len(expr)
        op_len = len(op)
        depth = 0
        in_string = False
        last_pos = -1
        for i in range(n):
            ch = expr[i]
            if ch == '"':
                in_string = not in_string
            if not in_string:
                if ch in ('(', '[', '{'):
                    depth += 1
                elif ch in (')', ']', '}'):
                    depth -= 1
                elif depth == 0 and i > 0:  # i > 0 avoids unary +/- at start
                    if expr[i:i + op_len] == op:
                        last_pos = i
        return last_pos

    def _eval_expr(self, expr: str, bindings: Dict[str, Any]) -> Any:
        """Evaluate an Erlang expression string."""
        expr = expr.strip()
        if not expr:
            return _ok

        # Integer
        if re.match(r"^-?\d+$", expr):
            return int(expr)

        # Float
        if re.match(r"^-?\d+\.\d+$", expr):
            return float(expr)

        # Char $A
        m = re.match(r"^\$(.)", expr)
        if m:
            return ord(m.group(1))

        # Atom
        if re.match(r"^[a-z][a-zA-Z0-9_@]*$", expr):
            return _Atom(expr)

        # Boolean atoms
        if expr == "true":
            return True
        if expr == "false":
            return False

        # String literal
        m = re.match(r'^"(.*)"$', expr, re.DOTALL)
        if m:
            return m.group(1)

        # Variable lookup
        if re.match(r"^[A-Z_][A-Za-z0-9_]*$", expr):
            if expr in bindings:
                return bindings[expr]
            raise _ErlangError(f"unbound variable: {expr}")

        # Pattern match / assignment: Var = Expr  (multiline-safe)
        m = re.match(r"^([A-Z_][A-Za-z0-9_]*)\s*=\s*(.+)$", expr, re.DOTALL)
        if m:
            val = self._eval_expr(m.group(2).strip(), bindings)
            self._match_pattern(m.group(1), val, bindings)
            return val

        # Complex-pattern match: {A,B} = Expr  or  [H|T] = Expr
        # Must come BEFORE bare tuple/list evals to avoid misparse
        m = re.match(r"^(\{[^{}]*\}|\[[^\[\]]*\])\s*=\s*(.+)$", expr, re.DOTALL)
        if m:
            val = self._eval_expr(m.group(2).strip(), bindings)
            self._match_pattern(m.group(1).strip(), val, bindings)
            return val

        # Tuple {E1, E2, ...}
        if expr.startswith("{") and expr.endswith("}"):
            inner = expr[1:-1].strip()
            if not inner:
                return _Tuple()
            elems = [self._eval_expr(e.strip(), bindings) for e in self._split_commas(inner)]
            return _Tuple(*elems)

        # List comprehension [Expr || Generator, ...] — depth-aware to avoid false
        # matches when || appears inside nested brackets like [P | f([X || ...])]
        if expr.startswith("[") and expr.endswith("]"):
            inner = expr[1:-1]
            lc_idx = self._find_op_at_depth0(inner, "||")
            if lc_idx >= 0:
                return self._eval_list_comp(inner[:lc_idx].strip(), inner[lc_idx + 2:].strip(), bindings)
            return self._eval_list(expr, bindings)

        # Parenthesised — verify the outer ( and ) actually match each other
        if expr.startswith("(") and expr.endswith(")"):
            if self._find_close_paren(expr, 0) == len(expr) - 1:
                return self._eval_expr(expr[1:-1], bindings)

        # Map #{K => V, ...}
        if expr.startswith("#{") and expr.endswith("}"):
            return self._eval_map(expr, bindings)

        # case Expr of Clauses end
        m = re.match(r"^case\s+(.+?)\s+of\s+(.+)\s+end$", expr, re.DOTALL)
        if m:
            return self._eval_case(m.group(1), m.group(2), bindings)

        # if Guard1 -> Body1; ... end
        m = re.match(r"^if\s+(.+)\s+end$", expr, re.DOTALL)
        if m:
            return self._eval_if(m.group(1), bindings)

        # Binary operations
        for op in (" orelse ", " andalso "):
            if op in expr:
                parts = expr.split(op, 1)
                lv = self._eval_expr(parts[0].strip(), bindings)
                if op.strip() == "orelse" and self._truthy(lv):
                    return True
                if op.strip() == "andalso" and not self._truthy(lv):
                    return False
                return self._truthy(self._eval_expr(parts[1].strip(), bindings))

        for op in (" or ", " and ", " not ", " xor "):
            if op in expr or expr.startswith("not "):
                break

        # Module:function(Args) call — use balanced-paren finder to support nested calls
        m = re.match(r"^(\w+)\s*:\s*(\w+)\s*\(", expr)
        if m:
            paren_start = expr.index("(", m.start(2))
            paren_end = self._find_close_paren(expr, paren_start)
            if paren_end == len(expr) - 1:
                mod = m.group(1)
                func = m.group(2)
                args_str = expr[paren_start + 1:paren_end].strip()
                args = [self._eval_expr(a.strip(), bindings) for a in self._split_commas(args_str)] if args_str else []
                return self._call_bif(mod, func, args)

        # Function call: name(Args) — use balanced-paren finder to support nested calls
        m = re.match(r"^(\w+)\s*\(", expr)
        if m:
            paren_start = expr.index("(", m.start(1))
            paren_end = self._find_close_paren(expr, paren_start)
            if paren_end == len(expr) - 1:
                fname = m.group(1)
                args_str = expr[paren_start + 1:paren_end].strip()
                args = [self._eval_expr(a.strip(), bindings) for a in self._split_commas(args_str)] if args_str else []
                return self._dispatch(fname, args, bindings)

        # fun Name/Arity  — function reference (must come before binary ops to avoid
        # searching for operators like rem/div inside the lambda body)
        m = re.match(r"^fun\s+(\w+)\s*/\s*(\d+)$", expr)
        if m:
            fname_ref = m.group(1)
            arity_ref = int(m.group(2))
            env_ref = self
            def _fun_ref(*call_args, _fn=fname_ref, _ar=arity_ref, _env=env_ref):
                return _env._call_function(_fn, list(call_args), None)
            return _fun_ref

        # fun Module:Name/Arity  — module-qualified function reference
        m = re.match(r"^fun\s+(\w+)\s*:\s*(\w+)\s*/\s*(\d+)$", expr)
        if m:
            mod_ref, fname_ref = m.group(1), m.group(2)
            env_ref = self
            def _modfun_ref(*call_args, _m=mod_ref, _f=fname_ref, _env=env_ref):
                return _env._call_bif(_m, _f, list(call_args))
            return _modfun_ref

        # fun(Params) -> Body end  — anonymous function (lambda)
        m = re.match(r"^fun\s*\(([^)]*)\)\s*->\s*(.+)\s+end$", expr, re.DOTALL)
        if m:
            params_str = m.group(1).strip()
            body_str = m.group(2).strip()
            params = [p.strip() for p in self._split_commas(params_str)] if params_str else []
            body_exprs = self._split_body(body_str)
            env_ref = self
            captured = dict(bindings)
            def _lambda(*call_args, _params=params, _body=body_exprs, _env=env_ref, _cap=captured):
                b = dict(_cap)
                for p, a in zip(_params, call_args):
                    _env._match_pattern(p, a, b)
                result = None
                for be in _body:
                    result = _env._eval_expr(be.strip(), b)
                return result
            return _lambda

        # Recursive binary arithmetic — handles expressions like "N * fact(N - 1)"
        # Operators are checked lowest-precedence-first.
        # Comparison ops (lowest precedence) — return _Atom("true"/"false") so
        # that case/if guards work correctly (case X =:= Y of true -> ...)
        for op_sym, op_fn in [
            (" =:= ", lambda a, b: _true_atom if a == b else _false_atom),
            (" =/= ", lambda a, b: _true_atom if a != b else _false_atom),
            (" /= ", lambda a, b: _true_atom if a != b else _false_atom),
            (" == ", lambda a, b: _true_atom if a == b else _false_atom),
            (" >= ", lambda a, b: _true_atom if a >= b else _false_atom),
            (" =< ", lambda a, b: _true_atom if a <= b else _false_atom),
            (" > ", lambda a, b: _true_atom if a > b else _false_atom),
            (" < ", lambda a, b: _true_atom if a < b else _false_atom),
        ]:
            idx = self._find_op_at_depth0(expr, op_sym)
            if idx >= 0:
                left = self._eval_expr(expr[:idx].strip(), bindings)
                right = self._eval_expr(expr[idx + len(op_sym):].strip(), bindings)
                return op_fn(left, right)

        # List ops (below additive precedence)
        for op_sym, op_fn in [
            (" ++ ", lambda a, b: (a if isinstance(a, list) else []) + (b if isinstance(b, list) else [])),
            (" -- ", lambda a, b: [x for x in (a if isinstance(a, list) else []) if x not in (b if isinstance(b, list) else [])]),
        ]:
            idx = self._find_op_at_depth0(expr, op_sym)
            if idx >= 0:
                left = self._eval_expr(expr[:idx].strip(), bindings)
                right = self._eval_expr(expr[idx + len(op_sym):].strip(), bindings)
                return op_fn(left, right)

        for op_sym, op_fn in [(" + ", lambda a, b: a + b), (" - ", lambda a, b: a - b)]:
            idx = self._find_op_at_depth0(expr, op_sym)
            if idx >= 0:
                left = self._eval_expr(expr[:idx].strip(), bindings)
                right = self._eval_expr(expr[idx + len(op_sym):].strip(), bindings)
                return op_fn(left, right)

        for op_sym, op_fn in [
            (" * ", lambda a, b: a * b),
            (" / ", lambda a, b: a / b),
            (" div ", lambda a, b: int(a) // int(b)),
            (" rem ", lambda a, b: int(a) % int(b)),
        ]:
            idx = self._find_op_at_depth0(expr, op_sym)
            if idx >= 0:
                left = self._eval_expr(expr[:idx].strip(), bindings)
                right = self._eval_expr(expr[idx + len(op_sym):].strip(), bindings)
                return op_fn(left, right)

        # Arithmetic / comparison via Python eval
        return self._safe_eval(expr, bindings)

    def _eval_list(self, expr: str, bindings: Dict[str, Any]) -> List[Any]:
        inner = expr[1:-1].strip()
        if not inner:
            return []
        if "|" in inner:
            head_s, tail_s = inner.split("|", 1)
            head = [self._eval_expr(h.strip(), bindings) for h in self._split_commas(head_s)]
            tail = self._eval_expr(tail_s.strip(), bindings)
            if isinstance(tail, list):
                return head + tail
            return head
        return [self._eval_expr(e.strip(), bindings) for e in self._split_commas(inner)]

    def _eval_map(self, expr: str, bindings: Dict[str, Any]) -> Dict[Any, Any]:
        inner = expr[2:-1].strip()
        result = {}
        for pair in self._split_commas(inner):
            m = re.match(r"^(.+?)\s*=>\s*(.+)$", pair.strip())
            if m:
                k = self._eval_expr(m.group(1).strip(), bindings)
                v = self._eval_expr(m.group(2).strip(), bindings)
                result[k] = v
        return result

    def _eval_case(self, subject_str: str, clauses_str: str, bindings: Dict[str, Any]) -> Any:
        value = self._eval_expr(subject_str.strip(), bindings)
        clauses = self._split_case_clauses(clauses_str)
        for pat, guard, body_exprs in clauses:
            b = dict(bindings)
            try:
                self._match_pattern(pat, value, b)
                if guard and not self._eval_guard(guard, b):
                    continue
                result = None
                for bexpr in body_exprs:
                    result = self._eval_expr(bexpr.strip(), b)
                return result
            except _PatternMatchError:
                continue
        raise _ErlangError(f"no case clause matched for {_erl_repr(value)}")

    def _split_case_clauses(self, s: str) -> List[Tuple[str, Optional[str], List[str]]]:
        """Parse case clause strings into (pattern, guard, body)."""
        raw_clauses = []
        current: List[str] = []
        depth = 0
        in_string = False
        for ch in s:
            if ch == '"':
                in_string = not in_string
            if not in_string:
                if ch in ('(', '[', '{'):
                    depth += 1
                elif ch in (')', ']', '}'):
                    depth -= 1
                elif ch == ';' and depth == 0:
                    raw_clauses.append("".join(current).strip())
                    current = []
                    continue
            current.append(ch)
        if current:
            raw_clauses.append("".join(current).strip())

        result = []
        for c in raw_clauses:
            c = c.strip()
            m = re.match(r"^(.+?)\s*(?:when\s+(.+?))?\s*->\s*(.+)$", c, re.DOTALL)
            if m:
                body_exprs = self._split_commas(m.group(3).strip())
                result.append((m.group(1).strip(), m.group(2), body_exprs))
        return result

    def _eval_if(self, s: str, bindings: Dict[str, Any]) -> Any:
        """Evaluate Erlang if expression (guards only, no patterns)."""
        clauses = [c.strip() for c in s.split(";") if c.strip()]
        for clause in clauses:
            m = re.match(r"^(.+?)\s*->\s*(.+)$", clause, re.DOTALL)
            if m:
                guard = m.group(1).strip()
                body = m.group(2).strip()
                if guard == "true" or self._eval_guard(guard, bindings):
                    body_exprs = self._split_commas(body)
                    result = None
                    for be in body_exprs:
                        result = self._eval_expr(be.strip(), bindings)
                    return result
        raise _ErlangError("no if clause matched")

    def _eval_list_comp(self, expr_str: str, generators_str: str, bindings: Dict[str, Any]) -> List[Any]:
        """Evaluate [Expr || Gen, ...] list comprehension."""
        gens = self._split_commas(generators_str)
        return self._eval_comp_inner(expr_str.strip(), gens, dict(bindings))

    def _eval_comp_inner(self, expr_str: str, gens: List[str], bindings: Dict[str, Any]) -> List[Any]:
        if not gens:
            return [self._eval_expr(expr_str, bindings)]
        gen = gens[0].strip()
        rest = gens[1:]

        # Generator: Pat <- List
        m = re.match(r"^(.+?)\s*<-\s*(.+)$", gen)
        if m:
            pat = m.group(1).strip()
            lst = self._eval_expr(m.group(2).strip(), bindings)
            result = []
            if isinstance(lst, list):
                for item in lst:
                    b = dict(bindings)
                    try:
                        self._match_pattern(pat, item, b)
                        result.extend(self._eval_comp_inner(expr_str, rest, b))
                    except _PatternMatchError:
                        continue
            return result

        # Guard / filter
        try:
            if self._truthy(self._eval_expr(gen, bindings)):
                return self._eval_comp_inner(expr_str, rest, bindings)
        except Exception:
            pass
        return []

    # ------------------------------------------------------------------
    # BIFs and module calls
    # ------------------------------------------------------------------

    def _dispatch(self, fname: str, args: List[Any], bindings: Dict[str, Any]) -> Any:
        """Dispatch a call: BIF, module function, or user function."""
        arity = len(args)

        # BIFs
        bif_result = self._call_bif_bare(fname, args)
        if bif_result is not self._UNSET:
            return bif_result

        # User function — always create a fresh scope; don't leak caller bindings
        if (fname, arity) in self._functions:
            return self._call_function(fname, args, None)

        raise _ErlangError(f"undefined function {fname}/{arity}")

    _UNSET = object()

    def _call_bif(self, mod: str, func: str, args: List[Any]) -> Any:
        """Call a module-qualified BIF: io:format, lists:map, etc."""
        a0 = args[0] if args else None
        a1 = args[1] if len(args) > 1 else None

        if mod == "io":
            if func == "format" or func == "fwrite":
                self._io_format(args)
                return _ok
            if func == "write":
                self._emit(_erl_repr(a0))
                return _ok
            if func == "nl":
                self._emit("\n")
                return _ok
            if func == "read":
                return (_ok, "")
            return _ok

        if mod == "lists":
            # Functions where a0 is NOT a list (Fun-first or integer-first)
            if func == "seq":
                start, stop = int(a0), int(a1)
                step = int(args[2]) if len(args) > 2 else 1
                return list(range(start, stop + 1, step))
            if func in ("map", "foreach") and callable(a0):
                lst = a1 if isinstance(a1, list) else []
                result = [a0(x) for x in lst]
                return [] if func == "foreach" else result
            if func == "filter" and callable(a0):
                lst = a1 if isinstance(a1, list) else []
                return [x for x in lst if a0(x)]
            if func == "foldl" and callable(a0):
                lst = args[2] if len(args) > 2 else []
                if not isinstance(lst, list):
                    lst = []
                acc = a1
                for x in lst:
                    acc = a0(x, acc)
                return acc
            if func == "foldr" and callable(a0):
                lst = args[2] if len(args) > 2 else []
                if not isinstance(lst, list):
                    lst = []
                acc = a1
                for x in reversed(lst):
                    acc = a0(x, acc)
                return acc
            if func == "member":
                val, lst = a0, a1
                return val in (lst if isinstance(lst, list) else [])
            if func == "nth":
                idx, lst = int(a0), a1
                return lst[idx - 1] if isinstance(lst, list) and 1 <= idx <= len(lst) else _Atom("undefined")
            if func == "duplicate":
                n, val = int(a0), a1
                return [val] * n
            if func == "split":
                n, lst = int(a0), a1
                if isinstance(lst, list):
                    return _Tuple(lst[:n], lst[n:])
                return _Tuple([], [])
            if func == "delete":
                val, lst = a0, a1
                if isinstance(lst, list):
                    result = lst[:]
                    try:
                        result.remove(val)
                    except ValueError:
                        pass
                    return result
                return []
            if func == "zip":
                if isinstance(a0, list) and isinstance(a1, list):
                    return [_Tuple(x, y) for x, y in zip(a0, a1)]
                return []
            # Remaining functions require a0 to be a list
            if not isinstance(a0, list):
                return []
            if func == "map" and callable(a1):
                return [a1(x) for x in a0]
            if func == "filter" and callable(a1):
                return [x for x in a0 if a1(x)]
            if func == "foreach" and callable(a1):
                for x in a0:
                    a1(x)
                return _ok
            if func == "foldl" and callable(a1):
                acc = args[2] if len(args) > 2 else 0
                for x in a0:
                    acc = a1(x, acc)
                return acc
            if func == "foldr" and callable(a1):
                acc = args[2] if len(args) > 2 else 0
                for x in reversed(a0):
                    acc = a1(x, acc)
                return acc
            if func == "reverse":
                return list(reversed(a0))
            if func == "sort":
                try:
                    return sorted(a0)
                except TypeError:
                    return a0
            if func == "append":
                if isinstance(a1, list):
                    return a0 + a1
                result = []
                for sub in a0:
                    if isinstance(sub, list):
                        result.extend(sub)
                return result
            if func == "flatten":
                result = []
                def flatten(lst):
                    for item in lst:
                        if isinstance(item, list):
                            flatten(item)
                        else:
                            result.append(item)
                flatten(a0)
                return result
            if func == "last":
                return a0[-1] if a0 else _Atom("undefined")
            if func == "sum":
                return sum(x for x in a0 if isinstance(x, (int, float)))
            if func == "max":
                return max(a0) if a0 else _Atom("undefined")
            if func == "min":
                return min(a0) if a0 else _Atom("undefined")
            if func == "unzip":
                if a0:
                    firsts = [t[0] for t in a0 if isinstance(t, _Tuple)]
                    seconds = [t[1] for t in a0 if isinstance(t, _Tuple)]
                    return _Tuple(firsts, seconds)
                return _Tuple([], [])
            if func == "concat":
                result = []
                for sub in a0:
                    if isinstance(sub, list):
                        result.extend(sub)
                    else:
                        result.append(sub)
                return result
            if func in ("usort", "ukeysort"):
                seen = []
                for x in a0:
                    if x not in seen:
                        seen.append(x)
                return seen
            return a0 if isinstance(a0, list) else []

        if mod == "string":
            if not isinstance(a0, str):
                a0 = str(a0)
            if func == "to_upper":
                return a0.upper()
            if func == "to_lower":
                return a0.lower()
            if func == "length":
                return len(a0)
            if func == "concat":
                return "".join(str(a) for a in args)
            if func == "substr":
                start = int(a1) - 1 if a1 else 0
                n = int(args[2]) if len(args) > 2 else len(a0)
                return a0[start:start + n]
            if func == "split":
                sep = str(a1) if a1 else " "
                return a0.split(sep)
            if func == "strip":
                return a0.strip()
            if func == "left":
                return a0[:int(a1)] if a1 else a0
            if func == "right":
                return a0[-int(a1):] if a1 else a0
            if func == "tokens":
                separators = str(a1) if a1 else " "
                return [t for t in re.split(f"[{re.escape(separators)}]+", a0) if t]
            if func == "join":
                sep = str(a1) if a1 else ""
                return sep.join(str(x) for x in (a0 if isinstance(a0, list) else [a0]))
            if func in ("to_integer",):
                try:
                    return _Tuple(_ok, int(a0))
                except ValueError:
                    return _Tuple(_error, _Atom("no_integer"))
            if func in ("to_float",):
                try:
                    return _Tuple(_ok, float(a0))
                except ValueError:
                    return _Tuple(_error, _Atom("no_float"))
            return a0

        if mod == "math":
            if func == "sqrt":
                return math.sqrt(float(a0))
            if func == "pow":
                return math.pow(float(a0), float(a1))
            if func == "log":
                return math.log(float(a0))
            if func == "log2":
                return math.log2(float(a0))
            if func == "log10":
                return math.log10(float(a0))
            if func == "sin":
                return math.sin(float(a0))
            if func == "cos":
                return math.cos(float(a0))
            if func == "tan":
                return math.tan(float(a0))
            if func == "exp":
                return math.exp(float(a0))
            if func == "pi":
                return math.pi
            if func == "e":
                return math.e
            if func == "floor":
                return int(math.floor(float(a0)))
            if func == "ceil":
                return int(math.ceil(float(a0)))
            return None

        if mod == "erlang" or mod == self._module_name:
            return self._call_bif_bare(func, args)

        if mod == "maps":
            if func == "new":
                return {}
            if func == "get":
                k, d = a0, a1
                return d.get(k, args[2] if len(args) > 2 else _Atom("undefined")) if isinstance(d, dict) else _Atom("undefined")
            if func == "put":
                k, v, d = a0, a1, args[2] if len(args) > 2 else {}
                result = dict(d) if isinstance(d, dict) else {}
                result[k] = v
                return result
            if func == "keys":
                return list(a0.keys()) if isinstance(a0, dict) else []
            if func == "values":
                return list(a0.values()) if isinstance(a0, dict) else []
            if func == "size":
                return len(a0) if isinstance(a0, dict) else 0
            if func == "is_key":
                return a0 in a1 if isinstance(a1, dict) else False
            return {}

        # Turtle module
        if mod == "turtle":
            return self._call_turtle(func, args)

        return _ok

    def _call_bif_bare(self, fname: str, args: List[Any]) -> Any:
        """Call Erlang BIFs without module prefix."""
        a0 = args[0] if args else None
        a1 = args[1] if len(args) > 1 else None

        if fname == "hd":
            if isinstance(a0, list) and a0:
                return a0[0]
            raise _ErlangError("badarg: hd([]) or not a list")
        if fname == "tl":
            if isinstance(a0, list) and a0:
                return a0[1:]
            raise _ErlangError("badarg: tl")
        if fname == "length":
            if isinstance(a0, list):
                return len(a0)
            return 0
        if fname == "tuple_size":
            return len(a0) if isinstance(a0, _Tuple) else 0
        if fname == "element":
            if isinstance(a0, int) and isinstance(a1, _Tuple):
                return a1.elements[a0 - 1]
            return None
        if fname == "setelement":
            if isinstance(a0, int) and isinstance(a1, _Tuple) and len(args) > 2:
                elems = list(a1.elements)
                elems[a0 - 1] = args[2]
                return _Tuple(*elems)
            return a1
        if fname == "abs":
            return abs(a0) if isinstance(a0, (int, float)) else 0
        if fname == "round":
            return round(a0) if isinstance(a0, (int, float)) else 0
        if fname == "trunc":
            return int(a0) if isinstance(a0, (int, float)) else 0
        if fname == "float":
            return float(a0) if isinstance(a0, (int, float)) else 0.0
        if fname == "integer_to_list":
            return list(str(a0)) if a0 is not None else []
        if fname == "list_to_integer":
            try:
                if isinstance(a0, list):
                    return int("".join(str(c) for c in a0))
                return int(str(a0))
            except ValueError:
                raise _ErlangError("list_to_integer: badarg")
        if fname == "integer_to_binary":
            return str(a0)
        if fname == "atom_to_list":
            return list(str(a0)) if a0 is not None else []
        if fname == "list_to_atom":
            if isinstance(a0, list):
                return _Atom("".join(str(c) for c in a0))
            return _Atom(str(a0))
        if fname == "atom_to_binary":
            return str(a0)
        if fname == "is_atom":
            return isinstance(a0, _Atom)
        if fname == "is_integer":
            return isinstance(a0, int) and not isinstance(a0, bool)
        if fname == "is_float":
            return isinstance(a0, float)
        if fname == "is_number":
            return isinstance(a0, (int, float)) and not isinstance(a0, bool)
        if fname == "is_list":
            return isinstance(a0, list)
        if fname == "is_tuple":
            return isinstance(a0, _Tuple)
        if fname == "is_binary":
            return isinstance(a0, (str, bytes))
        if fname == "is_boolean":
            return isinstance(a0, bool) or a0 in (_true_atom, _false_atom)
        if fname == "is_pid":
            return isinstance(a0, _Pid)
        if fname == "is_function":
            return callable(a0)
        if fname == "is_map":
            return isinstance(a0, dict)
        if fname == "not":
            return not self._truthy(a0)
        if fname == "max":
            return max(a0, a1) if a0 is not None and a1 is not None else a0
        if fname == "min":
            return min(a0, a1) if a0 is not None and a1 is not None else a0
        if fname == "error":
            raise _ErlangError(str(a0))
        if fname == "throw":
            raise _ErlangError(f"throw: {a0}")
        if fname == "exit":
            raise SystemExit(0)
        if fname == "self":
            return self._self_pid
        if fname == "spawn":
            pid = _Pid("spawned")
            self._pids[str(pid.pid_id)] = pid
            return pid
        if fname == "send":
            # Simulate: just discard message
            return a1
        if fname in ("!", "send"):
            return a1
        if fname == "receive":
            return _ok
        if fname == "node":
            return _Atom("nonode@nohost")
        if fname == "make_ref":
            import uuid
            return f"#Ref<{uuid.uuid4()}>"
        if fname == "now":
            import time as _time
            t = int(_time.time() * 1_000_000)
            mega = t // 1_000_000_000_000
            sec = (t // 1_000_000) % 1_000_000
            micro = t % 1_000_000
            return _Tuple(mega, sec, micro)
        if fname == "io_lib":
            return ""

        # io:format without module prefix
        if fname == "format":
            self._io_format(args)
            return _ok

        return self._UNSET

    def _io_format(self, args: List[Any]) -> None:
        """Implement io:format/2 with ~w, ~p, ~s, ~n, ~i format directives."""
        if not args:
            return
        fmt = args[0]
        params = list(args[1]) if len(args) > 1 and isinstance(args[1], list) else args[1:]

        if not isinstance(fmt, str):
            self._emit(_erl_repr(fmt))
            return

        result = []
        i = 0
        param_idx = 0
        while i < len(fmt):
            if fmt[i] == "~" and i + 1 < len(fmt):
                directive = fmt[i + 1]
                i += 2
                if directive == "n":
                    result.append("\n")
                elif directive == "t":
                    result.append("\t")
                elif directive in ("w", "p"):
                    if param_idx < len(params):
                        result.append(_erl_repr(params[param_idx]))
                        param_idx += 1
                elif directive == "s":
                    if param_idx < len(params):
                        v = params[param_idx]
                        result.append(str(v) if not isinstance(v, list) else "".join(chr(c) if isinstance(c, int) else str(c) for c in v))
                        param_idx += 1
                elif directive == "d":
                    if param_idx < len(params):
                        result.append(str(int(params[param_idx])))
                        param_idx += 1
                elif directive == "f":
                    if param_idx < len(params):
                        result.append(f"{float(params[param_idx]):.6f}")
                        param_idx += 1
                elif directive == "e":
                    if param_idx < len(params):
                        result.append(f"{float(params[param_idx]):.6e}")
                        param_idx += 1
                elif directive == "i":
                    param_idx += 1  # ignore
                elif directive == "~":
                    result.append("~")
                else:
                    result.append(f"~{directive}")
            else:
                result.append(fmt[i])
                i += 1

        self._emit("".join(result))

    def _call_turtle(self, func: str, args: List[Any]) -> Any:
        """Handle turtle:* calls for graphics."""
        t = self.turtle
        if t is None:
            return _ok
        a0 = float(args[0]) if args and isinstance(args[0], (int, float)) else 0
        if func == "forward" or func == "fd":
            t.forward(a0)
        elif func == "backward" or func == "bk" or func == "back":
            t.backward(a0)
        elif func == "right" or func == "rt":
            t.right(a0)
        elif func == "left" or func == "lt":
            t.left(a0)
        elif func == "penup" or func == "pu":
            t.pen_up()
        elif func == "pendown" or func == "pd":
            t.pen_down()
        elif func == "home":
            t.home()
        elif func == "clear":
            t.clear()
        elif func == "color" and args:
            t.set_color(str(args[0]))
        return _ok

    def _safe_eval(self, expr: str, bindings: Dict[str, Any]) -> Any:
        """Evaluate simple arithmetic/comparison expressions."""
        py = expr
        # Erlang → Python operator mapping
        py = re.sub(r"\bdiv\b", "//", py)
        py = re.sub(r"\brem\b", "%", py)
        py = re.sub(r"\bnot\b", " not ", py)
        py = re.sub(r"\band\b", " and ", py)
        py = re.sub(r"\bor\b", " or ", py)
        py = re.sub(r"/=", "!=", py)
        py = re.sub(r"=<", "<=", py)
        py = re.sub(r"=:=", "==", py)
        py = re.sub(r"=/=", "!=", py)
        py = re.sub(r"\+\+", "+", py)  # list concat (simplified)

        # Substitute bindings
        for k, v in bindings.items():
            if re.match(r"^[A-Z_][A-Za-z0-9_]*$", k):
                py = re.sub(rf"\b{re.escape(k)}\b", repr(v), py)

        try:
            result = eval(py, {"__builtins__": {}}, {  # noqa: S307
                "abs": abs, "round": round, "int": int, "float": float,
                "min": min, "max": max, "len": len, "True": True, "False": False,
                "None": None,
            })
            return result
        except Exception:
            return _ok

    def _truthy(self, val: Any) -> bool:
        if val is None or val is False:
            return False
        if isinstance(val, _Atom) and val in (_Atom("false"), _Atom("undefined")):
            return False
        return True
