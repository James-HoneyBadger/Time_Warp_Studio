"""Perl 5 language executor for Time Warp Studio.

Educational Perl interpreter — whole-program execution.
Supports a teaching subset of Perl 5 idioms:

  - Scalars: $var, numeric and string context, undef
  - Arrays: @arr, push/pop/shift/unshift/splice/reverse/sort/grep/map/join
  - Hashes: %hash, keys/values/each/exists/delete
  - String ops: chomp/chop/length/index/substr/lc/uc/ucfirst/lcfirst/
                reverse/split/join/sprintf/sprintf
  - Regex: =~ m//, =~ s///, !~, tr///y///, qw(...)
  - String interpolation in double-quoted strings
  - Control flow: if/elsif/else/unless, while/until, for/foreach, do..while,
                  last/next/redo (loop control)
  - Statement modifiers: print "x" if $cond; for/foreach/while/unless
  - Subroutines: sub name { ... }, return, @_ args, local $_
  - References: \\@arr, \\%hash, $ref->[n], $ref->{key} (basic)
  - String functions: chomp, chop, length, index, rindex, substr, sprintf,
                      uc, lc, ucfirst, lcfirst, reverse, chr, ord, hex, oct
  - Array functions: push, pop, shift, unshift, splice, reverse, sort,
                     grep, map, join, wantarray
  - Hash functions: keys, values, each, exists, delete
  - I/O: print, say, printf, warn, die (warn/die write to output)
  - Math: abs, int, sqrt, sin, cos, atan2, exp, log, rand, srand, POSIX
  - Misc: defined, ref, scalar, wantarray, chomp, chop, exit
  - Turtle graphics: forward/fd, backward/bk, left/lt, right/rt,
                     penup/pu, pendown/pd, color/pencolor, setpos/goto,
                     home, clear_canvas, circle, dot, stamp, speed,
                     setheading, hideturtle, showturtle
"""

from __future__ import annotations

import math
import random
import re
from typing import TYPE_CHECKING, Any, Dict, List, Optional, Tuple

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_perl(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete Perl 5 program and return all output as a string."""
    env = PerlEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Control-flow exceptions
# ---------------------------------------------------------------------------


class _PerlLast(Exception):
    def __init__(self, label: str = "") -> None:
        self.label = label


class _PerlNext(Exception):
    def __init__(self, label: str = "") -> None:
        self.label = label


class _PerlRedo(Exception):
    def __init__(self, label: str = "") -> None:
        self.label = label


class _PerlReturn(Exception):
    def __init__(self, value: Any = None) -> None:
        self.value = value


class _PerlDie(Exception):
    def __init__(self, message: str = "Died") -> None:
        self.message = message


class _PerlExit(Exception):
    pass


# ---------------------------------------------------------------------------
# Perl undef
# ---------------------------------------------------------------------------


class _PerlUndef:
    """Represents Perl's undef value."""

    def __repr__(self) -> str:
        return "undef"

    def __str__(self) -> str:
        return ""

    def __bool__(self) -> bool:
        return False

    def __int__(self) -> int:
        return 0

    def __float__(self) -> float:
        return 0.0


UNDEF = _PerlUndef()


# ---------------------------------------------------------------------------
# Utility helpers
# ---------------------------------------------------------------------------


def _to_num(val: Any) -> float:
    """Convert Perl value to numeric context."""
    if isinstance(val, (int, float)):
        return float(val)
    if isinstance(val, _PerlUndef):
        return 0.0
    if isinstance(val, bool):
        return 1.0 if val else 0.0
    s = str(val).strip()
    if not s:
        return 0.0
    m = re.match(r"^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?", s)
    if m:
        try:
            return float(m.group(0))
        except ValueError:
            return 0.0
    return 0.0


def _to_int(val: Any) -> int:
    return int(_to_num(val))


def _to_str(val: Any) -> str:
    if isinstance(val, _PerlUndef):
        return ""
    if isinstance(val, bool):
        return "1" if val else ""
    if isinstance(val, float):
        if val == int(val) and abs(val) < 1e15:
            return str(int(val))
        return str(val)
    return str(val)


def _is_true(val: Any) -> bool:
    """Perl truth semantics: undef/0/""/undef/"0" are false."""
    if isinstance(val, _PerlUndef):
        return False
    if isinstance(val, bool):
        return val
    if isinstance(val, (int, float)):
        return val != 0
    s = str(val)
    return s != "" and s != "0"


# ---------------------------------------------------------------------------
# Source pre-processing: join logical lines
# ---------------------------------------------------------------------------


def _preprocess(source: str) -> str:
    """Normalise line-endings and strip __END__ / __DATA__ sections."""
    lines = source.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    out: List[str] = []
    for line in lines:
        stripped = line.strip()
        if stripped in ("__END__", "__DATA__"):
            break
        out.append(line)
    return "\n".join(out)


# ---------------------------------------------------------------------------
# Block splitter: find matching closing brace
# ---------------------------------------------------------------------------


def _find_block_end(text: str, start: int) -> int:
    """Return index of the closing '}' matching the '{' at *start*."""
    depth = 0
    i = start
    in_sq = in_dq = in_re = False
    while i < len(text):
        c = text[i]
        if in_sq:
            if c == "\\" and i + 1 < len(text):
                i += 2
                continue
            if c == "'":
                in_sq = False
        elif in_dq:
            if c == "\\" and i + 1 < len(text):
                i += 2
                continue
            if c == '"':
                in_dq = False
        else:
            if c == "'":
                in_sq = True
            elif c == '"':
                in_dq = True
            elif c == "{":
                depth += 1
            elif c == "}":
                depth -= 1
                if depth == 0:
                    return i
        i += 1
    return len(text) - 1


# ---------------------------------------------------------------------------
# Statement splitter
# ---------------------------------------------------------------------------

_HEREDOC_RE = re.compile(r'<<(["\']?)(\w+)\1')


def _split_statements(source: str) -> List[str]:
    """Split Perl source into top-level statement strings."""
    stmts: List[str] = []
    buf: List[str] = []
    depth_brace = 0
    depth_paren = 0
    depth_bracket = 0
    i = 0
    n = len(source)
    in_sq = in_dq = in_re = False
    re_slash_ok = True  # could be a regex after operators

    while i < n:
        c = source[i]

        # Single-quoted string
        if in_sq:
            buf.append(c)
            if c == "\\" and i + 1 < n:
                buf.append(source[i + 1])
                i += 2
                continue
            if c == "'":
                in_sq = False
            i += 1
            continue

        # Double-quoted string
        if in_dq:
            buf.append(c)
            if c == "\\" and i + 1 < n:
                buf.append(source[i + 1])
                i += 2
                continue
            if c == '"':
                in_dq = False
            i += 1
            continue

        # Comments
        if c == "#":
            j = source.find("\n", i)
            if j == -1:
                j = n
            # skip comment
            i = j
            continue

        if c == "'":
            in_sq = True
            buf.append(c)
            re_slash_ok = False
            i += 1
            continue

        if c == '"':
            in_dq = True
            buf.append(c)
            re_slash_ok = False
            i += 1
            continue

        if c in "([{":
            depth_brace += (c == "{")
            depth_paren += (c == "(")
            depth_bracket += (c == "[")
            buf.append(c)
            re_slash_ok = True
            i += 1
            continue

        if c in ")]}":
            depth_brace -= (c == "}")
            depth_paren -= (c == ")")
            depth_bracket -= (c == "]")
            buf.append(c)
            re_slash_ok = False
            # When a } closes the outermost block, split the statement
            # (unless followed by else/elsif/while/until which continues it)
            if c == "}" and depth_brace == 0 and depth_paren == 0 and depth_bracket == 0:
                j = i + 1
                while j < n and source[j] in " \t\n\r":
                    j += 1
                lookahead = source[j:j + 7]
                if not (lookahead.startswith("else") or
                        lookahead.startswith("elsif") or
                        lookahead.startswith("while") or
                        lookahead.startswith("until")):
                    stmt = "".join(buf).strip()
                    if stmt:
                        stmts.append(stmt)
                    buf = []
                    re_slash_ok = True
            i += 1
            continue

        if c == ";" and depth_brace == 0 and depth_paren == 0 and depth_bracket == 0:
            stmt = "".join(buf).strip()
            if stmt:
                stmts.append(stmt)
            buf = []
            re_slash_ok = True
            i += 1
            continue

        if c == "\n":
            buf.append(c)
            i += 1
            continue

        buf.append(c)
        if c not in " \t\n":
            re_slash_ok = c in "=+\\-*/%!&|^~,(<[{;?"
        i += 1

    # trailing
    stmt = "".join(buf).strip()
    if stmt:
        stmts.append(stmt)

    return stmts


# ---------------------------------------------------------------------------
# Main environment
# ---------------------------------------------------------------------------


class PerlEnvironment:
    """State container and executor for a Perl 5 program."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self._interp = interpreter
        self._turtle = turtle
        self._output: List[str] = []
        self._output_buf: str = ""  # partial-line buffer

        # Variable storage: scalars, arrays, hashes
        self._scalars: Dict[str, Any] = {
            "_": UNDEF,  # $_ (topic variable)
        }
        self._arrays: Dict[str, List[Any]] = {
            "ARGV": [],
        }
        self._hashes: Dict[str, Dict[str, Any]] = {
            "ENV": {},
        }
        self._subs: Dict[str, Tuple[List[str], str]] = {}  # name -> (params, body)
        self._call_stack: List[Dict[str, Any]] = []  # local var frames

        # Loop depth for last/next/redo
        self._loop_depth = 0

        # Special variables
        self._scalars["0"] = "perl_program"
        self._scalars["/"] = "\n"  # input record separator
        self._scalars["\\"] = ""  # output record separator
        self._scalars[","] = ""  # output field separator
        self._scalars[";"] = "\034"  # subscript separator

    # ------------------------------------------------------------------
    # Output helpers
    # ------------------------------------------------------------------

    def _emit(self, text: str) -> None:
        """Buffer output; only flush to _output on newline boundaries."""
        combined = self._output_buf + text
        lines = combined.split("\n")
        # All but the last part are complete lines
        for line in lines[:-1]:
            self._output.append(line)
        self._output_buf = lines[-1]

    def _flush_output(self) -> None:
        """Flush any remaining buffered output."""
        if self._output_buf:
            self._output.append(self._output_buf)
            self._output_buf = ""

    def _emit_line(self, text: str) -> None:
        self._emit(text + "\n")

    # ------------------------------------------------------------------
    # Variable access
    # ------------------------------------------------------------------

    def _get_scalar(self, name: str) -> Any:
        # Check local frames first (innermost)
        for frame in reversed(self._call_stack):
            if name in frame:
                return frame[name]
        return self._scalars.get(name, UNDEF)

    def _set_scalar(self, name: str, value: Any) -> None:
        if self._call_stack:
            # If already in local frame, update there
            for frame in reversed(self._call_stack):
                if name in frame:
                    frame[name] = value
                    return
            # Set in current (innermost) frame if it's a local
            self._scalars[name] = value
        else:
            self._scalars[name] = value

    def _get_array(self, name: str) -> List[Any]:
        return self._arrays.setdefault(name, [])

    def _set_array(self, name: str, value: List[Any]) -> None:
        self._arrays[name] = list(value)

    def _get_hash(self, name: str) -> Dict[str, Any]:
        return self._hashes.setdefault(name, {})

    def _set_hash(self, name: str, value: Dict[str, Any]) -> None:
        self._hashes[name] = dict(value)

    # ------------------------------------------------------------------
    # Run
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        source = _preprocess(source)
        # First pass: collect sub definitions
        self._collect_subs(source)
        # Remove sub definitions from source and execute remainder
        cleaned = self._strip_subs(source)
        stmts = _split_statements(cleaned)
        try:
            for stmt in stmts:
                stmt = stmt.strip()
                if stmt:
                    self._exec_stmt(stmt)
        except _PerlDie as e:
            self._emit_line(f"❌ {e.message}")
        except _PerlExit:
            pass
        except Exception as e:
            self._emit_line(f"❌ Runtime error: {e}")
        self._flush_output()
        lines = [ln for ln in self._output]
        return "\n".join(lines) + ("\n" if lines else "")

    # ------------------------------------------------------------------
    # Sub collection / stripping
    # ------------------------------------------------------------------

    _SUB_RE = re.compile(
        r"sub\s+(\w+)\s*\{", re.DOTALL
    )

    def _collect_subs(self, source: str) -> None:
        """Find and store all sub definitions."""
        pos = 0
        while True:
            m = self._SUB_RE.search(source, pos)
            if not m:
                break
            name = m.group(1)
            brace_start = m.end() - 1
            brace_end = _find_block_end(source, brace_start)
            body = source[brace_start + 1 : brace_end].strip()
            self._subs[name] = ([], body)
            pos = brace_end + 1

    def _strip_subs(self, source: str) -> str:
        """Remove sub definitions from source (they run when called)."""
        result = []
        pos = 0
        while True:
            m = self._SUB_RE.search(source, pos)
            if not m:
                result.append(source[pos:])
                break
            result.append(source[pos : m.start()])
            brace_start = m.end() - 1
            brace_end = _find_block_end(source, brace_start)
            pos = brace_end + 1
        return "".join(result)

    # ------------------------------------------------------------------
    # Statement executor
    # ------------------------------------------------------------------

    def _exec_stmt(self, stmt: str) -> Any:
        """Execute a single Perl statement and return its value."""
        stmt = stmt.strip()
        if not stmt or stmt.startswith("#"):
            return UNDEF

        # use strict / use warnings / use POSIX / use constant — ignore
        if re.match(r"^use\s+", stmt):
            # Handle 'use constant NAME => value'
            m = re.match(r"^use\s+constant\s+(\w+)\s*=>\s*(.+)$", stmt)
            if m:
                self._set_scalar(m.group(1), self._eval_expr(m.group(2).strip()))
            return UNDEF

        # require — ignore
        if re.match(r"^require\s+", stmt):
            return UNDEF

        # our / local — treat as my
        stmt = re.sub(r"^(?:our|local)\b", "my", stmt)

        # --- if/elsif/else/unless blocks ---
        if re.match(r"^(?:if|unless)\s*\(", stmt):
            return self._exec_if(stmt)

        # --- while / until ---
        if re.match(r"^(?:while|until)\s*\(", stmt):
            return self._exec_while(stmt)

        # --- for my $var (LIST) / foreach my $var (LIST) ---
        if re.match(r"^(?:for|foreach)\s+(?:my\s+)?\$", stmt):
            return self._exec_foreach(stmt)

        # --- for (C-style) or for() ---
        if re.match(r"^for\s*\(", stmt):
            # Disambiguate: for(;;) vs foreach(list)
            inner = self._extract_paren_content(stmt, stmt.index("("))
            if inner.count(";") >= 2:
                return self._exec_for_c(stmt)
            else:
                return self._exec_foreach(stmt)

        # --- foreach ---
        if re.match(r"^foreach\s*", stmt):
            return self._exec_foreach(stmt)

        # --- do { } while/until ---
        if re.match(r"^do\s*\{", stmt):
            return self._exec_do_while(stmt)

        # --- bare block ---
        if stmt.startswith("{") and self._is_block(stmt):
            return self._exec_block(stmt[1:-1].strip())

        # --- my/our declarations with optional assignment ---
        if re.match(r"^my\s*[\$@%(]", stmt):
            return self._exec_my(stmt)

        # --- Statement modifiers: EXPR if/unless/while/until/for/foreach COND ---
        for mod_kw in ("if", "unless", "while", "until", "foreach", "for"):
            pattern = rf"^(.+?)\b{mod_kw}\b(.+)$"
            m = re.match(pattern, stmt, re.DOTALL)
            if m:
                # Make sure it's a modifier (not inside a block)
                body_part = m.group(1).strip()
                cond_part = m.group(2).strip()
                if not self._contains_block(body_part):
                    return self._exec_stmt_modifier(body_part, mod_kw, cond_part)

        # --- print / say / printf / warn / die ---
        if re.match(r"^(?:print|say|printf|warn|die)\b", stmt):
            return self._exec_print(stmt)

        # --- return ---
        if re.match(r"^return\b", stmt):
            m = re.match(r"^return\b\s*(.*)", stmt, re.DOTALL)
            val = self._eval_expr(m.group(1).strip()) if m and m.group(1).strip() else UNDEF
            raise _PerlReturn(val)

        # --- last / next / redo ---
        if re.match(r"^last\b", stmt):
            m = re.match(r"^last\s*(\w+)?", stmt)
            raise _PerlLast(m.group(1) or "" if m else "")
        if re.match(r"^next\b", stmt):
            m = re.match(r"^next\s*(\w+)?", stmt)
            raise _PerlNext(m.group(1) or "" if m else "")
        if re.match(r"^redo\b", stmt):
            raise _PerlRedo()

        # --- exit ---
        if re.match(r"^exit\b", stmt):
            raise _PerlExit()

        # --- die ---
        if re.match(r"^die\b", stmt):
            m = re.match(r"^die\s*(.*)", stmt)
            msg = self._eval_expr(m.group(1).strip()) if m and m.group(1).strip() else "Died"
            raise _PerlDie(_to_str(msg).rstrip("\n"))

        # --- Expression / assignment ---
        return self._eval_expr(stmt)

    def _exec_block(self, body: str) -> Any:
        """Execute a block of statements, return last value."""
        stmts = _split_statements(body)
        last = UNDEF
        for s in stmts:
            s = s.strip()
            if s:
                last = self._exec_stmt(s)
        return last

    def _is_block(self, stmt: str) -> bool:
        """True if stmt is a bare {...} block (not a hash ref)."""
        if not (stmt.startswith("{") and stmt.endswith("}")):
            return False
        inner = stmt[1:-1].strip()
        # Heuristic: if inner looks like key => val, it's a hash
        if re.match(r"^\w+\s*=>", inner):
            return False
        return True

    def _contains_block(self, text: str) -> bool:
        """True if text contains an unbalanced or block construct."""
        depth = 0
        in_sq = in_dq = False
        for c in text:
            if in_sq:
                if c == "'":
                    in_sq = False
            elif in_dq:
                if c == '"':
                    in_dq = False
            elif c == "'":
                in_sq = True
            elif c == '"':
                in_dq = True
            elif c in "({[":
                depth += 1
            elif c in ")}]":
                depth -= 1
        return depth != 0 or in_sq or in_dq

    # ------------------------------------------------------------------
    # if/elsif/else/unless
    # ------------------------------------------------------------------

    def _exec_if(self, stmt: str) -> Any:
        """Execute if/elsif/else/unless chain."""
        pos = 0
        # Find keyword
        m = re.match(r"^(if|unless)\s*\(", stmt)
        if not m:
            return UNDEF
        negate = m.group(1) == "unless"
        # Extract condition
        paren_start = stmt.index("(", m.start())
        cond_str = self._extract_paren_content(stmt, paren_start)
        pos = paren_start + len(cond_str) + 2  # skip parens

        # Find body block
        pos = self._skip_ws(stmt, pos)
        if pos >= len(stmt) or stmt[pos] != "{":
            return UNDEF
        body_end = _find_block_end(stmt, pos)
        body = stmt[pos + 1 : body_end]
        pos = body_end + 1

        cond_val = self._eval_expr(cond_str)
        if negate:
            cond_val = not _is_true(cond_val)
        else:
            cond_val = _is_true(cond_val)

        if cond_val:
            return self._exec_block(body)

        # elsif/else chain
        while pos < len(stmt):
            pos = self._skip_ws(stmt, pos)
            if pos >= len(stmt):
                break
            if stmt[pos:].startswith("elsif"):
                pos += 5
                pos = self._skip_ws(stmt, pos)
                if stmt[pos] != "(":
                    break
                cond_str2 = self._extract_paren_content(stmt, pos)
                pos = pos + len(cond_str2) + 2
                pos = self._skip_ws(stmt, pos)
                if pos >= len(stmt) or stmt[pos] != "{":
                    break
                body2_end = _find_block_end(stmt, pos)
                body2 = stmt[pos + 1 : body2_end]
                pos = body2_end + 1
                if _is_true(self._eval_expr(cond_str2)):
                    return self._exec_block(body2)
            elif stmt[pos:].startswith("else"):
                pos += 4
                pos = self._skip_ws(stmt, pos)
                if pos >= len(stmt) or stmt[pos] != "{":
                    break
                body_e_end = _find_block_end(stmt, pos)
                body_e = stmt[pos + 1 : body_e_end]
                return self._exec_block(body_e)
            else:
                break
        return UNDEF

    # ------------------------------------------------------------------
    # while / until
    # ------------------------------------------------------------------

    def _exec_while(self, stmt: str) -> Any:
        m = re.match(r"^(while|until)\s*\(", stmt)
        if not m:
            return UNDEF
        negate = m.group(1) == "until"
        paren_start = stmt.index("(", m.start())
        cond_str = self._extract_paren_content(stmt, paren_start)
        pos = paren_start + len(cond_str) + 2
        pos = self._skip_ws(stmt, pos)
        if pos >= len(stmt) or stmt[pos] != "{":
            return UNDEF
        body_end = _find_block_end(stmt, pos)
        body = stmt[pos + 1 : body_end]

        # optional while modifier (continue block not implemented)
        itr = 0
        while True:
            itr += 1
            if itr > 100000:
                self._emit_line("❌ Infinite loop detected")
                break
            cond = _is_true(self._eval_expr(cond_str))
            if negate:
                cond = not cond
            if not cond:
                break
            try:
                self._exec_block(body)
            except _PerlLast:
                break
            except _PerlNext:
                continue
            except _PerlRedo:
                continue
        return UNDEF

    # ------------------------------------------------------------------
    # for (C-style)
    # ------------------------------------------------------------------

    def _exec_for_c(self, stmt: str) -> Any:
        paren_start = stmt.index("(")
        inner = self._extract_paren_content(stmt, paren_start)
        parts = inner.split(";", 2)
        if len(parts) < 3:
            return UNDEF
        init_s, cond_s, incr_s = [p.strip() for p in parts]
        pos = paren_start + len(inner) + 2
        pos = self._skip_ws(stmt, pos)
        if pos >= len(stmt) or stmt[pos] != "{":
            return UNDEF
        body_end = _find_block_end(stmt, pos)
        body = stmt[pos + 1 : body_end]

        if init_s:
            self._exec_stmt(init_s)
        itr = 0
        while True:
            itr += 1
            if itr > 100000:
                self._emit_line("❌ Infinite loop detected")
                break
            if cond_s and not _is_true(self._eval_expr(cond_s)):
                break
            try:
                self._exec_block(body)
            except _PerlLast:
                break
            except _PerlNext:
                pass
            except _PerlRedo:
                continue
            if incr_s:
                self._exec_stmt(incr_s)
        return UNDEF

    # ------------------------------------------------------------------
    # foreach
    # ------------------------------------------------------------------

    def _exec_foreach(self, stmt: str) -> Any:
        # foreach my $var (LIST) { ... }
        # foreach (LIST) { ... }   -> $_ is set
        # for my $var (LIST) { ... }
        m = re.match(r"^(?:foreach|for)\s*(?:my\s+)?(\$\w+)?\s*\(", stmt)
        if not m:
            return UNDEF
        var_name = m.group(1)  # e.g. "$item" or None (use $_)
        paren_start = stmt.index("(", m.start())
        list_str = self._extract_paren_content(stmt, paren_start)
        pos = paren_start + len(list_str) + 2
        pos = self._skip_ws(stmt, pos)
        if pos >= len(stmt) or stmt[pos] != "{":
            return UNDEF
        body_end = _find_block_end(stmt, pos)
        body = stmt[pos + 1 : body_end]

        items = self._eval_list(list_str)
        scalar_name = var_name[1:] if var_name else "_"

        for item in items:
            self._set_scalar(scalar_name, item)
            try:
                self._exec_block(body)
            except _PerlLast:
                break
            except _PerlNext:
                continue
            except _PerlRedo:
                continue
        return UNDEF

    # ------------------------------------------------------------------
    # do { } while / until
    # ------------------------------------------------------------------

    def _exec_do_while(self, stmt: str) -> Any:
        brace_start = stmt.index("{")
        brace_end = _find_block_end(stmt, brace_start)
        body = stmt[brace_start + 1 : brace_end]
        tail = stmt[brace_end + 1 :].strip()

        m = re.match(r"^(while|until)\s*\((.+)\)", tail)
        if not m:
            # just do { } block
            return self._exec_block(body)

        negate = m.group(1) == "until"
        cond_str = m.group(2)

        itr = 0
        while True:
            itr += 1
            if itr > 100000:
                self._emit_line("❌ Infinite loop detected")
                break
            try:
                self._exec_block(body)
            except _PerlLast:
                break
            except _PerlNext:
                pass
            cond = _is_true(self._eval_expr(cond_str))
            if negate:
                cond = not cond
            if not cond:
                break
        return UNDEF

    # ------------------------------------------------------------------
    # Statement modifiers
    # ------------------------------------------------------------------

    def _exec_stmt_modifier(self, body: str, kw: str, cond: str) -> Any:
        if kw in ("if",):
            if _is_true(self._eval_expr(cond)):
                return self._exec_stmt(body)
        elif kw == "unless":
            if not _is_true(self._eval_expr(cond)):
                return self._exec_stmt(body)
        elif kw == "while":
            itr = 0
            while _is_true(self._eval_expr(cond)):
                itr += 1
                if itr > 100000:
                    self._emit_line("❌ Infinite loop detected")
                    break
                self._exec_stmt(body)
        elif kw == "until":
            itr = 0
            while not _is_true(self._eval_expr(cond)):
                itr += 1
                if itr > 100000:
                    self._emit_line("❌ Infinite loop detected")
                    break
                self._exec_stmt(body)
        elif kw in ("for", "foreach"):
            items = self._eval_list(cond)
            for item in items:
                self._set_scalar("_", item)
                self._exec_stmt(body)
        return UNDEF

    # ------------------------------------------------------------------
    # print / say / printf / warn / die
    # ------------------------------------------------------------------

    def _exec_print(self, stmt: str) -> Any:
        m = re.match(r"^(print|say|printf|warn|die)\b\s*", stmt)
        if not m:
            return UNDEF
        func = m.group(1)
        rest = stmt[m.end():].strip()

        # Handle filehandle: print STDOUT "..."  print STDERR "..."
        fh_m = re.match(r"^(STDOUT|STDERR|STDIN)\s+", rest)
        if fh_m:
            rest = rest[fh_m.end():]

        if func == "printf":
            args = self._eval_list(rest)
            if args:
                fmt = _to_str(args[0])
                try:
                    text = self._sprintf(fmt, list(args[1:]))
                except Exception:
                    text = fmt
                self._emit(text)
        elif func in ("print", "say"):
            # Evaluate as a list
            items = self._eval_list(rest) if rest else [self._get_scalar("_")]
            text = _to_str(self._get_scalar(",")).join(_to_str(x) for x in items)
            if func == "say":
                text += "\n"
            ors = _to_str(self._get_scalar("\\"))
            self._emit(text + ors)
        elif func == "warn":
            items = self._eval_list(rest) if rest else ["Warning: something's wrong"]
            text = "".join(_to_str(x) for x in items).rstrip("\n")
            self._emit_line(f"⚠️ {text}")
        elif func == "die":
            items = self._eval_list(rest) if rest else ["Died"]
            text = "".join(_to_str(x) for x in items).rstrip("\n")
            raise _PerlDie(text)
        return UNDEF

    # ------------------------------------------------------------------
    # my declarations
    # ------------------------------------------------------------------

    def _exec_my(self, stmt: str) -> Any:
        # my $var = EXPR  or  my @arr = (LIST)  or  my %hash = (...)
        # my ($a, $b) = (LIST)  -- list assignment
        m = re.match(r"^my\s+(\$(\w+))\s*(?:=\s*(.+))?$", stmt, re.DOTALL)
        if m:
            name = m.group(2)
            rhs = m.group(3)
            val = self._eval_expr(rhs.strip()) if rhs else UNDEF
            self._set_scalar(name, val)
            return val

        m = re.match(r"^my\s+(@(\w+))\s*(?:=\s*(.+))?$", stmt, re.DOTALL)
        if m:
            name = m.group(2)
            rhs = m.group(3)
            if rhs:
                items = self._eval_list(rhs.strip())
                self._set_array(name, items)
            else:
                self._set_array(name, [])
            return UNDEF

        m = re.match(r"^my\s+(%(\w+))\s*(?:=\s*(.+))?$", stmt, re.DOTALL)
        if m:
            name = m.group(2)
            rhs = m.group(3)
            if rhs:
                items = self._eval_list(rhs.strip())
                h = {_to_str(items[i]): items[i + 1] for i in range(0, len(items) - 1, 2)}
                self._set_hash(name, h)
            else:
                self._set_hash(name, {})
            return UNDEF

        # my ($a, $b, ...) = LIST
        m = re.match(r"^my\s+\(([^)]+)\)\s*=\s*(.+)$", stmt, re.DOTALL)
        if m:
            vars_str = m.group(1)
            rhs = m.group(2).strip()
            var_names = [v.strip() for v in vars_str.split(",")]
            items = self._eval_list(rhs)
            for idx, vname in enumerate(var_names):
                vname = vname.strip()
                if vname.startswith("$"):
                    self._set_scalar(vname[1:], items[idx] if idx < len(items) else UNDEF)
                elif vname.startswith("@"):
                    self._set_array(vname[1:], list(items[idx:]))
                    break
            return UNDEF

        # Fall through to expression eval
        return self._eval_expr(stmt)

    # ------------------------------------------------------------------
    # List evaluator
    # ------------------------------------------------------------------

    def _eval_list(self, text: str) -> List[Any]:
        """Evaluate text as a Perl list, return Python list."""
        text = text.strip()
        if not text:
            return []

        # qw(...)
        m = re.match(r"^qw\s*[\(\[/!|](.+?)[\)\]/!|]$", text, re.DOTALL)
        if m:
            return m.group(1).split()

        # Unwrap outer parens if present
        if text.startswith("(") and text.endswith(")"):
            inner = text[1:-1].strip()
            if inner:
                return self._split_list(inner)
            return []

        # @array in list context
        if text.startswith("@"):
            name = text[1:].strip()
            if re.match(r"^\w+$", name):
                return list(self._get_array(name))

        # keys/values %hash
        if re.match(r"^keys\s+%", text):
            name = text[text.index("%") + 1:].strip()
            return list(self._get_hash(name).keys())
        if re.match(r"^values\s+%", text):
            name = text[text.index("%") + 1:].strip()
            return list(self._get_hash(name).values())

        # range expression 1..10
        m = re.match(r"^(.+?)\.\.\s*(.+)$", text)
        if m:
            start = _to_int(self._eval_expr(m.group(1).strip()))
            end = _to_int(self._eval_expr(m.group(2).strip()))
            return list(range(start, end + 1))

        # split the comma-separated list
        return self._split_list(text)

    def _split_list(self, text: str) -> List[Any]:
        """Split comma-separated expressions and evaluate each."""
        # Pre-process fat comma: BAREWORD => becomes "BAREWORD",
        text = re.sub(r"(?<![=!<>])\b([A-Za-z_][A-Za-z0-9_]*)\s*=>", r'"\1",', text)
        parts = self._split_by_comma(text)
        result: List[Any] = []
        for p in parts:
            p = p.strip()
            if not p:
                continue
            # Range expansion inline: 1..5
            m = re.match(r"^(.+?)\.\.\s*(.+)$", p)
            if m:
                try:
                    start = _to_int(self._eval_expr(m.group(1).strip()))
                    end = _to_int(self._eval_expr(m.group(2).strip()))
                    result.extend(range(start, end + 1))
                    continue
                except Exception:
                    pass
            # @array expansion
            if p.startswith("@"):
                name = p[1:]
                if re.match(r"^\w+$", name):
                    result.extend(self._get_array(name))
                    continue
            val = self._eval_expr(p)
            if isinstance(val, list):
                result.extend(val)
            else:
                result.append(val)
        return result

    def _split_by_comma(self, text: str) -> List[str]:
        """Split text by commas, respecting parens/quotes/etc."""
        parts: List[str] = []
        depth = 0
        in_sq = in_dq = False
        buf: List[str] = []
        i = 0
        while i < len(text):
            c = text[i]
            if in_sq:
                buf.append(c)
                if c == "'":
                    in_sq = False
            elif in_dq:
                if c == "\\" and i + 1 < len(text):
                    buf.append(c)
                    buf.append(text[i + 1])
                    i += 2
                    continue
                buf.append(c)
                if c == '"':
                    in_dq = False
            elif c == "'":
                in_sq = True
                buf.append(c)
            elif c == '"':
                in_dq = True
                buf.append(c)
            elif c in "([{":
                depth += 1
                buf.append(c)
            elif c in ")]}":
                depth -= 1
                buf.append(c)
            elif c == "," and depth == 0:
                parts.append("".join(buf))
                buf = []
                i += 1
                continue
            else:
                buf.append(c)
            i += 1
        if buf:
            parts.append("".join(buf))
        return parts

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str) -> Any:
        """Evaluate a Perl expression and return its value."""
        expr = expr.strip()
        if not expr:
            return UNDEF

        # --- Parenthesized expression ---
        if expr.startswith("(") and expr.endswith(")"):
            inner = expr[1:-1].strip()
            # Could be a list; if no commas, eval as scalar
            if "," in inner:
                parts = self._split_list(inner)
                return parts[-1] if parts else UNDEF
            return self._eval_expr(inner)

        # --- String literals ---
        if expr.startswith('"') and len(expr) >= 2:
            # Find the REAL closing quote (not just expr[-1])
            pos = 1
            while pos < len(expr):
                if expr[pos] == '\\' and pos + 1 < len(expr):
                    pos += 2
                    continue
                if expr[pos] == '"':
                    break
                pos += 1
            if pos == len(expr) - 1:
                return self._interp_dquote(expr[1:-1])
        if expr.startswith("'") and len(expr) >= 2:
            pos = 1
            while pos < len(expr):
                if expr[pos] == '\\' and pos + 1 < len(expr):
                    pos += 2
                    continue
                if expr[pos] == "'":
                    break
                pos += 1
            if pos == len(expr) - 1:
                return self._interp_squote(expr[1:-1])
        # qq{...}
        m = re.match(r"^qq\s*\{(.+)\}$", expr, re.DOTALL)
        if m:
            return self._interp_dquote(m.group(1))
        # q{...}
        m = re.match(r"^q\s*\{(.+)\}$", expr, re.DOTALL)
        if m:
            return m.group(1)

        # --- qw(...) -> list, return as array ref in scalar context ---
        m = re.match(r"^qw\s*[\(\[/!|](.+?)[\)\]/!|]$", expr, re.DOTALL)
        if m:
            return m.group(1).split()

        # --- Heredoc (basic <<END ... END) ---
        m = re.match(r'^<<(\w+)$', expr, re.DOTALL)
        if m:
            # Can't really do heredocs in expression context here
            return ""

        # --- Numbers ---
        m = re.match(r"^0x([0-9a-fA-F]+)$", expr)
        if m:
            return int(m.group(1), 16)
        m = re.match(r"^0b([01]+)$", expr)
        if m:
            return int(m.group(1), 2)
        m = re.match(r"^0([0-7]+)$", expr)
        if m:
            return int(m.group(1), 8)
        m = re.match(r"^-?\d+\.?\d*(?:[eE][+-]?\d+)?$", expr)
        if m:
            val = float(expr)
            return int(val) if val == int(val) else val

        # --- undef ---
        if expr == "undef":
            return UNDEF

        # --- Boolean literals ---
        if expr == "1":
            return 1
        if expr in ("0", '""', "''"):
            return 0

        # --- Ternary: COND ? A : B ---
        tern = self._find_ternary(expr)
        if tern:
            cond_s, then_s, else_s = tern
            return self._eval_expr(then_s) if _is_true(self._eval_expr(cond_s)) else self._eval_expr(else_s)

        # --- Logical operators (short-circuit) ---
        op_expr = self._find_binary_op(expr, ("||", "or"))
        if op_expr:
            l, _, r = op_expr
            lv = self._eval_expr(l)
            return lv if _is_true(lv) else self._eval_expr(r)

        op_expr = self._find_binary_op(expr, ("&&", "and"))
        if op_expr:
            l, _, r = op_expr
            lv = self._eval_expr(l)
            return self._eval_expr(r) if _is_true(lv) else lv

        op_expr = self._find_binary_op(expr, ("//",))  # defined-or
        if op_expr:
            l, _, r = op_expr
            lv = self._eval_expr(l)
            return lv if not isinstance(lv, _PerlUndef) else self._eval_expr(r)

        # --- not / ! ---
        m = re.match(r"^(?:not\s+|!)(.+)$", expr)
        if m:
            return not _is_true(self._eval_expr(m.group(1).strip()))

        # --- String repetition x ---
        op_expr = self._find_binary_op(expr, (" x ",))
        if op_expr:
            l, _, r = op_expr
            s = _to_str(self._eval_expr(l))
            n = _to_int(self._eval_expr(r))
            return s * max(0, n)

        # --- String concatenation . ---
        op_expr = self._find_binary_op(expr, (".",), skip_double_dot=True)
        if op_expr:
            l, _, r = op_expr
            return _to_str(self._eval_expr(l)) + _to_str(self._eval_expr(r))

        # --- Regex binding: =~ and !~ (before arithmetic to prevent / misparse) ---
        op_expr = self._find_binary_op(expr, ("=~", "!~"))
        if op_expr:
            l, op, r = op_expr
            result = self._apply_regex(l.strip(), r.strip(), match=True)
            return ("" if _is_true(result) else 1) if op == "!~" else result

        # --- Comparison operators ---
        for op in ("==", "!=", "<=", ">=", "<=>", "<", ">"):
            op_expr = self._find_binary_op(expr, (op,))
            if op_expr:
                l, _, r = op_expr
                lv = _to_num(self._eval_expr(l))
                rv = _to_num(self._eval_expr(r))
                if op == "==":
                    return 1 if lv == rv else ""
                if op == "!=":
                    return 1 if lv != rv else ""
                if op == "<":
                    return 1 if lv < rv else ""
                if op == ">":
                    return 1 if lv > rv else ""
                if op == "<=":
                    return 1 if lv <= rv else ""
                if op == ">=":
                    return 1 if lv >= rv else ""
                if op == "<=>":
                    return (lv > rv) - (lv < rv)

        for op in ("eq", "ne", "lt", "gt", "le", "ge", "cmp"):
            pattern = rf"^(.+)\b{op}\b(.+)$"
            m = re.match(pattern, expr)
            if m:
                l = _to_str(self._eval_expr(m.group(1).strip()))
                r = _to_str(self._eval_expr(m.group(2).strip()))
                if op == "eq":
                    return 1 if l == r else ""
                if op == "ne":
                    return 1 if l != r else ""
                if op == "lt":
                    return 1 if l < r else ""
                if op == "gt":
                    return 1 if l > r else ""
                if op == "le":
                    return 1 if l <= r else ""
                if op == "ge":
                    return 1 if l >= r else ""
                if op == "cmp":
                    return (l > r) - (l < r)

        # --- Arithmetic: +, -, *, /, %, ** ---
        for op in ("+", "-", "*", "/", "%", "**"):
            op_expr = self._find_binary_op(expr, (op,))
            if op_expr:
                l, _, r = op_expr
                lv = _to_num(self._eval_expr(l))
                rv = _to_num(self._eval_expr(r))
                if op == "+":
                    result = lv + rv
                elif op == "-":
                    result = lv - rv
                elif op == "*":
                    result = lv * rv
                elif op == "/":
                    if rv == 0:
                        raise _PerlDie("Division by zero")
                    result = lv / rv
                elif op == "%":
                    if rv == 0:
                        raise _PerlDie("Modulo by zero")
                    result = int(lv) % int(rv)
                elif op == "**":
                    result = lv ** rv
                else:
                    result = 0
                if isinstance(result, float) and result == int(result):
                    return int(result)
                return result

        # --- Unary minus ---
        if expr.startswith("-") and not expr[1:2].isdigit():
            return -_to_num(self._eval_expr(expr[1:].strip()))

        # --- Regex match: $var =~ /.../ or $var =~ s/.../.../ ---
        m = re.match(r"^(\$\w+)\s*=~\s*(.+)$", expr)
        if m:
            return self._apply_regex(m.group(1), m.group(2).strip(), match=True)

        m = re.match(r"^(\$\w+)\s*!~\s*(.+)$", expr)
        if m:
            result = self._apply_regex(m.group(1), m.group(2).strip(), match=True)
            return "" if _is_true(result) else 1

        # --- Compound assignments ---
        for op in ("//=", ".=", "+=", "-=", "*=", "/=", "%=", "**=", "x=", "||=", "&&="):
            m = re.match(rf"^(\$\w+)\s*{re.escape(op)}\s*(.+)$", expr, re.DOTALL)
            if m:
                varname = m.group(1)[1:]
                rhs = self._eval_expr(m.group(2).strip())
                cur = self._get_scalar(varname)
                if op == ".=":
                    val = _to_str(cur) + _to_str(rhs)
                elif op == "+=":
                    val = _to_num(cur) + _to_num(rhs)
                elif op == "-=":
                    val = _to_num(cur) - _to_num(rhs)
                elif op == "*=":
                    val = _to_num(cur) * _to_num(rhs)
                elif op == "/=":
                    rv = _to_num(rhs)
                    val = _to_num(cur) / rv if rv != 0 else _PerlDie("Division by zero")
                elif op == "%=":
                    val = int(_to_num(cur)) % int(_to_num(rhs))
                elif op == "**=":
                    val = _to_num(cur) ** _to_num(rhs)
                elif op == "x=":
                    val = _to_str(cur) * max(0, _to_int(rhs))
                elif op == "||=":
                    val = cur if _is_true(cur) else rhs
                elif op == "&&=":
                    val = rhs if _is_true(cur) else cur
                elif op == "//=":
                    val = cur if not isinstance(cur, _PerlUndef) else rhs
                else:
                    val = rhs
                if isinstance(val, float) and val == int(val):
                    val = int(val)
                self._set_scalar(varname, val)
                return val

        # Array element compound assign: $arr[i] += n
        for op in ("+=", "-=", "*=", "/=", ".="):
            m = re.match(rf"^(\$\w+)\[(.+)\]\s*{re.escape(op)}\s*(.+)$", expr, re.DOTALL)
            if m:
                arr_name = m.group(1)[1:]
                idx = _to_int(self._eval_expr(m.group(2).strip()))
                rhs = _to_num(self._eval_expr(m.group(3).strip()))
                arr = self._get_array(arr_name)
                if idx < 0:
                    idx = len(arr) + idx
                if 0 <= idx < len(arr):
                    cur = _to_num(arr[idx])
                    if op == "+=":
                        arr[idx] = cur + rhs
                    elif op == "-=":
                        arr[idx] = cur - rhs
                    elif op == "*=":
                        arr[idx] = cur * rhs
                    elif op == "/=":
                        arr[idx] = cur / rhs if rhs != 0 else 0
                    elif op == ".=":
                        arr[idx] = _to_str(arr[idx]) + _to_str(rhs)
                return arr[idx] if 0 <= idx < len(arr) else UNDEF

        # --- Simple assignments: $var = EXPR, @arr = LIST, %hash = LIST ---
        m = re.match(r"^(?:my\s+)?(\$(\w+))\s*=\s*(.+)$", expr, re.DOTALL)
        if m and not m.group(3).startswith(("=", ">", "~")):
            name = m.group(2)
            rhs = self._eval_expr(m.group(3).strip())
            self._set_scalar(name, rhs)
            return rhs

        m = re.match(r"^(?:my\s+)?(@(\w+))\s*=\s*(.+)$", expr, re.DOTALL)
        if m:
            name = m.group(2)
            items = self._eval_list(m.group(3).strip())
            self._set_array(name, items)
            return len(items)

        m = re.match(r"^(?:my\s+)?(%(\w+))\s*=\s*(.+)$", expr, re.DOTALL)
        if m:
            name = m.group(2)
            items = self._eval_list(m.group(3).strip())
            h = {_to_str(items[i]): items[i + 1] for i in range(0, len(items) - 1, 2)}
            self._set_hash(name, h)
            return len(h)

        # Array element assign: $arr[i] = val
        m = re.match(r"^(\$(\w+))\[(.+)\]\s*=\s*(.+)$", expr, re.DOTALL)
        if m:
            arr_name = m.group(2)
            idx_s = m.group(3).strip()
            rhs = self._eval_expr(m.group(4).strip())
            idx = _to_int(self._eval_expr(idx_s))
            arr = self._get_array(arr_name)
            if idx < 0:
                idx = len(arr) + idx
            while len(arr) <= idx:
                arr.append(UNDEF)
            arr[idx] = rhs
            return rhs

        # Hash element assign: $hash{key} = val
        m = re.match(r"^(\$(\w+))\{(.+)\}\s*=\s*(.+)$", expr, re.DOTALL)
        if m:
            hash_name = m.group(2)
            key = _to_str(self._eval_expr(m.group(3).strip()))
            rhs = self._eval_expr(m.group(4).strip())
            self._get_hash(hash_name)[key] = rhs
            return rhs

        # List assignment: ($a, $b) = LIST
        m = re.match(r"^\(([^)]+)\)\s*=\s*(.+)$", expr, re.DOTALL)
        if m:
            vars_str = m.group(1)
            rhs_items = self._eval_list(m.group(2).strip())
            var_names = [v.strip() for v in vars_str.split(",")]
            for idx, vname in enumerate(var_names):
                vname = vname.strip()
                if vname.startswith("$"):
                    self._set_scalar(vname[1:], rhs_items[idx] if idx < len(rhs_items) else UNDEF)
                elif vname.startswith("@"):
                    self._set_array(vname[1:], list(rhs_items[idx:]))
                    break
            return rhs_items

        # --- Increment / decrement ---
        m = re.match(r"^\+\+(\$\w+)$", expr)
        if m:
            name = m.group(1)[1:]
            val = _to_num(self._get_scalar(name)) + 1
            val = int(val) if val == int(val) else val
            self._set_scalar(name, val)
            return val
        m = re.match(r"^--(\$\w+)$", expr)
        if m:
            name = m.group(1)[1:]
            val = _to_num(self._get_scalar(name)) - 1
            val = int(val) if val == int(val) else val
            self._set_scalar(name, val)
            return val
        m = re.match(r"^(\$\w+)\+\+$", expr)
        if m:
            name = m.group(1)[1:]
            old = _to_num(self._get_scalar(name))
            new = old + 1
            new = int(new) if new == int(new) else new
            self._set_scalar(name, new)
            return int(old) if old == int(old) else old
        m = re.match(r"^(\$\w+)--$", expr)
        if m:
            name = m.group(1)[1:]
            old = _to_num(self._get_scalar(name))
            new = old - 1
            new = int(new) if new == int(new) else new
            self._set_scalar(name, new)
            return int(old) if old == int(old) else old

        # --- Function / method calls ---
        m = re.match(r"^(\w+)\s*\((.*)\)$", expr, re.DOTALL)
        if m:
            fn_name = m.group(1)
            args_str = m.group(2).strip()
            args = self._eval_list(args_str) if args_str else []
            return self._call_builtin(fn_name, args, expr)

        # Function call without parens: push @arr, val
        m = re.match(r"^(\w+)\s+(.+)$", expr, re.DOTALL)
        if m:
            fn_name = m.group(1)
            if fn_name in self._subs or fn_name in self._BUILTINS:
                args_str = m.group(2).strip()
                args = self._eval_list(args_str) if args_str else []
                return self._call_builtin(fn_name, args, expr)

        # --- Array access: $arr[i] ---
        m = re.match(r"^(\$(\w+))\[(.+)\]$", expr)
        if m:
            arr_name = m.group(2)
            idx = _to_int(self._eval_expr(m.group(3).strip()))
            arr = self._get_array(arr_name)
            if idx < 0:
                idx = len(arr) + idx
            return arr[idx] if 0 <= idx < len(arr) else UNDEF

        # Array slice: @arr[LIST]
        m = re.match(r"^@(\w+)\[(.+)\]$", expr)
        if m:
            arr_name = m.group(1)
            indices = self._eval_list(m.group(2))
            arr = self._get_array(arr_name)
            return [arr[_to_int(i)] if 0 <= _to_int(i) < len(arr) else UNDEF for i in indices]

        # --- Hash access: $hash{key} ---
        m = re.match(r"^(\$(\w+))\{(.+)\}$", expr)
        if m:
            hash_name = m.group(2)
            key = _to_str(self._eval_expr(m.group(3).strip()))
            return self._get_hash(hash_name).get(key, UNDEF)

        # --- Array ref access: $ref->[n] ---
        m = re.match(r"^(\$\w+)->?\[(.+)\]$", expr)
        if m:
            ref_val = self._eval_expr(m.group(1))
            idx = _to_int(self._eval_expr(m.group(2).strip()))
            if isinstance(ref_val, list):
                if idx < 0:
                    idx = len(ref_val) + idx
                return ref_val[idx] if 0 <= idx < len(ref_val) else UNDEF
            return UNDEF

        # --- Hash ref access: $ref->{key} ---
        m = re.match(r"^(\$\w+)->?\{(.+)\}$", expr)
        if m:
            ref_val = self._eval_expr(m.group(1))
            key = _to_str(self._eval_expr(m.group(2).strip()))
            if isinstance(ref_val, dict):
                return ref_val.get(key, UNDEF)
            return UNDEF

        # --- References: \@arr, \%hash, \$scalar ---
        if expr.startswith("\\"):
            inner = expr[1:].strip()
            if inner.startswith("@"):
                return list(self._get_array(inner[1:]))
            if inner.startswith("%"):
                return dict(self._get_hash(inner[1:]))
            if inner.startswith("$"):
                return self._get_scalar(inner[1:])
            return self._eval_expr(inner)

        # --- Anonymous array ref: [...] ---
        if expr.startswith("[") and expr.endswith("]"):
            items = self._eval_list(expr[1:-1])
            return list(items)

        # --- Anonymous hash ref: {...} ---
        if expr.startswith("{") and expr.endswith("}"):
            inner = expr[1:-1].strip()
            items = self._eval_list(inner)
            return {_to_str(items[i]): items[i + 1] for i in range(0, len(items) - 1, 2)}

        # --- Scalar variable: $var ---
        m = re.match(r"^\$(\w+)$", expr)
        if m:
            return self._get_scalar(m.group(1))

        # --- Array in scalar context: scalar(@arr) ---
        m = re.match(r"^scalar\s+@(\w+)$", expr)
        if m:
            return len(self._get_array(m.group(1)))

        # --- @arr (list context) ---
        m = re.match(r"^@(\w+)$", expr)
        if m:
            return list(self._get_array(m.group(1)))

        # --- $#arr (last index) ---
        m = re.match(r"^\$#(\w+)$", expr)
        if m:
            return len(self._get_array(m.group(1))) - 1

        # --- %hash in scalar context ---
        m = re.match(r"^%(\w+)$", expr)
        if m:
            h = self._get_hash(m.group(1))
            return len(h)

        # Bareword string (e.g. hash keys without quotes)
        if re.match(r"^[A-Za-z_]\w*$", expr):
            return expr

        # Fallback: unknown expression
        return UNDEF

    # ------------------------------------------------------------------
    # Binary operator finder (respects nesting)
    # ------------------------------------------------------------------

    def _find_binary_op(
        self, expr: str, ops: Tuple[str, ...], skip_double_dot: bool = False
    ) -> Optional[Tuple[str, str, str]]:
        """Find the rightmost top-level binary operator."""
        # Sort by length descending so multi-char ops match first
        sorted_ops = sorted(ops, key=len, reverse=True)
        depth = 0
        in_sq = in_dq = False
        # Scan right to left for left-associativity
        i = len(expr) - 1
        while i >= 0:
            c = expr[i]
            # Track string boundaries (crude, scanning backwards)
            # Better to scan forward and record positions
            i -= 1
        # Scan forward instead
        depth = 0
        in_sq = in_dq = False
        candidates: List[Tuple[int, str]] = []
        i = 0
        n = len(expr)
        while i < n:
            c = expr[i]
            if in_sq:
                if c == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c == "'":
                    in_sq = False
                i += 1
                continue
            if in_dq:
                if c == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c == '"':
                    in_dq = False
                i += 1
                continue
            if c == "'":
                in_sq = True
                i += 1
                continue
            if c == '"':
                in_dq = True
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
                for op in sorted_ops:
                    if expr[i:i + len(op)] == op:
                        # Skip if preceded/followed by same char (e.g. == vs =)
                        before = expr[i - 1] if i > 0 else " "
                        after = expr[i + len(op)] if i + len(op) < n else " "
                        # Word-boundary check for keyword operators
                        if op in ("or", "and", "not", "eq", "ne", "lt", "gt", "le", "ge", "cmp", "x"):
                            if (before.isalnum() or before == "_" or
                                    after.isalnum() or after == "_"):
                                break
                        # skip .. when looking for .
                        if skip_double_dot and op == "." and (before == "." or after == "."):
                            break
                        # Don't match += as + 
                        if op in ("+", "-", "*", "/", ".", "%") and after == "=":
                            break
                        # Don't match ++ or -- as binary +/-
                        if op == "+" and after == "+":
                            break
                        if op == "-" and after == "-":
                            break
                        if op == "-" and before in "=+\\-*/%!&|^~,(<[{;?":
                            break  # unary minus
                        if op == "+" and before in "=+\\-*/%!&|^~,(<[{;?":
                            break  # unary plus
                        if op in ("*", "/") and before in "=+\\-*/%!&|^~,(<[{;?*":
                            break
                        candidates.append((i, op))
                        break
            i += 1

        if not candidates:
            return None

        # Use last (rightmost) candidate for left-associativity
        # For ** use first (rightmost) for right-associativity
        if "**" in ops:
            pos, op = candidates[0]  # right-associative
        else:
            pos, op = candidates[-1]  # left-associative

        left = expr[:pos].strip()
        right = expr[pos + len(op):].strip()
        if not left or not right:
            return None
        return left, op, right

    def _find_ternary(self, expr: str) -> Optional[Tuple[str, str, str]]:
        """Find COND ? THEN : ELSE at top level."""
        depth = 0
        in_sq = in_dq = False
        q_pos = c_pos = -1
        for i, c in enumerate(expr):
            if in_sq:
                if c == "'" and (i == 0 or expr[i - 1] != "\\"):
                    in_sq = False
            elif in_dq:
                if c == '"' and (i == 0 or expr[i - 1] != "\\"):
                    in_dq = False
            elif c == "'":
                in_sq = True
            elif c == '"':
                in_dq = True
            elif c in "([{":
                depth += 1
            elif c in ")]}":
                depth -= 1
            elif depth == 0:
                if c == "?" and q_pos == -1:
                    q_pos = i
                elif c == ":" and q_pos != -1 and c_pos == -1:
                    c_pos = i
        if q_pos > 0 and c_pos > q_pos:
            return expr[:q_pos].strip(), expr[q_pos + 1:c_pos].strip(), expr[c_pos + 1:].strip()
        return None

    # ------------------------------------------------------------------
    # String interpolation
    # ------------------------------------------------------------------

    def _interp_squote(self, s: str) -> str:
        """Single-quoted string: only \\ and \' are special."""
        return s.replace("\\'", "'").replace("\\\\", "\\")

    def _interp_dquote(self, s: str) -> str:
        """Double-quoted string with variable and escape interpolation."""
        result: List[str] = []
        i = 0
        while i < len(s):
            c = s[i]
            if c == "\\":
                nc = s[i + 1] if i + 1 < len(s) else ""
                escapes = {"n": "\n", "t": "\t", "r": "\r", "\\": "\\", '"': '"',
                           "a": "\a", "b": "\b", "f": "\f", "v": "\v", "0": "\0",
                           "e": "\x1b", "$": "$", "@": "@"}
                if nc in escapes:
                    result.append(escapes[nc])
                    i += 2
                    continue
                if nc == "x":
                    hex_m = re.match(r"x\{?([0-9a-fA-F]+)\}?", s[i + 1:])
                    if hex_m:
                        result.append(chr(int(hex_m.group(1), 16)))
                        i += 1 + len(hex_m.group(0))
                        continue
                result.append(nc)
                i += 2
                continue

            # Variable interpolation: $var, ${var}, $arr[n], $hash{k}
            if c == "$":
                # ${var} or $var
                m_brace = re.match(r"^\$\{(\w+)\}", s[i:])
                if m_brace:
                    result.append(_to_str(self._get_scalar(m_brace.group(1))))
                    i += len(m_brace.group(0))
                    continue
                # $var[n]
                m_arr = re.match(r"^\$(\w+)\[(\d+)\]", s[i:])
                if m_arr:
                    arr = self._get_array(m_arr.group(1))
                    idx = int(m_arr.group(2))
                    result.append(_to_str(arr[idx] if idx < len(arr) else UNDEF))
                    i += len(m_arr.group(0))
                    continue
                # $var{key}
                m_hash = re.match(r"^\$(\w+)\{(\w+)\}", s[i:])
                if m_hash:
                    h = self._get_hash(m_hash.group(1))
                    result.append(_to_str(h.get(m_hash.group(2), UNDEF)))
                    i += len(m_hash.group(0))
                    continue
                # $var
                m_var = re.match(r"^\$(\w+)", s[i:])
                if m_var:
                    result.append(_to_str(self._get_scalar(m_var.group(1))))
                    i += len(m_var.group(0))
                    continue
                result.append(c)
                i += 1
                continue

            # Array interpolation: @arr
            if c == "@":
                m_arr = re.match(r"^@(\w+)", s[i:])
                if m_arr:
                    arr = self._get_array(m_arr.group(1))
                    sep = _to_str(self._get_scalar('"'))  # $"
                    result.append(sep.join(_to_str(x) for x in arr))
                    i += len(m_arr.group(0))
                    continue

            result.append(c)
            i += 1

        return "".join(result)

    # ------------------------------------------------------------------
    # Regex operations
    # ------------------------------------------------------------------

    def _apply_regex(self, var_expr: str, pattern_expr: str, match: bool) -> Any:
        """Apply m// or s/// to $var."""
        var_name = var_expr[1:] if var_expr.startswith("$") else "_"
        text = _to_str(self._get_scalar(var_name))

        # s/pattern/replace/flags
        s_m = re.match(r"^s([/|!,])(.+?)\1(.*?)\1([gimsex]*)$", pattern_expr)
        if s_m:
            sep, pat, repl, flags = s_m.group(1), s_m.group(2), s_m.group(3), s_m.group(4)
            re_flags = 0
            if "i" in flags:
                re_flags |= re.IGNORECASE
            if "m" in flags:
                re_flags |= re.MULTILINE
            if "s" in flags:
                re_flags |= re.DOTALL
            # Perl $1, $2 -> Python \1, \2 in replacement
            py_repl = re.sub(r"\$(\d+)", r"\\\1", repl)
            py_repl = repl.replace("$1", "\\1").replace("$2", "\\2")
            try:
                count = "g" in flags
                if count:
                    new_text, n = re.subn(pat, py_repl, text, flags=re_flags)
                else:
                    new_text, n = re.subn(pat, py_repl, text, count=1, flags=re_flags)
                if n > 0:
                    self._set_scalar(var_name, new_text)
                    return n
                return ""
            except re.error:
                return ""

        # tr/y
        tr_m = re.match(r"^(?:tr|y)([/|!])(.+?)\1(.*?)\1([cdsg]*)$", pattern_expr)
        if tr_m:
            _, src_chars, dst_chars, flags = tr_m.group(1), tr_m.group(2), tr_m.group(3), tr_m.group(4)
            table = str.maketrans(src_chars, dst_chars[:len(src_chars)] if dst_chars else "")
            new_text = text.translate(table)
            self._set_scalar(var_name, new_text)
            return sum(1 for c in text if c in src_chars)

        # m/pattern/flags or /pattern/flags
        m_m = re.match(r"^(?:m?)([/|!])(.+?)\1([gimsx]*)$", pattern_expr)
        if m_m:
            _, pat, flags = m_m.group(1), m_m.group(2), m_m.group(3)
            re_flags = 0
            if "i" in flags:
                re_flags |= re.IGNORECASE
            if "m" in flags:
                re_flags |= re.MULTILINE
            if "s" in flags:
                re_flags |= re.DOTALL
            if "g" in flags:
                matches = re.findall(pat, text, flags=re_flags)
                if matches:
                    for idx, grp in enumerate(matches[0] if isinstance(matches[0], tuple) else [matches[0]], 1):
                        self._set_scalar(str(idx), grp)
                return len(matches) if "g" in flags else (1 if matches else "")
            try:
                mo = re.search(pat, text, flags=re_flags)
                if mo:
                    for idx, grp in enumerate(mo.groups(), 1):
                        self._set_scalar(str(idx), grp if grp is not None else UNDEF)
                    return 1
                return ""
            except re.error:
                return ""

        return ""

    # ------------------------------------------------------------------
    # sprintf
    # ------------------------------------------------------------------

    def _sprintf(self, fmt: str, args: List[Any]) -> str:
        """Basic Perl sprintf emulation."""
        result: List[str] = []
        i = 0
        arg_idx = 0
        while i < len(fmt):
            c = fmt[i]
            if c != "%":
                result.append(c)
                i += 1
                continue
            i += 1
            if i >= len(fmt):
                break
            if fmt[i] == "%":
                result.append("%")
                i += 1
                continue
            # Parse format spec: %[flags][width][.precision]type
            spec_m = re.match(r"([-+0 #]*)(\d*)?(?:\.(\d+))?([sdifegoxXb])", fmt[i:])
            if not spec_m:
                result.append("%")
                continue
            flags, width, prec, conv = spec_m.group(1), spec_m.group(2), spec_m.group(3), spec_m.group(4)
            i += len(spec_m.group(0))
            arg = args[arg_idx] if arg_idx < len(args) else UNDEF
            arg_idx += 1
            py_fmt = "%" + flags + (width or "") + ("." + prec if prec else "") + conv
            try:
                if conv in "di":
                    result.append(py_fmt % _to_int(arg))
                elif conv in "feEgG":
                    result.append(py_fmt % _to_num(arg))
                elif conv in "oxXb":
                    result.append(py_fmt % _to_int(arg))
                else:
                    result.append(py_fmt % _to_str(arg))
            except Exception:
                result.append(_to_str(arg))
        return "".join(result)

    # ------------------------------------------------------------------
    # Built-in function dispatcher
    # ------------------------------------------------------------------

    _BUILTINS = frozenset([
        "print", "say", "printf", "warn", "die", "exit",
        "push", "pop", "shift", "unshift", "splice", "reverse",
        "sort", "grep", "map", "join", "split", "wantarray",
        "keys", "values", "each", "exists", "delete",
        "defined", "undef", "ref", "scalar",
        "length", "index", "rindex", "substr",
        "uc", "lc", "ucfirst", "lcfirst", "chomp", "chop",
        "chr", "ord", "hex", "oct",
        "sprintf", "printf",
        "abs", "int", "sqrt", "sin", "cos", "atan2", "exp", "log",
        "rand", "srand", "int",
        # Turtle
        "forward", "fd", "backward", "back", "bk",
        "left", "lt", "right", "rt",
        "penup", "pu", "pendown", "pd",
        "color", "pencolor", "setpos", "goto",
        "home", "clear_canvas", "circle", "dot", "stamp", "speed",
        "setheading", "hideturtle", "showturtle",
    ])

    def _call_builtin(self, name: str, args: List[Any], full_expr: str) -> Any:
        """Dispatch to built-in or user-defined sub."""
        # User-defined subs take priority
        if name in self._subs:
            return self._call_sub(name, args)

        # --- Array functions ---
        if name == "push":
            arr_name = self._resolve_array_name(full_expr)
            if arr_name:
                arr = self._get_array(arr_name)
                # Re-extract values from full_expr to avoid array-expansion confusion
                m_push = re.match(r"^push\s+@\w+\s*,\s*(.+)$", full_expr.strip(), re.DOTALL)
                if m_push:
                    new_items = self._eval_list(m_push.group(1))
                    arr.extend(new_items)
                elif len(args) > 1:
                    arr.extend(args[1:])
                return len(arr)
            return 0

        if name == "pop":
            arr_name = self._resolve_array_name(full_expr)
            if arr_name:
                arr = self._get_array(arr_name)
                return arr.pop() if arr else UNDEF
            return UNDEF

        if name == "shift":
            arr_name = self._resolve_array_name(full_expr)
            if arr_name:
                arr = self._get_array(arr_name)
                return arr.pop(0) if arr else UNDEF
            return UNDEF

        if name == "unshift":
            arr_name = self._resolve_array_name(full_expr)
            if arr_name:
                arr = self._get_array(arr_name)
                new_items = args[1:] if len(args) > 1 else []
                for item in reversed(new_items):
                    arr.insert(0, item)
                return len(arr)
            return 0

        if name == "splice":
            arr_name = self._resolve_array_name(full_expr)
            if arr_name:
                arr = self._get_array(arr_name)
                offset = _to_int(args[1]) if len(args) > 1 else 0
                length = _to_int(args[2]) if len(args) > 2 else len(arr) - offset
                removed = arr[offset:offset + length]
                new_items = list(args[3:]) if len(args) > 3 else []
                arr[offset:offset + length] = new_items
                return removed
            return []

        if name == "reverse":
            if args:
                if isinstance(args[0], list):
                    return list(reversed(args[0]))
                if len(args) == 1:
                    return _to_str(args[0])[::-1]
                return list(reversed(args))
            return []

        if name == "sort":
            # sort LIST or sort BLOCK LIST (simplified)
            items = []
            for a in args:
                if isinstance(a, list):
                    items.extend(a)
                else:
                    items.append(a)
            try:
                return sorted(items, key=lambda x: (_to_str(x),))
            except Exception:
                return items

        if name == "grep":
            # grep { BLOCK } LIST  -- simplified: grep(sub, list)
            # Can't easily handle blocks here, return all non-false
            items = []
            for a in args:
                if isinstance(a, list):
                    items.extend(a)
                else:
                    items.append(a)
            return [x for x in items if _is_true(x)]

        if name == "map":
            items = []
            for a in args:
                if isinstance(a, list):
                    items.extend(a)
                else:
                    items.append(a)
            return items  # simplified: identity

        if name == "join":
            if len(args) >= 2:
                sep = _to_str(args[0])
                rest = []
                for a in args[1:]:
                    if isinstance(a, list):
                        rest.extend(a)
                    else:
                        rest.append(a)
                return sep.join(_to_str(x) for x in rest)
            return ""

        if name == "split":
            # Re-extract regex pattern from full_expr to handle /pat/ containing commas
            m_regex = re.match(
                r"split\s*[\(]?\s*/(.+?)/([gimsex]*)\s*,\s*(.+?)(?:\s*,\s*(\d+))?\s*[\)]?\s*$",
                full_expr.strip(), re.DOTALL
            )
            if m_regex:
                pat_str = m_regex.group(1)
                re_flags = 0
                for fl in (m_regex.group(2) or ""):
                    if fl == "i":
                        re_flags |= re.IGNORECASE
                    if fl == "m":
                        re_flags |= re.MULTILINE
                text_val = _to_str(self._eval_expr(m_regex.group(3).strip()))
                limit = int(m_regex.group(4)) if m_regex.group(4) else 0
            else:
                if len(args) < 2:
                    return []
                pat_str = _to_str(args[0])
                text_val = _to_str(args[1])
                limit = _to_int(args[2]) if len(args) > 2 else 0
                re_flags = 0
            try:
                if pat_str in (" ", "' '"):
                    parts = text_val.split()
                elif pat_str == "":
                    parts = list(text_val)
                else:
                    parts = re.split(pat_str, text_val,
                                     maxsplit=max(0, limit - 1) if limit > 0 else 0,
                                     flags=re_flags)
                if limit <= 0:
                    while parts and parts[-1] == "":
                        parts.pop()
                return parts
            except re.error:
                return [text_val]

        if name == "wantarray":
            return ""  # Always scalar context in our executor

        # --- Hash functions ---
        if name == "keys":
            hash_name = self._resolve_hash_name(full_expr)
            if hash_name:
                return list(self._get_hash(hash_name).keys())
            return []

        if name == "values":
            hash_name = self._resolve_hash_name(full_expr)
            if hash_name:
                return list(self._get_hash(hash_name).values())
            return []

        if name == "each":
            # Simplified: returns all pairs as flat list
            hash_name = self._resolve_hash_name(full_expr)
            if hash_name:
                h = self._get_hash(hash_name)
                return list(h.items())
            return []

        if name == "exists":
            # exists $hash{key}
            m = re.search(r"%?(\w+)\{(.+)\}", full_expr)
            if m:
                key = _to_str(self._eval_expr(m.group(2).strip()))
                return 1 if key in self._get_hash(m.group(1)) else ""
            # exists $arr[n]
            m = re.search(r"@?(\w+)\[(\d+)\]", full_expr)
            if m:
                idx = int(m.group(2))
                arr = self._get_array(m.group(1))
                return 1 if idx < len(arr) else ""
            return ""

        if name == "delete":
            m = re.search(r"\$(\w+)\{(.+)\}", full_expr)
            if m:
                key = _to_str(self._eval_expr(m.group(2).strip()))
                h = self._get_hash(m.group(1))
                return h.pop(key, UNDEF)
            return UNDEF

        # --- String functions ---
        if name == "length":
            val = args[0] if args else self._get_scalar("_")
            return len(_to_str(val))

        if name == "index":
            if len(args) >= 2:
                s = _to_str(args[0])
                sub = _to_str(args[1])
                pos = _to_int(args[2]) if len(args) > 2 else 0
                return s.find(sub, pos)
            return -1

        if name == "rindex":
            if len(args) >= 2:
                s = _to_str(args[0])
                sub = _to_str(args[1])
                pos = _to_int(args[2]) if len(args) > 2 else len(s)
                return s.rfind(sub, 0, pos + 1)
            return -1

        if name == "substr":
            if len(args) >= 2:
                s = _to_str(args[0])
                offset = _to_int(args[1])
                if offset < 0:
                    offset = max(0, len(s) + offset)
                length = _to_int(args[2]) if len(args) > 2 else len(s) - offset
                result = s[offset:offset + length]
                if len(args) > 3:  # replacement
                    repl = _to_str(args[3])
                    new_s = s[:offset] + repl + s[offset + length:]
                    # Update the variable if first arg was a variable
                    m = re.search(r"\$(\w+)", full_expr)
                    if m:
                        self._set_scalar(m.group(1), new_s)
                return result
            return ""

        if name in ("uc", "uppercase"):
            return _to_str(args[0] if args else self._get_scalar("_")).upper()
        if name in ("lc", "lowercase"):
            return _to_str(args[0] if args else self._get_scalar("_")).lower()
        if name == "ucfirst":
            s = _to_str(args[0] if args else self._get_scalar("_"))
            return s[0].upper() + s[1:] if s else s
        if name == "lcfirst":
            s = _to_str(args[0] if args else self._get_scalar("_"))
            return s[0].lower() + s[1:] if s else s

        if name == "chomp":
            # In-place strip trailing newline
            var_name = self._resolve_scalar_name(full_expr)
            if var_name:
                val = _to_str(self._get_scalar(var_name))
                removed = 1 if val.endswith("\n") else 0
                self._set_scalar(var_name, val.rstrip("\n"))
                return removed
            return 0

        if name == "chop":
            var_name = self._resolve_scalar_name(full_expr)
            if var_name:
                val = _to_str(self._get_scalar(var_name))
                if val:
                    removed = val[-1]
                    self._set_scalar(var_name, val[:-1])
                    return removed
            return ""

        if name == "chr":
            return chr(_to_int(args[0]) if args else 0)

        if name == "ord":
            s = _to_str(args[0] if args else self._get_scalar("_"))
            return ord(s[0]) if s else 0

        if name == "hex":
            try:
                return int(_to_str(args[0] if args else "0"), 16)
            except ValueError:
                return 0

        if name == "oct":
            s = _to_str(args[0] if args else "0").strip()
            try:
                if s.startswith("0x") or s.startswith("0X"):
                    return int(s, 16)
                if s.startswith("0b") or s.startswith("0B"):
                    return int(s, 2)
                if s.startswith("0"):
                    return int(s, 8)
                return int(s, 8)
            except ValueError:
                return 0

        if name == "sprintf":
            if args:
                fmt = _to_str(args[0])
                return self._sprintf(fmt, list(args[1:]))
            return ""

        # --- Scalar functions ---
        if name == "defined":
            val = args[0] if args else UNDEF
            return "" if isinstance(val, _PerlUndef) else 1

        if name == "ref":
            val = args[0] if args else UNDEF
            if isinstance(val, list):
                return "ARRAY"
            if isinstance(val, dict):
                return "HASH"
            return ""

        if name == "scalar":
            val = args[0] if args else UNDEF
            if isinstance(val, list):
                return len(val)
            return val

        # --- Math functions ---
        if name == "abs":
            return abs(_to_num(args[0] if args else 0))
        if name == "int":
            return int(_to_num(args[0] if args else 0))
        if name == "sqrt":
            n = _to_num(args[0] if args else 0)
            return math.sqrt(max(0, n))
        if name == "sin":
            return math.sin(_to_num(args[0] if args else 0))
        if name == "cos":
            return math.cos(_to_num(args[0] if args else 0))
        if name == "atan2":
            if len(args) >= 2:
                return math.atan2(_to_num(args[0]), _to_num(args[1]))
            return 0.0
        if name == "exp":
            return math.exp(_to_num(args[0] if args else 0))
        if name == "log":
            n = _to_num(args[0] if args else 1)
            return math.log(n) if n > 0 else 0.0
        if name == "rand":
            n = _to_num(args[0]) if args else 1.0
            return random.random() * n
        if name == "srand":
            random.seed(_to_int(args[0]) if args else None)
            return 0

        # --- I/O ---
        if name in ("print", "say", "printf"):
            return self._exec_print(name + " " + ", ".join(_to_str(a) for a in args))

        if name == "warn":
            msg = _to_str(args[0]) if args else "Warning"
            self._emit_line(f"⚠️ {msg.rstrip()}")
            return UNDEF

        if name == "die":
            msg = _to_str(args[0]) if args else "Died"
            raise _PerlDie(msg.rstrip("\n"))

        if name == "exit":
            raise _PerlExit()

        # --- Turtle graphics ---
        t = self._turtle
        turtle_fns = {
            "forward": lambda a: t.forward(_to_num(a[0]) if a else 0),
            "fd": lambda a: t.forward(_to_num(a[0]) if a else 0),
            "backward": lambda a: t.backward(_to_num(a[0]) if a else 0),
            "back": lambda a: t.backward(_to_num(a[0]) if a else 0),
            "bk": lambda a: t.backward(_to_num(a[0]) if a else 0),
            "left": lambda a: t.left(_to_num(a[0]) if a else 0),
            "lt": lambda a: t.left(_to_num(a[0]) if a else 0),
            "right": lambda a: t.right(_to_num(a[0]) if a else 0),
            "rt": lambda a: t.right(_to_num(a[0]) if a else 0),
            "penup": lambda a: t.pen_up(),
            "pu": lambda a: t.pen_up(),
            "pendown": lambda a: t.pen_down(),
            "pd": lambda a: t.pen_down(),
            "home": lambda a: t.home(),
            "clear_canvas": lambda a: t.clear(),
            "setheading": lambda a: t.set_heading(_to_num(a[0]) if a else 0),
            "speed": lambda a: None,  # no-op
        }
        if name in turtle_fns:
            try:
                turtle_fns[name](args)
            except Exception:
                pass
            return UNDEF

        if name in ("color", "pencolor"):
            if args:
                if len(args) >= 3:
                    t.set_color(int(_to_num(args[0])), int(_to_num(args[1])), int(_to_num(args[2])))
                else:
                    color_str = _to_str(args[0])
                    _colors = {"red": (255,0,0), "green": (0,128,0), "blue": (0,0,255),
                               "black": (0,0,0), "white": (255,255,255), "yellow": (255,255,0),
                               "orange": (255,165,0), "purple": (128,0,128), "cyan": (0,255,255),
                               "magenta": (255,0,255), "brown": (139,69,19), "pink": (255,192,203)}
                    rgb = _colors.get(color_str.lower())
                    if rgb:
                        t.set_color(*rgb)
            return UNDEF

        if name == "setpos":
            if len(args) >= 2:
                t.set_position(_to_num(args[0]), _to_num(args[1]))
            return UNDEF

        if name in ("goto",):
            if len(args) >= 2:
                t.set_position(_to_num(args[0]), _to_num(args[1]))
            return UNDEF

        if name == "circle":
            if args:
                try:
                    t.circle(_to_num(args[0]))
                except Exception:
                    pass
            return UNDEF

        if name in ("dot", "stamp"):
            return UNDEF

        if name in ("hideturtle", "showturtle"):
            return UNDEF

        return UNDEF

    # ------------------------------------------------------------------
    # User-defined sub caller
    # ------------------------------------------------------------------

    def _call_sub(self, name: str, args: List[Any]) -> Any:
        """Call a user-defined subroutine."""
        if name not in self._subs:
            raise _PerlDie(f"Undefined subroutine &{name}")
        _, body = self._subs[name]
        # Push a new local frame
        frame: Dict[str, Any] = {}
        self._call_stack.append(frame)
        # Set @_ in the frame
        self._arrays["_"] = list(args)
        # Shortcut: if sub expects named params via my ($a, $b) = @_
        try:
            return self._exec_block(body)
        except _PerlReturn as r:
            return r.value
        finally:
            self._call_stack.pop()
            self._arrays.pop("_", None)

    # ------------------------------------------------------------------
    # Name extraction helpers
    # ------------------------------------------------------------------

    def _resolve_scalar_name(self, expr: str) -> Optional[str]:
        """Extract scalar variable name from an expression string."""
        m = re.search(r"\$(\w+)", expr)
        return m.group(1) if m else None

    def _resolve_array_name(self, expr: str) -> Optional[str]:
        """Extract array name from push/pop/etc. expression."""
        m = re.search(r"@(\w+)", expr)
        return m.group(1) if m else None

    def _resolve_hash_name(self, expr: str) -> Optional[str]:
        """Extract hash name from keys/values/etc. expression."""
        m = re.search(r"%(\w+)", expr)
        return m.group(1) if m else None

    # ------------------------------------------------------------------
    # Utility
    # ------------------------------------------------------------------

    def _skip_ws(self, text: str, pos: int) -> int:
        """Skip whitespace characters."""
        while pos < len(text) and text[pos] in " \t\n\r":
            pos += 1
        return pos

    def _extract_paren_content(self, text: str, start: int) -> str:
        """Extract content between matching parens starting at 'start'."""
        assert text[start] == "("
        depth = 0
        in_sq = in_dq = False
        i = start
        while i < len(text):
            c = text[i]
            if in_sq:
                if c == "'" and (i == 0 or text[i - 1] != "\\"):
                    in_sq = False
            elif in_dq:
                if c == '"' and (i == 0 or text[i - 1] != "\\"):
                    in_dq = False
            elif c == "'":
                in_sq = True
            elif c == '"':
                in_dq = True
            elif c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0:
                    return text[start + 1 : i]
            i += 1
        return text[start + 1:]
