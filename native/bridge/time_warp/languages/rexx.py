"""REXX language executor for Time Warp Studio.

Educational REXX (Restructured Extended Executor) interpreter — executed
line-by-line.  REXX is a typeless string-processing language: every value is
a string that may happen to be a number, and variables need no declaration.

Supported statements:
    SAY expr                 display an expression
    PULL var | PULL          read a line of input
    PARSE VAR name a b c     split a string into words
    var = expr               assignment (``||`` concatenates)
    IF expr THEN [DO] … [ELSE …] END
    DO [n] … END             simple repetition
    DO i = a TO b [BY s] … END
    DO var OVER list … END
    DO WHILE expr … END
    DO UNTIL expr … END
    DO FOREVER … END
    SELECT / WHEN … THEN / OTHERWISE / END
    label:                   subroutine label
    CALL name                call a subroutine
    RETURN [expr]            return from a subroutine
    EXIT | STOP              terminate the program
    LEAVE | ITERATE          break / continue the innermost loop
    NOP                      do nothing
    /* comment */  and  -- line comment

Built-in functions include LENGTH, SUBSTR, WORD, WORDS, UPPER, LOWER, STRIP,
ABS, MAX, MIN, LEFT, RIGHT, COPIES, REVERSE, INDEX, SPACE, TRUNC, SIGN.

Turtle graphics helpers: FORWARD, BACK, RIGHT, LEFT, PENUP, PENDOWN, HOME.
"""

from __future__ import annotations

import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# Block-frame kinds that represent loops (LEAVE/ITERATE target these).
_LOOP_KINDS = {"while", "until", "forever", "count", "to", "over"}


class RexxError(Exception):
    """Runtime error raised by the REXX executor."""


# ---------------------------------------------------------------------------
# Value helpers
# ---------------------------------------------------------------------------


def _fmt(value: Any) -> str:
    """Format a REXX value as a string."""
    if value is None:
        return ""
    if isinstance(value, bool):
        return "1" if value else "0"
    if isinstance(value, float):
        return str(int(value)) if value.is_integer() else repr(value)
    return str(value)


def _num(value: Any) -> float:
    """Coerce a REXX value to a number (0.0 when non-numeric)."""
    if isinstance(value, bool):
        return 1.0 if value else 0.0
    if isinstance(value, (int, float)):
        return float(value)
    try:
        return float(str(value).strip())
    except (TypeError, ValueError):
        return 0.0


def _int_if_whole(value: float) -> float | int:
    return int(value) if float(value).is_integer() else value


def _truth(value: Any) -> bool:
    """REXX truth: 0 and the empty string are false, everything else is true."""
    text = _fmt(value).strip()
    if text == "":
        return False
    try:
        return float(text) != 0.0
    except ValueError:
        return True


def _is_numeric(text: str) -> bool:
    try:
        float(text.strip())
        return True
    except (TypeError, ValueError):
        return False


# ---------------------------------------------------------------------------
# Execution state
# ---------------------------------------------------------------------------


def _state(interp: "Interpreter") -> dict:
    """Return (creating on first use) the REXX execution state."""
    state = interp.__dict__.get("_rexx_state")
    if state is None:
        state = {
            "vars": {},
            "stack": [],
            "call_stack": [],
            "labels": None,
            "waiting": None,
        }
        interp.__dict__["_rexx_state"] = state
    return state


def _get_var(interp: "Interpreter", name: str) -> Any:
    state = _state(interp)
    key = name.upper()
    if key in state["vars"]:
        return state["vars"][key]
    # Uninitialised REXX variables evaluate to their own (upper-cased) name.
    return key


def _set_var(interp: "Interpreter", name: str, value: Any) -> None:
    state = _state(interp)
    key = name.upper()
    state["vars"][key] = value
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        interp.variables[key] = float(value)


# ---------------------------------------------------------------------------
# Expression tokenizer / parser
# ---------------------------------------------------------------------------

_OPERATOR_CHARS = set("+-*/%&|<>=¬\\")

#: Keywords that must never be treated as abuttal-concatenation terms.
_CONCAT_KEYWORDS = {
    "AND", "OR", "NOT", "THEN", "ELSE", "TO", "BY", "FOR",
    "WHILE", "UNTIL", "OVER", "SELECT", "WHEN", "OTHERWISE",
}


def _tokenize(expr: str) -> list[tuple[str, str]]:
    tokens: list[tuple[str, str]] = []
    i = 0
    n = len(expr)
    while i < n:
        ch = expr[i]
        if ch.isspace():
            i += 1
            continue
        if ch in "'\"":
            quote = ch
            j = i + 1
            buf: list[str] = []
            while j < n and expr[j] != quote:
                buf.append(expr[j])
                j += 1
            tokens.append(("str", "".join(buf)))
            i = j + 1
            continue
        if ch.isdigit() or (ch == "." and i + 1 < n and expr[i + 1].isdigit()):
            j = i
            while j < n and (expr[j].isdigit() or expr[j] == "."):
                j += 1
            tokens.append(("num", expr[i:j]))
            i = j
            continue
        if ch.isalpha() or ch in "_!":
            j = i
            while j < n and (expr[j].isalnum() or expr[j] in "_!.?"):
                j += 1
            tokens.append(("sym", expr[i:j]))
            i = j
            continue
        if ch in "(),":
            tokens.append(("op", ch))
            i += 1
            continue
        two = expr[i : i + 2]
        if two in (
            "<=", ">=", "<>", "==", "//", "**", "&&", "||",
            "\\=", "\\<", "\\>",
        ):
            tokens.append(("op", two))
            i += 2
            continue
        if ch == "¬" and i + 1 < n and expr[i + 1] == "=":
            tokens.append(("op", "\\="))
            i += 2
            continue
        if ch in _OPERATOR_CHARS:
            tokens.append(("op", ch))
            i += 1
            continue
        raise RexxError(f"unexpected character '{ch}' in expression")
    return tokens


class _Parser:
    """Recursive-descent evaluator for REXX expressions."""

    def __init__(self, interp: "Interpreter", tokens: list[tuple[str, str]]):
        self.interp = interp
        self.toks = tokens
        self.pos = 0

    def _peek(self) -> tuple[str, str] | None:
        return self.toks[self.pos] if self.pos < len(self.toks) else None

    def _next(self) -> tuple[str, str]:
        tok = self.toks[self.pos]
        self.pos += 1
        return tok

    def _accept_op(self, op: str) -> bool:
        tok = self._peek()
        if tok and tok[0] == "op" and tok[1] == op:
            self.pos += 1
            return True
        return False

    def _at_keyword(self, word: str) -> bool:
        tok = self._peek()
        if not (tok and tok[0] == "sym" and tok[1].upper() == word):
            return False
        nxt = self.toks[self.pos + 1] if self.pos + 1 < len(self.toks) else None
        return nxt != ("op", "(")

    def _accept_keyword(self, word: str) -> bool:
        if self._at_keyword(word):
            self.pos += 1
            return True
        return False

    # -- grammar ---------------------------------------------------------
    def parse(self) -> Any:
        value = self._or_expr()
        if self.pos != len(self.toks):
            raise RexxError(f"unexpected token '{self.toks[self.pos][1]}' in expression")
        return value

    def _or_expr(self) -> Any:
        left = self._and_expr()
        while self._accept_op("|") or self._accept_keyword("OR"):
            right = self._and_expr()
            left = 1 if (_truth(left) or _truth(right)) else 0
        return left

    def _and_expr(self) -> Any:
        left = self._cmp_expr()
        while (
            self._accept_op("&")
            or self._accept_op("&&")
            or self._accept_keyword("AND")
        ):
            right = self._cmp_expr()
            left = 1 if (_truth(left) and _truth(right)) else 0
        return left

    def _cmp_expr(self) -> Any:
        left = self._concat_expr()
        while True:
            tok = self._peek()
            if not tok or tok[0] != "op":
                return left
            op = tok[1]
            if op in ("=", "==", "<>", "\\=", "<", ">", "<=", ">=", "\\<", "\\>"):
                self.pos += 1
                right = self._concat_expr()
                left = 1 if _compare(op, left, right) else 0
            else:
                return left

    def _concat_expr(self) -> Any:
        left = self._add_expr()
        while True:
            nxt = self._peek()
            if nxt is None:
                return left
            if nxt == ("op", "||"):
                self.pos += 1
                left = _fmt(left) + _fmt(self._add_expr())
                continue
            is_term = (
                nxt[0] in ("num", "str")
                or nxt == ("op", "(")
                or (nxt[0] == "sym" and nxt[1].upper() not in _CONCAT_KEYWORDS)
            )
            if is_term:  # REXX abuttal concatenation
                left = _fmt(left) + _fmt(self._add_expr())
                continue
            return left

    def _add_expr(self) -> Any:
        left = self._mul_expr()
        while True:
            tok = self._peek()
            if tok and tok[0] == "op" and tok[1] in ("+", "-"):
                self.pos += 1
                right = self._mul_expr()
                value = _num(left) + _num(right) if tok[1] == "+" else _num(left) - _num(right)
                left = _int_if_whole(value)
            else:
                return left

    def _mul_expr(self) -> Any:
        left = self._pow_expr()
        while True:
            tok = self._peek()
            if tok and tok[0] == "op" and tok[1] in ("*", "/", "%", "//"):
                self.pos += 1
                right = self._pow_expr()
                a, b = _num(left), _num(right)
                if b == 0 and tok[1] in ("/", "%", "//"):
                    raise RexxError("division by zero")
                if tok[1] == "*":
                    value = a * b
                elif tok[1] == "/":
                    value = a / b
                elif tok[1] == "%":
                    value = int(a / b)
                else:  # // remainder
                    value = a - b * int(a / b)
                left = _int_if_whole(float(value))
            else:
                return left

    def _pow_expr(self) -> Any:
        base = self._unary()
        if self._accept_op("**"):
            return _int_if_whole(float(_num(base) ** _num(self._pow_expr())))
        return base

    def _unary(self) -> Any:
        tok = self._peek()
        if tok and tok[0] == "op" and tok[1] in ("-", "+", "\\", "¬"):
            self.pos += 1
            value = self._unary()
            if tok[1] == "-":
                return -_num(value)
            if tok[1] == "+":
                return _num(value)
            return 0 if _truth(value) else 1
        return self._primary()

    def _primary(self) -> Any:
        kind, text = self._next()
        if kind == "num":
            return float(text) if "." in text else int(text)
        if kind == "str":
            return text
        if kind == "op" and text == "(":
            value = self._or_expr()
            if self._next() != ("op", ")"):
                raise RexxError("missing closing parenthesis")
            return value
        if kind == "sym":
            if self._peek() == ("op", "("):
                self.pos += 1
                args: list[Any] = []
                if self._peek() != ("op", ")"):
                    args.append(self._or_expr())
                    while self._accept_op(","):
                        args.append(self._or_expr())
                if self._next() != ("op", ")"):
                    raise RexxError("missing closing parenthesis in call")
                return _call_builtin(self.interp, text.upper(), args)
            return _get_var(self.interp, text)
        raise RexxError(f"unexpected token '{text}'")


def _compare(op: str, left: Any, right: Any) -> bool:
    ls, rs = _fmt(left), _fmt(right)
    numeric = _is_numeric(ls) and _is_numeric(rs)
    ln, rn = _num(left), _num(right)
    if op in ("=", "=="):
        return ln == rn if numeric else ls == rs
    if op in ("<>", "\\="):
        return (ln != rn) if numeric else (ls != rs)
    if op == "<":
        return ln < rn if numeric else ls < rs
    if op == ">":
        return ln > rn if numeric else ls > rs
    if op == "<=":
        return ln <= rn if numeric else ls <= rs
    if op == ">=":
        return ln >= rn if numeric else ls >= rs
    if op == "\\<":  # not less than ⇒ >=
        return ln >= rn if numeric else ls >= rs
    if op == "\\>":  # not greater than ⇒ <=
        return ln <= rn if numeric else ls <= rs
    return False


def _eval_expr(interp: "Interpreter", expr: str) -> Any:
    return _Parser(interp, _tokenize(expr)).parse()


# ---------------------------------------------------------------------------
# Built-in functions
# ---------------------------------------------------------------------------


def _call_builtin(interp: "Interpreter", name: str, args: list[Any]) -> Any:
    def s(i: int) -> str:
        return _fmt(args[i]) if i < len(args) else ""

    def n(i: int, default: float = 0) -> float:
        return _num(args[i]) if i < len(args) else default

    if name == "ARG":
        items = _state(interp).get("args", [])
        index = int(n(0, 1)) - 1
        return items[index] if 0 <= index < len(items) else ""
    if name == "LENGTH":
        return len(s(0))
    if name == "SUBSTR":
        text = s(0)
        start = int(n(1, 1))
        if len(args) >= 3:
            return text[start - 1 : start - 1 + int(n(2))]
        return text[start - 1 :]
    if name == "LEFT":
        return s(0)[: int(n(1))]
    if name == "RIGHT":
        count = int(n(1))
        return s(0)[-count:] if count else ""
    if name == "COPIES":
        return s(0) * max(0, int(n(1)))
    if name == "REVERSE":
        return s(0)[::-1]
    if name == "UPPER":
        return s(0).upper()
    if name == "LOWER":
        return s(0).lower()
    if name == "STRIP":
        return s(0).strip()
    if name == "SPACE":
        # REXX's SPACE(string, n, pad): n pad characters between words, one by
        # default. The count used to be ignored, which answered
        # SPACE('a b c', 0) with 'a b c' -- a plausible wrong answer rather than
        # a missing one, so both the count and the pad character are honoured.
        words = s(0).split()
        if not words:
            return ""
        count = max(0, int(n(1, 1)))
        pad = s(2) or " "
        return (pad * count).join(words)
    if name == "WORD":
        words = s(0).split()
        idx = int(n(1, 1)) - 1
        return words[idx] if 0 <= idx < len(words) else ""
    if name == "WORDS":
        return len(s(0).split())
    if name == "INDEX":
        return s(0).find(s(1)) + 1
    if name == "ABS":
        return abs(n(0))
    if name == "MAX":
        return max(_num(a) for a in args) if args else 0
    if name == "MIN":
        return min(_num(a) for a in args) if args else 0
    if name == "TRUNC":
        return int(n(0))
    if name == "SIGN":
        value = n(0)
        return 1 if value > 0 else (-1 if value < 0 else 0)
    if name == "FORMAT":
        return _fmt(n(0))
    if name == "DATATYPE":
        return "NUM" if _is_numeric(s(0)) else "CHAR"
    raise RexxError(f"unknown function {name}")


# ---------------------------------------------------------------------------
# Line helpers
# ---------------------------------------------------------------------------

_BLOCK_OPENERS = {"DO", "SELECT"}
_STRING_RE = re.compile(r"'[^']*'|\"[^\"]*\"")
_LABEL_RE = re.compile(r"^\s*([A-Za-z_!?][A-Za-z0-9_!?.]*)\s*:\s*(.*)$")


def _strip_comment(line: str) -> str:
    line = re.sub(r"/\*.*?\*/", " ", line)
    line = re.sub(r"/\*.*$", " ", line)
    if line.strip().startswith("--"):
        return ""
    return line


def _split_statements(line: str) -> list[str]:
    parts: list[str] = []
    depth = 0
    in_str = False
    quote = ""
    current: list[str] = []
    for ch in line:
        if in_str:
            current.append(ch)
            if ch == quote:
                in_str = False
            continue
        if ch in "'\"":
            in_str = True
            quote = ch
            current.append(ch)
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch == ";" and depth == 0:
            parts.append("".join(current).strip())
            current = []
            continue
        current.append(ch)
    tail = "".join(current).strip()
    if tail:
        parts.append(tail)
    return [p for p in parts if p]


def _split_args(text: str) -> list[str]:
    """Split a CALL argument list on top-level commas."""
    if not text.strip():
        return []
    parts: list[str] = []
    depth = 0
    in_str = False
    quote = ""
    current: list[str] = []
    for ch in text:
        if in_str:
            current.append(ch)
            if ch == quote:
                in_str = False
            continue
        if ch in "'\"":
            in_str = True
            quote = ch
            current.append(ch)
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(current).strip())
            current = []
            continue
        current.append(ch)
    tail = "".join(current).strip()
    if tail:
        parts.append(tail)
    return [part for part in parts if part]


def _code_words(line: str) -> list[str]:
    """Extract code words from a line, ignoring string literal contents."""
    return re.findall(r"[A-Za-z0-9_]+", _STRING_RE.sub(" ", line))


def _scan_block_end(interp: "Interpreter", start_idx: int) -> int:
    """Return the index of the END matching the block opened at *start_idx*."""
    depth = 1
    idx = start_idx + 1
    lines = interp.program_lines
    while idx < len(lines):
        for word in _code_words(_strip_comment(lines[idx][1])):
            up = word.upper()
            if up in _BLOCK_OPENERS:
                depth += 1
            elif up == "END":
                depth -= 1
        if depth <= 0:
            return idx
        idx += 1
    raise RexxError("missing END for block")


def _labels(interp: "Interpreter") -> dict[str, int]:
    state = _state(interp)
    if state["labels"] is None:
        found: dict[str, int] = {}
        for idx, (_, text) in enumerate(interp.program_lines):
            match = _LABEL_RE.match(_strip_comment(text))
            if match:
                found.setdefault(match.group(1).upper(), idx)
        state["labels"] = found
    return state["labels"]


def _statement_after_label(text: str) -> str:
    match = _LABEL_RE.match(text)
    return match.group(2).strip() if match else text.strip()


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

#: Marker: the statement was not a flow-control keyword.
_NOT_HANDLED = object()


def execute_rexx(
    interp: "Interpreter", command: str, turtle: "TurtleState"
) -> str:
    """Execute one line of REXX source."""
    state = _state(interp)
    _sync_pending_input(interp, state)

    raw = _strip_comment(command)
    if not raw.strip():
        return ""

    output: list[str] = []
    for statement in _split_statements(_statement_after_label(raw)):
        try:
            jumped = _exec_statement(interp, statement, turtle, output)
        except RexxError as exc:
            output.append(f"❌ REXX error: {exc}")
            break
        if jumped == "__JUMP__":
            break
    if not output:
        return ""
    return "\n".join(output) + "\n"


def _sync_pending_input(interp: "Interpreter", state: dict) -> None:
    waiting = state.get("waiting")
    if not waiting:
        return
    state["waiting"] = None
    stored = interp.string_variables.get(waiting.upper() + "$")
    if stored is None:
        stored = _fmt(interp.variables.get(waiting.upper(), ""))
    _set_var(interp, waiting, str(stored))


def _exec_statement(
    interp: "Interpreter",
    statement: str,
    turtle: "TurtleState",
    output: list[str],
) -> str | None:
    """Execute one statement.  Returns ``"__JUMP__"`` when control flowed."""
    text = statement.strip()
    if not text:
        return None
    upper = text.upper()

    if upper == "NOP":
        return None
    if upper == "END":
        return _handle_end(interp)

    result = _handle_flow(interp, text, upper, turtle, output)
    if result is not _NOT_HANDLED:
        return result

    if upper.startswith("SAY"):
        output.append(_fmt(_eval_expr(interp, text[3:].strip())))
        return None

    if upper in ("EXIT", "STOP") or upper.startswith(("EXIT ", "STOP ")):
        interp.running = False
        return "__JUMP__"

    if _handle_turtle(interp, upper, text, turtle):
        return None

    assign = re.match(r"^([A-Za-z_!][A-Za-z0-9_!.]*)\s*=(?!=)\s*(.+)$", text, re.S)
    if assign:
        _set_var(interp, assign.group(1), _eval_expr(interp, assign.group(2).strip()))
        return None

    raise RexxError(f"unrecognised statement: {text}")


def _handle_flow(
    interp: "Interpreter",
    text: str,
    upper: str,
    turtle: "TurtleState",
    output: list[str],
) -> Any:
    if upper.startswith("PULL") or upper.startswith("PARSE PULL"):
        return _handle_pull(interp, text)
    if upper.startswith("PARSE VAR"):
        return _handle_parse_var(interp, text[9:].strip())
    if upper.startswith("IF "):
        return _handle_if(interp, text, turtle, output)
    if upper == "DO" or upper.startswith("DO "):
        return _handle_do(interp, text)
    if upper == "SELECT":
        _state(interp)["stack"].append({"kind": "select"})
        return None
    if upper.startswith("WHEN "):
        return _handle_when(interp, text, turtle, output)
    if upper == "OTHERWISE" or upper.startswith("OTHERWISE "):
        return _handle_otherwise(interp, text, turtle, output)
    if upper.startswith("CALL "):
        return _handle_call(interp, text[5:].strip())
    if upper == "RETURN" or upper.startswith("RETURN "):
        return _handle_return(interp, text)
    if upper == "LEAVE":
        return _handle_leave(interp)
    if upper == "ITERATE":
        return _handle_iterate(interp)
    return _NOT_HANDLED


def _handle_pull(interp: "Interpreter", text: str) -> str | None:
    state = _state(interp)
    body = re.sub(r"^\s*(PARSE\s+)?PULL\s*", "", text, flags=re.I).strip()
    if not body:
        return None
    var_name = body.split()[0]
    state["waiting"] = var_name
    interp.start_input_request("", var_name.upper() + "$", False)
    return "__JUMP__"


def _handle_parse_var(interp: "Interpreter", body: str) -> str | None:
    parts = body.split()
    if len(parts) < 2:
        return None
    words = _fmt(_get_var(interp, parts[0])).split()
    for position, name in enumerate(parts[1:]):
        _set_var(interp, name, words[position] if position < len(words) else "")
    return None


def _handle_if(
    interp: "Interpreter",
    text: str,
    turtle: "TurtleState",
    output: list[str],
) -> str | None:
    match = re.match(r"^IF\s+(.*?)\s+THEN\b(.*)$", text, re.I | re.S)
    if not match:
        raise RexxError("IF requires a THEN clause")
    condition = match.group(1).strip()
    rest = match.group(2).strip()
    truth = _truth(_eval_expr(interp, condition))

    if rest.upper().startswith("DO") and len(_code_words(rest)) <= 1:
        block_end = _scan_block_end(interp, interp.current_line)
        if truth:
            _state(interp)["stack"].append({"kind": "if_then"})
            return None  # fall through into the block
        return _after_block(interp, block_end, turtle, output)

    else_match = re.match(r"^(.*?)\s+ELSE\s+(.*)$", rest, re.I | re.S)
    if else_match:
        then_stmt = else_match.group(1).strip()
        else_stmt = else_match.group(2).strip()
    else:
        then_stmt, else_stmt = rest, None

    if truth:
        return _exec_statement(interp, then_stmt, turtle, output) if then_stmt else None
    if else_stmt:
        return _exec_statement(interp, else_stmt, turtle, output)
    return None


def _after_block(
    interp: "Interpreter",
    block_end: int,
    turtle: "TurtleState",
    output: list[str],
) -> str | None:
    """Continue after *block_end*, running a following ELSE clause if present."""
    lines = interp.program_lines
    nxt = block_end + 1
    if nxt < len(lines):
        rest = _statement_after_label(_strip_comment(lines[nxt][1]).strip())
        if re.match(r"^ELSE\b", rest, re.I):
            body = re.sub(r"^ELSE\b", "", rest, flags=re.I).strip()
            interp.jump_to(nxt + 1)
            if body.upper().startswith("DO"):
                _state(interp)["stack"].append({"kind": "else_body"})
            elif body:
                _exec_statement(interp, body, turtle, output)
            return "__JUMP__"
    interp.jump_to(nxt)
    return "__JUMP__"


def _handle_do(interp: "Interpreter", text: str) -> str | None:
    state = _state(interp)
    do_idx = interp.current_line
    spec = re.sub(r"^DO\b", "", text, flags=re.I).strip()
    spec_upper = spec.upper()

    if spec_upper.startswith("WHILE "):
        cond = spec[6:].strip()
        if not _truth(_eval_expr(interp, cond)):
            interp.jump_to(_scan_block_end(interp, do_idx) + 1)
            return "__JUMP__"
        state["stack"].append({"kind": "while", "do_idx": do_idx, "cond": cond})
        return None

    if spec_upper.startswith("UNTIL "):
        state["stack"].append(
            {"kind": "until", "do_idx": do_idx, "cond": spec[6:].strip()}
        )
        return None

    if spec_upper.startswith("FOREVER"):
        state["stack"].append({"kind": "forever", "do_idx": do_idx})
        return None

    control = re.match(
        r"^([A-Za-z_!][A-Za-z0-9_!.]*)\s*=\s*(.+?)\s+TO\s+(.+?)"
        r"(?:\s+BY\s+(.+?))?(?:\s+FOR\s+.+)?$",
        spec,
        re.I,
    )
    if control:
        _set_var(interp, control.group(1), _eval_expr(interp, control.group(2).strip()))
        state["stack"].append(
            {
                "kind": "to",
                "do_idx": do_idx,
                "var": control.group(1),
                "limit": _eval_expr(interp, control.group(3).strip()),
                "step": _eval_expr(interp, control.group(4).strip()) if control.group(4) else 1,
            }
        )
        return None

    over = re.match(r"^([A-Za-z_!][A-Za-z0-9_!.]*)\s+OVER\s+(.+)$", spec, re.I)
    if over:
        items = _fmt(_eval_expr(interp, over.group(2).strip())).split()
        if not items:
            interp.jump_to(_scan_block_end(interp, do_idx) + 1)
            return "__JUMP__"
        _set_var(interp, over.group(1), items[0])
        state["stack"].append(
            {"kind": "over", "do_idx": do_idx, "var": over.group(1), "items": items, "index": 0}
        )
        return None

    if spec:
        count = int(_num(_eval_expr(interp, spec)))
        if count <= 0:
            interp.jump_to(_scan_block_end(interp, do_idx) + 1)
            return "__JUMP__"
        state["stack"].append({"kind": "count", "do_idx": do_idx, "count": count})
        return None

    state["stack"].append({"kind": "block", "do_idx": do_idx})
    return None


def _handle_end(interp: "Interpreter") -> str | None:
    state = _state(interp)
    if not state["stack"]:
        return None
    frame = state["stack"].pop()
    kind = frame["kind"]
    do_idx = frame.get("do_idx", interp.current_line)

    if kind == "if_then":
        return _skip_else(interp)
    if kind in ("block", "select", "else_body", "when_body", "otherwise_body"):
        return None
    if kind == "while":
        if _truth(_eval_expr(interp, frame["cond"])):
            state["stack"].append(frame)
            interp.jump_to(do_idx + 1)
            return "__JUMP__"
        return None
    if kind == "until":
        if _truth(_eval_expr(interp, frame["cond"])):
            return None
        state["stack"].append(frame)
        interp.jump_to(do_idx + 1)
        return "__JUMP__"
    if kind == "forever":
        state["stack"].append(frame)
        interp.jump_to(do_idx + 1)
        return "__JUMP__"
    if kind == "count":
        frame["count"] -= 1
        if frame["count"] > 0:
            state["stack"].append(frame)
            interp.jump_to(do_idx + 1)
            return "__JUMP__"
        return None
    if kind == "to":
        step = _num(frame["step"])
        current = _num(_get_var(interp, frame["var"])) + (step if step else 1)
        limit = _num(frame["limit"])
        if (step >= 0 and current <= limit) or (step < 0 and current >= limit):
            _set_var(interp, frame["var"], _int_if_whole(current))
            state["stack"].append(frame)
            interp.jump_to(do_idx + 1)
            return "__JUMP__"
        return None
    if kind == "over":
        frame["index"] += 1
        if frame["index"] < len(frame["items"]):
            _set_var(interp, frame["var"], frame["items"][frame["index"]])
            state["stack"].append(frame)
            interp.jump_to(do_idx + 1)
            return "__JUMP__"
        return None
    return None


def _skip_else(interp: "Interpreter") -> str | None:
    """Skip an ELSE clause following the END of a taken THEN block."""
    lines = interp.program_lines
    nxt = interp.current_line + 1
    if nxt >= len(lines):
        return None
    rest = _statement_after_label(_strip_comment(lines[nxt][1]).strip())
    if not re.match(r"^ELSE\b", rest, re.I):
        return None
    body = re.sub(r"^ELSE\b", "", rest, flags=re.I).strip()
    if body.upper().startswith("DO"):
        interp.jump_to(_scan_block_end(interp, nxt) + 1)
    else:
        interp.jump_to(nxt + 1)
    return "__JUMP__"


def _find_select(interp: "Interpreter") -> dict | None:
    return next(
        (f for f in reversed(_state(interp)["stack"]) if f["kind"] == "select"), None
    )


def _handle_when(
    interp: "Interpreter",
    text: str,
    turtle: "TurtleState",
    output: list[str],
) -> str | None:
    state = _state(interp)
    frame = _find_select(interp)
    if frame is None:
        raise RexxError("WHEN outside of SELECT")
    do_idx = interp.current_line
    match = re.match(r"^WHEN\s+(.*?)\s+THEN\b(.*)$", text, re.I | re.S)
    if not match:
        raise RexxError("WHEN requires a THEN clause")
    condition = match.group(1).strip()
    body = match.group(2).strip()
    body_upper = body.upper()

    if frame.get("matched") or not _truth(_eval_expr(interp, condition)):
        if body_upper.startswith("DO"):
            interp.jump_to(_scan_block_end(interp, do_idx) + 1)
            return "__JUMP__"
        return None

    frame["matched"] = True
    if body_upper.startswith("DO") and len(_code_words(body)) <= 1:
        state["stack"].append({"kind": "when_body"})
        return None
    return _exec_statement(interp, body, turtle, output) if body else None


def _handle_otherwise(
    interp: "Interpreter",
    text: str,
    turtle: "TurtleState",
    output: list[str],
) -> str | None:
    state = _state(interp)
    frame = _find_select(interp)
    if frame is None:
        raise RexxError("OTHERWISE outside of SELECT")
    do_idx = interp.current_line
    body = re.sub(r"^OTHERWISE\b", "", text, flags=re.I).strip()
    body_upper = body.upper()

    if frame.get("matched"):
        if body_upper.startswith("DO"):
            interp.jump_to(_scan_block_end(interp, do_idx) + 1)
            return "__JUMP__"
        return None

    frame["matched"] = True
    if body_upper.startswith("DO") and len(_code_words(body)) <= 1:
        state["stack"].append({"kind": "otherwise_body"})
        return None
    return _exec_statement(interp, body, turtle, output) if body else None


def _handle_call(interp: "Interpreter", name: str) -> str | None:
    state = _state(interp)
    match = re.match(
        r"^([A-Za-z_!?][A-Za-z0-9_!?.]*)\s*(?:\((.*)\))?\s*(.*)$",
        name,
        re.S,
    )
    if not match:
        raise RexxError("malformed CALL statement")
    label = match.group(1).upper()
    if match.group(2) is not None:
        arg_text = match.group(2)
    else:
        arg_text = match.group(3)
    args = [_fmt(_eval_expr(interp, part)) for part in _split_args(arg_text)]
    target = _labels(interp).get(label)
    if target is None:
        raise RexxError(f"unknown subroutine '{label}'")
    state["call_stack"].append((interp.current_line, state.get("args")))
    state["args"] = args
    interp.jump_to(target + 1)
    return "__JUMP__"


def _handle_return(interp: "Interpreter", text: str) -> str | None:
    expression = re.sub(r"^RETURN\b", "", text, flags=re.I).strip()
    if expression:
        _set_var(interp, "RESULT", _eval_expr(interp, expression))
    state = _state(interp)
    stack = state["call_stack"]
    if stack:
        return_line, previous_args = stack.pop()
        state["args"] = previous_args
        interp.jump_to(return_line + 1)
        return "__JUMP__"
    interp.running = False
    return "__JUMP__"


def _nearest_loop(interp: "Interpreter") -> tuple[int, dict] | None:
    stack = _state(interp)["stack"]
    for index in range(len(stack) - 1, -1, -1):
        if stack[index]["kind"] in _LOOP_KINDS:
            return index, stack[index]
    return None


def _handle_leave(interp: "Interpreter") -> str | None:
    found = _nearest_loop(interp)
    if found is None:
        raise RexxError("LEAVE outside of a loop")
    index, frame = found
    stack = _state(interp)["stack"]
    del stack[index:]
    interp.jump_to(_scan_block_end(interp, frame["do_idx"]) + 1)
    return "__JUMP__"


def _handle_iterate(interp: "Interpreter") -> str | None:
    found = _nearest_loop(interp)
    if found is None:
        raise RexxError("ITERATE outside of a loop")
    _, frame = found
    interp.jump_to(_scan_block_end(interp, frame["do_idx"]))
    return "__JUMP__"


def _handle_turtle(
    interp: "Interpreter", upper: str, text: str, turtle: "TurtleState"
) -> bool:
    """Handle a turtle graphics keyword; return True when handled."""
    if turtle is None:
        return False
    keyword = upper.split()[0] if upper.split() else ""
    arg = text[len(keyword) :].strip()

    def amount() -> float:
        return _num(_eval_expr(interp, arg)) if arg else 0.0

    if keyword in ("FORWARD", "FD", "MOVE"):
        turtle.forward(amount())
        return True
    if keyword in ("BACK", "BK"):
        turtle.back(amount())
        return True
    if keyword in ("RIGHT", "RT", "TURN"):
        turtle.right(amount())
        return True
    if keyword in ("LEFT", "LT"):
        turtle.left(amount())
        return True
    if keyword == "PENUP":
        turtle.penup()
        return True
    if keyword == "PENDOWN":
        turtle.pendown()
        return True
    if keyword == "HOME":
        turtle.home()
        return True
    return False
