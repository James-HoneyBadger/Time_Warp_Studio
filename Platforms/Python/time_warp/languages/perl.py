"""Perl language executor for Time Warp Studio.

Educational Perl interpreter — whole-program execution.
Supports a teaching subset of Perl 5:
  Variables: $scalar, @array, %hash
  print / say / printf / warn / die
  Arithmetic, string, comparison, logical operators
  if / elsif / else / unless
  while / until / for / foreach / do-while
  last / next / redo                -- loop control
  sub name { ... } / return         -- subroutines
  Regular expressions: =~ m//, =~ s///
  String built-ins: length, substr, index, uc, lc, ucfirst, lcfirst,
                    chomp, chop, reverse, join, split, sprintf, chdir
  Array built-ins: push, pop, shift, unshift, splice, sort, reverse,
                   scalar, wantarray
  Hash: keys, values, exists, delete
  Numeric: abs, int, sqrt, sin, cos, atan2, exp, log, rand, int
  Special variables: $_, @_, $!, $0
  use strict / use warnings (accepted, non-fatal)
  Turtle graphics: forward($n), back($n), left($n), right($n),
                   penup(), pendown(), pencolor($c), setpos($x,$y),
                   setheading($d), circle($r)
"""

from __future__ import annotations

import math
import random
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_perl(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a Perl program and return all output as a string."""
    env = PerlEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Internal exceptions
# ---------------------------------------------------------------------------


class _PerlExit(Exception):
    def __init__(self, code: int = 0):
        self.code = code


class _PerlDie(Exception):
    def __init__(self, msg: str = "Died"):
        self.msg = msg


class _PerlNext(Exception):
    pass


class _PerlLast(Exception):
    pass


class _PerlRedo(Exception):
    pass


class _PerlReturn(Exception):
    def __init__(self, value: Any = None):
        self.value = value


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------

_NUM_RE = re.compile(r"^[+-]?\s*\d+(\.\d+)?([eE][+-]?\d+)?$")


def _to_num(v: Any) -> float:
    """Coerce a Perl value to a number."""
    if isinstance(v, (int, float)):
        return float(v)
    if isinstance(v, str):
        m = re.match(r"^[+-]?\s*(\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)", v.strip())
        if m:
            return float(m.group(1))
        return 0.0
    if v is None:
        return 0.0
    return 0.0


def _to_str(v: Any) -> str:
    """Coerce a Perl value to a string."""
    if v is None:
        return ""
    if isinstance(v, bool):
        return "1" if v else ""
    if isinstance(v, float):
        if v == int(v) and abs(v) < 1e15:
            return str(int(v))
        return str(v)
    return str(v)


def _is_true(v: Any) -> bool:
    """Perl truth: false is undef, "", "0", 0."""
    if v is None:
        return False
    if isinstance(v, (int, float)):
        return v != 0
    if isinstance(v, str):
        return v != "" and v != "0"
    if isinstance(v, list):
        return len(v) > 0
    if isinstance(v, dict):
        return len(v) > 0
    return bool(v)


class PerlEnvironment:
    MAX_ITERATIONS = 50_000
    MAX_DEPTH = 200

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        # Variable storage: scalars, arrays, hashes at global scope
        self._scalars: dict[str, Any] = {"_": None, "0": "perl"}
        self._arrays: dict[str, list] = {"_": []}
        self._hashes: dict[str, dict] = {}
        # Call stack: list of (scalars, arrays, hashes) dicts for local scope
        self._call_stack: list[dict[str, Any]] = []
        self._subs: dict[str, "PerlSub"] = {}
        self._depth = 0

    # ------------------------------------------------------------------
    # Output helpers
    # ------------------------------------------------------------------

    def _emit(self, text: str) -> None:
        self._output.append(text)

    # ------------------------------------------------------------------
    # Variable accessors
    # ------------------------------------------------------------------

    def _get_scalar(self, name: str) -> Any:
        if self._call_stack:
            frame = self._call_stack[-1]
            if name in frame.get("_locals", {}):
                return frame["_locals"][name]
        return self._scalars.get(name)

    def _set_scalar(self, name: str, value: Any, local: bool = False) -> None:
        if local and self._call_stack:
            self._call_stack[-1].setdefault("_locals", {})[name] = value
            return
        if self._call_stack and name in self._call_stack[-1].get("_locals", {}):
            self._call_stack[-1]["_locals"][name] = value
            return
        self._scalars[name] = value

    def _get_array(self, name: str) -> list:
        if self._call_stack:
            frame = self._call_stack[-1]
            if name in frame.get("_local_arrays", {}):
                return frame["_local_arrays"][name]
        return self._arrays.setdefault(name, [])

    def _set_array(self, name: str, value: list, local: bool = False) -> None:
        if local and self._call_stack:
            self._call_stack[-1].setdefault("_local_arrays", {})[name] = list(value)
            return
        self._arrays[name] = list(value)

    def _get_hash(self, name: str) -> dict:
        if self._call_stack:
            frame = self._call_stack[-1]
            if name in frame.get("_local_hashes", {}):
                return frame["_local_hashes"][name]
        return self._hashes.setdefault(name, {})

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        try:
            stmts = self._parse(source)
            self._exec_block(stmts)
        except _PerlExit:
            pass
        except _PerlDie as e:
            self._emit(f"❌ {e.msg}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    # ------------------------------------------------------------------
    # Parser  (produces a list of statement dicts)
    # ------------------------------------------------------------------

    def _parse(self, source: str) -> list[dict]:
        """Tokenise source into a flat statement list."""
        # Strip shebang
        if source.lstrip().startswith("#!"):
            source = "\n" + source.split("\n", 1)[1] if "\n" in source else ""
        tokens = self._tokenise(source)
        stmts, _ = self._parse_stmts(tokens, 0)
        return stmts

    # -- Tokeniser -------------------------------------------------------

    _TOK = re.compile(
        r'(?s)'
        r'(?P<COMMENT>#[^\n]*)'
        r'|(?P<HEREDOC><<["\']?(\w+)["\']?)'
        r'|(?P<DQSTR>"(?:[^"\\]|\\.)*")'
        r"|(?P<SQSTR>'(?:[^'\\]|\\.)*')"
        r'|(?P<QWLIST>qw\s*[(\[{]\s*[^)\]}\s][^)\]}]*[)\]}])'
        r"|(?P<NUMBER>(?:0x[0-9A-Fa-f]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?))"
        r"|(?P<ARROW>->)"
        r"|(?P<SIGIL>[\$@%])"
        r"|(?P<LBRACE>\{)"
        r"|(?P<RBRACE>\})"
        r"|(?P<LBRACK>\[)"
        r"|(?P<RBRACK>\])"
        r"|(?P<LPAREN>\()"
        r"|(?P<RPAREN>\))"
        r"|(?P<SEMI>;)"
        r"|(?P<COMMA>,)"
        r"|(?P<FAT_COMMA>=>)"
        r"|(?P<OP_ASSIGN>\.=|[+\-*/%.x]?=(?!=))"
        r"|(?P<OP_CMP>==|!=|<=>|<=|>=|<|>|eq|ne|lt|gt|le|ge|cmp)"
        r"|(?P<OP_LOGIC>&&|\|\||!|and\b|or\b|not\b)"
        r"|(?P<OP_RANGE>\.\.)"
        r"|(?P<OP_MATCH>=~|!~)"
        r"|(?P<OP_INC>\+\+|--)"
        r"|(?P<OP>\+|-|\*{1,2}|/|%|\.|\||\^|~|<<|>>|&)"
        r"|(?P<REGEX>/(?:[^/\\]|\\.)+/[gimsxe]*)"
        r"|(?P<IDENT>[A-Za-z_]\w*(?:::[A-Za-z_]\w*)*)"
        r"|(?P<WS>\s+)"
    )

    def _tokenise(self, src: str) -> list[tuple[str, str]]:
        toks: list[tuple[str, str]] = []
        pos = 0
        lines = src.split("\n")
        flat = src
        heredoc_queue: list[str] = []
        i = 0
        while i < len(lines):
            line = lines[i]
            for m in self._TOK.finditer(line):
                kind = m.lastgroup
                val = m.group()
                if kind == "COMMENT":
                    continue
                if kind == "WS":
                    continue
                if kind == "HEREDOC":
                    # collect until matching terminator
                    term = re.match(r'<<["\']?(\w+)["\']?', val).group(1)
                    body_lines = []
                    i += 1
                    while i < len(lines) and lines[i].strip() != term:
                        body_lines.append(lines[i])
                        i += 1
                    toks.append(("DQSTR", '"' + "\n".join(body_lines) + '"'))
                    break
                toks.append((kind, val))
            i += 1
        return toks

    # -- Statement parser -----------------------------------------------

    def _parse_stmts(self, toks: list, pos: int, stop: str | None = None) -> tuple[list, int]:
        stmts: list[dict] = []
        while pos < len(toks):
            kind, val = toks[pos]
            if stop and kind == stop:
                break
            if kind == "RBRACE":
                break
            if kind == "SEMI":
                pos += 1
                continue
            stmt, pos = self._parse_stmt(toks, pos)
            if stmt:
                stmts.append(stmt)
        return stmts, pos

    def _peek(self, toks: list, pos: int, offset: int = 0) -> tuple[str, str]:
        idx = pos + offset
        if idx < len(toks):
            return toks[idx]
        return ("EOF", "")

    def _consume(self, toks: list, pos: int) -> tuple[tuple[str, str], int]:
        return toks[pos], pos + 1

    def _expect(self, toks: list, pos: int, kind: str) -> int:
        if pos < len(toks) and toks[pos][0] == kind:
            return pos + 1
        return pos  # lenient

    def _parse_stmt(self, toks: list, pos: int) -> tuple[dict | None, int]:
        kind, val = self._peek(toks, pos)

        # use strict / use warnings / use POSIX — ignore
        if kind == "IDENT" and val == "use":
            pos += 1
            while pos < len(toks) and toks[pos][0] not in ("SEMI",):
                pos += 1
            return None, pos

        # sub definition
        if kind == "IDENT" and val == "sub":
            return self._parse_sub(toks, pos + 1)

        # my / local declaration
        if kind == "IDENT" and val in ("my", "local", "our"):
            return self._parse_my(toks, pos)

        # if / unless (statement form)
        if kind == "IDENT" and val in ("if", "unless"):
            return self._parse_if(toks, pos)

        # while / until
        if kind == "IDENT" and val in ("while", "until"):
            return self._parse_while(toks, pos)

        # for / foreach
        if kind == "IDENT" and val in ("for", "foreach"):
            return self._parse_for(toks, pos)

        # do { } while/until
        if kind == "IDENT" and val == "do":
            nk, nv = self._peek(toks, pos, 1)
            if nk == "LBRACE":
                return self._parse_do_while(toks, pos)

        # last / next / redo
        if kind == "IDENT" and val == "last":
            pos = self._skip_to_semi(toks, pos + 1)
            return {"type": "last"}, pos
        if kind == "IDENT" and val == "next":
            pos = self._skip_to_semi(toks, pos + 1)
            return {"type": "next"}, pos
        if kind == "IDENT" and val == "redo":
            pos = self._skip_to_semi(toks, pos + 1)
            return {"type": "redo"}, pos

        # return
        if kind == "IDENT" and val == "return":
            expr_toks, pos = self._collect_expr_toks(toks, pos + 1)
            return {"type": "return", "expr": expr_toks}, pos

        # exit
        if kind == "IDENT" and val == "exit":
            expr_toks, pos = self._collect_expr_toks(toks, pos + 1)
            return {"type": "exit", "expr": expr_toks}, pos

        # die
        if kind == "IDENT" and val == "die":
            expr_toks, pos = self._collect_expr_toks(toks, pos + 1)
            return {"type": "die", "expr": expr_toks}, pos

        # print / say / warn
        if kind == "IDENT" and val in ("print", "say", "warn", "printf"):
            return self._parse_print(toks, pos)

        # push / pop / shift / unshift / splice / chomp / chop / delete / undef
        if kind == "IDENT" and val in ("push", "pop", "shift", "unshift", "splice",
                                        "chomp", "chop", "delete", "undef"):
            expr_toks, pos = self._collect_expr_toks(toks, pos + 1)
            return {"type": "call_builtin", "name": val, "args": expr_toks}, pos

        # expression statement (assignment, func call, etc.)
        expr_toks, pos = self._collect_expr_toks(toks, pos)
        if expr_toks:
            return {"type": "expr", "toks": expr_toks}, pos
        return None, pos + 1

    def _parse_sub(self, toks: list, pos: int) -> tuple[dict, int]:
        _, name = toks[pos]
        pos += 1
        pos = self._expect(toks, pos, "LBRACE")
        body, pos = self._parse_stmts(toks, pos)
        pos = self._expect(toks, pos, "RBRACE")
        return {"type": "sub", "name": name, "body": body}, pos

    def _parse_my(self, toks: list, pos: int) -> tuple[dict, int]:
        decl_kw = toks[pos][1]
        pos += 1
        expr_toks, pos = self._collect_expr_toks(toks, pos)
        return {"type": "my", "toks": expr_toks}, pos

    def _parse_if(self, toks: list, pos: int) -> tuple[dict, int]:
        kw = toks[pos][1]
        pos += 1
        pos = self._expect(toks, pos, "LPAREN")
        cond_toks, pos = self._collect_balanced(toks, pos, "LPAREN", "RPAREN")
        pos = self._expect(toks, pos, "LBRACE")
        body, pos = self._parse_stmts(toks, pos)
        pos = self._expect(toks, pos, "RBRACE")
        branches = [{"cond": cond_toks, "body": body, "invert": kw == "unless"}]
        # elsif / else
        while pos < len(toks):
            nk, nv = self._peek(toks, pos)
            if nk == "IDENT" and nv == "elsif":
                pos += 1
                pos = self._expect(toks, pos, "LPAREN")
                ec, pos = self._collect_balanced(toks, pos, "LPAREN", "RPAREN")
                pos = self._expect(toks, pos, "LBRACE")
                eb, pos = self._parse_stmts(toks, pos)
                pos = self._expect(toks, pos, "RBRACE")
                branches.append({"cond": ec, "body": eb, "invert": False})
            elif nk == "IDENT" and nv == "else":
                pos += 1
                pos = self._expect(toks, pos, "LBRACE")
                eb, pos = self._parse_stmts(toks, pos)
                pos = self._expect(toks, pos, "RBRACE")
                branches.append({"cond": None, "body": eb, "invert": False})
                break
            else:
                break
        return {"type": "if", "branches": branches}, pos

    def _parse_while(self, toks: list, pos: int) -> tuple[dict, int]:
        kw = toks[pos][1]
        pos += 1
        pos = self._expect(toks, pos, "LPAREN")
        cond_toks, pos = self._collect_balanced(toks, pos, "LPAREN", "RPAREN")
        pos = self._expect(toks, pos, "LBRACE")
        body, pos = self._parse_stmts(toks, pos)
        pos = self._expect(toks, pos, "RBRACE")
        return {"type": "while", "cond": cond_toks, "body": body, "invert": kw == "until"}, pos

    def _parse_do_while(self, toks: list, pos: int) -> tuple[dict, int]:
        pos += 1  # skip 'do'
        pos = self._expect(toks, pos, "LBRACE")
        body, pos = self._parse_stmts(toks, pos)
        pos = self._expect(toks, pos, "RBRACE")
        kw_kind, kw_val = self._peek(toks, pos)
        invert = False
        cond_toks: list = []
        if kw_kind == "IDENT" and kw_val in ("while", "until"):
            invert = kw_val == "until"
            pos += 1
            pos = self._expect(toks, pos, "LPAREN")
            cond_toks, pos = self._collect_balanced(toks, pos, "LPAREN", "RPAREN")
        return {"type": "do_while", "cond": cond_toks, "body": body, "invert": invert}, pos

    def _parse_for(self, toks: list, pos: int) -> tuple[dict, int]:
        pos += 1  # skip 'for'/'foreach'
        # Check for C-style: for (init; cond; incr)
        # or foreach $var (@list)
        nk, nv = self._peek(toks, pos)
        # Optional variable binding: foreach my $var (...)
        var_name: str | None = None
        if nk == "IDENT" and nv == "my":
            pos += 1
            nk, nv = self._peek(toks, pos)
        if nk == "SIGIL" and nv == "$":
            # Could be foreach $var (LIST)
            pos2 = pos + 1
            if pos2 < len(toks) and toks[pos2][0] == "IDENT":
                var_name = toks[pos2][1]
                pos = pos2 + 1
                nk, nv = self._peek(toks, pos)

        pos = self._expect(toks, pos, "LPAREN")
        inner, pos = self._collect_balanced(toks, pos, "LPAREN", "RPAREN")
        pos = self._expect(toks, pos, "LBRACE")
        body, pos = self._parse_stmts(toks, pos)
        pos = self._expect(toks, pos, "RBRACE")

        # Check if C-style (has two semicolons)
        semi_count = sum(1 for k, v in inner if k == "SEMI")
        if semi_count >= 2:
            # C-style for loop — split at semicolons
            parts: list[list] = [[], [], []]
            idx = 0
            for tk in inner:
                if tk[0] == "SEMI":
                    idx += 1
                    if idx >= 3:
                        break
                else:
                    if idx < 3:
                        parts[idx].append(tk)
            return {"type": "for_c", "init": parts[0], "cond": parts[1],
                    "incr": parts[2], "body": body}, pos

        return {"type": "foreach", "var": var_name or "_", "list": inner, "body": body}, pos

    def _parse_print(self, toks: list, pos: int) -> tuple[dict, int]:
        fn = toks[pos][1]
        pos += 1
        # Skip optional filehandle (bare word followed by space, no parens/comma)
        if pos < len(toks) and toks[pos][0] == "IDENT":
            # Could be filehandle like STDOUT, STDERR
            if toks[pos][1] in ("STDOUT", "STDERR", "STDIN"):
                pos += 1  # skip filehandle
        expr_toks, pos = self._collect_expr_toks(toks, pos)
        return {"type": "print", "fn": fn, "args": expr_toks}, pos

    # -- Token collection helpers ----------------------------------------

    def _skip_to_semi(self, toks: list, pos: int) -> int:
        while pos < len(toks) and toks[pos][0] != "SEMI":
            pos += 1
        if pos < len(toks):
            pos += 1  # consume the semi
        return pos

    def _collect_expr_toks(self, toks: list, pos: int) -> tuple[list, int]:
        """Collect tokens up to (not including) semicolon at depth 0."""
        result = []
        depth_p = depth_b = depth_s = 0
        while pos < len(toks):
            k, v = toks[pos]
            if k == "SEMI" and depth_p == 0 and depth_b == 0 and depth_s == 0:
                pos += 1
                break
            if k == "RBRACE" and depth_b == 0:
                break
            if k == "LPAREN":
                depth_p += 1
            elif k == "RPAREN":
                depth_p -= 1
                if depth_p < 0:
                    break
            elif k == "LBRACE":
                depth_b += 1
            elif k == "RBRACE":
                depth_b -= 1
            elif k == "LBRACK":
                depth_s += 1
            elif k == "RBRACK":
                depth_s -= 1
            result.append((k, v))
            pos += 1
        return result, pos

    def _collect_balanced(self, toks: list, pos: int, open_t: str, close_t: str) -> tuple[list, int]:
        """Collect tokens inside a matched pair (open already consumed by _expect)."""
        result = []
        depth = 1
        while pos < len(toks):
            k, v = toks[pos]
            if k == open_t:
                depth += 1
            elif k == close_t:
                depth -= 1
                if depth == 0:
                    pos += 1
                    break
            result.append((k, v))
            pos += 1
        return result, pos

    # ------------------------------------------------------------------
    # Executor
    # ------------------------------------------------------------------

    def _exec_block(self, stmts: list) -> None:
        for stmt in stmts:
            self._exec_stmt(stmt)

    def _exec_stmt(self, stmt: dict) -> None:
        t = stmt["type"]

        if t == "sub":
            self._subs[stmt["name"]] = PerlSub(stmt["name"], stmt["body"])

        elif t in ("my", "expr"):
            key = "toks" if t == "expr" else "toks"
            self._eval_toks(stmt["toks"])

        elif t == "print":
            self._do_print(stmt)

        elif t == "call_builtin":
            self._call_builtin_stmt(stmt["name"], stmt["args"])

        elif t == "if":
            for branch in stmt["branches"]:
                if branch["cond"] is None:
                    self._exec_block(branch["body"])
                    break
                cond_val = self._eval_toks(branch["cond"])
                matched = _is_true(cond_val)
                if branch["invert"]:
                    matched = not matched
                if matched:
                    self._exec_block(branch["body"])
                    break

        elif t == "while":
            iters = 0
            while True:
                iters += 1
                if iters > self.MAX_ITERATIONS:
                    self._emit("❌ Perl: infinite loop limit reached")
                    break
                cond = _is_true(self._eval_toks(stmt["cond"]))
                if stmt["invert"]:
                    cond = not cond
                if not cond:
                    break
                try:
                    self._exec_block(stmt["body"])
                except _PerlLast:
                    break
                except _PerlNext:
                    continue
                except _PerlRedo:
                    pass

        elif t == "do_while":
            iters = 0
            while True:
                iters += 1
                if iters > self.MAX_ITERATIONS:
                    self._emit("❌ Perl: infinite loop limit reached")
                    break
                try:
                    self._exec_block(stmt["body"])
                except _PerlLast:
                    break
                except _PerlNext:
                    pass
                if stmt["cond"]:
                    cond = _is_true(self._eval_toks(stmt["cond"]))
                    if stmt["invert"]:
                        cond = not cond
                    if not cond:
                        break
                else:
                    break

        elif t == "foreach":
            list_val = self._eval_toks(stmt["list"])
            if not isinstance(list_val, list):
                list_val = [list_val]
            for item in list_val:
                self._set_scalar(stmt["var"], item)
                try:
                    self._exec_block(stmt["body"])
                except _PerlLast:
                    break
                except _PerlNext:
                    continue
                except _PerlRedo:
                    pass

        elif t == "for_c":
            if stmt["init"]:
                self._eval_toks(stmt["init"])
            iters = 0
            while True:
                iters += 1
                if iters > self.MAX_ITERATIONS:
                    self._emit("❌ Perl: infinite loop limit reached")
                    break
                if stmt["cond"] and not _is_true(self._eval_toks(stmt["cond"])):
                    break
                try:
                    self._exec_block(stmt["body"])
                except _PerlLast:
                    break
                except _PerlNext:
                    pass
                if stmt["incr"]:
                    self._eval_toks(stmt["incr"])

        elif t == "return":
            val = self._eval_toks(stmt["expr"]) if stmt["expr"] else None
            raise _PerlReturn(val)

        elif t == "exit":
            code = int(_to_num(self._eval_toks(stmt["expr"]))) if stmt["expr"] else 0
            raise _PerlExit(code)

        elif t == "die":
            msg = _to_str(self._eval_toks(stmt["expr"])) if stmt["expr"] else "Died"
            msg = msg.rstrip("\n")
            raise _PerlDie(msg)

        elif t == "last":
            raise _PerlLast()

        elif t == "next":
            raise _PerlNext()

        elif t == "redo":
            raise _PerlRedo()

    # ------------------------------------------------------------------
    # Print
    # ------------------------------------------------------------------

    def _do_print(self, stmt: dict) -> None:
        fn = stmt["fn"]
        args_val = self._eval_toks(stmt["args"])
        if isinstance(args_val, list):
            text = "".join(_to_str(v) for v in args_val)
        else:
            text = _to_str(args_val)
        if fn == "say":
            text = text.rstrip("\n") + "\n"
        if fn == "warn":
            # warn goes to stderr — show as info
            self._emit(f"ℹ️ {text.rstrip()}")
            return
        # Split by newlines and emit
        for line in text.split("\n"):
            self._emit(line)
        # If text ends with \n the last split produces "", which we don't want
        if self._output and self._output[-1] == "":
            self._output.pop()

    # ------------------------------------------------------------------
    # Built-in statement forms
    # ------------------------------------------------------------------

    def _call_builtin_stmt(self, name: str, arg_toks: list) -> Any:
        val = self._eval_toks(arg_toks)
        args = val if isinstance(val, list) else [val]

        if name == "push":
            arr = _to_str(args[0]) if args else "_"
            items = args[1:] if len(args) > 1 else []
            self._get_array(arr).extend(items)
        elif name == "pop":
            arr = _to_str(args[0]) if args else "_"
            a = self._get_array(arr)
            return a.pop() if a else None
        elif name == "shift":
            arr = _to_str(args[0]) if args else "_"
            a = self._get_array(arr)
            return a.pop(0) if a else None
        elif name == "unshift":
            arr = _to_str(args[0]) if args else "_"
            items = args[1:] if len(args) > 1 else []
            a = self._get_array(arr)
            for item in reversed(items):
                a.insert(0, item)
        elif name in ("chomp", "chop"):
            # operates on $_
            v = _to_str(self._get_scalar("_"))
            if name == "chomp":
                v = v.rstrip("\n")
            else:
                v = v[:-1] if v else v
            self._set_scalar("_", v)
        elif name == "delete":
            pass  # handled in eval
        elif name == "undef":
            if args:
                self._set_scalar(_to_str(args[0]), None)
        return None

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_toks(self, toks: list) -> Any:
        """Evaluate a flat token list, returning a single value or list."""
        if not toks:
            return None
        # Delegate to recursive descent
        val, _ = self._eval_expr(toks, 0)
        return val

    def _eval_expr(self, toks: list, pos: int) -> tuple[Any, int]:
        """Parse and evaluate an expression, return (value, new_pos)."""
        return self._eval_assign(toks, pos)

    def _eval_assign(self, toks: list, pos: int) -> tuple[Any, int]:
        """Handle assignment operators."""
        left_toks_start = pos
        lhs, pos = self._eval_or(toks, pos)

        if pos < len(toks) and toks[pos][0] == "OP_ASSIGN":
            op = toks[pos][1]
            pos += 1
            rhs, pos = self._eval_assign(toks, pos)
            lhs = self._do_assign(toks, left_toks_start, op, lhs, rhs)
        return lhs, pos

    def _do_assign(self, toks: list, lhs_start: int, op: str, lhs: Any, rhs: Any) -> Any:
        """Perform an assignment, writing back to the variable."""
        # Determine what we're assigning to
        var_info = self._lvalue_from_toks(toks, lhs_start)
        if var_info is None:
            return rhs

        kind, name, idx = var_info

        # Compute new value
        if op == "=":
            new_val = rhs
        else:
            base_op = op[:-1]  # e.g. "+=" -> "+"
            new_val = self._apply_op(base_op, lhs, rhs)

        # Write back
        if kind == "scalar":
            self._set_scalar(name, new_val)
        elif kind == "array_elem":
            a = self._get_array(name)
            i = int(_to_num(idx))
            while len(a) <= abs(i):
                a.append(None)
            a[i] = new_val
        elif kind == "hash_elem":
            self._get_hash(name)[_to_str(idx)] = new_val
        elif kind == "array":
            if isinstance(rhs, list):
                self._set_array(name, rhs)
            else:
                self._set_array(name, [rhs])
        return new_val

    def _lvalue_from_toks(self, toks: list, start: int) -> tuple[str, str, Any] | None:
        """Return (kind, varname, index_or_None) from token stream at start."""
        if start >= len(toks):
            return None
        k0, v0 = toks[start]
        if k0 == "SIGIL" and v0 == "$":
            if start + 1 < len(toks) and toks[start + 1][0] == "IDENT":
                name = toks[start + 1][1]
                # Check for subscript
                if start + 2 < len(toks):
                    k2, v2 = toks[start + 2]
                    if k2 == "LBRACK":
                        # array element $arr[idx]
                        inner = []
                        p = start + 3
                        while p < len(toks) and toks[p][0] != "RBRACK":
                            inner.append(toks[p])
                            p += 1
                        idx = self._eval_toks(inner)
                        return ("array_elem", name, idx)
                    elif k2 == "LBRACE":
                        # hash element $h{key}
                        inner = []
                        p = start + 3
                        while p < len(toks) and toks[p][0] != "RBRACE":
                            inner.append(toks[p])
                            p += 1
                        idx = self._eval_toks(inner)
                        return ("hash_elem", name, idx)
                return ("scalar", name, None)
        elif k0 == "SIGIL" and v0 == "@":
            if start + 1 < len(toks) and toks[start + 1][0] == "IDENT":
                return ("array", toks[start + 1][1], None)
        elif k0 == "IDENT" and v0 == "my":
            return self._lvalue_from_toks(toks, start + 1)
        return None

    def _eval_or(self, toks: list, pos: int) -> tuple[Any, int]:
        lhs, pos = self._eval_and(toks, pos)
        while pos < len(toks):
            k, v = toks[pos]
            if k == "OP_LOGIC" and v in ("||", "or"):
                pos += 1
                if _is_true(lhs):
                    rhs, pos = self._eval_and(toks, pos)  # consume but ignore
                else:
                    lhs, pos = self._eval_and(toks, pos)
            else:
                break
        return lhs, pos

    def _eval_and(self, toks: list, pos: int) -> tuple[Any, int]:
        lhs, pos = self._eval_not(toks, pos)
        while pos < len(toks):
            k, v = toks[pos]
            if k == "OP_LOGIC" and v in ("&&", "and"):
                pos += 1
                if not _is_true(lhs):
                    rhs, pos = self._eval_not(toks, pos)  # consume
                else:
                    lhs, pos = self._eval_not(toks, pos)
            else:
                break
        return lhs, pos

    def _eval_not(self, toks: list, pos: int) -> tuple[Any, int]:
        if pos < len(toks):
            k, v = toks[pos]
            if k == "OP_LOGIC" and v in ("!", "not"):
                pos += 1
                val, pos = self._eval_not(toks, pos)
                return (not _is_true(val)), pos
        return self._eval_cmp(toks, pos)

    def _eval_cmp(self, toks: list, pos: int) -> tuple[Any, int]:
        lhs, pos = self._eval_match(toks, pos)
        while pos < len(toks) and toks[pos][0] == "OP_CMP":
            op = toks[pos][1]
            pos += 1
            rhs, pos = self._eval_match(toks, pos)
            lhs = self._do_cmp(op, lhs, rhs)
        return lhs, pos

    def _do_cmp(self, op: str, lhs: Any, rhs: Any) -> Any:
        if op in ("==", "!=", "<=>", "<", ">", "<=", ">="):
            a, b = _to_num(lhs), _to_num(rhs)
            if op == "==":
                return 1 if a == b else ""
            if op == "!=":
                return 1 if a != b else ""
            if op == "<=>":
                return 0 if a == b else (1 if a > b else -1)
            if op == "<":
                return 1 if a < b else ""
            if op == ">":
                return 1 if a > b else ""
            if op == "<=":
                return 1 if a <= b else ""
            if op == ">=":
                return 1 if a >= b else ""
        else:
            a, b = _to_str(lhs), _to_str(rhs)
            if op == "eq":
                return 1 if a == b else ""
            if op == "ne":
                return 1 if a != b else ""
            if op == "lt":
                return 1 if a < b else ""
            if op == "gt":
                return 1 if a > b else ""
            if op == "le":
                return 1 if a <= b else ""
            if op == "ge":
                return 1 if a >= b else ""
            if op == "cmp":
                return 0 if a == b else (1 if a > b else -1)
        return ""

    def _eval_match(self, toks: list, pos: int) -> tuple[Any, int]:
        lhs, pos = self._eval_range(toks, pos)
        while pos < len(toks) and toks[pos][0] == "OP_MATCH":
            op = toks[pos][1]
            pos += 1
            k, v = toks[pos] if pos < len(toks) else ("EOF", "")
            pos += 1
            if k in ("REGEX", "IDENT"):
                lhs = self._do_regex(op, lhs, v)
        return lhs, pos

    def _do_regex(self, op: str, target: str, pattern_str: str) -> Any:
        target_s = _to_str(target)
        # Parse /pattern/flags or s/pat/rep/flags
        if pattern_str.startswith("s/"):
            # substitution
            m = re.match(r"s/((?:[^/\\]|\\.)*)/([^/]*)/([gimsxe]*)", pattern_str)
            if not m:
                return ""
            pat, repl, flags = m.group(1), m.group(2), m.group(3)
            count = 0 if "g" not in flags else None
            try:
                re_flags = re.IGNORECASE if "i" in flags else 0
                if count is None:
                    result = re.sub(pat, repl, target_s, flags=re_flags)
                else:
                    result = re.sub(pat, repl, target_s, count=1, flags=re_flags)
            except re.error:
                result = target_s
            self._set_scalar("_", result)
            return result
        else:
            # match
            m2 = re.match(r"m?/((?:[^/\\]|\\.)*)/([gimsxe]*)", pattern_str)
            if not m2:
                return ""
            pat, flags = m2.group(1), m2.group(2)
            try:
                re_flags = re.IGNORECASE if "i" in flags else 0
                result = re.search(pat, target_s, flags=re_flags)
            except re.error:
                result = None
            matched = result is not None
            if op == "!~":
                matched = not matched
            return 1 if matched else ""

    def _eval_range(self, toks: list, pos: int) -> tuple[Any, int]:
        lhs, pos = self._eval_add(toks, pos)
        if pos < len(toks) and toks[pos][0] == "OP_RANGE":
            pos += 1
            rhs, pos = self._eval_add(toks, pos)
            start, end = int(_to_num(lhs)), int(_to_num(rhs))
            return list(range(start, end + 1)), pos
        return lhs, pos

    def _eval_add(self, toks: list, pos: int) -> tuple[Any, int]:
        lhs, pos = self._eval_mul(toks, pos)
        while pos < len(toks):
            k, v = toks[pos]
            if k == "OP" and v in ("+", "-", "."):
                pos += 1
                rhs, pos = self._eval_mul(toks, pos)
                lhs = self._apply_op(v, lhs, rhs)
            else:
                break
        return lhs, pos

    def _eval_mul(self, toks: list, pos: int) -> tuple[Any, int]:
        lhs, pos = self._eval_unary(toks, pos)
        while pos < len(toks):
            k, v = toks[pos]
            if k == "OP" and v in ("*", "/", "%", "**", "x"):
                pos += 1
                rhs, pos = self._eval_unary(toks, pos)
                lhs = self._apply_op(v, lhs, rhs)
            else:
                break
        return lhs, pos

    def _eval_unary(self, toks: list, pos: int) -> tuple[Any, int]:
        if pos < len(toks):
            k, v = toks[pos]
            if k == "OP" and v == "-":
                pos += 1
                val, pos = self._eval_unary(toks, pos)
                return -_to_num(val), pos
            if k == "OP_INC":
                # Pre-increment
                pos += 1
                # peek at variable
                if pos < len(toks) and toks[pos][0] == "SIGIL" and toks[pos][1] == "$":
                    if pos + 1 < len(toks) and toks[pos + 1][0] == "IDENT":
                        name = toks[pos + 1][1]
                        pos += 2
                        cur = _to_num(self._get_scalar(name))
                        new = cur + 1 if v == "++" else cur - 1
                        self._set_scalar(name, new)
                        return new, pos
        val, pos = self._eval_postfix(toks, pos)
        # Post-increment
        if pos < len(toks) and toks[pos][0] == "OP_INC":
            op = toks[pos][1]
            pos += 1
            # val already retrieved; do the side-effect
            if isinstance(val, (int, float)):
                pass  # we'll handle only named-variable post-inc in lvalue context
        return val, pos

    def _eval_postfix(self, toks: list, pos: int) -> tuple[Any, int]:
        return self._eval_primary(toks, pos)

    def _eval_primary(self, toks: list, pos: int) -> tuple[Any, int]:
        if pos >= len(toks):
            return None, pos
        k, v = toks[pos]

        # Parenthesised expression or list
        if k == "LPAREN":
            pos += 1
            items = []
            while pos < len(toks) and toks[pos][0] != "RPAREN":
                if toks[pos][0] == "COMMA":
                    pos += 1
                    continue
                val, pos = self._eval_assign(toks, pos)
                if isinstance(val, list):
                    items.extend(val)
                else:
                    items.append(val)
            pos += 1  # consume RPAREN
            if len(items) == 1:
                return items[0], pos
            return items, pos

        # Scalar variable
        if k == "SIGIL" and v == "$":
            pos += 1
            if pos >= len(toks):
                return None, pos
            k2, v2 = toks[pos]
            pos += 1
            if k2 == "IDENT":
                name = v2
                # subscript?
                if pos < len(toks):
                    k3, v3 = toks[pos]
                    if k3 == "LBRACK":
                        pos += 1
                        idx_toks, pos = self._collect_balanced_tok(toks, pos, "LBRACK", "RBRACK")
                        idx = int(_to_num(self._eval_toks(idx_toks)))
                        a = self._get_array(name)
                        return (a[idx] if idx < len(a) else None), pos
                    elif k3 == "LBRACE":
                        pos += 1
                        key_toks, pos = self._collect_balanced_tok(toks, pos, "LBRACE", "RBRACE")
                        key = _to_str(self._eval_toks(key_toks))
                        return self._get_hash(name).get(key), pos
                val = self._get_scalar(name)
                # Post-increment handling in assignment context handled elsewhere
                return val, pos
            elif k2 == "LBRACE":
                # ${ \expr } — just skip
                return None, pos
            elif k2 == "NUMBER":
                return int(v2), pos  # $1, $2 capture vars — return undef
            return None, pos

        # Array variable @arr — returns list
        if k == "SIGIL" and v == "@":
            pos += 1
            if pos < len(toks) and toks[pos][0] == "IDENT":
                name = toks[pos][1]
                pos += 1
                # @arr[slice] or just @arr
                return list(self._get_array(name)), pos
            return [], pos

        # Hash variable %hash
        if k == "SIGIL" and v == "%":
            pos += 1
            if pos < len(toks) and toks[pos][0] == "IDENT":
                name = toks[pos][1]
                pos += 1
                h = self._get_hash(name)
                flat = []
                for kk, vv in h.items():
                    flat.extend([kk, vv])
                return flat, pos
            return {}, pos

        # Number literal
        if k == "NUMBER":
            pos += 1
            v_str = v
            if v_str.startswith("0x"):
                return int(v_str, 16), pos
            try:
                return int(v_str), pos
            except ValueError:
                return float(v_str), pos

        # Double-quoted string
        if k == "DQSTR":
            pos += 1
            inner = v[1:-1]  # strip quotes
            result = self._interpolate(inner)
            return result, pos

        # Single-quoted string
        if k == "SQSTR":
            pos += 1
            inner = v[1:-1]
            inner = inner.replace("\\'", "'").replace("\\\\", "\\")
            return inner, pos

        # qw(...)
        if k == "QWLIST":
            pos += 1
            m = re.match(r"qw\s*[(\[{](.*)[)\]}]", v, re.DOTALL)
            if m:
                return m.group(1).split(), pos
            return [], pos

        # Regex literal (bare /.../)
        if k == "REGEX":
            pos += 1
            return v, pos  # returned as string; =~ handles it

        # fat-comma — treat like regular comma
        if k == "FAT_COMMA":
            return None, pos + 1

        # Identifier — could be function call, keyword, bareword string
        if k == "IDENT":
            return self._eval_ident(toks, pos)

        pos += 1
        return None, pos

    def _collect_balanced_tok(self, toks: list, pos: int, open_t: str, close_t: str) -> tuple[list, int]:
        result = []
        depth = 1
        while pos < len(toks):
            k, v = toks[pos]
            if k == open_t:
                depth += 1
            elif k == close_t:
                depth -= 1
                if depth == 0:
                    pos += 1
                    break
            result.append((k, v))
            pos += 1
        return result, pos

    def _eval_ident(self, toks: list, pos: int) -> tuple[Any, int]:
        k, name = toks[pos]
        pos += 1

        # ---- Built-in functions ----
        if name == "undef":
            return None, pos

        if name in ("scalar", "int", "abs", "sqrt", "length", "defined",
                    "chomp", "chop", "uc", "lc", "ucfirst", "lcfirst",
                    "reverse", "sort", "keys", "values", "exists", "delete",
                    "pop", "push", "shift", "unshift", "join", "split",
                    "sprintf", "chr", "ord", "hex", "oct", "rand", "srand",
                    "sin", "cos", "atan2", "exp", "log", "pos", "index",
                    "rindex", "substr", "print", "say", "warn", "die",
                    "ref", "wantarray"):
            return self._call_builtin(name, toks, pos)

        # Turtle commands
        if name in ("forward", "back", "left", "right", "penup", "pendown",
                    "pencolor", "setpos", "setheading", "circle",
                    "fd", "bk", "lt", "rt", "pu", "pd"):
            return self._call_turtle(name, toks, pos)

        # User-defined sub call
        if name in self._subs:
            return self._call_user_sub(name, toks, pos)

        # Keyword used as bareword string (e.g. hash keys)
        # Or actual bareword string
        if pos < len(toks) and toks[pos][0] in ("LPAREN",):
            # Unknown function call — try anyway
            pos += 1
            args = []
            while pos < len(toks) and toks[pos][0] != "RPAREN":
                if toks[pos][0] == "COMMA":
                    pos += 1
                    continue
                v, pos = self._eval_assign(toks, pos)
                args.append(v)
            if pos < len(toks):
                pos += 1
            return None, pos

        # Bareword string
        return name, pos

    def _collect_args(self, toks: list, pos: int) -> tuple[list, int]:
        """Collect function arguments (optional parens or bare list)."""
        args = []
        has_parens = pos < len(toks) and toks[pos][0] == "LPAREN"
        if has_parens:
            pos += 1
            while pos < len(toks) and toks[pos][0] != "RPAREN":
                if toks[pos][0] in ("COMMA", "FAT_COMMA"):
                    pos += 1
                    continue
                v, pos = self._eval_assign(toks, pos)
                if isinstance(v, list):
                    args.extend(v)
                else:
                    args.append(v)
            if pos < len(toks):
                pos += 1  # consume RPAREN
        else:
            # Bare list until semicolon
            while pos < len(toks) and toks[pos][0] not in ("SEMI", "RBRACE"):
                if toks[pos][0] in ("COMMA", "FAT_COMMA"):
                    pos += 1
                    continue
                v, pos = self._eval_assign(toks, pos)
                if isinstance(v, list):
                    args.extend(v)
                else:
                    args.append(v)
        return args, pos

    def _call_builtin(self, name: str, toks: list, pos: int) -> tuple[Any, int]:
        args, pos = self._collect_args(toks, pos)

        def _a(i: int, default: Any = None) -> Any:
            return args[i] if i < len(args) else default

        if name == "scalar":
            a = _a(0)
            if isinstance(a, list):
                return len(a), pos
            return 1 if a is not None else 0, pos

        if name == "int":
            return int(_to_num(_a(0))), pos

        if name == "abs":
            return abs(_to_num(_a(0))), pos

        if name == "sqrt":
            v = _to_num(_a(0))
            return math.sqrt(v) if v >= 0 else 0.0, pos

        if name in ("sin", "cos"):
            v = _to_num(_a(0))
            return (math.sin(v) if name == "sin" else math.cos(v)), pos

        if name == "atan2":
            return math.atan2(_to_num(_a(0)), _to_num(_a(1, 1))), pos

        if name == "exp":
            return math.exp(_to_num(_a(0))), pos

        if name == "log":
            v = _to_num(_a(0))
            return math.log(v) if v > 0 else 0.0, pos

        if name == "rand":
            upper = _to_num(_a(0)) if args else 1.0
            return random.uniform(0, upper), pos

        if name == "srand":
            random.seed(int(_to_num(_a(0))) if args else None)
            return None, pos

        if name == "length":
            return len(_to_str(_a(0, self._get_scalar("_")))), pos

        if name == "defined":
            return 1 if _a(0) is not None else "", pos

        if name == "ref":
            return "", pos  # simplified

        if name == "wantarray":
            return 1 if self._call_stack else "", pos

        if name in ("uc", "lc", "ucfirst", "lcfirst"):
            s = _to_str(_a(0, self._get_scalar("_")))
            if name == "uc":
                return s.upper(), pos
            if name == "lc":
                return s.lower(), pos
            if name == "ucfirst":
                return s[0].upper() + s[1:] if s else s, pos
            return s[0].lower() + s[1:] if s else s, pos

        if name == "reverse":
            if isinstance(_a(0), list) or len(args) > 1:
                return list(reversed(args)), pos
            return _to_str(_a(0))[::-1], pos

        if name == "sort":
            items = args
            if items and callable(items[0]):
                items = items[1:]
            return sorted(items, key=_to_str), pos

        if name == "chomp":
            v = _to_str(_a(0, self._get_scalar("_"))).rstrip("\n")
            self._set_scalar("_", v)
            return v, pos

        if name == "chop":
            v = _to_str(_a(0, self._get_scalar("_")))
            v = v[:-1] if v else v
            self._set_scalar("_", v)
            return v, pos

        if name in ("keys", "values"):
            h = _a(0)
            if isinstance(h, list):
                # flat key-value pairs
                it = iter(h)
                pairs = list(zip(it, it))
                if name == "keys":
                    return [k for k, _ in pairs], pos
                return [vv for _, vv in pairs], pos
            if isinstance(h, dict):
                return list(h.keys()) if name == "keys" else list(h.values()), pos
            return [], pos

        if name == "exists":
            # args come in as evaluated — hard to re-check; return 1 for now
            return 1, pos

        if name == "delete":
            return None, pos

        if name == "chr":
            return chr(int(_to_num(_a(0)))), pos

        if name == "ord":
            s = _to_str(_a(0))
            return ord(s[0]) if s else 0, pos

        if name == "hex":
            try:
                return int(_to_str(_a(0)), 16), pos
            except ValueError:
                return 0, pos

        if name == "oct":
            try:
                s = _to_str(_a(0))
                if s.startswith("0x"):
                    return int(s, 16), pos
                if s.startswith("0b"):
                    return int(s, 2), pos
                return int(s, 8), pos
            except ValueError:
                return 0, pos

        if name == "index":
            s, sub = _to_str(_a(0)), _to_str(_a(1))
            start = int(_to_num(_a(2))) if len(args) > 2 else 0
            return s.find(sub, start), pos

        if name == "rindex":
            s, sub = _to_str(_a(0)), _to_str(_a(1))
            end = int(_to_num(_a(2))) if len(args) > 2 else -1
            if end >= 0:
                return s.rfind(sub, 0, end + 1), pos
            return s.rfind(sub), pos

        if name == "substr":
            s = _to_str(_a(0))
            start = int(_to_num(_a(1, 0)))
            if len(args) > 2:
                length = int(_to_num(_a(2)))
                return s[start:start + length], pos
            return s[start:], pos

        if name == "join":
            sep = _to_str(_a(0))
            items = args[1:]
            if len(items) == 1 and isinstance(items[0], list):
                items = items[0]
            return sep.join(_to_str(x) for x in items), pos

        if name == "split":
            pat = _to_str(_a(0, " "))
            string = _to_str(_a(1, self._get_scalar("_")))
            limit = int(_to_num(_a(2))) if len(args) > 2 else 0
            if pat == " ":
                return string.split(), pos
            try:
                parts = re.split(pat, string, maxsplit=max(0, limit - 1))
            except re.error:
                parts = string.split(pat)
            return parts, pos

        if name == "sprintf":
            fmt = _to_str(_a(0))
            fmt_args = args[1:]
            try:
                # Convert %s %d %f style
                result = self._do_sprintf(fmt, fmt_args)
            except Exception:
                result = fmt
            return result, pos

        if name in ("print", "say", "warn"):
            text = "".join(_to_str(a) for a in args)
            if name == "say":
                text = text.rstrip("\n") + "\n"
            if name == "warn":
                self._emit(f"ℹ️ {text.rstrip()}")
            else:
                for line in text.split("\n"):
                    self._emit(line)
                if self._output and self._output[-1] == "":
                    self._output.pop()
            return 1, pos

        if name == "die":
            msg = "".join(_to_str(a) for a in args) if args else "Died"
            raise _PerlDie(msg.rstrip("\n"))

        if name == "pos":
            return None, pos

        return None, pos

    def _call_turtle(self, name: str, toks: list, pos: int) -> tuple[Any, int]:
        args, pos = self._collect_args(toks, pos)

        def _n(i: int, default: float = 0) -> float:
            return _to_num(args[i]) if i < len(args) else default

        t = self.turtle
        aliases = {"fd": "forward", "bk": "back", "lt": "left", "rt": "right",
                   "pu": "penup", "pd": "pendown"}
        name = aliases.get(name, name)

        if name == "forward":
            t.forward(_n(0))
        elif name == "back":
            t.backward(_n(0))
        elif name == "left":
            t.left(_n(0))
        elif name == "right":
            t.right(_n(0))
        elif name == "penup":
            t.pen_up()
        elif name == "pendown":
            t.pen_down()
        elif name == "pencolor":
            t.set_color(_to_str(args[0]) if args else "black")
        elif name == "setpos":
            t.goto(_n(0), _n(1))
        elif name == "setheading":
            t.set_heading(_n(0))
        elif name == "circle":
            t.circle(_n(0))
        return None, pos

    def _call_user_sub(self, name: str, toks: list, pos: int) -> tuple[Any, int]:
        self._depth += 1
        if self._depth > self.MAX_DEPTH:
            self._depth -= 1
            raise _PerlDie("deep recursion")
        args, pos = self._collect_args(toks, pos)
        sub = self._subs[name]
        frame: dict[str, Any] = {"_locals": {}, "_local_arrays": {}, "_local_hashes": {}}
        # Set @_ to args
        frame["_local_arrays"]["_"] = list(args)
        self._call_stack.append(frame)
        result = None
        try:
            self._exec_block(sub.body)
        except _PerlReturn as r:
            result = r.value
        finally:
            self._call_stack.pop()
            self._depth -= 1
        return result, pos

    # ------------------------------------------------------------------
    # Operator application
    # ------------------------------------------------------------------

    def _apply_op(self, op: str, lhs: Any, rhs: Any) -> Any:
        if op == ".":
            return _to_str(lhs) + _to_str(rhs)
        if op == "x":
            return _to_str(lhs) * max(0, int(_to_num(rhs)))
        if op in ("+", "-", "*", "/", "%", "**"):
            a, b = _to_num(lhs), _to_num(rhs)
            if op == "+":
                return a + b
            if op == "-":
                return a - b
            if op == "*":
                return a * b
            if op == "/":
                return a / b if b != 0 else 0
            if op == "%":
                return int(a) % int(b) if b != 0 else 0
            if op == "**":
                return a ** b
        return lhs

    # ------------------------------------------------------------------
    # String interpolation
    # ------------------------------------------------------------------

    def _interpolate(self, s: str) -> str:
        """Expand $var and @arr in double-quoted strings."""
        # Handle escape sequences first
        s = s.replace("\\n", "\n").replace("\\t", "\t").replace("\\r", "\r")
        s = s.replace("\\\\", "\x00BSLASH\x00").replace('\\"', '"')

        # @array interpolation
        def repl_arr(m: re.Match) -> str:
            name = m.group(1)
            return " ".join(_to_str(x) for x in self._get_array(name))

        s = re.sub(r"@([A-Za-z_]\w*)", repl_arr, s)

        # $hash{key} and $arr[idx]
        def repl_scalar_sub(m: re.Match) -> str:
            name, subscript = m.group(1), m.group(2)
            if subscript.startswith("["):
                idx = int(_to_num(subscript[1:-1]))
                a = self._get_array(name)
                return _to_str(a[idx] if idx < len(a) else None)
            else:
                key = subscript[1:-1]
                return _to_str(self._get_hash(name).get(key))

        s = re.sub(r"\$([A-Za-z_]\w*)(\[\d+\]|\{[^}]+\})", repl_scalar_sub, s)

        # $scalar
        def repl_scalar(m: re.Match) -> str:
            return _to_str(self._get_scalar(m.group(1)))

        s = re.sub(r"\$([A-Za-z_]\w*)", repl_scalar, s)
        s = s.replace("\x00BSLASH\x00", "\\")
        return s

    # ------------------------------------------------------------------
    # sprintf helper
    # ------------------------------------------------------------------

    def _do_sprintf(self, fmt: str, args: list) -> str:
        """Simple sprintf implementation."""
        result = []
        arg_idx = 0
        i = 0
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
            # Parse optional flags/width/precision
            spec_start = i - 1
            while i < len(fmt) and fmt[i] in "-+ 0#":
                i += 1
            while i < len(fmt) and fmt[i].isdigit():
                i += 1
            if i < len(fmt) and fmt[i] == ".":
                i += 1
                while i < len(fmt) and fmt[i].isdigit():
                    i += 1
            if i >= len(fmt):
                break
            conv = fmt[i]
            i += 1
            spec = fmt[spec_start:i]
            arg = args[arg_idx] if arg_idx < len(args) else 0
            arg_idx += 1
            try:
                if conv in ("d", "i", "u", "o", "x", "X"):
                    result.append(spec % int(_to_num(arg)))
                elif conv in ("f", "e", "E", "g", "G"):
                    result.append(spec % float(_to_num(arg)))
                elif conv == "s":
                    result.append(spec % _to_str(arg))
                elif conv == "c":
                    result.append(chr(int(_to_num(arg))))
                else:
                    result.append(spec)
            except Exception:
                result.append(_to_str(arg))
        return "".join(result)


# ---------------------------------------------------------------------------
# PerlSub
# ---------------------------------------------------------------------------

class PerlSub:
    __slots__ = ("name", "body")

    def __init__(self, name: str, body: list):
        self.name = name
        self.body = body
