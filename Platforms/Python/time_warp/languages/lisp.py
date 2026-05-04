"""LISP/Scheme language executor for Time Warp Studio.

Educational Scheme interpreter — whole-program execution.
Implements a teaching subset of R5RS Scheme / Common Lisp hybrid.

Supported features:
  - All standard data types: numbers, strings, booleans, symbols, pairs/lists
  - define, lambda, let, let*, letrec, named let
  - if, cond, case, when, unless, and, or, not
  - begin, do, quasiquote (` , ,@)
  - Tail-call optimisation (trampolined)
  - First-class functions, closures, higher-order functions
  - Variadic functions (&rest / . rest)
  - Macros: define-syntax / syntax-rules (simple pattern matching)
  - Standard library: arithmetic, list ops, string ops, char ops, I/O
  - Turtle graphics: (forward n), (right n), (left n), (penup), (pendown),
    (color r g b), (setpos x y), (home)
  - display / newline / write / print / format
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_lisp(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a complete Scheme/LISP program and return all output."""
    env = SchemeEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Tokeniser & Reader
# ---------------------------------------------------------------------------

_TOKEN_RE = re.compile(
    r"""
    \s*                            # skip whitespace only (commas are NOT whitespace in Scheme)
    (
      ;[^\n]*                    | # line comment
      \#\|[\s\S]*?\|\#           | # block comment
      "(?:[^"\\]|\\.)*"          | # string literal
      (?:\#[tfTF])               | # booleans #t #f
      \#\\(?:space|newline|tab|[^ ])  # character literals
      | (?:\#[bodxe][-+]?[0-9a-fA-F.]+)  # number with radix
      | [-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[eE][-+]?\d+)?  # numbers
      | [()'\`]                  | # parens, quote, quasiquote
      ,@                         | # unquote-splicing
      ,                          | # unquote
      \.\.\.                     | # ellipsis
      \.                         | # dot
      [^\s()"`;,]+               # symbol / identifier
    )
    """,
    re.VERBOSE,
)


def _tokenise(source: str) -> list[str]:
    tokens = []
    for m in _TOKEN_RE.finditer(source):
        tok = m.group(1)
        if tok.startswith(";") or tok.startswith("#|"):
            continue  # skip comments
        tokens.append(tok)
    return tokens


class _Reader:
    def __init__(self, tokens: list[str]):
        self._tokens = tokens
        self._pos = 0

    def peek(self) -> str | None:
        return self._tokens[self._pos] if self._pos < len(self._tokens) else None

    def next(self) -> str:
        tok = self._tokens[self._pos]
        self._pos += 1
        return tok

    def read(self) -> Any:
        tok = self.next()
        # Compound forms
        if tok == "(":
            items: list[Any] = []
            while self.peek() != ")":
                if self.peek() is None:
                    raise SchemeError("unexpected end of input (unmatched '(')")
                if self.peek() == ".":
                    self.next()  # consume dot
                    cdr = self.read()
                    if self.peek() != ")":
                        raise SchemeError("malformed dotted pair")
                    self.next()  # consume ")"
                    # build dotted list from items + cdr
                    result: Any = cdr
                    for x in reversed(items):
                        result = SchemeCell(x, result)
                    return result
                items.append(self.read())
            self.next()  # consume ")"
            return _list_to_cells(items)
        if tok == ")":
            raise SchemeError("unexpected ')'")
        # Quote shortcuts
        if tok == "'":
            return _list_to_cells([_SYM("quote"), self.read()])
        if tok == "`":
            return _list_to_cells([_SYM("quasiquote"), self.read()])
        if tok == ",@":
            return _list_to_cells([_SYM("unquote-splicing"), self.read()])
        if tok == ",":
            return _list_to_cells([_SYM("unquote"), self.read()])
        return _parse_atom(tok)


def _parse_atom(tok: str) -> Any:
    # Booleans
    if tok in ("#t", "#T", "#true"):
        return True
    if tok in ("#f", "#F", "#false"):
        return False
    # Character
    if tok.startswith("#\\"):
        rest = tok[2:]
        if rest == "space":
            return SchemeChar(" ")
        if rest == "newline":
            return SchemeChar("\n")
        if rest == "tab":
            return SchemeChar("\t")
        return SchemeChar(rest[0] if rest else " ")
    # Numbers
    try:
        if tok.startswith("#b"):
            return int(tok[2:], 2)
        if tok.startswith("#o"):
            return int(tok[2:], 8)
        if tok.startswith("#d"):
            return float(tok[2:]) if "." in tok else int(tok[2:])
        if tok.startswith("#x"):
            return int(tok[2:], 16)
        if "." in tok or "e" in tok.lower() or "/" in tok:
            if "/" in tok:
                a, b = tok.split("/")
                return int(a) / int(b)
            return float(tok)
        return int(tok)
    except (ValueError, ZeroDivisionError):
        pass
    # String
    if tok.startswith('"'):
        s = tok[1:-1]
        s = s.replace("\\n", "\n").replace("\\t", "\t").replace('\\"', '"')
        s = s.replace("\\\\", "\\")
        return s
    # Symbol
    return _SYM(tok)


def _read_all(source: str) -> list[Any]:
    """Parse all top-level expressions from source."""
    tokens = _tokenise(source)
    reader = _Reader(tokens)
    exprs: list[Any] = []
    while reader.peek() is not None:
        exprs.append(reader.read())
    return exprs


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------

class SchemeError(Exception):
    pass


class _Symbol(str):
    """Interned symbol type — identity is equality."""
    _pool: dict[str, "_Symbol"] = {}

    def __new__(cls, name: str) -> "_Symbol":
        if name not in cls._pool:
            obj = super().__new__(cls, name)
            cls._pool[name] = obj
        return cls._pool[name]

    def __repr__(self):
        return str(self)


def _SYM(name: str) -> _Symbol:
    return _Symbol(name)


class SchemeCell:
    """Lisp cons cell."""
    __slots__ = ("car", "cdr")

    def __init__(self, car: Any, cdr: Any):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        return f"({_display_list(self)})"

    def __iter__(self):
        node = self
        while isinstance(node, SchemeCell):
            yield node.car
            node = node.cdr
        if node is not _NIL:
            yield _SYM(".")
            yield node


class SchemeChar:
    """Character type."""
    def __init__(self, ch: str):
        self.ch = ch

    def __repr__(self):
        if self.ch == " ":
            return "#\\space"
        if self.ch == "\n":
            return "#\\newline"
        if self.ch == "\t":
            return "#\\tab"
        return f"#\\{self.ch}"

    def __str__(self):
        return self.ch

    def __eq__(self, other):
        return isinstance(other, SchemeChar) and self.ch == other.ch

    def __hash__(self):
        return hash(self.ch)


class SchemeVector:
    """Mutable vector."""
    def __init__(self, items: list):
        self.items = list(items)

    def __repr__(self):
        items_str = " ".join(_scheme_repr(x) for x in self.items)
        return f"#({items_str})"


class SchemeProcedure:
    """User-defined lambda."""
    __slots__ = ("params", "rest_param", "body", "env", "name")

    def __init__(self, params, rest_param, body, env, name="lambda"):
        self.params = params
        self.rest_param = rest_param
        self.body = body
        self.env = env
        self.name = name

    def __repr__(self):
        return f"#<procedure {self.name}>"


class SchemeMacro:
    """syntax-rules macro."""
    __slots__ = ("keywords", "rules", "def_env")

    def __init__(self, keywords, rules, def_env):
        self.keywords = keywords
        self.rules = rules
        self.def_env = def_env

    def __repr__(self):
        return "#<macro>"


class _Nil:
    """The empty list ()."""
    _inst = None

    def __new__(cls):
        if cls._inst is None:
            cls._inst = super().__new__(cls)
        return cls._inst

    def __repr__(self):
        return "()"

    def __iter__(self):
        return iter([])

    def __bool__(self):
        return False


_NIL = _Nil()

# Tail-call thunk
class _Thunk:
    __slots__ = ("expr", "env")

    def __init__(self, expr, env):
        self.expr = expr
        self.env = env


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _list_to_cells(items: list) -> Any:
    result: Any = _NIL
    for x in reversed(items):
        result = SchemeCell(x, result)
    return result


def _cells_to_list(cell: Any) -> list:
    result = []
    node = cell
    while isinstance(node, SchemeCell):
        result.append(node.car)
        node = node.cdr
    if node is not _NIL:
        raise SchemeError(f"improper list: {_scheme_repr(cell)}")
    return result


def _scheme_repr(x: Any) -> str:
    if x is _NIL:
        return "()"
    if isinstance(x, bool):
        return "#t" if x else "#f"
    if isinstance(x, _Symbol):
        return str(x)
    if isinstance(x, str):
        s = x.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\t", "\\t")
        return f'"{s}"'
    if isinstance(x, SchemeChar):
        return repr(x)
    if isinstance(x, SchemeCell):
        return f"({_display_list(x)})"
    if isinstance(x, SchemeVector):
        return repr(x)
    if isinstance(x, float):
        if x == int(x) and not math.isinf(x):
            return f"{x:.1f}"
        return repr(x)
    return repr(x)


def _scheme_display(x: Any) -> str:
    """Like display — no quotes on strings/chars."""
    if x is _NIL:
        return "()"
    if isinstance(x, bool):
        return "#t" if x else "#f"
    if isinstance(x, _Symbol):
        return str(x)
    if isinstance(x, str):
        return x
    if isinstance(x, SchemeChar):
        return x.ch
    if isinstance(x, SchemeCell):
        return f"({_display_list(x, display=True)})"
    return _scheme_repr(x)


def _display_list(cell: SchemeCell, display: bool = False) -> str:
    parts = []
    node: Any = cell
    while isinstance(node, SchemeCell):
        fn = _scheme_display if display else _scheme_repr
        parts.append(fn(node.car))
        node = node.cdr
    if node is _NIL:
        return " ".join(parts)
    fn = _scheme_display if display else _scheme_repr
    return " ".join(parts) + " . " + fn(node)


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------

class SchemeEnv:
    """Lexical environment frame."""
    __slots__ = ("_bindings", "_parent")

    def __init__(self, parent: "SchemeEnv | None" = None):
        self._bindings: dict[str, Any] = {}
        self._parent = parent

    def lookup(self, name: str) -> Any:
        env = self
        while env is not None:
            if name in env._bindings:
                return env._bindings[name]
            env = env._parent
        raise SchemeError(f"unbound variable: {name}")

    def define(self, name: str, val: Any) -> None:
        self._bindings[name] = val

    def set(self, name: str, val: Any) -> None:
        env = self
        while env is not None:
            if name in env._bindings:
                env._bindings[name] = val
                return
            env = env._parent
        raise SchemeError(f"set!: unbound variable: {name}")

    def extend(self, params: list[str], rest: str | None, args: list) -> "SchemeEnv":
        child = SchemeEnv(self)
        if len(args) < len(params):
            raise SchemeError(
                f"arity mismatch: expected {len(params)} args, got {len(args)}"
            )
        for p, a in zip(params, args):
            child.define(p, a)
        if rest is not None:
            child.define(rest, _list_to_cells(list(args[len(params):])))
        elif len(args) != len(params):
            raise SchemeError(
                f"arity mismatch: expected {len(params)} args, got {len(args)}"
            )
        return child


# ---------------------------------------------------------------------------
# Evaluator
# ---------------------------------------------------------------------------

class SchemeInterpreter:
    def __init__(self, output_fn, turtle=None):
        self._output = output_fn
        self._turtle = turtle
        self._global_env = SchemeEnv()
        self._call_depth = 0
        self._max_depth = 2000
        self._setup_builtins()

    # ── Trampoline ────────────────────────────────────────────────────────

    def eval(self, expr: Any, env: SchemeEnv) -> Any:
        """Evaluate expr in env with trampolining for tail calls."""
        while True:
            if isinstance(expr, _Thunk):
                env = expr.env
                expr = expr.expr
                continue
            result = self._eval(expr, env)
            if isinstance(result, _Thunk):
                expr = result.expr
                env = result.env
            else:
                return result

    def _eval(self, expr: Any, env: SchemeEnv) -> Any:
        # Symbol lookup must come BEFORE self-eval check because _Symbol is a str subclass
        if isinstance(expr, _Symbol):
            return env.lookup(str(expr))

        # Self-evaluating
        if expr is _NIL or isinstance(expr, (bool, int, float, str, SchemeChar, SchemeVector)):
            return expr
        if expr is None:
            return _NIL

        # Pair — special form or application
        if not isinstance(expr, SchemeCell):
            raise SchemeError(f"cannot evaluate: {_scheme_repr(expr)}")

        head = expr.car
        args_cell = expr.cdr

        # ── Special forms ────────────────────────────────────────────────
        if isinstance(head, _Symbol):
            name = str(head)

            if name == "quote":
                return args_cell.car if isinstance(args_cell, SchemeCell) else _NIL

            if name == "quasiquote":
                return self._expand_quasiquote(args_cell.car, env, depth=0)

            if name == "if":
                args = _cells_to_list(args_cell)
                if len(args) < 2:
                    raise SchemeError("if: needs at least 2 subforms")
                test = self.eval(args[0], env)
                if test is not False:
                    return _Thunk(args[1], env)
                if len(args) >= 3:
                    return _Thunk(args[2], env)
                return _NIL  # no else branch

            if name == "cond":
                clauses = _cells_to_list(args_cell)
                for clause in clauses:
                    cl = _cells_to_list(clause)
                    if not cl:
                        continue
                    test_expr = cl[0]
                    if isinstance(test_expr, _Symbol) and str(test_expr) == "else":
                        if len(cl) == 1:
                            return _NIL
                        return _Thunk(_list_to_cells([_SYM("begin")] + cl[1:]), env)
                    test_val = self.eval(test_expr, env)
                    if test_val is not False:
                        if len(cl) == 1:
                            return test_val
                        if len(cl) == 3 and isinstance(cl[1], _Symbol) and str(cl[1]) == "=>":
                            proc = self.eval(cl[2], env)
                            return self._apply(proc, [test_val])
                        return _Thunk(_list_to_cells([_SYM("begin")] + cl[1:]), env)
                return _NIL

            if name == "case":
                args = _cells_to_list(args_cell)
                if not args:
                    raise SchemeError("case: empty form")
                key = self.eval(args[0], env)
                for clause in args[1:]:
                    cl = _cells_to_list(clause)
                    if not cl:
                        continue
                    datum = cl[0]
                    if isinstance(datum, _Symbol) and str(datum) == "else":
                        return _Thunk(_list_to_cells([_SYM("begin")] + cl[1:]), env)
                    datums = _cells_to_list(datum)
                    if any(self._eqv(key, self.eval(_list_to_cells([_SYM("quote"), d]), env))
                           for d in datums):
                        return _Thunk(_list_to_cells([_SYM("begin")] + cl[1:]), env)
                return _NIL

            if name == "and":
                vals = _cells_to_list(args_cell)
                if not vals:
                    return True
                for v in vals[:-1]:
                    r = self.eval(v, env)
                    if r is False:
                        return False
                return _Thunk(vals[-1], env)

            if name == "or":
                vals = _cells_to_list(args_cell)
                if not vals:
                    return False
                for v in vals[:-1]:
                    r = self.eval(v, env)
                    if r is not False:
                        return r
                return _Thunk(vals[-1], env)

            if name == "when":
                args = _cells_to_list(args_cell)
                if self.eval(args[0], env) is not False:
                    return _Thunk(_list_to_cells([_SYM("begin")] + args[1:]), env)
                return _NIL

            if name == "unless":
                args = _cells_to_list(args_cell)
                if self.eval(args[0], env) is False:
                    return _Thunk(_list_to_cells([_SYM("begin")] + args[1:]), env)
                return _NIL

            if name == "begin":
                stmts = _cells_to_list(args_cell)
                if not stmts:
                    return _NIL
                for s in stmts[:-1]:
                    self.eval(s, env)
                return _Thunk(stmts[-1], env)

            if name == "define":
                args = _cells_to_list(args_cell)
                if not args:
                    raise SchemeError("define: empty form")
                target = args[0]
                # Short-form (define (name params...) body...)
                if isinstance(target, SchemeCell):
                    fn_name = str(target.car)
                    params, rest = _parse_params(target.cdr)
                    body = args[1:]
                    if not body:
                        raise SchemeError(f"define: {fn_name} has no body")
                    proc = SchemeProcedure(params, rest, body, env, fn_name)
                    env.define(fn_name, proc)
                    return _SYM(fn_name)
                # Normal (define name expr)
                sym = str(target)
                val = self.eval(args[1], env) if len(args) > 1 else _NIL
                # Give lambdas the name of the variable
                if isinstance(val, SchemeProcedure) and val.name == "lambda":
                    val.name = sym
                env.define(sym, val)
                return _SYM(sym)

            if name == "set!":
                args = _cells_to_list(args_cell)
                sym = str(args[0])
                env.set(sym, self.eval(args[1], env))
                return _NIL

            if name == "lambda":
                args = _cells_to_list(args_cell)
                if not args:
                    raise SchemeError("lambda: empty form")
                params, rest = _parse_params(args[0])
                body = args[1:]
                if not body:
                    raise SchemeError("lambda: empty body")
                return SchemeProcedure(params, rest, body, env)

            if name == "let":
                args = _cells_to_list(args_cell)
                # Named let: (let name ((var init) ...) body)
                if isinstance(args[0], _Symbol):
                    loop_name = str(args[0])
                    bindings = _cells_to_list(args[1])
                    body = args[2:]
                    params = [str(_cells_to_list(b)[0]) for b in bindings]
                    inits = [self.eval(_cells_to_list(b)[1], env) for b in bindings]
                    child = SchemeEnv(env)
                    proc = SchemeProcedure(params, None, body, child, loop_name)
                    child.define(loop_name, proc)
                    for p, v in zip(params, inits):
                        child.define(p, v)
                    return _Thunk(_list_to_cells([_SYM("begin")] + body), child)
                # Regular let
                bindings = _cells_to_list(args[0])
                body = args[1:]
                child = SchemeEnv(env)
                for b in bindings:
                    bl = _cells_to_list(b)
                    child.define(str(bl[0]), self.eval(bl[1], env))
                return _Thunk(_list_to_cells([_SYM("begin")] + body), child)

            if name == "let*":
                args = _cells_to_list(args_cell)
                bindings = _cells_to_list(args[0])
                body = args[1:]
                child = SchemeEnv(env)
                for b in bindings:
                    bl = _cells_to_list(b)
                    child.define(str(bl[0]), self.eval(bl[1], child))
                return _Thunk(_list_to_cells([_SYM("begin")] + body), child)

            if name == "letrec" or name == "letrec*":
                args = _cells_to_list(args_cell)
                bindings = _cells_to_list(args[0])
                body = args[1:]
                child = SchemeEnv(env)
                for b in bindings:
                    child.define(str(_cells_to_list(b)[0]), _NIL)
                for b in bindings:
                    bl = _cells_to_list(b)
                    child.set(str(bl[0]), self.eval(bl[1], child))
                return _Thunk(_list_to_cells([_SYM("begin")] + body), child)

            if name == "do":
                args = _cells_to_list(args_cell)
                var_specs = _cells_to_list(args[0])
                test_clause = _cells_to_list(args[1])
                commands = args[2:]
                child = SchemeEnv(env)
                vars_steps: list[tuple[str, Any, Any]] = []
                for vs in var_specs:
                    vsl = _cells_to_list(vs)
                    vname = str(vsl[0])
                    init = self.eval(vsl[1], env)
                    step = vsl[2] if len(vsl) > 2 else _SYM(vname)
                    child.define(vname, init)
                    vars_steps.append((vname, vname, step))
                while True:
                    test_val = self.eval(test_clause[0], child)
                    if test_val is not False:
                        if len(test_clause) == 1:
                            return _NIL
                        return _Thunk(_list_to_cells([_SYM("begin")] + test_clause[1:]), child)
                    for cmd in commands:
                        self.eval(cmd, child)
                    new_vals = [self.eval(step, child) for _, _, step in vars_steps]
                    for (vname, _, _), nv in zip(vars_steps, new_vals):
                        child.set(vname, nv)

            if name == "define-syntax":
                args = _cells_to_list(args_cell)
                macro_name = str(args[0])
                transformer = self.eval(args[1], env)
                env.define(macro_name, transformer)
                return _SYM(macro_name)

            if name == "syntax-rules":
                args = _cells_to_list(args_cell)
                keywords = [str(k) for k in _cells_to_list(args[0])]
                rules = [_cells_to_list(r) for r in args[1:]]
                return SchemeMacro(keywords, rules, env)

            if name == "define-record-type":
                return self._define_record(args_cell, env)

            if name == "values":
                vals = [self.eval(v, env) for v in _cells_to_list(args_cell)]
                return _list_to_cells(vals)

            if name == "call-with-values":
                args = _cells_to_list(args_cell)
                producer = self.eval(args[0], env)
                consumer = self.eval(args[1], env)
                produced = self._apply(producer, [])
                vals = _cells_to_list(produced) if isinstance(produced, SchemeCell) else [produced]
                return self._apply(consumer, vals)

        # ── Macro expansion ──────────────────────────────────────────────
        op = self.eval(head, env)
        if isinstance(op, SchemeMacro):
            expanded = self._expand_macro(op, expr)
            return _Thunk(expanded, env)

        # ── Function application ─────────────────────────────────────────
        args_list = [self.eval(a, env) for a in _cells_to_list(args_cell)]
        return self._apply_tco(op, args_list, env)

    def _apply_tco(self, proc, args: list, env: SchemeEnv) -> Any:
        """Apply proc to args, returning a _Thunk for tail-call user procs."""
        if isinstance(proc, SchemeProcedure):
            child = proc.env.extend(proc.params, proc.rest_param, args)
            if len(proc.body) == 1:
                return _Thunk(proc.body[0], child)
            for s in proc.body[:-1]:
                self.eval(s, child)
            return _Thunk(proc.body[-1], child)
        return self._apply(proc, args)

    def _apply(self, proc: Any, args: list) -> Any:
        """Apply proc to args (no TCO — use for non-tail positions)."""
        if isinstance(proc, SchemeProcedure):
            child = proc.env.extend(proc.params, proc.rest_param, args)
            result = _NIL
            for s in proc.body[:-1]:
                result = self.eval(s, child)
            return self.eval(proc.body[-1], child)
        if callable(proc):
            return proc(*args)
        raise SchemeError(f"not a procedure: {_scheme_repr(proc)}")

    # ── Quasiquote ────────────────────────────────────────────────────────

    def _expand_quasiquote(self, tmpl: Any, env: SchemeEnv, depth: int) -> Any:
        if not isinstance(tmpl, SchemeCell):
            return tmpl
        head = tmpl.car
        if isinstance(head, _Symbol):
            if str(head) == "unquote":
                if depth == 0:
                    return self.eval(tmpl.cdr.car, env)
                return _list_to_cells([_SYM("unquote"),
                                       self._expand_quasiquote(tmpl.cdr.car, env, depth - 1)])
            if str(head) == "quasiquote":
                return _list_to_cells([_SYM("quasiquote"),
                                       self._expand_quasiquote(tmpl.cdr.car, env, depth + 1)])
        # Check for unquote-splicing in head position
        if isinstance(head, SchemeCell) and isinstance(head.car, _Symbol):
            if str(head.car) == "unquote-splicing":
                if depth == 0:
                    spliced = self.eval(head.cdr.car, env)
                    tail = self._expand_quasiquote(tmpl.cdr, env, depth)
                    # Append spliced items before tail
                    if spliced is _NIL:
                        return tail
                    items = list(spliced)
                    items_dot = [x for x in items if x is not _SYM(".")]
                    result: Any = tail
                    for x in reversed(items_dot):
                        result = SchemeCell(x, result)
                    return result
        new_car = self._expand_quasiquote(head, env, depth)
        new_cdr = self._expand_quasiquote(tmpl.cdr, env, depth)
        return SchemeCell(new_car, new_cdr)

    # ── Macro expansion ───────────────────────────────────────────────────

    def _expand_macro(self, macro: SchemeMacro, form: Any) -> Any:
        form_list = list(form)  # iterate pairs
        form_items = []
        node = form
        while isinstance(node, SchemeCell):
            form_items.append(node.car)
            node = node.cdr

        for pattern_cell, template in macro.rules:
            pat_items = list(pattern_cell)
            bindings: dict[str, Any] = {}
            if self._match_pattern(pat_items, form_items, macro.keywords, bindings):
                return self._instantiate_template(template, bindings)
        raise SchemeError(f"no matching syntax-rules pattern for {_scheme_repr(form)}")

    def _match_pattern(self, pat, form, keywords, bindings) -> bool:
        if not isinstance(pat, list):
            pat = list(pat) if isinstance(pat, SchemeCell) else [pat]
        if not isinstance(form, list):
            form = list(form) if isinstance(form, SchemeCell) else [form]
        # Skip the macro name (first element of pattern)
        pat = pat[1:]
        form = form[1:]
        return self._match_seq(pat, form, keywords, bindings)

    def _match_seq(self, pat, form, keywords, bindings) -> bool:
        pi = 0
        fi = 0
        while pi < len(pat):
            p = pat[pi]
            # ellipsis pattern
            if pi + 1 < len(pat) and isinstance(pat[pi + 1], _Symbol) and str(pat[pi + 1]) == "...":
                sym = str(p) if isinstance(p, _Symbol) else None
                matched = []
                while fi < len(form):
                    sub: dict[str, Any] = {}
                    if sym and not (sym in keywords):
                        matched.append(form[fi])
                        fi += 1
                    else:
                        break
                if sym:
                    bindings[sym] = _list_to_cells(matched)
                pi += 2
                continue
            if fi >= len(form):
                return False
            f = form[fi]
            if isinstance(p, _Symbol):
                s = str(p)
                if s == "_":
                    pass  # wildcard
                elif s in keywords:
                    if not (isinstance(f, _Symbol) and str(f) == s):
                        return False
                else:
                    bindings[s] = f
            elif isinstance(p, SchemeCell):
                if not isinstance(f, SchemeCell):
                    return False
                sub: dict[str, Any] = {}
                pl = list(p)
                fl = list(f)
                if not self._match_seq(pl, fl, keywords, sub):
                    return False
                bindings.update(sub)
            else:
                if p != f:
                    return False
            pi += 1
            fi += 1
        return fi == len(form)

    def _instantiate_template(self, tmpl: Any, bindings: dict) -> Any:
        if isinstance(tmpl, _Symbol):
            s = str(tmpl)
            return bindings.get(s, tmpl)
        if isinstance(tmpl, SchemeCell):
            # Check for ellipsis
            items: list[Any] = []
            node = tmpl
            while isinstance(node, SchemeCell):
                item = node.car
                rest = node.cdr
                if isinstance(rest, SchemeCell) and isinstance(rest.car, _Symbol) and str(rest.car) == "...":
                    # Expand the repeated pattern
                    if isinstance(item, _Symbol) and str(item) in bindings:
                        expanded = bindings[str(item)]
                        for x in (expanded if expanded is not _NIL else []):
                            items.append(x)
                    node = rest.cdr
                    continue
                items.append(self._instantiate_template(item, bindings))
                node = rest
            # Build result
            result: Any = self._instantiate_template(node, bindings) if node is not _NIL else _NIL
            for x in reversed(items):
                result = SchemeCell(x, result)
            return result
        return tmpl

    # ── Record types ──────────────────────────────────────────────────────

    def _define_record(self, args_cell: Any, env: SchemeEnv) -> Any:
        args = _cells_to_list(args_cell)
        type_name = str(args[0])
        constructor_spec = _cells_to_list(args[1])
        ctor_name = str(constructor_spec[0])
        ctor_fields = [str(x) for x in constructor_spec[1:]]
        predicate = str(args[2])
        field_specs = args[3:]

        class SchemeRecord:
            __slots__ = ["_type", "_fields"]
            _type_name = type_name

            def __init__(self, **kw):
                self._type = type_name
                self._fields = kw

            def __repr__(self):
                return f"#<{type_name}>"

        env.define(
            predicate,
            lambda x: isinstance(x, SchemeRecord) and x._type == type_name,
        )
        env.define(
            ctor_name,
            lambda *args_inner: _make_record(SchemeRecord, ctor_fields, args_inner),
        )
        for fs in field_specs:
            fsl = _cells_to_list(fs)
            fname = str(fsl[0])
            getter_name = str(fsl[1])
            env.define(getter_name, lambda r, fn=fname: r._fields[fn])
            if len(fsl) > 2:
                setter_name = str(fsl[2])
                env.define(setter_name, lambda r, v, fn=fname: r._fields.update({fn: v}) or _NIL)
        return _SYM(type_name)

    # ── eqv? ─────────────────────────────────────────────────────────────

    def _eqv(self, a: Any, b: Any) -> bool:
        if a is b:
            return True
        if type(a) != type(b):
            return False
        return a == b

    # ── Built-in procedures ───────────────────────────────────────────────

    def _setup_builtins(self):
        e = self._global_env
        T = True
        F = False

        def _num(x):
            if isinstance(x, bool):
                raise SchemeError(f"not a number: {_scheme_repr(x)}")
            if not isinstance(x, (int, float)):
                raise SchemeError(f"not a number: {_scheme_repr(x)}")
            return x

        def _arith(op_name, op, identity):
            def fn(*args):
                if not args:
                    return identity
                result = _num(args[0])
                for a in args[1:]:
                    result = op(result, _num(a))
                return result
            fn.__name__ = op_name
            return fn

        # Arithmetic
        import operator as _op
        e.define("+", _arith("+", _op.add, 0))
        e.define("*", _arith("*", _op.mul, 1))
        e.define("-", lambda *args: _num(args[0]) if len(args) == 1 else
                 _arith("-", _op.sub, 0)(*args))
        e.define("/", lambda *args: _num(args[0]) if len(args) == 1 else
                 _arith("/", lambda a, b: a // b if isinstance(a, int) and isinstance(b, int) and a % b == 0 else a / b, 1)(*args))
        e.define("quotient", lambda a, b: int(_num(a) / _num(b)))
        e.define("remainder", lambda a, b: int(_num(a)) % int(_num(b)))
        e.define("modulo", lambda a, b: int(_num(a)) % int(_num(b)))
        e.define("expt", lambda a, b: _num(a) ** _num(b))
        e.define("sqrt", lambda a: math.sqrt(_num(a)))
        e.define("abs", lambda a: abs(_num(a)))
        e.define("max", lambda *args: max(_num(a) for a in args))
        e.define("min", lambda *args: min(_num(a) for a in args))
        e.define("floor", lambda a: math.floor(_num(a)))
        e.define("ceiling", lambda a: math.ceil(_num(a)))
        e.define("truncate", lambda a: math.trunc(_num(a)))
        e.define("round", lambda a: round(_num(a)))
        e.define("gcd", lambda *args: _gcd_list([int(_num(a)) for a in args]))
        e.define("lcm", lambda *args: _lcm_list([int(_num(a)) for a in args]))
        e.define("exact->inexact", lambda a: float(_num(a)))
        e.define("inexact->exact", lambda a: int(_num(a)))
        e.define("number->string", lambda a, *rest: str(int(_num(a))) if not rest else
                 _num_to_str(int(_num(a)), int(_num(rest[0]))))
        e.define("string->number", lambda s, *rest: _str_to_num(s, int(_num(rest[0])) if rest else 10))
        e.define("zero?", lambda a: _num(a) == 0)
        e.define("positive?", lambda a: _num(a) > 0)
        e.define("negative?", lambda a: _num(a) < 0)
        e.define("odd?", lambda a: int(_num(a)) % 2 != 0)
        e.define("even?", lambda a: int(_num(a)) % 2 == 0)
        e.define("exact?", lambda a: isinstance(_num(a), int))
        e.define("inexact?", lambda a: isinstance(_num(a), float))
        e.define("number?", lambda a: isinstance(a, (int, float)) and not isinstance(a, bool))

        # Math
        e.define("sin", lambda a: math.sin(_num(a)))
        e.define("cos", lambda a: math.cos(_num(a)))
        e.define("tan", lambda a: math.tan(_num(a)))
        e.define("asin", lambda a: math.asin(_num(a)))
        e.define("acos", lambda a: math.acos(_num(a)))
        e.define("atan", lambda a, *b: math.atan2(_num(a), _num(b[0])) if b else math.atan(_num(a)))
        e.define("log", lambda a, *b: math.log(_num(a), _num(b[0])) if b else math.log(_num(a)))
        e.define("exp", lambda a: math.exp(_num(a)))
        e.define("floor/", lambda a, b: _list_to_cells([math.floor(_num(a) / _num(b)),
                                                          int(_num(a)) % int(_num(b))]))

        # Comparison
        e.define("=",  lambda *a: all(_num(a[i]) == _num(a[i+1]) for i in range(len(a)-1)))
        e.define("<",  lambda *a: all(_num(a[i]) <  _num(a[i+1]) for i in range(len(a)-1)))
        e.define(">",  lambda *a: all(_num(a[i]) >  _num(a[i+1]) for i in range(len(a)-1)))
        e.define("<=", lambda *a: all(_num(a[i]) <= _num(a[i+1]) for i in range(len(a)-1)))
        e.define(">=", lambda *a: all(_num(a[i]) >= _num(a[i+1]) for i in range(len(a)-1)))

        # Boolean
        e.define("not", lambda a: a is False)
        e.define("boolean?", lambda a: isinstance(a, bool))

        # Equality
        e.define("eq?",    lambda a, b: a is b or (isinstance(a, _Symbol) and isinstance(b, _Symbol) and str(a) == str(b)))
        e.define("eqv?",   lambda a, b: self._eqv(a, b))
        e.define("equal?", lambda a, b: _scheme_repr(a) == _scheme_repr(b))

        # Pairs / Lists
        e.define("cons", lambda a, b: SchemeCell(a, b))
        e.define("car",  lambda p: p.car if isinstance(p, SchemeCell) else (_ for _ in ()).throw(SchemeError(f"car: not a pair: {_scheme_repr(p)}")))
        e.define("cdr",  lambda p: p.cdr if isinstance(p, SchemeCell) else (_ for _ in ()).throw(SchemeError(f"cdr: not a pair: {_scheme_repr(p)}")))
        e.define("set-car!", lambda p, v: _setcar(p, v))
        e.define("set-cdr!", lambda p, v: _setcdr(p, v))
        e.define("pair?", lambda a: isinstance(a, SchemeCell))
        e.define("null?", lambda a: a is _NIL)
        e.define("list?", lambda a: _is_proper_list(a))
        e.define("list",  lambda *args: _list_to_cells(list(args)))
        e.define("length", lambda a: len(_cells_to_list(a)))
        e.define("append", lambda *args: _append(*args))
        e.define("reverse", lambda a: _list_to_cells(list(reversed(_cells_to_list(a)))))
        e.define("list-tail", lambda lst, k: _list_tail(lst, int(k)))
        e.define("list-ref",  lambda lst, k: _cells_to_list(lst)[int(k)])
        e.define("list-copy", lambda a: _list_to_cells(_cells_to_list(a)))
        e.define("assoc",  lambda k, lst: _assoc(k, lst, lambda a, b: _scheme_repr(a) == _scheme_repr(b)))
        e.define("assq",   lambda k, lst: _assoc(k, lst, lambda a, b: a is b))
        e.define("assv",   lambda k, lst: _assoc(k, lst, self._eqv))
        e.define("member", lambda k, lst: _member(k, lst, lambda a, b: _scheme_repr(a) == _scheme_repr(b)))
        e.define("memq",   lambda k, lst: _member(k, lst, lambda a, b: a is b))
        e.define("memv",   lambda k, lst: _member(k, lst, self._eqv))
        e.define("list->vector", lambda lst: SchemeVector(_cells_to_list(lst)))
        e.define("vector->list", lambda v: _list_to_cells(list(v.items)))

        # caar, cadr, cdar, cddr, caaar... (up to 4 deep)
        for combo in _cxr_combos():
            name = "c" + combo + "r"
            e.define(name, _make_cxr(combo))

        # Higher-order
        e.define("apply", lambda proc, *args: self._apply(proc, _apply_args(args)))
        e.define("map", lambda proc, *lsts: _list_to_cells(_map(self, proc, lsts)))
        e.define("for-each", lambda proc, *lsts: _for_each(self, proc, lsts))
        e.define("filter", lambda pred, lst: _list_to_cells([x for x in _cells_to_list(lst)
                                                              if self._apply(pred, [x]) is not False]))
        e.define("reduce", lambda fn, init, lst: _reduce(self, fn, init, lst))
        e.define("fold-left",  lambda fn, init, lst: _fold_left(self, fn, init, lst))
        e.define("fold-right", lambda fn, init, lst: _fold_right(self, fn, init, lst))
        e.define("for-all",  lambda pred, lst: all(self._apply(pred, [x]) is not False
                                                   for x in _cells_to_list(lst)))
        e.define("exists",   lambda pred, lst: any(self._apply(pred, [x]) is not False
                                                   for x in _cells_to_list(lst)))
        e.define("sort", lambda lst, cmp=None: _scheme_sort(self, lst, cmp))

        # Strings
        e.define("string?", lambda a: isinstance(a, str))
        e.define("string=?", lambda *a: all(a[i] == a[i+1] for i in range(len(a)-1)))
        e.define("string<?", lambda *a: all(a[i] < a[i+1] for i in range(len(a)-1)))
        e.define("string>?", lambda *a: all(a[i] > a[i+1] for i in range(len(a)-1)))
        e.define("string<=?", lambda *a: all(a[i] <= a[i+1] for i in range(len(a)-1)))
        e.define("string>=?", lambda *a: all(a[i] >= a[i+1] for i in range(len(a)-1)))
        e.define("string-ci=?", lambda a, b: a.lower() == b.lower())
        e.define("string",  lambda *chars: "".join(c.ch if isinstance(c, SchemeChar) else str(c) for c in chars))
        e.define("string-length", lambda s: len(s))
        e.define("string-ref",    lambda s, i: SchemeChar(s[int(i)]))
        e.define("substring",     lambda s, a, *b: s[int(a):int(b[0]) if b else None])
        e.define("string-append", lambda *args: "".join(args))
        e.define("string->list",  lambda s: _list_to_cells([SchemeChar(c) for c in s]))
        e.define("list->string",  lambda lst: "".join(c.ch if isinstance(c, SchemeChar) else str(c) for c in _cells_to_list(lst)))
        e.define("string-copy",   lambda s: s)
        e.define("string-upcase",   lambda s: s.upper())
        e.define("string-downcase", lambda s: s.lower())
        e.define("string-contains", lambda s, sub: s.find(sub) >= 0)
        e.define("number->string", lambda n, *r: _num_to_str(int(_num(n)), int(_num(r[0])) if r else 10))
        e.define("string->symbol", lambda s: _SYM(s))
        e.define("symbol->string", lambda s: str(s))
        e.define("string->number", _str_to_num)
        e.define("string-split",  lambda s, sep=" ": _list_to_cells(s.split(sep)))
        e.define("string-join",   lambda lst, sep=" ": sep.join(_cells_to_list(lst)))
        e.define("string-trim",   lambda s, *_: s.strip())
        e.define("format", lambda dest, fmt, *args: self._format(dest, fmt, args))
        e.define("number-format", lambda n, *_: str(n))

        # Characters
        e.define("char?", lambda a: isinstance(a, SchemeChar))
        e.define("char->integer", lambda c: ord(c.ch))
        e.define("integer->char", lambda n: SchemeChar(chr(int(n))))
        e.define("char-alphabetic?", lambda c: c.ch.isalpha())
        e.define("char-numeric?",    lambda c: c.ch.isdigit())
        e.define("char-whitespace?", lambda c: c.ch.isspace())
        e.define("char-upper-case?", lambda c: c.ch.isupper())
        e.define("char-lower-case?", lambda c: c.ch.islower())
        e.define("char-upcase",      lambda c: SchemeChar(c.ch.upper()))
        e.define("char-downcase",    lambda c: SchemeChar(c.ch.lower()))
        e.define("char=?",  lambda a, b: a.ch == b.ch)
        e.define("char<?",  lambda a, b: a.ch < b.ch)
        e.define("char>?",  lambda a, b: a.ch > b.ch)
        e.define("char<=?", lambda a, b: a.ch <= b.ch)
        e.define("char>=?", lambda a, b: a.ch >= b.ch)

        # Vectors
        e.define("vector?",    lambda a: isinstance(a, SchemeVector))
        e.define("vector",     lambda *args: SchemeVector(list(args)))
        e.define("make-vector", lambda n, *fill: SchemeVector([(fill[0] if fill else 0)] * int(n)))
        e.define("vector-ref",  lambda v, i: v.items[int(i)])
        e.define("vector-set!", lambda v, i, x: _vector_set(v, int(i), x))
        e.define("vector-length", lambda v: len(v.items))
        e.define("vector-fill!", lambda v, x: v.items.__setitem__(slice(None), [x]*len(v.items)) or _NIL)
        e.define("vector->list", lambda v: _list_to_cells(v.items))
        e.define("list->vector",  lambda lst: SchemeVector(_cells_to_list(lst)))
        e.define("vector-copy",   lambda v, *s: SchemeVector(v.items[int(s[0]) if s else 0:
                                                                       int(s[1]) if len(s) > 1 else None]))

        # I/O
        e.define("display",  lambda x, *_: self._print(_scheme_display(x), newline=False))
        e.define("write",    lambda x, *_: self._print(_scheme_repr(x), newline=False))
        e.define("newline",  lambda *_: self._print("", newline=True))
        e.define("print",    lambda *args: self._print(" ".join(_scheme_display(a) for a in args), newline=True))
        e.define("println",  lambda *args: self._print(" ".join(_scheme_display(a) for a in args), newline=True))
        e.define("write-string", lambda s, *_: self._print(s, newline=False))
        e.define("read",     lambda *_: _NIL)  # stub
        e.define("read-line", lambda *_: "")   # stub
        e.define("eof-object?", lambda _: False)

        # Type predicates
        e.define("procedure?", lambda a: callable(a) or isinstance(a, SchemeProcedure))
        e.define("symbol?",    lambda a: isinstance(a, _Symbol))
        e.define("integer?",   lambda a: isinstance(a, int) and not isinstance(a, bool))
        e.define("real?",      lambda a: isinstance(a, (int, float)) and not isinstance(a, bool))
        e.define("rational?",  lambda a: isinstance(a, (int, float)) and not isinstance(a, bool))
        e.define("complex?",   lambda a: isinstance(a, (int, float)) and not isinstance(a, bool))
        e.define("port?",      lambda a: False)

        # Control
        e.define("error", lambda msg, *args: (_ for _ in ()).throw(SchemeError(
            str(msg) + ("" if not args else ": " + " ".join(_scheme_repr(a) for a in args)))))
        e.define("call/cc", lambda proc: self._call_cc(proc))
        e.define("call-with-current-continuation", lambda proc: self._call_cc(proc))
        e.define("dynamic-wind", lambda before, thunk, after: _dynamic_wind(self, before, thunk, after))
        e.define("with-exception-handler",
                 lambda handler, thunk: self._with_exception_handler(handler, thunk))
        e.define("raise", lambda obj: (_ for _ in ()).throw(SchemeError(_scheme_display(obj))))
        e.define("raise-continuable", lambda obj: (_ for _ in ()).throw(SchemeError(_scheme_display(obj))))
        e.define("guard", _NIL)  # simplified — handled as special form if needed

        # Misc
        e.define("void",       lambda *_: _NIL)
        e.define("gensym",     lambda *p: _SYM(f"__g{id(object())}"))
        e.define("make-string",lambda n, *c: (c[0].ch if c else "\x00") * int(n))
        e.define("pi",  math.pi)
        e.define("e",   math.e)
        e.define("nan", float("nan"))
        e.define("inf", float("inf"))
        e.define("*pi*", math.pi)
        e.define("*e*",  math.e)

        # Hash tables (simplified using dict)
        e.define("make-hash-table",  lambda *_: {})
        e.define("hash-table-set!",  lambda h, k, v: h.__setitem__(_scheme_repr(k), v) or _NIL)
        e.define("hash-table-ref",   lambda h, k, *d: h.get(_scheme_repr(k), d[0] if d else _NIL))
        e.define("hash-table-delete!", lambda h, k: h.pop(_scheme_repr(k), _NIL) and _NIL)
        e.define("hash-table->alist", lambda h: _list_to_cells(
            [_list_to_cells([_parse_atom(k), v]) for k, v in h.items()]))
        e.define("hash-table-keys", lambda h: _list_to_cells([_parse_atom(k) for k in h]))
        e.define("hash-table-values", lambda h: _list_to_cells(list(h.values())))

        # Turtle graphics
        if self._turtle is not None:
            t = self._turtle
            e.define("forward",  lambda n: t.forward(float(n)))
            e.define("backward", lambda n: t.forward(-float(n)))
            e.define("fd",       lambda n: t.forward(float(n)))
            e.define("bk",       lambda n: t.forward(-float(n)))
            e.define("right",    lambda n: t.right(float(n)))
            e.define("left",     lambda n: t.left(float(n)))
            e.define("rt",       lambda n: t.right(float(n)))
            e.define("lt",       lambda n: t.left(float(n)))
            e.define("penup",    lambda *_: t.penup())
            e.define("pendown",  lambda *_: t.pendown())
            e.define("pu",       lambda *_: t.penup())
            e.define("pd",       lambda *_: t.pendown())
            e.define("home",     lambda *_: t.home())
            e.define("setpos",   lambda x, y: t.goto(float(x), float(y)))
            e.define("setx",     lambda x: t.goto(float(x), t.y))
            e.define("sety",     lambda y: t.goto(t.x, float(y)))
            e.define("setheading", lambda a: t.setheading(float(a)))
            e.define("hideturtle", lambda *_: None)
            e.define("showturtle", lambda *_: None)
            e.define("pencolor", lambda r, g, b: t.setcolor(int(r), int(g), int(b)))
            e.define("color",    lambda r, g, b: t.setcolor(int(r), int(g), int(b)))
            e.define("bgcolor",  lambda *_: None)
            e.define("clearscreen", lambda *_: t.reset())
            e.define("cs",       lambda *_: t.reset())
            e.define("circle",   lambda r, *_: _draw_circle(t, float(r)))
            e.define("xcor",     lambda: t.x)
            e.define("ycor",     lambda: t.y)
            e.define("heading",  lambda: t.heading)
        else:
            # Stub turtle functions when no canvas
            for name in ["forward","backward","fd","bk","right","left","rt","lt",
                         "penup","pendown","pu","pd","home","setpos","setx","sety",
                         "setheading","hideturtle","showturtle","pencolor","color",
                         "bgcolor","clearscreen","cs","circle","xcor","ycor","heading"]:
                e.define(name, lambda *_: _NIL)

    # ── I/O ───────────────────────────────────────────────────────────────

    def _print(self, text: str, newline: bool = True) -> Any:
        if newline:
            self._output(text)
        else:
            # Accumulate until newline
            if not hasattr(self, "_partial"):
                self._partial = ""
            self._partial += text
            if "\n" in self._partial:
                lines = self._partial.split("\n")
                for line in lines[:-1]:
                    self._output(line)
                self._partial = lines[-1]
        return _NIL

    def _flush_partial(self):
        if hasattr(self, "_partial") and self._partial:
            self._output(self._partial)
            self._partial = ""

    def _format(self, dest: Any, fmt: str, args: tuple) -> Any:
        """SRFI-28 / Common Lisp format subset."""
        result = []
        ai = 0
        i = 0
        while i < len(fmt):
            ch = fmt[i]
            if ch == "~" and i + 1 < len(fmt):
                directive = fmt[i + 1]
                i += 2
                if directive in ("a", "A"):
                    result.append(_scheme_display(args[ai]) if ai < len(args) else "")
                    ai += 1
                elif directive in ("s", "S"):
                    result.append(_scheme_repr(args[ai]) if ai < len(args) else "")
                    ai += 1
                elif directive == "%":
                    result.append("\n")
                elif directive == "~":
                    result.append("~")
                elif directive in ("d", "D"):
                    result.append(str(int(args[ai])) if ai < len(args) else "")
                    ai += 1
                else:
                    result.append(f"~{directive}")
            else:
                result.append(ch)
                i += 1
        text = "".join(result)
        if dest is False:
            return text  # return string
        # dest is #t or port — print it
        for line in text.split("\n"):
            if line:
                self._output(line)
        return _NIL

    # ── call/cc (basic — no full continuation capture) ────────────────────

    def _call_cc(self, proc: Any) -> Any:
        class _Escape(Exception):
            def __init__(self, val):
                self.val = val

        def escape(val=_NIL):
            raise _Escape(val)

        try:
            return self._apply(proc, [escape])
        except _Escape as e:
            return e.val

    def _with_exception_handler(self, handler, thunk):
        try:
            return self._apply(thunk, [])
        except SchemeError as e:
            return self._apply(handler, [str(e)])


# ---------------------------------------------------------------------------
# Environment (top-level runner)
# ---------------------------------------------------------------------------

class SchemeEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output_lines: list[str] = []

    def _emit(self, line: str) -> None:
        self._output_lines.append(line)

    def run(self, source: str) -> str:
        interp = SchemeInterpreter(self._emit, self.turtle)
        try:
            exprs = _read_all(source)
        except SchemeError as e:
            return f"❌ Parse error: {e}\n"

        for expr in exprs:
            try:
                interp.eval(expr, interp._global_env)
            except SchemeError as e:
                self._output_lines.append(f"❌ {e}")
            except RecursionError:
                self._output_lines.append("❌ Stack overflow (infinite recursion?)")
            except Exception as e:
                self._output_lines.append(f"❌ Runtime error: {e}")

        interp._flush_partial()
        return "\n".join(self._output_lines) + ("\n" if self._output_lines else "")


# ---------------------------------------------------------------------------
# Standard library helpers
# ---------------------------------------------------------------------------

def _parse_params(params_cell: Any) -> tuple[list[str], str | None]:
    """Parse (a b . rest) into (['a','b'], 'rest')."""
    if isinstance(params_cell, _Symbol):
        return [], str(params_cell)
    if params_cell is _NIL:
        return [], None
    if not isinstance(params_cell, SchemeCell):
        return [], None
    positional: list[str] = []
    node = params_cell
    while isinstance(node, SchemeCell):
        positional.append(str(node.car))
        node = node.cdr
    rest = str(node) if node is not _NIL else None
    return positional, rest


def _make_record(cls, fields, args):
    r = cls.__new__(cls)
    r._type = cls._type_name
    r._fields = dict(zip(fields, args))
    return r


def _setcar(p, v):
    if not isinstance(p, SchemeCell):
        raise SchemeError(f"set-car!: not a pair: {_scheme_repr(p)}")
    p.car = v
    return _NIL


def _setcdr(p, v):
    if not isinstance(p, SchemeCell):
        raise SchemeError(f"set-cdr!: not a pair: {_scheme_repr(p)}")
    p.cdr = v
    return _NIL


def _is_proper_list(x: Any) -> bool:
    slow = x
    fast = x
    while True:
        if fast is _NIL:
            return True
        if not isinstance(fast, SchemeCell):
            return False
        fast = fast.cdr
        if fast is _NIL:
            return True
        if not isinstance(fast, SchemeCell):
            return False
        fast = fast.cdr
        slow = slow.cdr
        if slow is fast:
            return False  # cycle


def _append(*args) -> Any:
    if not args:
        return _NIL
    if len(args) == 1:
        return args[0]
    head = args[0]
    tail = _append(*args[1:])
    if head is _NIL:
        return tail
    items = _cells_to_list(head)
    result = tail
    for x in reversed(items):
        result = SchemeCell(x, result)
    return result


def _list_tail(lst: Any, k: int) -> Any:
    node = lst
    for _ in range(k):
        if not isinstance(node, SchemeCell):
            raise SchemeError("list-tail: index out of range")
        node = node.cdr
    return node


def _assoc(key, lst, eq_fn) -> Any:
    node = lst
    while isinstance(node, SchemeCell):
        pair = node.car
        if isinstance(pair, SchemeCell) and eq_fn(key, pair.car):
            return pair
        node = node.cdr
    return False


def _member(key, lst, eq_fn) -> Any:
    node = lst
    while isinstance(node, SchemeCell):
        if eq_fn(key, node.car):
            return node
        node = node.cdr
    return False


def _map(interp: SchemeInterpreter, proc, lsts) -> list:
    lists = [_cells_to_list(l) for l in lsts]
    return [interp._apply(proc, [l[i] for l in lists]) for i in range(min(len(l) for l in lists))]


def _for_each(interp: SchemeInterpreter, proc, lsts) -> Any:
    lists = [_cells_to_list(l) for l in lsts]
    for i in range(min(len(l) for l in lists)):
        interp._apply(proc, [l[i] for l in lists])
    return _NIL


def _reduce(interp, fn, init, lst) -> Any:
    items = _cells_to_list(lst)
    acc = init
    for x in items:
        acc = interp._apply(fn, [acc, x])
    return acc


def _fold_left(interp, fn, init, lst) -> Any:
    acc = init
    for x in _cells_to_list(lst):
        acc = interp._apply(fn, [acc, x])
    return acc


def _fold_right(interp, fn, init, lst) -> Any:
    acc = init
    for x in reversed(_cells_to_list(lst)):
        acc = interp._apply(fn, [x, acc])
    return acc


def _scheme_sort(interp, lst, cmp=None) -> Any:
    items = _cells_to_list(lst)
    if cmp is None or cmp is _NIL:
        import functools
        items_sorted = sorted(items, key=lambda x: x if isinstance(x, (int, float)) else str(x))
    else:
        import functools
        def key_fn(a, b):
            r = interp._apply(cmp, [a, b])
            return -1 if r is not False else 1
        items_sorted = sorted(items, key=functools.cmp_to_key(key_fn))
    return _list_to_cells(items_sorted)


def _apply_args(args) -> list:
    """Build argument list for (apply f a b ... list)."""
    if not args:
        return []
    if len(args) == 1:
        return _cells_to_list(args[0])
    return list(args[:-1]) + _cells_to_list(args[-1])


def _cxr_combos():
    """Generate combinations for caar, cadr, ... cdddr etc."""
    combos = ["aa", "ad", "da", "dd",
              "aaa", "aad", "ada", "add", "daa", "dad", "dda", "ddd",
              "aaaa", "aaad", "aada", "aadd", "adaa", "adad", "adda", "addd",
              "daaa", "daad", "dada", "dadd", "ddaa", "ddad", "ddda", "dddd"]
    return combos


def _make_cxr(combo: str):
    def fn(lst):
        x = lst
        for c in reversed(combo):
            if c == "a":
                x = x.car
            else:
                x = x.cdr
        return x
    fn.__name__ = f"c{combo}r"
    return fn


def _gcd(a: int, b: int) -> int:
    while b:
        a, b = b, a % b
    return abs(a)


def _gcd_list(items: list) -> int:
    if not items:
        return 0
    result = items[0]
    for x in items[1:]:
        result = _gcd(result, x)
    return result


def _lcm_list(items: list) -> int:
    if not items:
        return 1
    result = items[0]
    for x in items[1:]:
        g = _gcd(result, x)
        result = abs(result * x) // g if g else 0
    return result


def _num_to_str(n: int, base: int) -> str:
    if base == 10:
        return str(n)
    if base == 16:
        return hex(n)[2:]
    if base == 8:
        return oct(n)[2:]
    if base == 2:
        return bin(n)[2:]
    return str(n)


def _str_to_num(s: str, base: int = 10) -> Any:
    if not isinstance(s, str):
        return False
    try:
        if "." in s:
            return float(s)
        return int(s, base)
    except (ValueError, TypeError):
        return False


def _vector_set(v: SchemeVector, i: int, x: Any) -> Any:
    v.items[i] = x
    return _NIL


def _dynamic_wind(interp, before, thunk, after):
    interp._apply(before, [])
    try:
        result = interp._apply(thunk, [])
    finally:
        interp._apply(after, [])
    return result


def _draw_circle(turtle, radius: float):
    """Draw a circle approximated by polygon steps."""
    import math as _math
    steps = max(36, int(abs(radius) * 2))
    angle = 360.0 / steps
    dist = 2 * _math.pi * abs(radius) / steps
    for _ in range(steps):
        turtle.forward(dist)
        turtle.right(angle)
