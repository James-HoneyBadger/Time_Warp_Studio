"""Scheme language executor for Time Warp Studio.

Educational Scheme interpreter — whole-program execution.

Supports the classic Lisp/Scheme feature set used for teaching functional
programming:

  * ``define`` (variables, procedures, ``(define (f x) ...)`` sugar)
  * ``lambda`` with lexical closures and variadic ``. rest`` parameters
  * special forms: ``if``, ``cond``, ``case``, ``when``, ``unless``, ``and``,
    ``or``, ``begin``, ``let``, ``let*``, ``letrec``, ``do``, ``quote``,
    ``quasiquote``/``unquote``/``unquote-splicing``, ``set!``, ``else``
  * booleans, numbers, strings, characters, symbols, pairs/lists, vectors
  * arithmetic, comparison, list, string, char and predicate builtins
  * ``display`` / ``write`` / ``newline`` for program output
  * turtle graphics bindings (``forward``, ``right``, ``penup`` …)

Errors are reported with the ``❌`` prefix, matching every other Time Warp
executor.
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------


class Symbol(str):
    """A Scheme symbol.  Subclasses ``str`` so it prints naturally."""

    __slots__ = ()


class Pair:
    """A mutable cons cell."""

    __slots__ = ("car", "cdr")

    def __init__(self, car: Any, cdr: Any) -> None:
        self.car = car
        self.cdr = cdr


class Procedure:
    """A user-defined (or primitive) Scheme procedure."""

    __slots__ = ("params", "rest", "body", "env", "prim", "name")

    def __init__(
        self,
        params: list[Symbol] | None = None,
        rest: Symbol | None = None,
        body: list[Any] | None = None,
        env: "Env | None" = None,
        prim: Any = None,
        name: str = "lambda",
    ) -> None:
        self.params = params or []
        self.rest = rest
        self.body = body or []
        self.env = env
        self.prim = prim
        self.name = name


class _Nil:
    """The unique empty-list value, distinct from Python lists."""

    __slots__ = ()

    def __repr__(self) -> str:  # pragma: no cover - debugging aid
        return "()"


NIL = _Nil()

# Internal marker for the "undefined" result of set!/define.
_UNSPECIFIED = object()


class SchemeError(Exception):
    """Raised for Scheme-level runtime errors."""


class Env:
    """A lexical environment frame."""

    __slots__ = ("bindings", "parent")

    def __init__(self, parent: "Env | None" = None, bindings: dict | None = None):
        self.parent = parent
        self.bindings: dict[str, Any] = bindings or {}

    def lookup(self, name: str) -> Any:
        env: Env | None = self
        while env is not None:
            if name in env.bindings:
                return env.bindings[name]
            env = env.parent
        raise SchemeError(f"unbound variable: {name}")

    def set(self, name: str, value: Any) -> None:
        env: Env | None = self
        while env is not None:
            if name in env.bindings:
                env.bindings[name] = value
                return
            env = env.parent
        raise SchemeError(f"set!: unbound variable: {name}")

    def define(self, name: str, value: Any) -> None:
        self.bindings[name] = value


# ---------------------------------------------------------------------------
# Tokenizer / parser
# ---------------------------------------------------------------------------

_DELIMITERS = set("()'\"`; \t\r\n,")


def _tokenize(source: str) -> list[str]:
    tokens: list[str] = []
    i = 0
    n = len(source)
    while i < n:
        ch = source[i]
        if ch in " \t\r\n":
            i += 1
            continue
        if ch == ";":
            while i < n and source[i] != "\n":
                i += 1
            continue
        if ch in "()'`,":
            if ch == "," and i + 1 < n and source[i + 1] == "@":
                tokens.append(",@")
                i += 2
                continue
            tokens.append(ch)
            i += 1
            continue
        if ch == '"':
            j = i + 1
            buf: list[str] = []
            while j < n and source[j] != '"':
                if source[j] == "\\" and j + 1 < n:
                    esc = source[j + 1]
                    buf.append(
                        {"n": "\n", "t": "\t", "r": "\r", "\\": "\\", '"': '"'}.get(
                            esc, esc
                        )
                    )
                    j += 2
                    continue
                buf.append(source[j])
                j += 1
            if j >= n:
                raise SchemeError("unterminated string literal")
            tokens.append('"' + "".join(buf) + '"')
            i = j + 1
            continue
        if ch == "|":  # |symbol with spaces|
            j = source.find("|", i + 1)
            if j == -1:
                raise SchemeError("unterminated |symbol|")
            tokens.append(source[i + 1 : j])
            i = j + 1
            continue
        # bare atom
        j = i
        while j < n and source[j] not in _DELIMITERS:
            j += 1
        tokens.append(source[i:j])
        i = j
    return tokens


def _atom(token: str) -> Any:
    if token == "#t" or token == "#true":
        return True
    if token == "#f" or token == "#false":
        return False
    if len(token) > 1 and token.startswith('"') and token.endswith('"'):
        return token[1:-1]
    if len(token) > 1 and token[0] == "#" and token[1] == "\\":
        named = {"space": " ", "newline": "\n", "tab": "\t"}
        return named.get(token[2:].lower(), token[2:])
    try:
        return int(token)
    except ValueError:
        pass
    try:
        return float(token)
    except ValueError:
        pass
    return Symbol(token)


def _parse(tokens: list[str]) -> list[Any]:
    pos = 0

    def parse_expr() -> Any:
        nonlocal pos
        if pos >= len(tokens):
            raise SchemeError("unexpected end of input")
        token = tokens[pos]
        pos += 1
        if token == "(":
            lst: list[Any] = []
            while pos < len(tokens) and tokens[pos] != ")":
                lst.append(parse_expr())
            if pos >= len(tokens):
                raise SchemeError("missing closing )")
            pos += 1  # consume )
            return lst
        if token == ")":
            raise SchemeError("unexpected )")
        if token == "'":
            return [Symbol("quote"), parse_expr()]
        if token == "`":
            return [Symbol("quasiquote"), parse_expr()]
        if token == ",":
            return [Symbol("unquote"), parse_expr()]
        if token == ",@":
            return [Symbol("unquote-splicing"), parse_expr()]
        return _atom(token)

    forms: list[Any] = []
    while pos < len(tokens):
        forms.append(parse_expr())
    return forms


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _truthy(value: Any) -> bool:
    return value is not False


def is_list(value: Any) -> bool:
    while isinstance(value, Pair):
        value = value.cdr
    return value is NIL


def _to_py_list(value: Any) -> list[Any]:
    out: list[Any] = []
    while isinstance(value, Pair):
        out.append(value.car)
        value = value.cdr
    if value is not NIL:
        raise SchemeError("expected a proper list")
    return out


def _from_py_list(items: list[Any], tail: Any = NIL) -> Any:
    result = tail
    for item in reversed(items):
        result = Pair(item, result)
    return result


def _num(value: Any, who: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise SchemeError(f"{who}: expected a number, got {_type_name(value)}")
    return value


def _type_name(value: Any) -> str:
    if isinstance(value, bool):
        return "boolean"
    if isinstance(value, (int, float)):
        return "number"
    if isinstance(value, str) and not isinstance(value, Symbol):
        return "string"
    if isinstance(value, Symbol):
        return "symbol"
    if isinstance(value, Procedure):
        return "procedure"
    if value is NIL or isinstance(value, Pair):
        return "list"
    if isinstance(value, list):
        return "vector"
    return type(value).__name__


def _default_float(fmt: float) -> str:
    if isinstance(fmt, float) and fmt.is_integer():
        return str(int(fmt))
    return str(fmt)


def _display_str(value: Any) -> str:
    """Format a value the way ``display`` does (strings unquoted)."""
    if value is True:
        return "#t"
    if value is False:
        return "#f"
    if value is NIL:
        return "()"
    if isinstance(value, str) and not isinstance(value, Symbol):
        return value
    return _write_str(value)


def _write_str(value: Any) -> str:
    """Format a value the way ``write`` does (strings quoted)."""
    if value is True:
        return "#t"
    if value is False:
        return "#f"
    if value is NIL:
        return "()"
    if isinstance(value, float):
        return _default_float(value)
    if isinstance(value, int) and not isinstance(value, bool):
        return str(value)
    if isinstance(value, Symbol):
        return str(value)
    if isinstance(value, str):
        return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'
    if isinstance(value, Procedure):
        return f"#<procedure {value.name}>"
    if isinstance(value, Pair):
        parts: list[str] = []
        cur = value
        while isinstance(cur, Pair):
            parts.append(_write_str(cur.car))
            cur = cur.cdr
        if cur is not NIL:
            parts.append(".")
            parts.append(_write_str(cur))
        return "(" + " ".join(parts) + ")"
    if isinstance(value, list):
        return "#(" + " ".join(_write_str(v) for v in value) + ")"
    return str(value)


# ---------------------------------------------------------------------------
# Evaluation
# ---------------------------------------------------------------------------


class Evaluator:
    """Tree-walking Evaluator with a step budget for runaway protection."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState") -> None:
        self.interpreter = interpreter
        self.turtle = turtle
        self.out: list[str] = []
        self._steps = 0
        self._global = Env()
        self._install_builtins()

    # -- tick -------------------------------------------------------------
    def _tick(self, n: int = 1) -> None:
        self._steps += n
        if self._steps % 512 == 0:
            tick = getattr(self.interpreter, "tick", None)
            if callable(tick):
                tick(512)

    # -- output -----------------------------------------------------------
    def emit(self, text: str) -> None:
        self.out.append(text)

    # -- turtle -----------------------------------------------------------
    def _install_turtle(self, env: Env) -> None:
        tu = self.turtle

        def _t_forward(d):
            tu.forward(float(d))
            return _UNSPECIFIED

        def _t_back(d):
            tu.back(float(d))
            return _UNSPECIFIED

        def _t_right(d):
            tu.right(float(d))
            return _UNSPECIFIED

        def _t_left(d):
            tu.left(float(d))
            return _UNSPECIFIED

        def _t_goto(x, y):
            tu.goto(float(x), float(y))
            return _UNSPECIFIED

        def _t_setheading(d):
            tu.setheading(float(d))
            return _UNSPECIFIED

        def _t_pensize(w):
            tu.setpenwidth(float(w))
            return _UNSPECIFIED

        def _t_color(r, g, b):
            tu.setcolor(int(r), int(g), int(b))
            return _UNSPECIFIED

        def _t_penup():
            tu.penup()
            return _UNSPECIFIED

        def _t_pendown():
            tu.pendown()
            return _UNSPECIFIED

        def _t_home():
            tu.home()
            return _UNSPECIFIED

        def _t_clear():
            tu.clear()
            return _UNSPECIFIED

        def _t_circle(radius):
            tu.circle(float(radius))
            return _UNSPECIFIED

        def _t_show():
            tu.showturtle()
            return _UNSPECIFIED

        def _t_hide():
            tu.hideturtle()
            return _UNSPECIFIED

        prims = {
            "forward": _t_forward,
            "fd": _t_forward,
            "backward": _t_back,
            "back": _t_back,
            "bk": _t_back,
            "right": _t_right,
            "rt": _t_right,
            "left": _t_left,
            "lt": _t_left,
            "goto": _t_goto,
            "setpos": _t_goto,
            "setheading": _t_setheading,
            "pensize": _t_pensize,
            "width": _t_pensize,
            "color": _t_color,
            "penup": _t_penup,
            "pu": _t_penup,
            "pendown": _t_pendown,
            "pd": _t_pendown,
            "home": _t_home,
            "clear-canvas": _t_clear,
            "circle": _t_circle,
            "show-turtle": _t_show,
            "hide-turtle": _t_hide,
        }
        for name, fn in prims.items():
            env.define(name, Procedure(prim=fn, name=name))

    # -- builtins ---------------------------------------------------------
    def _install_builtins(self) -> None:  # noqa: C901 - long but flat
        env = self._global

        def _arith(op, identity):
            def fn(*args):
                if not args:
                    return identity
                acc = _num(args[0], op)
                for a in args[1:]:
                    acc = op(acc, _num(a, op.__name__))
                return acc

            return fn

        import operator

        env.define("+", Procedure(prim=_arith(operator.add, 0), name="+"))
        env.define("*", Procedure(prim=_arith(operator.mul, 1), name="*"))
        env.define("-", Procedure(prim=lambda *a: -_num(a[0], "-") if len(a) == 1 else _arith(operator.sub, 0)(*a), name="-"))
        env.define("abs", Procedure(prim=lambda x: abs(_num(x, "abs")), name="abs"))
        env.define("min", Procedure(prim=lambda *a: min(_num(x, "min") for x in a), name="min"))
        env.define("max", Procedure(prim=lambda *a: max(_num(x, "max") for x in a), name="max"))

        def _div(*args):
            if len(args) == 1:
                args = (1, args[0])
            acc = _num(args[0], "/")
            for a in args[1:]:
                divisor = _num(a, "/")
                if divisor == 0:
                    raise SchemeError("division by zero")
                acc = acc / divisor
            return acc

        env.define("/", Procedure(prim=_div, name="/"))

        def _quotient(a, b):
            b = _num(b, "quotient")
            if b == 0:
                raise SchemeError("division by zero")
            return math.trunc(_num(a, "quotient") / b)

        env.define("quotient", Procedure(prim=_quotient, name="quotient"))
        env.define("remainder", Procedure(prim=lambda a, b: math.fmod(_num(a, "remainder"), _num(b, "remainder")), name="remainder"))
        env.define("modulo", Procedure(prim=lambda a, b: _num(a, "modulo") % _num(b, "modulo"), name="modulo"))
        env.define("expt", Procedure(prim=lambda a, b: _num(a, "expt") ** _num(b, "expt"), name="expt"))
        env.define("sqrt", Procedure(prim=lambda x: math.sqrt(_num(x, "sqrt")), name="sqrt"))
        env.define("floor", Procedure(prim=lambda x: math.floor(_num(x, "floor")), name="floor"))
        env.define("ceiling", Procedure(prim=lambda x: math.ceil(_num(x, "ceiling")), name="ceiling"))
        env.define("round", Procedure(prim=lambda x: round(_num(x, "round")), name="round"))
        env.define("truncate", Procedure(prim=lambda x: math.trunc(_num(x, "truncate")), name="truncate"))
        env.define("exact->inexact", Procedure(prim=lambda x: float(_num(x, "exact->inexact")), name="exact->inexact"))
        env.define("inexact->exact", Procedure(prim=lambda x: int(_num(x, "inexact->exact")), name="inexact->exact"))

        cmp_ops = {
            "=": operator.eq,
            "<": operator.lt,
            ">": operator.gt,
            "<=": operator.le,
            ">=": operator.ge,
        }

        def _chain(op, name):
            def fn(*args):
                if len(args) < 2:
                    return True
                vals = [_num(a, name) for a in args]
                return all(op(vals[i], vals[i + 1]) for i in range(len(vals) - 1))

            return fn

        for name, op in cmp_ops.items():
            env.define(name, Procedure(prim=_chain(op, name), name=name))

        env.define("not", Procedure(prim=lambda x: x is False, name="not"))
        env.define("eq?", Procedure(prim=lambda a, b: a is b or a == b or (isinstance(a, Symbol) and isinstance(b, Symbol) and str(a) == str(b)), name="eq?"))
        env.define("eqv?", Procedure(prim=lambda a, b: a == b or a is b, name="eqv?"))
        env.define("equal?", Procedure(prim=lambda a, b: _equal(a, b), name="equal?"))
        env.define("null?", Procedure(prim=lambda x: x is NIL, name="null?"))
        env.define("pair?", Procedure(prim=lambda x: isinstance(x, Pair), name="pair?"))
        env.define("list?", Procedure(prim=lambda x: is_list(x), name="list?"))
        env.define("number?", Procedure(prim=lambda x: isinstance(x, (int, float)) and not isinstance(x, bool), name="number?"))
        env.define("integer?", Procedure(prim=lambda x: isinstance(x, int) and not isinstance(x, bool), name="integer?"))
        env.define("boolean?", Procedure(prim=lambda x: isinstance(x, bool), name="boolean?"))
        env.define("string?", Procedure(prim=lambda x: isinstance(x, str) and not isinstance(x, Symbol), name="string?"))
        env.define("symbol?", Procedure(prim=lambda x: isinstance(x, Symbol), name="symbol?"))
        env.define("procedure?", Procedure(prim=lambda x: isinstance(x, Procedure), name="procedure?"))
        env.define("vector?", Procedure(prim=lambda x: isinstance(x, list), name="vector?"))
        env.define("zero?", Procedure(prim=lambda x: _num(x, "zero?") == 0, name="zero?"))
        env.define("positive?", Procedure(prim=lambda x: _num(x, "positive?") > 0, name="positive?"))
        env.define("negative?", Procedure(prim=lambda x: _num(x, "negative?") < 0, name="negative?"))
        env.define("odd?", Procedure(prim=lambda x: int(_num(x, "odd?")) % 2 != 0, name="odd?"))
        env.define("even?", Procedure(prim=lambda x: int(_num(x, "even?")) % 2 == 0, name="even?"))

        env.define("cons", Procedure(prim=lambda a, b: Pair(a, b), name="cons"))
        env.define("car", Procedure(prim=lambda p: _car(p), name="car"))
        env.define("cdr", Procedure(prim=lambda p: _cdr(p), name="cdr"))
        env.define("caar", Procedure(prim=lambda p: _car(_car(p)), name="caar"))
        env.define("cadr", Procedure(prim=lambda p: _car(_cdr(p)), name="cadr"))
        env.define("cdar", Procedure(prim=lambda p: _cdr(_car(p)), name="cdar"))
        env.define("cddr", Procedure(prim=lambda p: _cdr(_cdr(p)), name="cddr"))
        env.define("list", Procedure(prim=lambda *a: _from_py_list(list(a)), name="list"))
        env.define("length", Procedure(prim=lambda lst: len(_to_py_list(lst)), name="length"))
        env.define("append", Procedure(prim=lambda *ls: _append(ls), name="append"))
        env.define("reverse", Procedure(prim=lambda lst: _from_py_list(list(reversed(_to_py_list(lst)))), name="reverse"))
        env.define("list-ref", Procedure(prim=lambda lst, k: _to_py_list(lst)[int(k)], name="list-ref"))
        env.define("list-tail", Procedure(prim=lambda lst, k: _from_py_list(_to_py_list(lst)[int(k):]), name="list-tail"))
        env.define("member", Procedure(prim=lambda x, lst: _member(x, lst), name="member"))
        env.define("memq", Procedure(prim=lambda x, lst: _member(x, lst), name="memq"))
        env.define("assoc", Procedure(prim=lambda x, al: _assoc(x, al), name="assoc"))
        env.define("vector", Procedure(prim=lambda *a: list(a), name="vector"))
        env.define("make-vector", Procedure(prim=lambda n, fill=0: [fill] * int(n), name="make-vector"))
        env.define("vector-ref", Procedure(prim=lambda v, i: v[int(i)], name="vector-ref"))
        env.define("vector-length", Procedure(prim=lambda v: len(v), name="vector-length"))
        env.define("vector->list", Procedure(prim=lambda v: _from_py_list(list(v)), name="vector->list"))
        env.define("list->vector", Procedure(prim=lambda lst: _to_py_list(lst), name="list->vector"))

        env.define("display", Procedure(prim=lambda x: self._display(x, False), name="display"))
        env.define("write", Procedure(prim=lambda x: self._display(x, True), name="write"))
        def _newline():
            self.emit("\n")
            return _UNSPECIFIED

        env.define("newline", Procedure(prim=_newline, name="newline"))

        env.define("number->string", Procedure(prim=lambda n: _write_str(n), name="number->string"))
        env.define("string->number", Procedure(prim=lambda s, radix=10: _str_to_num(s), name="string->number"))
        env.define("symbol->string", Procedure(prim=lambda s: str(s), name="symbol->string"))
        env.define("string->symbol", Procedure(prim=lambda s: Symbol(s), name="string->symbol"))
        env.define("string-length", Procedure(prim=lambda s: len(s), name="string-length"))
        env.define("string-append", Procedure(prim=lambda *a: "".join(str(x) for x in a), name="string-append"))
        env.define("string-upcase", Procedure(prim=lambda s: str(s).upper(), name="string-upcase"))
        env.define("string-downcase", Procedure(prim=lambda s: str(s).lower(), name="string-downcase"))
        env.define("substring", Procedure(prim=lambda s, a, b=None: str(s)[int(a): int(b) if b is not None else None], name="substring"))
        env.define("string->list", Procedure(prim=lambda s: _from_py_list(list(str(s))), name="string->list"))
        env.define("string=? ", Procedure(prim=lambda *a: all(str(a[i]) == str(a[i + 1]) for i in range(len(a) - 1)), name="string=?"))
        env.define("string=?", Procedure(prim=lambda *a: all(str(a[i]) == str(a[i + 1]) for i in range(len(a) - 1)), name="string=?"))
        env.define("string<?", Procedure(prim=lambda *a: all(str(a[i]) < str(a[i + 1]) for i in range(len(a) - 1)), name="string<?"))
        env.define("string-ci=?", Procedure(prim=lambda *a: all(str(a[i]).lower() == str(a[i + 1]).lower() for i in range(len(a) - 1)), name="string-ci=?"))
        env.define("error", Procedure(prim=lambda *a: _raise_error(*a), name="error"))
        env.define("apply", Procedure(prim=self._apply_prim, name="apply"))
        env.define("map", Procedure(prim=lambda f, *ls: self._map(f, ls), name="map"))
        env.define("for-each", Procedure(prim=lambda f, *ls: self._for_each(f, ls), name="for-each"))
        env.define("filter", Procedure(prim=lambda f, ls: _from_py_list([x for x in _to_py_list(ls) if _truthy(self.apply(f, [x]))]), name="filter"))
        env.define("foldl", Procedure(prim=lambda f, init, ls: _foldl(self, f, init, _to_py_list(ls)), name="foldl"))

        self._install_turtle(env)

    def _display(self, value: Any, quoted: bool) -> Any:
        self.emit(_write_str(value) if quoted else _display_str(value))
        return _UNSPECIFIED

    def _apply_prim(self, proc, *args) -> Any:
        # (apply f a b '(c d)) → f(a, b, c, d)
        if not args:
            return self.apply(proc, [])
        head = list(args[:-1])
        tail = _to_py_list(args[-1])
        return self.apply(proc, head + tail)

    def _map(self, proc, lists) -> Any:
        cols = [_to_py_list(ls) for ls in lists]
        n = min(len(c) for c in cols)
        return _from_py_list([self.apply(proc, [c[i] for c in cols]) for i in range(n)])

    def _for_each(self, proc, lists) -> Any:
        cols = [_to_py_list(ls) for ls in lists]
        n = min(len(c) for c in cols)
        for i in range(n):
            self.apply(proc, [c[i] for c in cols])
        return _UNSPECIFIED

    # -- core evaluation --------------------------------------------------
    def eval(self, expr: Any, env: Env) -> Any:
        self._tick()
        while True:
            if isinstance(expr, Symbol):
                return env.lookup(str(expr))
            if not isinstance(expr, list):
                return expr
            if not expr:
                raise SchemeError("cannot evaluate ()")
            head = expr[0]
            if isinstance(head, Symbol):
                name = str(head)
                handler = _SPECIAL_FORMS.get(name)
                if handler is not None:
                    result = handler(self, expr[1:], env)
                    if result is _TAIL:
                        expr, env = _PENDING.pop()
                        continue
                    return result
            proc = self.eval(head, env)
            args = [self.eval(a, env) for a in expr[1:]]
            result = self.apply(proc, args)
            if result is _TAIL:
                expr, env = _PENDING.pop()
                continue
            return result

    def apply(self, proc: Any, args: list[Any]) -> Any:
        if not isinstance(proc, Procedure):
            raise SchemeError(f"attempt to call non-procedure: {_write_str(proc)}")
        if proc.prim is not None:
            return proc.prim(*args)
        local = Env(proc.env)
        for i, pname in enumerate(proc.params):
            local.define(str(pname), args[i] if i < len(args) else NIL)
        if proc.rest is not None:
            local.define(str(proc.rest), _from_py_list(args[len(proc.params):]))
        result: Any = _UNSPECIFIED
        for form in proc.body:
            result = self.eval(form, local)
        return result

    def run(self, source: str) -> str:
        forms = _parse(_tokenize(source))
        for form in forms:
            self.eval(form, self._global)
        return "".join(self.out)


# Sentinel used to signal the trampoline should re-dispatch.
_TAIL = object()
_PENDING: list[tuple[Any, Env]] = []


def _car(p: Any) -> Any:
    if isinstance(p, Pair):
        return p.car
    raise SchemeError("car: not a pair")


def _cdr(p: Any) -> Any:
    if isinstance(p, Pair):
        return p.cdr
    raise SchemeError("cdr: not a pair")


def _equal(a: Any, b: Any) -> bool:
    if isinstance(a, Pair) and isinstance(b, Pair):
        return _equal(a.car, b.car) and _equal(a.cdr, b.cdr)
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(_equal(x, y) for x, y in zip(a, b))
    if isinstance(a, Symbol) and isinstance(b, Symbol):
        return str(a) == str(b)
    return a == b


def _append(lists: tuple) -> Any:
    if not lists:
        return NIL
    result: list[Any] = []
    for lst in lists[:-1]:
        result.extend(_to_py_list(lst))
    return _from_py_list(result, lists[-1])


def _member(x: Any, lst: Any) -> Any:
    cur = lst
    while isinstance(cur, Pair):
        if _equal(cur.car, x):
            return cur
        cur = cur.cdr
    return False


def _assoc(x: Any, alist: Any) -> Any:
    cur = alist
    while isinstance(cur, Pair):
        entry = cur.car
        if isinstance(entry, Pair) and _equal(entry.car, x):
            return entry
        cur = cur.cdr
    return False


def _str_to_num(s: Any) -> Any:
    try:
        return int(str(s))
    except ValueError:
        pass
    try:
        return float(str(s))
    except ValueError:
        return False


def _raise_error(*args: Any) -> Any:
    raise SchemeError(" ".join(_display_str(a) for a in args))


def _foldl(ev: Evaluator, proc: Any, init: Any, items: list[Any]) -> Any:
    acc = init
    for item in items:
        acc = ev.apply(proc, [acc, item])
    return acc


# ---------------------------------------------------------------------------
# Special forms
# ---------------------------------------------------------------------------

_TAIL_FORMS = {"if", "begin", "cond", "when", "unless", "and", "or", "let", "let*"}


def _sf_quote(ev: Evaluator, args: list, env: Env) -> Any:
    return _quote(args[0]) if args else NIL


def _quote(value: Any) -> Any:
    if isinstance(value, list):
        return _from_py_list([_quote(v) for v in value])
    return value


def _sf_quasiquote(ev: Evaluator, args: list, env: Env) -> Any:
    return _qq(ev, args[0], env, 1) if args else NIL


def _qq(ev: Evaluator, value: Any, env: Env, depth: int) -> Any:
    if isinstance(value, list) and value:
        head = value[0]
        if isinstance(head, Symbol):
            if head == "unquote" and depth == 1:
                return ev.eval(value[1], env)
            if head == "quasiquote":
                return _from_py_list([Symbol("quasiquote"), _qq(ev, value[1], env, depth + 1)])
        out: list[Any] = []
        for item in value:
            if (
                isinstance(item, list)
                and item
                and isinstance(item[0], Symbol)
                and item[0] == "unquote-splicing"
                and depth == 1
            ):
                out.extend(_to_py_list(ev.eval(item[1], env)))
            else:
                out.append(_qq(ev, item, env, depth))
        return _from_py_list(out)
    return value


def _sf_if(ev: Evaluator, args: list, env: Env) -> Any:
    test = ev.eval(args[0], env)
    if _truthy(test):
        return ev.eval(args[1], env) if len(args) > 1 else _UNSPECIFIED
    return ev.eval(args[2], env) if len(args) > 2 else _UNSPECIFIED


def _sf_define(ev: Evaluator, args: list, env: Env) -> Any:
    target = args[0]
    if isinstance(target, list):  # (define (f x) body...)
        name = str(target[0])
        params, rest = _parse_params(target[1:])
        proc = Procedure(params, rest, args[1:], env, name=name)
        env.define(name, proc)
        return Symbol(name)
    name = str(target)
    value = ev.eval(args[1], env) if len(args) > 1 else _UNSPECIFIED
    if isinstance(value, Procedure) and value.name == "lambda":
        value.name = name
    env.define(name, value)
    return Symbol(name)


def _parse_params(items: list) -> tuple[list[Symbol], Symbol | None]:
    params: list[Symbol] = []
    rest: Symbol | None = None
    i = 0
    while i < len(items):
        item = items[i]
        if isinstance(item, Symbol) and str(item) == ".":
            rest = items[i + 1]
            break
        params.append(item)
        i += 1
    return params, rest


def _sf_lambda(ev: Evaluator, args: list, env: Env) -> Any:
    params, rest = _parse_params(args[0])
    return Procedure(params, rest, args[1:], env, name="lambda")


def _sf_set(ev: Evaluator, args: list, env: Env) -> Any:
    name = str(args[0])
    value = ev.eval(args[1], env)
    try:
        env.set(name, value)
    except SchemeError:
        env.define(name, value)
    return _UNSPECIFIED


def _sf_begin(ev: Evaluator, args: list, env: Env) -> Any:
    result: Any = _UNSPECIFIED
    for form in args:
        result = ev.eval(form, env)
    return result


def _sf_when(ev: Evaluator, args: list, env: Env) -> Any:
    if _truthy(ev.eval(args[0], env)):
        return _sf_begin(ev, args[1:], env)
    return _UNSPECIFIED


def _sf_unless(ev: Evaluator, args: list, env: Env) -> Any:
    if not _truthy(ev.eval(args[0], env)):
        return _sf_begin(ev, args[1:], env)
    return _UNSPECIFIED


def _sf_and(ev: Evaluator, args: list, env: Env) -> Any:
    result: Any = True
    for form in args:
        result = ev.eval(form, env)
        if result is False:
            return False
    return result


def _sf_or(ev: Evaluator, args: list, env: Env) -> Any:
    for form in args:
        result = ev.eval(form, env)
        if _truthy(result):
            return result
    return False


def _sf_cond(ev: Evaluator, args: list, env: Env) -> Any:
    for clause in args:
        if not isinstance(clause, list) or not clause:
            continue
        test = clause[0]
        if isinstance(test, Symbol) and str(test) == "else":
            return _sf_begin(ev, clause[1:], env)
        value = ev.eval(test, env)
        if _truthy(value):
            if len(clause) == 1:
                return value
            if len(clause) >= 3 and isinstance(clause[1], Symbol) and str(clause[1]) == "=>":
                return ev.apply(ev.eval(clause[2], env), [value])
            return _sf_begin(ev, clause[1:], env)
    return _UNSPECIFIED


def _sf_case(ev: Evaluator, args: list, env: Env) -> Any:
    key = ev.eval(args[0], env)
    for clause in args[1:]:
        if not isinstance(clause, list) or not clause:
            continue
        if isinstance(clause[0], Symbol) and str(clause[0]) == "else":
            return _sf_begin(ev, clause[1:], env)
        datums = _to_py_list(clause[0])
        if any(_equal(key, d) for d in datums):
            return _sf_begin(ev, clause[1:], env)
    return _UNSPECIFIED


def _sf_let(ev: Evaluator, args: list, env: Env) -> Any:
    if isinstance(args[0], Symbol):  # named let
        name = str(args[0])
        bindings = args[1]
        names = [str(b[0]) for b in bindings]
        values = [ev.eval(b[1], env) for b in bindings]
        local = Env(env)
        proc = Procedure([Symbol(n) for n in names], None, args[2:], local, name=name)
        local.define(name, proc)
        return ev.apply(proc, values)
    bindings = args[0]
    local = Env(env)
    for b in bindings:
        local.define(str(b[0]), ev.eval(b[1], env))
    return _sf_begin(ev, args[1:], local)


def _sf_let_star(ev: Evaluator, args: list, env: Env) -> Any:
    local = Env(env)
    for b in args[0]:
        local.define(str(b[0]), ev.eval(b[1], local))
    return _sf_begin(ev, args[1:], local)


def _sf_letrec(ev: Evaluator, args: list, env: Env) -> Any:
    local = Env(env)
    for b in args[0]:
        local.define(str(b[0]), _UNSPECIFIED)
    for b in args[0]:
        local.bindings[str(b[0])] = ev.eval(b[1], local)
    return _sf_begin(ev, args[1:], local)


def _sf_do(ev: Evaluator, args: list, env: Env) -> Any:
    specs = args[0]
    test_clause = args[1]
    body = args[2:]
    local = Env(env)
    names: list[str] = []
    for spec in specs:
        names.append(str(spec[0]))
        local.define(str(spec[0]), ev.eval(spec[1], local) if len(spec) > 1 else NIL)
    while True:
        if _truthy(ev.eval(test_clause[0], local)):
            if len(test_clause) > 1:
                return _sf_begin(ev, test_clause[1:], local)
            return _UNSPECIFIED
        _sf_begin(ev, body, local)
        for spec in specs:
            if len(spec) > 2:
                local.bindings[str(spec[0])] = ev.eval(spec[2], local)


_SPECIAL_FORMS = {
    "quote": _sf_quote,
    "quasiquote": _sf_quasiquote,
    "if": _sf_if,
    "define": _sf_define,
    "defun": _sf_define,
    "lambda": _sf_lambda,
    "set!": _sf_set,
    "begin": _sf_begin,
    "when": _sf_when,
    "unless": _sf_unless,
    "and": _sf_and,
    "or": _sf_or,
    "cond": _sf_cond,
    "case": _sf_case,
    "let": _sf_let,
    "let*": _sf_let_star,
    "letrec": _sf_letrec,
    "do": _sf_do,
}


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_scheme(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete Scheme program and return all output."""
    try:
        evaluator = Evaluator(interpreter, turtle)
        text = evaluator.run(source)
    except SchemeError as exc:
        return f"❌ Scheme error: {exc}\n"
    except RecursionError:
        return "❌ Scheme error: Maximum recursion depth exceeded\n"
    except MemoryError:
        return "❌ Scheme error: Out of memory\n"
    if not text:
        return ""
    return text if text.endswith("\n") else text + "\n"
