"""Scheme language executor for Time Warp Studio.

Educational R5RS-flavoured Scheme interpreter.
Supports: define, lambda, let/let*/letrec, if, cond, begin,
          list operations, tail calls (trampoline), and basic I/O.
"""

from __future__ import annotations

import math
import operator
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..core.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def execute_scheme(interpreter: "Interpreter", source: str, turtle: "TurtleState") -> str:
    """Execute a complete Scheme program and return all output."""
    env = SchemeEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Tokeniser / Parser
# ---------------------------------------------------------------------------

def _tokenise(source: str) -> list[str]:
    """Tokenise Scheme source into a flat list of tokens.

    Handles string literals with spaces correctly by extracting them
    before splitting on whitespace.
    """
    # Remove comments
    source = re.sub(r";[^\n]*", "", source)
    # Tokenise using a regex that recognises strings, parens, and atoms
    token_re = re.compile(
        r"""("(?:[^"\\]|\\.)*")"""   # double-quoted string (group 1)
        r"""|([()]"""                # parentheses
        r"""|'|`|,@|,"""            # quote shorthands
        r"""|[^\s()"`,;]+)""",      # atoms / symbols / numbers
        re.VERBOSE,
    )
    tokens: list[str] = []
    for m in token_re.finditer(source):
        tokens.append(m.group(0))
    return tokens


def _parse(tokens: list[str]) -> list:
    """Return list of top-level S-expressions."""
    exprs = []
    pos = [0]

    def read():
        if pos[0] >= len(tokens):
            raise SchemeSyntaxError("Unexpected EOF")
        tok = tokens[pos[0]]
        pos[0] += 1
        if tok == "(":
            lst = []
            while pos[0] < len(tokens) and tokens[pos[0]] != ")":
                lst.append(read())
            if pos[0] >= len(tokens):
                raise SchemeSyntaxError("Missing )")
            pos[0] += 1  # consume )
            return lst
        if tok == ")":
            raise SchemeSyntaxError("Unexpected )")
        if tok == "'":
            return ["quote", read()]
        if tok == "`":
            return ["quasiquote", read()]
        if tok == ",@":
            return ["unquote-splicing", read()]
        if tok == ",":
            return ["unquote", read()]
        # Atom
        return _atom(tok)

    while pos[0] < len(tokens):
        exprs.append(read())
    return exprs


def _atom(tok: str) -> Any:
    if tok == "#t" or tok.lower() == "true":
        return True
    if tok == "#f" or tok.lower() == "false":
        return False
    try:
        return int(tok)
    except ValueError:
        pass
    try:
        return float(tok)
    except ValueError:
        pass
    if tok.startswith('"'):
        return tok.strip('"').replace("\\n", "\n").replace("\\t", "\t")
    return Symbol(tok)


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------

class Symbol(str):
    pass


class Pair:
    __slots__ = ("car", "cdr")

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __repr__(self):
        parts = []
        cur = self
        while isinstance(cur, Pair):
            parts.append(scheme_str(cur.car))
            cur = cur.cdr
        if cur is NIL:
            return "(" + " ".join(parts) + ")"
        return "(" + " ".join(parts) + " . " + scheme_str(cur) + ")"


NIL = []  # empty list


class SchemeProc:
    _evaluator = None  # set by SchemeExecutor at init

    def __init__(self, params, body, env, name="λ"):
        self.params = params
        self.body = body
        self.env = env
        self.name = name

    def __repr__(self):
        return f"#<procedure:{self.name}>"

    def __call__(self, *args):
        """Allow SchemeProc to be called like a Python function (for map/filter)."""
        ev = SchemeProc._evaluator
        if ev is None:
            raise SchemeError("No evaluator set for SchemeProc")
        call_env = Env(self.params, args, outer=self.env)
        for sub in self.body[:-1]:
            ev.eval_expr(sub, call_env)
        return ev.eval_expr(self.body[-1], call_env)


class TailCall:
    __slots__ = ("expr", "env")

    def __init__(self, expr, env):
        self.expr = expr
        self.env = env


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------

class Env(dict):
    def __init__(self, params=(), args=(), outer=None):
        super().__init__()
        if isinstance(params, Symbol):
            self[params] = list(args)
        else:
            if len(params) != len(args):
                raise SchemeError(f"Expected {len(params)} args, got {len(args)}")
            self.update(zip(params, args))
        self.outer = outer

    def find(self, var):
        if var in self:
            return self
        if self.outer is not None:
            return self.outer.find(var)
        raise SchemeError(f"Unbound variable: {var}")


# ---------------------------------------------------------------------------
# Interpreter
# ---------------------------------------------------------------------------

class SchemeEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self.global_env = self._build_global_env()
        # Allow SchemeProc to call back into the evaluator
        SchemeProc._evaluator = self

    def _emit(self, text: str):
        self._output.append(str(text))

    def run(self, source: str) -> str:
        try:
            tokens = _tokenise(source)
            exprs = _parse(tokens)
            for expr in exprs:
                self.eval_expr(expr, self.global_env)
        except SchemeError as e:
            self._emit(f"❌ Scheme error: {e}")
        except SchemeSyntaxError as e:
            self._emit(f"❌ Scheme syntax error: {e}")
        except RecursionError:
            self._emit("❌ Scheme error: maximum recursion depth exceeded")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        return "\n".join(self._output)

    def eval_expr(self, expr, env: Env) -> Any:
        """Evaluate expr with trampoline for tail calls."""
        current_expr = expr
        current_env = env
        for _ in range(100_000):
            result = self._step(current_expr, current_env)
            if isinstance(result, TailCall):
                current_expr = result.expr
                current_env = result.env
            else:
                return result
        raise SchemeError("Execution limit exceeded (infinite loop?)")

    def _step(self, expr, env: Env) -> Any:
        # Self-evaluating
        if expr is True or expr is False:
            return expr
        if isinstance(expr, (int, float, str)) and not isinstance(expr, Symbol):
            return expr
        if expr is NIL or expr == []:
            return NIL
        if isinstance(expr, Symbol):
            return env.find(expr)[expr]

        if not isinstance(expr, list) or len(expr) == 0:
            return expr

        head = expr[0]

        # Special forms
        if head == Symbol("quote"):
            return expr[1]

        if head == Symbol("if"):
            test = self.eval_expr(expr[1], env)
            if _scheme_truthy(test):
                return TailCall(expr[2], env)
            elif len(expr) > 3:
                return TailCall(expr[3], env)
            return False

        if head == Symbol("cond"):
            for clause in expr[1:]:
                if clause[0] == Symbol("else") or _scheme_truthy(self.eval_expr(clause[0], env)):
                    return TailCall(["begin"] + clause[1:], env)
            return False

        if head == Symbol("and"):
            result = True
            for e in expr[1:]:
                result = self.eval_expr(e, env)
                if not _scheme_truthy(result):
                    return False
            return result

        if head == Symbol("or"):
            for e in expr[1:]:
                result = self.eval_expr(e, env)
                if _scheme_truthy(result):
                    return result
            return False

        if head == Symbol("define"):
            if isinstance(expr[1], list):
                # (define (f args) body)
                name = expr[1][0]
                params = expr[1][1:]
                body = expr[2:]
                env[name] = SchemeProc(params, body, env, name=str(name))
            else:
                env[expr[1]] = self.eval_expr(expr[2], env)
            return None

        if head == Symbol("set!"):
            env.find(expr[1])[expr[1]] = self.eval_expr(expr[2], env)
            return None

        if head == Symbol("lambda"):
            return SchemeProc(expr[1], expr[2:], env)

        if head == Symbol("begin"):
            for sub in expr[1:-1]:
                self.eval_expr(sub, env)
            return TailCall(expr[-1], env) if len(expr) > 1 else None

        if head == Symbol("let"):
            if isinstance(expr[1], Symbol):
                # Named let
                name = expr[1]
                bindings = expr[2]
                body = expr[3:]
                params = [b[0] for b in bindings]
                args = [self.eval_expr(b[1], env) for b in bindings]
                new_env = Env(params, args, outer=env)
                proc = SchemeProc(params, body, new_env, name=str(name))
                new_env[name] = proc
                return TailCall(["begin"] + body, new_env)
            bindings = expr[1]
            new_env = Env(outer=env)
            for name, val_expr in bindings:
                new_env[name] = self.eval_expr(val_expr, env)
            return TailCall(["begin"] + expr[2:], new_env)

        if head == Symbol("let*"):
            new_env = Env(outer=env)
            for name, val_expr in expr[1]:
                new_env[name] = self.eval_expr(val_expr, new_env)
            return TailCall(["begin"] + expr[2:], new_env)

        if head == Symbol("letrec"):
            new_env = Env(outer=env)
            for name, _ in expr[1]:
                new_env[name] = None
            for name, val_expr in expr[1]:
                new_env[name] = self.eval_expr(val_expr, new_env)
            return TailCall(["begin"] + expr[2:], new_env)

        if head == Symbol("do"):
            bindings = expr[1]
            test_clause = expr[2]
            commands = expr[3:]
            vars_ = [b[0] for b in bindings]
            inits = [self.eval_expr(b[1], env) for b in bindings]
            steps = [b[2] if len(b) > 2 else b[0] for b in bindings]
            do_env = Env(vars_, inits, outer=env)
            limit = 10000
            count = 0
            while True:
                if count >= limit:
                    raise SchemeError("do loop limit exceeded")
                cond = self.eval_expr(test_clause[0], do_env)
                if _scheme_truthy(cond):
                    if len(test_clause) > 1:
                        return self.eval_expr(["begin"] + test_clause[1:], do_env)
                    return None
                for cmd in commands:
                    self.eval_expr(cmd, do_env)
                new_vals = [self.eval_expr(s, do_env) for s in steps]
                for v, nv in zip(vars_, new_vals):
                    do_env[v] = nv
                count += 1

        if head == Symbol("when"):
            if _scheme_truthy(self.eval_expr(expr[1], env)):
                return TailCall(["begin"] + expr[2:], env)
            return None

        if head == Symbol("unless"):
            if not _scheme_truthy(self.eval_expr(expr[1], env)):
                return TailCall(["begin"] + expr[2:], env)
            return None

        if head == Symbol("case"):
            key = self.eval_expr(expr[1], env)
            for clause in expr[2:]:
                if clause[0] == Symbol("else") or (isinstance(clause[0], list) and key in clause[0]):
                    return TailCall(["begin"] + clause[1:], env)
            return None

        if head == Symbol("letrec*"):
            new_env = Env(outer=env)
            for name, val_expr in expr[1]:
                new_env[name] = self.eval_expr(val_expr, new_env)
            return TailCall(["begin"] + expr[2:], new_env)

        if head == Symbol("receive"):
            # (receive formals expression body ...)
            formals = expr[1]
            exp = self.eval_expr(expr[2], env)
            new_env = Env(outer=env)
            if isinstance(exp, tuple):
                if isinstance(formals, list):
                    for name, val in zip(formals, exp):
                        new_env[name] = val
                else:
                    new_env[formals] = list(exp)
            else:
                if isinstance(formals, list) and formals:
                    new_env[formals[0]] = exp
                else:
                    new_env[formals] = [exp]
            return TailCall(["begin"] + expr[3:], new_env)

        # define-syntax / syntax-rules: stub — macros not supported, silently skip
        if head == Symbol("define-syntax") or head == Symbol("syntax-rules"):
            return None
        if head == Symbol("let-syntax") or head == Symbol("letrec-syntax"):
            return TailCall(["begin"] + expr[2:], env) if len(expr) > 2 else None

        # delay / force for lazy evaluation
        if head == Symbol("delay"):
            thunk_expr = expr[1]
            thunk_env = env
            return lambda: self.eval_expr(thunk_expr, thunk_env)
        if head == Symbol("force"):
            promise = self.eval_expr(expr[1], env)
            return promise() if callable(promise) else promise
        if head == Symbol("make-promise"):
            v = self.eval_expr(expr[1], env)
            return lambda: v

        if head == Symbol("define-record-type"):
            # Simplified R7RS record types
            type_name = expr[1]
            constructor_spec = expr[2]
            predicate = expr[3]
            fields = expr[4:]
            ctor_name = constructor_spec[0]
            ctor_fields = constructor_spec[1:]
            # Create constructor
            def make_ctor(cn, flds):
                def ctor(*args):
                    return {str(f): args[i] if i < len(args) else None for i, f in enumerate(flds)}
                return ctor
            env[ctor_name] = make_ctor(ctor_name, ctor_fields)
            env[predicate] = lambda x: isinstance(x, dict)
            for field in fields:
                if isinstance(field, list) and len(field) >= 2:
                    acc_name = field[1]
                    field_name = str(field[0])
                    def make_accessor(fn):
                        return lambda rec: rec.get(fn)
                    env[acc_name] = make_accessor(field_name)
                    if len(field) >= 3:
                        mut_name = field[2]
                        def make_mutator(fn):
                            return lambda rec, val: rec.__setitem__(fn, val) or None
                        env[mut_name] = make_mutator(field_name)
            return None

        if head == Symbol("guard"):
            # (guard (var clause ...) body ...)
            var_clauses = expr[1]
            var = var_clauses[0]
            clauses = var_clauses[1:]
            body = expr[2:]
            try:
                return TailCall(["begin"] + body, env)
            except SchemeError as exc:
                handler_env = Env(outer=env)
                handler_env[var] = str(exc)
                for clause in clauses:
                    if clause[0] == Symbol("else") or _scheme_truthy(self.eval_expr(clause[0], handler_env)):
                        return TailCall(["begin"] + clause[1:], handler_env)
                raise

        if head == Symbol("parameterize"):
            # (parameterize ((p v) ...) body ...)
            pairs = expr[1]
            body = expr[2:]
            saved = {}
            new_env = Env(outer=env)
            for p_expr, v_expr in pairs:
                param = self.eval_expr(p_expr, env)
                val = self.eval_expr(v_expr, env)
                if callable(param):
                    try:
                        saved[p_expr] = param()
                        # Apply new value
                    except Exception:
                        pass
                new_env[p_expr] = val
            return TailCall(["begin"] + body, new_env)

        if head == Symbol("quasiquote"):
            return self._quasiquote(expr[1], env)

        # Procedure call
        proc = self.eval_expr(head, env)
        args = [self.eval_expr(a, env) for a in expr[1:]]

        if callable(proc) and not isinstance(proc, SchemeProc):
            return proc(*args)

        if isinstance(proc, SchemeProc):
            call_env = Env(proc.params, args, outer=proc.env)
            body = proc.body
            for sub in body[:-1]:
                self.eval_expr(sub, call_env)
            return TailCall(body[-1], call_env)

        raise SchemeError(f"Not a procedure: {scheme_str(proc)}")

    def _quasiquote(self, expr, env):
        if isinstance(expr, list) and len(expr) > 0:
            if expr[0] == Symbol("unquote"):
                return self.eval_expr(expr[1], env)
            result = []
            for item in expr:
                if isinstance(item, list) and len(item) > 0 and item[0] == Symbol("unquote-splicing"):
                    result.extend(self.eval_expr(item[1], env))
                else:
                    result.append(self._quasiquote(item, env))
            return result
        return expr

    def _build_global_env(self) -> Env:
        env = Env()
        env.update({
            "+": lambda *a: sum(a),
            "-": lambda a, *b: -a if not b else a - sum(b),
            "*": lambda *a: _product(a),
            "/": lambda a, b: a / b,
            "quotient": lambda a, b: int(a / b),
            "remainder": lambda a, b: a % b,
            "modulo": lambda a, b: a % b,
            "abs": abs,
            "max": max,
            "min": min,
            "floor": math.floor,
            "ceiling": math.ceil,
            "round": round,
            "sqrt": math.sqrt,
            "expt": pow,
            "=": lambda a, b: a == b,
            "<": operator.lt,
            ">": operator.gt,
            "<=": operator.le,
            ">=": operator.ge,
            "not": lambda a: not _scheme_truthy(a),
            "eq?": lambda a, b: a is b or a == b,
            "equal?": lambda a, b: a == b,
            "eqv?": lambda a, b: a == b,
            "number?": lambda a: isinstance(a, (int, float)) and not isinstance(a, bool),
            "string?": lambda a: isinstance(a, str) and not isinstance(a, Symbol),
            "symbol?": lambda a: isinstance(a, Symbol),
            "boolean?": lambda a: isinstance(a, bool),
            "pair?": lambda a: isinstance(a, list) and len(a) > 0,
            "null?": lambda a: a == [] or a is NIL,
            "list?": lambda a: isinstance(a, list),
            "procedure?": lambda a: callable(a) or isinstance(a, SchemeProc),
            "car": lambda a: a[0] if isinstance(a, list) and a else (_ for _ in ()).throw(SchemeError("car: empty list")),
            "cdr": lambda a: a[1:] if isinstance(a, list) and a else (_ for _ in ()).throw(SchemeError("cdr: empty list")),
            "cons": lambda a, b: [a] + b if isinstance(b, list) else [a, b],
            "list": lambda *a: list(a),
            "append": lambda *a: [item for sub in a for item in (sub if isinstance(sub, list) else [sub])],
            "reverse": lambda a: list(reversed(a)),
            "length": lambda a: len(a),
            "map": lambda f, *lsts: [f(*args) for args in zip(*lsts)],
            "for-each": lambda f, *lsts: [f(*args) for args in zip(*lsts)] and None,
            "filter": lambda f, lst: [x for x in lst if _scheme_truthy(f(x))],
            "apply": lambda f, *a: f(*(list(a[-1]) if isinstance(a[-1], list) else [a[-1]])),
            "assoc": lambda k, lst: next((p for p in lst if isinstance(p, list) and p and p[0] == k), False),
            "assq": lambda k, lst: next((p for p in lst if isinstance(p, list) and p and p[0] is k), False),
            "member": lambda k, lst: (sub := [x for x in lst if x == k]) and sub or False,
            "memq": lambda k, lst: (sub := [x for x in lst if x is k]) and sub or False,
            "cadr": lambda a: a[1],
            "caddr": lambda a: a[2],
            "caar": lambda a: a[0][0],
            "cadar": lambda a: a[1][0],
            "number->string": lambda n, r=10: str(int(n)) if r == 10 else format(int(n), "b" if r == 2 else "o" if r == 8 else "x"),
            "string->number": lambda s, r=10: (int(s, r) if "." not in s else float(s)),
            "string->symbol": Symbol,
            "symbol->string": str,
            "string-length": len,
            "string-append": lambda *a: "".join(a),
            "string-ref": lambda s, i: s[i],
            "substring": lambda s, i, j=None: s[i:j],
            "string->list": lambda s: list(s),
            "list->string": lambda lst: "".join(lst),
            "string-upcase": str.upper,
            "string-downcase": str.lower,
            "string=?": lambda a, b: a == b,
            "string<?": lambda a, b: a < b,
            "char->integer": ord,
            "integer->char": chr,
            "vector": lambda *a: list(a),
            "make-vector": lambda n, v=0: [v] * n,
            "vector-ref": lambda v, i: v[i],
            "vector-set!": lambda v, i, x: v.__setitem__(i, x) or None,
            "vector-length": len,
            "vector->list": list,
            "list->vector": list,
            "display": lambda x: self._emit(scheme_display(x)) or None,
            "newline": lambda: self._emit("") or None,
            "write": lambda x: self._emit(scheme_str(x)) or None,
            "read": lambda: self.interpreter.request_input("scheme> ") if hasattr(self.interpreter, "request_input") else "",
            "error": lambda msg, *irritants: (_ for _ in ()).throw(SchemeError(str(msg) + " " + " ".join(map(scheme_str, irritants)))),
            "void": lambda: None,
            "#t": True,
            "#f": False,
            # ── Number predicates ─────────────────────────────────────────
            "zero?": lambda x: x == 0,
            "positive?": lambda x: x > 0,
            "negative?": lambda x: x < 0,
            "odd?": lambda x: int(x) % 2 != 0,
            "even?": lambda x: int(x) % 2 == 0,
            "exact?": lambda x: isinstance(x, int),
            "inexact?": lambda x: isinstance(x, float),
            "exact": int,
            "inexact": float,
            "exact->inexact": float,
            "inexact->exact": lambda x: int(x) if isinstance(x, float) and x.is_integer() else x,
            "integer?": lambda x: isinstance(x, (int, float)) and (isinstance(x, int) or float(x).is_integer()),
            "real?": lambda x: isinstance(x, (int, float)) and not isinstance(x, bool),
            "rational?": lambda x: isinstance(x, (int, float)) and not isinstance(x, bool),
            "complex?": lambda x: isinstance(x, (int, float)) and not isinstance(x, bool),
            "finite?": lambda x: not (isinstance(x, float) and (x != x or abs(x) == float("inf"))),
            "infinite?": lambda x: isinstance(x, float) and abs(x) == float("inf"),
            "nan?": lambda x: isinstance(x, float) and x != x,
            "gcd": lambda *a: (lambda lst: (lst[0] if len(lst) == 1 else __import__("math").gcd(int(lst[0]), int(_product(lst[1:])))))(list(a)) if a else 0,
            "lcm": lambda *a: (lambda lst: abs(int(lst[0]) * int(lst[1])) // __import__("math").gcd(abs(int(lst[0])), abs(int(lst[1]))))(list(a)) if len(list(a)) >= 2 else (abs(int(list(a)[0])) if a else 1),
            "floor/": lambda a, b: (a // b, a % b),
            "truncate/": lambda a, b: (int(a / b), a - int(a / b) * b),
            "floor-quotient": lambda a, b: a // b,
            "floor-remainder": lambda a, b: a % b,
            "truncate-quotient": lambda a, b: int(a / b),
            "truncate-remainder": lambda a, b: a - int(a / b) * b,
            "gcd": lambda *a: __import__("math").gcd(*[abs(int(x)) for x in a]) if a else 0,
            "lcm": lambda *a: round(abs(__import__("functools").reduce(lambda x,y: x*y//(__import__("math").gcd(x,y)), [abs(int(x)) for x in a]))) if a else 1,
            # ── Trig/math ─────────────────────────────────────────────────
            "sin": math.sin, "cos": math.cos, "tan": math.tan,
            "asin": math.asin, "acos": math.acos,
            "atan": lambda a, b=None: math.atan2(a, b) if b is not None else math.atan(a),
            "exp": math.exp, "log": lambda x, b=None: math.log(x, b) if b is not None else math.log(x),
            "floor": math.floor, "ceiling": math.ceil,
            "truncate": math.trunc,
            "square": lambda x: x * x,
            "exact-integer-sqrt": lambda x: (int(math.isqrt(int(x))), int(x) - int(math.isqrt(int(x)))**2),
            # ── Character predicates ───────────────────────────────────────
            "char?": lambda x: isinstance(x, str) and len(x) == 1,
            "char-alphabetic?": lambda c: str(c).isalpha(),
            "char-numeric?": lambda c: str(c).isdigit(),
            "char-whitespace?": lambda c: str(c).isspace(),
            "char-upper-case?": lambda c: str(c).isupper(),
            "char-lower-case?": lambda c: str(c).islower(),
            "char-upcase": lambda c: str(c).upper(),
            "char-downcase": lambda c: str(c).lower(),
            "char=?": lambda a, b: a == b,
            "char<?": lambda a, b: a < b,
            "char>?": lambda a, b: a > b,
            "char<=?": lambda a, b: a <= b,
            "char>=?": lambda a, b: a >= b,
            "char-ci=?": lambda a, b: a.lower() == b.lower(),
            "char-ci<?": lambda a, b: a.lower() < b.lower(),
            # ── More string functions ──────────────────────────────────────
            "make-string": lambda n, c=" ": c * int(n),
            "string": lambda *a: "".join(a),
            "string-copy": lambda s, i=0, j=None: s[int(i):j],
            "string-fill!": lambda s, c, i=0, j=None: c * len(s),
            "string-contains": lambda s, sub: (s.find(sub) if sub in s else False),
            "string-prefix?": lambda p, s: s.startswith(p),
            "string-suffix?": lambda suf, s: s.endswith(suf),
            "string-split": lambda s, sep=None: s.split(sep),
            "string-join": lambda lst, sep="": sep.join(lst),
            "string-trim": lambda s, *a: s.lstrip(),
            "string-trim-right": lambda s, *a: s.rstrip(),
            "string-replace": lambda s, r, i, j: s[:int(i)] + r + s[int(j):],
            "string-for-each": lambda f, *lsts: [f(*args) for args in zip(*lsts)] and None,
            "string-map": lambda f, *lsts: "".join(f(*args) for args in zip(*lsts)),
            "string->utf8": lambda s: list(s.encode("utf-8")),
            "utf8->string": lambda b: bytes(b).decode("utf-8"),
            "number->string": lambda n, r=10: str(int(n)) if isinstance(n, int) else (str(n) if r == 10 else format(int(n), "b" if r == 2 else "o" if r == 8 else "x")),
            "string->number": lambda s, r=10: (int(str(s), int(r)) if "." not in str(s) else float(str(s))),
            # ── More list/pair functions ───────────────────────────────────
            "list-tail": lambda lst, k: lst[int(k):],
            "list-ref": lambda lst, k: lst[int(k)],
            "list-copy": lambda lst: list(lst) if isinstance(lst, list) else lst,
            "list->set": lambda lst: list(dict.fromkeys(lst)),
            "iota": lambda count, start=0, step=1: [start + i * step for i in range(int(count))],
            "make-list": lambda n, fill=False: [fill] * int(n),
            "flatten": lambda lst: [item for sublist in lst for item in (sublist if isinstance(sublist, list) else [sublist])],
            "assv": lambda k, lst: next((p for p in lst if isinstance(p, list) and p and p[0] == k), False),
            "memv": lambda k, lst: (sub := [x for x in lst if x == k]) and sub or False,
            "last-pair": lambda lst: [lst[-1]] if lst else [],
            "list-head": lambda lst, k: lst[:int(k)],
            "zip": lambda *lsts: [list(x) for x in zip(*lsts)],
            "reduce": lambda f, init, lst: (lambda: __import__("functools").reduce(f, lst, init))(),
            "fold": lambda f, init, lst: (lambda: __import__("functools").reduce(lambda acc, x: f(x, acc), lst, init))(),
            "fold-right": lambda f, init, lst: (lambda: __import__("functools").reduce(lambda x, acc: f(x, acc), reversed(lst), init))(),
            "for-all": lambda f, lst: all(_scheme_truthy(f(x)) for x in lst),
            "exists": lambda f, lst: any(_scheme_truthy(f(x)) for x in lst),
            "partition": lambda f, lst: ([x for x in lst if _scheme_truthy(f(x))], [x for x in lst if not _scheme_truthy(f(x))]),
            "count": lambda f, lst: sum(1 for x in lst if _scheme_truthy(f(x))),
            "sort": lambda lst, cmp=None: sorted(lst, key=lambda x: x) if cmp is None else sorted(lst, key=lambda x: x),
            "vector-for-each": lambda f, *vecs: [f(*args) for args in zip(*vecs)] and None,
            "vector-map": lambda f, *vecs: [f(*args) for args in zip(*vecs)],
            "vector-append": lambda *vecs: [item for v in vecs for item in v],
            "vector-copy": lambda v, i=0, j=None: v[int(i):j],
            "vector-fill!": lambda v, fill, i=0, j=None: [v.__setitem__(k, fill) for k in range(int(i), j or len(v))] and None,
            "vector-copy!": lambda to, at, frm, start=0, end=None: [to.__setitem__(int(at)+k-int(start), frm[k]) for k in range(int(start), end or len(frm))] and None,
            # ── Port/IO functions ──────────────────────────────────────────
            "current-input-port": lambda: None,
            "current-output-port": lambda: None,
            "current-error-port": lambda: None,
            "input-port?": lambda x: x is None,
            "output-port?": lambda x: x is None,
            "port?": lambda x: x is None,
            "read-char": lambda *a: "",
            "peek-char": lambda *a: "",
            "char-ready?": lambda *a: True,
            "read-line": lambda *a: self.interpreter.request_input("") if hasattr(self.interpreter, "request_input") else "",
            "write-char": lambda c, *a: self._emit(c) or None,
            "write-string": lambda s, *a: self._emit(s) or None,
            "flush-output-port": lambda *a: None,
            "close-input-port": lambda *a: None,
            "close-output-port": lambda *a: None,
            "open-input-string": lambda s: s,
            "open-output-string": lambda: [],
            "get-output-string": lambda p: "".join(p) if isinstance(p, list) else str(p),
            "open-input-file": lambda f: f,
            "open-output-file": lambda f: f,
            "with-input-from-file": lambda f, thunk: thunk(),
            "with-output-to-file": lambda f, thunk: thunk(),
            # ── Multiple values ────────────────────────────────────────────
            "values": lambda *a: tuple(a) if len(a) != 1 else a[0],
            "call-with-values": lambda producer, consumer: consumer(*(producer() if isinstance(producer(), tuple) else [producer()])),
            # ── Misc ──────────────────────────────────────────────────────
            "gensym": lambda *a: Symbol(f"g{id(object())}"),
            "with-exception-handler": lambda h, thunk: thunk(),
            "raise": lambda x: (_ for _ in ()).throw(SchemeError(str(x))),
            "raise-continuable": lambda x: (_ for _ in ()).throw(SchemeError(str(x))),
            "error-message": lambda e: str(e),
            "condition/report-string": lambda e: str(e),
            "dynamic-wind": lambda before, thunk, after: (before(), thunk(), after())[1],
            "force": lambda promise: promise if not isinstance(promise, SchemeProc) else self.eval_expr([promise], env),
            "make-promise": lambda x: x,
            "promise?": lambda x: True,
            "make-parameter": lambda val, *a: (lambda v: lambda *nv: v if not nv else None)(val),
            "exit": lambda *a: None,
            "command-line": lambda: [],
            "get-environment-variable": lambda name: None,
            "get-environment-variables": lambda: [],
            "interaction-environment": lambda: self._build_global_env(),
            "scheme-report-environment": lambda n: self._build_global_env(),
            "the-environment": self._build_global_env,
            "load": lambda f: None,
            "eval": lambda expr, env_: self.eval_expr(expr, env_),
            "apply": lambda f, *a: f(*(list(a[-1]) if isinstance(a[-1], list) else [a[-1]])),
            # cXXr combinations
            "cddr": lambda a: a[2:],
            "caar": lambda a: a[0][0],
            "cadr": lambda a: a[1],
            "cdar": lambda a: a[0][1:],
            "caddr": lambda a: a[2],
            "cdddr": lambda a: a[3:],
            "caadr": lambda a: a[1][0],
            "cdadr": lambda a: a[1][1:],
            "caddr": lambda a: a[2],
            "cadar": lambda a: a[0][1],
            "caddr": lambda a: a[2],
            "cadddr": lambda a: a[3],
            "cddddr": lambda a: a[4:],
            "first": lambda a: a[0],
            "second": lambda a: a[1],
            "third": lambda a: a[2],
            "fourth": lambda a: a[3],
            "fifth": lambda a: a[4],
        })
        # Aliases
        env[Symbol("+")] = env["+"]
        env[Symbol("else")] = True
        return env


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _scheme_truthy(val) -> bool:
    return val is not False


def _product(args):
    r = 1
    for a in args:
        r *= a
    return r


def scheme_str(val) -> str:
    if val is True: return "#t"
    if val is False: return "#f"
    if val is None or val == []: return "()"
    if isinstance(val, str) and not isinstance(val, Symbol):
        return '"' + val + '"'
    if isinstance(val, list):
        return "(" + " ".join(scheme_str(v) for v in val) + ")"
    if isinstance(val, SchemeProc):
        return repr(val)
    return str(val)


def scheme_display(val) -> str:
    if isinstance(val, str) and not isinstance(val, Symbol):
        return val
    return scheme_str(val)


class SchemeError(Exception):
    pass


class SchemeSyntaxError(Exception):
    pass
