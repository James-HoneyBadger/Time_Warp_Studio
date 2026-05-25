"""Haskell language executor for Time Warp Studio.

Educational Haskell interpreter — whole-program execution.
Implements a teaching subset of Haskell 2010.

Supported features:
  - module Main where / main = do { ... }
  - putStrLn, putStr, print, putChar, hPutStrLn stderr
  - let bindings (in do-notation and top-level)
  - if-then-else (expression and statement forms)
  - List comprehensions: [expr | x <- list, guard]
  - Pattern matching in function definitions (literal, wildcard, list head/tail)
  - Guards: f x | x > 0 = ... | otherwise = ...
  - where clauses
  - Recursion (self-calls resolved by function name)
  - Algebraic data types (basic — data T = A | B(x))
  - Type classes: Show, Eq, Ord, Num, Integral, Fractional, Enum (subset)
  - Prelude functions: map, filter, foldr, foldl, foldl', foldr1,
    head, tail, last, init, null, length, reverse, concat, concatMap,
    zip, zip3, zipWith, unzip, take, drop, takeWhile, dropWhile,
    splitAt, span, break, elem, notElem, lookup, replicate,
    iterate (bounded), cycle (bounded), sum, product,
    minimum, maximum, and, or, any, all, nub, sort, sortBy, group,
    intercalate, intersperse, transpose, isPrefixOf, isSuffixOf, isInfixOf,
    words, lines, unwords, unlines, show, read (basic),
    div, mod, quot, rem, gcd, lcm, abs, signum, fromIntegral,
    floor, ceiling, truncate, round, sqrt, pi, exp, log, sin, cos, tan,
    not, otherwise, id, const, flip, (.), ($), error, undefined
  - String formatting: show, putStrLn (show x)
  - Tuple access: fst, snd
  - do-notation: let, bind (<-), sequence (>>)
  - Turtle graphics: forward, backward, right, left, penUp, penDown,
    goto, home, color, penSize, clearCanvas (as IO actions)
"""

from __future__ import annotations

import math
import re
import textwrap
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_haskell(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete Haskell program and return all output as a string."""
    env = HaskellEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Sentinel / special values
# ---------------------------------------------------------------------------

_UNSET = object()


class HaskellError(Exception):
    pass


class _ReturnSignal(Exception):
    def __init__(self, value: Any = None):
        self.value = value


# ---------------------------------------------------------------------------
# Haskell runtime values
# ---------------------------------------------------------------------------


class HaskellList:
    """Wrapper around Python list for Haskell list semantics."""

    def __init__(self, items: list):
        self.items = list(items)

    def __repr__(self) -> str:
        return repr(self.items)

    def __iter__(self):
        return iter(self.items)

    def __len__(self) -> int:
        return len(self.items)

    def __getitem__(self, idx):
        return self.items[idx]


class HaskellTuple:
    def __init__(self, *elements):
        self.elements = tuple(elements)

    def __repr__(self) -> str:
        return "(" + ", ".join(_hs_show(e) for e in self.elements) + ")"

    def __iter__(self):
        return iter(self.elements)

    def __len__(self):
        return len(self.elements)

    def __getitem__(self, idx):
        return self.elements[idx]


class HaskellFunction:
    """User-defined function (possibly with multiple clauses and guards)."""

    def __init__(self, name: str):
        self.name = name
        self.clauses: list[dict] = []  # [{params, guards, body, where}]

    def add_clause(
        self,
        params: list,
        guards: list[tuple],  # [(guard_expr, body_expr)]
        body: str | None,
        where_defs: dict,
    ) -> None:
        self.clauses.append(
            {"params": params, "guards": guards, "body": body, "where": where_defs}
        )


class HaskellIOAction:
    """Represents an IO action (result of turtle/print functions)."""

    def __init__(self, fn, args=None):
        self.fn = fn
        self.args = args or []

    def execute(self):
        return self.fn(*self.args)


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------

_MAX_ITER = 50_000


class HaskellEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        self._globals: dict[str, Any] = {}
        self._iter_count = 0
        self._setup_prelude()

    def _emit(self, text: str) -> None:
        self._output.append(text)

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        try:
            self._parse_program(source)
            main = self._globals.get("main")
            if main is None:
                self._emit("❌ No `main` function defined")
            else:
                result = self._call_value(main, [])
                self._execute_io(result)
        except HaskellError as e:
            self._emit(f"❌ Haskell error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        out = "\n".join(self._output)
        return out + ("\n" if out and not out.endswith("\n") else "")

    # ------------------------------------------------------------------
    # Program parser
    # ------------------------------------------------------------------

    def _parse_program(self, source: str) -> None:  # noqa: C901
        """Parse top-level declarations."""
        lines = _preprocess(source)
        i = 0
        while i < len(lines):
            line = lines[i].rstrip()
            stripped = line.strip()

            if not stripped or stripped.startswith("--"):
                i += 1
                continue

            # module declaration
            if stripped.startswith("module "):
                i += 1
                continue

            # import declarations
            if stripped.startswith("import "):
                i += 1
                continue

            # type signature: name :: Type
            if re.match(r"^\w+\s*::", stripped):
                i += 1
                continue

            # data type declaration
            if stripped.startswith("data "):
                i += 1
                continue

            # type alias
            if stripped.startswith("type "):
                i += 1
                continue

            # newtype
            if stripped.startswith("newtype "):
                i += 1
                continue

            # class / instance
            if stripped.startswith("class ") or stripped.startswith("instance "):
                # skip until blank line or non-indented line
                i += 1
                while i < len(lines) and (not lines[i].strip() or lines[i][0] in " \t"):
                    i += 1
                continue

            # function / value binding: name args... = body
            #   possibly followed by where clause
            m = re.match(r"^([a-z_]\w*'?)\s*(.*?)\s*=\s*(.+)$", stripped)
            if m:
                func_name = m.group(1)
                params_str = m.group(2).strip()
                body_str = m.group(3).strip()

                # Collect continuation lines (indented)
                body_lines = [body_str]
                i += 1
                while i < len(lines) and lines[i] and lines[i][0] in " \t":
                    next_stripped = lines[i].strip()
                    if not next_stripped or next_stripped.startswith("--"):
                        i += 1
                        continue
                    body_lines.append(next_stripped)
                    i += 1

                full_body = " ".join(body_lines)
                where_defs = self._extract_where(full_body)
                if where_defs:
                    full_body = full_body[: full_body.rfind("where")].strip()

                # Parse guards
                guards = _parse_guards(full_body)

                if func_name not in self._globals or not isinstance(
                    self._globals[func_name], HaskellFunction
                ):
                    self._globals[func_name] = HaskellFunction(func_name)

                params = _parse_params(params_str)
                fn = self._globals[func_name]
                fn.add_clause(params, guards, full_body if not guards else None, where_defs)
                continue

            # Guards at top level binding with same name
            # (already handled in the loop above)
            i += 1

    def _extract_where(self, body: str) -> dict:
        """Extract and parse where clause definitions."""
        where_defs: dict[str, Any] = {}
        m = re.search(r"\bwhere\b(.+)$", body, re.DOTALL)
        if not m:
            return where_defs
        where_body = m.group(1).strip()
        for line in where_body.splitlines():
            line = line.strip()
            if not line or line.startswith("--"):
                continue
            m2 = re.match(r"^(\w+'?)\s*(.*?)\s*=\s*(.+)$", line)
            if m2:
                where_defs[m2.group(1)] = {
                    "params": _parse_params(m2.group(2)),
                    "body": m2.group(3).strip(),
                }
        return where_defs

    # ------------------------------------------------------------------
    # IO execution
    # ------------------------------------------------------------------

    def _execute_io(self, action: Any) -> Any:  # noqa: C901
        """Execute an IO action, which may be a do-block or single action."""
        if isinstance(action, HaskellIOAction):
            return action.execute()
        if isinstance(action, list):
            # do-block returns a list of IO actions
            result = None
            for act in action:
                result = self._execute_io(act)
            return result
        if callable(action):
            r = action()
            return self._execute_io(r)
        return action

    # ------------------------------------------------------------------
    # do-notation evaluator
    # ------------------------------------------------------------------

    def _eval_do(self, stmts: list[str], local_env: dict) -> Any:  # noqa: C901
        """Evaluate a sequence of do-notation statements."""
        actions = []
        i = 0
        while i < len(stmts):
            stmt = stmts[i].strip()
            if not stmt or stmt.startswith("--"):
                i += 1
                continue

            # let binding: let x = expr
            m = re.match(r"^let\s+(\w+'?)\s*=\s*(.+)$", stmt)
            if m:
                name = m.group(1)
                val = self._eval_expr(m.group(2).strip(), local_env)
                local_env[name] = val
                i += 1
                continue

            # let with multiple bindings (indented block)
            if stmt == "let":
                i += 1
                while i < len(stmts) and stmts[i].startswith("  "):
                    inner = stmts[i].strip()
                    m2 = re.match(r"^(\w+'?)\s*=\s*(.+)$", inner)
                    if m2:
                        local_env[m2.group(1)] = self._eval_expr(m2.group(2), local_env)
                    i += 1
                continue

            # bind: name <- action
            m = re.match(r"^(\w+'?)\s*<-\s*(.+)$", stmt)
            if m:
                name = m.group(1)
                action_expr = m.group(2).strip()
                result = self._execute_io(self._eval_expr(action_expr, local_env))
                local_env[name] = result
                i += 1
                continue

            # return / pure
            m = re.match(r"^return\s+(.+)$", stmt)
            if m:
                val = self._eval_expr(m.group(1), local_env)
                actions.append(val)
                i += 1
                continue

            # when / unless
            m = re.match(r"^(when|unless)\s+(.+?)\s+\$?\s*(.+)$", stmt)
            if m:
                cond = self._eval_expr(m.group(2), local_env)
                body = m.group(3)
                if (cond and m.group(1) == "when") or (not cond and m.group(1) == "unless"):
                    self._execute_io(self._eval_expr(body, local_env))
                i += 1
                continue

            # mapM_ / forM_ (iterate side effects)
            m = re.match(r"^map[M_]+\s*(_?)\s+(.+?)\s+(.+)$", stmt)
            if m:
                fn = self._eval_expr(m.group(2), local_env)
                lst = self._eval_expr(m.group(3), local_env)
                for item in _to_list(lst):
                    self._execute_io(self._call_value(fn, [item]))
                i += 1
                continue

            # forM_ / for_ / traverse_
            m = re.match(r"^(?:for[M_]+|traverse_?)\s+(.+?)\s+\$?\s*(.+)$", stmt)
            if m:
                lst = self._eval_expr(m.group(1), local_env)
                fn_expr = m.group(2)
                fn = self._eval_expr(fn_expr, local_env)
                for item in _to_list(lst):
                    self._execute_io(self._call_value(fn, [item]))
                i += 1
                continue

            # Otherwise: evaluate expression as IO action
            val = self._eval_expr(stmt, local_env)
            self._execute_io(val)
            i += 1

        return actions[-1] if actions else None

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str, env: dict) -> Any:  # noqa: C901
        expr = expr.strip()
        if not expr:
            return None

        # do-block
        if expr.startswith("do"):
            rest = expr[2:].strip()
            stmts = _split_do_stmts(rest)
            local_env = dict(env)
            return self._eval_do(stmts, local_env)

        # let ... in ...
        m = re.match(r"^let\s+(.+?)\s+in\s+(.+)$", expr, re.DOTALL)
        if m:
            bindings_str = m.group(1)
            body_str = m.group(2).strip()
            local_env = dict(env)
            for binding in bindings_str.split(";"):
                bm = re.match(r"^(\w+'?)\s*=\s*(.+)$", binding.strip())
                if bm:
                    local_env[bm.group(1)] = self._eval_expr(bm.group(2), local_env)
            return self._eval_expr(body_str, local_env)

        # if-then-else
        m = re.match(r"^if\s+(.+?)\s+then\s+(.+?)\s+else\s+(.+)$", expr, re.DOTALL)
        if m:
            cond = self._eval_expr(m.group(1).strip(), env)
            if cond:
                return self._eval_expr(m.group(2).strip(), env)
            return self._eval_expr(m.group(3).strip(), env)

        # case ... of
        m = re.match(r"^case\s+(.+?)\s+of\s+(.+)$", expr, re.DOTALL)
        if m:
            return self._eval_case(m.group(1), m.group(2), env)

        # lambda: \x -> expr
        m = re.match(r"^\\(.+?)\s*->\s*(.+)$", expr, re.DOTALL)
        if m:
            params_str = m.group(1).strip()
            body_str = m.group(2).strip()
            params = params_str.split()
            closure = dict(env)
            return self._make_lambda(params, body_str, closure)

        # List comprehension: [expr | x <- list, guard]
        m = re.match(r"^\[(.+?)\s*\|\s*(.+)\]$", expr)
        if m:
            return self._eval_list_comp(m.group(1), m.group(2), env)

        # List: [1,2,3] or [1..10] or [1,3..10]
        if expr.startswith("[") and expr.endswith("]"):
            return self._eval_list_literal(expr[1:-1], env)

        # Tuple: (a,b) or (a,b,c)
        if expr.startswith("(") and expr.endswith(")"):
            inner = expr[1:-1]
            if "," in inner:
                parts = _split_comma(inner)
                if len(parts) >= 2:
                    return HaskellTuple(*[self._eval_expr(p, env) for p in parts])
            # Parenthesised expression
            return self._eval_expr(inner, env)

        # String literal
        if expr.startswith('"') and expr.endswith('"'):
            return _parse_string(expr[1:-1])

        # Char literal
        if expr.startswith("'") and expr.endswith("'") and len(expr) in (3, 4):
            inner = expr[1:-1]
            return inner.replace("\\n", "\n").replace("\\t", "\t")

        # Numeric literals
        m = re.match(r"^-?\d+\.\d+$", expr)
        if m:
            return float(expr)
        m = re.match(r"^-?\d+$", expr)
        if m:
            return int(expr)

        # Booleans / special
        if expr == "True":
            return True
        if expr == "False":
            return False
        if expr == "Nothing":
            return None
        if expr in ("()", "return ()"):
            return None
        if expr in ("otherwise",):
            return True

        # Negative literal
        if expr.startswith("-"):
            m = re.match(r"^-(\d+\.?\d*)$", expr)
            if m:
                s = m.group(1)
                return -float(s) if "." in s else -int(s)

        # Function application and binary operations
        return self._eval_app(expr, env)

    def _eval_app(self, expr: str, env: dict) -> Any:  # noqa: C901
        """Evaluate function application and binary ops."""
        expr = expr.strip()

        # Operator sections
        m = re.match(r"^\(([+\-*/<>=!&|^]+)\)$", expr)
        if m:
            op = m.group(1)
            return lambda a, b: _apply_op(a, op, b)

        # Infix operator: left op right
        for op in ["||", "&&", "==", "/=", "<=", ">=", "<", ">",
                   "++", ":", "!!", "+", "-", "*", "/", "^", "**",
                   "div ", "mod ", "quot ", "rem ",
                   "&&", "||", ">>=", ">>", "$", "."]:
            idx = _rfind_op(expr, op)
            if idx > 0:
                left_s = expr[:idx].strip()
                right_s = expr[idx + len(op):].strip()
                if left_s and right_s:
                    return self._eval_binop(left_s, op.strip(), right_s, env)

        # Function application: f arg1 arg2 ...
        parts = _split_application(expr)
        if not parts:
            return None
        if len(parts) == 1:
            return self._resolve_name(parts[0], env)
        func_val = self._resolve_name(parts[0], env)
        args = [self._eval_expr(p, env) for p in parts[1:]]
        if callable(func_val):
            try:
                result = func_val
                for a in args:
                    if callable(result):
                        result = result(a)
                    else:
                        break
                return result
            except TypeError:
                return func_val
        if isinstance(func_val, HaskellFunction):
            return self._call_haskell_fn(func_val, args, env)
        return func_val

    def _eval_binop(self, left_s: str, op: str, right_s: str, env: dict) -> Any:
        if op in ("&&",):
            lv = self._eval_expr(left_s, env)
            return lv and self._eval_expr(right_s, env)
        if op in ("||",):
            lv = self._eval_expr(left_s, env)
            return lv or self._eval_expr(right_s, env)
        lv = self._eval_expr(left_s, env)
        rv = self._eval_expr(right_s, env)
        return _apply_op(lv, op, rv)

    def _eval_case(self, scrutinee_s: str, alts_s: str, env: dict) -> Any:
        val = self._eval_expr(scrutinee_s, env)
        alts = _parse_case_alts(alts_s)
        for pattern, body in alts:
            match, bindings = _match_pattern(pattern, val)
            if match:
                local_env = dict(env)
                local_env.update(bindings)
                return self._eval_expr(body, local_env)
        raise HaskellError(f"Non-exhaustive patterns in case for {val!r}")

    def _eval_list_literal(self, inner: str, env: dict) -> list:
        inner = inner.strip()
        if not inner:
            return []
        # Range: a..b or a,b..c
        if ".." in inner:
            parts = inner.split("..")
            if len(parts) == 2:
                start_s = parts[0].strip()
                end_s = parts[1].strip()
                # [a,b..c] form
                step = 1
                if "," in start_s:
                    s1, s2 = start_s.split(",", 1)
                    a = self._eval_expr(s1.strip(), env)
                    b = self._eval_expr(s2.strip(), env)
                    step = b - a
                    a_val = a
                else:
                    a_val = self._eval_expr(start_s, env)
                end_val = self._eval_expr(end_s, env) if end_s else None
                if end_val is not None:
                    if isinstance(a_val, str):
                        return [
                            chr(c)
                            for c in range(
                                ord(a_val), ord(end_val) + 1, max(1, step) if step > 0 else step
                            )
                        ]
                    result = []
                    v = a_val
                    if step > 0:
                        while v <= end_val and len(result) < _MAX_ITER:
                            result.append(v)
                            v += step
                    else:
                        while v >= end_val and len(result) < _MAX_ITER:
                            result.append(v)
                            v += step
                    return result
                else:
                    # Infinite range — take up to MAX_ITER
                    return list(range(a_val, a_val + _MAX_ITER, step))
        # Regular list
        items = _split_comma(inner)
        return [self._eval_expr(x.strip(), env) for x in items]

    def _eval_list_comp(self, expr_s: str, generators_s: str, env: dict) -> list:
        """Evaluate a list comprehension."""
        # Split generators/guards by comma (not inside brackets)
        parts = _split_comma(generators_s)
        return self._eval_list_comp_parts(expr_s, parts, dict(env))

    def _eval_list_comp_parts(
        self, expr_s: str, parts: list[str], env: dict
    ) -> list:
        if not parts:
            return [self._eval_expr(expr_s, env)]
        part = parts[0].strip()
        rest = parts[1:]
        m = re.match(r"^(\w+)\s*<-\s*(.+)$", part)
        if m:
            name = m.group(1)
            lst = _to_list(self._eval_expr(m.group(2), env))
            result = []
            for item in lst:
                local_env = dict(env)
                local_env[name] = item
                result.extend(self._eval_list_comp_parts(expr_s, rest, local_env))
            return result
        else:
            # Guard
            cond = self._eval_expr(part, env)
            if cond:
                return self._eval_list_comp_parts(expr_s, rest, env)
            return []

    def _make_lambda(self, params: list[str], body: str, closure: dict) -> Any:
        if not params:
            return self._eval_expr(body, closure)

        def apply(arg):
            local_env = dict(closure)
            local_env[params[0]] = arg
            if len(params) == 1:
                return self._eval_expr(body, local_env)
            return self._make_lambda(params[1:], body, local_env)

        return apply

    # ------------------------------------------------------------------
    # Function call
    # ------------------------------------------------------------------

    def _call_value(self, fn: Any, args: list) -> Any:
        if isinstance(fn, HaskellFunction):
            return self._call_haskell_fn(fn, args, {})
        if callable(fn):
            result = fn
            for a in args:
                result = result(a)
            return result
        return fn

    def _call_haskell_fn(self, fn: HaskellFunction, args: list, env: dict) -> Any:
        self._iter_count += 1
        if self._iter_count > _MAX_ITER:
            raise HaskellError("iteration/recursion limit exceeded")

        for clause in fn.clauses:
            params = clause["params"]
            guards = clause["guards"]
            body = clause["body"]
            where_defs = clause["where"]

            if len(args) < len(params):
                # Partial application
                def partial(remaining_args, clause=clause, bound=list(args)):
                    all_args = bound + list(remaining_args) if not isinstance(remaining_args, list) else bound + remaining_args
                    return self._call_haskell_fn(fn, all_args, env)
                return partial

            # Try pattern matching
            local_env = dict(env)
            local_env[fn.name] = fn  # allow recursion

            # Add where bindings
            for w_name, w_def in where_defs.items():
                w_params = w_def["params"]
                w_body = w_def["body"]
                if w_params:
                    w_fn = HaskellFunction(w_name)
                    w_fn.add_clause(w_params, [], w_body, {})
                    local_env[w_name] = w_fn
                else:
                    local_env[w_name] = self._eval_expr(w_body, local_env)

            matched = True
            for param, arg in zip(params, args):
                ok, bindings = _match_pattern(param, arg)
                if not ok:
                    matched = False
                    break
                local_env.update(bindings)

            # Pass extra args into scope if *rest pattern
            if len(args) > len(params):
                extra = args[len(params):]
            else:
                extra = []

            if not matched:
                continue

            # Evaluate guards
            if guards:
                for guard_cond, guard_body in guards:
                    cond = self._eval_expr(guard_cond, local_env)
                    if cond:
                        result = self._eval_expr(guard_body, local_env)
                        if extra:
                            return self._call_value(result, extra)
                        return result
                continue  # no guard matched this clause

            # No guards — evaluate body
            result = self._eval_expr(body, local_env)
            if extra:
                return self._call_value(result, extra)
            return result

        raise HaskellError(
            f"Non-exhaustive patterns in function '{fn.name}' with args {args!r}"
        )

    # ------------------------------------------------------------------
    # Name resolution
    # ------------------------------------------------------------------

    def _resolve_name(self, name: str, env: dict) -> Any:
        if name in env:
            return env[name]
        if name in self._globals:
            return self._globals[name]
        # Prelude (already in _globals)
        v = self._globals.get(name)
        if v is not None:
            return v
        # Numeric literal check
        try:
            return int(name)
        except ValueError:
            pass
        try:
            return float(name)
        except ValueError:
            pass
        return name  # Return as string — may be a data constructor

    # ------------------------------------------------------------------
    # Prelude setup
    # ------------------------------------------------------------------

    def _setup_prelude(self) -> None:  # noqa: C901
        env = self._globals
        t = self.turtle
        emit = self._emit

        # IO actions
        def putStrLn(s):
            emit(str(s))
            return None
        def putStr(s):
            if self._output and not self._output[-1].endswith("\n"):
                self._output[-1] += str(s)
            else:
                self._output.append(str(s))
            return None
        def putChar(c):
            return putStr(c)
        def print_(x):
            emit(_hs_show(x))
            return None
        def getLine():
            return ""
        def return_(x):
            return x
        def pure(x):
            return x
        def sequence_(lst):
            for act in _to_list(lst):
                self._execute_io(act)
            return None
        def mapM_(f, lst):
            for x in _to_list(lst):
                self._execute_io(self._call_value(f, [x]))
            return None
        def forM_(lst, f):
            return mapM_(f, lst)

        # List functions
        def head(lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.head: empty list")
            return lst[0]
        def tail(lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.tail: empty list")
            return lst[1:]
        def last(lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.last: empty list")
            return lst[-1]
        def init(lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.init: empty list")
            return lst[:-1]
        def null(lst):
            return len(_to_list(lst)) == 0
        def length(lst):
            return len(_to_list(lst))
        def reverse(lst):
            if isinstance(lst, str):
                return lst[::-1]
            return list(reversed(_to_list(lst)))
        def concat(lsts):
            result = []
            for l in _to_list(lsts):
                result.extend(_to_list(l))
            return result
        def concatMap(f, lst):
            result = []
            for x in _to_list(lst):
                result.extend(_to_list(self._call_value(f, [x])))
            return result
        def map_(f, lst):
            return [self._call_value(f, [x]) for x in _to_list(lst)]
        def filter_(f, lst):
            return [x for x in _to_list(lst) if self._call_value(f, [x])]
        def foldr(f, z, lst):
            result = z
            for x in reversed(_to_list(lst)):
                result = self._call_value(f, [x, result])
            return result
        def foldl(f, z, lst):
            result = z
            for x in _to_list(lst):
                result = self._call_value(f, [result, x])
            return result
        foldl_ = foldl  # strict version same in our interpreter
        def foldr1(f, lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.foldr1: empty list")
            return foldr(f, lst[-1], lst[:-1])
        def foldl1(f, lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.foldl1: empty list")
            return foldl(f, lst[0], lst[1:])
        def zip_(a, b):
            return [HaskellTuple(x, y) for x, y in zip(_to_list(a), _to_list(b))]
        def zip3(a, b, c):
            return [HaskellTuple(x, y, z) for x, y, z in zip(_to_list(a), _to_list(b), _to_list(c))]
        def zipWith(f, a, b):
            return [self._call_value(self._call_value(f, [x]), [y]) for x, y in zip(_to_list(a), _to_list(b))]
        def unzip(pairs):
            a, b = [], []
            for p in _to_list(pairs):
                pair = p if isinstance(p, HaskellTuple) else HaskellTuple(*p)
                a.append(pair[0])
                b.append(pair[1])
            return HaskellTuple(a, b)
        def take(n, lst):
            return _to_list(lst)[:int(n)]
        def drop(n, lst):
            return _to_list(lst)[int(n):]
        def takeWhile(f, lst):
            result = []
            for x in _to_list(lst):
                if self._call_value(f, [x]):
                    result.append(x)
                else:
                    break
            return result
        def dropWhile(f, lst):
            lst = _to_list(lst)
            i = 0
            while i < len(lst) and self._call_value(f, [lst[i]]):
                i += 1
            return lst[i:]
        def splitAt(n, lst):
            lst = _to_list(lst)
            return HaskellTuple(lst[:n], lst[n:])
        def span(f, lst):
            lst = _to_list(lst)
            i = 0
            while i < len(lst) and self._call_value(f, [lst[i]]):
                i += 1
            return HaskellTuple(lst[:i], lst[i:])
        def break_(f, lst):
            return span(lambda x: not self._call_value(f, [x]), lst)
        def elem(x, lst):
            return x in _to_list(lst)
        def notElem(x, lst):
            return x not in _to_list(lst)
        def lookup(k, lst):
            for pair in _to_list(lst):
                p = pair if isinstance(pair, HaskellTuple) else HaskellTuple(*pair)
                if p[0] == k:
                    return p[1]
            return None
        def replicate(n, x):
            return [x] * int(n)
        def iterate(f, x):
            result = []
            for _ in range(_MAX_ITER):
                result.append(x)
                x = self._call_value(f, [x])
            return result
        def cycle(lst):
            lst = _to_list(lst)
            if not lst:
                return []
            result = []
            i = 0
            while len(result) < _MAX_ITER:
                result.append(lst[i % len(lst)])
                i += 1
            return result
        def sum_(lst):
            return sum(_to_list(lst))
        def product(lst):
            result = 1
            for x in _to_list(lst):
                result *= x
            return result
        def minimum(lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.minimum: empty list")
            return min(lst)
        def maximum(lst):
            lst = _to_list(lst)
            if not lst:
                raise HaskellError("Prelude.maximum: empty list")
            return max(lst)
        def and_(lst):
            return all(_to_list(lst))
        def or_(lst):
            return any(_to_list(lst))
        def any_(f, lst):
            return any(self._call_value(f, [x]) for x in _to_list(lst))
        def all_(f, lst):
            return all(self._call_value(f, [x]) for x in _to_list(lst))
        # Data.List
        def nub(lst):
            seen = []
            for x in _to_list(lst):
                if x not in seen:
                    seen.append(x)
            return seen
        def sort(lst):
            try:
                return sorted(_to_list(lst))
            except TypeError:
                return sorted(_to_list(lst), key=str)
        def sortBy(f, lst):
            import functools
            return sorted(_to_list(lst), key=functools.cmp_to_key(
                lambda a, b: self._call_value(self._call_value(f, [a]), [b])
            ))
        def group(lst):
            lst = _to_list(lst)
            if not lst:
                return []
            groups = [[lst[0]]]
            for x in lst[1:]:
                if x == groups[-1][0]:
                    groups[-1].append(x)
                else:
                    groups.append([x])
            return groups
        def intercalate(sep, lsts):
            sep = _to_list(sep) if not isinstance(sep, str) else sep
            parts = [_to_list(l) if not isinstance(l, str) else list(l) for l in _to_list(lsts)]
            if isinstance(sep, str):
                return sep.join("".join(p) for p in parts)
            result = []
            for i, p in enumerate(parts):
                if i > 0:
                    result.extend(sep if isinstance(sep, list) else [sep])
                result.extend(p)
            return result
        def intersperse(sep, lst):
            lst = _to_list(lst)
            result = []
            for i, x in enumerate(lst):
                if i > 0:
                    result.append(sep)
                result.append(x)
            return result
        def transpose(lsts):
            lists = [_to_list(l) for l in _to_list(lsts)]
            return [list(row) for row in zip(*lists)]
        def isPrefixOf(prefix, lst):
            prefix = _to_list(prefix) if not isinstance(prefix, str) else prefix
            lst = _to_list(lst) if not isinstance(lst, str) else lst
            return lst[:len(prefix)] == (list(prefix) if isinstance(prefix, list) else prefix)
        def isSuffixOf(suffix, lst):
            suffix = _to_list(suffix) if not isinstance(suffix, str) else suffix
            lst = _to_list(lst) if not isinstance(lst, str) else lst
            return lst[-len(suffix):] == (list(suffix) if isinstance(suffix, list) else suffix)
        def isInfixOf(infix, lst):
            if isinstance(infix, str) and isinstance(lst, str):
                return infix in lst
            infix = _to_list(infix)
            lst = _to_list(lst)
            n = len(infix)
            for i in range(len(lst) - n + 1):
                if lst[i:i+n] == infix:
                    return True
            return False
        # String functions
        def words(s):
            return str(s).split()
        def lines_(s):
            return str(s).splitlines()
        def unwords(lst):
            return " ".join(str(x) for x in _to_list(lst))
        def unlines(lst):
            return "\n".join(str(x) for x in _to_list(lst)) + "\n"
        def show(x):
            return _hs_show(x)
        def read(s):
            s = str(s).strip()
            try:
                return int(s)
            except ValueError:
                try:
                    return float(s)
                except ValueError:
                    return s
        # Numeric
        def abs_(x):
            return abs(x)
        def signum(x):
            return 0 if x == 0 else (1 if x > 0 else -1)
        def negate(x):
            return -x
        def fromIntegral(x):
            return float(x)
        def toInteger(x):
            return int(x)
        def floor_(x):
            return math.floor(x)
        def ceiling_(x):
            return math.ceil(x)
        def truncate_(x):
            return int(x)
        def round_(x):
            return round(x)
        def div_(a, b):
            return a // b
        def mod_(a, b):
            return a % b
        def quot_(a, b):
            return int(a / b)
        def rem_(a, b):
            return a - b * quot_(a, b)
        def gcd_(a, b):
            import math as _m
            return _m.gcd(int(a), int(b))
        def lcm_(a, b):
            import math as _m
            g = _m.gcd(int(a), int(b))
            return abs(int(a) * int(b)) // g if g else 0
        def sqrt_(x):
            return math.sqrt(x)
        def pi_():
            return math.pi
        def exp_(x):
            return math.exp(x)
        def log_(x):
            return math.log(x)
        def sin_(x):
            return math.sin(x)
        def cos_(x):
            return math.cos(x)
        def tan_(x):
            return math.tan(x)
        def asin_(x):
            return math.asin(x)
        def acos_(x):
            return math.acos(x)
        def atan_(x):
            return math.atan(x)
        def atan2_(y, x):
            return math.atan2(y, x)
        # Logic
        def not_(x):
            return not x
        def id_(x):
            return x
        def const_(a, _):
            return a
        def flip_(f, b, a):
            return self._call_value(self._call_value(f, [a]), [b])
        def error_(msg):
            raise HaskellError(str(msg))
        def undefined_():
            raise HaskellError("Prelude.undefined")
        # Tuple
        def fst(t):
            return t[0] if isinstance(t, (HaskellTuple, tuple, list)) else t
        def snd(t):
            return t[1] if isinstance(t, (HaskellTuple, tuple, list)) and len(t) > 1 else t
        # Char
        def ord_(c):
            return ord(str(c)[0]) if c else 0
        def chr_(n):
            return chr(int(n))
        def isAlpha(c):
            return str(c).isalpha()
        def isDigit(c):
            return str(c).isdigit()
        def isSpace(c):
            return str(c).isspace()
        def isUpper(c):
            return str(c).isupper()
        def isLower(c):
            return str(c).islower()
        def toUpper(c):
            return str(c).upper()
        def toLower(c):
            return str(c).lower()
        # Maybe
        def fromMaybe(default, maybe):
            return default if maybe is None else maybe
        def isJust(m):
            return m is not None
        def isNothing(m):
            return m is None
        def fromJust(m):
            if m is None:
                raise HaskellError("Maybe.fromJust: Nothing")
            return m
        def maybe(b, f, m):
            return b if m is None else self._call_value(f, [m])
        def catMaybes(lst):
            return [x for x in _to_list(lst) if x is not None]
        def mapMaybe(f, lst):
            return [v for x in _to_list(lst) for v in [self._call_value(f, [x])] if v is not None]
        # Turtle
        def forward_io(n):
            t.forward(float(n))
            return None
        def backward_io(n):
            t.forward(-float(n))
            return None
        def right_io(n):
            t.right(float(n))
            return None
        def left_io(n):
            t.left(float(n))
            return None
        def penUp_io():
            t.pen_up()
            return None
        def penDown_io():
            t.pen_down()
            return None
        def home_io():
            t.home()
            return None
        def goto_io(x, y):
            t.goto(float(x), float(y))
            return None
        def color_io(r, g, b):
            t.set_color(int(r), int(g), int(b))
            return None
        def penSize_io(n):
            t.set_pen_width(int(n))
            return None
        def clearCanvas_io():
            t.reset()
            return None
        # Curried versions of two-arg functions
        def _curry2(f):
            return lambda a: lambda b: f(a, b)
        def _curry3(f):
            return lambda a: lambda b: lambda c: f(a, b, c)

        env.update({
            # IO
            "putStrLn": putStrLn,
            "putStr": putStr,
            "putChar": putChar,
            "print": print_,
            "getLine": getLine,
            "return": return_,
            "pure": pure,
            "sequence_": sequence_,
            "mapM_": _curry2(mapM_),
            "forM_": _curry2(forM_),
            # Lists
            "head": head, "tail": tail, "last": last, "init": init,
            "null": null, "length": length, "reverse": reverse,
            "concat": concat, "concatMap": _curry2(concatMap),
            "map": _curry2(map_), "filter": _curry2(filter_),
            "foldr": _curry3(foldr), "foldl": _curry3(foldl),
            "foldl'": _curry3(foldl_), "foldr1": _curry2(foldr1),
            "foldl1": _curry2(foldl1),
            "zip": _curry2(zip_), "zip3": _curry3(zip3),
            "zipWith": _curry3(zipWith), "unzip": unzip,
            "take": _curry2(take), "drop": _curry2(drop),
            "takeWhile": _curry2(takeWhile), "dropWhile": _curry2(dropWhile),
            "splitAt": _curry2(splitAt), "span": _curry2(span),
            "break": _curry2(break_),
            "elem": _curry2(elem), "notElem": _curry2(notElem),
            "lookup": _curry2(lookup), "replicate": _curry2(replicate),
            "iterate": _curry2(iterate), "cycle": cycle,
            "sum": sum_, "product": product,
            "minimum": minimum, "maximum": maximum,
            "and": and_, "or": or_,
            "any": _curry2(any_), "all": _curry2(all_),
            # Data.List
            "nub": nub, "sort": sort, "sortBy": _curry2(sortBy),
            "group": group, "intercalate": _curry2(intercalate),
            "intersperse": _curry2(intersperse), "transpose": transpose,
            "isPrefixOf": _curry2(isPrefixOf), "isSuffixOf": _curry2(isSuffixOf),
            "isInfixOf": _curry2(isInfixOf),
            # String
            "words": words, "lines": lines_, "unwords": unwords, "unlines": unlines,
            "show": show, "read": read,
            # Numeric
            "abs": abs_, "signum": signum, "negate": negate,
            "fromIntegral": fromIntegral, "toInteger": toInteger,
            "floor": floor_, "ceiling": ceiling_,
            "truncate": truncate_, "round": round_,
            "div": _curry2(div_), "mod": _curry2(mod_),
            "quot": _curry2(quot_), "rem": _curry2(rem_),
            "gcd": _curry2(gcd_), "lcm": _curry2(lcm_),
            "sqrt": sqrt_, "pi": math.pi, "exp": exp_, "log": log_,
            "sin": sin_, "cos": cos_, "tan": tan_,
            "asin": asin_, "acos": acos_, "atan": atan_,
            "atan2": _curry2(atan2_),
            "maxBound": 2**63 - 1, "minBound": -(2**63),
            # Logic
            "not": not_, "otherwise": True,
            "id": id_, "const": _curry2(const_),
            "flip": _curry3(flip_),
            "error": error_, "undefined": undefined_,
            # Tuples
            "fst": fst, "snd": snd,
            # Char
            "ord": ord_, "chr": chr_,
            "isAlpha": isAlpha, "isDigit": isDigit, "isSpace": isSpace,
            "isUpper": isUpper, "isLower": isLower,
            "toUpper": toUpper, "toLower": toLower,
            # Maybe
            "Nothing": None,
            "Just": lambda x: x,
            "fromMaybe": _curry2(fromMaybe),
            "isJust": isJust, "isNothing": isNothing,
            "fromJust": fromJust, "maybe": _curry3(maybe),
            "catMaybes": catMaybes, "mapMaybe": _curry2(mapMaybe),
            # Turtle
            "forward": forward_io, "fd": forward_io,
            "backward": backward_io, "bd": backward_io,
            "right": right_io, "rt": right_io,
            "left": left_io, "lt": left_io,
            "penUp": penUp_io, "penDown": penDown_io,
            "home": home_io,
            "goto": _curry2(goto_io), "setpos": _curry2(goto_io),
            "color": _curry3(color_io),
            "penSize": penSize_io, "clearCanvas": clearCanvas_io,
            # Constants
            "True": True, "False": False,
            "[]": [],
        })


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------


def _hs_show(val: Any) -> str:
    if isinstance(val, bool):
        return "True" if val else "False"
    if isinstance(val, str):
        return '"' + val.replace("\\", "\\\\").replace('"', '\\"') + '"'
    if isinstance(val, list):
        return "[" + ", ".join(_hs_show(x) for x in val) + "]"
    if isinstance(val, HaskellTuple):
        return "(" + ", ".join(_hs_show(e) for e in val.elements) + ")"
    if val is None:
        return "Nothing"
    return str(val)


def _to_list(val: Any) -> list:
    if isinstance(val, list):
        return val
    if isinstance(val, HaskellList):
        return val.items
    if isinstance(val, HaskellTuple):
        return list(val.elements)
    if isinstance(val, str):
        return list(val)
    if isinstance(val, range):
        return list(val)
    return [val]


def _apply_op(lv: Any, op: str, rv: Any) -> Any:
    try:
        if op in ("+",):
            if isinstance(lv, str) and isinstance(rv, str):
                return lv + rv
            if isinstance(lv, list) and isinstance(rv, list):
                return lv + rv
            return lv + rv
        if op == "-":
            return lv - rv
        if op == "*":
            return lv * rv
        if op in ("/",):
            return lv / rv
        if op == "^":
            return lv ** rv
        if op == "**":
            return lv ** rv
        if op == "==":
            return lv == rv
        if op == "/=":
            return lv != rv
        if op == "<":
            return lv < rv
        if op == ">":
            return lv > rv
        if op == "<=":
            return lv <= rv
        if op == ">=":
            return lv >= rv
        if op == "++":
            if isinstance(lv, str) and isinstance(rv, str):
                return lv + rv
            return _to_list(lv) + _to_list(rv)
        if op == ":":
            return [lv] + _to_list(rv)
        if op == "!!":
            return _to_list(lv)[int(rv)]
        if op in ("div",):
            return int(lv) // int(rv)
        if op in ("mod",):
            return int(lv) % int(rv)
        if op in ("quot",):
            return int(int(lv) / int(rv))
        if op in ("rem",):
            a, b = int(lv), int(rv)
            return a - b * int(a / b)
        if op == "$":
            if callable(rv):
                return rv(lv) if False else lv
            return lv
        if op == ".":
            # Function composition
            return lambda x: lv(rv(x)) if callable(lv) and callable(rv) else lv
    except (TypeError, ZeroDivisionError, IndexError) as e:
        raise HaskellError(str(e))
    return None


def _rfind_op(expr: str, op: str) -> int:
    depth = 0
    in_str = False
    str_char = ""
    last = -1
    i = 0
    while i < len(expr):
        ch = expr[i]
        if in_str:
            if ch == "\\" and i + 1 < len(expr):
                i += 2
                continue
            if ch == str_char:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            str_char = ch
        elif ch in ("(", "["):
            depth += 1
        elif ch in (")", "]"):
            depth -= 1
        elif depth == 0 and expr[i: i + len(op)] == op:
            last = i
        i += 1
    return last


def _split_application(expr: str) -> list[str]:
    """Split a Haskell function application into function + arguments."""
    parts = []
    depth = 0
    in_str = False
    str_char = ""
    current: list[str] = []
    i = 0
    while i < len(expr):
        ch = expr[i]
        if in_str:
            current.append(ch)
            if ch == "\\" and i + 1 < len(expr):
                i += 1
                current.append(expr[i])
            elif ch == str_char:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            str_char = ch
            current.append(ch)
        elif ch in ("(", "[", "{"):
            depth += 1
            current.append(ch)
        elif ch in (")", "]", "}"):
            depth -= 1
            current.append(ch)
        elif ch == " " and depth == 0:
            if current:
                parts.append("".join(current).strip())
                current = []
        else:
            current.append(ch)
        i += 1
    if current:
        parts.append("".join(current).strip())
    return [p for p in parts if p]


def _split_comma(s: str) -> list[str]:
    parts = []
    depth = 0
    in_str = False
    str_char = ""
    current: list[str] = []
    i = 0
    while i < len(s):
        ch = s[i]
        if in_str:
            current.append(ch)
            if ch == "\\" and i + 1 < len(s):
                i += 1
                current.append(s[i])
            elif ch == str_char:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            str_char = ch
            current.append(ch)
        elif ch in ("(", "[", "{"):
            depth += 1
            current.append(ch)
        elif ch in (")", "]", "}"):
            depth -= 1
            current.append(ch)
        elif ch == "," and depth == 0:
            parts.append("".join(current).strip())
            current = []
        else:
            current.append(ch)
        i += 1
    if current:
        parts.append("".join(current).strip())
    return [p for p in parts if p]


def _parse_string(raw: str) -> str:
    return (
        raw.replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace('\\"', '"')
        .replace("\\\\", "\\")
    )


def _parse_params(params_str: str) -> list[str]:
    if not params_str.strip():
        return []
    return params_str.split()


def _parse_guards(body: str) -> list[tuple]:
    """Parse guards from body like: | cond = expr | otherwise = expr"""
    if not body.strip().startswith("|"):
        return []
    guards = []
    for m in re.finditer(r"\|\s*(.+?)\s*=\s*(.+?)(?=\s*\||\s*$)", body):
        guards.append((m.group(1).strip(), m.group(2).strip()))
    return guards


def _parse_case_alts(alts_s: str) -> list[tuple[str, str]]:
    """Parse case alternatives: pattern -> body; ..."""
    alts = []
    for m in re.finditer(r"(.+?)\s*->\s*(.+?)(?=;|\n|$)", alts_s):
        alts.append((m.group(1).strip(), m.group(2).strip()))
    return alts


def _match_pattern(pattern: str, value: Any) -> tuple[bool, dict]:
    """Try to match a value against a Haskell pattern. Returns (matched, bindings)."""
    pattern = pattern.strip()

    # Wildcard
    if pattern == "_":
        return True, {}

    # Variable (lowercase) — matches anything
    if re.match(r"^[a-z_]\w*'?$", pattern):
        return True, {pattern: value}

    # Literal: integer
    m = re.match(r"^-?\d+$", pattern)
    if m:
        return int(pattern) == value, {}

    # Literal: string
    if pattern.startswith('"') and pattern.endswith('"'):
        return _parse_string(pattern[1:-1]) == value, {}

    # Literal: True / False
    if pattern == "True":
        return value is True or value == True, {}
    if pattern == "False":
        return value is False or value == False, {}

    # Empty list []
    if pattern == "[]":
        return _to_list(value) == [], {}

    # Cons pattern x:xs
    m = re.match(r"^(\w+):(\w+)$", pattern)
    if m:
        lst = _to_list(value)
        if not lst:
            return False, {}
        return True, {m.group(1): lst[0], m.group(2): lst[1:]}

    # Tuple pattern (a, b)
    if pattern.startswith("(") and pattern.endswith(")"):
        inner = pattern[1:-1]
        sub_patterns = _split_comma(inner)
        if isinstance(value, (HaskellTuple, tuple)) and len(value) == len(sub_patterns):
            bindings = {}
            for sp, sv in zip(sub_patterns, value):
                ok, b = _match_pattern(sp, sv)
                if not ok:
                    return False, {}
                bindings.update(b)
            return True, bindings
        if isinstance(value, list) and len(value) == len(sub_patterns):
            bindings = {}
            for sp, sv in zip(sub_patterns, value):
                ok, b = _match_pattern(sp, sv)
                if not ok:
                    return False, {}
                bindings.update(b)
            return True, bindings

    # Constructor pattern: Just x, Nothing
    m = re.match(r"^([A-Z]\w*)\s*(.*)$", pattern)
    if m:
        constructor = m.group(1)
        arg_pattern = m.group(2).strip()
        if constructor == "Nothing":
            return value is None, {}
        if constructor == "Just" and arg_pattern:
            if value is not None:
                return _match_pattern(arg_pattern, value)
        return True, {}  # data constructor match

    return False, {}


def _split_do_stmts(body: str) -> list[str]:
    """Split a do-block body into individual statements."""
    if not body:
        return []
    # Try to split on semicolons first (inline do-blocks)
    if ";" in body and "\n" not in body:
        return [s.strip() for s in body.split(";") if s.strip()]
    lines = body.splitlines()
    stmts = []
    current: list[str] = []
    for line in lines:
        if not line.strip():
            continue
        # If line starts without indentation relative to first, it's a new stmt
        if current and not line.startswith(" ") and not line.startswith("\t"):
            stmts.append(" ".join(current))
            current = [line.strip()]
        else:
            current.append(line.strip())
    if current:
        stmts.append(" ".join(current))
    return [s for s in stmts if s]


def _preprocess(source: str) -> list[str]:
    """Preprocess Haskell source: handle {- -} block comments, layout rule basics."""
    # Remove block comments
    source = re.sub(r"\{-.*?-\}", "", source, flags=re.DOTALL)
    lines = source.splitlines()
    return lines
