"""Haskell-subset language executor for Time Warp Studio.

Educational Haskell interpreter — whole-program execution.
Implements a teaching subset of Haskell in a Python tree-walker:
  - main = do ... (do-notation block)
  - let x = expr in do blocks
  - putStrLn, putStr, print
  - if expr then expr else expr
  - Basic numeric and list operations
  - List ranges [1..10], [1,3..20]
  - map, filter, foldl, foldr
  - List comprehensions [expr | var <- list, guard]
  - where clauses (in function definitions)
  - Function application (f x y)
  - Lambda: \\x -> expr
  - Pattern matching (simple)
  - Type class show, read
  - String concatenation ++
  - Numeric: ceiling, floor, round, truncate, abs, signum
"""

from __future__ import annotations

import math
import re
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..core.turtle_state import TurtleState


def execute_haskell(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a Haskell-subset program and return all output."""
    env = HaskellEnvironment(interpreter, turtle)
    return env.run(source)


class HaskellEnvironment:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []  # flat chunks, joined with "" at the end
        self._defs: dict[str, Any] = {}
        self._setup_prelude()

    def _emit(self, text: str):
        """Append text to the output buffer."""
        self._output.append(str(text))

    def run(self, source: str) -> str:
        if not source.strip():
            return ""
        try:
            lines = source.splitlines()
            self._parse_definitions(lines)
            # Run main
            main = self._defs.get("main")
            if main is None:
                self._emit("❌ Haskell: no 'main' defined\n")
                return "".join(self._output)
            if callable(main):
                main()
            elif isinstance(main, HaskellDoBlock):
                main.execute(self)
        except HaskellError as e:
            self._emit(f"❌ Haskell error: {e}\n")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}\n")
        return "".join(self._output)

    def _setup_prelude(self):
        d = self._defs
        emit = self._emit
        interp = self.interpreter

        # IO actions
        # putStrLn prints text followed by a newline (like Haskell's putStrLn)
        # putStr prints text with NO trailing newline
        # print shows the Haskell representation followed by a newline
        d["putStrLn"] = lambda s: emit(str(s) + "\n") or ()
        d["putStr"] = lambda s: emit(str(s)) or ()
        d["print"] = lambda x: emit(_haskell_show(x) + "\n") or ()
        d["getLine"] = lambda: (
            interp.request_input("haskell> ")
            if hasattr(interp, "request_input")
            else ""
        )
        d["return"] = lambda x: x

        # Prelude functions
        d["show"] = _haskell_show
        d["read"] = lambda s: int(s) if str(s).lstrip("-").isdigit() else float(s)
        d["length"] = len
        d["null"] = lambda x: len(x) == 0 if hasattr(x, "__len__") else x is None
        d["head"] = lambda x: x[0]
        d["tail"] = lambda x: x[1:]
        d["last"] = lambda x: x[-1]
        d["init"] = lambda x: x[:-1]
        d["reverse"] = lambda x: x[::-1]
        d["sum"] = sum
        d["product"] = lambda xs: _product(xs)
        d["maximum"] = max
        d["minimum"] = min
        d["abs"] = abs
        d["signum"] = lambda x: 0 if x == 0 else (1 if x > 0 else -1)
        d["negate"] = lambda x: -x
        d["ceiling"] = math.ceil
        d["floor"] = math.floor
        d["round"] = round
        d["truncate"] = int
        d["sqrt"] = math.sqrt
        d["pi"] = math.pi
        d["exp"] = math.exp
        d["log"] = math.log
        d["sin"] = math.sin
        d["cos"] = math.cos
        d["tan"] = math.tan
        d["fromIntegral"] = lambda x: float(x)
        d["toInteger"] = int
        d["even"] = lambda x: x % 2 == 0
        d["odd"] = lambda x: x % 2 != 0
        d["div"] = lambda a: lambda b: int(a) // int(b)
        d["mod"] = lambda a: lambda b: int(a) % int(b)
        d["quot"] = lambda a: lambda b: int(int(a) / int(b))
        d["rem"] = lambda a: lambda b: int(a) % int(b)
        d["max"] = lambda a: lambda b: a if a >= b else b
        d["min"] = lambda a: lambda b: a if a <= b else b
        d["not"] = lambda x: not x
        d["and"] = all
        d["or"] = any
        d["all"] = lambda f: lambda xs: all(f(x) for x in xs)
        d["any"] = lambda f: lambda xs: any(f(x) for x in xs)
        d["map"] = lambda f: lambda xs: list(map(f, xs))
        d["filter"] = lambda f: lambda xs: list(filter(f, xs))
        d["foldl"] = lambda f: lambda z: lambda xs: _foldl(f, z, xs)
        d["foldr"] = lambda f: lambda z: lambda xs: _foldr(f, z, xs)
        d["foldl1"] = lambda f: lambda xs: _foldl(f, xs[0], xs[1:])
        d["foldr1"] = lambda f: lambda xs: _foldr(f, xs[-1], xs[:-1])
        d["scanl"] = lambda f: lambda z: lambda xs: _scanl(f, z, xs)
        d["zip"] = lambda a: lambda b: list(zip(a, b))
        d["zipWith"] = lambda f: lambda a: lambda b: [f(x, y) for x, y in zip(a, b)]
        d["unzip"] = lambda pairs: ([a for a, _ in pairs], [b for _, b in pairs])
        d["concat"] = lambda xss: [x for xs in xss for x in xs]
        d["concatMap"] = lambda f: lambda xs: [y for x in xs for y in f(x)]
        d["take"] = lambda n: lambda xs: xs[: int(n)]
        d["drop"] = lambda n: lambda xs: xs[int(n) :]
        d["takeWhile"] = lambda f: lambda xs: list(_takewhile(f, xs))
        d["dropWhile"] = lambda f: lambda xs: list(_dropwhile(f, xs))
        d["splitAt"] = lambda n: lambda xs: (xs[: int(n)], xs[int(n) :])
        d["span"] = lambda f: lambda xs: _span(f, xs)
        d["break_"] = lambda f: lambda xs: _span(lambda x: not f(x), xs)
        d["elem"] = lambda x: lambda xs: x in xs
        d["notElem"] = lambda x: lambda xs: x not in xs
        d["lookup"] = lambda k: lambda pairs: next(
            (v for key, v in pairs if key == k), None
        )
        d["replicate"] = lambda n: lambda x: [x] * int(n)
        d["iterate"] = lambda f: lambda x: _iterate(f, x)
        d["lines"] = lambda s: s.splitlines()
        d["words"] = lambda s: s.split()
        d["unlines"] = lambda xs: "\n".join(xs)
        d["unwords"] = lambda xs: " ".join(xs)
        d["lines_"] = lambda s: s.splitlines()
        d["chr"] = chr
        d["ord"] = ord
        d["digitToInt"] = lambda c: int(c, 16)
        d["intToDigit"] = lambda n: "0123456789abcdef"[n]
        d["isDigit"] = lambda c: c.isdigit()
        d["isAlpha"] = lambda c: c.isalpha()
        d["isAlphaNum"] = lambda c: c.isalnum()
        d["isSpace"] = lambda c: c.isspace()
        d["isUpper"] = lambda c: c.isupper()
        d["isLower"] = lambda c: c.islower()
        d["toUpper"] = lambda c: c.upper()
        d["toLower"] = lambda c: c.lower()
        d["succ"] = lambda x: x + 1
        d["pred"] = lambda x: x - 1
        d["id"] = lambda x: x
        d["const"] = lambda x: lambda _: x
        d["flip"] = lambda f: lambda a: lambda b: f(b)(a)
        d["curry"] = lambda f: lambda a: lambda b: f(a, b)
        d["uncurry"] = lambda f: lambda pair: f(pair[0], pair[1])
        d["error"] = lambda msg: (_ for _ in ()).throw(HaskellError(str(msg)))
        d["undefined"] = lambda: (_ for _ in ()).throw(HaskellError("undefined"))
        d["True"] = True
        d["False"] = False
        d["Nothing"] = None
        d["Just"] = lambda x: x
        d["fromJust"] = lambda x: x
        d["isNothing"] = lambda x: x is None
        d["isJust"] = lambda x: x is not None
        d["maybe"] = lambda default, f, mx: default if mx is None else f(mx)

    # ------------------------------------------------------------------
    # Definition parser
    # ------------------------------------------------------------------

    def _parse_definitions(self, lines: list[str]):
        """Parse top-level definitions."""
        i = 0
        while i < len(lines):
            line = lines[i]
            stripped = line.strip()
            if not stripped or stripped.startswith("--"):
                i += 1
                continue
            # Type signature: name :: type  — ignore
            if re.match(r"^\w+\s*::", stripped):
                i += 1
                continue
            # Module/import lines
            if re.match(r"^(module|import)\b", stripped):
                i += 1
                continue
            # Function/value definition: name [args] = expr
            m = re.match(r"^(\w+)((?:\s+\w+)*)\s*=\s*(.+)$", stripped)
            if m:
                name = m.group(1)
                args_str = m.group(2).strip()
                body_str = m.group(3).strip()
                # Gather continuation lines (indented)
                body_lines = [body_str]
                j = i + 1
                while j < len(lines) and (
                    lines[j].startswith(" ") or lines[j].startswith("\t")
                ):
                    body_lines.append(lines[j].strip())
                    j += 1
                full_body = "\n".join(body_lines)
                params = args_str.split() if args_str else []
                self._defs[name] = self._make_def(name, params, full_body)
                i = j
                continue
            i += 1

    def _make_def(self, name: str, params: list[str], body: str) -> Any:
        """Create a definition: either a value or a function."""
        if not params:
            # Check for do-notation
            if body.strip().startswith("do"):
                return HaskellDoBlock(body, self)
            try:
                return self._eval_expr(body)
            except Exception:
                return HaskellThunk(body, self)
        else:
            # Curried function — use live _defs so recursive calls work
            env = self._defs

            def make_fn(p_list, body_, env_):
                if len(p_list) == 1:

                    def fn(arg):
                        local = env_.copy()
                        local[p_list[0]] = arg
                        return HaskellEvaluator(self, local).eval(body_)

                    return fn
                else:

                    def fn(arg):
                        local = env_.copy()
                        local[p_list[0]] = arg
                        return make_fn(p_list[1:], body_, local)

                    return fn

            return make_fn(params, body, env)

    def _eval_expr(self, expr: str) -> Any:
        return HaskellEvaluator(self, self._defs).eval(expr)


class HaskellDoBlock:
    def __init__(self, source: str, env: "HaskellEnvironment"):
        self.source = source
        self.env = env

    def execute(self, env: "HaskellEnvironment"):
        evaluator = HaskellEvaluator(env, env._defs.copy())
        evaluator.exec_do(self.source)


class HaskellThunk:
    def __init__(self, expr: str, env: "HaskellEnvironment"):
        self.expr = expr
        self.env = env
        self._value = None
        self._forced = False

    def force(self) -> Any:
        if not self._forced:
            self._value = HaskellEvaluator(self.env, self.env._defs.copy()).eval(
                self.expr
            )
            self._forced = True
        return self._value


class HaskellEvaluator:
    def __init__(self, env: "HaskellEnvironment", scope: dict):
        self.env = env
        self.scope = scope

    def eval(self, expr: str) -> Any:
        expr = expr.strip()
        if not expr:
            return ()

        # Force thunks
        if isinstance(expr, HaskellThunk):
            return expr.force()

        # Literals
        if expr == "True":
            return True
        if expr == "False":
            return False
        if expr == "Nothing":
            return None
        if expr == "[]":
            return []
        if expr == "()":
            return ()

        # String literal
        m = re.match(r'^"((?:[^"\\]|\\.)*)"$', expr)
        if m:
            return m.group(1).replace("\\n", "\n").replace("\\t", "\t")

        # Char literal
        m = re.match(r"^'(.)'$", expr, re.DOTALL)
        if m:
            return m.group(1)

        # Number
        try:
            return int(expr)
        except ValueError:
            pass
        try:
            return float(expr)
        except ValueError:
            pass

        # Negative number
        if re.match(r"^-\d", expr):
            try:
                return int(expr)
            except ValueError:
                try:
                    return float(expr)
                except ValueError:
                    pass

        # List literal [a, b, c]
        m = re.match(r"^\[(.+)\]$", expr)
        if m:
            inner = m.group(1).strip()
            # List comprehension [expr | ...] — check for '|' at bracket depth 0
            # before range, because inner may contain nested [a..b] ranges.
            depth = 0
            has_pipe_at_depth0 = False
            for ch in inner:
                if ch in "([":
                    depth += 1
                elif ch in ")]":
                    depth -= 1
                elif ch == "|" and depth == 0:
                    has_pipe_at_depth0 = True
                    break
            if has_pipe_at_depth0:
                return self._eval_list_comp(inner)
            # Range [a..b] or [a,b..c]
            range_m = re.match(r"^(.+?)\.\.\s*(.+)?$", inner)
            if range_m:
                return self._eval_range(
                    range_m.group(1).strip(),
                    range_m.group(2) and range_m.group(2).strip(),
                )
            # Tuple from list (treat as Python list)
            parts = _split_commas_haskell(inner)
            return [self.eval(p.strip()) for p in parts]

        # Tuple (a, b, c) or operator section
        if expr.startswith("(") and expr.endswith(")"):
            inner = expr[1:-1].strip()
            if not inner:
                return ()
            # Operator section: (+) or (*) — whole operator in parens
            if re.match(r"^[+\-*/<>=!&|.]+$", inner):
                op = inner
                return lambda a: lambda b: _apply_op_haskell(a, op, b)
            # Right section: (* 2), (+ 3), (*2) — but not negative numbers like (-5)
            sm = re.match(r"^([+\-*/<>=!]+)\s*(.+)$", inner)
            if sm:
                op = sm.group(1)
                rhs = sm.group(2).strip()
                # Guard: (-5) is a negative number, not a right section
                if op == "-" and re.match(r"^\d+\.?\d*$", rhs):
                    return -self.eval(rhs)
                right = self.eval(rhs)
                return lambda x, _o=op, _r=right: _apply_op_haskell(x, _o, _r)
            # Left section: (2 +), (3 *)
            sm = re.match(r"^(.+?)\s+([+\-*/<>=!]+)$", inner)
            if sm and not re.match(r"^\d", sm.group(2)):
                left = self.eval(sm.group(1).strip())
                op = sm.group(2)
                return lambda y, _o=op, _l=left: _apply_op_haskell(_l, _o, y)
            parts = _split_commas_haskell(inner)
            if len(parts) == 1:
                return self.eval(parts[0].strip())
            return tuple(self.eval(p.strip()) for p in parts)

        # Lambda:  \x -> expr  or  \x y -> expr
        m = re.match(r"^\\([\w\s,()]+?)\s*->\s*(.+)$", expr)
        if m:
            params_str = m.group(1).strip()
            body = m.group(2).strip()
            params = params_str.split()
            return self._make_lambda(params, body)

        # Let ... in ...
        m = re.match(r"^let\s+(.+?)\s+in\s+(.+)$", expr, re.DOTALL)
        if m:
            bindings_str = m.group(1)
            body = m.group(2).strip()
            local = self.scope.copy()
            for binding in bindings_str.split(";"):
                bm = re.match(r"^\s*(\w+)\s*=\s*(.+)$", binding.strip())
                if bm:
                    local[bm.group(1)] = HaskellEvaluator(self.env, local).eval(
                        bm.group(2).strip()
                    )
            return HaskellEvaluator(self.env, local).eval(body)

        # if-then-else
        m = re.match(r"^if\s+(.+?)\s+then\s+(.+?)\s+else\s+(.+)$", expr, re.DOTALL)
        if m:
            cond = self.eval(m.group(1).strip())
            if cond:
                return self.eval(m.group(2).strip())
            else:
                return self.eval(m.group(3).strip())

        # Case expression
        m = re.match(r"^case\s+(.+?)\s+of\b(.+)$", expr, re.DOTALL)
        if m:
            subject = self.eval(m.group(1).strip())
            alts_str = m.group(2).strip()
            # Parse alternatives: pattern -> expr; ...
            for alt in re.split(r";\s*", alts_str):
                alt_m = re.match(r"^\s*(.+?)\s*->\s*(.+)$", alt.strip())
                if alt_m:
                    pattern = alt_m.group(1).strip()
                    alt_expr = alt_m.group(2).strip()
                    if self._match_pattern(pattern, subject):
                        return self.eval(alt_expr)
            return None

        # do notation — single-line
        if expr.startswith("do ") or expr.startswith("do\n"):
            return self.exec_do(expr)

        # where
        m = re.match(r"^(.+?)\s+where\s+(.+)$", expr, re.DOTALL)
        if m:
            body = m.group(1).strip()
            where_str = m.group(2).strip()
            local = self.scope.copy()
            # Split bindings by semicolons or newlines, supporting multi-line where blocks
            import re as _re2

            raw_bindings = _re2.split(r";|\n", where_str)
            for binding in raw_bindings:
                bm = re.match(r"^\s*(\w+)\s*=\s*(.+)$", binding.strip())
                if bm:
                    local[bm.group(1)] = HaskellEvaluator(self.env, local).eval(
                        bm.group(2).strip()
                    )
            return HaskellEvaluator(self.env, local).eval(body)

        # Operator expressions (lowest precedence first)
        for op in [
            "||",
            "&&",
            "==",
            "/=",
            "<=",
            ">=",
            "<",
            ">",
            ":",
            "++",
            "+",
            "-",
            "*",
            "/",
            "^",
            "**",
            "!!",
            ".",
            "$",
            "`div`",
            "`mod`",
            "`elem`",
        ]:
            idx = _find_op_haskell(expr, op)
            if idx > 0:
                lhs_s = expr[:idx].strip()
                rhs_s = expr[idx + len(op) :].strip()
                if not lhs_s:
                    continue
                lhs = self.eval(lhs_s)
                if op == "$":
                    # f $ x = f x
                    return self._apply(lhs, rhs_s)
                rhs = self.eval(rhs_s)
                return _apply_op_haskell(lhs, op, rhs)

        # Function application: f x y z
        if " " in expr:
            parts = _split_application(expr)
            if len(parts) > 1:
                fn = self.eval(parts[0])
                for arg_s in parts[1:]:
                    fn = self._apply(fn, arg_s)
                return fn

        # Variable lookup
        if expr in self.scope:
            val = self.scope[expr]
            if isinstance(val, HaskellThunk):
                return val.force()
            return val

        # Section: (+3) or (3+)  (partially applied operators)
        m = re.match(r"^\(([+\-*/<>=!]+)\s*(.+)\)$", expr)
        if m:
            op = m.group(1)
            right = self.eval(m.group(2).strip())
            return lambda x: _apply_op_haskell(x, op, right)
        m = re.match(r"^\((.+)\s*([+\-*/<>=!]+)\)$", expr)
        if m:
            left = self.eval(m.group(1).strip())
            op = m.group(2)
            return lambda y: _apply_op_haskell(left, op, y)

        return expr  # unresolved

    def _apply(self, fn: Any, arg_s: str) -> Any:
        if callable(fn):
            arg = self.eval(arg_s) if isinstance(arg_s, str) else arg_s
            try:
                return fn(arg)
            except Exception:
                return None
        return fn

    def _make_lambda(self, params: list[str], body: str):
        scope = self.scope.copy()
        if len(params) == 1:

            def lam(x, p=params[0], b=body, s=scope):
                local = s.copy()
                local[p] = x
                return HaskellEvaluator(self.env, local).eval(b)

            return lam
        else:

            def lam_curry(x, p=params[0], rest=params[1:], b=body, s=scope):
                local = s.copy()
                local[p] = x
                return HaskellEvaluator(self.env, local)._make_lambda(rest, b)

            return lam_curry

    def _eval_range(self, start_s: str, end_s: str | None) -> list:
        # [a,b..c] or [a..b]
        parts = _split_commas_haskell(start_s)
        if len(parts) == 2:
            # [a,b..c]
            a = self.eval(parts[0].strip())
            b = self.eval(parts[1].strip())
            step = b - a
            if end_s:
                c = self.eval(end_s)
                result = []
                cur = a
                while (step > 0 and cur <= c + 1e-9) or (step < 0 and cur >= c - 1e-9):
                    result.append(
                        cur
                        if isinstance(cur, int)
                        else (int(cur) if cur == int(cur) else cur)
                    )
                    if len(result) > 10000:
                        break
                    cur += step
                return result
            else:
                return list(range(int(a), int(a) + 100, int(step)))
        else:
            a = self.eval(start_s)
            if end_s:
                b = self.eval(end_s)
                if isinstance(a, str) and isinstance(b, str):
                    return [chr(i) for i in range(ord(a), ord(b) + 1)]
                return list(range(int(a), int(b) + 1))
            else:
                return list(range(int(a), int(a) + 100))

    def _eval_list_comp(self, inner: str) -> list:
        """[expr | x <- list, guard, ...]"""
        pipe_idx = inner.index("|")
        expr_s = inner[:pipe_idx].strip()
        generators_s = inner[pipe_idx + 1 :].strip()
        generators = self._split_generators(generators_s)
        results: list = []
        self._expand_generators(generators, 0, self.scope.copy(), expr_s, results)
        return results

    @staticmethod
    def _split_generators(s: str) -> list[str]:
        """Split generator clauses on commas, respecting brackets."""
        parts: list[str] = []
        depth = 0
        current: list[str] = []
        for ch in s:
            if ch in "([{":
                depth += 1
            elif ch in ")]}":
                depth -= 1
            if ch == "," and depth == 0:
                parts.append("".join(current).strip())
                current = []
            else:
                current.append(ch)
        rest = "".join(current).strip()
        if rest:
            parts.append(rest)
        return parts

    def _expand_generators(
        self, gens: list[str], idx: int, scope: dict, expr_s: str, results: list
    ):
        if idx >= len(gens):
            results.append(HaskellEvaluator(self.env, scope).eval(expr_s))
            return
        gen = gens[idx]
        m = re.match(r"^(\w+)\s*<-\s*(.+)$", gen)
        if m:
            var = m.group(1)
            lst = HaskellEvaluator(self.env, scope).eval(m.group(2).strip())
            for item in lst:
                new_scope = scope.copy()
                new_scope[var] = item
                self._expand_generators(gens, idx + 1, new_scope, expr_s, results)
        else:
            # Guard
            if HaskellEvaluator(self.env, scope).eval(gen):
                self._expand_generators(gens, idx + 1, scope, expr_s, results)

    def _match_pattern(self, pattern: str, value: Any) -> bool:
        if pattern == "_":
            return True
        if pattern.startswith('"') or pattern.startswith("'"):
            return str(value) == pattern.strip("\"'")
        try:
            return value == int(pattern)
        except ValueError:
            pass
        try:
            return value == float(pattern)
        except ValueError:
            pass
        if pattern == "True":
            return value is True
        if pattern == "False":
            return value is False
        if pattern == "[]":
            return value == []
        if pattern.startswith("(") and pattern.endswith(")"):
            inner = pattern[1:-1].strip()
            parts = _split_commas_haskell(inner)
            if isinstance(value, (list, tuple)) and len(parts) == len(value):
                return all(
                    self._match_pattern(p.strip(), v) for p, v in zip(parts, value)
                )
        # Variable (always matches, binds)
        if re.match(r"^\w+$", pattern):
            self.scope[pattern] = value
            return True
        return False

    def exec_do(self, source: str) -> Any:
        """Execute a do-notation block."""
        # Extract body after 'do'
        if source.strip().startswith("do"):
            body = source.strip()[2:].strip()
        else:
            body = source
        lines = self._split_do_lines(body)
        result = None
        for line in lines:
            line = line.strip()
            if not line or line.startswith("--"):
                continue
            # let binding
            m = re.match(r"^let\s+(\w+)\s*=\s*(.+)$", line)
            if m:
                self.scope[m.group(1)] = self.eval(m.group(2).strip())
                continue
            # <- binding
            m = re.match(r"^(\w+)\s*<-\s*(.+)$", line)
            if m:
                result = self.eval(m.group(2).strip())
                self.scope[m.group(1)] = result
                continue
            # return statement
            m = re.match(r"^return\s+(.+)$", line)
            if m:
                result = self.eval(m.group(1).strip())
                continue
            # IO action
            result = self.eval(line)
            if callable(result):
                result = result()
        return result

    def _split_do_lines(self, body: str) -> list[str]:
        """Split do block into individual statements."""
        lines = body.splitlines()
        result = []
        current: list[str] = []
        base_indent = None
        for line in lines:
            if not line.strip():
                continue
            indent = len(line) - len(line.lstrip())
            if base_indent is None:
                base_indent = indent
            if indent <= base_indent and current:
                result.append(" ".join(current))
                current = [line.strip()]
            else:
                current.append(line.strip())
        if current:
            result.append(" ".join(current))
        # Also split on semicolons
        final = []
        for stmt in result:
            for part in stmt.split(";"):
                if part.strip():
                    final.append(part.strip())
        return final


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _haskell_show(val: Any) -> str:
    if isinstance(val, bool):
        return "True" if val else "False"
    if isinstance(val, str):
        return '"' + val + '"'
    if isinstance(val, list):
        return "[" + ", ".join(_haskell_show(x) for x in val) + "]"
    if isinstance(val, tuple):
        return "(" + ", ".join(_haskell_show(x) for x in val) + ")"
    if val is None:
        return "Nothing"
    return str(val)


def _product(xs):
    r = 1
    for x in xs:
        r *= x
    return r


def _foldl(f, z, xs):
    acc = z
    for x in xs:
        acc = (
            f(acc)(x)
            if not isinstance(f(acc), (int, float, str, bool, list, tuple))
            else f(acc, x)
        )
    return acc


def _foldr(f, z, xs):
    acc = z
    for x in reversed(xs):
        try:
            acc = f(x)(acc)
        except TypeError:
            acc = f(x, acc)
    return acc


def _scanl(f, z, xs):
    result = [z]
    acc = z
    for x in xs:
        try:
            acc = f(acc)(x)
        except TypeError:
            acc = f(acc, x)
        result.append(acc)
    return result


def _takewhile(f, xs):
    for x in xs:
        if not f(x):
            break
        yield x


def _dropwhile(f, xs):
    drop = True
    for x in xs:
        if drop and f(x):
            continue
        drop = False
        yield x


def _span(f, xs):
    took = []
    rest = []
    dropped = False
    for x in xs:
        if not dropped and f(x):
            took.append(x)
        else:
            dropped = True
            rest.append(x)
    return took, rest


def _iterate(f, x):
    result = []
    cur = x
    for _ in range(100):
        result.append(cur)
        cur = f(cur)
    return result


def _split_commas_haskell(s: str) -> list[str]:
    parts = []
    depth = 0
    in_str = False
    current: list[str] = []
    for ch in s:
        if in_str:
            current.append(ch)
            if ch == '"':
                in_str = False
            continue
        if ch == '"':
            in_str = True
            current.append(ch)
            continue
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif ch == "," and depth == 0:
            parts.append("".join(current))
            current = []
            continue
        current.append(ch)
    if current:
        parts.append("".join(current))
    return parts


def _find_op_haskell(expr: str, op: str) -> int:
    depth = 0
    in_str = False
    i = 0
    while i < len(expr):
        ch = expr[i]
        if in_str:
            if ch == '"':
                in_str = False
        elif ch == '"':
            in_str = True
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif depth == 0 and expr[i : i + len(op)] == op:
            # Skip '.' when it's a decimal point (digit.digit)
            if (
                op == "."
                and i > 0
                and i + 1 < len(expr)
                and expr[i - 1].isdigit()
                and expr[i + 1].isdigit()
            ):
                i += 1
                continue
            if i > 0:
                return i
        i += 1
    return -1


def _apply_op_haskell(lhs: Any, op: str, rhs: Any) -> Any:
    if op == "+":
        return lhs + rhs
    if op == "-":
        return lhs - rhs
    if op == "*":
        return lhs * rhs
    if op == "/":
        return lhs / rhs
    if op == "^" or op == "**":
        return lhs**rhs
    if op == "==":
        return lhs == rhs
    if op == "/=":
        return lhs != rhs
    if op == "<":
        return lhs < rhs
    if op == ">":
        return lhs > rhs
    if op == "<=":
        return lhs <= rhs
    if op == ">=":
        return lhs >= rhs
    if op == "&&":
        return lhs and rhs
    if op == "||":
        return lhs or rhs
    if op == ":":
        return [lhs] + list(rhs)
    if op == "++":
        if isinstance(lhs, str) and isinstance(rhs, str):
            return lhs + rhs
        return list(lhs) + list(rhs)
    if op == "!!":
        return list(lhs)[int(rhs)]
    if op == "`div`" or op == "div":
        return int(lhs) // int(rhs)
    if op == "`mod`" or op == "mod":
        return int(lhs) % int(rhs)
    if op == "`elem`" or op == "elem":
        return lhs in rhs
    if op == ".":
        # Function composition
        fn_g, fn_f = lhs, rhs
        return lambda x: fn_g(fn_f(x))
    return None


def _split_application(expr: str) -> list[str]:
    """Split 'f x y' into ['f', 'x', 'y'] respecting parens, brackets, strings."""
    parts = []
    depth = 0
    in_str = False
    current: list[str] = []
    for ch in expr:
        if in_str:
            current.append(ch)
            if ch == '"':
                in_str = False
        elif ch == '"':
            in_str = True
            current.append(ch)
        elif ch in "([{":
            depth += 1
            current.append(ch)
        elif ch in ")]}":
            depth -= 1
            current.append(ch)
        elif ch == " " and depth == 0:
            if current:
                parts.append("".join(current))
                current = []
        else:
            current.append(ch)
    if current:
        parts.append("".join(current))
    return parts


class HaskellError(Exception):
    pass
