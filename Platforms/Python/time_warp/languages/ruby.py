"""Ruby language executor for Time Warp Studio.

Educational Ruby interpreter — whole-program execution.
Supports a teaching subset of Ruby 3.x idioms.

Supported features:
  - Variables (local, global $, instance @, constant CONST)
  - Methods: def/end with positional, keyword and default args
  - Classes: class/end, initialize, attr_accessor/reader/writer, new, super
  - Modules and mixins (include, basic method lookup)
  - Control flow: if/elsif/else/unless/end, while/until/end, for/in/end,
    loop, break, next, return
  - Iterators: times, upto, downto, each, map, select, reject, reduce/inject,
    each_with_index, each_with_object, flat_map, any?, all?, none?, count
  - Blocks: do…end and { } with |params|
  - Arrays: literals, push/pop/shift/unshift/first/last/length/size,
    sort/sort_by, uniq, flatten, zip, compact, join, include?, index
  - Hashes: {key: val} / {"k" => v}, [], []=, keys, values, each, merge,
    any?, all?, map, select, to_a, count
  - Strings: interpolation "#{expr}", frozen?, upcase/downcase/capitalize,
    reverse, length/size, include?, start_with?, end_with?, split, strip,
    chomp, chop, gsub/sub, chars, bytes, to_i, to_f, to_s, center/ljust/rjust
  - Ranges: (1..10), (1...10), .each, .to_a, .include?, .min, .max, .sum
  - Symbols: :name, .to_s, .inspect
  - Exceptions: begin/rescue/ensure/raise/end
  - Comparable / Enumerable helpers
  - puts / print / p / pp / printf
  - Math: Math::PI, Math::E, Math.sqrt/sin/cos/tan/log/log2/log10/exp/cbrt
  - Random: rand, srand
  - Kernel: exit (stops execution), sleep (ignored), require (ignored)
  - Turtle graphics: forward(n), backward(n), right(n), left(n), penup(),
    pendown(), color(r,g,b), setpos(x,y), home(), clear_canvas()
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


def execute_ruby(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a complete Ruby program and return all output as a string."""
    env = RubyEnvironment(interpreter, turtle)
    return env.run(source)


# ---------------------------------------------------------------------------
# Sentinel values
# ---------------------------------------------------------------------------

_UNSET = object()


class _RubyNil:
    def __repr__(self) -> str:
        return "nil"

    def __str__(self) -> str:
        return ""

    def __bool__(self) -> bool:
        return False


NIL = _RubyNil()


class _ReturnSignal(Exception):
    def __init__(self, value: Any = NIL):
        self.value = value


class _BreakSignal(Exception):
    def __init__(self, value: Any = NIL):
        self.value = value


class _NextSignal(Exception):
    def __init__(self, value: Any = NIL):
        self.value = value


class RubyError(Exception):
    pass


class RubyStopExecution(Exception):
    pass


# ---------------------------------------------------------------------------
# Ruby runtime types
# ---------------------------------------------------------------------------


class RubySymbol:
    _cache: dict[str, "RubySymbol"] = {}
    name: str  # declared here so type checkers know about the attribute

    def __new__(cls, name: str) -> "RubySymbol":
        if name not in cls._cache:
            obj = super().__new__(cls)
            obj.name = name  # type: ignore[attr-defined]
            cls._cache[name] = obj
        return cls._cache[name]

    def __repr__(self) -> str:
        return f":{self.name}"  # type: ignore[attr-defined]

    def __str__(self) -> str:
        return self.name  # type: ignore[attr-defined]

    def __hash__(self) -> int:
        return hash(self.name)  # type: ignore[attr-defined]

    def __eq__(self, other: object) -> bool:
        return isinstance(other, RubySymbol) and self.name == other.name  # type: ignore[attr-defined]


class RubyRange:
    def __init__(self, start: Any, stop: Any, exclusive: bool = False):
        self.start = start
        self.stop = stop
        self.exclusive = exclusive

    def to_list(self) -> list:
        if isinstance(self.start, int) and isinstance(self.stop, int):
            end = self.stop if not self.exclusive else self.stop - 1
            return list(range(self.start, end + 1))
        if isinstance(self.start, str) and isinstance(self.stop, str):
            start_ord = ord(self.start)
            end_ord = ord(self.stop) if not self.exclusive else ord(self.stop) - 1
            return [chr(c) for c in range(start_ord, end_ord + 1)]
        return []

    def __repr__(self) -> str:
        op = "..." if self.exclusive else ".."
        return f"({self.start!r}{op}{self.stop!r})"

    def __contains__(self, item: Any) -> bool:
        lst = self.to_list()
        return item in lst


class RubyProc:
    def __init__(self, params: list[str], body: str, closure: dict[str, Any]):
        self.params = params
        self.body = body
        self.closure = closure


class RubyMethod:
    def __init__(
        self,
        name: str,
        params: list[str],
        body: str,
        defaults: dict[str, str],
        klass: "RubyClass | None" = None,
    ):
        self.name = name
        self.params = params
        self.body = body
        self.defaults = defaults  # param_name -> default_expr_str
        self.klass = klass


class RubyClass:
    def __init__(self, name: str, superclass: "RubyClass | None" = None):
        self.name = name
        self.superclass = superclass
        self.methods: dict[str, RubyMethod] = {}
        self.class_methods: dict[str, RubyMethod] = {}
        self.instance_vars: dict[str, Any] = {}

    def find_method(self, name: str) -> "RubyMethod | None":
        if name in self.methods:
            return self.methods[name]
        if self.superclass:
            return self.superclass.find_method(name)
        return None


class RubyObject:
    def __init__(self, klass: RubyClass):
        self.klass = klass
        self.ivars: dict[str, Any] = {}

    def __repr__(self) -> str:
        return f"#<{self.klass.name}>"


class RubyModule:
    def __init__(self, name: str):
        self.name = name
        self.methods: dict[str, RubyMethod] = {}


# ---------------------------------------------------------------------------
# Environment / interpreter
# ---------------------------------------------------------------------------


class RubyEnvironment:
    MAX_ITERATIONS = 100_000

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []
        # Scope stack: each frame is a dict of local vars
        self._scopes: list[dict[str, Any]] = [{}]
        self._globals: dict[str, Any] = {}
        self._constants: dict[str, Any] = {}
        self._classes: dict[str, RubyClass] = {}
        self._modules: dict[str, RubyModule] = {}
        self._current_self: Any = None
        self._current_class: RubyClass | None = None
        self._iteration_count = 0
        self._setup_stdlib()

    # ------------------------------------------------------------------
    # Output
    # ------------------------------------------------------------------

    def _emit(self, text: str) -> None:
        self._output.append(text)

    # ------------------------------------------------------------------
    # Scope helpers
    # ------------------------------------------------------------------

    def _get_var(self, name: str) -> Any:
        # Local scopes (innermost first)
        for frame in reversed(self._scopes):
            if name in frame:
                return frame[name]
        # Instance variables
        if name.startswith("@") and isinstance(self._current_self, RubyObject):
            return self._current_self.ivars.get(name, NIL)
        # Globals
        if name.startswith("$"):
            return self._globals.get(name, NIL)
        # Constants
        if name and name[0].isupper():
            return self._constants.get(name, NIL)
        return NIL

    def _set_var(self, name: str, value: Any, local: bool = True) -> None:
        if name.startswith("@") and isinstance(self._current_self, RubyObject):
            self._current_self.ivars[name] = value
        elif name.startswith("$"):
            self._globals[name] = value
        elif name and name[0].isupper():
            self._constants[name] = value
        elif local and self._scopes:
            # Ruby closure semantics: if the variable exists in an outer scope,
            # update it there rather than shadowing with a new local binding.
            for frame in reversed(self._scopes):
                if name in frame:
                    frame[name] = value
                    return
            # Not found anywhere — create in innermost scope.
            self._scopes[-1][name] = value
        else:
            self._globals[name] = value

    def _push_scope(self, bindings: dict[str, Any] | None = None) -> None:
        frame: dict[str, Any] = dict(bindings) if bindings else {}
        self._scopes.append(frame)

    def _pop_scope(self) -> None:
        if len(self._scopes) > 1:
            self._scopes.pop()

    # ------------------------------------------------------------------
    # stdlib setup
    # ------------------------------------------------------------------

    def _setup_stdlib(self) -> None:
        self._constants["RUBY_VERSION"] = "3.2.0 (Time Warp Studio)"
        self._constants["Math"] = self  # handled specially in method call
        self._constants["STDIN"] = None
        self._constants["STDOUT"] = None

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        try:
            stmts = _split_statements(source)
            self._exec_stmts(stmts)
        except RubyStopExecution:
            pass
        except _ReturnSignal:
            pass
        except RubyError as e:
            self._emit(f"❌ Ruby error: {e}")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}")
        out = "\n".join(self._output)
        return out + ("\n" if out and not out.endswith("\n") else "")

    # ------------------------------------------------------------------
    # Statement execution
    # ------------------------------------------------------------------

    def _exec_stmts(self, stmts: list[str]) -> Any:
        last: Any = NIL
        for stmt in stmts:
            stmt = stmt.strip()
            if not stmt or stmt.startswith("#"):
                continue
            last = self._exec_stmt(stmt)
        return last

    def _exec_stmt(self, stmt: str) -> Any:  # noqa: C901 (complex by design)
        stmt = stmt.strip()
        if not stmt or stmt.startswith("#"):
            return NIL

        self._iteration_count += 1
        if self._iteration_count > self.MAX_ITERATIONS * 100:
            raise RubyError("iteration limit exceeded")

        # Remove inline comments (naive — not inside strings)
        stmt_no_comment = _strip_inline_comment(stmt)
        # For block-opening patterns we only need to match the first line.
        first_line_nc = stmt_no_comment.split("\n")[0].rstrip()

        # ----- class definition -----
        m = re.match(r"^class\s+([A-Z]\w*)(?:\s*<\s*([A-Z]\w*))?\s*$", first_line_nc)
        if m:
            return self._exec_class_def(m.group(1), m.group(2), stmt)

        # ----- module definition -----
        m = re.match(r"^module\s+([A-Z]\w*)\s*$", first_line_nc)
        if m:
            return self._exec_module_def(m.group(1), stmt)

        # ----- method definition -----
        m = re.match(r"^def\s+(\w+[\?!]?)\s*(?:\((.*?)\))?\s*$", first_line_nc)
        if m:
            return self._exec_def(m.group(1), m.group(2) or "", stmt)

        # ----- class method def -----
        m = re.match(r"^def\s+self\.(\w+[\?!]?)\s*(?:\((.*?)\))?\s*$", first_line_nc)
        if m and self._current_class:
            return self._exec_class_method_def(m.group(1), m.group(2) or "", stmt)

        # ----- if/elsif/else block -----
        if re.match(r"^if\s+", stmt_no_comment) or re.match(
            r"^unless\s+", stmt_no_comment
        ):
            return self._exec_if_block(stmt)

        # ----- while / until -----
        m = re.match(r"^(while|until)\s+([^\n]+)", stmt_no_comment)
        if m:
            return self._exec_while(m.group(1), m.group(2).strip(), stmt)

        # ----- for x in range/array -----
        m = re.match(r"^for\s+(\w+)\s+in\s+(.+)$", stmt_no_comment)
        if m:
            return self._exec_for(m.group(1), m.group(2), stmt)

        # ----- loop do...end -----
        if stmt_no_comment == "loop do" or stmt_no_comment.startswith("loop do\n"):
            return self._exec_loop(stmt)

        # ----- begin/rescue block -----
        if stmt_no_comment.strip() == "begin":
            return self._exec_begin(stmt)

        # ----- raise -----
        m = re.match(r"^raise\s*(.*)", stmt_no_comment)
        if m:
            msg = m.group(1).strip().strip('"\'')
            raise RubyError(msg or "RuntimeError")

        # ----- return -----
        m = re.match(r"^return(?:\s+(.+))?$", stmt_no_comment)
        if m:
            expr = m.group(1) or ""
            mod = _find_modifier_keyword(expr)
            # Handle: return if COND / return unless COND (no value, modifier only)
            if mod is None:
                m2 = re.match(r"^(if|unless)\s+(.+)$", expr)
                if m2:
                    kw, cond = m2.group(1), m2.group(2)
                    cond_val = _is_truthy(self._eval(cond))
                    if (kw == "if" and cond_val) or (kw == "unless" and not cond_val):
                        raise _ReturnSignal(NIL)
                    return NIL
            if mod:
                lhs, kw, cond = mod
                cond_val = _is_truthy(self._eval(cond))
                if (kw == "if" and cond_val) or (kw == "unless" and not cond_val):
                    raise _ReturnSignal(self._eval(lhs))
                return NIL
            val = self._eval(expr) if expr else NIL
            raise _ReturnSignal(val)

        # ----- break / next -----
        m = re.match(r"^break(?:\s+(.+))?$", stmt_no_comment)
        if m:
            expr = m.group(1) or ""
            mod = _find_modifier_keyword(expr)
            # Handle: break if COND / break unless COND (no value, modifier only)
            if mod is None:
                m2 = re.match(r"^(if|unless)\s+(.+)$", expr)
                if m2:
                    kw, cond = m2.group(1), m2.group(2)
                    cond_val = _is_truthy(self._eval(cond))
                    if (kw == "if" and cond_val) or (kw == "unless" and not cond_val):
                        raise _BreakSignal(NIL)
                    return NIL
            if mod:
                lhs, kw, cond = mod
                cond_val = _is_truthy(self._eval(cond))
                if (kw == "if" and cond_val) or (kw == "unless" and not cond_val):
                    raise _BreakSignal(self._eval(lhs))
                return NIL
            val = self._eval(expr) if expr else NIL
            raise _BreakSignal(val)
        m = re.match(r"^next(?:\s+(.+))?$", stmt_no_comment)
        if m:
            expr = m.group(1) or ""
            mod = _find_modifier_keyword(expr)
            # Handle: next if COND / next unless COND (no value, modifier only)
            if mod is None:
                m2 = re.match(r"^(if|unless)\s+(.+)$", expr)
                if m2:
                    kw, cond = m2.group(1), m2.group(2)
                    cond_val = _is_truthy(self._eval(cond))
                    if (kw == "if" and cond_val) or (kw == "unless" and not cond_val):
                        raise _NextSignal(NIL)
                    return NIL
            if mod:
                lhs, kw, cond = mod
                cond_val = _is_truthy(self._eval(cond))
                if (kw == "if" and cond_val) or (kw == "unless" and not cond_val):
                    raise _NextSignal(self._eval(lhs))
                return NIL
            val = self._eval(expr) if expr else NIL
            raise _NextSignal(val)

        # ----- puts / print / p / pp -----
        m = re.match(r"^(puts|print|p|pp)\b(.*)", stmt_no_comment)
        if m:
            return self._exec_print(m.group(1), m.group(2).strip(), stmt)

        # ----- printf -----
        m = re.match(r"^printf\s*\(?(.*)\)?$", stmt_no_comment)
        if m:
            return self._exec_printf(m.group(1))

        # ----- assignment: var = expr -----
        m = re.match(r"^([@$]?\w+|\$\w+|[A-Z]\w*)\s*([\+\-\*\/\%]?=(?!=)|\|\|=|&&=)\s*(.+)$", stmt, re.DOTALL)
        if m and m.group(2) in ("=", "+=", "-=", "*=", "/=", "%=", "**=", "||=", "&&="):
            return self._exec_assign(m.group(1), m.group(2), m.group(3))

        # ----- index assignment: var[key] = expr / var[key] op= expr -----
        m = re.match(r"^(\w+)\[(.+)\]\s*([\+\-\*\/\%]?=(?!=)|\|\|=|&&=)\s*(.+)$", stmt, re.DOTALL)
        if m and m.group(3) in ("=", "+=", "-=", "*=", "/=", "%=", "||=", "&&="):
            return self._exec_index_assign(m.group(1), m.group(2), m.group(4), m.group(3))

        # ----- generic modifier if/unless: STMT if COND / STMT unless COND -----
        mod = _find_modifier_keyword(stmt_no_comment)
        if mod:
            body, kw, cond = mod
            cond_val = _is_truthy(self._eval(cond))
            if (kw == "if" and cond_val) or (kw == "unless" and not cond_val):
                return self._exec_stmt(body)
            return NIL

        # ----- var << expr: string append (must store result back for immutable str) -----
        m = re.match(r"^([@$]?\w+)\s*<<\s*(.+)$", stmt_no_comment, re.DOTALL)
        if m:
            var_name = m.group(1)
            rhs_expr = m.group(2).strip()
            current = self._get_var(var_name)
            if isinstance(current, str):
                new_val = current + _ruby_to_s(self._eval(rhs_expr))
                self._set_var(var_name, new_val)
                return new_val

        # ----- any expression (method call, etc.) -----
        return self._eval(stmt)

    # ------------------------------------------------------------------
    # class / module
    # ------------------------------------------------------------------

    def _exec_class_def(
        self, name: str, superclass_name: str | None, full_stmt: str
    ) -> Any:
        super_cls = None
        if superclass_name:
            super_cls = self._classes.get(superclass_name)
        cls = self._classes.get(name) or RubyClass(name, super_cls)
        if superclass_name and not super_cls:
            pass  # silently ignore unknown superclass
        cls.superclass = super_cls
        self._classes[name] = cls
        self._constants[name] = cls
        # Extract body between class...end
        body = _extract_body(full_stmt, "class", "end")
        old_class = self._current_class
        self._current_class = cls
        old_self = self._current_self
        self._current_self = cls
        self._push_scope()
        try:
            stmts = _split_statements(body)
            self._exec_stmts(stmts)
        finally:
            self._pop_scope()
            self._current_class = old_class
            self._current_self = old_self
        return cls

    def _exec_module_def(self, name: str, full_stmt: str) -> Any:
        mod = self._modules.get(name) or RubyModule(name)
        self._modules[name] = mod
        body = _extract_body(full_stmt, "module", "end")
        stmts = _split_statements(body)
        self._exec_stmts(stmts)
        return mod

    # ------------------------------------------------------------------
    # method definition
    # ------------------------------------------------------------------

    def _exec_def(self, name: str, params_str: str, full_stmt: str) -> Any:
        params, defaults = _parse_params(params_str)
        body = _extract_body(full_stmt, "def", "end")
        method = RubyMethod(name, params, body, defaults, self._current_class)
        if self._current_class:
            self._current_class.methods[name] = method
        else:
            self._set_var(name, method)
        return method

    def _exec_class_method_def(self, name: str, params_str: str, full_stmt: str) -> Any:
        if not self._current_class:
            return NIL
        params, defaults = _parse_params(params_str)
        body = _extract_body(full_stmt, "def", "end")
        method = RubyMethod(name, params, body, defaults, self._current_class)
        self._current_class.class_methods[name] = method
        return method

    # ------------------------------------------------------------------
    # control flow
    # ------------------------------------------------------------------

    def _exec_if_block(self, full_stmt: str) -> Any:  # noqa: C901
        # Split into if/elsif/else/end segments
        branches = _split_if_branches(full_stmt)
        for kind, condition, body in branches:
            if kind == "else":
                stmts = _split_statements(body)
                return self._exec_stmts(stmts)
            val = self._eval(condition)
            truthy = _is_truthy(val)
            if kind == "unless":
                truthy = not truthy
            if truthy:
                stmts = _split_statements(body)
                return self._exec_stmts(stmts)
        return NIL

    def _exec_while(self, keyword: str, condition: str, full_stmt: str) -> Any:
        body = _extract_body(full_stmt, keyword, "end")
        stmts = _split_statements(body)
        iters = 0
        while True:
            val = self._eval(condition)
            cond_met = _is_truthy(val) if keyword == "while" else not _is_truthy(val)
            if not cond_met:
                break
            try:
                self._exec_stmts(stmts)
            except _BreakSignal as b:
                return b.value
            except _NextSignal:
                pass
            iters += 1
            if iters > self.MAX_ITERATIONS:
                raise RubyError("infinite loop detected (while)")
        return NIL

    def _exec_for(self, var: str, iterable_expr: str, full_stmt: str) -> Any:
        body = _extract_body(full_stmt, "for", "end")
        stmts = _split_statements(body)
        iterable = self._to_iterable(self._eval(iterable_expr))
        for item in iterable:
            self._set_var(var, item)
            try:
                self._exec_stmts(stmts)
            except _BreakSignal as b:
                return b.value
            except _NextSignal:
                pass
        return NIL

    def _exec_loop(self, full_stmt: str) -> Any:
        body = _extract_body(full_stmt, "loop do", "end")
        stmts = _split_statements(body)
        iters = 0
        while True:
            try:
                self._exec_stmts(stmts)
            except _BreakSignal as b:
                return b.value
            except _NextSignal:
                pass
            iters += 1
            if iters > self.MAX_ITERATIONS:
                raise RubyError("infinite loop detected (loop)")

    def _exec_begin(self, full_stmt: str) -> Any:
        # begin ... rescue ExceptionClass => var ... ensure ... end
        parts = _split_begin_rescue(full_stmt)
        try:
            stmts = _split_statements(parts.get("begin", ""))
            return self._exec_stmts(stmts)
        except (RubyError, Exception) as exc:
            rescue_var = parts.get("rescue_var")
            if rescue_var:
                self._set_var(rescue_var, str(exc))
            rescue_body = parts.get("rescue", "")
            if rescue_body:
                stmts = _split_statements(rescue_body)
                return self._exec_stmts(stmts)
        finally:
            ensure_body = parts.get("ensure", "")
            if ensure_body:
                stmts = _split_statements(ensure_body)
                self._exec_stmts(stmts)
        return NIL

    # ------------------------------------------------------------------
    # assignment
    # ------------------------------------------------------------------

    def _exec_assign(self, name: str, op: str, expr: str) -> Any:
        if op == "||=":
            current = self._get_var(name)
            if _is_truthy(current):
                return current
            value = self._eval(expr)
            self._set_var(name, value)
            return value
        if op == "&&=":
            current = self._get_var(name)
            if not _is_truthy(current):
                return current
            value = self._eval(expr)
            self._set_var(name, value)
            return value
        value = self._eval(expr)
        if op != "=":
            current = self._get_var(name)
            op_char = op[0]
            value = _apply_op(current, op_char, value)
        self._set_var(name, value)
        return value

    def _exec_index_assign(self, name: str, key_expr: str, val_expr: str, op: str = "=") -> Any:
        obj = self._get_var(name)
        key = self._eval(key_expr)
        if op in ("||=", "&&="):
            # Conditional assignment
            if isinstance(obj, list) and isinstance(key, int):
                n = len(obj)
                real_idx = key if key >= 0 else n + key
                current = obj[real_idx] if 0 <= real_idx < n else NIL
            elif isinstance(obj, dict):
                current = obj.get(key, NIL)
            else:
                current = NIL
            if op == "||=" and _is_truthy(current):
                return current
            if op == "&&=" and not _is_truthy(current):
                return current
        val = self._eval(val_expr)
        if op not in ("=", "||=", "&&="):
            # Compound assignment: get current value then apply operator
            if isinstance(obj, list) and isinstance(key, int):
                n = len(obj)
                real_idx = key if key >= 0 else n + key
                current = obj[real_idx] if 0 <= real_idx < n else NIL
            elif isinstance(obj, dict):
                current = obj.get(key, NIL)
            else:
                current = NIL
            val = _apply_op(current, op[0], val)
        if isinstance(obj, list):
            if isinstance(key, int):
                n = len(obj)
                real_idx = key if key >= 0 else n + key
                if real_idx < 0:
                    return NIL  # out of range
                while len(obj) <= real_idx:
                    obj.append(NIL)
                obj[real_idx] = val
        elif isinstance(obj, dict):
            obj[key] = val
        return val

    # ------------------------------------------------------------------
    # print
    # ------------------------------------------------------------------

    def _exec_print(self, cmd: str, args_str: str, full_stmt: str) -> Any:
        if not args_str:
            if cmd == "puts":
                self._emit("")
            return NIL
        vals = self._eval_arg_list(args_str)
        if cmd == "puts":
            for v in vals:
                if isinstance(v, list):
                    for item in v:
                        self._emit(_ruby_to_s(item))
                else:
                    self._emit(_ruby_to_s(v))
        elif cmd == "print":
            self._emit("".join(_ruby_to_s(v) for v in vals))
        elif cmd in ("p", "pp"):
            for v in vals:
                self._emit(_ruby_inspect(v))
        return vals[0] if vals else NIL

    def _exec_printf(self, args_str: str) -> Any:
        vals = self._eval_arg_list(args_str)
        if not vals:
            return NIL
        fmt = _ruby_to_s(vals[0])
        rest = vals[1:]
        result = _sprintf(fmt, rest)
        self._emit(result)
        return NIL

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval(self, expr: str) -> Any:  # noqa: C901
        if expr is None:
            return NIL
        expr = expr.strip()
        if not expr:
            return NIL

        # nil / true / false
        if expr == "nil":
            return NIL
        if expr == "true":
            return True
        if expr == "false":
            return False

        # integer
        m = re.match(r"^-?\d+$", expr)
        if m:
            return int(expr)

        # float
        m = re.match(r"^-?\d+\.\d+$", expr)
        if m:
            return float(expr)

        # string literal (double-quoted with interpolation)
        if expr.startswith('"') and expr.endswith('"') and len(expr) >= 2:
            return self._eval_dstring(expr[1:-1])

        # string literal (single-quoted, no interpolation)
        if expr.startswith("'") and expr.endswith("'") and len(expr) >= 2:
            return expr[1:-1].replace("\\'", "'")

        # symbol :name
        m = re.match(r"^:(\w+)$", expr)
        if m:
            return RubySymbol(m.group(1))

        # range (1..10) or (1...10)
        m = re.match(r"^\(?(.+?)(\.\.\.?)(.*?)\)?$", expr)
        if m and not _has_unbalanced_parens(expr[1:-1] if expr.startswith("(") else expr):
            start = self._eval(m.group(1).strip())
            stop = self._eval(m.group(3).strip())
            exclusive = m.group(2) == "..."
            return RubyRange(start, stop, exclusive)

        # array literal [...]
        if expr.startswith("[") and expr.endswith("]"):
            inner = expr[1:-1].strip()
            if not inner:
                return []
            items = _split_args(inner)
            return [self._eval(x.strip()) for x in items]

        # hash literal {...}
        if expr.startswith("{") and expr.endswith("}"):
            return self._eval_hash(expr[1:-1])

        # parenthesised expression
        if expr.startswith("(") and expr.endswith(")"):
            inner = expr[1:-1]
            if _balanced(inner):
                return self._eval(inner)

        # unary operators
        if expr.startswith("!"):
            return not _is_truthy(self._eval(expr[1:]))
        if expr.startswith("-") and len(expr) > 1 and expr[1:].strip()[0].isalpha():
            return -self._eval(expr[1:])

        # binary operators (low precedence, split on rightmost op outside parens)
        # Skip for multi-line expressions (blocks/method calls spanning lines)
        if "\n" not in expr:
            for op in ("||", "&&", "or ", "and ", "==", "!=", "<=>",
                       ">=", "<=", ">", "<",
                       "<<", "+", "-", "*", "**", "/", "%"):
                idx = _rfind_op(expr, op)
                if idx != -1:
                    left = expr[:idx].strip()
                    right = expr[idx + len(op):].strip()
                    if left and right:
                        return self._eval_binop(left, op.strip(), right)

        # Method chain or single method call
        return self._eval_expr(expr)

    def _eval_dstring(self, raw: str) -> str:
        """Evaluate double-quoted string with #{...} interpolation."""
        result = []
        i = 0
        while i < len(raw):
            if raw[i] == "\\" and i + 1 < len(raw):
                esc = raw[i + 1]
                result.append({"n": "\n", "t": "\t", "r": "\r", '"': '"', "\\": "\\"}.get(esc, raw[i + 1]))
                i += 2
            elif raw[i : i + 2] == "#{":
                # Find matching closing } respecting nested braces
                depth = 1
                j = i + 2
                while j < len(raw) and depth > 0:
                    if raw[j] == "{":
                        depth += 1
                    elif raw[j] == "}":
                        depth -= 1
                    j += 1
                inner = raw[i + 2 : j - 1]
                result.append(_ruby_to_s(self._eval(inner)))
                i = j
            else:
                result.append(raw[i])
                i += 1
        return "".join(result)

    def _eval_hash(self, inner: str) -> dict:
        h: dict = {}
        if not inner.strip():
            return h
        pairs = _split_args(inner)
        for pair in pairs:
            pair = pair.strip()
            # symbol key: name: val
            m = re.match(r"^(\w+):\s*(.+)$", pair)
            if m:
                h[RubySymbol(m.group(1))] = self._eval(m.group(2))
                continue
            # rocket: key => val
            m = re.match(r"^(.+?)\s*=>\s*(.+)$", pair)
            if m:
                h[self._eval(m.group(1).strip())] = self._eval(m.group(2).strip())
                continue
        return h

    def _eval_binop(self, left_s: str, op: str, right_s: str) -> Any:
        if op in ("&&", "and"):
            lv = self._eval(left_s)
            return lv if not _is_truthy(lv) else self._eval(right_s)
        if op in ("||", "or"):
            lv = self._eval(left_s)
            return lv if _is_truthy(lv) else self._eval(right_s)
        lv = self._eval(left_s)
        rv = self._eval(right_s)
        return _apply_op(lv, op, rv)

    def _eval_expr(self, expr: str) -> Any:  # noqa: C901
        """Evaluate a method call chain or simple identifier."""
        expr = expr.strip()

        # Multiple assignment: a, b = expr
        if re.match(r"^\w+\s*,\s*\w+", expr) and " = " in expr:
            lhs, rhs = expr.split(" = ", 1)
            names = [n.strip() for n in lhs.split(",")]
            val = self._eval(rhs.strip())
            lst = val if isinstance(val, list) else [val]
            for i, nm in enumerate(names):
                self._set_var(nm, lst[i] if i < len(lst) else NIL)
            return lst

        # Compound literals / method calls: parse left-to-right
        return self._eval_chain(expr)

    def _eval_chain(self, expr: str) -> Any:  # noqa: C901
        """Parse and evaluate method chains like obj.method(args).other(args)."""
        # Standalone method call with args: name(args) or name args
        m = re.match(r"^([a-z_]\w*[\?!]?)\s*\((.*)?\)\s*$", expr, re.DOTALL)
        if m and _balanced(m.group(2) or ""):
            return self._call_function(m.group(1), m.group(2) or "")

        # No-arg identifier
        if re.match(r"^[a-z_]\w*[\?!]?$", expr):
            v = self._get_var(expr)
            if v is not NIL:
                return v
            # Could be a no-arg method call
            return self._call_function(expr, "")

        # Constant / class name
        if re.match(r"^[A-Z]\w*$", expr):
            v = self._get_var(expr)
            return v if v is not NIL else NIL

        # Global / instance var
        if re.match(r"^[@$]\w+$", expr):
            return self._get_var(expr)

        # Array/hash subscript access: name[idx]
        m = re.match(r"^([@$]?\w+)\[(.+)\]$", expr)
        if m:
            container = self._get_var(m.group(1))
            idx = self._eval(m.group(2).strip())
            if isinstance(container, list):
                if isinstance(idx, int):
                    n = len(container)
                    # Support negative indices like Ruby
                    real_idx = idx if idx >= 0 else n + idx
                    return container[real_idx] if 0 <= real_idx < n else NIL
                return NIL
            if isinstance(container, dict):
                return container.get(idx, NIL)
            return NIL

        # Method chain: receiver.method(args)
        dot = _find_dot(expr)
        if dot != -1:
            receiver_s = expr[:dot].strip()
            rest = expr[dot + 1:]
            receiver = self._eval(receiver_s)
            return self._eval_method_chain(receiver, rest)

        return NIL

    def _eval_method_chain(self, receiver: Any, rest: str) -> Any:  # noqa: C901
        """Evaluate remaining method chain after the first dot."""
        # Extract method name and optional explicit (args).
        # The remainder may contain a trailing {...} or do...end block and/or
        # a further .method chain — we don't need to capture those here because
        # _call_method receives the full `rest` string and uses _extract_block
        # to find any block.
        m = re.match(r"^(\w+[\?!]?)\s*(?:\(([^)]*)\))?", rest)
        if not m:
            return NIL
        method_name = m.group(1)
        args_str = m.group(2) or ""
        after_args = rest[m.end():]

        # Look for a tail chain: anything after the optional block
        tail = ""
        stripped_after = after_args.strip()
        if stripped_after.startswith("{"):
            # Inline {..} block — skip to the closing }
            depth = 0
            for ci, ch in enumerate(stripped_after):
                if ch == "{":
                    depth += 1
                elif ch == "}":
                    depth -= 1
                    if depth == 0:
                        tail_part = stripped_after[ci + 1:].strip()
                        if tail_part.startswith("."):
                            tail = tail_part[1:]
                        break
        elif re.match(r"do\b", stripped_after):
            # do...end block — find the closing end
            end_m = re.search(r"\bend\s*$", stripped_after, re.MULTILINE)
            if end_m:
                tail_part = stripped_after[end_m.end():].strip()
                if tail_part.startswith("."):
                    tail = tail_part[1:]
        elif stripped_after.startswith("."):
            tail = stripped_after[1:]

        result = self._call_method(receiver, method_name, args_str, rest)

        if tail:
            result = self._eval_method_chain(result, tail)
        return result

    # ------------------------------------------------------------------
    # Function / method dispatch
    # ------------------------------------------------------------------

    def _call_function(self, name: str, args_str: str) -> Any:  # noqa: C901
        args = self._eval_arg_list(args_str) if args_str.strip() else []

        # Built-in kernel methods
        if name == "puts":
            return self._exec_print("puts", args_str, "")
        if name == "print":
            return self._exec_print("print", args_str, "")
        if name in ("p", "pp"):
            return self._exec_print(name, args_str, "")
        if name == "printf":
            return self._exec_printf(args_str)
        if name == "rand":
            if args:
                n = args[0]
                if isinstance(n, int) and n > 0:
                    return random.randint(0, n - 1)
                if isinstance(n, float):
                    return random.uniform(0, n)
            return random.random()
        if name == "srand":
            if args:
                random.seed(args[0])
            return NIL
        if name == "exit":
            raise RubyStopExecution()
        if name in ("sleep", "require", "require_relative", "load", "include"):
            return NIL
        if name == "raise":
            msg = _ruby_to_s(args[0]) if args else "RuntimeError"
            raise RubyError(msg)
        if name == "Integer":
            return int(args[0]) if args else 0
        if name == "Float":
            return float(args[0]) if args else 0.0
        if name == "String":
            return _ruby_to_s(args[0]) if args else ""
        if name == "Array":
            v = args[0] if args else NIL
            if isinstance(v, list):
                return v
            if isinstance(v, RubyRange):
                return v.to_list()
            return [v] if v is not NIL else []
        if name == "sprintf" or name == "format":
            if args:
                return _sprintf(_ruby_to_s(args[0]), args[1:])
            return ""

        # Turtle graphics
        turtle_result = self._try_turtle(name, args)
        if turtle_result is not _UNSET:
            return turtle_result

        # User-defined method / proc / lambda in scope
        v = self._get_var(name)
        if isinstance(v, RubyMethod):
            return self._invoke_method(v, args, self._current_self)
        if isinstance(v, RubyProc):
            return self._invoke_proc(v, args)

        # Class constructor
        cls = self._classes.get(name) or self._constants.get(name)
        if isinstance(cls, RubyClass):
            return self._instantiate(cls, args)

        return NIL

    def _call_method(  # noqa: C901
        self, receiver: Any, name: str, args_str: str, full_chain: str
    ) -> Any:
        """Call a method on a receiver object."""
        args = self._eval_arg_list(args_str) if args_str.strip() else []

        # Block extraction for iterator methods
        block = _extract_block(full_chain)

        # RubyObject (user class instance)
        if isinstance(receiver, RubyObject):
            method = receiver.klass.find_method(name)
            if method:
                old_self = self._current_self
                self._current_self = receiver
                try:
                    result = self._invoke_method(method, args, receiver)
                finally:
                    self._current_self = old_self
                return result
            # attr_reader / attr_writer generated accessors
            if name in receiver.ivars:
                return receiver.ivars[name]
            if name.endswith("=") and len(name) > 1:
                ivar = "@" + name[:-1]
                val = args[0] if args else NIL
                receiver.ivars[ivar] = val
                return val

        # RubyClass (class method call)
        if isinstance(receiver, RubyClass):
            if name == "new":
                return self._instantiate(receiver, args)
            cm = receiver.class_methods.get(name)
            if cm:
                return self._invoke_method(cm, args, receiver)

        # Math module
        if receiver is self:  # Math constant resolves to self
            return self._call_math(name, args)
        if isinstance(receiver, type(self)):
            return self._call_math(name, args)

        # Integer methods
        if isinstance(receiver, int):
            return self._call_int_method(receiver, name, args, block)

        # Float methods
        if isinstance(receiver, float):
            return self._call_float_method(receiver, name, args)

        # String methods
        if isinstance(receiver, str):
            return self._call_str_method(receiver, name, args, block)

        # Array methods
        if isinstance(receiver, list):
            return self._call_array_method(receiver, name, args, block)

        # Hash methods
        if isinstance(receiver, dict):
            return self._call_hash_method(receiver, name, args, block)

        # Range methods
        if isinstance(receiver, RubyRange):
            return self._call_range_method(receiver, name, args, block)

        # Symbol
        if isinstance(receiver, RubySymbol):
            if name == "to_s":
                return str(receiver)
            if name == "inspect":
                return repr(receiver)
            if name == "to_proc":
                return RubyProc([":item"], f":item.{receiver.name}", {})

        # Nil
        if isinstance(receiver, _RubyNil):
            if name == "nil?":
                return True
            if name in ("to_s", "inspect"):
                return ""
            return NIL

        # Boolean
        if isinstance(receiver, bool):
            if name == "to_s":
                return "true" if receiver else "false"
            if name == "!":
                return not receiver
        return NIL

    def _call_math(self, name: str, args: list) -> Any:
        fns = {
            "sqrt": math.sqrt, "sin": math.sin, "cos": math.cos,
            "tan": math.tan, "asin": math.asin, "acos": math.acos,
            "atan": math.atan, "atan2": math.atan2,
            "log": math.log, "log2": math.log2, "log10": math.log10,
            "exp": math.exp, "cbrt": math.cbrt if hasattr(math, "cbrt") else lambda x: x ** (1 / 3),
            "hypot": math.hypot, "ceil": math.ceil, "floor": math.floor,
        }
        if name == "PI":
            return math.pi
        if name == "E":
            return math.e
        fn = fns.get(name)
        if fn and args:
            try:
                return fn(*args)  # type: ignore[operator]
            except Exception:
                return 0.0
        return NIL

    def _call_int_method(  # noqa: C901
        self, receiver: int, name: str, args: list, block: "RubyProc | None"
    ) -> Any:
        if name == "times":
            results = []
            for i in range(receiver):
                try:
                    v = self._call_block(block, [i]) if block else i
                    results.append(v)
                except _BreakSignal as b:
                    return b.value
                except _NextSignal:
                    pass
            return results if block else receiver
        if name == "upto":
            end = args[0] if args else receiver
            results = []
            for i in range(receiver, int(end) + 1):
                try:
                    v = self._call_block(block, [i]) if block else i
                    results.append(v)
                except _BreakSignal as b:
                    return b.value
            return results
        if name == "downto":
            end = args[0] if args else receiver
            results = []
            for i in range(receiver, int(end) - 1, -1):
                try:
                    v = self._call_block(block, [i]) if block else i
                    results.append(v)
                except _BreakSignal as b:
                    return b.value
            return results
        if name == "step":
            stop = args[0] if args else receiver
            step = args[1] if len(args) > 1 else 1
            i = receiver
            results = []
            while i <= int(stop):
                try:
                    v = self._call_block(block, [i]) if block else i
                    results.append(v)
                except _BreakSignal as b:
                    return b.value
                i += int(step)
            return results
        if name == "to_s":
            return str(receiver)
        if name == "to_f":
            return float(receiver)
        if name == "to_i":
            return receiver
        if name == "even?":
            return receiver % 2 == 0
        if name == "odd?":
            return receiver % 2 != 0
        if name == "zero?":
            return receiver == 0
        if name == "abs":
            return abs(receiver)
        if name == "chr":
            return chr(receiver)
        if name in ("nil?",):
            return False
        if name == "between?":
            return args[0] <= receiver <= args[1] if len(args) >= 2 else False
        if name == "ceil":
            return receiver
        if name == "floor":
            return receiver
        if name == "divmod":
            n = int(args[0]) if args else 1
            return [receiver // n, receiver % n]
        if name == "gcd":
            return math.gcd(receiver, int(args[0])) if args else receiver
        if name == "lcm":
            n = int(args[0]) if args else 1
            return (abs(receiver * n) // math.gcd(receiver, n)) if n else 0
        if name == "pow":
            return receiver ** int(args[0]) if args else receiver
        if name in ("inspect", "pretty_print"):
            return str(receiver)
        return NIL

    def _call_float_method(self, receiver: float, name: str, args: list) -> Any:
        if name == "to_i":
            return int(receiver)
        if name == "to_f":
            return receiver
        if name == "to_s":
            return str(receiver)
        if name == "abs":
            return abs(receiver)
        if name == "ceil":
            n = int(args[0]) if args else 0
            factor = 10 ** n
            return math.ceil(receiver * factor) / factor
        if name == "floor":
            n = int(args[0]) if args else 0
            factor = 10 ** n
            return math.floor(receiver * factor) / factor
        if name == "round":
            n = int(args[0]) if args else 0
            return round(receiver, n)
        if name == "nan?":
            return math.isnan(receiver)
        if name == "infinite?":
            return 1 if math.isinf(receiver) and receiver > 0 else -1 if math.isinf(receiver) else NIL
        if name == "zero?":
            return receiver == 0.0
        return NIL

    def _call_str_method(  # noqa: C901
        self, receiver: str, name: str, args: list, block: "RubyProc | None"
    ) -> Any:
        if name in ("length", "size"):
            return len(receiver)
        if name == "upcase":
            return receiver.upper()
        if name == "downcase":
            return receiver.lower()
        if name == "capitalize":
            return receiver.capitalize()
        if name == "reverse":
            return receiver[::-1]
        if name == "strip":
            return receiver.strip()
        if name == "lstrip":
            return receiver.lstrip()
        if name == "rstrip":
            return receiver.rstrip()
        if name == "chomp":
            return receiver.rstrip("\n\r")
        if name == "chop":
            return receiver[:-1] if receiver else ""
        if name == "empty?":
            return len(receiver) == 0
        if name == "to_i":
            m = re.match(r"^-?\d+", receiver)
            return int(m.group()) if m else 0
        if name == "to_f":
            m = re.match(r"^-?\d+\.?\d*", receiver)
            return float(m.group()) if m else 0.0
        if name == "to_s":
            return receiver
        if name == "to_sym":
            return RubySymbol(receiver)
        if name == "inspect":
            return repr(receiver)
        if name == "include?":
            return (args[0] if args else "") in receiver
        if name == "start_with?":
            return any(receiver.startswith(_ruby_to_s(a)) for a in args)
        if name == "end_with?":
            return any(receiver.endswith(_ruby_to_s(a)) for a in args)
        if name == "index":
            sub = _ruby_to_s(args[0]) if args else ""
            idx = receiver.find(sub)
            return idx if idx != -1 else NIL
        if name == "split":
            sep = _ruby_to_s(args[0]) if args else None
            limit = int(args[1]) if len(args) > 1 else -1
            if sep is None:
                return receiver.split()
            return receiver.split(sep, limit)
        if name in ("gsub", "sub"):
            pattern = _ruby_to_s(args[0]) if args else ""
            repl = _ruby_to_s(args[1]) if len(args) > 1 else ""
            try:
                if name == "gsub":
                    return re.sub(pattern, repl, receiver)
                return re.sub(pattern, repl, receiver, count=1)
            except re.error:
                return receiver.replace(pattern, repl)
        if name == "match?":
            pattern = _ruby_to_s(args[0]) if args else ""
            try:
                return bool(re.search(pattern, receiver))
            except re.error:
                return False
        if name == "chars":
            return list(receiver)
        if name == "bytes":
            return list(receiver.encode())
        if name == "lines":
            return receiver.splitlines(keepends=True)
        if name in ("center", "ljust", "rjust"):
            width = int(args[0]) if args else len(receiver)
            pad = _ruby_to_s(args[1]) if len(args) > 1 else " "
            if name == "center":
                return receiver.center(width, pad[0] if pad else " ")
            if name == "ljust":
                return receiver.ljust(width, pad[0] if pad else " ")
            return receiver.rjust(width, pad[0] if pad else " ")
        if name == "slice":
            if len(args) == 2:
                start, len_ = int(args[0]), int(args[1])
                return receiver[start : start + len_]
            if args:
                return receiver[int(args[0])]
            return NIL
        if name == "tr":
            from_s = _ruby_to_s(args[0]) if args else ""
            to_s = _ruby_to_s(args[1]) if len(args) > 1 else ""
            table = str.maketrans(from_s, to_s.ljust(len(from_s), to_s[-1] if to_s else " "))
            return receiver.translate(table)
        if name == "count":
            chars = _ruby_to_s(args[0]) if args else ""
            return sum(receiver.count(c) for c in chars)
        if name == "delete":
            chars = _ruby_to_s(args[0]) if args else ""
            result = receiver
            for c in chars:
                result = result.replace(c, "")
            return result
        if name == "replace":
            return _ruby_to_s(args[0]) if args else ""
        if name == "each_char":
            for ch in receiver:
                self._call_block(block, [ch])
            return receiver
        if name == "each_line":
            for ln in receiver.splitlines(keepends=True):
                self._call_block(block, [ln])
            return receiver
        if name == "frozen?":
            return True
        if name == "freeze":
            return receiver
        if name in ("nil?",):
            return False
        if name == "*":
            return receiver * (int(args[0]) if args else 1)
        if name == "+":
            return receiver + _ruby_to_s(args[0]) if args else receiver
        if name == "[]":
            idx = args[0] if args else 0
            if isinstance(idx, int):
                return receiver[idx] if 0 <= idx < len(receiver) else NIL
            return NIL
        if name == "format":
            return _sprintf(receiver, args)
        if name == "encode":
            return receiver  # simplified
        if name == "hex":
            try:
                return int(receiver, 16)
            except ValueError:
                return 0
        if name == "oct":
            try:
                return int(receiver, 8)
            except ValueError:
                return 0
        return NIL

    def _call_array_method(  # noqa: C901
        self, receiver: list, name: str, args: list, block: "RubyProc | None"
    ) -> Any:
        if name in ("length", "size", "count"):
            if not block and not args:
                return len(receiver)
            if block:
                return sum(1 for x in receiver if _is_truthy(self._call_block(block, [x])))
        if name == "empty?":
            return len(receiver) == 0
        if name == "first":
            n = int(args[0]) if args else None
            return receiver[:n] if n is not None else (receiver[0] if receiver else NIL)
        if name == "last":
            n = int(args[0]) if args else None
            return receiver[-n:] if n is not None else (receiver[-1] if receiver else NIL)
        if name in ("push", "append", "<<"):
            for a in args:
                receiver.append(a)
            return receiver
        if name == "pop":
            return receiver.pop() if receiver else NIL
        if name == "shift":
            return receiver.pop(0) if receiver else NIL
        if name == "unshift":
            for a in reversed(args):
                receiver.insert(0, a)
            return receiver
        if name == "each":
            for item in list(receiver):
                try:
                    self._call_block(block, [item]) if block else None
                except _BreakSignal:
                    break
                except _NextSignal:
                    pass
            return receiver
        if name == "each_with_index":
            for i, item in enumerate(list(receiver)):
                try:
                    self._call_block(block, [item, i]) if block else None
                except _BreakSignal:
                    break
            return receiver
        if name == "each_with_object":
            obj = args[0] if args else {}
            for item in list(receiver):
                if block:
                    self._call_block(block, [item, obj])
            return obj
        if name == "map" or name == "collect":
            if not block:
                return receiver[:]
            return [self._call_block(block, [x]) for x in receiver]
        if name in ("select", "filter", "find_all"):
            if not block:
                return receiver[:]
            return [x for x in receiver if _is_truthy(self._call_block(block, [x]))]
        if name in ("reject",):
            if not block:
                return receiver[:]
            return [x for x in receiver if not _is_truthy(self._call_block(block, [x]))]
        if name in ("reduce", "inject"):
            if not receiver:
                return NIL
            start = args[0] if args else receiver[0]
            lst = receiver if args else receiver[1:]
            if block:
                acc = start
                for x in lst:
                    acc = self._call_block(block, [acc, x])
                return acc
            # Symbol shorthand inject(:+)
            if args and isinstance(args[0], RubySymbol):
                op = str(args[0])
                acc = lst[0] if lst else NIL
                for x in lst[1:]:
                    acc = _apply_op(acc, op, x)
                return acc
            return start
        if name == "flat_map" or name == "collect_concat":
            if not block:
                return list(receiver)
            result = []
            for x in receiver:
                v = self._call_block(block, [x])
                if isinstance(v, list):
                    result.extend(v)
                else:
                    result.append(v)
            return result
        if name in ("any?",):
            if not block:
                return any(_is_truthy(x) for x in receiver)
            return any(_is_truthy(self._call_block(block, [x])) for x in receiver)
        if name in ("all?",):
            if not block:
                return all(_is_truthy(x) for x in receiver)
            return all(_is_truthy(self._call_block(block, [x])) for x in receiver)
        if name in ("none?",):
            if not block:
                return not any(_is_truthy(x) for x in receiver)
            return not any(_is_truthy(self._call_block(block, [x])) for x in receiver)
        if name == "find" or name == "detect":
            if not block:
                return NIL
            for x in receiver:
                if _is_truthy(self._call_block(block, [x])):
                    return x
            return NIL
        if name in ("sort",):
            try:
                if not block:
                    return sorted(receiver, key=lambda x: (str(type(x)), x))
                return sorted(receiver)
            except TypeError:
                return sorted(receiver, key=str)
        if name == "sort_by":
            if not block:
                return receiver[:]
            return sorted(receiver, key=lambda x: self._call_block(block, [x]))
        if name == "min":
            if not receiver:
                return NIL
            try:
                return min(receiver)
            except TypeError:
                return min(receiver, key=str)
        if name == "max":
            if not receiver:
                return NIL
            try:
                return max(receiver)
            except TypeError:
                return max(receiver, key=str)
        if name == "min_by":
            if not block or not receiver:
                return NIL
            return min(receiver, key=lambda x: self._call_block(block, [x]))
        if name == "max_by":
            if not block or not receiver:
                return NIL
            return max(receiver, key=lambda x: self._call_block(block, [x]))
        if name == "sum":
            init = args[0] if args else 0
            return sum(receiver, init)
        if name in ("include?", "member?"):
            return (args[0] if args else None) in receiver
        if name == "index" or name == "find_index":
            target = args[0] if args else None
            if block:
                for i, x in enumerate(receiver):
                    if _is_truthy(self._call_block(block, [x])):
                        return i
                return NIL
            return receiver.index(target) if target in receiver else NIL
        if name == "uniq":
            seen: list = []
            for x in receiver:
                if x not in seen:
                    seen.append(x)
            return seen
        if name == "flatten":
            depth = int(args[0]) if args else -1
            return _flatten(receiver, depth)
        if name == "compact":
            return [x for x in receiver if not isinstance(x, _RubyNil) and x is not None]
        if name == "zip":
            others = [self._to_iterable(a) for a in args]
            return [[x] + [o[i] if i < len(o) else NIL for o in others] for i, x in enumerate(receiver)]
        if name == "join":
            sep = _ruby_to_s(args[0]) if args else ""
            return sep.join(_ruby_to_s(x) for x in receiver)
        if name == "reverse":
            return list(reversed(receiver))
        if name == "rotate":
            n = int(args[0]) if args else 1
            if not receiver:
                return []
            n = n % len(receiver)
            return receiver[n:] + receiver[:n]
        if name == "take":
            n = int(args[0]) if args else 0
            return receiver[:n]
        if name == "drop":
            n = int(args[0]) if args else 0
            return receiver[n:]
        if name == "sample":
            return random.choice(receiver) if receiver else NIL
        if name == "shuffle":
            lst = receiver[:]
            random.shuffle(lst)
            return lst
        if name == "combination":
            import itertools
            n = int(args[0]) if args else 0
            return [list(c) for c in itertools.combinations(receiver, n)]
        if name == "permutation":
            import itertools
            n = int(args[0]) if args else len(receiver)
            return [list(p) for p in itertools.permutations(receiver, n)]
        if name == "product":
            import itertools
            others = [self._to_iterable(a) for a in args]
            return [list(p) for p in itertools.product(receiver, *others)]
        if name in ("delete", "delete_if"):
            target = args[0] if args else NIL
            orig_len = len(receiver)
            receiver[:] = [x for x in receiver if x != target]
            return len(receiver) < orig_len
        if name == "concat":
            for a in args:
                if isinstance(a, list):
                    receiver.extend(a)
            return receiver
        if name == "insert":
            idx = int(args[0]) if args else 0
            for i, v in enumerate(args[1:]):
                receiver.insert(idx + i, v)
            return receiver
        if name == "to_a":
            return receiver
        if name == "to_s" or name == "inspect":
            return "[" + ", ".join(_ruby_inspect(x) for x in receiver) + "]"
        if name == "each_slice":
            n = int(args[0]) if args else 1
            slices = [receiver[i : i + n] for i in range(0, len(receiver), n)]
            if block:
                for s in slices:
                    self._call_block(block, [s])
            return slices
        if name == "each_cons":
            n = int(args[0]) if args else 1
            for i in range(len(receiver) - n + 1):
                if block:
                    self._call_block(block, [receiver[i : i + n]])
            return NIL
        if name == "tally":
            tally: dict = {}
            for x in receiver:
                tally[x] = tally.get(x, 0) + 1
            return tally
        if name == "group_by":
            if not block:
                return {}
            groups: dict = {}
            for x in receiver:
                key = self._call_block(block, [x])
                groups.setdefault(key, []).append(x)
            return groups
        if name in ("nil?",):
            return False
        if name == "*":
            n = args[0] if args else 1
            if isinstance(n, int):
                return receiver * n
            return _ruby_to_s(n).join(_ruby_to_s(x) for x in receiver)
        if name == "+":
            other = args[0] if args else []
            return receiver + (other if isinstance(other, list) else [])
        if name == "-":
            other = args[0] if args else []
            return [x for x in receiver if x not in other]
        if name == "&":
            other = args[0] if args else []
            return [x for x in receiver if x in other]
        if name == "|":
            other = args[0] if args else []
            result = receiver[:]
            for x in other:
                if x not in result:
                    result.append(x)
            return result
        if name == "map!":
            if block:
                for i, x in enumerate(receiver):
                    receiver[i] = self._call_block(block, [x])
            return receiver
        if name == "select!":
            if block:
                receiver[:] = [x for x in receiver if _is_truthy(self._call_block(block, [x]))]
            return receiver
        if name == "count":
            if block:
                return sum(1 for x in receiver if _is_truthy(self._call_block(block, [x])))
            if args:
                return receiver.count(args[0])
            return len(receiver)
        if name == "chunk":
            if not block:
                return []
            chunks = []
            current_key = _UNSET
            current_group: list = []
            for x in receiver:
                key = self._call_block(block, [x])
                if key == current_key:
                    current_group.append(x)
                else:
                    if current_key is not _UNSET:
                        chunks.append([current_key, current_group])
                    current_key = key
                    current_group = [x]
            if current_group:
                chunks.append([current_key, current_group])
            return chunks
        return NIL

    def _call_hash_method(  # noqa: C901
        self, receiver: dict, name: str, args: list, block: "RubyProc | None"
    ) -> Any:
        if name == "keys":
            return list(receiver.keys())
        if name == "values":
            return list(receiver.values())
        if name in ("length", "size", "count"):
            return len(receiver)
        if name == "empty?":
            return len(receiver) == 0
        if name == "has_key?" or name == "include?" or name == "key?" or name == "member?":
            return (args[0] if args else None) in receiver
        if name == "has_value?" or name == "value?":
            return (args[0] if args else None) in receiver.values()
        if name == "each" or name == "each_pair":
            for k, v in list(receiver.items()):
                if block:
                    self._call_block(block, [k, v])
            return receiver
        if name == "each_key":
            for k in list(receiver.keys()):
                if block:
                    self._call_block(block, [k])
            return receiver
        if name == "each_value":
            for v in list(receiver.values()):
                if block:
                    self._call_block(block, [v])
            return receiver
        if name == "map" or name == "collect":
            if not block:
                return list(receiver.items())
            return [self._call_block(block, [k, v]) for k, v in receiver.items()]
        if name in ("select", "filter"):
            if not block:
                return dict(receiver)
            return {k: v for k, v in receiver.items() if _is_truthy(self._call_block(block, [k, v]))}
        if name == "reject":
            if not block:
                return dict(receiver)
            return {k: v for k, v in receiver.items() if not _is_truthy(self._call_block(block, [k, v]))}
        if name == "merge":
            result = dict(receiver)
            for a in args:
                if isinstance(a, dict):
                    result.update(a)
            return result
        if name == "update":
            for a in args:
                if isinstance(a, dict):
                    receiver.update(a)
            return receiver
        if name == "delete":
            key = args[0] if args else NIL
            return receiver.pop(key, NIL)
        if name == "fetch":
            key = args[0] if args else NIL
            default = args[1] if len(args) > 1 else _UNSET
            if key in receiver:
                return receiver[key]
            if default is not _UNSET:
                return default
            if block:
                return self._call_block(block, [key])
            raise RubyError(f"KeyError: key not found: {_ruby_inspect(key)}")
        if name == "to_a":
            return [[k, v] for k, v in receiver.items()]
        if name == "to_s" or name == "inspect":
            pairs = ", ".join(f"{_ruby_inspect(k)}: {_ruby_inspect(v)}" for k, v in receiver.items())
            return "{" + pairs + "}"
        if name == "any?":
            if not block:
                return bool(receiver)
            return any(_is_truthy(self._call_block(block, [k, v])) for k, v in receiver.items())
        if name == "all?":
            if not block:
                return all(_is_truthy(v) for v in receiver.values())
            return all(_is_truthy(self._call_block(block, [k, v])) for k, v in receiver.items())
        if name == "none?":
            if not block:
                return not any(_is_truthy(v) for v in receiver.values())
            return not any(_is_truthy(self._call_block(block, [k, v])) for k, v in receiver.items())
        if name == "find" or name == "detect":
            if not block:
                return NIL
            for k, v in receiver.items():
                if _is_truthy(self._call_block(block, [k, v])):
                    return [k, v]
            return NIL
        if name == "min_by":
            if not block or not receiver:
                return NIL
            return min(receiver.items(), key=lambda kv: self._call_block(block, [kv[0], kv[1]]))
        if name == "max_by":
            if not block or not receiver:
                return NIL
            return max(receiver.items(), key=lambda kv: self._call_block(block, [kv[0], kv[1]]))
        if name == "transform_values":
            if not block:
                return dict(receiver)
            return {k: self._call_block(block, [v]) for k, v in receiver.items()}
        if name == "transform_keys":
            if not block:
                return dict(receiver)
            return {self._call_block(block, [k]): v for k, v in receiver.items()}
        if name == "group_by":
            if not block:
                return {}
            groups: dict = {}
            for k, v in receiver.items():
                key = self._call_block(block, [k, v])
                groups.setdefault(key, {})[k] = v
            return groups
        if name == "sort_by":
            if not block:
                return sorted(receiver.items())
            return sorted(receiver.items(), key=lambda kv: self._call_block(block, list(kv)))
        if name in ("nil?",):
            return False
        if name == "[]":
            key = args[0] if args else NIL
            return receiver.get(key, NIL)
        if name == "[]=":
            key = args[0] if args else NIL
            val = args[1] if len(args) > 1 else NIL
            receiver[key] = val
            return val
        if name == "key":
            target = args[0] if args else NIL
            for k, v in receiver.items():
                if v == target:
                    return k
            return NIL
        if name == "invert":
            return {v: k for k, v in receiver.items()}
        if name == "flatten":
            return list(sum(([k, v] for k, v in receiver.items()), []))
        return NIL

    def _call_range_method(
        self, receiver: RubyRange, name: str, args: list, block: "RubyProc | None"
    ) -> Any:
        lst = receiver.to_list()
        if name == "each":
            for x in lst:
                try:
                    self._call_block(block, [x]) if block else None
                except _BreakSignal:
                    break
            return receiver
        if name == "to_a":
            return lst
        if name in ("map", "collect"):
            return [self._call_block(block, [x]) for x in lst] if block else lst
        if name in ("select", "filter"):
            return [x for x in lst if _is_truthy(self._call_block(block, [x]))] if block else lst
        if name == "sum":
            return sum(lst)
        if name == "min":
            return min(lst) if lst else NIL
        if name == "max":
            return max(lst) if lst else NIL
        if name == "size" or name == "count":
            return len(lst)
        if name == "include?":
            return (args[0] if args else None) in receiver
        if name == "any?":
            if block:
                return any(_is_truthy(self._call_block(block, [x])) for x in lst)
            return bool(lst)
        if name == "all?":
            if block:
                return all(_is_truthy(self._call_block(block, [x])) for x in lst)
            return True
        if name == "reduce" or name == "inject":
            return self._call_array_method(lst, name, args, block)
        if name == "first":
            n = int(args[0]) if args else None
            return lst[:n] if n else (lst[0] if lst else NIL)
        if name == "last":
            n = int(args[0]) if args else None
            return lst[-n:] if n else (lst[-1] if lst else NIL)
        if name == "step":
            step = int(args[0]) if args else 1
            stepped = lst[::step]
            if block:
                for x in stepped:
                    self._call_block(block, [x])
            return stepped
        return NIL

    # ------------------------------------------------------------------
    # Block invocation
    # ------------------------------------------------------------------

    def _call_block(self, block: "RubyProc | None", args: list) -> Any:
        if block is None:
            return NIL
        bindings = dict(block.closure)
        # Ruby auto-splatting: if block expects multiple params but receives
        # a single array/tuple argument, destructure the array into params.
        effective_args = args
        if len(args) == 1 and isinstance(args[0], (list, tuple)) and len(block.params) > 1:
            effective_args = list(args[0])
        for i, param in enumerate(block.params):
            param = param.strip()
            if param:
                bindings[param] = effective_args[i] if i < len(effective_args) else NIL
        self._push_scope(bindings)
        try:
            stmts = _split_statements(block.body)
            return self._exec_stmts(stmts)
        except _ReturnSignal as r:
            return r.value
        except _NextSignal as n:
            return n.value
        finally:
            self._pop_scope()

    # ------------------------------------------------------------------
    # Method invocation
    # ------------------------------------------------------------------

    def _invoke_method(
        self, method: RubyMethod, args: list, receiver: Any
    ) -> Any:
        bindings: dict[str, Any] = {}
        for i, param in enumerate(method.params):
            if param.startswith("*"):
                bindings[param[1:]] = args[i:]
                break
            elif param.startswith("**"):
                # keyword args dict
                bindings[param[2:]] = args[i] if i < len(args) else {}
                break
            else:
                if i < len(args):
                    bindings[param] = args[i]
                elif param in method.defaults:
                    bindings[param] = self._eval(method.defaults[param])
                else:
                    bindings[param] = NIL
        old_self = self._current_self
        self._current_self = receiver
        self._push_scope(bindings)
        try:
            stmts = _split_statements(method.body)
            return self._exec_stmts(stmts)
        except _ReturnSignal as r:
            return r.value
        finally:
            self._pop_scope()
            self._current_self = old_self

    def _invoke_proc(self, proc: RubyProc, args: list) -> Any:
        return self._call_block(proc, args)

    def _instantiate(self, klass: RubyClass, args: list) -> RubyObject:
        obj = RubyObject(klass)
        # Collect attr_accessor/attr_reader/attr_writer from class body if already processed
        init_method = klass.find_method("initialize")
        if init_method:
            old_self = self._current_self
            self._current_self = obj
            self._push_scope()
            for i, param in enumerate(init_method.params):
                p = param.strip().lstrip("*")
                self._set_var(p, args[i] if i < len(args) else NIL)
            try:
                stmts = _split_statements(init_method.body)
                self._exec_stmts(stmts)
            except _ReturnSignal:
                pass
            finally:
                self._pop_scope()
                self._current_self = old_self
        return obj

    # ------------------------------------------------------------------
    # Turtle graphics
    # ------------------------------------------------------------------

    def _try_turtle(self, name: str, args: list) -> Any:
        t = self.turtle
        try:
            n = float(args[0]) if args else 0
        except (TypeError, ValueError):
            n = 0
        if name == "forward" or name == "fd":
            t.forward(n)
            return NIL
        if name == "backward" or name == "bd":
            t.forward(-n)
            return NIL
        if name == "right" or name == "rt":
            t.right(n)
            return NIL
        if name == "left" or name == "lt":
            t.left(n)
            return NIL
        if name == "penup" or name == "pu":
            t.penup()
            return NIL
        if name == "pendown" or name == "pd":
            t.pendown()
            return NIL
        if name == "home":
            t.home()
            return NIL
        if name == "clear_canvas":
            t.reset()
            return NIL
        if name == "setheading":
            t.setheading(n)
            return NIL
        if name == "setpos" or name == "goto":
            x = float(args[0]) if args else 0
            y = float(args[1]) if len(args) > 1 else 0
            t.goto(x, y)
            return NIL
        if name == "color":
            if len(args) >= 3:
                r, g, b = int(args[0]), int(args[1]), int(args[2])
                t.pencolor((r, g, b))
            elif args:
                t.pencolor(_ruby_to_s(args[0]))
            return NIL
        if name == "pensize" or name == "width":
            t.setpenwidth(int(n))
            return NIL
        return _UNSET

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _to_iterable(self, val: Any) -> list:
        if isinstance(val, list):
            return val
        if isinstance(val, RubyRange):
            return val.to_list()
        if isinstance(val, dict):
            return [[k, v] for k, v in val.items()]
        if isinstance(val, str):
            return list(val)
        return [val]

    def _eval_arg_list(self, args_str: str) -> list:
        if not args_str.strip():
            return []
        parts = _split_args(args_str)
        return [self._eval(p.strip()) for p in parts if p.strip()]


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------


def _is_truthy(val: Any) -> bool:
    if val is None or isinstance(val, _RubyNil):
        return False
    if isinstance(val, bool):
        return val
    return True


def _ruby_to_s(val: Any) -> str:
    if isinstance(val, _RubyNil):
        return ""
    if isinstance(val, bool):
        return "true" if val else "false"
    if isinstance(val, list):
        return "[" + ", ".join(_ruby_inspect(x) for x in val) + "]"
    if isinstance(val, dict):
        pairs = ", ".join(f"{_ruby_inspect(k)}: {_ruby_inspect(v)}" for k, v in val.items())
        return "{" + pairs + "}"
    if isinstance(val, RubySymbol):
        return val.name
    return str(val)


def _ruby_inspect(val: Any) -> str:
    if isinstance(val, str):
        return '"' + val.replace("\\", "\\\\").replace('"', '\\"') + '"'
    if isinstance(val, bool):
        return "true" if val else "false"
    if isinstance(val, _RubyNil):
        return "nil"
    if isinstance(val, list):
        return "[" + ", ".join(_ruby_inspect(x) for x in val) + "]"
    if isinstance(val, dict):
        pairs = ", ".join(f"{_ruby_inspect(k)}: {_ruby_inspect(v)}" for k, v in val.items())
        return "{" + pairs + "}"
    return str(val)


def _apply_op(lv: Any, op: str, rv: Any) -> Any:
    try:
        if op == "+":
            if isinstance(lv, str):
                return lv + _ruby_to_s(rv)
            if isinstance(lv, list):
                return lv + (rv if isinstance(rv, list) else [rv])
            return lv + rv
        if op == "-":
            return lv - rv
        if op == "*":
            if isinstance(lv, str) and isinstance(rv, int):
                return lv * rv
            if isinstance(lv, list) and isinstance(rv, int):
                return lv * rv
            return lv * rv
        if op == "/":
            if isinstance(lv, int) and isinstance(rv, int):
                return lv // rv
            return lv / rv
        if op == "%":
            return lv % rv
        if op == "**":
            return lv ** rv
        if op == "==":
            return lv == rv
        if op == "!=":
            return lv != rv
        if op == "<":
            return lv < rv
        if op == ">":
            return lv > rv
        if op == "<=":
            return lv <= rv
        if op == ">=":
            return lv >= rv
        if op == "<=>":
            if lv < rv:
                return -1
            if lv > rv:
                return 1
            return 0
        if op == "<<":
            if isinstance(lv, list):
                lv.append(rv)
                return lv
            if isinstance(lv, str):
                return lv + _ruby_to_s(rv)
            return lv << rv
        if op == ">>":
            return lv >> rv
        if op == "&":
            if isinstance(lv, list):
                return [x for x in lv if x in rv]
            return lv & rv
        if op == "|":
            if isinstance(lv, list):
                result = list(lv)
                for x in rv:
                    if x not in result:
                        result.append(x)
                return result
            return lv | rv
        if op == "^":
            return lv ^ rv
    except (TypeError, ZeroDivisionError) as e:
        if "division" in str(e).lower() or "zero" in str(e).lower():
            raise RubyError("ZeroDivisionError: divided by 0") from e
        raise RubyError(f"TypeError: {e}") from e
    return NIL


def _flatten(lst: list, depth: int) -> list:
    result = []
    for x in lst:
        if isinstance(x, list) and depth != 0:
            result.extend(_flatten(x, depth - 1))
        else:
            result.append(x)
    return result


def _sprintf(fmt: str, args: list) -> str:
    """Minimal sprintf-like formatter."""
    result = []
    arg_idx = 0
    i = 0
    while i < len(fmt):
        if fmt[i] == "%" and i + 1 < len(fmt):
            i += 1
            spec_start = i
            # parse flags, width, precision, type
            while i < len(fmt) and fmt[i] in "-+ 0#":
                i += 1
            while i < len(fmt) and fmt[i].isdigit():
                i += 1
            if i < len(fmt) and fmt[i] == ".":
                i += 1
                while i < len(fmt) and fmt[i].isdigit():
                    i += 1
            if i < len(fmt):
                type_char = fmt[i]
                spec = "%" + fmt[spec_start:i + 1]
                i += 1
                if type_char == "%":
                    result.append("%")
                elif arg_idx < len(args):
                    val = args[arg_idx]
                    arg_idx += 1
                    try:
                        if type_char in "di":
                            result.append(spec % int(val))
                        elif type_char in "fFeEgG":
                            result.append(spec % float(val))
                        elif type_char == "s":
                            result.append(spec % _ruby_to_s(val))
                        elif type_char in "xX":
                            result.append(spec % int(val))
                        elif type_char == "o":
                            result.append(spec % int(val))
                        elif type_char == "b":
                            result.append(bin(int(val))[2:])
                        else:
                            result.append(spec % val)
                    except (TypeError, ValueError):
                        result.append(str(val))
                continue
        else:
            result.append(fmt[i])
            i += 1
    return "".join(result)


# ---------------------------------------------------------------------------
# Parser helpers
# ---------------------------------------------------------------------------


def _balanced(s: str) -> bool:
    depth = 0
    in_str = False
    str_char = ""
    for ch in s:
        if in_str:
            if ch == str_char:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            str_char = ch
        elif ch in ("(", "[", "{"):
            depth += 1
        elif ch in (")", "]", "}"):
            depth -= 1
            if depth < 0:
                return False
    return depth == 0


def _has_unbalanced_parens(s: str) -> bool:
    return not _balanced(s)


def _find_dot(expr: str) -> int:
    """Find the first dot that is a method separator (not inside string/parens/brackets)."""
    depth = 0
    in_str = False
    str_char = ""
    for i, ch in enumerate(expr):
        if in_str:
            if ch == "\\" and i + 1 < len(expr):
                continue
            if ch == str_char:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            str_char = ch
        elif ch in ("(", "[", "{"):
            depth += 1
        elif ch in (")", "]", "}"):
            depth -= 1
        elif ch == "." and depth == 0:
            # Avoid matching .. and ... (range operators)
            if i + 1 < len(expr) and expr[i + 1] == ".":
                continue
            return i
    return -1


def _rfind_op(expr: str, op: str) -> int:
    """Find rightmost occurrence of operator not inside strings/parens."""
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
        elif ch in ("(", "[", "{"):
            depth += 1
        elif ch in (")", "]", "}"):
            depth -= 1
        elif depth == 0 and expr[i : i + len(op)] == op:
            # Avoid matching **= as *= etc.
            end = i + len(op)
            if op in ("+", "-") and end < len(expr) and expr[end] == "=":
                i += 1
                continue
            # Avoid matching -> as >
            if op == ">" and i > 0 and expr[i - 1] == "-":
                i += 1
                continue
            # Avoid matching a lone * when it is part of **
            if op == "*" and (
                (i > 0 and expr[i - 1] == "*") or (end < len(expr) and expr[end] == "*")
            ):
                i += 1
                continue
            # Avoid matching a lone < when it is part of << (append operator)
            if op == "<" and (
                (i > 0 and expr[i - 1] == "<") or (end < len(expr) and expr[end] == "<")
            ):
                i += 1
                continue
            # Avoid matching a lone > when it is part of >>
            if op == ">" and end < len(expr) and expr[end] == ">":
                i += 1
                continue
            last = i
        i += 1
    return last


def _find_modifier_keyword(expr: str) -> tuple[str, str, str] | None:
    """Scan *expr* left-to-right at bracket-depth 0 and return the last
    (lhs, keyword, rhs) triple where keyword is 'if' or 'unless'.
    Returns ``None`` when no modifier keyword is found."""
    depth = 0
    in_str = False
    str_char = ""
    result = None
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
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif depth == 0:
            for kw in (" if ", " unless "):
                if expr[i : i + len(kw)] == kw:
                    lhs = expr[:i].strip()
                    rhs = expr[i + len(kw) :].strip()
                    if lhs and rhs:
                        result = (lhs, kw.strip(), rhs)
                    break
        i += 1
    return result


def _strip_inline_comment(line: str) -> str:
    """Remove trailing # comment not inside a string."""
    in_str = False
    str_char = ""
    for i, ch in enumerate(line):
        if in_str:
            if ch == "\\" and i + 1 < len(line):
                continue
            if ch == str_char:
                in_str = False
        elif ch in ('"', "'"):
            in_str = True
            str_char = ch
        elif ch == "#":
            return line[:i].rstrip()
    return line


def _split_args(args_str: str) -> list[str]:
    """Split a comma-separated arg list, respecting nested brackets and strings."""
    parts = []
    depth = 0
    in_str = False
    str_char = ""
    current = []
    i = 0
    while i < len(args_str):
        ch = args_str[i]
        if in_str:
            current.append(ch)
            if ch == "\\" and i + 1 < len(args_str):
                i += 1
                current.append(args_str[i])
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


def _parse_params(params_str: str) -> tuple[list[str], dict[str, str]]:
    """Parse method parameter list. Returns (params, defaults)."""
    if not params_str.strip():
        return [], {}
    params = []
    defaults: dict[str, str] = {}
    for part in _split_args(params_str):
        part = part.strip()
        if "=" in part and not part.startswith("*"):
            name, default = part.split("=", 1)
            params.append(name.strip())
            defaults[name.strip()] = default.strip()
        else:
            params.append(part)
    return params, defaults


def _split_on_semicolons(line: str) -> list[str]:
    """Split a single line on ; separators outside strings/brackets."""
    parts = []
    depth = 0
    in_str = False
    str_char = ""
    current: list[str] = []
    i = 0
    while i < len(line):
        ch = line[i]
        if in_str:
            current.append(ch)
            if ch == "\\" and i + 1 < len(line):
                i += 1
                current.append(line[i])
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
            depth = max(0, depth - 1)
            current.append(ch)
        elif ch == ";" and depth == 0:
            part = "".join(current).strip()
            if part:
                parts.append(part)
            current = []
        else:
            current.append(ch)
        i += 1
    part = "".join(current).strip()
    if part:
        parts.append(part)
    return parts


def _split_statements(source: str) -> list[str]:  # noqa: C901
    """Split Ruby source into top-level statements (respecting block nesting)."""
    stmts: list[str] = []
    # Pre-expand ; separators into separate lines
    raw_lines = source.splitlines()
    lines: list[str] = []
    for raw_line in raw_lines:
        parts = _split_on_semicolons(raw_line)
        if len(parts) <= 1:
            lines.append(raw_line)
        else:
            indent = len(raw_line) - len(raw_line.lstrip())
            prefix = raw_line[:indent]
            for p in parts:
                lines.append(prefix + p)
    current: list[str] = []
    depth = 0
    paren_depth = 0  # tracks unclosed ( [ {
    i = 0

    while i < len(lines):
        line = lines[i]
        stripped = line.strip()
        stripped_nc = _strip_inline_comment(stripped)

        if not stripped or stripped.startswith("#"):
            if depth > 0 or paren_depth > 0:
                current.append(line)
            i += 1
            continue

        # Update paren/bracket/brace depth for this line
        # (count outside strings only)
        _in_s = False
        _sc = ""
        for ch in stripped_nc:
            if _in_s:
                if ch == _sc:
                    _in_s = False
            elif ch in ('"', "'"):
                _in_s = True
                _sc = ch
            elif ch in ("(", "[", "{"):
                paren_depth += 1
            elif ch in (")", "]", "}"):
                paren_depth = max(0, paren_depth - 1)

        # If inside an unfinished literal expression, keep accumulating
        if paren_depth > 0:
            current.append(line)
            i += 1
            continue

        # Count block depth changes
        opens = len(re.findall(
            r"\b(if|unless|while|until|for|begin|def|class|module|case)\b|"
            r"\bdo\b(?:\s*\|[^|]*\|)?\s*(?:#.*)?$",
            stripped_nc,
        ))
        closes = len(re.findall(r"\bend\b", stripped_nc))

        # Inline if/unless/while/until modifiers don't open a block
        if re.search(r"\b(if|unless|while|until)\b", stripped_nc):
            # Check if it is a standalone condition line (not inline modifier)
            if not re.match(r"^(if|unless|while|until)\b", stripped_nc):
                opens = max(0, opens - 1)

        if depth == 0 and opens == 0:
            # Simple statement
            current.append(line)
            stmts.append("\n".join(current))
            current = []
        else:
            current.append(line)
            depth += opens - closes
            if depth <= 0:
                depth = 0
                stmts.append("\n".join(current))
                current = []
            else:
                i += 1
                continue
        i += 1

    if current:
        stmts.append("\n".join(current))
    return [s.strip() for s in stmts if s.strip()]


def _count_block_openers(stripped: str, is_opener_line: bool = False) -> int:
    """Count block-opening keywords on a line, excluding modifier forms.

    Modifier ``if``/``unless``/``while``/``until`` (appearing mid-line after
    an expression) do NOT open a block and must not be counted.  We detect
    block-form by requiring these keywords to start the (stripped) line.
    ``do`` is always a block opener UNLESS it sits on the same line as a
    ``while``/``for``/``until`` that already opens the block.
    ``for``/``begin``/``def``/``class``/``module``/``case`` are always block
    openers wherever they appear, but are only meaningful at line-start in
    practice.
    """
    # Block form: these keywords only open a block at the start of the line
    line_starts_block = bool(re.match(
        r"^\s*(if|unless|while|until|for|begin|def|class|module|case)\b",
        stripped,
    ))
    # For opener lines the first keyword is already accounted for in the
    # caller's depth initialisation, so don't count it a second time via 'do'.
    is_loop_opener = bool(re.match(r"^\s*(while|until|for)\b", stripped))
    # 'do' counts as a block opener only when it is NOT the trailing 'do' of a
    # while/for/until line (which would cause double-counting).
    do_count = 0 if is_loop_opener else len(re.findall(r"\bdo\b", stripped))
    return (1 if line_starts_block else 0) + do_count


def _extract_body(full_stmt: str, opener: str, closer: str = "end") -> str:
    """Extract the body lines between opener and end."""
    lines = full_stmt.splitlines()
    # Skip first line (the opener line)
    body_lines = []
    depth = 0

    for i, line in enumerate(lines):
        stripped = _strip_inline_comment(line.strip())
        if i == 0:
            # First line is the opener (e.g. "def foo" / "class Foo" /
            # "while x > 0 do"). Depth starts at 0, meaning "we are now
            # inside the outermost block with no additional nesting".
            depth = 0
            continue

        closes = len(re.findall(r"\bend\b", stripped))
        opens = _count_block_openers(stripped)

        if stripped.startswith("else") or stripped.startswith("elsif") or \
           stripped.startswith("rescue") or stripped.startswith("ensure") or \
           stripped.startswith("when"):
            if depth == 0:
                body_lines.append(line)
                continue

        if closes > opens and depth == 0:
            # This is the closing "end"
            break

        depth += opens - closes
        body_lines.append(line)

    return "\n".join(body_lines)


def _split_if_branches(  # noqa: C901
    full_stmt: str,
) -> list[tuple[str, str, str]]:
    """Return list of (kind, condition, body) tuples for if/elsif/else/end."""
    lines = full_stmt.splitlines()
    branches: list[tuple[str, str, str]] = []
    current_kind = ""
    current_cond = ""
    current_body: list[str] = []
    depth = 0

    for i, line in enumerate(lines):
        stripped = _strip_inline_comment(line.strip())

        if i == 0:
            m = re.match(r"^(if|unless)\s+(.+)$", stripped)
            if m:
                current_kind = m.group(1)
                current_cond = m.group(2).rstrip(" then")
            continue

        closes = len(re.findall(r"\bend\b", stripped))
        opens = len(re.findall(
            r"\b(if|unless|while|until|for|begin|def|class|module|case)\b|"
            r"\bdo\b",
            stripped,
        ))

        if depth == 0 and re.match(r"^(elsif|else|end)\b", stripped):
            # Save current branch
            branches.append((current_kind, current_cond, "\n".join(current_body)))
            current_body = []
            if stripped == "end":
                break
            elif stripped.startswith("elsif"):
                m = re.match(r"^elsif\s+(.+)$", stripped)
                current_kind = "if"
                current_cond = m.group(1).rstrip(" then") if m else ""
            elif stripped.startswith("else"):
                current_kind = "else"
                current_cond = ""
        else:
            depth += opens - closes
            current_body.append(line)

    if current_body:
        branches.append((current_kind, current_cond, "\n".join(current_body)))

    return branches


def _split_begin_rescue(full_stmt: str) -> dict[str, str]:
    """Split begin...rescue...ensure...end into sections."""
    lines = full_stmt.splitlines()
    result: dict[str, str] = {"begin": "", "rescue": "", "ensure": ""}
    current_section = "begin"
    body_lines: list[str] = []
    rescue_var = None
    depth = 0

    for i, line in enumerate(lines):
        stripped = _strip_inline_comment(line.strip())
        if i == 0 and stripped == "begin":
            continue
        closes = len(re.findall(r"\bend\b", stripped))
        opens = len(re.findall(
            r"\b(if|unless|while|until|for|begin|def|class|module|case)\b|"
            r"\bdo\b",
            stripped,
        ))
        if depth == 0 and re.match(r"^(rescue|ensure|end)\b", stripped):
            result[current_section] = "\n".join(body_lines)
            body_lines = []
            if stripped.startswith("rescue"):
                current_section = "rescue"
                m = re.match(r"^rescue(?:\s+\w+)?\s*=>\s*(\w+)", stripped)
                if m:
                    rescue_var = m.group(1)
                    result["rescue_var"] = rescue_var
            elif stripped.startswith("ensure"):
                current_section = "ensure"
            elif stripped == "end":
                break
        else:
            depth += opens - closes
            body_lines.append(line)

    if body_lines:
        result[current_section] = "\n".join(body_lines)
    return result


def _extract_block(chain: str) -> "RubyProc | None":
    """Extract a trailing do...end or {...} block from a method call chain string."""
    # Inline block: method { |x| body }
    m = re.search(r"\{(\s*\|([^|]*)\|)?\s*(.*?)\s*\}$", chain, re.DOTALL)
    if m:
        params_str = m.group(2) or ""
        body = m.group(3) or ""
        params = [p.strip() for p in params_str.split(",") if p.strip()]
        return RubyProc(params, body, {})

    # Block: do |x| ... end
    m = re.search(r"\bdo\s*(?:\|([^|]*)\|)?\s*\n([\s\S]*?)\nend\s*$", chain)
    if m:
        params_str = m.group(1) or ""
        body = m.group(2) or ""
        params = [p.strip() for p in params_str.split(",") if p.strip()]
        return RubyProc(params, body, {})

    return None
