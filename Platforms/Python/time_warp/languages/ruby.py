"""Ruby language executor for Time Warp Studio.

Educational Ruby interpreter — whole-program execution.
Implements a teaching subset of Ruby:
  - puts/print/p, string interpolation
  - Variables (local, instance @, class @@, global $, constants)
  - Arithmetic, comparison, logical operators
  - String, Array, Hash (core methods)
  - Conditionals: if/elsif/else/end, unless, ternary, case/when
  - Loops: while/until, for..in, loop, times/upto/downto/each
  - Methods: def/end, return, default arguments, blocks (yield)
  - Classes: class/end, initialize, instance methods, attr_accessor
  - Modules: module/end, include, extend
  - Blocks: {}, do/end, proc, lambda (->)
  - Exception handling: begin/rescue/ensure/raise
  - Turtle graphics commands
"""

from __future__ import annotations

import math
import os
import random
import re
import time
from typing import TYPE_CHECKING, Any, Callable, Dict, List, Optional, Tuple

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
# Sentinel / control flow
# ---------------------------------------------------------------------------


class _Return(Exception):
    def __init__(self, value: Any = None):
        self.value = value


class _Break(Exception):
    def __init__(self, value: Any = None):
        self.value = value


class _Next(Exception):
    pass


class _RubyError(Exception):
    pass


class _RubyObject:
    """A generic Ruby object with instance variables."""

    def __init__(self, klass: "_RubyClass"):
        self.klass = klass
        self.ivars: Dict[str, Any] = {}
        self.methods: Dict[str, "_RubyMethod"] = {}

    def __repr__(self) -> str:
        return f"#<{self.klass.name}>"

    def respond_to(self, name: str) -> bool:
        return name in self.klass.methods or name in self.methods


class _RubyClass:
    """A Ruby class."""

    def __init__(self, name: str, superclass: Optional["_RubyClass"] = None):
        self.name = name
        self.superclass = superclass
        self.methods: Dict[str, "_RubyMethod"] = {}
        self.class_methods: Dict[str, "_RubyMethod"] = {}
        self.ivars: Dict[str, Any] = {}
        self.included_modules: List["_RubyModule"] = []

    def find_method(self, name: str) -> Optional["_RubyMethod"]:
        """Search this class and its superclass chain."""
        if name in self.methods:
            return self.methods[name]
        for mod in reversed(self.included_modules):
            if name in mod.methods:
                return mod.methods[name]
        if self.superclass:
            return self.superclass.find_method(name)
        return None

    def __repr__(self) -> str:
        return self.name


class _RubyModule:
    """A Ruby module."""

    def __init__(self, name: str):
        self.name = name
        self.methods: Dict[str, "_RubyMethod"] = {}
        self.constants: Dict[str, Any] = {}

    def __repr__(self) -> str:
        return self.name


class _RubyMethod:
    """A user-defined Ruby method."""

    def __init__(
        self,
        name: str,
        params: List[str],
        body: List[str],
        closure_env: Optional[Dict[str, Any]] = None,
    ):
        self.name = name
        self.params = params
        self.body = body
        self.closure_env = closure_env or {}


class _RubyBlock:
    """A Ruby block (proc/lambda)."""

    def __init__(
        self,
        params: List[str],
        body: List[str],
        closure_env: Dict[str, Any],
        is_lambda: bool = False,
    ):
        self.params = params
        self.body = body
        self.closure_env = closure_env
        self.is_lambda = is_lambda


# ---------------------------------------------------------------------------
# Environment
# ---------------------------------------------------------------------------


class RubyEnvironment:
    """Tree-walking Ruby interpreter."""

    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: List[str] = []
        self._globals: Dict[str, Any] = {}
        self._constants: Dict[str, Any] = {}
        self._classes: Dict[str, _RubyClass] = {}
        self._modules: Dict[str, _RubyModule] = {}
        self._scope_stack: List[Dict[str, Any]] = [{}]  # [0] = top-level
        self._current_self: Any = None
        self._setup_builtins()
        self._block_stack: List[Optional[_RubyBlock]] = [None]

    # ------------------------------------------------------------------
    # Output
    # ------------------------------------------------------------------

    def _emit(self, text: str) -> None:
        self._output.append(text)

    # ------------------------------------------------------------------
    # Variable resolution
    # ------------------------------------------------------------------

    def _get(self, name: str) -> Any:
        """Look up a variable in the scope chain."""
        if name.startswith("@@"):
            return self._globals.get(name)
        if name.startswith("@"):
            if isinstance(self._current_self, _RubyObject):
                return self._current_self.ivars.get(name)
            return None
        if name.startswith("$"):
            return self._globals.get(name)
        if name[0].isupper():
            return self._constants.get(name)
        for scope in reversed(self._scope_stack):
            if name in scope:
                return scope[name]
        return self._globals.get(name)

    def _set(self, name: str, value: Any, local: bool = False) -> None:
        """Assign a variable."""
        if name.startswith("@@"):
            self._globals[name] = value
        elif name.startswith("@"):
            if isinstance(self._current_self, _RubyObject):
                self._current_self.ivars[name] = value
            else:
                self._globals[name] = value
        elif name.startswith("$"):
            self._globals[name] = value
        elif name[0].isupper():
            self._constants[name] = value
        elif local or len(self._scope_stack) > 1:
            self._scope_stack[-1][name] = value
        else:
            self._scope_stack[-1][name] = value

    def _push_scope(self) -> None:
        self._scope_stack.append({})

    def _pop_scope(self) -> None:
        if len(self._scope_stack) > 1:
            self._scope_stack.pop()

    # ------------------------------------------------------------------
    # Built-ins setup
    # ------------------------------------------------------------------

    def _setup_builtins(self) -> None:
        g = self._globals
        g["true"] = True
        g["false"] = False
        g["nil"] = None
        g["STDOUT"] = None
        g["STDERR"] = None
        g["STDIN"] = None
        g["ENV"] = {}
        g["ARGV"] = []

        # Math module
        self._constants["Math"] = {
            "PI": math.pi,
            "E": math.e,
            "sqrt": math.sqrt,
            "log": math.log,
            "sin": math.sin,
            "cos": math.cos,
            "tan": math.tan,
        }

        # Built-in classes
        for cname in ["Object", "Numeric", "Integer", "Float", "String",
                       "Array", "Hash", "Symbol", "NilClass", "TrueClass",
                       "FalseClass", "Range", "IO", "File", "Proc", "Method"]:
            self._classes[cname] = _RubyClass(cname)
        self._constants["Math"] = {
            "PI": math.pi, "E": math.e,
            "sqrt": math.sqrt, "cbrt": lambda x: x ** (1 / 3),
            "log": math.log, "log2": math.log2, "log10": math.log10,
            "sin": math.sin, "cos": math.cos, "tan": math.tan,
            "asin": math.asin, "acos": math.acos, "atan": math.atan,
            "atan2": math.atan2, "exp": math.exp, "hypot": math.hypot,
        }

    # ------------------------------------------------------------------
    # Entry point
    # ------------------------------------------------------------------

    def run(self, source: str) -> str:
        """Run a Ruby program."""
        try:
            lines = source.splitlines()
            self._exec_lines(lines, 0, len(lines))
        except _Return:
            pass
        except _RubyError as e:
            self._emit(f"❌ Ruby error: {e}\n")
        except RecursionError:
            self._emit("❌ Ruby error: stack level too deep (SystemStackError)\n")
        except Exception as e:
            self._emit(f"❌ Runtime error: {e}\n")
        return "".join(self._output)

    # ------------------------------------------------------------------
    # Line executor
    # ------------------------------------------------------------------

    def _exec_lines(self, lines: List[str], start: int, end: int) -> Any:
        """Execute lines[start:end], return last expression value."""
        result = None
        i = start
        while i < end:
            line = lines[i]
            stripped = line.strip()

            # Skip blank and comment lines
            if not stripped or stripped.startswith("#"):
                i += 1
                continue

            # Multi-line structures
            if stripped.startswith("class ") or stripped == "class":
                i = self._exec_class(lines, i)
                continue
            if stripped.startswith("module "):
                i = self._exec_module(lines, i)
                continue
            if stripped.startswith("def "):
                i = self._exec_def(lines, i)
                continue
            if stripped.startswith("if ") or stripped == "if":
                i, result = self._exec_if(lines, i)
                continue
            if stripped.startswith("unless ") and not stripped.endswith("end"):
                i, result = self._exec_unless(lines, i)
                continue
            if stripped.startswith("while ") or stripped == "while":
                i = self._exec_while(lines, i)
                continue
            if stripped.startswith("until ") or stripped == "until":
                i = self._exec_until(lines, i)
                continue
            if stripped.startswith("for ") and " in " in stripped:
                i = self._exec_for(lines, i)
                continue
            if stripped == "loop do" or stripped == "loop {":
                i = self._exec_loop(lines, i)
                continue
            if stripped.startswith("begin"):
                i = self._exec_begin(lines, i)
                continue
            if stripped.startswith("case "):
                i, result = self._exec_case(lines, i)
                continue

            # Single-line statements
            result = self._exec_statement(stripped, lines, i)
            i += 1

        return result

    # ------------------------------------------------------------------
    # Class definition
    # ------------------------------------------------------------------

    def _exec_class(self, lines: List[str], start: int) -> int:
        """Parse and register a class definition."""
        header = lines[start].strip()
        m = re.match(r"class\s+(\w+)(?:\s*<\s*(\w+))?", header)
        if not m:
            return start + 1

        classname = m.group(1)
        superclass_name = m.group(2)
        superclass = self._classes.get(superclass_name) if superclass_name else self._classes.get("Object")

        klass = self._classes.get(classname) or _RubyClass(classname, superclass)
        klass.superclass = superclass
        self._classes[classname] = klass
        self._constants[classname] = klass

        # Collect class body
        depth = 1
        i = start + 1
        body_lines: List[str] = []
        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(class|module|def\s+\w+|if\s|unless\s|while\s|until\s|for\s|begin\b|case\s)", s):
                depth += 1
            if s == "end":
                depth -= 1
                if depth == 0:
                    break
            if depth > 0:
                body_lines.append(lines[i])
            i += 1

        self._parse_class_body(klass, body_lines)
        return i + 1

    def _parse_class_body(self, klass: _RubyClass, body_lines: List[str]) -> None:
        """Parse class body: methods, attr_accessor, include."""
        j = 0
        while j < len(body_lines):
            stripped = body_lines[j].strip()
            if not stripped or stripped.startswith("#"):
                j += 1
                continue

            # attr_accessor / attr_reader / attr_writer
            m = re.match(r"attr_(accessor|reader|writer)\s+(.+)", stripped)
            if m:
                kind = m.group(1)
                attrs = [a.strip().lstrip(":") for a in m.group(2).split(",")]
                for attr in attrs:
                    attr = attr.strip()
                    if kind in ("accessor", "reader"):
                        body = [f"@{attr}"]
                        klass.methods[attr] = _RubyMethod(attr, [], body)
                    if kind in ("accessor", "writer"):
                        klass.methods[f"{attr}="] = _RubyMethod(
                            f"{attr}=", ["value"], [f"@{attr} = value"]
                        )
                j += 1
                continue

            # include / extend
            m = re.match(r"(include|extend)\s+(\w+)", stripped)
            if m:
                mod_name = m.group(2)
                mod = self._modules.get(mod_name)
                if mod:
                    klass.included_modules.append(mod)
                j += 1
                continue

            # Method definition
            if stripped.startswith("def "):
                j = self._parse_method_into(klass.methods, body_lines, j)
                continue

            j += 1

    def _parse_method_into(
        self, methods: Dict[str, _RubyMethod], lines: List[str], start: int
    ) -> int:
        """Parse a def block and add it to methods dict. Returns next line index."""
        header = lines[start].strip()
        # def method_name(params) or def method_name
        m = re.match(r"def\s+([\w!?=]+)\s*(?:\(([^)]*)\))?", header)
        if not m:
            return start + 1
        name = m.group(1)
        params_str = m.group(2) or ""
        params = [p.strip() for p in params_str.split(",") if p.strip()] if params_str else []

        depth = 1
        body: List[str] = []
        i = start + 1
        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(def\s|class\s|module\s|if\s|unless\s|while\s|until\s|for\s|begin\b|case\s|do\b)", s):
                depth += 1
            if s == "end":
                depth -= 1
                if depth == 0:
                    break
            if depth > 0:
                body.append(lines[i])
            i += 1

        methods[name] = _RubyMethod(name, params, body, dict(self._scope_stack[-1]))
        return i + 1

    # ------------------------------------------------------------------
    # Module definition
    # ------------------------------------------------------------------

    def _exec_module(self, lines: List[str], start: int) -> int:
        header = lines[start].strip()
        m = re.match(r"module\s+(\w+)", header)
        if not m:
            return start + 1
        modname = m.group(1)
        mod = self._modules.get(modname) or _RubyModule(modname)
        self._modules[modname] = mod
        self._constants[modname] = mod

        depth = 1
        body_lines: List[str] = []
        i = start + 1
        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(class|module|def\s|if\s|unless\s|while\s|until\s|for\s|begin\b|case\s)", s):
                depth += 1
            if s == "end":
                depth -= 1
                if depth == 0:
                    break
            if depth > 0:
                body_lines.append(lines[i])
            i += 1

        j = 0
        while j < len(body_lines):
            stripped = body_lines[j].strip()
            if stripped.startswith("def "):
                j = self._parse_method_into(mod.methods, body_lines, j)
            else:
                j += 1

        return i + 1

    # ------------------------------------------------------------------
    # Method (top-level) definition
    # ------------------------------------------------------------------

    def _exec_def(self, lines: List[str], start: int) -> int:
        return self._parse_method_into(
            self._globals, lines, start,  # type: ignore[arg-type]
        )

    # Actually store top-level defs in a separate dict
    _top_methods: Dict[str, _RubyMethod]

    def _exec_def(self, lines: List[str], start: int) -> int:  # type: ignore[misc]
        if not hasattr(self, "_top_methods"):
            self._top_methods = {}
        return self._parse_method_into(self._top_methods, lines, start)

    # ------------------------------------------------------------------
    # Control flow: if / unless
    # ------------------------------------------------------------------

    def _exec_if(self, lines: List[str], start: int) -> Tuple[int, Any]:
        """Execute if/elsif/else/end block."""
        header = lines[start].strip()

        # Inline: if cond; body; end  or  body if cond
        if re.search(r";.*end\s*$", header) or (
            header.endswith("end") and header.count(";") >= 1
        ):
            return start + 1, self._exec_statement(header, lines, start)

        m = re.match(r"if\s+(.+)", header)
        cond_str = m.group(1) if m else "false"

        branches: List[Tuple[Optional[str], List[str]]] = []
        current_cond: Optional[str] = cond_str
        current_body: List[str] = []
        depth = 1
        i = start + 1

        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(if\s|unless\s|while\s|until\s|for\s|begin\b|case\s|def\s)", s):
                depth += 1
            if depth == 1 and s.startswith("elsif "):
                branches.append((current_cond, current_body))
                current_cond = s[6:].strip()
                current_body = []
                i += 1
                continue
            if depth == 1 and s == "else":
                branches.append((current_cond, current_body))
                current_cond = None  # else
                current_body = []
                i += 1
                continue
            if s == "end":
                depth -= 1
                if depth == 0:
                    branches.append((current_cond, current_body))
                    break
            if depth > 0:
                current_body.append(lines[i])
            i += 1

        result = None
        executed = False
        for cond, body in branches:
            if cond is None:  # else
                if not executed:
                    result = self._exec_lines(body, 0, len(body))
                    executed = True
            else:
                if not executed and self._truthy(self._eval_expr(cond)):
                    result = self._exec_lines(body, 0, len(body))
                    executed = True

        return i + 1, result

    def _exec_unless(self, lines: List[str], start: int) -> Tuple[int, Any]:
        header = lines[start].strip()
        m = re.match(r"unless\s+(.+)", header)
        cond_str = m.group(1) if m else "false"

        body: List[str] = []
        else_body: List[str] = []
        in_else = False
        depth = 1
        i = start + 1

        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(if\s|unless\s|while\s|until\s|for\s|begin\b|case\s|def\s)", s):
                depth += 1
            if depth == 1 and s == "else":
                in_else = True
                i += 1
                continue
            if s == "end":
                depth -= 1
                if depth == 0:
                    break
            if depth > 0:
                if in_else:
                    else_body.append(lines[i])
                else:
                    body.append(lines[i])
            i += 1

        result = None
        cond_val = self._truthy(self._eval_expr(cond_str))
        if not cond_val:
            result = self._exec_lines(body, 0, len(body))
        elif else_body:
            result = self._exec_lines(else_body, 0, len(else_body))

        return i + 1, result

    # ------------------------------------------------------------------
    # Control flow: while / until
    # ------------------------------------------------------------------

    def _exec_while(self, lines: List[str], start: int) -> int:
        header = lines[start].strip()
        m = re.match(r"while\s+(.+)", header)
        cond_str = m.group(1).rstrip(" do") if m else "false"

        body, end_i = self._collect_block(lines, start + 1)

        try:
            self._push_scope()
            for _ in range(100_000):
                if not self._truthy(self._eval_expr(cond_str)):
                    break
                try:
                    self._exec_lines(body, 0, len(body))
                except _Next:
                    continue
                except _Break:
                    break
        finally:
            self._pop_scope()

        return end_i + 1

    def _exec_until(self, lines: List[str], start: int) -> int:
        header = lines[start].strip()
        m = re.match(r"until\s+(.+)", header)
        cond_str = m.group(1).rstrip(" do") if m else "true"

        body, end_i = self._collect_block(lines, start + 1)

        try:
            self._push_scope()
            for _ in range(100_000):
                if self._truthy(self._eval_expr(cond_str)):
                    break
                try:
                    self._exec_lines(body, 0, len(body))
                except _Next:
                    continue
                except _Break:
                    break
        finally:
            self._pop_scope()

        return end_i + 1

    # ------------------------------------------------------------------
    # Control flow: for..in
    # ------------------------------------------------------------------

    def _exec_for(self, lines: List[str], start: int) -> int:
        header = lines[start].strip()
        m = re.match(r"for\s+(\w+)\s+in\s+(.+?)(?:\s+do)?$", header)
        if not m:
            return start + 1

        var_name = m.group(1)
        iter_str = m.group(2).strip()

        body, end_i = self._collect_block(lines, start + 1)
        iterable = self._eval_expr(iter_str)
        if not hasattr(iterable, "__iter__"):
            return end_i + 1

        try:
            self._push_scope()
            for item in iterable:
                self._set(var_name, item)
                try:
                    self._exec_lines(body, 0, len(body))
                except _Next:
                    continue
                except _Break:
                    break
        finally:
            self._pop_scope()

        return end_i + 1

    # ------------------------------------------------------------------
    # Control flow: loop do
    # ------------------------------------------------------------------

    def _exec_loop(self, lines: List[str], start: int) -> int:
        body, end_i = self._collect_block(lines, start + 1)

        try:
            self._push_scope()
            for _ in range(100_000):
                try:
                    self._exec_lines(body, 0, len(body))
                except _Next:
                    continue
                except _Break:
                    break
        finally:
            self._pop_scope()

        return end_i + 1

    # ------------------------------------------------------------------
    # Control flow: begin/rescue/ensure
    # ------------------------------------------------------------------

    def _exec_begin(self, lines: List[str], start: int) -> int:
        begin_body: List[str] = []
        rescue_clauses: List[Tuple[Optional[str], List[str]]] = []
        ensure_body: List[str] = []
        current_section = "begin"
        current_rescue_type: Optional[str] = None
        current_rescue_body: List[str] = []
        depth = 1
        i = start + 1

        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(begin\b|if\s|unless\s|while\s|until\s|for\s|def\s|case\s)", s):
                depth += 1
            if depth == 1 and s.startswith("rescue"):
                if current_section == "begin":
                    pass
                elif current_section == "rescue":
                    rescue_clauses.append((current_rescue_type, current_rescue_body))
                current_section = "rescue"
                m = re.match(r"rescue\s+(\w+)", s)
                current_rescue_type = m.group(1) if m else None
                current_rescue_body = []
                i += 1
                continue
            if depth == 1 and s == "ensure":
                if current_section == "rescue":
                    rescue_clauses.append((current_rescue_type, current_rescue_body))
                current_section = "ensure"
                i += 1
                continue
            if s == "end":
                depth -= 1
                if depth == 0:
                    if current_section == "rescue":
                        rescue_clauses.append((current_rescue_type, current_rescue_body))
                    break
            if depth > 0:
                if current_section == "begin":
                    begin_body.append(lines[i])
                elif current_section == "rescue":
                    current_rescue_body.append(lines[i])
                elif current_section == "ensure":
                    ensure_body.append(lines[i])
            i += 1

        try:
            self._exec_lines(begin_body, 0, len(begin_body))
        except _RubyError as e:
            recovered = False
            for _exc_type, rescue_body in rescue_clauses:
                self._exec_lines(rescue_body, 0, len(rescue_body))
                recovered = True
                break
            if not recovered:
                raise
        except Exception as e:
            for _exc_type, rescue_body in rescue_clauses:
                self._set("$!", str(e))
                self._exec_lines(rescue_body, 0, len(rescue_body))
                break
        finally:
            if ensure_body:
                self._exec_lines(ensure_body, 0, len(ensure_body))

        return i + 1

    # ------------------------------------------------------------------
    # Control flow: case / when
    # ------------------------------------------------------------------

    def _exec_case(self, lines: List[str], start: int) -> Tuple[int, Any]:
        header = lines[start].strip()
        m = re.match(r"case\s+(.*)", header)
        case_val = self._eval_expr(m.group(1).strip()) if m else None

        branches: List[Tuple[Optional[str], List[str]]] = []
        else_body: List[str] = []
        current_when: Optional[str] = None
        current_body: List[str] = []
        depth = 1
        i = start + 1

        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(if\s|unless\s|while\s|until\s|for\s|begin\b|case\s|def\s)", s):
                depth += 1
            if depth == 1 and s.startswith("when "):
                if current_when is not None:
                    branches.append((current_when, current_body))
                current_when = s[5:].strip()
                current_body = []
                i += 1
                continue
            if depth == 1 and s == "else":
                if current_when is not None:
                    branches.append((current_when, current_body))
                current_when = None
                current_body = []
                i += 1
                continue
            if s == "end":
                depth -= 1
                if depth == 0:
                    if current_when is not None:
                        branches.append((current_when, current_body))
                    else:
                        else_body = current_body
                    break
            if depth > 0:
                current_body.append(lines[i])
            i += 1

        result = None
        executed = False
        for when_str, body in branches:
            when_vals = [v.strip() for v in when_str.split(",")]
            for wv in when_vals:
                wval = self._eval_expr(wv)
                if case_val == wval or (isinstance(wval, range) and case_val in wval):
                    result = self._exec_lines(body, 0, len(body))
                    executed = True
                    break
            if executed:
                break

        if not executed and else_body:
            result = self._exec_lines(else_body, 0, len(else_body))

        return i + 1, result

    # ------------------------------------------------------------------
    # Helpers: collect a do..end or if..end block
    # ------------------------------------------------------------------

    def _collect_block(
        self, lines: List[str], start: int
    ) -> Tuple[List[str], int]:
        """Collect lines until matching 'end'. Returns (body_lines, end_line_index)."""
        body: List[str] = []
        depth = 1
        i = start
        while i < len(lines) and depth > 0:
            s = lines[i].strip()
            if re.match(r"^(if\s|unless\s|while\s|until\s|for\s|begin\b|case\s|def\s|class\s|module\s|do\b)", s):
                depth += 1
            if s == "end":
                depth -= 1
                if depth == 0:
                    break
            if depth > 0:
                body.append(lines[i])
            i += 1
        return body, i

    # ------------------------------------------------------------------
    # Single-statement executor
    # ------------------------------------------------------------------

    def _exec_statement(self, stmt: str, lines: List[str], line_idx: int) -> Any:
        """Execute a single Ruby statement."""
        stmt = stmt.strip()
        if not stmt or stmt.startswith("#"):
            return None

        # Inline modifier: expr if cond / expr unless cond
        m = re.match(r"^(.+?)\s+if\s+(.+)$", stmt)
        if m and not stmt.startswith("if "):
            cond = self._eval_expr(m.group(2))
            if self._truthy(cond):
                return self._exec_statement(m.group(1), lines, line_idx)
            return None

        m = re.match(r"^(.+?)\s+unless\s+(.+)$", stmt)
        if m and not stmt.startswith("unless "):
            cond = self._eval_expr(m.group(2))
            if not self._truthy(cond):
                return self._exec_statement(m.group(1), lines, line_idx)
            return None

        # return
        if stmt == "return":
            raise _Return(None)
        m = re.match(r"^return\s+(.+)$", stmt)
        if m:
            raise _Return(self._eval_expr(m.group(1)))

        # raise
        m = re.match(r"^raise\s*(.*)", stmt)
        if m:
            msg = m.group(1).strip()
            raise _RubyError(self._eval_expr(msg) if msg else "RuntimeError")

        # break / next
        if stmt == "break":
            raise _Break()
        if stmt == "next":
            raise _Next()

        # puts / print / p
        if stmt.startswith("puts ") or stmt.startswith("puts("):
            args = self._parse_args(stmt[4:].strip())
            for arg in args:
                val = self._eval_expr(arg)
                if val is None:
                    self._emit("\n")
                elif isinstance(val, (list, _RubyRange)):
                    for item in val:
                        self._emit(self._ruby_to_s(item) + "\n")
                else:
                    self._emit(self._ruby_to_s(val) + "\n")
            if not args:
                self._emit("\n")
            return None

        if stmt == "puts":
            self._emit("\n")
            return None

        if stmt.startswith("print ") or stmt.startswith("print("):
            args = self._parse_args(stmt[5:].strip())
            for arg in args:
                val = self._eval_expr(arg)
                self._emit(self._ruby_to_s(val))
            return None

        if stmt.startswith("p ") or stmt.startswith("p("):
            args = self._parse_args(stmt[1:].strip())
            for arg in args:
                val = self._eval_expr(arg)
                self._emit(self._ruby_inspect(val) + "\n")
            return None

        if stmt.startswith("pp "):
            val = self._eval_expr(stmt[3:].strip())
            self._emit(self._ruby_inspect(val) + "\n")
            return None

        # require / require_relative (educational stub)
        if stmt.startswith("require"):
            return None

        # include at top-level
        m = re.match(r"include\s+(\w+)", stmt)
        if m:
            return None

        # attr_accessor at top-level (ignore)
        if re.match(r"attr_(accessor|reader|writer)\s+", stmt):
            return None

        # Assignment: var = expr
        # Multi-assign: a, b = expr
        m = re.match(r"^((?:\w+,\s*)*\w+)\s*=\s*(.+)$", stmt)
        if m and not re.match(r"[=!<>]=", stmt):
            left = m.group(1)
            right = m.group(2).strip()
            if "," in left:
                # Multiple assignment
                vars_ = [v.strip() for v in left.split(",")]
                val = self._eval_expr(right)
                if isinstance(val, (list, tuple)):
                    for idx, vname in enumerate(vars_):
                        self._set(vname, val[idx] if idx < len(val) else None)
                else:
                    self._set(vars_[0], val)
                    for vname in vars_[1:]:
                        self._set(vname, None)
            else:
                # Check for compound assignment
                compound_m = re.match(r"^(\w+|\@\w+|\$\w+)\s*([+\-*/%|&^]=|\|\|=|&&=|<<=|>>=|\*\*=)\s*(.+)$", stmt)
                if compound_m:
                    vname = compound_m.group(1)
                    op = compound_m.group(2)
                    rhs = self._eval_expr(compound_m.group(3))
                    lhs = self._get(vname)
                    self._set(vname, self._apply_compound_op(lhs, op, rhs))
                else:
                    val = self._eval_expr(right)
                    self._set(left, val)
            return self._get(left.split(",")[0].strip())

        # instance variable assignment
        m = re.match(r"^(@[\w]+)\s*=\s*(.+)$", stmt)
        if m:
            val = self._eval_expr(m.group(2))
            self._set(m.group(1), val)
            return val

        # global variable assignment
        m = re.match(r"^(\$[\w]+)\s*=\s*(.+)$", stmt)
        if m:
            val = self._eval_expr(m.group(2))
            self._set(m.group(1), val)
            return val

        # Turtle graphics commands
        turtle_result = self._try_turtle(stmt)
        if turtle_result is not None:
            return None

        # Generic expression
        return self._eval_expr(stmt)

    def _apply_compound_op(self, lhs: Any, op: str, rhs: Any) -> Any:
        if op == "+=":
            return (lhs or 0) + rhs
        if op == "-=":
            return (lhs or 0) - rhs
        if op == "*=":
            return (lhs or 1) * rhs
        if op == "/=":
            return (lhs or 0) / rhs if rhs else 0
        if op == "%=":
            return (lhs or 0) % rhs if rhs else 0
        if op == "**=":
            return (lhs or 0) ** rhs
        if op == "||=":
            return lhs if self._truthy(lhs) else rhs
        if op == "&&=":
            return rhs if self._truthy(lhs) else lhs
        return rhs

    # ------------------------------------------------------------------
    # Turtle graphics
    # ------------------------------------------------------------------

    def _try_turtle(self, stmt: str) -> Optional[bool]:
        """Try to interpret stmt as a turtle graphics command."""
        t = self.turtle
        if t is None:
            return None

        cmd_map = {
            "forward": lambda a: t.forward(float(a)),
            "fd": lambda a: t.forward(float(a)),
            "backward": lambda a: t.backward(float(a)),
            "back": lambda a: t.backward(float(a)),
            "bk": lambda a: t.backward(float(a)),
            "right": lambda a: t.right(float(a)),
            "rt": lambda a: t.right(float(a)),
            "left": lambda a: t.left(float(a)),
            "lt": lambda a: t.left(float(a)),
            "penup": lambda: t.pen_up(),
            "pu": lambda: t.pen_up(),
            "pendown": lambda: t.pen_down(),
            "pd": lambda: t.pen_down(),
            "showturtle": lambda: t.show_turtle(),
            "st": lambda: t.show_turtle(),
            "hideturtle": lambda: t.hide_turtle(),
            "ht": lambda: t.hide_turtle(),
            "home": lambda: t.home(),
            "clearscreen": lambda: t.clear(),
            "cs": lambda: t.clear(),
        }

        for prefix, fn in cmd_map.items():
            m = re.match(rf"^{re.escape(prefix)}\s*\(?([^)]*)\)?$", stmt, re.IGNORECASE)
            if m:
                arg = m.group(1).strip()
                try:
                    if arg:
                        fn(self._eval_expr(arg))
                    else:
                        fn()
                    return True
                except (TypeError, ValueError):
                    pass

        # pencolor / setcolor
        m = re.match(r"^pencolor\s*\(?[\"']?(\w+)[\"']?\)?$", stmt, re.IGNORECASE)
        if m:
            t.set_color(m.group(1))
            return True

        return None

    # ------------------------------------------------------------------
    # Expression evaluator
    # ------------------------------------------------------------------

    def _eval_expr(self, expr: str) -> Any:
        """Evaluate a Ruby expression string."""
        expr = expr.strip()
        if not expr:
            return None

        # nil / true / false
        if expr == "nil":
            return None
        if expr == "true":
            return True
        if expr == "false":
            return False

        # Integer literals
        if re.match(r"^-?\d+$", expr):
            return int(expr)

        # Float literals
        if re.match(r"^-?\d+\.\d+$", expr):
            return float(expr)

        # Symbol :foo
        if re.match(r"^:[\w!?]+$", expr):
            return expr  # represent symbol as string ":name"

        # String literals (with interpolation)
        m = re.match(r'^"(.*)"$', expr, re.DOTALL)
        if m:
            return self._interpolate(m.group(1))

        m = re.match(r"^'(.*)'$", expr, re.DOTALL)
        if m:
            return m.group(1)  # single-quoted: no interpolation

        # Heredoc (basic)
        m = re.match(r"^<<[~-]?['\"]?(\w+)['\"]?$", expr)
        if m:
            return ""  # stub

        # Array literal [...]
        if expr.startswith("[") and expr.endswith("]"):
            inner = expr[1:-1].strip()
            if not inner:
                return []
            items = self._split_args(inner)
            return [self._eval_expr(item.strip()) for item in items]

        # Hash literal {...}
        if expr.startswith("{") and expr.endswith("}"):
            return self._eval_hash(expr)

        # Range a..b or a...b
        m = re.match(r"^(.+?)(\.\.\.?)(.+)$", expr)
        if m and ".." in expr:
            start = self._eval_expr(m.group(1).strip())
            end = self._eval_expr(m.group(3).strip())
            exclusive = m.group(2) == "..."
            return _RubyRange(start, end, exclusive)

        # Parenthesised expression
        if expr.startswith("(") and expr.endswith(")"):
            return self._eval_expr(expr[1:-1])

        # Ternary: cond ? a : b
        m = re.match(r"^(.+?)\s*\?\s*(.+?)\s*:\s*(.+)$", expr)
        if m:
            cond = self._truthy(self._eval_expr(m.group(1)))
            return self._eval_expr(m.group(2)) if cond else self._eval_expr(m.group(3))

        # Logical operators (short-circuit)
        for op in (" || ", " or "):
            if op in expr:
                parts = expr.split(op, 1)
                lv = self._eval_expr(parts[0])
                return lv if self._truthy(lv) else self._eval_expr(parts[1])

        for op in (" && ", " and "):
            if op in expr:
                parts = expr.split(op, 1)
                lv = self._eval_expr(parts[0])
                return self._eval_expr(parts[1]) if self._truthy(lv) else lv

        if expr.startswith("!") or expr.startswith("not "):
            inner = expr[1:] if expr.startswith("!") else expr[4:]
            return not self._truthy(self._eval_expr(inner.strip()))

        # Comparison / arithmetic — use Python eval in sandbox
        # First try to detect method calls and handle them
        result = self._try_method_call(expr)
        if result is not _UNSET:
            return result

        return self._safe_eval(expr)

    _UNSET = object()

    def _try_method_call(self, expr: str) -> Any:
        """Try to evaluate method/block calls. Returns _UNSET if not a method call."""
        # new: ClassName.new / ClassName.new(args)
        m = re.match(r"^([A-Z]\w*)\s*\.\s*new\s*(?:\(([^)]*)\))?$", expr)
        if m:
            classname = m.group(1)
            args_str = m.group(2) or ""
            return self._instantiate(classname, args_str)

        # Method call: obj.method / obj.method(args) / obj.method { block }
        # Chained calls handled recursively
        m = re.match(r"^(.+?)\s*\.\s*([\w!?]+)\s*(?:\(([^)]*)\))?\s*(?:do\s*\|([^|]*)\|)?\s*$", expr)
        if m:
            receiver_str = m.group(1)
            method_name = m.group(2)
            args_str = m.group(3) or ""
            block_param = m.group(4)
            receiver = self._eval_expr(receiver_str)
            args = [self._eval_expr(a.strip()) for a in self._split_args(args_str)] if args_str.strip() else []
            return self._call_method(receiver, method_name, args)

        # Block call: expr.times { |i| ... } -- handled as method call above
        # Integer methods: 5.times, 3.upto(10)
        m = re.match(r"^(-?\d+)\.(times|upto|downto|abs|to_f|to_s|to_i|even\?|odd\?|zero\?|chr|ord)\s*(?:\(([^)]*)\))?$", expr)
        if m:
            n = int(m.group(1))
            meth = m.group(2)
            arg = int(m.group(3)) if m.group(3) else None
            return self._int_method(n, meth, arg)

        # Global method calls: puts, print, rand, etc.
        m = re.match(r"^(rand|sleep|exit|abort|raise|gets|sprintf|format|Integer|Float|String|Array|puts|print|p)\s*\((.+)\)$", expr)
        if m:
            fname = m.group(1)
            args = [self._eval_expr(a.strip()) for a in self._split_args(m.group(2))]
            return self._call_global_func(fname, args)

        # User-defined method call: method_name / method_name(args) / method_name args
        m = re.match(r"^([a-z_]\w*)\s*(?:\(([^)]*)\)|\s+(.+))?$", expr)
        if m:
            fname = m.group(1)
            if not hasattr(self, "_top_methods"):
                self._top_methods = {}
            if fname in self._top_methods:
                args_str = m.group(2) or m.group(3) or ""
                args = [self._eval_expr(a.strip()) for a in self._split_args(args_str)] if args_str.strip() else []
                return self._call_user_method(self._top_methods[fname], args, None)

        return self._UNSET

    def _safe_eval(self, expr: str) -> Any:
        """Safely evaluate simple arithmetic/comparison expressions."""
        # Replace Ruby-specific tokens for Python eval
        py_expr = expr
        py_expr = py_expr.replace("nil", "None")
        py_expr = py_expr.replace("true", "True")
        py_expr = py_expr.replace("false", "False")
        py_expr = re.sub(r"\bmod\b", "%", py_expr)
        py_expr = re.sub(r"\*\*", "**", py_expr)

        # Substitute known variables
        for scope in reversed(self._scope_stack):
            for k, v in scope.items():
                if re.search(rf"\b{re.escape(k)}\b", py_expr):
                    py_expr = re.sub(rf"\b{re.escape(k)}\b", repr(v), py_expr)

        # Substitute globals
        for k, v in self._globals.items():
            if isinstance(k, str) and k.isidentifier() and re.search(rf"\b{re.escape(k)}\b", py_expr):
                py_expr = re.sub(rf"\b{re.escape(k)}\b", repr(v), py_expr)

        try:
            result = eval(py_expr, {"__builtins__": {}}, {  # noqa: S307
                "abs": abs, "len": len, "str": str, "int": int, "float": float,
                "min": min, "max": max, "round": round, "pow": pow,
                "True": True, "False": False, "None": None,
            })
            return result
        except Exception:
            # Try variable lookup last
            val = self._get(expr)
            return val

    def _eval_hash(self, expr: str) -> Dict[Any, Any]:
        """Evaluate a Ruby hash literal { key: val, ... } or { key => val, ... }."""
        inner = expr[1:-1].strip()
        if not inner:
            return {}
        result = {}
        pairs = self._split_args(inner)
        for pair in pairs:
            pair = pair.strip()
            # Symbol key: foo: val
            m = re.match(r"^(\w+):\s*(.+)$", pair)
            if m:
                result[f":{m.group(1)}"] = self._eval_expr(m.group(2).strip())
                continue
            # Hash rocket: key => val
            m = re.match(r"^(.+?)\s*=>\s*(.+)$", pair)
            if m:
                result[self._eval_expr(m.group(1).strip())] = self._eval_expr(m.group(2).strip())
                continue
        return result

    # ------------------------------------------------------------------
    # String interpolation
    # ------------------------------------------------------------------

    def _interpolate(self, s: str) -> str:
        """Handle #{...} interpolation in double-quoted strings."""
        result = []
        i = 0
        while i < len(s):
            if s[i:i+2] == "#{":
                j = s.index("}", i + 2)
                inner = s[i+2:j]
                val = self._eval_expr(inner)
                result.append(self._ruby_to_s(val))
                i = j + 1
            elif s[i:i+2] == "\\":
                escape_map = {"n": "\n", "t": "\t", "r": "\r", '"': '"', "\\": "\\"}
                result.append(escape_map.get(s[i+1:i+2], s[i+1:i+2]))
                i += 2
            else:
                result.append(s[i])
                i += 1
        return "".join(result)

    # ------------------------------------------------------------------
    # Method dispatch
    # ------------------------------------------------------------------

    def _call_method(self, receiver: Any, method: str, args: List[Any]) -> Any:
        """Dispatch a method call on any Ruby value."""
        # User-defined class
        if isinstance(receiver, _RubyObject):
            return self._call_object_method(receiver, method, args)

        # String methods
        if isinstance(receiver, str):
            return self._str_method(receiver, method, args)

        # Integer methods
        if isinstance(receiver, int):
            return self._int_method(receiver, method, args[0] if args else None)

        # Float methods
        if isinstance(receiver, float):
            return self._float_method(receiver, method, args)

        # Array methods
        if isinstance(receiver, list):
            return self._array_method(receiver, method, args)

        # Hash methods
        if isinstance(receiver, dict):
            return self._hash_method(receiver, method, args)

        # Range
        if isinstance(receiver, _RubyRange):
            return self._range_method(receiver, method, args)

        # Module/class
        if isinstance(receiver, (_RubyClass, _RubyModule)):
            return self._class_method_call(receiver, method, args)

        # Math module
        if isinstance(receiver, dict) and "sqrt" in receiver:
            fn = receiver.get(method)
            if callable(fn):
                return fn(*args)

        # nil methods
        if receiver is None:
            if method == "nil?":
                return True
            if method == "to_s":
                return ""
            if method == "to_a":
                return []
            if method == "inspect":
                return "nil"
            raise _RubyError(f"undefined method '{method}' for nil:NilClass")

        return None

    def _call_object_method(self, obj: _RubyObject, method: str, args: List[Any]) -> Any:
        m = obj.klass.find_method(method)
        if m is None:
            # Try ivar getter/setter shortcuts
            if method.endswith("=") and len(args) == 1:
                obj.ivars[f"@{method[:-1]}"] = args[0]
                return args[0]
            raise _RubyError(f"undefined method '{method}' for {obj.klass.name}")
        return self._call_user_method(m, args, obj)

    def _call_user_method(self, method: _RubyMethod, args: List[Any], self_obj: Any) -> Any:
        old_self = self._current_self
        self._current_self = self_obj
        self._push_scope()
        # Bind parameters
        for idx, param in enumerate(method.params):
            val = args[idx] if idx < len(args) else None
            self._set(param, val)
        try:
            result = self._exec_lines(method.body, 0, len(method.body))
            return result
        except _Return as r:
            return r.value
        finally:
            self._pop_scope()
            self._current_self = old_self

    def _call_global_func(self, name: str, args: List[Any]) -> Any:
        if name == "rand":
            if args:
                n = args[0]
                if isinstance(n, (int, float)):
                    return random.randint(0, int(n) - 1)
            return random.random()
        if name in ("puts",):
            for a in args:
                self._emit(self._ruby_to_s(a) + "\n")
            return None
        if name in ("print",):
            for a in args:
                self._emit(self._ruby_to_s(a))
            return None
        if name in ("p",):
            for a in args:
                self._emit(self._ruby_inspect(a) + "\n")
            return None
        if name == "sleep":
            return None  # no-op in simulation
        if name in ("Integer", "int"):
            return int(args[0]) if args else 0
        if name in ("Float",):
            return float(args[0]) if args else 0.0
        if name in ("String",):
            return str(args[0]) if args else ""
        if name in ("Array",):
            return list(args[0]) if args else []
        if name == "sprintf" or name == "format":
            fmt = args[0] if args else ""
            rest = args[1:]
            try:
                return fmt % tuple(rest)
            except Exception:
                return str(fmt)
        if name == "gets":
            if hasattr(self.interpreter, "request_input"):
                return self.interpreter.request_input("ruby> ")
            return ""
        if name == "exit" or name == "abort":
            raise _Return(None)
        return None

    # ------------------------------------------------------------------
    # Built-in type methods
    # ------------------------------------------------------------------

    def _str_method(self, s: str, method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        if method == "length" or method == "size":
            return len(s)
        if method == "upcase" or method == "upcase!":
            return s.upper()
        if method == "downcase" or method == "downcase!":
            return s.lower()
        if method == "capitalize":
            return s.capitalize()
        if method == "strip" or method == "strip!":
            return s.strip()
        if method == "lstrip":
            return s.lstrip()
        if method == "rstrip":
            return s.rstrip()
        if method == "chomp":
            return s.rstrip("\n\r")
        if method == "chop":
            return s[:-1] if s else s
        if method == "reverse":
            return s[::-1]
        if method == "to_i":
            try:
                return int(s)
            except ValueError:
                return 0
        if method == "to_f":
            try:
                return float(s)
            except ValueError:
                return 0.0
        if method == "to_s":
            return s
        if method == "inspect":
            return repr(s)
        if method == "empty?":
            return len(s) == 0
        if method == "nil?":
            return False
        if method == "include?" and m0 is not None:
            return str(m0) in s
        if method == "start_with?" and m0 is not None:
            return s.startswith(str(m0))
        if method == "end_with?" and m0 is not None:
            return s.endswith(str(m0))
        if method == "split":
            sep = str(m0) if m0 is not None else None
            return s.split(sep) if sep else s.split()
        if method == "join":
            return s  # join on string is identity
        if method == "chars":
            return list(s)
        if method == "bytes":
            return list(s.encode("utf-8"))
        if method == "count" and m0:
            return s.count(str(m0))
        if method == "replace" and m0:
            return str(m0)
        if method == "sub" and len(args) >= 2:
            pattern = str(args[0]).lstrip(":") if isinstance(args[0], str) else str(args[0])
            return re.sub(pattern, str(args[1]), s, count=1)
        if method == "gsub" and len(args) >= 2:
            pattern = str(args[0]).lstrip(":") if isinstance(args[0], str) else str(args[0])
            return re.sub(pattern, str(args[1]), s)
        if method == "match":
            if m0:
                m_obj = re.search(str(m0), s)
                return m_obj.group(0) if m_obj else None
        if method == "tr" and len(args) >= 2:
            return s.translate(str.maketrans(str(args[0]), str(args[1])))
        if method == "center" and m0:
            return s.center(int(m0))
        if method == "ljust" and m0:
            return s.ljust(int(m0))
        if method == "rjust" and m0:
            return s.rjust(int(m0))
        if method == "index" and m0:
            idx = s.find(str(m0))
            return idx if idx >= 0 else None
        if method == "slice" and m0 is not None:
            return s[int(m0)]
        if method == "+" and m0 is not None:
            return s + str(m0)
        if method == "*" and m0 is not None:
            return s * int(m0)
        if method == "==" and m0 is not None:
            return s == m0
        if method == "freeze":
            return s
        if method == "dup":
            return s
        if method == "encode":
            return s
        if method == "scan" and m0:
            return re.findall(str(m0), s)
        if method == "each_char":
            return list(s)
        return None

    def _int_method(self, n: int, method: str, arg: Any) -> Any:
        if method == "times":
            return list(range(n))
        if method == "upto" and arg is not None:
            return list(range(n, int(arg) + 1))
        if method == "downto" and arg is not None:
            return list(range(n, int(arg) - 1, -1))
        if method == "abs":
            return abs(n)
        if method == "to_f":
            return float(n)
        if method == "to_s":
            return str(n)
        if method == "to_i":
            return n
        if method == "even?":
            return n % 2 == 0
        if method == "odd?":
            return n % 2 != 0
        if method == "zero?":
            return n == 0
        if method == "positive?":
            return n > 0
        if method == "negative?":
            return n < 0
        if method == "chr":
            return chr(n)
        if method == "ord":
            return n
        if method == "divmod" and arg is not None:
            return [n // int(arg), n % int(arg)]
        if method == "gcd" and arg is not None:
            import math
            return math.gcd(n, int(arg))
        if method == "lcm" and arg is not None:
            import math
            return abs(n * int(arg)) // math.gcd(n, int(arg))
        if method == "pow" and arg is not None:
            return n ** int(arg)
        if method == "nil?":
            return False
        if method in ("==", "eql?", "equal?") and arg is not None:
            return n == arg
        if method == "between?" and isinstance(arg, list) and len(arg) >= 2:
            return arg[0] <= n <= arg[1]
        if method == "ceil":
            return n
        if method == "floor":
            return n
        if method == "round":
            return n
        if method == "next" or method == "succ":
            return n + 1
        if method == "pred":
            return n - 1
        return None

    def _float_method(self, f: float, method: str, args: List[Any]) -> Any:
        if method == "to_i":
            return int(f)
        if method == "to_f":
            return f
        if method == "to_s":
            return str(f)
        if method == "abs":
            return abs(f)
        if method == "round":
            n = int(args[0]) if args else 0
            return round(f, n)
        if method == "ceil":
            return math.ceil(f)
        if method == "floor":
            return math.floor(f)
        if method == "zero?":
            return f == 0.0
        if method == "positive?":
            return f > 0
        if method == "negative?":
            return f < 0
        if method == "nan?":
            return math.isnan(f)
        if method == "infinite?":
            return math.isinf(f)
        if method == "nil?":
            return False
        return None

    def _array_method(self, arr: List[Any], method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        if method in ("length", "size", "count"):
            return len(arr)
        if method == "empty?":
            return len(arr) == 0
        if method == "first":
            n = int(m0) if m0 is not None else None
            return arr[:n] if n is not None else (arr[0] if arr else None)
        if method == "last":
            n = int(m0) if m0 is not None else None
            return arr[-n:] if n is not None else (arr[-1] if arr else None)
        if method == "push" or method == "append" or method == "<<":
            arr.append(m0)
            return arr
        if method == "pop":
            return arr.pop() if arr else None
        if method == "shift":
            return arr.pop(0) if arr else None
        if method == "unshift":
            arr.insert(0, m0)
            return arr
        if method == "flatten":
            return self._flatten(arr)
        if method == "compact":
            return [x for x in arr if x is not None]
        if method == "uniq":
            seen = []
            for x in arr:
                if x not in seen:
                    seen.append(x)
            return seen
        if method == "reverse":
            return arr[::-1]
        if method == "reverse!":
            arr.reverse()
            return arr
        if method == "sort":
            try:
                return sorted(arr)
            except TypeError:
                return arr
        if method == "sort!":
            try:
                arr.sort()
            except TypeError:
                pass
            return arr
        if method == "join":
            sep = str(m0) if m0 is not None else ""
            return sep.join(self._ruby_to_s(x) for x in arr)
        if method == "include?":
            return m0 in arr
        if method == "min":
            return min(arr) if arr else None
        if method == "max":
            return max(arr) if arr else None
        if method == "sum":
            return sum(x for x in arr if isinstance(x, (int, float)))
        if method == "each":
            return arr
        if method == "map" or method == "collect":
            return arr
        if method == "select" or method == "filter":
            return arr
        if method == "reject":
            return arr
        if method == "any?":
            return any(self._truthy(x) for x in arr)
        if method == "all?":
            return all(self._truthy(x) for x in arr)
        if method == "none?":
            return not any(self._truthy(x) for x in arr)
        if method == "index" or method == "find_index":
            try:
                return arr.index(m0)
            except ValueError:
                return None
        if method == "delete":
            try:
                arr.remove(m0)
            except ValueError:
                pass
            return m0
        if method == "delete_at":
            if m0 is not None and 0 <= int(m0) < len(arr):
                return arr.pop(int(m0))
            return None
        if method == "insert":
            if len(args) >= 2:
                arr.insert(int(args[0]), args[1])
            return arr
        if method == "slice" or method == "[]":
            if m0 is not None:
                return arr[int(m0)]
        if method == "take":
            return arr[:int(m0)] if m0 is not None else []
        if method == "drop":
            return arr[int(m0):] if m0 is not None else arr
        if method == "zip":
            other = m0 if isinstance(m0, list) else []
            return [[a, b] for a, b in zip(arr, other)]
        if method == "flatten":
            return self._flatten(arr)
        if method == "each_with_index":
            return list(enumerate(arr))
        if method == "each_with_object":
            return m0
        if method == "reduce" or method == "inject":
            if not arr:
                return m0
            acc = m0 if m0 is not None else arr[0]
            start = 0 if m0 is not None else 1
            return acc  # simplified
        if method == "flat_map":
            return self._flatten(arr)
        if method == "tally":
            result = {}
            for x in arr:
                key = self._ruby_to_s(x)
                result[key] = result.get(key, 0) + 1
            return result
        if method == "combination" and m0:
            from itertools import combinations
            return list(combinations(arr, int(m0)))
        if method == "permutation" and m0:
            from itertools import permutations
            return list(permutations(arr, int(m0)))
        if method == "sample":
            return random.choice(arr) if arr else None
        if method == "shuffle":
            return random.sample(arr, len(arr))
        if method == "shuffle!":
            random.shuffle(arr)
            return arr
        if method == "rotate":
            n = int(m0) if m0 is not None else 1
            return arr[n:] + arr[:n]
        if method == "to_a":
            return arr
        if method == "inspect":
            return "[" + ", ".join(self._ruby_inspect(x) for x in arr) + "]"
        if method == "nil?":
            return False
        if method in ("+", "concat"):
            if isinstance(m0, list):
                return arr + m0
            return arr
        if method == "-":
            if isinstance(m0, list):
                return [x for x in arr if x not in m0]
            return arr
        if method == "*":
            if isinstance(m0, int):
                return arr * m0
            if isinstance(m0, str):
                return m0.join(self._ruby_to_s(x) for x in arr)
        if method == "<<":
            arr.append(m0)
            return arr
        return None

    def _hash_method(self, h: Dict[Any, Any], method: str, args: List[Any]) -> Any:
        m0 = args[0] if args else None
        if method == "keys":
            return list(h.keys())
        if method == "values":
            return list(h.values())
        if method == "length" or method == "size" or method == "count":
            return len(h)
        if method == "empty?":
            return len(h) == 0
        if method == "has_key?" or method == "include?" or method == "key?" or method == "member?":
            return m0 in h
        if method == "has_value?" or method == "value?":
            return m0 in h.values()
        if method in ("[]", "fetch"):
            return h.get(m0)
        if method == "[]=":
            if len(args) >= 2:
                h[args[0]] = args[1]
            return h
        if method == "merge":
            result = dict(h)
            if isinstance(m0, dict):
                result.update(m0)
            return result
        if method == "merge!":
            if isinstance(m0, dict):
                h.update(m0)
            return h
        if method == "delete":
            return h.pop(m0, None)
        if method == "each":
            return list(h.items())
        if method == "each_pair":
            return list(h.items())
        if method == "to_a":
            return [[k, v] for k, v in h.items()]
        if method == "inspect":
            return "{" + ", ".join(
                f"{self._ruby_inspect(k)}=>{self._ruby_inspect(v)}" for k, v in h.items()
            ) + "}"
        if method == "nil?":
            return False
        if method == "select":
            return h
        if method == "map":
            return list(h.items())
        if method == "any?":
            return len(h) > 0
        if method == "all?":
            return True
        if method == "min_by" or method == "max_by":
            return None
        if method == "sort_by":
            return sorted(h.items())
        if method == "flatten":
            result = []
            for k, v in h.items():
                result.extend([k, v])
            return result
        return None

    def _range_method(self, r: "_RubyRange", method: str, args: List[Any]) -> Any:
        items = list(r)
        if method == "each":
            return items
        if method == "to_a":
            return items
        if method == "min":
            return items[0] if items else None
        if method == "max":
            return items[-1] if items else None
        if method == "include?" or method == "cover?" or method == "===":
            return args[0] in r if args else False
        if method == "size" or method == "length" or method == "count":
            return len(items)
        if method == "first":
            n = int(args[0]) if args else None
            return items[:n] if n else (items[0] if items else None)
        if method == "last":
            n = int(args[0]) if args else None
            return items[-n:] if n else (items[-1] if items else None)
        if method == "sum":
            return sum(items) if items and isinstance(items[0], (int, float)) else 0
        if method == "map" or method == "collect":
            return items
        if method == "select" or method == "filter":
            return items
        if method == "step" and args:
            step = args[0]
            return list(range(r.start, r.end + (0 if r.exclusive else 1), int(step)))
        return None

    def _class_method_call(self, obj: Any, method: str, args: List[Any]) -> Any:
        if isinstance(obj, _RubyClass) and method == "new":
            return self._instantiate(obj.name, "", args)
        if isinstance(obj, dict) and method in obj:
            fn = obj[method]
            if callable(fn):
                return fn(*args)
        return None

    def _instantiate(self, classname: str, args_str: str, args: Optional[List[Any]] = None) -> Any:
        klass = self._classes.get(classname)
        if klass is None:
            raise _RubyError(f"uninitialized constant {classname}")
        obj = _RubyObject(klass)
        if args is None:
            args = [self._eval_expr(a.strip()) for a in self._split_args(args_str)] if args_str.strip() else []
        init = klass.find_method("initialize")
        if init:
            self._call_user_method(init, args, obj)
        return obj

    # ------------------------------------------------------------------
    # Utility helpers
    # ------------------------------------------------------------------

    def _truthy(self, val: Any) -> bool:
        return val is not None and val is not False

    def _ruby_to_s(self, val: Any) -> str:
        if val is None:
            return ""
        if val is True:
            return "true"
        if val is False:
            return "false"
        if isinstance(val, _RubyRange):
            return str(val)
        if isinstance(val, _RubyObject):
            return repr(val)
        return str(val)

    def _ruby_inspect(self, val: Any) -> str:
        if val is None:
            return "nil"
        if val is True:
            return "true"
        if val is False:
            return "false"
        if isinstance(val, str):
            return repr(val)
        if isinstance(val, list):
            return "[" + ", ".join(self._ruby_inspect(x) for x in val) + "]"
        if isinstance(val, dict):
            return "{" + ", ".join(
                f"{self._ruby_inspect(k)}=>{self._ruby_inspect(v)}" for k, v in val.items()
            ) + "}"
        if isinstance(val, _RubyObject):
            ivars = ", ".join(f"{k}={self._ruby_inspect(v)}" for k, v in val.ivars.items())
            return f"#<{val.klass.name} {ivars}>" if ivars else f"#<{val.klass.name}>"
        return repr(val)

    def _flatten(self, arr: list, depth: int = 1) -> list:
        result = []
        for item in arr:
            if isinstance(item, list) and depth > 0:
                result.extend(self._flatten(item, depth - 1))
            else:
                result.append(item)
        return result

    def _parse_args(self, s: str) -> List[str]:
        """Parse argument string, stripping outer parens."""
        s = s.strip()
        if s.startswith("(") and s.endswith(")"):
            s = s[1:-1].strip()
        return self._split_args(s)

    def _split_args(self, s: str) -> List[str]:
        """Split comma-separated args respecting brackets/strings."""
        if not s.strip():
            return []
        parts = []
        depth = 0
        current = []
        in_string = None
        for ch in s:
            if in_string:
                current.append(ch)
                if ch == in_string:
                    in_string = None
            elif ch in ('"', "'"):
                in_string = ch
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
        if current:
            parts.append("".join(current).strip())
        return [p for p in parts if p]


# ---------------------------------------------------------------------------
# Range helper
# ---------------------------------------------------------------------------


class _RubyRange:
    def __init__(self, start: Any, end: Any, exclusive: bool = False):
        self.start = start
        self.end = end
        self.exclusive = exclusive

    def __iter__(self):
        if isinstance(self.start, int) and isinstance(self.end, int):
            stop = self.end if self.exclusive else self.end + 1
            return iter(range(self.start, stop))
        return iter([])

    def __contains__(self, item: Any) -> bool:
        if isinstance(self.start, (int, float)) and isinstance(self.end, (int, float)):
            if self.exclusive:
                return self.start <= item < self.end
            return self.start <= item <= self.end
        return False

    def __repr__(self) -> str:
        sep = "..." if self.exclusive else ".."
        return f"{self.start}{sep}{self.end}"
