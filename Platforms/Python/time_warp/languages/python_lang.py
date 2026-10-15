"""Python language executor for Time Warp Studio.

Educational Python interpreter — whole-program execution via sandboxed exec().

Supports the full Python 3 language via a restricted exec() environment:
  - All built-in types and operators
  - Safe subset of builtins (no file I/O, no subprocess, no os.system)
  - math, random, itertools, functools, collections, string, json, re, copy
  - print() → captured to output
  - input() → returns '' (non-interactive mode); turtle-style INPUT command
  - Turtle graphics via `forward(n)`, `backward(n)`, `right(n)`, `left(n)`,
    `penup()`, `pendown()`, `goto(x,y)`, `color(r,g,b)`, `home()`,
    `setheading(deg)`, `pensize(n)`, `clear_canvas()`

Security notes:
  - open, exec, eval, compile, globals, locals, vars, dir,
    getattr, setattr, delattr are not in the safe builtins
  - sys, os, subprocess, socket, threading, multiprocessing are blocked
  - __import__ is restricted to an explicit safe-module allowlist
  - AST pre-check blocks access to __subclasses__ and __globals__ attributes,
    preventing the classic object.__subclasses__() → subprocess.Popen escape
  - Resource-bound: max iterations enforced via sys.settrace not used here;
    instead the executor relies on Python's own timeout (called from
    interpreter with MAX_EXECUTION_TIME)
"""

from __future__ import annotations

import ast as _ast
import io
import math
import random
import traceback
from contextlib import redirect_stdout
from typing import TYPE_CHECKING, Any

from ..utils.execution_timeout import ExecutionTimeoutError

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState

# ---------------------------------------------------------------------------
# Sandbox AST safety checker
# ---------------------------------------------------------------------------

# Attribute names that can be used to escape the exec() sandbox.
# The classic escape: object.__subclasses__() finds subprocess.Popen;
# function.__globals__ leaks the outer Python namespace.
_BLOCKED_ATTRIBUTES: frozenset[str] = frozenset({
    "__subclasses__",  # object.__subclasses__() → subprocess.Popen escape
    "__globals__",     # func.__globals__ → outer-scope namespace leak
    "__builtins__",
    "__code__",
    "__closure__",
    "gi_frame",
    "gi_code",
    "cr_frame",
    "f_globals",
    "f_locals",
    "f_back",
    "f_builtins",
})


class _SandboxChecker(_ast.NodeVisitor):
    """AST visitor that raises SyntaxError on forbidden attribute accesses."""

    def visit_Attribute(self, node: _ast.Attribute) -> None:
        if node.attr in _BLOCKED_ATTRIBUTES:
            raise SyntaxError(
                f"Access to '{node.attr}' is not allowed in the Time Warp Python sandbox",
                ("<sandbox_check>", node.lineno, node.col_offset, None),
            )
        self.generic_visit(node)


class _ASTBudgetInjector(_ast.NodeTransformer):
    """Injects calls to ``__tw_tick__()`` at loop and function headers."""

    @staticmethod
    def _make_tick(ref_node: _ast.AST) -> _ast.Expr:
        lineno = getattr(ref_node, "lineno", 1)
        call = _ast.Expr(
            value=_ast.Call(
                func=_ast.Name(id="__tw_tick__", ctx=_ast.Load()),
                args=[_ast.Constant(value=lineno)],
                keywords=[],
            )
        )
        return _ast.copy_location(call, ref_node)

    def visit_While(self, node: _ast.While) -> _ast.AST:
        self.generic_visit(node)
        node.body.insert(0, self._make_tick(node))
        return node

    def visit_For(self, node: _ast.For) -> _ast.AST:
        self.generic_visit(node)
        node.body.insert(0, self._make_tick(node))
        return node

    def visit_AsyncFor(self, node: _ast.AsyncFor) -> _ast.AST:
        self.generic_visit(node)
        node.body.insert(0, self._make_tick(node))
        return node

    def visit_FunctionDef(self, node: _ast.FunctionDef) -> _ast.AST:
        self.generic_visit(node)
        insert_idx = 1 if (
            node.body
            and isinstance(node.body[0], _ast.Expr)
            and isinstance(getattr(node.body[0], "value", None), _ast.Constant)
            and isinstance(node.body[0].value.value, str)
        ) else 0
        node.body.insert(insert_idx, self._make_tick(node))
        return node


# ---------------------------------------------------------------------------
# Allowed imports for the sandboxed executor
# ---------------------------------------------------------------------------

_ALLOWED_IMPORTS: frozenset[str] = frozenset({
    "math", "random", "itertools", "functools", "collections",
    "string", "json", "re", "copy", "abc", "typing",
    "decimal", "fractions", "statistics", "heapq", "bisect",
    "operator", "datetime", "enum", "dataclasses",
    "collections.abc", "typing_extensions", "time", "__future__",
})

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def execute_python_lang(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    """Execute a Python program in a sandboxed namespace and return output."""
    executor = PythonExecutor(interpreter, turtle)
    return executor.run(source)


# ---------------------------------------------------------------------------
# Executor
# ---------------------------------------------------------------------------


class PythonExecutor:
    def __init__(self, interpreter: "Interpreter", turtle: "TurtleState"):
        self.interpreter = interpreter
        self.turtle = turtle
        self._output: list[str] = []

    def run(self, source: str) -> str:
        buf = io.StringIO()
        # AST safety pre-check — must happen before compile() / exec()
        try:
            tree = _ast.parse(source, "<time_warp_python>", "exec")
            _SandboxChecker().visit(tree)
            tree = _ASTBudgetInjector().visit(tree)
            _ast.fix_missing_locations(tree)
            code = compile(tree, "<time_warp_python>", "exec")
        except SyntaxError as e:
            line_info = f" (line {e.lineno})" if e.lineno else ""
            buf.write(f"❌ SyntaxError{line_info}: {e.msg}\n")
            out = buf.getvalue()
            return out if out.endswith("\n") else out + "\n"
        sandbox = self._build_sandbox(buf)
        try:
            with redirect_stdout(buf):
                exec(code, sandbox)  # noqa: S102  # type: ignore[misc]  # pylint: disable=exec-used
        except SystemExit:
            pass
        except ExecutionTimeoutError as e:
            buf.write(f"❌ Python execution stopped: {e}\n")
        except SyntaxError as e:
            line_info = f" (line {e.lineno})" if e.lineno else ""  # type: ignore[truthy-function]  # pylint: disable=using-constant-test
            buf.write(f"❌ SyntaxError{line_info}: {e.msg}\n")
        except Exception:
            tb_lines = traceback.format_exc().splitlines()
            # Keep only: file-ref lines for user code, and the final exception line.
            # Exclude internal executor frames and raw source-code lines.
            filtered = [
                line for line in tb_lines
                if "<time_warp_python>" in line
                or (not line.startswith("  File") and not line.startswith("    "))
            ]
            # Always include the last line (ExcType: message)
            user_lines = [line for line in filtered if line.strip()]
            msg = "\n".join(user_lines[-3:]) if user_lines else "unknown error"
            buf.write(f"❌ {msg}\n")
        out = buf.getvalue()
        return out if out.endswith("\n") else out + "\n"

    def _build_sandbox(self, buf: io.StringIO) -> dict[str, Any]:
        """Build the restricted execution namespace."""
        import itertools
        import functools
        import collections
        import string
        import json
        import re as _re
        import copy

        turtle = self.turtle

        # Turtle functions exposed to user code
        def forward(n: float) -> None:
            turtle.forward(float(n))

        def backward(n: float) -> None:
            turtle.forward(-float(n))

        def right(n: float) -> None:
            turtle.right(float(n))

        def left(n: float) -> None:
            turtle.left(float(n))

        def penup() -> None:
            turtle.penup()

        def pendown() -> None:
            turtle.pendown()

        def home() -> None:
            turtle.home()

        def goto(x: float, y: float) -> None:
            turtle.goto(float(x), float(y))

        def setheading(deg: float) -> None:
            turtle.setheading(float(deg))

        def color(*args: Any) -> None:
            if len(args) >= 3:
                turtle.setcolor(int(args[0]), int(args[1]), int(args[2]))
            elif args:
                turtle.pencolor(str(args[0]))

        def pensize(n: int) -> None:
            turtle.setpenwidth(float(n))

        def clear_canvas() -> None:
            turtle.reset()

        max_output_bytes = 512_000
        if hasattr(self.interpreter, "budget") and self.interpreter.budget is not None:
            max_output_bytes = self.interpreter.budget.max_output_bytes

        def _tick(line_no: int = 1) -> None:
            if hasattr(self.interpreter, "tick"):
                self.interpreter.tick(1)
            if getattr(self.interpreter, "debug_mode", False):
                timeline = getattr(self.interpreter, "debug_timeline", None)
                if timeline is not None and len(timeline.frames) < 500:
                    timeline.record_frame(
                        line=int(line_no),
                        line_content=f"python:L{int(line_no)}",
                        variables=self.interpreter.get_variables(),
                    )
                if (
                    getattr(self.interpreter, "step_mode", False)
                    or int(line_no) in getattr(self.interpreter, "breakpoints", set())
                ):
                    if hasattr(self.interpreter, "_do_debug_pause"):
                        self.interpreter._do_debug_pause(int(line_no))

        # Safe print that writes to buf with output size cap
        def safe_print(*args: Any, sep: str = " ", end: str = "\n") -> None:
            _tick()
            text = sep.join(str(a) for a in args) + end
            if buf.tell() + len(text) > max_output_bytes:
                raise ExecutionTimeoutError(
                    f"Output limit ({max_output_bytes} bytes) exceeded"
                )
            buf.write(text)

        # Non-interactive input
        def safe_input(prompt: str = "") -> str:
            _tick()
            if prompt:
                buf.write(str(prompt))
            if hasattr(self.interpreter, "request_input"):
                return str(self.interpreter.request_input(str(prompt)) or "")
            return ""

        # Safe exit
        def safe_exit(code: int = 0) -> None:
            raise SystemExit(code)

        # Controlled __import__ — only allows whitelisted safe modules
        _real_import = __import__

        def controlled_import(
            name: str,
            globs: Any = None,
            locs: Any = None,
            fromlist: tuple = (),
            level: int = 0,
        ) -> Any:
            top = name.split(".")[0]
            if top in _ALLOWED_IMPORTS:
                return _real_import(name, globs, locs, fromlist, level)
            raise ImportError(
                f"Import of '{name}' is not allowed in the Time Warp Python sandbox"
            )

        # Restricted builtins — whitelist approach
        safe_builtins: dict[str, Any] = {
            # Types
            "int": int, "float": float, "str": str, "bool": bool,
            "list": list, "tuple": tuple, "dict": dict, "set": set,
            "frozenset": frozenset, "bytes": bytes, "bytearray": bytearray,
            "complex": complex, "type": type,
            # Iterators / sequence tools
            "range": range, "enumerate": enumerate, "zip": zip,
            "map": map, "filter": filter, "reversed": reversed,
            "sorted": sorted, "len": len, "sum": sum,
            "min": min, "max": max, "abs": abs, "round": round,
            "divmod": divmod, "pow": pow, "hash": hash, "id": id,
            # String / repr
            "repr": repr, "format": format, "chr": chr, "ord": ord,
            "hex": hex, "oct": oct, "bin": bin,
            # Logic
            "any": any, "all": all, "callable": callable,
            "isinstance": isinstance, "issubclass": issubclass,
            # Collections helpers
            "iter": iter, "next": next, "slice": slice,
            "staticmethod": staticmethod, "classmethod": classmethod,
            "property": property, "super": super,
            # Class definition support
            "__build_class__": __build_class__,
            # I/O
            "print": safe_print, "input": safe_input,
            # Exceptions
            "Exception": Exception, "ValueError": ValueError,
            "TypeError": TypeError, "KeyError": KeyError,
            "IndexError": IndexError, "AttributeError": AttributeError,
            "RuntimeError": RuntimeError, "StopIteration": StopIteration,
            "ZeroDivisionError": ZeroDivisionError,
            "NotImplementedError": NotImplementedError,
            "OverflowError": OverflowError, "MemoryError": MemoryError,
            "RecursionError": RecursionError, "AssertionError": AssertionError,
            "__tw_tick__": _tick,
            "ArithmeticError": ArithmeticError, "OSError": OSError,
            "IOError": IOError, "FileNotFoundError": FileNotFoundError,
            "PermissionError": PermissionError, "TimeoutError": TimeoutError,
            "UnicodeError": UnicodeError, "UnicodeDecodeError": UnicodeDecodeError,
            "UnicodeEncodeError": UnicodeEncodeError,
            # Misc
            "True": True, "False": False, "None": None,
            "NotImplemented": NotImplemented, "Ellipsis": ...,
            "exit": safe_exit, "quit": safe_exit,
            "breakpoint": lambda *a, **k: None,  # no-op
            "__import__": controlled_import,
            "__name__": "__main__",
            "__doc__": None,
        }

        sandbox: dict[str, Any] = {
            "__builtins__": safe_builtins,
            # Standard library modules (safe subsets)
            "math": math,
            "random": random,
            "itertools": itertools,
            "functools": functools,
            "collections": collections,
            "string": string,
            "json": json,
            "re": _re,
            "copy": copy,
            # Turtle graphics
            "forward": forward, "fd": forward,
            "backward": backward, "bd": backward,
            "right": right, "rt": right,
            "left": left, "lt": left,
            "penup": penup, "pu": penup,
            "pendown": pendown, "pd": pendown,
            "home": home,
            "goto": goto, "setpos": goto,
            "setheading": setheading,
            "color": color,
            "pensize": pensize, "width": pensize,
            "clear_canvas": clear_canvas,
        }
        return sandbox
