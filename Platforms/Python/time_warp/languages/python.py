"""Python sandbox executor for Time Warp Studio.

Executes student Python code in a restricted namespace that blocks access to
dangerous modules (os, subprocess, sys, socket, etc.) while providing the
standard math/string/turtle-style builtins students need.

Design: The executor collects the full program text instead of running it
line-by-line (unlike BASIC/Logo) because Python is block-structured.
The interpreter's ``load_program`` method still splits lines into
``program_lines``, but ``execute_python`` receives the original source text
through the ``program_source`` attribute set by the interpreter.
"""

from __future__ import annotations

import builtins
import math
import random
import traceback
from io import StringIO
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Safe builtins whitelist
# ---------------------------------------------------------------------------

_SAFE_BUILTINS = {
    # Built-in types
    "bool", "int", "float", "str", "bytes", "bytearray",
    "list", "tuple", "set", "frozenset", "dict",
    "complex", "range", "slice", "type", "object",
    # I/O helpers
    "print", "input", "len", "repr", "format",
    # Itertools-style
    "enumerate", "zip", "map", "filter", "reversed", "sorted",
    "iter", "next", "any", "all", "sum", "min", "max",
    # Conversion / inspection
    "abs", "round", "chr", "ord", "bin", "oct", "hex",
    "hash", "id", "callable", "isinstance", "issubclass",
    "hasattr", "getattr", "setattr", "delattr", "vars", "dir",
    # Misc
    "staticmethod", "classmethod", "property", "super",
    "NotImplemented", "Ellipsis",
    # Exceptions (commonly needed)
    "Exception", "ValueError", "TypeError", "KeyError",
    "IndexError", "AttributeError", "NameError",
    "ZeroDivisionError", "StopIteration", "RuntimeError",
    "OverflowError", "NotImplementedError", "AssertionError",
    "ArithmeticError", "LookupError", "IOError",
    # Truth constants
    "True", "False", "None",
}

_BLOCKED_MODULES = frozenset({
    "os", "sys", "subprocess", "socket", "shutil", "pathlib",
    "importlib", "builtins", "ctypes", "multiprocessing",
    "threading", "signal", "pty", "atexit", "gc",
    "inspect", "ast", "dis", "code", "codeop",
    "pkgutil", "importlib", "site", "sysconfig",
})


def _make_safe_builtins(output_buffer: StringIO, input_fn) -> dict:
    """Return a builtins dict restricted to safe operations."""
    safe: dict = {
        name: getattr(builtins, name)
        for name in _SAFE_BUILTINS
        if hasattr(builtins, name)
    }

    # Override print to write to our captured buffer
    def _safe_print(*args, sep=" ", end="\n", **_kwargs):
        text = sep.join(str(a) for a in args) + end
        output_buffer.write(text)

    def _safe_input(prompt=""):
        if prompt:
            output_buffer.write(str(prompt))
        return input_fn(str(prompt))

    safe["print"] = _safe_print
    safe["input"] = _safe_input
    safe["__build_class__"] = builtins.__build_class__

    # Provide safe __import__ — only allow a curated set
    _ALLOWED_IMPORTS = {"math", "random", "string", "re", "json",
                        "collections", "itertools", "functools",
                        "datetime", "time", "decimal", "fractions"}

    def _safe_import(name, *args, **kwargs):
        base = name.split(".")[0]
        if base in _BLOCKED_MODULES:
            raise ImportError(
                f"❌ Module '{name}' is not available in the sandbox.\n"
                f"   Allowed modules: {', '.join(sorted(_ALLOWED_IMPORTS))}"
            )
        if base not in _ALLOWED_IMPORTS:
            raise ImportError(
                f"❌ Module '{name}' is not in the allowed list.\n"
                f"   Allowed modules: {', '.join(sorted(_ALLOWED_IMPORTS))}"
            )
        return __import__(name, *args, **kwargs)

    safe["__import__"] = _safe_import
    safe["__builtins__"] = safe
    return safe


def execute_python(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Execute a full Python program in a restricted sandbox.

    Args:
        interpreter: The shared interpreter state.
        source: Complete Python source text.
        turtle: Current turtle graphics state (not used directly but
                available via the ``turtle`` name in the sandbox).

    Returns:
        All captured output as a single string.
    """
    output_buf = StringIO()

    # Simple input function — uses pending_input queue if available
    def _input_fn(prompt: str = "") -> str:
        return interpreter.request_input(str(prompt)) or ""

    safe_globals = _make_safe_builtins(output_buf, _input_fn)
    safe_globals["__name__"] = "__main__"

    # Provide math and random at module level for convenience
    safe_globals["math"] = math
    safe_globals["random"] = random

    # Minimal turtle-turtle bridge: expose simple drawing calls that
    # update the shared TurtleState so the canvas shows the output.
    class _Turtle:
        """Minimal turtle bridge for Python sandbox."""

        def forward(self, dist):
            turtle.forward(float(dist))

        def fd(self, dist):
            self.forward(dist)

        def back(self, dist):
            turtle.forward(-float(dist))

        def bk(self, dist):
            self.back(dist)

        def right(self, angle):
            turtle.turn(float(angle))

        def rt(self, angle):
            self.right(angle)

        def left(self, angle):
            turtle.turn(-float(angle))

        def lt(self, angle):
            self.left(angle)

        def penup(self):
            turtle.pen_up()

        def pu(self):
            self.penup()

        def pendown(self):
            turtle.pen_down()

        def pd(self):
            self.pendown()

        def home(self):
            turtle.home()

        def clear(self):
            turtle.clear()

        def setpos(self, x, y=None):
            if y is None and hasattr(x, "__iter__"):
                x, y = x
            turtle.set_position(float(x), float(y))

        def goto(self, x, y=None):
            self.setpos(x, y)

        def setheading(self, angle):
            turtle.angle = float(angle)

        def seth(self, angle):
            self.setheading(angle)

        def pencolor(self, *args):
            if len(args) == 1:
                turtle.color = args[0]
            elif len(args) == 3:
                turtle.color = "#{:02x}{:02x}{:02x}".format(*[int(v) for v in args])

        def pensize(self, width):
            turtle.pen_width = int(width)

        def width(self, w):
            self.pensize(w)

        def hideturtle(self):
            pass  # visual-only; not tracked in TurtleState

        def showturtle(self):
            pass

        def speed(self, _s):
            pass

        # ---- Fill / colour API ----

        def color(self, *args):
            """Set pen (and optionally fill) colour. Mirrors stdlib turtle.color()."""
            if len(args) == 1:
                turtle.color = args[0]
            elif len(args) >= 2:
                # first arg = pen colour, second = fill colour (store as pen)
                turtle.color = args[0]
                turtle._fill_color = args[1]  # type: ignore[attr-defined]

        def fillcolor(self, *args):
            """Set fill colour independently."""
            if len(args) == 1:
                turtle._fill_color = args[0]  # type: ignore[attr-defined]
            elif len(args) == 3:
                turtle._fill_color = "#{:02x}{:02x}{:02x}".format(*[int(v) for v in args])  # type: ignore[attr-defined]

        def begin_fill(self):
            """Mark the start of a fill shape."""
            turtle._filling = True  # type: ignore[attr-defined]
            turtle._fill_points = []  # type: ignore[attr-defined]

        def end_fill(self):
            """Close and fill the current shape."""
            turtle._filling = False  # type: ignore[attr-defined]

        def write(self, text, *_args, **_kwargs):
            """Write text at current turtle position (logged to output)."""
            output_buf.write(str(text) + "\n")

    safe_globals["turtle"] = _Turtle()

    # ── SQL Server 2000 integration ──────────────────────────────────────
    # Provide sql_execute() and sql_connect() so students can run T-SQL
    # from Python programs just like they would with a real DB connection.
    def _sql_connect(db: str = "master"):
        """Return a lightweight SQL connection handle for the named database."""
        from ..core.sql_engine import SQLSession
        sess = SQLSession()
        if db and db != "master":
            sess._switch_db(db)
        # Store on interpreter so other languages can share the session
        interpreter.sql_session = sess
        return sess

    def _sql_execute(query: str, db: str = "master"):
        """Execute a T-SQL statement and return output as a string.

        Example::

            result = sql_execute("SELECT TOP 5 * FROM Students")
            print(result)
        """
        sess = getattr(interpreter, "sql_session", None) or _sql_connect(db)
        output = sess.run_statement(query)
        output_buf.write(output + "\n")
        return output

    def _sql_query(query: str, db: str = "master"):
        """Execute a SELECT and return a list of dicts (one per row)."""
        from ..core.sql_engine import _translate_tsql
        sess = getattr(interpreter, "sql_session", None) or _sql_connect(db)
        try:
            cur = sess._conn.execute(_translate_tsql(query))
            if cur.description:
                cols = [d[0] for d in cur.description]
                return [dict(zip(cols, row)) for row in cur.fetchall()]
        except Exception as e:
            output_buf.write(f"❌ sql_query error: {e}\n")
        return []

    safe_globals["sql_execute"] = _sql_execute
    safe_globals["sql_query"] = _sql_query
    safe_globals["sql_connect"] = _sql_connect
    # ─────────────────────────────────────────────────────────────────────

    try:
        exec(compile(source, "<sandbox>", "exec"), safe_globals)  # noqa: S102
    except SyntaxError as exc:
        return (
            f"❌ SyntaxError on line {exc.lineno}: {exc.msg}\n"
            f"   {exc.text or ''}"
        )
    except Exception as exc:  # pylint: disable=broad-except
        tb = traceback.extract_tb(exc.__traceback__)
        # Only show the user's stack frames (from <sandbox>)
        user_frames = [f for f in tb if f.filename == "<sandbox>"]
        if user_frames:
            last = user_frames[-1]
            return (
                f"❌ {type(exc).__name__} on line {last.lineno}: {exc}\n"
            )
        return f"❌ {type(exc).__name__}: {exc}\n"

    return output_buf.getvalue()
