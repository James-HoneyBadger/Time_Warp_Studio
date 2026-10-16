#!/usr/bin/env python3
"""Headless driver for the Python Scheme reference implementation.

`execute_scheme()` expects an Interpreter and a TurtleState, but it only ever
touches `interpreter.tick(n)` and the turtle drawing methods.  Both are duck
typed, so lightweight stubs let the reference run with no Qt, no display and no
interpreter instance -- which is what the differential harness needs.

Used by:
    native/tools/scheme_differential_check.py
    native/tools/scheme_probe.py
"""

from __future__ import annotations

import sys
from pathlib import Path

_REPO_ROOT = Path(__file__).resolve().parents[2]
_PY_ROOT = _REPO_ROOT / "native" / "bridge"
if str(_PY_ROOT) not in sys.path:
    sys.path.insert(0, str(_PY_ROOT))

from time_warp.languages.scheme import execute_scheme  # noqa: E402


class _StubTurtle:
    """Records turtle calls; drawing is irrelevant to Scheme output."""

    def __init__(self) -> None:
        self.calls: list[tuple[str, tuple]] = []

    def _record(self, name, *args):
        self.calls.append((name, args))

    def forward(self, d):
        self._record("forward", d)

    def back(self, d):
        self._record("back", d)

    def right(self, d):
        self._record("right", d)

    def left(self, d):
        self._record("left", d)

    def goto(self, x, y):
        self._record("goto", x, y)

    def setheading(self, d):
        self._record("setheading", d)

    def setpenwidth(self, w):
        self._record("setpenwidth", w)

    def setcolor(self, r, g, b):
        self._record("setcolor", r, g, b)

    def penup(self):
        self._record("penup")

    def pendown(self):
        self._record("pendown")

    def home(self):
        self._record("home")

    def clear(self):
        self._record("clear")

    def circle(self, r):
        self._record("circle", r)

    def showturtle(self):
        self._record("showturtle")

    def hideturtle(self):
        self._record("hideturtle")


class _StubInterpreter:
    """Counts step-budget ticks so runaway programs can be observed."""

    def __init__(self) -> None:
        self.steps = 0

    def tick(self, n: int = 1) -> None:
        self.steps += n


def make_context():
    """Returns (interpreter, turtle) stubs for one program run."""
    return _StubInterpreter(), _StubTurtle()


def run_scheme(source: str) -> str:
    """Runs a Scheme program through the reference, returning its output text."""
    interpreter, turtle = make_context()
    return execute_scheme(interpreter, source, turtle)


def run_capture(source: str):
    """Returns (output, turtle_calls, steps)."""
    interpreter, turtle = make_context()
    out = execute_scheme(interpreter, source, turtle)
    return out, turtle.calls, interpreter.steps


if __name__ == "__main__":
    print(run_scheme(sys.stdin.read()), end="")
