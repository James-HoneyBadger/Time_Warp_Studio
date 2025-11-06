#!/usr/bin/env python3
"""Small debug runner for TempleInterpreter procedures.

Loads a short TempleCode program, prints interpreter metadata, and runs it.
"""

from __future__ import annotations

import os
import sys

# Ensure the app shim package (Python/time_warp/templecode) is importable
_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
_APP_PKG = os.path.join(_ROOT, "Python", "time_warp")
if _APP_PKG not in sys.path:
    sys.path.insert(0, _APP_PKG)

import importlib

templecode = importlib.import_module("templecode")
TempleInterpreter = templecode.TempleInterpreter  # type: ignore[attr-defined]


def main() -> int:
    """Run a tiny program through the interpreter and print details."""
    code = """PRINT "Line 1"
PROC test n
  PRINT "In proc"
ENDPROC
PRINT "Line after ENDPROC"
CALL test 5
PRINT "Line after CALL"
"""

    interp = TempleInterpreter()
    interp.load_program(code)

    print(f"Loaded {len(interp.lines)} lines")
    for i, line in enumerate(interp.lines):
        print(f"{i}: {line}")

    print(f"\nProcs: {interp.procs}")

    print("\nRunning:")
    interp.run_program()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
