#!/usr/bin/env python3
"""Simple runner for TempleCode files.

Usage:
    python scripts/run_templecode.py <file.tc>

This script initializes the Temple interpreter with console I/O and a
no-op turtle backend, loads the provided source file, and executes it.
It returns a non-zero exit code on error to support shell scripting.
"""

import sys
import importlib
from pathlib import Path


def main() -> int:
    """Run a TempleCode source file passed on the command line.

    Returns an integer process exit code:
    - 0 on success
    - 1 on usage error (missing argument)
    - 2 when the file cannot be read
    """
    if len(sys.argv) < 2:
        print("Usage: python scripts/run_templecode.py <file.tc>")
        return 1
    path = sys.argv[1]
    try:
        with open(path, "r", encoding="utf-8") as f:
            code = f.read()
    except OSError as e:
        print(f"Error reading {path}: {e}")
        return 2

    # Import 'templecode' shim dynamically; adjust sys.path when
    # running from the repository checkout
    try:
        tc_mod = importlib.import_module("templecode")
    except (ImportError, ModuleNotFoundError):
        repo_root = Path(__file__).resolve().parents[1]
        shim_dir = repo_root / "Python" / "time_warp"
        if str(shim_dir) not in sys.path:
            sys.path.insert(0, str(shim_dir))
        tc_mod = importlib.import_module("templecode")

    interp = tc_mod.TempleInterpreter(
        io=tc_mod.ConsoleIO(),
        turtle=tc_mod.NullTurtle(),
    )
    interp.run(code)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
