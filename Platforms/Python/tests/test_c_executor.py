#!/usr/bin/env python3
"""Unit tests for the in-tree C executor (c_lang_fixed).

These tests make sure common C-like inputs (preprocessor, block comments,
function headers, return statements) are tolerated and that printf works.
"""

import sys
from pathlib import Path
import importlib

# Load in-tree package: ensure Python implementation package is importable
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

# Dynamic imports to avoid static resolver issues in linters
core_mod = importlib.import_module("time_warp.core.interpreter")
graphics_mod = importlib.import_module("time_warp.graphics.turtle_state")

Interpreter = getattr(core_mod, "Interpreter")
Language = getattr(core_mod, "Language")
TurtleState = getattr(graphics_mod, "TurtleState")


def test_c_executor_ignores_includes_and_comments():
    """C-like inputs are tolerated; printf output appears without errors."""
    code = """
/* Hello World - Your First C Program */
#include <stdio.h>
int main() {
    printf("Hello, World!\n");
    printf("Welcome to C programming!\n");
    return 0;
}
"""

    interp = Interpreter()
    interp.set_language(Language.C)
    interp.load_program(code)

    turtle = TurtleState()
    out = interp.execute(turtle)

    # Ensure the expected printf output appears and no Unknown command errors
    full = "\n".join(out)
    assert "Hello, World!" in full
    assert "Welcome to C programming!" in full
    assert "Unknown C command" not in full
