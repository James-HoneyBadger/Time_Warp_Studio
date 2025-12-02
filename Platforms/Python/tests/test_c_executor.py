#!/usr/bin/env python3
"""Unit tests for the in-tree C executor (c_lang_fixed).

These tests make sure common C-like inputs (preprocessor, block comments,
function headers, return statements) are tolerated and that printf works.
"""

import sys
from pathlib import Path

# Load in-tree package
p = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(p))

from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


def test_c_executor_ignores_includes_and_comments():
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
