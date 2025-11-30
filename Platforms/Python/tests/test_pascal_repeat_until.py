"""Tests for Pascal 'repeat...until' loops.

These tests live in-tree so we need to ensure the Platforms/Python package
is on sys.path during collection. Use pathlib to add the in-repo path.
"""

import sys
import unittest
from pathlib import Path

# Add in-tree package path so tests import the in-repo package correctly.
p = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(p))  # pylint: disable=wrong-import-position

# Imports below intentionally after sys.path insertion when running tests
# from the repository root.
# pylint: disable=wrong-import-position
# pylint: disable=import-error
# Silence static import errors for the in-tree package imports below.
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


class TestPascalRepeatUntil(unittest.TestCase):
    """Unit tests for Pascal repeat...until semantics."""

    def test_repeat_until_terminates(self):
        """Verify that a repeat-until loop terminates and produces output."""
        code = "\n".join(
            [
                "var i: integer;",
                "repeat",
                "  writeln(i);",
                "  i := i + 1;",
                "until i >= 3;",
            ]
        )
        interp = Interpreter()
        interp.set_language(Language.PASCAL)
        interp.load_program(code, language=Language.PASCAL)
        out = interp.execute(TurtleState())
        # Expect exactly 0,1,2 then terminate
        self.assertEqual(out, ["0\n", "1\n", "2\n"])  # no runaway


if __name__ == "__main__":
    unittest.main()
