"""Unit tests for the C-language loop/assignment semantics.

These tests run from the repository root; add the in-tree Platforms/Python
package path at runtime so the package is importable during pytest collection.
"""

import sys
import unittest
from pathlib import Path

# Add in-tree import path (Platforms/Python) and allow test-time imports.
p = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(p))  # pylint: disable=wrong-import-position

# These imports require the in-tree package path be added above; keep the
# placement intentional and silence the pylinters for import position.
# pylint: disable=wrong-import-position
# These imports are intentionally after sys.path modification during tests.
# Silence static import-error diagnostics raised by linters.
# pylint: disable=import-error
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


class TestCLoops(unittest.TestCase):
    """Tests for basic loop and assignment behaviour in the C language."""

    def test_simple_assignment_updates_variable(self):
        """Simple assignment should update a scalar variable and print it."""
        code = "\n".join(
            [
                "int i = 0;",
                "i = i + 1;",
                'printf("%d\\n", i);',
            ]
        )
        interp = Interpreter()
        interp.set_language(Language.C)
        interp.load_program(code, language=Language.C)
        out = interp.execute(TurtleState())
        self.assertEqual(out, ["1\n"])  # assignment should work

    def test_for_loop_basic(self):
        """A standard for loop should iterate the expected number of times."""
        code = "\n".join(
            [
                "int i = 0;",
                "for (i = 0; i < 3; i++) {",
                '  printf("%d\\n", i);',
                "}",
            ]
        )
        interp = Interpreter()
        interp.set_language(Language.C)
        interp.load_program(code, language=Language.C)
        out = interp.execute(TurtleState())
        self.assertEqual(out, ["0\n", "1\n", "2\n"])  # exact 0..2

    def test_do_while_runs_at_least_once(self):
        """A do..while loop executes its body at least once."""
        code = "\n".join(
            [
                "int i = 5;",
                "do {",
                '  printf("hello\\n");',
                "}",
                "while (i < 0);",
            ]
        )
        interp = Interpreter()
        interp.set_language(Language.C)
        interp.load_program(code, language=Language.C)
        out = interp.execute(TurtleState())
        self.assertEqual(out, ["hello\n"])  # should execute once


if __name__ == "__main__":
    unittest.main()
