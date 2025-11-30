"""Unit tests for the interpreter debugger features.

These tests exercise breakpoints, stepping and debugger callbacks in the
interpreter.
"""

import threading
import sys
from pathlib import Path

# Add in-tree package path so pytest collects the in-repo package.
p = Path(__file__).resolve().parents[1] / "Platforms" / "Python"
sys.path.insert(0, str(p))  # pylint: disable=wrong-import-position
# The import of the in-tree package follows the sys.path insertion above.
# pylint: disable=wrong-import-position
import unittest

# Allow imports from the in-tree package after adjusting sys.path above.
# pylint: disable=wrong-import-position
from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleState  # noqa: E402


class TestDebugger(unittest.TestCase):
    """Tests verifying the interpreter debugger and related controls."""

    def test_debugger_breakpoint(self):
        """Test that the interpreter pauses at a breakpoint."""
        code = """
        10 LET A = 1
        20 LET B = 2
        30 LET C = A + B
        40 PRINT C
        """

        interp = Interpreter()
        interp.load_program(code, Language.BASIC)

        interp.set_debug_mode(True)
        interp.add_breakpoint(3)

        debug_hit = threading.Event()
        captured_vars = {}

        def debug_callback(_line, variables):
            captured_vars.update(variables)
            debug_hit.set()

        interp.set_debug_callback(debug_callback)

        # Run in a thread
        t = threading.Thread(target=interp.execute, args=(TurtleState(),))
        t.start()

        # Wait for breakpoint
        self.assertTrue(debug_hit.wait(timeout=2.0), "Did not hit breakpoint")

        # Check state
        self.assertEqual(captured_vars.get("A"), 1.0)
        self.assertEqual(captured_vars.get("B"), 2.0)
        self.assertNotIn("C", captured_vars)  # Not executed yet

        # Resume
        interp.resume_execution()

        t.join(timeout=1.0)
        self.assertFalse(t.is_alive())
        self.assertEqual(interp.variables.get("C"), 3.0)

    def test_debugger_step(self):
        """Test stepping through code."""
        code = """
        10 LET X = 10
        20 LET Y = 20
        """

        interp = Interpreter()
        interp.load_program(code, Language.BASIC)
        interp.set_debug_mode(True)

        interp.add_breakpoint(1)

        debug_hit = threading.Event()

        def debug_callback(_line, _variables):
            debug_hit.set()

        interp.set_debug_callback(debug_callback)

        t = threading.Thread(target=interp.execute, args=(TurtleState(),))
        t.start()

        # Hit line 1
        self.assertTrue(debug_hit.wait(timeout=1.0))
        debug_hit.clear()
        self.assertIsNone(interp.variables.get("X"))

        # Step
        interp.step_execution()

        # Should hit line 2
        self.assertTrue(debug_hit.wait(timeout=1.0))
        debug_hit.clear()
        self.assertEqual(interp.variables.get("X"), 10.0)
        self.assertIsNone(interp.variables.get("Y"))

        # Resume to finish
        interp.resume_execution()
        t.join(timeout=1.0)
        self.assertEqual(interp.variables.get("Y"), 20.0)


if __name__ == "__main__":
    unittest.main()
