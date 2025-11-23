import sys
import os
import unittest

# Ensure Python platform is importable
sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


class TestPascalRepeatUntil(unittest.TestCase):
    def test_repeat_until_terminates(self):
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
