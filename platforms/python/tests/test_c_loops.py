import sys
import os
import unittest

# Ensure Python platform is importable
sys.path.append(os.path.join(os.getcwd(), "platforms/python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


class TestCLoops(unittest.TestCase):
    def test_simple_assignment_updates_variable(self):
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
