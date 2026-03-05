"""Tests for Logo command execution behavior."""

from time_warp.core.interpreter import Interpreter  # type: ignore[import-not-found]
from time_warp.graphics.turtle_state import TurtleState  # type: ignore[import-not-found]
from time_warp.languages.logo import execute_logo  # type: ignore[import-not-found]


class TestLogoExecutor:

    def test_repeat_allows_repcounter_alias_in_expressions(self):
        interpreter = Interpreter()
        turtle = TurtleState()

        output = execute_logo(
            interpreter,
            "REPEAT 5 [FORWARD 2 + REPCOUNTER * 0.5]",
            turtle,
        )

        assert "Undefined variable" not in output
        assert len(turtle.lines) == 5
        assert interpreter.variables["REPCOUNT"] == 5.0
        assert interpreter.variables["REPCOUNTER"] == 5.0

    def test_repeat_allows_repcount_in_expressions(self):
        interpreter = Interpreter()
        turtle = TurtleState()

        output = execute_logo(
            interpreter,
            "REPEAT 4 [FORWARD 3 + REPCOUNT * 0.25]",
            turtle,
        )

        assert "Undefined variable" not in output
        assert len(turtle.lines) == 4
        assert interpreter.variables["REPCOUNT"] == 4.0
        assert interpreter.variables["REPCOUNTER"] == 4.0

    def test_execute_logo_splits_multiple_commands(self):
        interpreter = Interpreter()
        turtle = TurtleState()

        output = execute_logo(interpreter, 'MAKE "A 10 PRINT :A', turtle)

        assert output == "10.0\n"
        assert interpreter.variables["A"] == 10.0
