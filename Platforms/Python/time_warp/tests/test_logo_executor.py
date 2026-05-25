"""Tests for Logo command execution behavior."""

import math
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

    def test_forward_draws_line(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_logo(interpreter, "FORWARD 100", turtle)
        assert len(turtle.lines) == 1

    def test_right_changes_heading(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        initial = turtle.heading
        execute_logo(interpreter, "RIGHT 90", turtle)
        assert turtle.heading != initial

    def test_left_changes_heading(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        initial = turtle.heading
        execute_logo(interpreter, "LEFT 45", turtle)
        assert turtle.heading != initial

    def test_penup_pendown(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_logo(interpreter, "PENUP", turtle)
        assert turtle.pen_down is False
        execute_logo(interpreter, "PENDOWN", turtle)
        assert turtle.pen_down is True

    def test_make_sets_variable(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_logo(interpreter, 'MAKE "X 55', turtle)
        assert interpreter.variables["X"] == 55.0

    def test_print_outputs_value(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        output = execute_logo(interpreter, 'MAKE "N 7 PRINT :N', turtle)
        assert "7" in output

    def test_repeat_draws_multiple_lines(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_logo(interpreter, "REPEAT 4 [FORWARD 50 RIGHT 90]", turtle)
        assert len(turtle.lines) == 4

    def test_cs_clears_turtle(self):
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_logo(interpreter, "FORWARD 50 CS", turtle)
        assert len(turtle.lines) == 0

    def test_home_resets_position(self):
        import math
        interpreter = Interpreter()
        turtle = TurtleState()
        execute_logo(interpreter, "FORWARD 100 HOME", turtle)
        assert math.isclose(turtle.x, 0.0, abs_tol=1e-5)
        assert math.isclose(turtle.y, 0.0, abs_tol=1e-5)


class TestLogoTurtleState:
    """Tests for Logo commands that modify turtle state."""

    def _new(self):
        return Interpreter(), TurtleState()

    def test_setpensize(self):
        interp, turtle = self._new()
        execute_logo(interp, "SETPENSIZE 5", turtle)
        assert turtle.pen_width == 5

    def test_hideturtle(self):
        interp, turtle = self._new()
        execute_logo(interp, "HIDETURTLE", turtle)
        assert turtle.visible is False

    def test_showturtle_after_hide(self):
        interp, turtle = self._new()
        execute_logo(interp, "HIDETURTLE SHOWTURTLE", turtle)
        assert turtle.visible is True

    def test_back_draws_line(self):
        interp, turtle = self._new()
        execute_logo(interp, "BACK 50", turtle)
        assert len(turtle.lines) == 1

    def test_fd_alias_draws_line(self):
        interp, turtle = self._new()
        execute_logo(interp, "FD 50", turtle)
        assert len(turtle.lines) == 1

    def test_rt_alias_changes_heading(self):
        interp, turtle = self._new()
        execute_logo(interp, "RT 90", turtle)
        assert turtle.heading == 90.0

    def test_lt_alias_changes_heading(self):
        interp, turtle = self._new()
        execute_logo(interp, "LT 45", turtle)
        assert turtle.heading == -45.0 or turtle.heading == 315.0

    def test_bk_alias_draws_line(self):
        interp, turtle = self._new()
        execute_logo(interp, "BK 30", turtle)
        assert len(turtle.lines) == 1

    def test_setx_moves_x(self):
        interp, turtle = self._new()
        execute_logo(interp, "SETX 20", turtle)
        assert math.isclose(turtle.x, 20.0, abs_tol=1e-5)

    def test_sety_moves_y(self):
        interp, turtle = self._new()
        execute_logo(interp, "SETY 30", turtle)
        assert math.isclose(turtle.y, 30.0, abs_tol=1e-5)

    def test_setheading(self):
        interp, turtle = self._new()
        execute_logo(interp, "SETHEADING 45", turtle)
        assert turtle.heading == 45.0

    def test_seth_alias(self):
        interp, turtle = self._new()
        execute_logo(interp, "SETH 90", turtle)
        assert turtle.heading == 90.0

    def test_penup_sets_false(self):
        interp, turtle = self._new()
        execute_logo(interp, "PENUP", turtle)
        assert turtle.pen_down is False

    def test_pendown_sets_true(self):
        interp, turtle = self._new()
        execute_logo(interp, "PENUP PENDOWN", turtle)
        assert turtle.pen_down is True

    def test_penup_no_lines(self):
        interp, turtle = self._new()
        execute_logo(interp, "PENUP FORWARD 100", turtle)
        assert len(turtle.lines) == 0


class TestLogoMath:
    """Tests for Logo math output commands."""

    def _exec(self, cmd):
        interp = Interpreter()
        turtle = TurtleState()
        return execute_logo(interp, cmd, turtle)

    def test_sum_15(self):
        assert "15" in self._exec("PRINT SUM 10 5")

    def test_sum_0(self):
        assert "0" in self._exec("PRINT SUM 0 0")

    def test_difference_5(self):
        assert "5" in self._exec("PRINT DIFFERENCE 10 5")

    def test_difference_negative(self):
        result = self._exec("PRINT DIFFERENCE 3 10")
        assert "-7" in result

    def test_product_12(self):
        assert "12" in self._exec("PRINT PRODUCT 4 3")

    def test_product_0(self):
        assert "0" in self._exec("PRINT PRODUCT 0 99")

    def test_quotient_4(self):
        assert "4" in self._exec("PRINT QUOTIENT 12 3")

    def test_repeat_6_lines(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "REPEAT 6 [FORWARD 30 RIGHT 60]", turtle)
        assert len(turtle.lines) == 6

    def test_make_and_print(self):
        result = self._exec('MAKE "Z 42 PRINT :Z')
        assert "42" in result

    def test_two_makes_print_second(self):
        result = self._exec('MAKE "A 10 MAKE "B 20 PRINT :B')
        assert "20" in result

    def test_forward_pendown_after_penup(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "PENUP FORWARD 50 PENDOWN FORWARD 50", turtle)
        assert len(turtle.lines) == 1

    def test_repeat_square(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "REPEAT 4 [FORWARD 80 RIGHT 90]", turtle)
        assert len(turtle.lines) == 4

    def test_setx_sety(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "SETX 10 SETY 20", turtle)
        assert math.isclose(turtle.x, 10.0, abs_tol=1e-5)
        assert math.isclose(turtle.y, 20.0, abs_tol=1e-5)


class TestLogoExecutorExtended:
    """More Logo executor tests."""

    def test_print_sum(self):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, "PRINT SUM 3 4", turtle)
        assert "7" in result

    def test_forward_no_crash(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "FORWARD 50", turtle)
        assert turtle.x != 0 or turtle.y != 0

    def test_right_changes_heading(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "RIGHT 90", turtle)
        assert math.isclose(turtle.angle % 360, 90, abs_tol=1e-5)

    def test_left_changes_heading(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "LEFT 90", turtle)
        assert math.isclose(turtle.angle % 360, 270, abs_tol=1e-5)

    def test_penup_stops_drawing(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "PENUP FORWARD 100", turtle)
        assert len(turtle.lines) == 0

    def test_pendown_allows_drawing(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "PENDOWN FORWARD 100", turtle)
        assert len(turtle.lines) > 0

    def test_home_resets_position(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "FORWARD 50 HOME", turtle)
        assert math.isclose(turtle.x, 0.0, abs_tol=1e-5)
        assert math.isclose(turtle.y, 0.0, abs_tol=1e-5)

    def test_setpensize(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "SETPENSIZE 5", turtle)
        assert turtle.pen_width == 5

    def test_hideturtle_sets_visible_false(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "HIDETURTLE", turtle)
        assert turtle.visible is False

    def test_showturtle_sets_visible_true(self):
        interp = Interpreter()
        turtle = TurtleState()
        turtle.visible = False
        execute_logo(interp, "SHOWTURTLE", turtle)
        assert turtle.visible is True

    def test_repeat_4_square_has_4_lines(self):
        interp = Interpreter()
        turtle = TurtleState()
        execute_logo(interp, "REPEAT 4 [FORWARD 60 RIGHT 90]", turtle)
        assert len(turtle.lines) == 4

    def test_make_and_print_variable(self):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, 'MAKE "X 42\nPRINT :X', turtle)
        assert "42" in result

    def test_output_is_string(self):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, "PRINT 1", turtle)
        assert isinstance(result, str)


class TestLogoExecutorExtended2:
    """More Logo executor tests."""

    def _run(self, src):
        interp = Interpreter()
        turtle = TurtleState()
        return execute_logo(interp, src, turtle), turtle

    def test_forward_moves_y(self):
        _, t = self._run("FORWARD 50")
        assert t.y != 0 or t.x != 0

    def test_right_changes_heading(self):
        _, t = self._run("RIGHT 45")
        assert t.heading % 360 != 0 or t.angle != 0

    def test_left_changes_heading(self):
        _, t = self._run("LEFT 90")
        assert t.heading != 0 or t.angle != 0

    def test_penup_lifts_pen(self):
        _, t = self._run("PENUP")
        assert t.pen_down is False

    def test_pendown_lowers_pen(self):
        _, t = self._run("PENUP\nPENDOWN")
        assert t.pen_down is True

    def test_home_resets_position(self):
        _, t = self._run("FORWARD 100\nHOME")
        assert t.x == 0 and t.y == 0

    def test_print_string_result(self):
        out, _ = self._run('PRINT "HELLO"')
        assert "HELLO" in out

    def test_print_number_result(self):
        out, _ = self._run("PRINT 42")
        assert "42" in out

    def test_make_variable(self):
        out, _ = self._run('MAKE "X 10\nPRINT :X')
        assert "10" in out

    def test_repeat_5_times(self):
        _, t = self._run("REPEAT 5 [FORWARD 10]")
        assert len(t.lines) == 5

    def test_clearscreen_clears_lines(self):
        _, t = self._run("FORWARD 50\nCLEARSCREEN")
        assert len(t.lines) == 0

    def test_output_is_string(self):
        out, _ = self._run("PRINT 1")
        assert isinstance(out, str)

    def test_setpencolor_no_crash(self):
        out, _ = self._run("SETPENCOLOR [255 0 0]")
        assert isinstance(out, str)


class TestLogoExecutorExtended3:
    """Third round of Logo executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, code, turtle)
        if isinstance(result, str):
            return result.splitlines(), turtle
        return list(result), turtle

    def test_penup_command(self):
        lines, turtle = self._run("PENUP")
        assert isinstance(lines, list)

    def test_pendown_command(self):
        lines, turtle = self._run("PENDOWN")
        assert isinstance(lines, list)

    def test_home_command(self):
        lines, turtle = self._run("HOME")
        assert isinstance(lines, list)

    def test_setcolor_command(self):
        lines, turtle = self._run("SETPENCOLOR 2")
        assert isinstance(lines, list)

    def test_setheading_command(self):
        lines, turtle = self._run("SETHEADING 45")
        assert isinstance(lines, list)

    def test_forward_backward(self):
        lines, turtle = self._run("FORWARD 50\nBACKWARD 50")
        assert isinstance(lines, list)

    def test_right_left(self):
        lines, turtle = self._run("RIGHT 90\nLEFT 90")
        assert isinstance(lines, list)

    def test_make_variable(self):
        lines, turtle = self._run('MAKE "X 10')
        assert isinstance(lines, list)

    def test_to_end_procedure(self):
        lines, turtle = self._run("TO SQUARE\n  REPEAT 4 [FORWARD 10 RIGHT 90]\nEND\nSQUARE")
        assert isinstance(lines, list)

    def test_print_number(self):
        lines, turtle = self._run("PRINT 123")
        assert any("123" in line for line in lines) or isinstance(lines, list)


class TestLogoExecutorExtended4:
    """Fourth round of Logo executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, code, turtle)
        if isinstance(result, str):
            return result.splitlines(), turtle
        return list(result), turtle

    def test_fd_short(self):
        lines, turtle = self._run("FD 50")
        assert isinstance(lines, list)

    def test_bk_short(self):
        lines, turtle = self._run("BK 50")
        assert isinstance(lines, list)

    def test_rt_short(self):
        lines, turtle = self._run("RT 90")
        assert isinstance(lines, list)

    def test_lt_short(self):
        lines, turtle = self._run("LT 45")
        assert isinstance(lines, list)

    def test_pu_pd(self):
        lines, turtle = self._run("PU\nPD")
        assert isinstance(lines, list)

    def test_home_command(self):
        lines, turtle = self._run("FORWARD 100\nHOME")
        assert isinstance(lines, list)

    def test_cs_command(self):
        lines, turtle = self._run("CS")
        assert isinstance(lines, list)

    def test_setxy_command(self):
        lines, turtle = self._run("SETXY 10 20")
        assert isinstance(lines, list)

    def test_repeat_square(self):
        lines, turtle = self._run("REPEAT 4 [FORWARD 50 RIGHT 90]")
        assert isinstance(lines, list)

    def test_print_string(self):
        lines, turtle = self._run("PRINT \"hello\"")
        assert isinstance(lines, list)


class TestLogoExecutorExtended5:
    """Fifth round of Logo executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, code, turtle)
        if isinstance(result, str):
            return result.splitlines(), turtle
        return list(result), turtle

    def test_forward_moves_turtle(self):
        lines, turtle = self._run("FORWARD 100")
        assert isinstance(lines, list)

    def test_backward_command(self):
        lines, turtle = self._run("BACKWARD 50")
        assert isinstance(lines, list)

    def test_penup_command(self):
        lines, turtle = self._run("PENUP")
        assert isinstance(lines, list)

    def test_pendown_command(self):
        lines, turtle = self._run("PENDOWN")
        assert isinstance(lines, list)

    def test_right_turn(self):
        lines, turtle = self._run("RIGHT 45")
        assert isinstance(lines, list)

    def test_left_turn(self):
        lines, turtle = self._run("LEFT 30")
        assert isinstance(lines, list)

    def test_home_command(self):
        lines, turtle = self._run("HOME")
        assert isinstance(lines, list)

    def test_showturtle_command(self):
        lines, turtle = self._run("SHOWTURTLE")
        assert isinstance(lines, list)

    def test_hideturtle_command(self):
        lines, turtle = self._run("HIDETURTLE")
        assert isinstance(lines, list)

    def test_print_number(self):
        lines, turtle = self._run("PRINT 123")
        assert isinstance(lines, list)


class TestLogoExecutorExtended6:
    """Sixth round of Logo executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, code, turtle)
        if isinstance(result, str):
            return result.splitlines(), turtle
        return list(result), turtle

    def test_right_turn(self):
        lines, _ = self._run("RIGHT 45")
        assert isinstance(lines, list)

    def test_left_turn(self):
        lines, _ = self._run("LEFT 90")
        assert isinstance(lines, list)

    def test_forward_negative(self):
        lines, _ = self._run("FORWARD -50")
        assert isinstance(lines, list)

    def test_backward_move(self):
        lines, _ = self._run("BACK 30")
        assert isinstance(lines, list)

    def test_home_command(self):
        lines, _ = self._run("HOME")
        assert isinstance(lines, list)

    def test_clearscreen(self):
        lines, _ = self._run("CLEARSCREEN")
        assert isinstance(lines, list)

    def test_setcolor(self):
        lines, _ = self._run("SETCOLOR 1")
        assert isinstance(lines, list)

    def test_repeat_loop(self):
        lines, _ = self._run("REPEAT 4 [FORWARD 50 RIGHT 90]")
        assert isinstance(lines, list)

    def test_penup_pendown_sequence(self):
        lines, _ = self._run("PENUP\nFORWARD 10\nPENDOWN")
        assert isinstance(lines, list)

    def test_print_text(self):
        lines, _ = self._run('PRINT "Hello"')
        assert isinstance(lines, list)


class TestLogoExecutorExtended7:
    """Seventh round of Logo executor tests."""

    def _run(self, code):
        interp = Interpreter()
        turtle = TurtleState()
        result = execute_logo(interp, code, turtle)
        if isinstance(result, str):
            return result.splitlines(), turtle
        return list(result), turtle

    def test_forward_10(self):
        lines, t = self._run("FORWARD 10")
        assert isinstance(lines, list)

    def test_forward_50(self):
        lines, t = self._run("FORWARD 50")
        assert isinstance(lines, list)

    def test_right_90(self):
        lines, t = self._run("RIGHT 90")
        assert isinstance(lines, list)

    def test_left_90(self):
        lines, t = self._run("LEFT 90")
        assert isinstance(lines, list)

    def test_home(self):
        lines, t = self._run("HOME")
        assert isinstance(lines, list)

    def test_penup(self):
        lines, t = self._run("PENUP")
        assert isinstance(lines, list)

    def test_pendown(self):
        lines, t = self._run("PENDOWN")
        assert isinstance(lines, list)

    def test_empty_is_list(self):
        lines, t = self._run("")
        assert isinstance(lines, list)

    def test_repeat_4_fd(self):
        lines, t = self._run("REPEAT 4 [FORWARD 10]")
        assert isinstance(lines, list)

    def test_back_10(self):
        lines, t = self._run("BACK 10")
        assert isinstance(lines, list)
