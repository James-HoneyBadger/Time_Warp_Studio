"""Logo executor coverage tests - Part 4.

Targets untested commands: FOREACH, MAP, FILTER, REDUCE, FOREVER, BYE,
TEXTSIZE, DOT with size, pen-mode commands, 3D turtle commands, OP/REM,
WINDOW/WRAP/FENCE, STAMPCLEAN, CLEAN/CLEAR/CS, SENTENCE, and error paths.
"""
import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors

L = Language.LOGO


def logo(src, **kw):
    return run(src, L, **kw)


# ---------------------------------------------------------------------------
# Higher-order list primitives: FOREACH
# ---------------------------------------------------------------------------

class TestLogoForeach:
    """Test FOREACH [list] [template] — iterate with :? bound to each element."""

    def test_foreach_prints_items(self):
        out = logo("FOREACH [1 2 3] [PRINT :?]")
        combined = " ".join(out)
        assert "1" in combined and "2" in combined and "3" in combined

    def test_foreach_with_words(self):
        out = logo('FOREACH [a b c] [PRINT :?]')
        combined = " ".join(out)
        assert "A" in combined.upper() or "a" in combined

    def test_foreach_accumulates(self):
        out = logo('MAKE "S 0\nFOREACH [1 2 3 4] [MAKE "S :S + :?]\nPRINT :S')
        assert has(out, "10")

    def test_foreach_empty_list(self):
        out = logo('FOREACH [] [PRINT ?]')
        assert no_errors(out)  # No error, just no output

    def test_foreach_missing_template_error(self):
        out = logo('FOREACH [1 2 3]')
        assert has(out, "❌")

    def test_foreach_single_item(self):
        out = logo('FOREACH [42] [PRINT :?]')
        assert has(out, "42")


# ---------------------------------------------------------------------------
# Higher-order list primitives: MAP
# ---------------------------------------------------------------------------

class TestLogoMap:
    """Test MAP [template] list — return transformed list."""

    def test_map_double(self):
        out = logo('MAP [:? * 2] [1 2 3]')
        combined = " ".join(out)
        assert "2" in combined and "4" in combined and "6" in combined

    def test_map_add_one(self):
        out = logo('MAP [:? + 1] [10 20 30]')
        combined = " ".join(out)
        assert "11" in combined and "21" in combined and "31" in combined

    @pytest.mark.xfail(reason="MAP with single list arg silently returns [] instead of erroring")
    def test_map_missing_template_error(self):
        out = logo('MAP [1 2 3]')
        assert has(out, "❌")

    def test_map_with_list_arg(self):
        out = logo('MAP [:? + 0] [5 10 15]')
        combined = " ".join(out)
        assert "5" in combined


# ---------------------------------------------------------------------------
# Higher-order list primitives: FILTER
# ---------------------------------------------------------------------------

class TestLogoFilter:
    """Test FILTER [predicate] list — return filtered list."""

    def test_filter_greater_than(self):
        out = logo('FILTER [:? > 3] [1 2 3 4 5]')
        combined = " ".join(out)
        assert "4" in combined and "5" in combined

    def test_filter_all_pass(self):
        out = logo('FILTER [:? > 0] [1 2 3]')
        combined = " ".join(out)
        assert "1" in combined and "2" in combined and "3" in combined

    def test_filter_none_pass(self):
        out = logo('FILTER [:? > 100] [1 2 3]')
        assert no_errors(out)  # Empty result - no errors

    @pytest.mark.xfail(reason="FILTER with single list arg silently returns [] instead of erroring")
    def test_filter_missing_template_error(self):
        out = logo('FILTER [1 2 3]')
        assert has(out, "❌")


# ---------------------------------------------------------------------------
# Higher-order list primitives: REDUCE
# ---------------------------------------------------------------------------

class TestLogoReduce:
    """Test REDUCE [template] list — fold list."""

    def test_reduce_sum(self):
        out = logo('PRINT REDUCE [:?1 + :?2] [1 2 3 4 5]')
        combined = " ".join(out)
        assert "15" in combined

    def test_reduce_product(self):
        out = logo('PRINT REDUCE [:?1 * :?2] [1 2 3 4]')
        combined = " ".join(out)
        assert "24" in combined

    def test_reduce_single_item(self):
        out = logo('PRINT REDUCE [:?1 + :?2] [42]')
        combined = " ".join(out)
        assert "42" in combined

    def test_reduce_missing_template_error(self):
        out = logo('REDUCE [1 2 3]')
        assert has(out, "❌")

    def test_reduce_empty_list_error(self):
        out = logo('REDUCE [?1 + ?2] []')
        assert has(out, "❌")  # REDUCE requires a non-empty list


# ---------------------------------------------------------------------------
# FOREVER loop
# ---------------------------------------------------------------------------

class TestLogoForever:
    """Test FOREVER [commands] — runs until BYE or iteration limit."""

    def test_forever_runs_body(self):
        # BYE inside stops after first iteration
        out = logo('MAKE "I 0\nFOREVER [MAKE "I :I + 1\nBYE]\nPRINT :I')
        assert has(out, "1") or has(out, "Goodbye")

    def test_forever_bye_stops(self):
        out = logo('FOREVER [BYE]')
        assert has(out, "Goodbye") or has(out, "goodbye")

    def test_forever_missing_bracket_error(self):
        out = logo('FOREVER')
        assert has(out, "❌")

    def test_forever_malformed_bracket_error(self):
        out = logo('FOREVER [unclosed')
        assert has(out, "❌")


# ---------------------------------------------------------------------------
# BYE command
# ---------------------------------------------------------------------------

class TestLogoBye:
    """Test BYE — stops Logo execution."""

    def test_bye_produces_goodbye(self):
        out = logo('BYE')
        assert has(out, "Goodbye") or has(out, "goodbye")

    def test_bye_stops_further_execution(self):
        out = logo('BYE\nPRINT "after')
        # AFTER should not appear because BYE stopped execution
        assert not has(out, "AFTER") and not has(out, "after")


# ---------------------------------------------------------------------------
# OP command (alias for OUTPUT)
# ---------------------------------------------------------------------------

class TestLogoOp:
    """Test OP — short alias for OUTPUT."""

    def test_op_returns_value(self):
        out = logo('TO DOUBLE :X\nOP :X + :X\nEND\nDOUBLE 5')
        # OP should work like OUTPUT; procedure call should succeed
        assert no_errors(out)

    def test_op_without_args(self):
        out = logo('OP')
        assert no_errors(out)  # OP with no args is a no-op outside a procedure


# ---------------------------------------------------------------------------
# REM command (comment)
# ---------------------------------------------------------------------------

class TestLogoRem:
    """Test REM — BASIC-style comment line."""

    def test_rem_is_ignored(self):
        out = logo('REM this is a comment\nPRINT "hello')
        assert has(out, "HELLO") or has(out, "hello")

    def test_rem_at_start(self):
        out = logo('REM just a comment')
        assert no_errors(out)

    def test_rem_with_content(self):
        out = logo('MAKE "X 5\nREM set X to 5\nPRINT :X')
        assert has(out, "5")


# ---------------------------------------------------------------------------
# TEXTSIZE command
# ---------------------------------------------------------------------------

class TestLogoTextsize:
    """Test TEXTSIZE / SETLABELHEIGHT — set font size for LABEL."""

    def test_textsize_basic(self):
        out = logo('TEXTSIZE 14')
        assert no_errors(out)

    def test_setlabelheight_alias(self):
        out = logo('SETLABELHEIGHT 18')
        assert no_errors(out)

    def test_textsize_too_small_error(self):
        out = logo('TEXTSIZE 0')
        assert has(out, "❌")

    def test_textsize_too_large_error(self):
        out = logo('TEXTSIZE 201')
        assert has(out, "❌")

    def test_textsize_then_label(self):
        out = logo('TEXTSIZE 20\nLABEL "Hello')
        assert no_errors(out)

    def test_textsize_no_args_error(self):
        out = logo('TEXTSIZE')
        assert has(out, "❌")

    def test_textsize_invalid_error(self):
        out = logo('TEXTSIZE abc')
        assert has(out, "❌")


# ---------------------------------------------------------------------------
# DOT command with explicit size
# ---------------------------------------------------------------------------

class TestLogoDotWithSize:
    """Test DOT with optional size argument."""

    def test_dot_with_size(self):
        out = logo('DOT 10')
        assert no_errors(out)

    def test_dot_large_size(self):
        out = logo('DOT 50')
        assert no_errors(out)


# ---------------------------------------------------------------------------
# STAMPCLEAN command
# ---------------------------------------------------------------------------

class TestLogoStampclean:
    """Test STAMPCLEAN — stamp turtle shape and clear."""

    def test_stampclean(self):
        out = logo('STAMPCLEAN')
        assert no_errors(out)


# ---------------------------------------------------------------------------
# CLEAN / CLEAR / CS (CLEARSCREEN)
# ---------------------------------------------------------------------------

class TestLogoClearCommands:
    """Test CLEAN, CLEAR, CS — clear canvas variations."""

    def test_clean_command(self):
        out = logo('FORWARD 50\nCLEAN')
        assert no_errors(out)

    def test_clear_command(self):
        out = logo('FORWARD 50\nCLEAR')
        assert no_errors(out)

    def test_cs_clearscreen(self):
        out = logo('FORWARD 100\nCS')
        assert no_errors(out)

    def test_clearscreen_alias(self):
        out = logo('FORWARD 100\nCLEARSCREEN')
        assert no_errors(out)


# ---------------------------------------------------------------------------
# Pen mode commands
# ---------------------------------------------------------------------------

class TestLogoPenModes:
    """Test PENPAINT, PENERASE, PENREVERSE, PENMODE."""

    def test_penpaint(self):
        out = logo('PENPAINT')
        assert no_errors(out)

    def test_ppt_alias(self):
        out = logo('PPT')
        assert no_errors(out)

    def test_penerase(self):
        out = logo('PENERASE')
        assert no_errors(out)

    def test_pe_alias(self):
        out = logo('PE')
        assert no_errors(out)

    def test_penreverse(self):
        out = logo('PENREVERSE')
        assert no_errors(out)

    def test_px_alias(self):
        out = logo('PX')
        assert no_errors(out)

    def test_penmode_default(self):
        out = logo('PENMODE')
        assert no_errors(out)

    def test_penmode_after_penerase(self):
        out = logo('PENERASE\nPENMODE')
        assert has(out, "erase") or has(out, "ERASE")

    def test_penmode_after_penreverse(self):
        out = logo('PENREVERSE\nPENMODE')
        assert has(out, "reverse") or has(out, "REVERSE")


# ---------------------------------------------------------------------------
# Screen boundary mode commands
# ---------------------------------------------------------------------------

class TestLogoScreenBoundaryModes:
    """Test WINDOW, WRAP, FENCE — turtle boundary handling."""

    def test_window_mode(self):
        out = logo('WINDOW')
        assert has(out, "WINDOW")

    def test_wrap_mode(self):
        out = logo('WRAP')
        assert has(out, "WRAP")

    def test_fence_mode(self):
        out = logo('FENCE')
        assert has(out, "FENCE")


# ---------------------------------------------------------------------------
# 3D turtle commands
# ---------------------------------------------------------------------------

class TestLogo3dTurtle:
    """Test ENABLE3D, DISABLE3D, PITCH, ROLL, YAW, TILTUP, TILTDOWN."""

    def test_enable3d(self):
        out = logo('ENABLE3D')
        assert no_errors(out)

    def test_3don_alias(self):
        out = logo('3DON')
        assert no_errors(out)

    def test_disable3d(self):
        out = logo('ENABLE3D\nDISABLE3D')
        assert no_errors(out)

    def test_3doff_alias(self):
        out = logo('3DON\n3DOFF')
        assert no_errors(out)

    def test_2don_alias(self):
        out = logo('3DON\n2DON')
        assert no_errors(out)

    def test_pitch_command(self):
        out = logo('PITCH 45')
        assert no_errors(out)

    def test_pitch_negative(self):
        out = logo('PITCH -30')
        assert no_errors(out)

    def test_roll_command(self):
        out = logo('ROLL 90')
        assert no_errors(out)

    def test_tiltroll_alias(self):
        out = logo('TILTROLL 45')
        assert no_errors(out)

    def test_yaw_command(self):
        out = logo('YAW 90')
        assert no_errors(out)

    def test_tiltup_command(self):
        out = logo('TILTUP')
        assert no_errors(out)

    def test_tiltup_with_angle(self):
        out = logo('TILTUP 30')
        assert no_errors(out)

    def test_upward_alias(self):
        out = logo('UPWARD')
        assert no_errors(out)

    def test_up_alias(self):
        out = logo('UP 20')
        assert no_errors(out)

    def test_tiltdown_command(self):
        out = logo('TILTDOWN')
        assert no_errors(out)

    def test_tiltdown_with_angle(self):
        out = logo('TILTDOWN 30')
        assert no_errors(out)

    def test_downward_alias(self):
        out = logo('DOWNWARD')
        assert no_errors(out)

    def test_down_alias(self):
        out = logo('DOWN 20')
        assert no_errors(out)

    def test_pitch_no_arg_error(self):
        out = logo('PITCH')
        assert has(out, "❌")

    def test_roll_no_arg_error(self):
        out = logo('ROLL')
        assert has(out, "❌")

    def test_yaw_no_arg_error(self):
        out = logo('YAW')
        assert has(out, "❌")

    def test_3d_full_movement(self):
        out = logo('ENABLE3D\nFORWARD 50\nPITCH 45\nFORWARD 30\nROLL 90\nFORWARD 20')
        assert no_errors(out)


# ---------------------------------------------------------------------------
# SENTENCE command (additional coverage)
# ---------------------------------------------------------------------------

class TestLogoSentence:
    """Test SENTENCE — merge lists or items into a flat list."""

    def test_sentence_two_lists(self):
        out = logo('PRINT SENTENCE [1 2] [3 4]')
        combined = " ".join(out)
        assert "1" in combined and "3" in combined

    def test_sentence_words(self):
        out = logo('PRINT SENTENCE "hello "world')
        assert no_errors(out)

    def test_sentence_mixed(self):
        out = logo('PRINT SENTENCE [1 2] "three')
        assert no_errors(out)


# ---------------------------------------------------------------------------
# Higher-order error paths
# ---------------------------------------------------------------------------

class TestLogoHigherOrderErrors:
    """Test error handling in MAP, FILTER, REDUCE."""

    @pytest.mark.xfail(reason="MAP with single list arg silently returns [] instead of erroring")
    def test_map_no_list_error(self):
        out = logo('MAP [? + 1]')
        assert has(out, "❌")

    @pytest.mark.xfail(reason="FILTER with single list arg silently returns [] instead of erroring")
    def test_filter_no_list_error(self):
        out = logo('FILTER [? > 0]')
        assert has(out, "❌")

    def test_reduce_empty_error(self):
        out = logo('REDUCE [?1 + ?2]')
        assert has(out, "❌")


# ---------------------------------------------------------------------------
# Semicolon comment stripping
# ---------------------------------------------------------------------------

class TestLogoSemicolonComment:
    """Test that ; comments are stripped."""

    def test_semicolon_inline_comment(self):
        out = logo('PRINT "hello ; this is a comment')
        assert has(out, "HELLO") or has(out, "hello")

    def test_full_comment_line(self):
        out = logo('; full comment line\nPRINT "ok')
        assert has(out, "OK") or has(out, "ok")


# ---------------------------------------------------------------------------
# No-turtle error paths
# ---------------------------------------------------------------------------

class TestLogoNoTurtleErrors:
    """Test commands that require turtle but run without one."""

    def test_pitch_no_turtle(self):
        # Run PITCH without graphics (no turtle available)
        # When run via run() helper with turtle=None
        from time_warp.core.interpreter import Interpreter
        interp = Interpreter()
        from time_warp.languages.logo import execute_logo
        result = execute_logo(interp, "PITCH 45", None)
        assert "❌" in result

    def test_roll_no_turtle(self):
        from time_warp.core.interpreter import Interpreter
        from time_warp.languages.logo import execute_logo
        interp = Interpreter()
        result = execute_logo(interp, "ROLL 30", None)
        assert "❌" in result

    def test_yaw_no_turtle(self):
        from time_warp.core.interpreter import Interpreter
        from time_warp.languages.logo import execute_logo
        interp = Interpreter()
        result = execute_logo(interp, "YAW 90", None)
        assert "❌" in result

    def test_tiltup_no_turtle(self):
        from time_warp.core.interpreter import Interpreter
        from time_warp.languages.logo import execute_logo
        interp = Interpreter()
        result = execute_logo(interp, "TILTUP 45", None)
        assert "❌" in result

    def test_tiltdown_no_turtle(self):
        from time_warp.core.interpreter import Interpreter
        from time_warp.languages.logo import execute_logo
        interp = Interpreter()
        result = execute_logo(interp, "TILTDOWN 45", None)
        assert "❌" in result
