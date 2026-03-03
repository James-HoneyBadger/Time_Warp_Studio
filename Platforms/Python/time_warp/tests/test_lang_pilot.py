"""Comprehensive tests for the PILOT language executor."""

import pytest

from time_warp.core.interpreter import Language

from .conftest_lang import run, ok, has, no_errors, first_error

L = Language.PILOT


# ── helpers ────────────────────────────────────────────────────────────────


def pil(source: str, **kw) -> list[str]:
    """Shortcut: run a PILOT program."""
    return run(source, L, **kw)


# ============================================================================
# T: (Type / Print)
# ============================================================================


class TestType:
    """T: outputs text."""

    def test_simple_text(self):
        out = pil("T:Hello World\nE:")
        assert has(out, "Hello World")

    def test_multiple_lines(self):
        out = pil("T:Line one\nT:Line two\nE:")
        assert has(out, "Line one", "Line two")

    def test_empty_type(self):
        out = pil("T:\nE:")
        assert no_errors(out)

    def test_variable_interpolation(self):
        out = pil("C:X = 5\nT:Value is $X\nE:")
        # Pilot interpolates $X within T: text
        assert has(out, "5")


# ============================================================================
# A: (Accept / Input)
# ============================================================================


class TestAccept:
    """A: reads user input (handled by run() helper)."""

    def test_accept_stores_input(self):
        out = pil("A:\nT:You said something\nE:", input_val="yes")
        assert has(out, "You said something")

    def test_accept_with_variable(self):
        out = pil("A:$NAME\nT:Hello\nE:", input_val="Alice")
        assert has(out, "Hello")


# ============================================================================
# M: (Match)
# ============================================================================


class TestMatch:
    """M: matches against last input."""

    def test_match_success(self):
        out = pil("A:\nM:YES,YEAH,Y\nTY:Matched yes\nE:", input_val="yes")
        assert has(out, "Matched yes")

    def test_match_failure(self):
        out = pil("A:\nM:YES,YEAH,Y\nTN:No match\nE:", input_val="no")
        assert has(out, "No match")

    def test_match_case_insensitive(self):
        out = pil("A:\nM:HELLO\nTY:matched\nE:", input_val="hello")
        assert has(out, "matched")

    def test_match_multiple_alternatives(self):
        out = pil("A:\nM:CAT,DOG,BIRD\nTY:animal\nE:", input_val="dog")
        assert has(out, "animal")


# ============================================================================
# TY: / TN: (Conditional type)
# ============================================================================


class TestConditionalType:
    """TY: and TN: – execute only if last match was Yes/No."""

    def test_ty_after_match(self):
        out = pil("A:\nM:YES\nTY:Correct\nE:", input_val="YES")
        assert has(out, "Correct")

    def test_tn_after_no_match(self):
        out = pil("A:\nM:YES\nTN:Wrong\nE:", input_val="NO")
        assert has(out, "Wrong")

    def test_ty_skipped_when_no_match(self):
        out = pil("A:\nM:YES\nTY:Correct\nT:Done\nE:", input_val="NO")
        assert not has(out, "Correct")
        assert has(out, "Done")

    def test_tn_skipped_when_matched(self):
        out = pil("A:\nM:YES\nTN:Wrong\nT:Done\nE:", input_val="YES")
        assert not has(out, "Wrong")
        assert has(out, "Done")


# ============================================================================
# C: (Compute)
# ============================================================================


class TestCompute:
    """C: sets numeric variables."""

    def test_compute_simple(self):
        out = pil("C:X = 5\nT:$X\nE:")
        assert has(out, "5")

    def test_compute_expression(self):
        out = pil("C:X = 3 + 4\nT:$X\nE:")
        assert has(out, "7")

    def test_compute_with_variables(self):
        out = pil("C:A = 10\nC:B = $A + 5\nT:$B\nE:")
        # Should output 15
        assert has(out, "15")

    def test_compute_missing_equals(self):
        out = pil("C:X 5\nE:")
        assert first_error(out) is not None


# ============================================================================
# J: (Jump)
# ============================================================================


class TestJump:
    """J: unconditional jump to label."""

    def test_jump_to_label(self):
        src = (
            "T:Start\n"
            "J:*SKIP\n"
            "T:Skipped\n"
            "*SKIP\n"
            "T:End\n"
            "E:"
        )
        out = pil(src)
        assert has(out, "Start", "End")
        assert not has(out, "Skipped")


# ============================================================================
# U: (Use subroutine / display variable)
# ============================================================================


class TestUse:
    """U: calls subroutine or displays a variable value."""

    def test_use_display_variable(self):
        out = pil("C:COUNT = 42\nU:COUNT\nE:")
        assert has(out, "42")


# ============================================================================
# E: (End)
# ============================================================================


class TestEnd:
    """E: ends the program."""

    def test_end_stops(self):
        out = pil("T:Before\nE:\nT:After")
        assert has(out, "Before")
        assert not has(out, "After")


# ============================================================================
# R: (Return from subroutine)
# ============================================================================


class TestReturn:
    """S: and R: for subroutine calls and returns."""

    def test_subroutine_call_and_return(self):
        src = (
            "T:Main\n"
            "S:*SUB1\n"
            "T:Back\n"
            "E:\n"
            "*SUB1\n"
            "T:InSub\n"
            "R:"
        )
        out = pil(src)
        assert has(out, "Main", "InSub", "Back")


# ============================================================================
# B: (Branch)
# ============================================================================


class TestBranch:
    """B: conditional branch (condition=label)."""

    def test_branch_true(self):
        src = (
            "C:#X = 1\n"
            "B:#X=*DEST\n"
            "T:Skipped\n"
            "*DEST\n"
            "T:Branched\n"
            "E:"
        )
        out = pil(src)
        assert has(out, "Branched")

    def test_branch_false(self):
        src = (
            "C:#X = 0\n"
            "B:#X=*DEST\n"
            "T:Continued\n"
            "E:\n"
            "*DEST\n"
            "T:Branched\n"
            "E:"
        )
        out = pil(src)
        assert has(out, "Continued")


# ============================================================================
# D: (Delay)
# ============================================================================


class TestDelay:
    """D: pauses execution (capped at 10s)."""

    def test_delay_small(self):
        out = pil("D:0.01\nT:Done\nE:")
        assert has(out, "Done")

    def test_delay_invalid(self):
        out = pil("D:abc\nE:")
        assert first_error(out) is not None


# ============================================================================
# G: (Graphics)
# ============================================================================


class TestGraphics:
    """G: turtle graphics commands."""

    def test_forward(self):
        out = pil("G:FORWARD 50\nE:")
        assert no_errors(out) or len(out) == 0

    def test_back(self):
        out = pil("G:BACK 30\nE:")
        assert no_errors(out) or len(out) == 0

    def test_right(self):
        out = pil("G:RIGHT 90\nE:")
        assert no_errors(out) or len(out) == 0

    def test_left(self):
        out = pil("G:LEFT 45\nE:")
        assert no_errors(out) or len(out) == 0

    def test_penup(self):
        out = pil("G:PENUP\nE:")
        assert no_errors(out) or len(out) == 0

    def test_pendown(self):
        out = pil("G:PENDOWN\nE:")
        assert no_errors(out) or len(out) == 0

    def test_home(self):
        out = pil("G:HOME\nE:")
        assert no_errors(out) or len(out) == 0

    def test_clear(self):
        out = pil("G:CLEAR\nE:")
        assert no_errors(out) or len(out) == 0

    def test_setxy(self):
        out = pil("G:SETXY 100 100\nE:")
        assert no_errors(out) or len(out) == 0

    def test_setpencolor(self):
        out = pil("G:SETPENCOLOR 255 0 0\nE:")
        assert no_errors(out) or len(out) == 0

    def test_setpenwidth(self):
        out = pil("G:SETPENWIDTH 3\nE:")
        assert no_errors(out) or len(out) == 0

    def test_circle(self):
        out = pil("G:CIRCLE 30\nE:")
        assert no_errors(out) or len(out) == 0

    def test_setbgcolor(self):
        out = pil("G:SETBGCOLOR 0 0 128\nE:")
        assert no_errors(out) or len(out) == 0

    def test_fd_alias(self):
        out = pil("G:FD 50\nE:")
        assert no_errors(out) or len(out) == 0

    def test_bk_alias(self):
        out = pil("G:BK 50\nE:")
        assert no_errors(out) or len(out) == 0

    def test_rt_alias(self):
        out = pil("G:RT 90\nE:")
        assert no_errors(out) or len(out) == 0

    def test_lt_alias(self):
        out = pil("G:LT 45\nE:")
        assert no_errors(out) or len(out) == 0

    def test_pu_alias(self):
        out = pil("G:PU\nE:")
        assert no_errors(out) or len(out) == 0

    def test_pd_alias(self):
        out = pil("G:PD\nE:")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Long-form keywords
# ============================================================================


class TestLongForm:
    """Long-form PILOT keywords (PRINT, ACCEPT, COMPUTE, etc.)."""

    def test_print_longform(self):
        out = pil("PRINT Hello\nE:")
        assert has(out, "Hello")

    def test_type_longform(self):
        out = pil("TYPE Hello\nE:")
        assert has(out, "Hello")

    def test_compute_longform(self):
        out = pil("COMPUTE X = 42\nT:$X\nE:")
        assert has(out, "42")

    def test_stop_longform(self):
        out = pil("T:Before\nSTOP\nT:After")
        assert has(out, "Before")
        assert not has(out, "After")

    def test_end_longform(self):
        out = pil("T:First\nEND\nT:Second")
        assert has(out, "First")
        assert not has(out, "Second")


# ============================================================================
# Label definitions
# ============================================================================


class TestLabels:
    """Labels (*LABEL) and L: are no-ops."""

    def test_label_line_noop(self):
        out = pil("*MYLABEL\nT:after label\nE:")
        assert has(out, "after label")

    def test_l_command_noop(self):
        out = pil("L:\nT:after L\nE:")
        assert has(out, "after L")


# ============================================================================
# REMARK / REM
# ============================================================================


class TestRemarks:
    """Remark/comment lines."""

    def test_remark(self):
        out = pil("REMARK this is a comment\nT:output\nE:")
        assert has(out, "output")
        assert no_errors(out)

    def test_rem(self):
        out = pil("REM this is a comment\nT:output\nE:")
        assert has(out, "output")
        assert no_errors(out)


# ============================================================================
# P: (Pause)
# ============================================================================


class TestPause:
    """P: pauses for Enter (handled by run() helper)."""

    def test_pause(self):
        out = pil("T:Before pause\nP:\nT:After pause\nE:", input_val="")
        assert has(out, "Before pause", "After pause")


# ============================================================================
# Error handling
# ============================================================================


class TestErrors:
    """Error cases."""

    def test_invalid_command(self):
        out = pil("Z:something\nE:")
        assert first_error(out) is not None

    def test_empty_line(self):
        out = pil("\nT:ok\nE:")
        assert has(out, "ok")

    def test_short_line(self):
        """Lines shorter than 2 chars should be ignored."""
        out = pil("X\nT:ok\nE:")
        assert no_errors(out) or has(out, "ok")


# ============================================================================
# A: + #answer variable interpolation
# ============================================================================


class TestAnswerInterpolation:
    """A: stores input in ANSWER; T: interpolates #answer."""

    def test_hash_answer(self):
        out = pil("A:Name?\nT:Hello, #answer!", input_val="World")
        assert has(out, "Hello, World!")

    def test_dollar_answer(self):
        out = pil("A:Name?\nT:Hello, $ANSWER!", input_val="World")
        assert has(out, "Hello, World!")
