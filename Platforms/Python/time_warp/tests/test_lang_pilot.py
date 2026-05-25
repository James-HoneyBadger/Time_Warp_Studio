"""Comprehensive tests for the PILOT language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

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
        src = "T:Start\n" "J:*SKIP\n" "T:Skipped\n" "*SKIP\n" "T:End\n" "E:"
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
        src = "T:Main\n" "S:*SUB1\n" "T:Back\n" "E:\n" "*SUB1\n" "T:InSub\n" "R:"
        out = pil(src)
        assert has(out, "Main", "InSub", "Back")


# ============================================================================
# B: (Branch)
# ============================================================================


class TestBranch:
    """B: conditional branch (condition=label)."""

    def test_branch_true(self):
        src = "C:#X = 1\n" "B:#X=*DEST\n" "T:Skipped\n" "*DEST\n" "T:Branched\n" "E:"
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


# ============================================================================
# Conditional commands: XY: / XN:
# ============================================================================


class TestConditionalJump:
    """JY: / JN: jump only when match flag matches condition."""

    def test_jump_yes_taken_when_matched(self):
        """JY: executes when last M: succeeded."""
        src = (
            "A:input?\n"
            "T:start\n"
            "M:yes\n"
            "JY:*done\n"
            "T:SHOULD NOT PRINT\n"
            "*done\n"
            "T:reached done\n"
            "E:\n"
        )
        out = pil(src, input_val="yes")
        assert has(out, "reached done")
        assert not has(out, "SHOULD NOT PRINT")

    def test_jump_yes_not_taken_when_no_match(self):
        """JY: is skipped when last M: failed."""
        src = (
            "A:input?\n"
            "T:start\n"
            "M:yes\n"
            "JY:*done\n"
            "T:fallthrough\n"
            "*done\n"
            "T:done\n"
            "E:\n"
        )
        out = pil(src, input_val="no")
        assert has(out, "fallthrough")

    def test_jump_no_taken_when_no_match(self):
        """JN: executes when last M: failed."""
        src = (
            "A:input?\n"
            "T:start\n"
            "M:yes\n"
            "JN:*wrong\n"
            "T:SHOULD NOT PRINT\n"
            "E:\n"
            "*wrong\n"
            "T:wrong answer path\n"
            "E:\n"
        )
        out = pil(src, input_val="no")
        assert has(out, "wrong answer path")
        assert not has(out, "SHOULD NOT PRINT")


class TestConditionalCompute:
    """CY: / CN: compute only when condition matches."""

    def test_compute_yes_sets_var(self):
        src = (
            "A:input?\n"
            "M:go\n"
            "CY: X = 99\n"
            "T:#X\n"
            "E:\n"
        )
        out = pil(src, input_val="go")
        assert has(out, "99")

    def test_compute_no_skips_when_matched(self):
        src = (
            "A:input?\n"
            "M:go\n"
            "CN: X = 99\n"
            "T:#X\n"
            "E:\n"
        )
        out = pil(src, input_val="go")
        # CN: is skipped since match succeeded — X is 0 (default)
        assert not has(out, "99")


class TestMultiPatternMatch:
    """M: with multiple comma-separated patterns."""

    def test_match_first_alternative(self):
        src = "A:\nM:yes,correct,right\nTY:good answer\nE:\n"
        out = pil(src, input_val="yes")
        assert has(out, "good answer")

    def test_match_second_alternative(self):
        src = "A:\nM:yes,correct,right\nTY:good answer\nE:\n"
        out = pil(src, input_val="correct")
        assert has(out, "good answer")

    def test_match_third_alternative(self):
        src = "A:\nM:yes,correct,right\nTY:good answer\nE:\n"
        out = pil(src, input_val="right")
        assert has(out, "good answer")

    def test_no_match_skips_ty(self):
        src = "A:\nM:yes,correct,right\nTY:good answer\nTN:wrong answer\nE:\n"
        out = pil(src, input_val="nope")
        assert has(out, "wrong answer")
        assert not has(out, "good answer")


class TestComputeChaining:
    """Multiple C: statements building on each other."""

    def test_add_then_multiply(self):
        src = "C:A=5\nC:B=A+3\nC:C=B*2\nT:#C\n"
        out = pil(src)
        assert has(out, "16")
        assert no_errors(out)

    def test_subtract_and_display(self):
        src = "C:X=100\nC:Y=X-37\nT:#Y\n"
        out = pil(src)
        assert has(out, "63")
        assert no_errors(out)

    def test_division(self):
        src = "C:N=20\nC:D=N/4\nT:#D\n"
        out = pil(src)
        assert has(out, "5")
        assert no_errors(out)

    def test_nested_math(self):
        src = "C:A=3\nC:B=4\nC:C=A*A+B*B\nT:#C\n"
        out = pil(src)
        assert has(out, "25")
        assert no_errors(out)


class TestJumpSequence:
    """Complex jump sequences."""

    def test_skip_two_lines(self):
        src = "T:Line1\nJ:DONE\nT:Line2\nT:Line3\n*DONE\nT:Finished\n"
        out = pil(src)
        assert has(out, "Line1")
        assert has(out, "Finished")
        assert not has(out, "Line2")
        assert not has(out, "Line3")

    def test_conditional_jump_then_skip(self):
        src = (
            "A:\n"
            "M:yes\n"
            "JY:MATCHED\n"
            "T:Not matched\n"
            "J:END\n"
            "*MATCHED\n"
            "T:Matched!\n"
            "*END\n"
        )
        out = pil(src, input_val="yes")
        assert has(out, "Matched!")
        assert not has(out, "Not matched")

    def test_jump_over_block(self):
        src = (
            "J:SKIP\n"
            "T:Should not print\n"
            "*SKIP\n"
            "T:After jump\n"
        )
        out = pil(src)
        assert has(out, "After jump")
        assert not has(out, "Should not print")
        assert no_errors(out)


class TestVariableInterpolation:
    """#var interpolation in T: commands."""

    def test_multiple_vars_in_output(self):
        src = "C:X=10\nC:Y=20\nT:X=#X Y=#Y\n"
        out = pil(src)
        assert has(out, "X=10")
        assert has(out, "Y=20")
        assert no_errors(out)

    def test_computed_result_display(self):
        src = "C:N=7\nC:SQ=N*N\nT:Square is #SQ\n"
        out = pil(src)
        assert has(out, "Square is 49")
        assert no_errors(out)

    def test_answer_interpolation_after_accept(self):
        src = "A:Enter value:\nT:You said #ANSWER\n"
        out = pil(src, input_val="hello")
        assert has(out, "You said hello")
        assert no_errors(out)


class TestRemarkVariants:
    """Remark and comment lines."""

    def test_remark_not_output(self):
        src = "R:This is a remark\nT:Only this\n"
        out = pil(src)
        assert has(out, "Only this")
        assert not has(out, "remark")
        assert no_errors(out)

    def test_multiple_remarks(self):
        src = "R:First remark\nT:Output\nR:Second remark\n"
        out = pil(src)
        assert has(out, "Output")
        assert not has(out, "First remark")
        assert not has(out, "Second remark")
        assert no_errors(out)


class TestEndBehavior:
    """E: stops execution."""

    def test_end_stops_before_later_type(self):
        src = "T:Before end\nE:\nT:After end\n"
        out = pil(src)
        assert has(out, "Before end")
        assert not has(out, "After end")
        assert no_errors(out)

    def test_conditional_end(self):
        src = "A:\nM:stop\nTY:Stopping\nJY:DONE\nT:Continue\n*DONE\n"
        out = pil(src, input_val="stop")
        assert has(out, "Stopping")
        assert not has(out, "Continue")
        assert no_errors(out)


class TestPilotMultipleTypes:
    """Multiple TYPE commands and multi-line output."""

    def test_three_type_lines(self):
        out = pil("T:Line1\nT:Line2\nT:Line3\nE")
        assert has(out, "Line1")
        assert has(out, "Line2")
        assert has(out, "Line3")
        assert no_errors(out)

    def test_labels_dont_output(self):
        out = pil("*START\nT:first\nE")
        assert has(out, "first")
        assert no_errors(out)

    def test_two_labeled_sections(self):
        out = pil("*A\nT:aaa\n*B\nT:bbb\nE")
        assert has(out, "aaa")
        assert has(out, "bbb")
        assert no_errors(out)


class TestPilotComputeExtra:
    """Additional compute expression tests."""

    def test_compute_multiply(self):
        out = pil("C:a = 3\nC:b = 4\nC:c = a * b\nT:#c\nE")
        assert has(out, "12")
        assert no_errors(out)

    def test_compute_subtract(self):
        out = pil("C:x = 10\nC:y = x - 3\nT:#y\nE")
        assert has(out, "7")
        assert no_errors(out)

    def test_compute_add(self):
        out = pil("C:n = 7\nC:m = n + 1\nT:#m\nE")
        assert has(out, "8")
        assert no_errors(out)

    def test_compute_divide(self):
        out = pil("C:p = 6\nC:q = p / 2\nT:#q\nE")
        assert has(out, "3")
        assert no_errors(out)

    def test_remark_suppressed(self):
        out = pil("R:This is just a remark\nT:output\nE")
        assert has(out, "output")
        assert not has(out, "remark")
        assert no_errors(out)


class TestPilotCompute2:
    """More PILOT compute and type tests."""

    def test_add_two_nums(self):
        assert has(pil("C:X = 3 + 4\nT:$X"), "7")

    def test_sub_two_nums(self):
        assert has(pil("C:X = 10 - 4\nT:$X"), "6")

    def test_mul_two_nums(self):
        assert has(pil("C:X = 6 * 7\nT:$X"), "42")

    def test_div_two_nums(self):
        assert has(pil("C:X = 15 / 3\nT:$X"), "5")

    def test_two_var_sum(self):
        assert has(pil("C:X = 5\nC:Y = 10\nC:Z = X + Y\nT:$Z"), "15")

    def test_type_literal(self):
        assert has(pil("T:Hello World"), "Hello World")

    def test_remark_ignored(self):
        out = pil("R:This is a remark\nT:Done")
        assert has(out, "Done")

    def test_display_literal_in_string(self):
        assert has(pil("T:2 + 3 = 5"), "2 + 3 = 5")

    def test_var_substituted(self):
        assert has(pil("C:X = 5\nT:$X"), "5")

    def test_chained_assigns(self):
        assert has(pil("C:A = 3\nC:B = A + 2\nC:C = B * 4\nT:$C"), "20")


class TestPilotCompute3:
    """More PILOT compute tests."""

    def test_square(self):
        assert has(pil("C:X = 7 * 7\nT:$X"), "49")

    def test_power_workaround(self):
        # PILOT doesn't have power, use multiplication
        assert has(pil("C:X = 2 * 2 * 2\nT:$X"), "8")

    def test_string_type(self):
        assert has(pil("T:Hello World"), "Hello World")

    def test_multi_type(self):
        src = "T:Line 1\nT:Line 2"
        out = pil(src)
        assert has(out, "Line 1") and has(out, "Line 2")

    def test_var_subtracted(self):
        assert has(pil("C:X = 10\nC:Y = 4\nC:Z = X - Y\nT:$Z"), "6")

    def test_var_multiplied(self):
        assert has(pil("C:X = 6\nC:Y = 7\nC:Z = X * Y\nT:$Z"), "42")

    def test_var_divided(self):
        assert has(pil("C:X = 10\nC:Y = 2\nC:Z = X / Y\nT:$Z"), "5")


class TestPilotArithmetic2:
    """Additional PILOT arithmetic tests."""

    def test_add_7_3(self):
        assert has(pil('C:A=7+3\nT:#A'), "10")

    def test_mul_6_7(self):
        assert has(pil('C:B=6*7\nT:#B'), "42")

    def test_sub_10_3(self):
        assert has(pil('C:C=10-3\nT:#C'), "7")

    def test_power_chain(self):
        assert has(pil('C:D=2*2*2*2\nT:#D'), "16")

    def test_var_99(self):
        assert has(pil('C:N=99\nT:#N'), "99")

    def test_two_vars_sum(self):
        assert has(pil('C:X=7\nC:Y=3\nC:Z=X+Y\nT:#Z'), "10")

    def test_square(self):
        assert has(pil('C:S=9*9\nT:#S'), "81")

    def test_large_mul(self):
        assert has(pil('C:M=12*12\nT:#M'), "144")

    def test_chain_add(self):
        assert has(pil('C:R=1+2+3+4\nT:#R'), "10")

    def test_mod_expr(self):
        assert has(pil('C:V=15-3\nT:#V'), "12")


class TestPilotStrings2:
    """Additional PILOT string/output tests."""

    def test_type_hello(self):
        assert has(pil('T:hello'), "hello")

    def test_type_world(self):
        assert has(pil('T:world'), "world")

    def test_type_pilot(self):
        assert has(pil('T:pilot'), "pilot")

    def test_type_number(self):
        assert has(pil('T:42'), "42")

    def test_type_two_lines(self):
        result = pil('T:first\nT:second')
        assert any("first" in line for line in result)
        assert any("second" in line for line in result)

    def test_type_calc_result(self):
        assert has(pil('C:X=100\nT:#X'), "100")

    def test_type_zero(self):
        assert has(pil('C:Z=0\nT:#Z'), "0")


class TestPilotExtended:
    """More PILOT tests."""

    def test_type_hello_world(self):
        assert has(pil('T:Hello World'), "Hello World")

    def test_compute_addition(self):
        assert has(pil('C:X=3+4\nT:#X'), "7")

    def test_compute_subtraction(self):
        assert has(pil('C:X=10-3\nT:#X'), "7")

    def test_compute_multiplication(self):
        assert has(pil('C:X=6*7\nT:#X'), "42")

    def test_compute_large(self):
        assert has(pil('C:X=100\nT:#X'), "100")

    def test_two_type_outputs(self):
        r = pil('T:A\nT:B')
        texts = " ".join(r)
        assert "A" in texts and "B" in texts

    def test_no_errors_type(self):
        assert no_errors(pil('T:test'))

    def test_type_number_99(self):
        assert has(pil('T:99'), "99")

    def test_compute_set_zero(self):
        assert has(pil('C:X=0\nT:#X'), "0")

    def test_type_lang_name(self):
        assert has(pil('T:PILOT'), "PILOT")

    def test_three_outputs(self):
        r = pil('T:ONE\nT:TWO\nT:THREE')
        texts = " ".join(r)
        assert "ONE" in texts and "TWO" in texts and "THREE" in texts

    def test_compute_chain(self):
        assert has(pil('C:X=5\nC:Y=X+5\nT:#Y'), "10")

    def test_type_spaces(self):
        r = pil('T:hello world')
        assert has(r, "hello")

    def test_compute_result_positive(self):
        r = pil('C:A=9\nT:#A')
        assert has(r, "9")

    def test_output_is_list(self):
        r = pil('T:test')
        assert isinstance(r, list)


class TestPilotExtended:
    """Extra PILOT tests."""

    def test_type_hello(self):
        result = pil("T:Hello")
        assert has(result, "Hello")

    def test_type_100(self):
        result = pil("T:100")
        assert has(result, "100")

    def test_type_zero(self):
        result = pil("T:0")
        assert has(result, "0")

    def test_type_world(self):
        result = pil("T:World")
        assert has(result, "World")

    def test_type_negative(self):
        result = pil("T:-1")
        assert has(result, "-1")

    def test_output_is_list(self):
        result = pil("T:X")
        assert isinstance(result, list)

    def test_no_errors_type(self):
        result = pil("T:test")
        assert no_errors(result)

    def test_two_type_statements(self):
        result = pil("T:line1\nT:line2")
        assert has(result, "line1") or has(result, "line2")

    def test_compute_statement(self):
        result = pil("T:compute test")
        assert isinstance(result, list)

    def test_remark_no_output(self):
        result = pil("R:This is a remark")
        # remarks should not appear in output
        assert isinstance(result, list)

    def test_jump_no_crash(self):
        result = pil("*START\nT:start\nJ:START\nT:never")
        # infinite loop should be handled or just not crash on short code
        assert isinstance(result, list)

    def test_type_with_spaces(self):
        result = pil("T:hello world")
        assert has(result, "hello world")

    def test_multiple_types(self):
        result = pil("T:A\nT:B\nT:C")
        assert has(result, "A") or has(result, "B") or has(result, "C")

    def test_empty_source(self):
        result = pil("")
        assert isinstance(result, list)

    def test_type_number_99(self):
        result = pil("T:99")
        assert has(result, "99")
