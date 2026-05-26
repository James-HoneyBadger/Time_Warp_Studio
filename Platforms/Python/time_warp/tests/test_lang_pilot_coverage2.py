"""Extended coverage tests for pilot.py — targeting uncovered code paths.

Focus areas:
  - COMPUTE (long-form) normalization without "="
  - TU command (long-form subroutine call)
  - Invalid command with no colon (colon_pos < 1)
  - Empty M: pattern
  - M: wildcard patterns (*/?), substring matching
  - Y: standalone conditional jump
  - C: with string variable (NAME$ = value)
  - J: with empty label (no-op)
  - U: with "=" (alias for C:), U: without "=" (print var)
  - P: Pause command
  - B: branch command (error cases and successful jump)
  - L: lesson link command
  - H: hint command
  - N: no-match command
  - G: graphics error paths (missing args for FORWARD/BACK/LEFT/RIGHT/SETXY/CIRCLE/etc.)
  - G: SETHEADING/SETH, HIDETURTLE/HT, SHOWTURTLE/ST, ARC, FILL, DOT, STAMP, TEXT, SPEED
  - G: unknown graphics command
  - F: file operations (OPEN/CLOSE/READ/WRITE)
"""
from __future__ import annotations

import os
import tempfile

from time_warp.core.interpreter import Language

from .conftest_lang import has, no_errors, run

L = Language.PILOT


def pil(source: str, **kw) -> list[str]:
    return run(source, L, **kw)


# ============================================================================
# LONG-FORM COMMANDS
# ============================================================================


class TestLongFormCommands:
    """Tests for the LONGFORM_MAP normalization (lines 100-130)."""

    def test_compute_longform_with_equals(self):
        """COMPUTE X = 5 (standard long form — longform map path)."""
        out = pil("COMPUTE X = 5\nT:#X\nE:")
        assert has(out, "5")

    def test_compute_longform_without_equals(self):
        """COMPUTE X 5 — normalised to X = 5 (lines 114-115)."""
        out = pil("COMPUTE X 5\nT:#X\nE:")
        assert has(out, "5")

    def test_type_longform(self):
        """TYPE is long form of T:."""
        out = pil("TYPE Hello world\nE:")
        assert has(out, "Hello world")

    def test_accept_longform(self):
        """ACCEPT is long form of A:."""
        # Just exercise the path; no input needed since start_input_request handles it
        out = pil("ACCEPT\nE:")
        assert isinstance(out, list)

    def test_jump_longform(self):
        """JUMP is long form of J:."""
        out = pil("JUMP *done\n*done:\nT:reached\nE:")
        assert has(out, "reached")

    def test_end_longform_stop(self):
        """STOP is long form of E: (end/stop)."""
        out = pil("T:start\nSTOP\nT:unreachable\n")
        assert has(out, "start")
        # Should NOT reach "unreachable"
        text = " ".join(out)
        assert "unreachable" not in text

    def test_end_longform_end(self):
        """END is long form of E:."""
        out = pil("T:begin\nEND\nT:never\n")
        assert has(out, "begin")

    def test_tu_subroutine_call(self):
        """TU *LABEL — long-form subroutine call (lines 124-130)."""
        src = (
            "TU *sub\n"
            "T:after return\n"
            "E:\n"
            "*sub\n"
            "T:in sub\n"
            "R:\n"
        )
        out = pil(src)
        assert has(out, "in sub")
        assert has(out, "after return")

    def test_tu_no_label_noop(self):
        """TU with no label is a no-op (label is empty)."""
        out = pil("TU\nT:ok\nE:")
        assert has(out, "ok")


# ============================================================================
# INVALID COMMANDS
# ============================================================================


class TestInvalidCommands:
    def test_no_colon_invalid(self):
        """Command without colon produces an error (line 135)."""
        out = pil("NOTACOMMAND\nE:")
        text = " ".join(out)
        assert "❌" in text or "Invalid" in text

    def test_invalid_prefix_too_long(self):
        """Prefix longer than 2 chars is invalid."""
        out = pil("TYN:hello\nE:")
        text = " ".join(out)
        assert "❌" in text or "Invalid" in text


# ============================================================================
# M: MATCH — EDGE CASES
# ============================================================================


class TestMatchEdgeCases:
    def test_m_empty_pattern(self):
        """M: with no pattern sets last_match_succeeded = False (lines 182-183)."""
        out = pil("M:\nTY:should not appear\nT:ok\nE:")
        text = " ".join(out)
        assert "should not appear" not in text
        assert has(out, "ok")

    def test_m_wildcard_match(self):
        """M: with * wildcard matches via regex (lines 198-206)."""
        out = pil("A:\nM:H*LLO\nTY:matched\nE:", input_val="HELLO")
        assert has(out, "matched")

    def test_m_wildcard_question(self):
        """M: with ? wildcard matches via regex."""
        out = pil("A:\nM:HEL?O\nTY:matched\nE:", input_val="HELLO")
        assert has(out, "matched")

    def test_m_wildcard_no_match(self):
        """M: wildcard that doesn't match sets last_match_succeeded = False."""
        out = pil("A:\nM:Z*Z\nTY:matched\nTN:not matched\nE:", input_val="HELLO")
        assert has(out, "not matched")

    def test_m_substring_match(self):
        """M: substring matching (lines 217-226)."""
        out = pil("A:\nM:ELL\nTY:found substring\nE:", input_val="HELLO WORLD")
        assert has(out, "found substring")

    def test_m_substring_no_match(self):
        """Substring not found — match fails."""
        out = pil("A:\nM:XYZ\nTY:found\nTN:not found\nE:", input_val="HELLO")
        assert has(out, "not found")

    def test_m_comma_separated_alternatives(self):
        """M: matches second alternative."""
        out = pil("A:\nM:YES,YEAH,YEP\nTY:yes!\nE:", input_val="YEAH")
        assert has(out, "yes!")

    def test_m_broken_regex_is_safe(self):
        """M: with a broken wildcard pattern that produces bad regex doesn't crash."""
        out = pil("A:\nM:[\nTN:no match\nE:", input_val="HELLO")
        # Should not crash
        assert isinstance(out, list)


# ============================================================================
# Y: STANDALONE CONDITIONAL JUMP
# ============================================================================


class TestYJump:
    def test_y_jumps_on_match(self):
        """Y:*label jumps if last M: succeeded (line 236+)."""
        # First match everything with wildcard, then Y: should jump
        out = pil(
            "A:\n"
            "M:*\n"
            "Y:*done\n"
            "T:not reached\n"
            "*done\n"
            "T:jumped\n"
            "E:",
            input_val="anything",
        )
        text = " ".join(out)
        assert "not reached" not in text
        assert has(out, "jumped")

    def test_y_skips_when_no_match(self):
        """Y:*label does NOT jump if last M: failed."""
        out = pil(
            "A:\n"
            "M:IMPOSSIBLE_MATCH_XYZ\n"
            "Y:*done\n"
            "T:continued\n"
            "*done:\n"
            "T:jumped\n"
            "E:",
            input_val="HELLO",
        )
        assert has(out, "continued")

    def test_y_no_label_noop(self):
        """Y: with no label is a no-op."""
        out = pil("A:\nM:*\nY:\nT:ok\nE:", input_val="test")
        assert has(out, "ok")


# ============================================================================
# C: COMPUTE — STRING VARIABLE
# ============================================================================


class TestComputeStringVar:
    def test_c_string_variable(self):
        """C:NAME$ = HELLO stores in string variables (lines 256-258)."""
        out = pil("C:GREETING$ = Hello World\nT:$GREETING$\nE:")
        assert has(out, "Hello World")

    def test_c_string_variable_multi_word(self):
        """C:MSG$ = multi word value."""
        out = pil("C:MSG$ = Good Morning\nT:$MSG$\nE:")
        assert has(out, "Good Morning")


# ============================================================================
# J: JUMP — EDGE CASES
# ============================================================================


class TestJumpEdgeCases:
    def test_j_with_label_jumps(self):
        """J:*label jumps to label."""
        out = pil("J:*end\nT:skipped\n*end\nT:done\nE:")
        assert has(out, "done")
        text = " ".join(out)
        assert "skipped" not in text

    def test_j_empty_label_noop(self):
        """J: with no label is a no-op (line 268+)."""
        out = pil("J:\nT:after jump\nE:")
        assert has(out, "after jump")


# ============================================================================
# U: COMMAND
# ============================================================================


class TestUCommand:
    def test_u_with_equals_computes(self):
        """U:X = 5 is an alias for C:X = 5 (line 272)."""
        out = pil("U:X = 42\nT:#X\nE:")
        assert has(out, "42")

    def test_u_without_equals_prints_var(self):
        """U:VARNAME prints the current value of the variable."""
        out = pil("C:SCORE = 100\nU:SCORE\nE:")
        assert has(out, "100")

    def test_u_empty_var_name(self):
        """U: with no variable name returns an error."""
        out = pil("U:\nE:")
        text = " ".join(out)
        assert "❌" in text


# ============================================================================
# P: PAUSE COMMAND
# ============================================================================


class TestPauseCommand:
    def test_p_pause(self):
        """P: triggers input request and returns empty (no output)."""
        out = pil("T:before\nP:\nT:after\nE:")
        # P: starts input request; in test mode no crash expected
        assert has(out, "before")


# ============================================================================
# B: BRANCH COMMAND
# ============================================================================


class TestBranchCommand:
    def test_b_without_equals_error(self):
        """B: without = produces an error (lines 285-290)."""
        out = pil("B:SOMETHING\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_b_empty_label_error(self):
        """B:condition= with empty label produces an error (line 293)."""
        out = pil("B:1=\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_b_true_condition_jumps(self):
        """B:1=*done jumps when condition is non-zero (line 297)."""
        out = pil(
            "B:1=*end\n"
            "T:skipped\n"
            "*end\n"
            "T:branched\n"
            "E:"
        )
        text = " ".join(out)
        assert "skipped" not in text
        assert has(out, "branched")

    def test_b_false_condition_no_jump(self):
        """B:0=*done does not jump when condition is zero."""
        out = pil(
            "B:0=*end\n"
            "T:continued\n"
            "*end:\n"
            "T:label\n"
            "E:"
        )
        assert has(out, "continued")

    def test_b_with_variable(self):
        """B:X=*done jumps when X != 0."""
        out = pil(
            "C:X = 5\n"
            "B:X=*end\n"
            "T:skipped\n"
            "*end\n"
            "T:branched\n"
            "E:"
        )
        assert has(out, "branched")


# ============================================================================
# L: LESSON LINK COMMAND
# ============================================================================


class TestLessonLink:
    def test_l_lesson_link(self):
        """L:lesson1 stores lesson link and returns info message (lines 358-359)."""
        out = pil("L:lesson01\nE:")
        # L: returns a lesson link message — check output or no crash
        assert isinstance(out, list)

    def test_l_empty_noop(self):
        """L: with empty name is a no-op."""
        out = pil("L:\nT:ok\nE:")
        assert has(out, "ok")


# ============================================================================
# H: HINT COMMAND
# ============================================================================


class TestHintCommand:
    def test_h_basic_hint(self):
        """H:hint text returns a hint message (lines 363-389)."""
        out = pil("H:Try using a loop\nE:")
        text = " ".join(out)
        assert "Try using a loop" in text

    def test_h_with_variable(self):
        """H: interpolates string variables."""
        out = pil("C:TOPIC$ = loops\nH:Read about $TOPIC$\nE:")
        text = " ".join(out)
        assert "loops" in text

    def test_h_empty_noop(self):
        """H: with no text returns empty string."""
        out = pil("H:\nT:ok\nE:")
        assert has(out, "ok")


# ============================================================================
# N: NO-MATCH COMMAND
# ============================================================================


class TestNoMatchCommand:
    def test_n_executes_when_no_match(self):
        """N:message executes as T:message when last match failed (line 394)."""
        out = pil(
            "A:\n"
            "M:IMPOSSIBLE_XYZ\n"
            "N:Please try again\n"
            "E:",
            input_val="HELLO",
        )
        assert has(out, "Please try again")

    def test_n_skipped_when_matched(self):
        """N:message is skipped when last M: succeeded."""
        out = pil(
            "A:\n"
            "M:HELLO\n"
            "N:Please try again\n"
            "T:good\n"
            "E:",
            input_val="HELLO",
        )
        text = " ".join(out)
        assert "Please try again" not in text
        assert has(out, "good")


# ============================================================================
# G: GRAPHICS — ERROR PATHS
# ============================================================================


class TestGraphicsErrors:
    def test_g_forward_no_arg(self):
        """G:FORWARD with no arg returns error (line 499)."""
        out = pil("G:FORWARD\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_forward_invalid(self):
        """G:FORWARD with non-numeric arg returns error."""
        out = pil("G:FORWARD abc\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_back_no_arg(self):
        """G:BACK with no arg returns error (line 509-510)."""
        out = pil("G:BACK\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_back_invalid(self):
        """G:BACK with invalid arg returns error."""
        out = pil("G:BACK xyz\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_left_no_arg(self):
        """G:LEFT with no arg returns error (lines 517-518)."""
        out = pil("G:LEFT\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_left_invalid(self):
        """G:LEFT with invalid arg returns error."""
        out = pil("G:LEFT qqq\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_right_no_arg(self):
        """G:RIGHT with no arg returns error (line 526+)."""
        out = pil("G:RIGHT\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_right_invalid(self):
        """G:RIGHT with invalid arg returns error."""
        out = pil("G:RIGHT bad\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setxy_one_arg(self):
        """G:SETXY 10 with only one coord returns error (lines 532-533)."""
        out = pil("G:SETXY 10\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setxy_invalid_coords(self):
        """G:SETXY x,y with invalid values returns error."""
        out = pil("G:SETXY abc,def\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_circle_no_arg(self):
        """G:CIRCLE with no arg returns error (line 541+)."""
        out = pil("G:CIRCLE\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_circle_invalid(self):
        """G:CIRCLE with invalid arg returns error."""
        out = pil("G:CIRCLE abc\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setbgcolor_two_args(self):
        """G:SETBGCOLOR 255 0 with only 2 args returns error (lines 547-548)."""
        out = pil("G:SETBGCOLOR 255 0\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setbgcolor_invalid(self):
        """G:SETBGCOLOR with non-numeric values returns error."""
        out = pil("G:SETBGCOLOR r g b\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setpencolor_two_args(self):
        """G:SETPENCOLOR 0 255 with only 2 args returns error (line 551+)."""
        out = pil("G:SETPENCOLOR 0 255\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setpenwidth_no_arg(self):
        """G:SETPENWIDTH with no arg returns error."""
        out = pil("G:SETPENWIDTH\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setpenwidth_invalid(self):
        """G:SETPENWIDTH with invalid arg returns error."""
        out = pil("G:SETPENWIDTH abc\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setheading_no_arg(self):
        """G:SETHEADING with no arg returns error."""
        out = pil("G:SETHEADING\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_setheading_invalid(self):
        """G:SETHEADING with invalid arg returns error."""
        out = pil("G:SETHEADING abc\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_arc_one_arg(self):
        """G:ARC with only radius returns error."""
        out = pil("G:ARC 50\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_arc_invalid(self):
        """G:ARC with invalid values returns error."""
        out = pil("G:ARC abc def\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_g_unknown_command(self):
        """Unknown G: command returns error."""
        out = pil("G:UNKNOWNCMD\nE:")
        text = " ".join(out)
        assert "❌" in text or "Unknown" in text

    def test_g_speed_invalid(self):
        """G:SPEED with invalid value returns error."""
        out = pil("G:SPEED abc\nE:")
        text = " ".join(out)
        assert "❌" in text


# ============================================================================
# G: GRAPHICS — SUCCESS PATHS (previously uncovered)
# ============================================================================


class TestGraphicsSuccess:
    def test_g_setheading(self):
        """G:SETHEADING 90 sets heading without error."""
        out = pil("G:SETHEADING 90\nE:")
        assert no_errors(out)

    def test_g_seth(self):
        """G:SETH 45 is alias for SETHEADING."""
        out = pil("G:SETH 45\nE:")
        assert no_errors(out)

    def test_g_hideturtle(self):
        """G:HIDETURTLE hides the turtle."""
        out = pil("G:HIDETURTLE\nE:")
        assert no_errors(out)

    def test_g_ht(self):
        """G:HT is alias for HIDETURTLE."""
        out = pil("G:HT\nE:")
        assert no_errors(out)

    def test_g_showturtle(self):
        """G:SHOWTURTLE shows the turtle."""
        out = pil("G:SHOWTURTLE\nE:")
        assert no_errors(out)

    def test_g_st(self):
        """G:ST is alias for SHOWTURTLE."""
        out = pil("G:ST\nE:")
        assert no_errors(out)

    def test_g_arc(self):
        """G:ARC radius extent draws an arc."""
        out = pil("G:ARC 50 180\nE:")
        assert no_errors(out)

    def test_g_arc_comma(self):
        """G:ARC 50,90 draws an arc (comma-separated args)."""
        out = pil("G:ARC 50,90\nE:")
        assert no_errors(out)

    def test_g_fill(self):
        """G:FILL fills the last closed shape."""
        out = pil("G:FILL\nE:")
        assert no_errors(out)

    def test_g_dot_no_arg(self):
        """G:DOT without radius draws a default-sized dot."""
        out = pil("G:DOT\nE:")
        assert no_errors(out)

    def test_g_dot_with_arg(self):
        """G:DOT 5 draws a dot with radius 5."""
        out = pil("G:DOT 5\nE:")
        assert no_errors(out)

    def test_g_stamp(self):
        """G:STAMP draws turtle stamp at current position."""
        out = pil("G:STAMP\nE:")
        assert no_errors(out)

    def test_g_text(self):
        """G:TEXT message outputs a label."""
        out = pil("G:TEXT Hello there\nE:")
        text = " ".join(out)
        assert "Hello there" in text

    def test_g_text_empty(self):
        """G:TEXT with empty message is a no-op."""
        out = pil("G:TEXT\nE:")
        assert no_errors(out)

    def test_g_speed(self):
        """G:SPEED 5 sets turtle speed."""
        out = pil("G:SPEED 5\nE:")
        assert no_errors(out)

    def test_g_setxy_success(self):
        """G:SETXY 10,20 moves turtle to position."""
        out = pil("G:SETXY 10,20\nE:")
        assert no_errors(out)

    def test_g_setxy_space_separated(self):
        """G:SETXY 10 20 moves turtle using space-separated coords."""
        out = pil("G:SETXY 10 20\nE:")
        assert no_errors(out)

    def test_g_setbgcolor_success(self):
        """G:SETBGCOLOR 0 0 255 sets background color."""
        out = pil("G:SETBGCOLOR 0 0 255\nE:")
        assert no_errors(out)

    def test_g_setpencolor_success(self):
        """G:SETPENCOLOR 255 0 0 sets pen color."""
        out = pil("G:SETPENCOLOR 255 0 0\nE:")
        assert no_errors(out)

    def test_g_forward_back_sequence(self):
        """G:FORWARD then G:BACK draws and returns."""
        out = pil("G:FORWARD 50\nG:BACK 50\nE:")
        assert no_errors(out)

    def test_g_turn_sequence(self):
        """G:LEFT 90 then G:RIGHT 90 — combined turn."""
        out = pil("G:LEFT 90\nG:RIGHT 90\nE:")
        assert no_errors(out)

    def test_g_circle_success(self):
        """G:CIRCLE 30 draws a circle."""
        out = pil("G:CIRCLE 30\nE:")
        assert no_errors(out)

    def test_g_setpenwidth_success(self):
        """G:SETPENWIDTH 3 sets pen width."""
        out = pil("G:SETPENWIDTH 3\nE:")
        assert no_errors(out)


# ============================================================================
# F: FILE OPERATIONS
# ============================================================================


class TestFileOperations:
    """File tests use relative paths (validator rejects absolute paths)."""

    def _make_tmpfile(self, content: str = "", suffix: str = ".txt") -> str:
        """Create a temp file in current dir; return basename."""
        with tempfile.NamedTemporaryFile(
            suffix=suffix, dir=".", delete=False, mode="w", encoding="utf-8"
        ) as f:
            f.write(content)
            return os.path.basename(f.name)

    def test_f_write_and_read(self):
        """F:OPEN/WRITE/CLOSE round-trip (lines 810-913)."""
        fname = self._make_tmpfile()
        try:
            src = (
                f"F:OPEN {fname} W\n"
                f"F:WRITE {fname} HelloPILOT\n"
                f"F:CLOSE {fname}\n"
                "E:"
            )
            out = pil(src)
            assert no_errors(out)
            with open(fname) as fh:
                content = fh.read()
            assert "HelloPILOT" in content
        finally:
            os.unlink(fname)

    def test_f_read_variable(self):
        """F:READ stores line into variable."""
        fname = self._make_tmpfile("42\n")
        try:
            src = (
                f"F:OPEN {fname} R\n"
                f"F:READ {fname} X\n"
                f"F:CLOSE {fname}\n"
                "T:#X\n"
                "E:"
            )
            out = pil(src)
            assert has(out, "42")
        finally:
            os.unlink(fname)

    def test_f_read_string_variable(self):
        """F:READ stores text into string variable."""
        fname = self._make_tmpfile("hello world\n")
        try:
            src = (
                f"F:OPEN {fname} R\n"
                f"F:READ {fname} LINE$\n"
                f"F:CLOSE {fname}\n"
                "T:$LINE$\n"
                "E:"
            )
            out = pil(src)
            assert has(out, "hello world")
        finally:
            os.unlink(fname)

    def test_f_append_mode(self):
        """F:OPEN in append mode writes to end of file."""
        fname = self._make_tmpfile("line1\n")
        try:
            src = (
                f"F:OPEN {fname} A\n"
                f"F:WRITE {fname} line2\n"
                f"F:CLOSE {fname}\n"
                "E:"
            )
            out = pil(src)
            assert no_errors(out)
            with open(fname) as fh:
                content = fh.read()
            assert "line1" in content
            assert "line2" in content
        finally:
            os.unlink(fname)

    def test_f_open_invalid_mode(self):
        """F:OPEN with invalid mode returns error."""
        out = pil("F:OPEN somefile.txt X\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_f_close_not_open(self):
        """F:CLOSE on a file that's not open returns error."""
        out = pil("F:CLOSE notopen.txt\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_f_read_not_open(self):
        """F:READ on a file that's not open returns error."""
        out = pil("F:READ notopen.txt MYVAR\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_f_write_not_open(self):
        """F:WRITE on a file that's not open returns error."""
        out = pil("F:WRITE notopen.txt hello\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_f_unknown_operation(self):
        """Unknown F: operation returns error."""
        out = pil("F:MERGE somefile.txt\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_f_empty_returns_error(self):
        """F: with no operation returns error."""
        out = pil("F:\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_f_open_nonexistent_read(self):
        """F:OPEN non-existent file for reading returns error."""
        out = pil("F:OPEN nonexistent_pilot_99999.txt R\nE:")
        text = " ".join(out)
        assert "❌" in text

    def test_f_open_read_alias(self):
        """F:OPEN file READ (alias for R) works."""
        fname = self._make_tmpfile("test\n")
        try:
            src = f"F:OPEN {fname} READ\nF:CLOSE {fname}\nE:"
            out = pil(src)
            assert no_errors(out)
        finally:
            os.unlink(fname)

    def test_f_open_write_alias(self):
        """F:OPEN file WRITE (alias for W) works."""
        fname = self._make_tmpfile()
        try:
            src = f"F:OPEN {fname} WRITE\nF:CLOSE {fname}\nE:"
            out = pil(src)
            assert no_errors(out)
        finally:
            os.unlink(fname)


# ============================================================================
# A: ACCEPT WITH $VARIABLE SYNTAX
# ============================================================================


class TestAcceptWithVariable:
    def test_a_dollar_var(self):
        """A:$myvar routes input to MYVAR$ string variable (lines 182-183)."""
        # Just test that this doesn't crash — input will be empty in test mode
        out = pil("A:$USERNAME\nE:")
        assert isinstance(out, list)


# ============================================================================
# COMBINED INTEGRATION TESTS
# ============================================================================


class TestIntegration:
    def test_quiz_flow(self):
        """Full quiz flow: type question, accept, match, give hint."""
        src = (
            "T:What is 2 + 2?\n"
            "A:\n"
            "M:4,FOUR\n"
            "TY:Correct!\n"
            "TN:Incorrect\n"
            "H:Try again — add 2 to 2\n"
            "E:"
        )
        out = pil(src, input_val="4")
        assert has(out, "Correct!")

    def test_branch_loop(self):
        """B: branch used for loop-like control."""
        src = (
            "C:X = 1\n"
            "*loop:\n"
            "T:X = #X\n"
            "C:X = X + 1\n"
            "B:X <= 3 = *loop\n"
            "T:done\n"
            "E:"
        )
        out = pil(src)
        # At least one "X =" line should appear
        text = " ".join(out)
        assert "X =" in text

    def test_subroutine_with_s_and_r(self):
        """S:*sub and R: used for real subroutines."""
        src = (
            "S:*greet\n"
            "T:main done\n"
            "E:\n"
            "*greet\n"
            "T:Hello from sub\n"
            "R:\n"
        )
        out = pil(src)
        assert has(out, "Hello from sub")
        assert has(out, "main done")
