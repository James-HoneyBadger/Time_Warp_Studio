"""Comprehensive tests for the BASIC executor."""

from __future__ import annotations


from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.BASIC


def bas(source: str, *, input_val: str = "42") -> list[str]:
    return run(source, LANG, input_val=input_val)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = bas('10 PRINT "Hello, World!"')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_number():
    out = bas("10 PRINT 42")
    assert no_errors(out)
    assert has(out, "42")


def test_print_multiple():
    out = bas('10 PRINT "A"\n20 PRINT "B"\n30 PRINT "C"')
    assert no_errors(out)
    assert has(out, "A", "B", "C")


# ---------------------------------------------------------------------------
# Variables and arithmetic
# ---------------------------------------------------------------------------


def test_let_and_print():
    out = bas("10 LET X = 7\n20 PRINT X")
    assert no_errors(out)
    assert has(out, "7")


def test_arithmetic():
    out = bas(
        "10 LET A = 10\n"
        "20 LET B = 3\n"
        "30 PRINT A + B\n"
        "40 PRINT A - B\n"
        "50 PRINT A * B\n"
        "60 PRINT A MOD B\n"
    )
    assert no_errors(out)
    assert has(out, "13", "7", "30", "1")


def test_string_variable():
    out = bas('10 A$ = "BASIC"\n20 PRINT A$')
    assert no_errors(out)
    assert has(out, "BASIC")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_then():
    out = bas('10 LET X = 5\n20 IF X > 3 THEN PRINT "YES"')
    assert no_errors(out)
    assert has(out, "YES")


def test_if_else():
    out = bas("10 LET X = 1\n" '20 IF X > 5 THEN PRINT "BIG" ELSE PRINT "SMALL"')
    assert no_errors(out)
    assert has(out, "SMALL")


def test_for_loop():
    out = bas("10 FOR I = 1 TO 3\n20 PRINT I\n30 NEXT I")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_for_step():
    out = bas("10 FOR I = 2 TO 10 STEP 2\n20 PRINT I\n30 NEXT I")
    assert no_errors(out)
    assert has(out, "2", "4", "6", "8", "10")


def test_for_with_an_empty_body_does_not_end_its_outer_loop():
    # `NEXT I` is the first line of the inner loop, so "go back to the body" is
    # a jump to the statement that is already running. The enclosing loop used
    # to lose its frame to it and run exactly once, silently.
    out = bas(
        "10 FOR F = 1 TO 3\n"
        "20 C = C + 1\n"
        "30 FOR I = 1 TO 4\n"
        "40 NEXT I\n"
        "50 NEXT F\n"
        "60 PRINT C"
    )
    assert no_errors(out)
    assert has(out, "3")


def test_goto():
    out = bas(
        '10 PRINT "START"\n' "20 GOTO 40\n" '30 PRINT "SKIPPED"\n' '40 PRINT "END"'
    )
    assert no_errors(out)
    assert has(out, "START", "END")
    blob = "\n".join(out)
    assert "SKIPPED" not in blob


# ---------------------------------------------------------------------------
# Subroutines
# ---------------------------------------------------------------------------


def test_gosub_return():
    out = bas(
        '10 PRINT "BEFORE"\n'
        "20 GOSUB 100\n"
        '30 PRINT "AFTER"\n'
        "40 END\n"
        '100 PRINT "INSIDE"\n'
        "110 RETURN"
    )
    assert no_errors(out)
    assert has(out, "BEFORE", "INSIDE", "AFTER")


# ---------------------------------------------------------------------------
# String functions
# ---------------------------------------------------------------------------


def test_len():
    out = bas('10 PRINT LEN("HELLO")')
    assert no_errors(out)
    assert has(out, "5")


def test_left_right_mid():
    out = bas(
        '10 PRINT LEFT$("Hello", 3)\n'
        '20 PRINT RIGHT$("Hello", 3)\n'
        '30 PRINT MID$("Hello", 2, 3)'
    )
    assert no_errors(out)
    assert has(out, "Hel", "llo", "ell")


def test_chr_dollar():
    out = bas("10 PRINT CHR$(65)")
    assert no_errors(out)
    assert has(out, "A")


def test_chr_lowercase():
    out = bas("10 PRINT CHR$(97)")
    assert no_errors(out)
    assert has(out, "a")


# ---------------------------------------------------------------------------
# Math functions
# ---------------------------------------------------------------------------


def test_abs():
    out = bas("10 PRINT ABS(-7)")
    assert no_errors(out)
    assert has(out, "7")


def test_int_function():
    out = bas("10 PRINT INT(3.9)")
    assert no_errors(out)
    assert has(out, "3")


def test_while_wend():
    out = bas(
        "10 LET I=1\n" "20 WHILE I<=3\n" "30 PRINT I\n" "40 LET I=I+1\n" "50 WEND"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_read_data():
    out = bas("10 READ X\n" "20 READ Y\n" "30 PRINT X+Y\n" "40 DATA 5,3")
    assert no_errors(out)
    assert has(out, "8")


def test_dim_array():
    out = bas(
        "10 DIM A(5)\n" "20 LET A(1)=10\n" "30 LET A(2)=20\n" "40 PRINT A(1)+A(2)"
    )
    assert no_errors(out)
    assert has(out, "30")


def test_mid_string():
    out = bas('10 PRINT MID$("HELLO", 2, 3)\n20 END')
    assert no_errors(out)
    assert has(out, "ELL")


def test_chr_function():
    out = bas("10 PRINT CHR$(65)\n20 END")
    assert no_errors(out)
    assert has(out, "A")


def test_abs_negative():
    out = bas("10 LET X = -42\n20 PRINT ABS(X)\n30 END")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Regressions
# ---------------------------------------------------------------------------

# The keywords each used to be matched with a bare `startswith`, so a variable
# whose name merely began with one of them was swallowed by that keyword's
# statement: the assignment never happened and PRINT echoed the name back.
KEYWORD_PERFIXED_NAMES = (
    "DOUBLE",  # swallowed by DO
    "RUNNER",  # swallowed by RUN
    "CLOSER",  # swallowed by CLOSE
    "NEXTX",  # swallowed by NEXT
    "LOOPS",  # swallowed by LOOP
    "SYSTEMS",  # swallowed by SYSTEM
    "SLEEPER",  # swallowed by SLEEP
    "RESTORED",  # swallowed by RESTORE
)


def test_variable_names_starting_with_a_keyword_are_assignable():
    for name in KEYWORD_PERFIXED_NAMES:
        out = bas(f"10 {name} = 42\n20 PRINT {name}\n30 END")
        assert no_errors(out), f"{name}: {out}"
        assert has(out, "42"), f"{name} did not survive; output was {out}"


def test_on_timer_accepts_its_documented_format():
    # The dispatcher sliced one character too many, dropping the "(", so the
    # handler rejected the very format its own error message asks for.
    out = bas('10 ON TIMER(2) GOSUB 100\n20 PRINT "OK"\n30 END\n100 RETURN')
    assert no_errors(out), out
    assert has(out, "Timer set")


def test_function_block_returns_its_value_through_call():
    # A FUNCTION is callable with CALL, and its result lands in a variable named
    # after the function. Calling it as an expression is a separate, recorded gap.
    out = bas(
        "10 FUNCTION TWICE(N)\n20 TWICE = N * 2\n30 END FUNCTION\n40 CALL TWICE(21)\n50 PRINT TWICE"
    )
    assert no_errors(out)
    assert has(out, "42")


def test_joyinit_absence_is_informational_not_an_error():
    # An optional backend being absent is not the learner's program failing, and
    # the error prefix is what the IDE presents as a failure.
    out = bas("10 JOYINIT")
    assert no_errors(out), out
    assert ok(out)


def test_unresolved_name_is_reported_rather_than_echoed():
    out = bas("10 PRINT NOSUCHFN(3)")
    assert not has(out, "NOSUCHFN(3)"), out


def test_unset_numeric_variable_prints_zero():
    out = bas("10 PRINT UNSET")
    assert no_errors(out)
    assert has(out, "0")


def test_unset_name_inside_an_expression_is_zero():
    out = bas("10 PRINT UNSET + 1")
    assert no_errors(out)
    assert has(out, "1")


def test_peek_and_in_work_inside_expressions():
    out = bas(
        "10 POKE 1000, 42\n20 X = PEEK(1000)\n30 PRINT X\n40 Y = IN(60)\n50 PRINT Y"
    )
    assert no_errors(out)
    assert has(out, "42")


def test_stick_and_strig_exist_without_a_gamepad():
    out = bas("10 PRINT STICK(0)\n20 PRINT STRIG(0)")
    assert no_errors(out)
    assert has(out, "0")


def test_defstr_name_prints_its_string_value():
    out = bas('10 DEFSTR A-Z\n20 A = "hello"\n30 PRINT A')
    assert no_errors(out)
    assert has(out, "hello")


def test_type_dollar_reports_the_type():
    out = bas("10 X = 5\n20 PRINT TYPE$(X)")
    assert no_errors(out)
    assert has(out, "DOUBLE")


def test_speak_reaches_the_speech_handler():
    out = bas('10 SPEAK "hello"')
    assert not any("Unknown BASIC command" in line for line in out), out


def test_resume_next_without_line_numbers():
    out = bas('ON ERROR GOTO 100\nX = 1 / 0\nPRINT "AFTER"\nEND\n100 RESUME NEXT')
    assert no_errors(out)
    assert has(out, "AFTER")


# ---------------------------------------------------------------------------
# Example programs run without errors
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib

    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "basic" / "hello.bas"
    ).read_text()
    out = bas(src)
    assert no_errors(out)
    assert ok(out)
