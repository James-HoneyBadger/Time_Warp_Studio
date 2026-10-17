"""Comprehensive tests for the COMAL executor."""

from __future__ import annotations

from .conftest_lang import first_error, has, no_errors, run

from time_warp.core.interpreter import Language

LANG = Language.COMAL


def comal(source: str) -> list[str]:
    return run(source, LANG)


def test_hello_world():
    out = comal('10 PRINT "Hello, World!"\n')
    assert has(out, "Hello, World!")
    assert no_errors(out)


def test_print_arithmetic():
    assert comal("10 PRINT 6 * 7\n") == ["42"]


def test_for_loop_counts():
    assert comal("10 FOR I := 1 TO 5\n20 PRINT I\n30 NEXT I\n") == [
        "1",
        "2",
        "3",
        "4",
        "5",
    ]


def test_for_loop_negative_step():
    out = comal("10 FOR I := 10 TO 1 STEP -3\n20 PRINT I\n30 NEXT I\n")
    assert out == ["10", "7", "4", "1"]


def test_while_loop():
    out = comal("10 I := 0\n20 WHILE I < 3\n30 I := I + 1\n40 PRINT I\n50 ENDWHILE\n")
    assert out == ["1", "2", "3"]


def test_repeat_until():
    out = comal("10 I := 0\n20 REPEAT\n30 I := I + 1\n40 PRINT I\n50 UNTIL I >= 3\n")
    assert out == ["1", "2", "3"]


def test_if_elif_else_block():
    src = (
        '10 X := 5\n20 IF X > 10 THEN\n30 PRINT "big"\n'
        '40 ELIF X > 3 THEN\n50 PRINT "medium"\n60 ELSE\n70 PRINT "small"\n80 ENDIF\n'
    )
    assert comal(src) == ["medium"]


def test_if_single_line():
    assert comal('10 IF 1 < 2 THEN PRINT "yes"\n') == ["yes"]


def test_procedure_call_with_parameter():
    src = (
        '10 PROC GREET(NAME$)\n20 PRINT "Hello, "; NAME$\n30 ENDPROC\n'
        '40 CALL GREET("World")\n'
    )
    assert has(comal(src), "Hello, World")


def test_loop_exit_statement():
    src = "10 FOR I := 1 TO 10\n20 IF I = 4 THEN EXIT\n30 PRINT I\n40 NEXT I\n"
    assert comal(src) == ["1", "2", "3"]


def test_array_element_assignment():
    assert comal("10 DIM A(3)\n20 A(1) := 42\n30 PRINT A(1)\n") == ["42"]


def test_data_and_read():
    assert comal("10 DATA 7, 8\n20 READ X\n30 READ Y\n40 PRINT X + Y\n") == ["15"]


def test_goto_jumps_to_line():
    assert comal('10 GOTO 30\n20 PRINT "skipped"\n30 PRINT "here"\n') == ["here"]


def test_input_reads_numeric_value():
    out = run('10 INPUT "n: ": N\n20 PRINT N * 2\n', LANG, input_val="21")
    assert has(out, "42")


def test_comments_are_ignored():
    assert comal('10 REM a comment\n20 PRINT "x"  // trailing\n') == ["x"]


def test_string_concatenation_in_print():
    assert comal('10 PRINT "AB"; "CD"\n') == ["ABCD"]


def test_syntax_error_is_reported():
    assert first_error(comal("10 FOR\n")) is not None


# ---------------------------------------------------------------------------
# User functions as expressions
# ---------------------------------------------------------------------------


def test_func_is_callable_in_an_expression():
    out = comal("10 FUNC DOUBLE(N)\n20 RETURN N * 2\n30 ENDFUNC\n40 PRINT DOUBLE(21)")
    assert no_errors(out), out
    assert has(out, "42")


def test_func_with_two_parameters_in_an_expression():
    out = comal("10 FUNC ADD(A,B)\n20 RETURN A + B\n30 ENDFUNC\n40 PRINT ADD(2,3)")
    assert no_errors(out), out
    assert has(out, "5")


def test_func_local_variable_in_an_expression():
    out = comal(
        "10 FUNC TRIPLE(N)\n20 M = N * 3\n30 RETURN M\n40 ENDFUNC\n50 PRINT TRIPLE(4)"
    )
    assert no_errors(out), out
    assert has(out, "12")
