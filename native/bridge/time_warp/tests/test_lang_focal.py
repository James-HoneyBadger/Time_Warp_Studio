"""FOCAL-69 executor tests.

The emphasis is on the behaviour that makes FOCAL FOCAL, because that is the part
an educational reimplementation is most likely to get subtly wrong: branching by
sign, the FOR forms, and DO returning at the end of the program rather than at a
RETURN statement.
"""

from __future__ import annotations

from time_warp.core.interpreter import Language

from .conftest_lang import first_error, has, no_errors, run

FOCAL = Language.FOCAL


def test_type_prints_text_and_expressions() -> None:
    output = run('01.10 T "X=",2+3', FOCAL)
    assert has(output, "X=5.00")
    assert no_errors(output)


def test_set_then_read_back() -> None:
    output = run('01.10 S X=2+3\n01.20 T "X=",X', FOCAL)
    assert has(output, "X=5.00")


def test_format_directive_sets_decimal_places() -> None:
    output = run('01.10 T %0,"R=",7/2', FOCAL)
    assert has(output, "R=4")


def test_bang_suppresses_the_newline_within_a_statement() -> None:
    """`!` joins output inside one TYPE.

    It cannot join output *across* statements: the engine logs each statement's
    output as its own entry, so the newline between two lines is added by the
    logger rather than by the statement. That is an engine-level limit, and
    docs/ROADMAP.md records it with FOCAL's other boundaries.
    """
    output = run('01.10 T "A"!"B"', FOCAL)
    assert has(output, "AB")


def test_if_branches_on_the_sign() -> None:
    for value, expected in ((-3, "NEG"), (0, "ZERO"), (5, "POS")):
        output = run(
            f"01.10 S X={value}\n"
            "01.20 IF (X) 1.40,1.30,1.50\n"
            '01.30 T "ZERO"\n'
            '01.40 T "NEG"\n'
            '01.50 T "POS"',
            FOCAL,
        )
        assert has(output, expected), (value, output)


def test_if_omitted_targets_fall_through() -> None:
    output = run(
        '01.10 S X=5\n01.20 IF (X) ,,1.40\n01.30 T "SKIPPED"\n01.40 T "DONE"',
        FOCAL,
    )
    assert has(output, "DONE")
    assert not has(output, "SKIPPED")


def test_for_runs_the_rest_of_the_line() -> None:
    output = run("01.10 FOR I=1,3;T I", FOCAL)
    assert has(output, "1.00", "2.00", "3.00")


def test_for_takes_a_step_and_a_limit() -> None:
    output = run("01.10 FOR I=0,2,6;T I;", FOCAL)
    assert has(output, "0.00", "2.00", "4.00", "6.00")
    assert not has(output, "8.00")


def test_for_accepts_several_statements_in_its_body() -> None:
    """`FOR I=1,4;FD 50;RT 90` is a square, not a syntax error."""
    output = run("01.10 FOR I=1,4;FD 50;RT 90", FOCAL)
    assert no_errors(output)


def test_for_keeps_the_last_value_the_body_saw() -> None:
    output = run("01.10 FOR I=1,4;T I", FOCAL)
    assert has(output, "4.00")
    assert not has(output, "5.00")


def test_do_runs_a_group_and_returns_at_the_end() -> None:
    output = run(
        '01.10 T "START"\n01.20 DO 2.10\n01.30 T "END"\n01.40 Q\n02.10 T "GROUP"',
        FOCAL,
    )
    assert has(output, "START", "GROUP", "END")


def test_do_nesting_is_bounded_and_reported() -> None:
    output = run("01.10 DO 1.10", FOCAL)
    assert has(output, "❌")
    assert has(output, "DO nested more than")


def test_labels_compare_as_numbers() -> None:
    """`01.10` and `1.10` name the same line; a reference need not match spelling."""
    output = run('01.10 GOTO 2.10\n02.10 T "THERE"', FOCAL)
    assert has(output, "THERE")


def test_erase_all_forgets_variables() -> None:
    output = run("01.10 S A=1\n01.20 E ALL\n01.30 T A", FOCAL)
    assert has(output, "0.00")


def test_quit_stops_the_program() -> None:
    output = run('01.10 T "A"\n01.20 Q\n01.30 T "NEVER"', FOCAL)
    assert has(output, "A")
    assert not has(output, "NEVER")


def test_turtle_movement_is_silent() -> None:
    output = run("01.10 FOR I=1,4;FD 80;RT 90", FOCAL)
    assert output == [], output


def test_turtle_argument_may_be_a_variable() -> None:
    output = run("01.10 S D=80\n01.20 FD D", FOCAL)
    assert no_errors(output)


def test_turtle_without_a_distance_is_an_error() -> None:
    output = run("01.10 FD", FOCAL)
    assert has(output, "❌", "needs a distance")


def test_function_set_is_available() -> None:
    output = run("01.10 T FSQT(16)", FOCAL)
    assert has(output, "4.00")


def test_unknown_command_is_reported_not_swallowed() -> None:
    output = run("01.10 TIPE X", FOCAL)
    assert has(output, "❌", "unknown command")


def test_unquoted_word_is_reported() -> None:
    """FOCAL zeroes unassigned variables, so `T HELLO` must not print 0.00."""
    output = run("01.10 T HELLO", FOCAL)
    assert has(output, "❌")
    assert has(output, "quotes")


def test_unassigned_single_letter_is_zero() -> None:
    output = run("01.10 T X", FOCAL)
    assert has(output, "0.00")
    assert no_errors(output)


def test_missing_label_is_reported() -> None:
    output = run("01.10 GOTO 9.9", FOCAL)
    assert has(output, "❌", "no line labelled")


def test_division_by_zero_is_reported() -> None:
    output = run("01.10 T 1/0", FOCAL)
    assert first_error(output) is not None
    assert has(output, "division by zero")


def test_ask_requests_input() -> None:
    from time_warp.core.interpreter import Interpreter
    from time_warp.graphics.turtle_state import TurtleState

    interp = Interpreter()
    interp.load_program('01.10 ASK "How many? " N\n01.20 T N', language=FOCAL)
    turtle = TurtleState()
    interp.execute(turtle)
    assert interp.pending_input is not None
    assert interp.pending_input.var_name == "N"


# -- regressions -------------------------------------------------------------


def test_ask_answer_reaches_the_program() -> None:
    """The value ASK reads is visible to the statements that follow it.

    ASK recorded the answer in a private dictionary, so the prompt appeared and
    the answer went nowhere: `T N` printed 0.00 whatever was typed.
    """
    output = run('01.10 ASK "How many? " N\n01.20 T "You said ",N', FOCAL, input_val="42")
    assert has(output, "You said 42.00")
    assert no_errors(output)


def test_fpi_and_fran_need_no_parentheses() -> None:
    """FOCAL writes `T FPI`, not `T FPI()`.

    Requiring the parentheses made the documented form fall through to the
    variable lookup, where an unset name is 0, and print 0.00.
    """
    output = run("01.10 T FPI", FOCAL)
    assert has(output, "3.14")
    assert no_errors(output)

    output = run("01.10 T FRAN", FOCAL)
    # A fraction of [0, 1) at the default two decimals, so the integer part is
    # a single zero rather than a value in the range.
    assert has(output, "0.")
    assert no_errors(output)


def test_fpi_rejects_an_argument() -> None:
    assert first_error(run("01.10 T FPI(1)", FOCAL)) is not None


def test_ask_accepts_a_comma_before_the_name() -> None:
    """Every other FOCAL statement separates its items with a comma, so
    `ASK "How many? ", N` is the form a TYPE habit produces."""
    spaced = run('01.10 ASK "How many? " N\n01.20 T "You said ",N', FOCAL, input_val="42")
    comma = run('01.10 ASK "How many? ", N\n01.20 T "You said ",N', FOCAL, input_val="42")
    assert has(comma, "You said 42.00")
    assert no_errors(comma)
    assert comma == spaced
