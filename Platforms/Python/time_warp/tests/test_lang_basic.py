"""Comprehensive tests for the BASIC executor."""

from __future__ import annotations

import pytest
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
        '30 PRINT A + B\n'
        '40 PRINT A - B\n'
        '50 PRINT A * B\n'
        '60 PRINT A MOD B\n'
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
    out = bas("10 LET X = 5\n20 IF X > 3 THEN PRINT \"YES\"")
    assert no_errors(out)
    assert has(out, "YES")


def test_if_else():
    out = bas(
        "10 LET X = 1\n"
        "20 IF X > 5 THEN PRINT \"BIG\" ELSE PRINT \"SMALL\""
    )
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


def test_goto():
    out = bas(
        "10 PRINT \"START\"\n"
        "20 GOTO 40\n"
        "30 PRINT \"SKIPPED\"\n"
        "40 PRINT \"END\""
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
        "10 PRINT \"BEFORE\"\n"
        "20 GOSUB 100\n"
        "30 PRINT \"AFTER\"\n"
        "40 END\n"
        "100 PRINT \"INSIDE\"\n"
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


# ---------------------------------------------------------------------------
# Hardware simulator commands
# ---------------------------------------------------------------------------


def test_hardware_device_add():
    out = bas("10 HARDWARE DEVICE ADD led1 LED PIN 13")
    assert no_errors(out)
    assert has(out, "led1")


def test_hardware_pin_set_get():
    out = bas(
        "10 HARDWARE DEVICE ADD pin1 LED PIN 5\n"
        "20 HARDWARE PIN SET pin1 1\n"
        "30 HARDWARE PIN GET pin1"
    )
    assert no_errors(out)
    assert has(out, "pin1")


def test_hardware_sensor_read():
    from time_warp.features.hardware_simulator import SensorType
    out = bas(
        "10 HARDWARE DEVICE ADD temp1 TEMPERATURE_SENSOR PIN 0\n"
        "20 HARDWARE SENSOR READ temp1"
    )
    assert no_errors(out)
    assert has(out, "temp1")


def test_hardware_status():
    out = bas(
        "10 HARDWARE DEVICE ADD d1 LED PIN 1\n"
        "20 HARDWARE STATUS"
    )
    assert no_errors(out)
    assert has(out, "d1")


def test_while_wend():
    out = bas(
        "10 LET I=1\n"
        "20 WHILE I<=3\n"
        "30 PRINT I\n"
        "40 LET I=I+1\n"
        "50 WEND"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_read_data():
    out = bas(
        "10 READ X\n"
        "20 READ Y\n"
        "30 PRINT X+Y\n"
        "40 DATA 5,3"
    )
    assert no_errors(out)
    assert has(out, "8")


def test_dim_array():
    out = bas(
        "10 DIM A(5)\n"
        "20 LET A(1)=10\n"
        "30 LET A(2)=20\n"
        "40 PRINT A(1)+A(2)"
    )
    assert no_errors(out)
    assert has(out, "30")


def test_mid_string():
    out = bas("10 PRINT MID$(\"HELLO\", 2, 3)\n20 END")
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
