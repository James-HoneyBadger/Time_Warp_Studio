"""Comprehensive tests for the Tcl executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.TCL


def tcl(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = tcl('puts "Hello, World!"')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_variable_substitution():
    out = tcl('set name "World"\nputs "Hello, $name!"')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_puts_number():
    out = tcl("puts 42")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Variables and arithmetic
# ---------------------------------------------------------------------------


def test_set_variable():
    out = tcl("set x 42\nputs $x")
    assert no_errors(out)
    assert has(out, "42")


def test_expr_arithmetic():
    out = tcl(
        "set a 10\n"
        "set b 3\n"
        "puts [expr {$a + $b}]\n"
        "puts [expr {$a * $b}]\n"
        "puts [expr {$a % $b}]\n"
    )
    assert no_errors(out)
    assert has(out, "13", "30", "1")


def test_incr():
    out = tcl("set i 0\nincr i\nincr i\nincr i\nputs $i")
    assert no_errors(out)
    assert has(out, "3")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_else():
    out = tcl(
        "set x 7\n"
        'if {$x > 5} { puts "BIG" } else { puts "SMALL" }'
    )
    assert no_errors(out)
    assert has(out, "BIG")


def test_for_loop():
    out = tcl("for {set i 1} {$i <= 3} {incr i} { puts $i }")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_while_loop():
    out = tcl(
        "set i 1\n"
        "while {$i <= 3} {\n"
        "  puts $i\n"
        "  incr i\n"
        "}"
    )
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_foreach():
    out = tcl("foreach item {alpha beta gamma} { puts $item }")
    assert no_errors(out)
    assert has(out, "alpha", "beta", "gamma")


# ---------------------------------------------------------------------------
# Procedures
# ---------------------------------------------------------------------------


def test_proc_def():
    out = tcl(
        "proc square {n} { return [expr {$n * $n}] }\n"
        "puts [square 7]"
    )
    assert no_errors(out)
    assert has(out, "49")


# ---------------------------------------------------------------------------
# Lists
# ---------------------------------------------------------------------------


def test_list_length():
    out = tcl("set lst {a b c d}\nputs [llength $lst]")
    assert no_errors(out)
    assert has(out, "4")


def test_lindex():
    out = tcl("set lst {10 20 30}\nputs [lindex $lst 1]")
    assert no_errors(out)
    assert has(out, "20")


# ---------------------------------------------------------------------------
# String operations
# ---------------------------------------------------------------------------


def test_string_length():
    out = tcl('puts [string length "Hello"]')
    assert no_errors(out)
    assert has(out, "5")


def test_string_upper():
    out = tcl('puts [string toupper "hello"]')
    assert no_errors(out)
    assert has(out, "HELLO")


def test_string_range():
    out = tcl('puts [string range "hello world" 0 4]')
    assert no_errors(out)
    assert has(out, "hello")


def test_expr_power():
    out = tcl('puts [expr {3 ** 4}]')
    assert no_errors(out)
    assert has(out, "81")


def test_proc_return_value():
    out = tcl(
        'proc add {a b} { return [expr {$a + $b}] }\n'
        'puts [add 3 7]'
    )
    assert no_errors(out)
    assert has(out, "10")


def test_string_length():
    out = tcl('puts [string length "hello world"]')
    assert no_errors(out)
    assert has(out, "11")


def test_list_length():
    out = tcl("set L {a b c d}\nputs [llength $L]")
    assert no_errors(out)
    assert has(out, "4")


def test_lindex():
    out = tcl("set L {10 20 30 40}\nputs [lindex $L 2]")
    assert no_errors(out)
    assert has(out, "30")


def test_string_index():
    out = tcl('set s "hello world"\nputs [string index $s 0]')
    assert no_errors(out)
    assert has(out, "h")


def test_expr_multiplication():
    out = tcl("puts [expr 6 * 7]")
    assert no_errors(out)
    assert has(out, "42")


def test_if_then_else():
    out = tcl("set x 10\nif {$x > 5} {\n  puts \"big\"\n} else {\n  puts \"small\"\n}")
    assert no_errors(out)
    assert has(out, "big")


def test_set_and_puts():
    out = tcl("set x 42\nputs $x")
    assert no_errors(out)
    assert has(out, "42")


def test_expr_addition():
    out = tcl("set x 5\nset y 3\nputs [expr $x + $y]")
    assert no_errors(out)
    assert has(out, "8")


def test_string_length():
    out = tcl('set s "hello"\nputs [string length $s]')
    assert no_errors(out)
    assert has(out, "5")


def test_string_toupper():
    out = tcl("puts [string toupper \"hello\"]")
    assert no_errors(out)
    assert has(out, "HELLO")


def test_lappend_list():
    out = tcl("set lst {}\nlappend lst 1 2 3\nputs $lst")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_string_index():
    out = tcl('set s "hello"\nputs [string index $s 0]')
    assert no_errors(out)
    assert has(out, "h")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "tcl" / "hello.tcl"
    ).read_text()
    out = tcl(src)
    assert no_errors(out)
    assert ok(out)
