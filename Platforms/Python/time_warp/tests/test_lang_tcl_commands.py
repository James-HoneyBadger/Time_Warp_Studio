"""Targeted command coverage and regressions for Tcl executor."""

from __future__ import annotations

from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors

LANG = Language.TCL


def tcl(source: str) -> list[str]:
    return run(source, LANG)


def test_expr_not_equal_operator():
    out = tcl("set x 5\nif {$x != 0} { puts ok }")
    assert no_errors(out)
    assert has(out, "ok")


def test_lsort_integer_and_decreasing_options():
    out = tcl(
        "set data {64 25 12 90 3}\n"
        "puts [lsort -integer $data]\n"
        "puts [lsort -integer -decreasing $data]"
    )
    assert no_errors(out)
    assert has(out, "3 12 25 64 90")
    assert has(out, "90 64 25 12 3")


def test_lrepeat_lassign_lset_round_trip():
    out = tcl(
        "set xs [lrepeat 4 0]\n"
        "lset xs 1 5\n"
        "lset xs 3 9\n"
        "lassign $xs a b c d\n"
        "puts $xs\n"
        'puts "$a,$b,$c,$d"'
    )
    assert no_errors(out)
    assert has(out, "0 5 0 9")
    assert has(out, "0,5,0,9")


def test_if_elseif_else_multiline_chain():
    out = tcl(
        "set x 2\n"
        "if {$x == 1} { puts one }\n"
        "elseif {$x == 2} { puts two }\n"
        "else { puts other }"
    )
    assert no_errors(out)
    assert has(out, "two")


def test_switch_catch_array_info_commands():
    out = tcl(
        "array set scores {alice 10 bob 20}\n"
        "puts [array exists SCORES]\n"
        "puts [array size SCORES]\n"
        "set who bob\n"
        "switch $who { alice {puts A} bob {puts B} default {puts Z} }\n"
        "set rc [catch {error boom} msg]\n"
        "puts $rc\n"
        "puts $msg\n"
        "puts [info exists who]"
    )
    assert no_errors(out)
    assert has(out, "1")
    assert has(out, "2")
    assert has(out, "B")
    assert has(out, "boom")


def test_gcd_while_uses_not_equal_condition():
    out = tcl(
        "proc gcd {a b} {\n"
        "    while {$b != 0} {\n"
        "        set t $b\n"
        "        set b [expr {$a % $b}]\n"
        "        set a $t\n"
        "    }\n"
        "    return $a\n"
        "}\n"
        "puts [gcd 48 18]"
    )
    assert no_errors(out)
    assert has(out, "6")
