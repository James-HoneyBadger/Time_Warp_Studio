"""Tests for the Smalltalk language executor."""
from __future__ import annotations
import pytest
from .conftest_lang import run, ok, has, no_errors
from ..core.interpreter import Language

ST = Language.SMALLTALK


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------

def test_hello_world():
    out = run("Transcript showCr: 'Hello, World!'.", ST)
    assert no_errors(out) and has(out, "Hello, World!")


def test_show_nl():
    out = run("Transcript show: 'Hello'; nl.", ST)
    assert no_errors(out) and has(out, "Hello")


def test_print_number():
    out = run("Transcript showCr: 42 printString.", ST)
    assert no_errors(out) and has(out, "42")


def test_print_nl():
    out = run("'Hello' printNl.", ST)
    assert no_errors(out) and has(out, "Hello")


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------

def test_variable_assignment():
    out = run("| x |\nx := 42.\nTranscript showCr: x printString.", ST)
    assert no_errors(out) and has(out, "42")


def test_multiple_variables():
    out = run("| a b |\na := 10.\nb := 20.\nTranscript showCr: (a + b) printString.", ST)
    assert no_errors(out) and has(out, "30")


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------

def test_addition():
    out = run("Transcript showCr: (3 + 4) printString.", ST)
    assert no_errors(out) and has(out, "7")


def test_subtraction():
    out = run("Transcript showCr: (10 - 3) printString.", ST)
    assert no_errors(out) and has(out, "7")


def test_multiplication():
    out = run("Transcript showCr: (6 * 7) printString.", ST)
    assert no_errors(out) and has(out, "42")


def test_division():
    out = run("Transcript showCr: (10 / 2) printString.", ST)
    assert no_errors(out) and has(out, "5")


def test_integer_division():
    out = run("Transcript showCr: (10 // 3) printString.", ST)
    assert no_errors(out) and has(out, "3")


def test_modulo():
    out = run(r"Transcript showCr: (10 \\ 3) printString.", ST)
    assert no_errors(out) and has(out, "1")


def test_factorial():
    out = run("Transcript showCr: 5 factorial printString.", ST)
    assert no_errors(out) and has(out, "120")


def test_sqrt():
    out = run("Transcript showCr: 16 sqrt printString.", ST)
    assert no_errors(out) and has(out, "4")


# ---------------------------------------------------------------------------
# String operations
# ---------------------------------------------------------------------------

def test_string_concat():
    out = run("Transcript showCr: ('Hello' , ' World').", ST)
    assert no_errors(out) and has(out, "Hello World")


def test_string_size():
    out = run("Transcript showCr: 'Hello' size printString.", ST)
    assert no_errors(out) and has(out, "5")


def test_string_reversed():
    out = run("Transcript showCr: 'abc' reversed.", ST)
    assert no_errors(out) and has(out, "cba")


def test_string_uppercase():
    out = run("Transcript showCr: 'hello' asUppercase.", ST)
    assert no_errors(out) and has(out, "HELLO")


def test_string_lowercase():
    out = run("Transcript showCr: 'HELLO' asLowercase.", ST)
    assert no_errors(out) and has(out, "hello")


def test_copy_from_to():
    out = run("Transcript showCr: ('Hello World' copyFrom: 7 to: 11).", ST)
    assert no_errors(out) and has(out, "World")


# ---------------------------------------------------------------------------
# Boolean / conditionals
# ---------------------------------------------------------------------------

def test_if_true():
    out = run("(3 > 2) ifTrue: [Transcript showCr: 'yes'].", ST)
    assert no_errors(out) and has(out, "yes")


def test_if_false():
    out = run("(1 > 2) ifFalse: [Transcript showCr: 'no'].", ST)
    assert no_errors(out) and has(out, "no")


def test_if_true_if_false():
    out = run("(5 > 3) ifTrue: ['yes'] ifFalse: ['no'].", ST)
    assert no_errors(out)


def test_comparison_equal():
    out = run("Transcript showCr: (3 = 3) printString.", ST)
    assert no_errors(out) and has(out, "true")


def test_comparison_not_equal():
    out = run("Transcript showCr: (3 ~= 4) printString.", ST)
    assert no_errors(out) and has(out, "true")


# ---------------------------------------------------------------------------
# Loops
# ---------------------------------------------------------------------------

def test_times_repeat():
    out = run("| count |\ncount := 0.\n3 timesRepeat: [count := count + 1].\nTranscript showCr: count printString.", ST)
    assert no_errors(out) and has(out, "3")


def test_to_do():
    out = run("1 to: 5 do: [:i | Transcript showCr: i printString].", ST)
    assert no_errors(out) and has(out, "1", "5")


def test_to_by_do():
    out = run("1 to: 10 by: 2 do: [:i | Transcript show: i printString; show: ' '].\nTranscript nl.", ST)
    assert no_errors(out) and has(out, "1")


def test_while_true():
    out = run("| x |\nx := 1.\n[x <= 3] whileTrue: [\n  Transcript showCr: x printString.\n  x := x + 1.\n].", ST)
    assert no_errors(out) and has(out, "1", "3")


# ---------------------------------------------------------------------------
# Collections
# ---------------------------------------------------------------------------

def test_array_literal():
    out = run("| a |\na := #(10 20 30).\nTranscript showCr: (a at: 2) printString.", ST)
    assert no_errors(out) and has(out, "20")


def test_array_size():
    out = run("Transcript showCr: #(1 2 3 4 5) size printString.", ST)
    assert no_errors(out) and has(out, "5")


def test_ordered_collection_add():
    out = run("| c |\nc := OrderedCollection new.\nc add: 10.\nc add: 20.\nTranscript showCr: c size printString.", ST)
    assert no_errors(out) and has(out, "2")


def test_ordered_collection_do():
    out = run("| c |\nc := OrderedCollection new.\nc add: 1.\nc add: 2.\nc add: 3.\nc do: [:each | Transcript showCr: each printString].", ST)
    assert no_errors(out) and has(out, "1", "2", "3")


def test_collect():
    out = run("| result |\nresult := #(1 2 3) collect: [:x | x * 2].\nresult do: [:each | Transcript showCr: each printString].", ST)
    assert no_errors(out) and has(out, "2", "4", "6")


def test_select():
    out = run("| result |\nresult := #(1 2 3 4 5) select: [:x | x > 3].\nTranscript showCr: result size printString.", ST)
    assert no_errors(out) and has(out, "2")


def test_inject_into():
    out = run("| sum |\nsum := #(1 2 3 4 5) inject: 0 into: [:acc :each | acc + each].\nTranscript showCr: sum printString.", ST)
    assert no_errors(out) and has(out, "15")


# ---------------------------------------------------------------------------
# Number messages
# ---------------------------------------------------------------------------

def test_abs():
    out = run("Transcript showCr: -42 abs printString.", ST)
    assert no_errors(out) and has(out, "42")


def test_max_min():
    out = run("Transcript showCr: (3 max: 7) printString.\nTranscript showCr: (3 min: 7) printString.", ST)
    assert no_errors(out) and has(out, "7", "3")
