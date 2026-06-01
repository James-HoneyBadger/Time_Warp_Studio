"""Tests for the APL language executor."""
from __future__ import annotations
import pytest
from .conftest_lang import run, ok, has, no_errors
from ..core.interpreter import Language

APL = Language.APL


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------

def test_print_number():
    out = run("⎕← 42", APL)
    assert no_errors(out) and has(out, "42")


def test_print_string():
    out = run("⎕← 'Hello, World!'", APL)
    assert no_errors(out) and has(out, "Hello, World!")


def test_bare_number_prints():
    out = run("42", APL)
    assert no_errors(out) and has(out, "42")


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------

def test_variable_assignment():
    out = run("x ← 42\n⎕← x", APL)
    assert no_errors(out) and has(out, "42")


def test_variable_used_in_expr():
    out = run("x ← 10\n⎕← x + 5", APL)
    assert no_errors(out) and has(out, "15")


# ---------------------------------------------------------------------------
# Arithmetic
# ---------------------------------------------------------------------------

def test_addition():
    out = run("⎕← 3 + 4", APL)
    assert no_errors(out) and has(out, "7")


def test_subtraction():
    out = run("⎕← 10 - 3", APL)
    assert no_errors(out) and has(out, "7")


def test_multiplication():
    out = run("⎕← 6 × 7", APL)
    assert no_errors(out) and has(out, "42")


def test_division():
    out = run("⎕← 10 ÷ 2", APL)
    assert no_errors(out) and has(out, "5")


def test_power():
    out = run("⎕← 2 * 8", APL)
    assert no_errors(out) and has(out, "256")


def test_negate():
    out = run("⎕← -5", APL)
    assert no_errors(out) and has(out, "¯5") or has(out, "-5")


# ---------------------------------------------------------------------------
# Iota and shape
# ---------------------------------------------------------------------------

def test_iota():
    out = run("⎕← ⍳5", APL)
    assert no_errors(out) and has(out, "1 2 3 4 5")


def test_iota_zero_origin():
    out = run("⎕IO ← 0\n⎕← ⍳5", APL)
    assert no_errors(out) and has(out, "0 1 2 3 4")


def test_shape_vector():
    out = run("⎕← ⍴ ⍳5", APL)
    assert no_errors(out) and has(out, "5")


def test_reshape():
    out = run("⎕← 2 3 ⍴ ⍳6", APL)
    assert no_errors(out) and has(out, "1 2 3")


# ---------------------------------------------------------------------------
# Reduction
# ---------------------------------------------------------------------------

def test_sum_reduce():
    out = run("⎕← +/ ⍳5", APL)
    assert no_errors(out) and has(out, "15")


def test_product_reduce():
    out = run("⎕← ×/ ⍳5", APL)
    assert no_errors(out) and has(out, "120")


def test_max_reduce():
    out = run("⎕← ⌈/ 3 1 4 1 5 9 2 6", APL)
    assert no_errors(out) and has(out, "9")


def test_min_reduce():
    out = run("⎕← ⌊/ 3 1 4 1 5", APL)
    assert no_errors(out) and has(out, "1")


# ---------------------------------------------------------------------------
# Scan
# ---------------------------------------------------------------------------

def test_plus_scan():
    out = run("⎕← +\\ ⍳4", APL)
    assert no_errors(out) and has(out, "1 3 6 10")


# ---------------------------------------------------------------------------
# Comparison / logical
# ---------------------------------------------------------------------------

def test_comparison_equal():
    out = run("⎕← 3 = 3", APL)
    assert no_errors(out) and has(out, "1")


def test_comparison_less():
    out = run("⎕← (⍳5) < 3", APL)
    assert no_errors(out) and has(out, "1 1 0 0 0")


def test_logical_and():
    out = run("⎕← 1 ∧ 1", APL)
    assert no_errors(out) and has(out, "1")


def test_logical_not():
    out = run("⎕← ~1", APL)
    assert no_errors(out) and has(out, "0")


# ---------------------------------------------------------------------------
# Vector operations
# ---------------------------------------------------------------------------

def test_catenate():
    out = run("⎕← 1 2 3 , 4 5 6", APL)
    assert no_errors(out) and has(out, "1 2 3 4 5 6")


def test_take():
    out = run("⎕← 3 ↑ ⍳10", APL)
    assert no_errors(out) and has(out, "1 2 3")


def test_drop():
    out = run("⎕← 3 ↓ ⍳5", APL)
    assert no_errors(out) and has(out, "4 5")


def test_reverse():
    out = run("⎕← ⌽ ⍳5", APL)
    assert no_errors(out) and has(out, "5 4 3 2 1")


def test_grade_up():
    out = run("⎕← ⍋ 3 1 4 1 5", APL)
    assert no_errors(out) and has(out, "2")  # first min at position 2


def test_member():
    out = run("⎕← 3 ∊ 1 2 3 4 5", APL)
    assert no_errors(out) and has(out, "1")


# ---------------------------------------------------------------------------
# Monadic functions
# ---------------------------------------------------------------------------

def test_ceiling():
    out = run("⎕← ⌈ 3.2", APL)
    assert no_errors(out) and has(out, "4")


def test_floor():
    out = run("⎕← ⌊ 3.8", APL)
    assert no_errors(out) and has(out, "3")


def test_abs():
    out = run("v ← 3 ¯2 1 ¯4\n⎕← |v", APL)
    assert no_errors(out) and has(out, "3 2 1 4")


def test_sign():
    out = run("⎕← × 5", APL)
    assert no_errors(out) and has(out, "1")


# ---------------------------------------------------------------------------
# Inner product
# ---------------------------------------------------------------------------

def test_inner_product():
    out = run("⎕← 1 2 3 +.× 4 5 6", APL)
    assert no_errors(out) and has(out, "32")


# ---------------------------------------------------------------------------
# System variable
# ---------------------------------------------------------------------------

def test_set_io():
    out = run("⎕IO ← 0\n⎕← ⎕IO", APL)
    assert no_errors(out) and has(out, "0")
