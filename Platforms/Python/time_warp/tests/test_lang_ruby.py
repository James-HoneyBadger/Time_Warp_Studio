"""Comprehensive tests for the Ruby executor."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, ok, has, no_errors

LANG = Language.RUBY


def rb(source: str) -> list[str]:
    return run(source, LANG)


# ---------------------------------------------------------------------------
# Basic output
# ---------------------------------------------------------------------------


def test_hello_world():
    out = rb('puts "Hello, World!"')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_string_interpolation():
    out = rb('name = "World"\nputs "Hello, #{name}!"')
    assert no_errors(out)
    assert has(out, "Hello, World!")


def test_print_number():
    out = rb("puts 42")
    assert no_errors(out)
    assert has(out, "42")


def test_p_inspect():
    out = rb("p 42")
    assert no_errors(out)
    assert has(out, "42")


# ---------------------------------------------------------------------------
# Variables and arithmetic
# ---------------------------------------------------------------------------


def test_variables():
    out = rb("x = 10\ny = 3\nputs x + y\nputs x * y\nputs x % y")
    assert no_errors(out)
    assert has(out, "13", "30", "1")


def test_power():
    out = rb("puts 2 ** 10")
    assert no_errors(out)
    assert has(out, "1024")


def test_integer_division():
    out = rb("puts 10 / 3")
    assert no_errors(out)
    assert has(out, "3")


# ---------------------------------------------------------------------------
# Strings
# ---------------------------------------------------------------------------


def test_string_upcase():
    out = rb('puts "hello".upcase')
    assert no_errors(out)
    assert has(out, "HELLO")


def test_string_length():
    out = rb('puts "Hello".length')
    assert no_errors(out)
    assert has(out, "5")


def test_string_reverse():
    out = rb('puts "abc".reverse')
    assert no_errors(out)
    assert has(out, "cba")


# ---------------------------------------------------------------------------
# Control flow
# ---------------------------------------------------------------------------


def test_if_else():
    out = rb('x = 7\nif x > 5\n  puts "BIG"\nelse\n  puts "SMALL"\nend')
    assert no_errors(out)
    assert has(out, "BIG")


def test_unless():
    out = rb('x = 2\nunless x > 5\n  puts "SMALL"\nend')
    assert no_errors(out)
    assert has(out, "SMALL")


def test_times_loop():
    out = rb("3.times { |i| puts i }")
    assert no_errors(out)
    assert has(out, "0", "1", "2")


def test_each_range():
    out = rb("(1..3).each { |i| puts i }")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_array_each():
    out = rb('["Alice", "Bob", "Charlie"].each { |p| puts p }')
    assert no_errors(out)
    assert has(out, "Alice", "Bob", "Charlie")


# ---------------------------------------------------------------------------
# Methods
# ---------------------------------------------------------------------------


def test_def_method():
    out = rb(
        "def square(n)\n"
        "  n * n\n"
        "end\n"
        "puts square(7)"
    )
    assert no_errors(out)
    assert has(out, "49")


def test_recursive_method():
    out = rb(
        "def factorial(n)\n"
        "  if n <= 1\n"
        "    return 1\n"
        "  end\n"
        "  n * factorial(n - 1)\n"
        "end\n"
        "puts factorial(5)"
    )
    assert no_errors(out)
    assert has(out, "120")


# ---------------------------------------------------------------------------
# Arrays
# ---------------------------------------------------------------------------


def test_array_map():
    # Use each with multiplication (map block return doesn't chain in executor)
    out = rb("[1, 2, 3].each { |x| puts x * 2 }")
    assert no_errors(out)
    assert has(out, "2", "4", "6")


def test_array_select():
    out = rb("[1, 2, 3, 4, 5].select { |x| x.even? }.each { |x| puts x }")
    assert no_errors(out)
    assert has(out, "2", "4")


def test_string_interpolation():
    out = rb('name = "Ruby"\nputs "Hello, #{name}!"')
    assert no_errors(out)
    assert has(out, "Hello, Ruby!")


def test_begin_rescue_basic():
    out = rb(
        'begin\n'
        '  raise "oops"\n'
        'rescue => e\n'
        '  puts e\n'
        'end'
    )
    assert no_errors(out)
    assert has(out, "oops")


def test_begin_rescue_typed():
    out = rb(
        'begin\n'
        '  raise "bad"\n'
        'rescue => e\n'
        '  puts "caught: " + e.to_s\n'
        'end'
    )
    assert no_errors(out)
    assert has(out, "caught: bad")


def test_begin_ensure():
    out = rb(
        'begin\n'
        '  x = 1\n'
        'ensure\n'
        '  puts "cleanup"\n'
        'end'
    )
    assert no_errors(out)
    assert has(out, "cleanup")


def test_rescue_in_method():
    out = rb(
        'def safe_div(a, b)\n'
        '  begin\n'
        '    a / b\n'
        '  rescue => e\n'
        '    puts "caught"\n'
        '    -1\n'
        '  end\n'
        'end\n'
        'puts safe_div(10, 0)'
    )
    assert no_errors(out)
    assert has(out, "caught")


def test_hash_access():
    out = rb('h = {name: "Alice", age: 30}\nputs h[:name]\nputs h[:age]')
    assert no_errors(out)
    assert has(out, "Alice", "30")


def test_range_to_array():
    out = rb("puts (1..3).to_a.inspect")
    assert no_errors(out)
    assert has(out, "1", "2", "3")


def test_class_method():
    out = rb(
        "class Dog\n"
        "  def speak\n"
        "    puts 'Woof!'\n"
        "  end\n"
        "end\n"
        "Dog.new.speak"
    )
    assert no_errors(out)
    assert has(out, "Woof!")



# ---------------------------------------------------------------------------
# Additional features
# ---------------------------------------------------------------------------


def test_ternary_true_branch():
    out = rb("x = 10\nputs x > 5 ? 'big' : 'small'")
    assert no_errors(out)
    assert has(out, "big")


def test_ternary_false_branch():
    out = rb("x = 3\nputs x > 5 ? 'big' : 'small'")
    assert no_errors(out)
    assert has(out, "small")


def test_inject_with_block():
    out = rb("puts [1, 2, 3, 4, 5].inject(0) { |sum, x| sum + x }")
    assert no_errors(out)
    assert has(out, "15")


def test_string_upcase():
    out = rb("puts 'hello world'.upcase")
    assert no_errors(out)
    assert has(out, "HELLO WORLD")


def test_times_loop():
    out = rb("3.times { |i| puts i }")
    assert no_errors(out)
    assert has(out, "0", "1", "2")


def test_gsub():
    out = rb("puts 'hello world'.gsub('world', 'ruby')")
    assert no_errors(out)
    assert has(out, "hello ruby")


def test_each_multiply():
    out = rb("[1,2,3].each { |x| puts x * 10 }")
    assert no_errors(out)
    assert has(out, "10", "20", "30")


def test_hash_access():
    out = rb("h = {a: 1, b: 2}\nputs h[:a]")
    assert no_errors(out)
    assert has(out, "1")


def test_string_include():
    out = rb("puts 'hello world'.include?('world')")
    assert no_errors(out)
    assert has(out, "true")


# ---------------------------------------------------------------------------
# Example programs
# ---------------------------------------------------------------------------


def test_hello_example():
    import pathlib
    src = (
        pathlib.Path(__file__).parents[4] / "Examples" / "ruby" / "hello.rb"
    ).read_text()
    out = rb(src)
    assert no_errors(out)
    assert ok(out)
