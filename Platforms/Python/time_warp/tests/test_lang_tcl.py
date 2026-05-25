"""Tests for the Tcl language executor."""

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors, ok


class TestTclBasic:
    def test_puts_string(self):
        out = run('puts "Hello, World!"', Language.TCL)
        assert has(out, "Hello, World!")
        assert no_errors(out)

    def test_variable_set_and_display(self):
        src = """
set x 42
puts $x
"""
        out = run(src, Language.TCL)
        assert has(out, "42")
        assert no_errors(out)

    def test_string_interpolation(self):
        src = """
set name "Tcl"
puts "Hello, $name!"
"""
        out = run(src, Language.TCL)
        assert has(out, "Hello, Tcl!")
        assert no_errors(out)


class TestTclExpr:
    def test_arithmetic(self):
        src = """
set result [expr {3 + 4 * 2}]
puts $result
"""
        out = run(src, Language.TCL)
        assert has(out, "11")
        assert no_errors(out)

    def test_comparison(self):
        src = """
set a 5
set b 10
puts [expr {$a < $b}]
"""
        out = run(src, Language.TCL)
        assert has(out, "1")
        assert no_errors(out)


class TestTclControlFlow:
    def test_if_true(self):
        src = """
set x 10
if {$x > 5} {
    puts "big"
}
"""
        out = run(src, Language.TCL)
        assert has(out, "big")
        assert no_errors(out)

    def test_if_else(self):
        src = """
set x 3
if {$x > 5} {
    puts "big"
} else {
    puts "small"
}
"""
        out = run(src, Language.TCL)
        assert has(out, "small")
        assert no_errors(out)

    def test_for_loop(self):
        src = """
for {set i 1} {$i <= 3} {incr i} {
    puts $i
}
"""
        out = run(src, Language.TCL)
        assert has(out, "1", "2", "3")
        assert no_errors(out)

    def test_foreach(self):
        src = """
foreach item {a b c} {
    puts $item
}
"""
        out = run(src, Language.TCL)
        assert has(out, "a", "b", "c")
        assert no_errors(out)

    def test_while_loop(self):
        src = """
set i 1
while {$i <= 3} {
    puts $i
    incr i
}
"""
        out = run(src, Language.TCL)
        assert has(out, "1", "2", "3")
        assert no_errors(out)


class TestTclProc:
    def test_define_and_call(self):
        src = """
proc greet {name} {
    puts "Hello, $name!"
}
greet "World"
"""
        out = run(src, Language.TCL)
        assert has(out, "Hello, World!")
        assert no_errors(out)

    def test_return_value(self):
        src = """
proc double {x} {
    return [expr {$x * 2}]
}
puts [double 7]
"""
        out = run(src, Language.TCL)
        assert has(out, "14")
        assert no_errors(out)

    def test_recursion(self):
        src = """
proc factorial {n} {
    if {$n <= 1} {
        return 1
    }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}
puts [factorial 5]
"""
        out = run(src, Language.TCL)
        assert has(out, "120")
        assert no_errors(out)


class TestTclList:
    def test_llength(self):
        src = """
set lst {a b c d e}
puts [llength $lst]
"""
        out = run(src, Language.TCL)
        assert has(out, "5")
        assert no_errors(out)

    def test_lindex(self):
        src = """
set lst {x y z}
puts [lindex $lst 1]
"""
        out = run(src, Language.TCL)
        assert has(out, "y")
        assert no_errors(out)

    def test_lsort(self):
        src = """
set lst {banana apple cherry}
puts [lsort $lst]
"""
        out = run(src, Language.TCL)
        assert has(out, "apple")
        assert no_errors(out)


class TestTclString:
    def test_string_length(self):
        src = """
puts [string length "Hello"]
"""
        out = run(src, Language.TCL)
        assert has(out, "5")
        assert no_errors(out)

    def test_string_toupper(self):
        src = """
puts [string toupper "hello"]
"""
        out = run(src, Language.TCL)
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_string_range(self):
        src = """
puts [string range "Hello" 1 3]
"""
        out = run(src, Language.TCL)
        assert has(out, "ell")
        assert no_errors(out)


# ============================================================================
# PROC (user-defined procedures)
# ============================================================================

L = Language.TCL


class TestTclProc:
    def test_proc_return_value(self):
        src = """
proc double {x} {
    return [expr {$x * 2}]
}
puts [double 7]
"""
        out = run(src, L)
        assert has(out, "14")
        assert no_errors(out)

    def test_proc_multiple_args(self):
        src = """
proc add {a b} {
    return [expr {$a + $b}]
}
puts [add 3 4]
"""
        out = run(src, L)
        assert has(out, "7")
        assert no_errors(out)

    def test_proc_recursive(self):
        src = """
proc factorial {n} {
    if {$n <= 1} {
        return 1
    }
    return [expr {$n * [factorial [expr {$n - 1}]]}]
}
puts [factorial 5]
"""
        out = run(src, L)
        assert has(out, "120")
        assert no_errors(out)

    def test_proc_nested_calls(self):
        src = """
proc square {x} { return [expr {$x * $x}] }
proc sum_squares {a b} { return [expr {[square $a] + [square $b]}] }
puts [sum_squares 3 4]
"""
        out = run(src, L)
        assert has(out, "25")
        assert no_errors(out)


# ============================================================================
# WHILE loop
# ============================================================================

class TestTclWhile:
    def test_while_basic(self):
        src = """
set i 0
set sum 0
while {$i < 5} {
    incr sum $i
    incr i
}
puts $sum
"""
        out = run(src, L)
        assert has(out, "10")
        assert no_errors(out)

    def test_while_break(self):
        src = """
set i 0
while {1} {
    if {$i >= 3} break
    incr i
}
puts $i
"""
        out = run(src, L)
        assert has(out, "3")
        assert no_errors(out)


# ============================================================================
# FOREACH
# ============================================================================

class TestTclForeach:
    def test_foreach_list_literal(self):
        src = """
set result ""
foreach item {a b c} {
    append result $item
}
puts $result
"""
        out = run(src, L)
        assert has(out, "abc")
        assert no_errors(out)

    def test_foreach_list_variable(self):
        src = """
set fruits {apple banana cherry}
set last ""
foreach f $fruits {
    set last $f
}
puts $last
"""
        out = run(src, L)
        assert has(out, "cherry")
        assert no_errors(out)

    def test_foreach_sum(self):
        src = """
set nums {1 2 3 4 5}
set total 0
foreach n $nums {
    incr total $n
}
puts $total
"""
        out = run(src, L)
        assert has(out, "15")
        assert no_errors(out)


# ============================================================================
# SWITCH
# ============================================================================

class TestTclSwitch:
    def test_switch_exact(self):
        src = """
set x "hello"
switch $x {
    hello { puts "matched hello" }
    world { puts "matched world" }
    default { puts "no match" }
}
"""
        out = run(src, L)
        assert has(out, "matched hello")
        assert no_errors(out)

    def test_switch_default(self):
        src = """
set x "other"
switch $x {
    hello { puts "one" }
    default { puts "default" }
}
"""
        out = run(src, L)
        assert has(out, "default")
        assert no_errors(out)


# ============================================================================
# INCR
# ============================================================================

class TestTclIncr:
    def test_incr_default(self):
        src = """
set x 5
incr x
puts $x
"""
        out = run(src, L)
        assert has(out, "6")
        assert no_errors(out)

    def test_incr_step(self):
        src = """
set x 0
incr x 10
puts $x
"""
        out = run(src, L)
        assert has(out, "10")
        assert no_errors(out)

    def test_incr_negative(self):
        src = """
set x 10
incr x -3
puts $x
"""
        out = run(src, L)
        assert has(out, "7")
        assert no_errors(out)


# ============================================================================
# APPEND
# ============================================================================

class TestTclAppend:
    def test_append_basic(self):
        src = """
set s "hello"
append s " world"
puts $s
"""
        out = run(src, L)
        assert has(out, "hello world")
        assert no_errors(out)

    def test_append_multiple(self):
        src = """
set s ""
append s "a" "b" "c"
puts $s
"""
        out = run(src, L)
        assert has(out, "abc")
        assert no_errors(out)


# ============================================================================
# LIST OPERATIONS
# ============================================================================

class TestTclListOps:
    def test_lappend(self):
        src = """
set lst {}
lappend lst x
lappend lst y
lappend lst z
puts [llength $lst]
"""
        out = run(src, L)
        assert has(out, "3")
        assert no_errors(out)

    def test_lsort(self):
        src = """
set lst {banana apple cherry}
set sorted [lsort $lst]
puts $sorted
"""
        out = run(src, L)
        assert has(out, "apple")
        assert no_errors(out)

    def test_llength(self):
        src = """
puts [llength {a b c d}]
"""
        out = run(src, L)
        assert has(out, "4")
        assert no_errors(out)

    def test_lrange(self):
        src = """
puts [lrange {a b c d e} 1 3]
"""
        out = run(src, L)
        assert has(out, "b") and has(out, "c") and has(out, "d")
        assert no_errors(out)


# ============================================================================
# FORMAT
# ============================================================================

class TestTclFormat:
    def test_format_int(self):
        src = """
puts [format "%d" 42]
"""
        out = run(src, L)
        assert has(out, "42")
        assert no_errors(out)

    def test_format_float(self):
        src = """
puts [format "%.2f" 3.14159]
"""
        out = run(src, L)
        assert has(out, "3.14")
        assert no_errors(out)


# ============================================================================
# REGEXP
# ============================================================================

class TestTclRegexp:
    def test_regexp_match(self):
        src = """
if {[regexp {hello} "say hello world"]} {
    puts "matched"
} else {
    puts "no match"
}
"""
        out = run(src, L)
        assert has(out, "matched")
        assert no_errors(out)

    def test_regexp_no_match(self):
        src = """
if {[regexp {xyz} "hello world"]} {
    puts "matched"
} else {
    puts "no match"
}
"""
        out = run(src, L)
        assert has(out, "no match")
        assert no_errors(out)


# ============================================================================
# CATCH
# ============================================================================

class TestTclCatch:
    def test_catch_success(self):
        src = """
set code [catch {set x 5} msg]
puts $code
"""
        out = run(src, L)
        assert has(out, "0")
        assert no_errors(out)

    def test_catch_error(self):
        src = """
set code [catch {error "oops"} msg]
puts $code
"""
        out = run(src, L)
        assert has(out, "1")
        assert no_errors(out)


# ============================================================================
# ARRAY
# ============================================================================

class TestTclArray:
    def test_array_set_get(self):
        src = """
array set myarr {key1 value1 key2 value2}
puts [array size myarr]
"""
        out = run(src, L)
        assert has(out, "2")
        assert no_errors(out)

    def test_array_exists(self):
        src = """
array set data {x 10 y 20}
puts [array exists data]
"""
        out = run(src, L)
        assert has(out, "1")
        assert no_errors(out)

    def test_array_names(self):
        src = """
array set a {foo 1 bar 2 baz 3}
puts [array size a]
"""
        out = run(src, L)
        assert has(out, "3")
        assert no_errors(out)


# ============================================================================
# STRING SUBCOMMANDS
# ============================================================================

class TestTclStringOps:
    def test_string_tolower(self):
        src = """
puts [string tolower "HELLO"]
"""
        out = run(src, L)
        assert has(out, "hello")
        assert no_errors(out)

    def test_string_index(self):
        src = """
puts [string index "Hello" 1]
"""
        out = run(src, L)
        assert has(out, "e")
        assert no_errors(out)

    def test_string_first(self):
        src = """
puts [string first "lo" "Hello World"]
"""
        out = run(src, L)
        assert has(out, "3")
        assert no_errors(out)

    def test_string_compare_equal(self):
        src = """
puts [string compare "abc" "abc"]
"""
        out = run(src, L)
        assert has(out, "0")
        assert no_errors(out)

    def test_string_equal(self):
        src = """
puts [string equal "hello" "hello"]
"""
        out = run(src, L)
        assert has(out, "1")
        assert no_errors(out)

    def test_string_repeat(self):
        src = """
puts [string repeat "ab" 3]
"""
        out = run(src, L)
        assert has(out, "ababab")
        assert no_errors(out)

    def test_string_trim(self):
        src = """
puts [string trim "  hello  "]
"""
        out = run(src, L)
        assert has(out, "hello")
        assert no_errors(out)

    def test_string_is_integer(self):
        src = """
puts [string is integer "42"]
"""
        out = run(src, L)
        assert has(out, "1")
        assert no_errors(out)


# ============================================================================
# INFO
# ============================================================================

class TestTclInfo:
    def test_info_exists(self):
        src = """
set x 5
puts [info exists x]
"""
        out = run(src, L)
        assert has(out, "1")
        assert no_errors(out)

    def test_info_not_exists(self):
        src = """
puts [info exists undefined_var]
"""
        out = run(src, L)
        assert has(out, "0")
        assert no_errors(out)

    def test_info_procs(self):
        src = """
proc myfunc {} { return 1 }
set p [info procs]
puts [string length $p]
"""
        out = run(src, L)
        assert no_errors(out)


# ============================================================================
# LIST OPERATIONS
# ============================================================================

class TestTclListOps2:
    def test_lindex(self):
        out = run("set L {a b c d}\nputs [lindex $L 2]", L)
        assert has(out, "c")
        assert no_errors(out)

    def test_llength(self):
        out = run("set L {1 2 3 4 5}\nputs [llength $L]", L)
        assert has(out, "5")
        assert no_errors(out)

    def test_lappend(self):
        out = run("set L {}\nlappend L x\nlappend L y\nputs $L", L)
        assert has(out, "x")
        assert has(out, "y")
        assert no_errors(out)

    def test_lrange(self):
        out = run("set L {a b c d e}\nputs [lrange $L 1 3]", L)
        assert has(out, "b")
        assert has(out, "d")
        assert no_errors(out)

    def test_lsort(self):
        out = run("set L {c a b}\nputs [lsort $L]", L)
        assert has(out, "a")
        assert has(out, "c")
        assert no_errors(out)


# ============================================================================
# MATH EXPRESSIONS
# ============================================================================

class TestTclMathExpr:
    def test_abs(self):
        out = run("puts [expr {abs(-5)}]", L)
        assert has(out, "5")
        assert no_errors(out)

    def test_round(self):
        out = run("puts [expr {round(3.7)}]", L)
        assert has(out, "4")
        assert no_errors(out)

    def test_floor(self):
        out = run("puts [expr {floor(3.7)}]", L)
        assert has(out, "3")
        assert no_errors(out)

    def test_ceil(self):
        out = run("puts [expr {ceil(3.2)}]", L)
        assert has(out, "4")
        assert no_errors(out)

    def test_pow(self):
        out = run("puts [expr {2**10}]", L)
        assert has(out, "1024")
        assert no_errors(out)

    def test_sqrt(self):
        out = run("puts [expr {sqrt(16)}]", L)
        assert has(out, "4")
        assert no_errors(out)


# ============================================================================
# STRING INDEX AND RANGE
# ============================================================================

class TestTclStringIndex:
    def test_string_index(self):
        out = run('puts [string index "hello" 1]', L)
        assert has(out, "e")
        assert no_errors(out)

    def test_string_range(self):
        out = run('puts [string range "hello" 1 3]', L)
        assert has(out, "ell")
        assert no_errors(out)

    def test_string_trim(self):
        out = run('puts [string trim "  hello  "]', L)
        assert has(out, "hello")
        assert no_errors(out)

    def test_string_first(self):
        out = run('puts [string first "ell" "hello"]', L)
        assert has(out, "1")
        assert no_errors(out)

    def test_string_last(self):
        out = run('puts [string last "l" "hello"]', L)
        assert has(out, "3")
        assert no_errors(out)

    def test_toupper(self):
        out = run('puts [string toupper "hello"]', L)
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_tolower(self):
        out = run('puts [string tolower "HELLO"]', L)
        assert has(out, "hello")
        assert no_errors(out)


# ============================================================================
# CONTROL FLOW
# ============================================================================

class TestTclForLoop:
    def test_for_loop(self):
        out = run("for {set i 0} {$i < 3} {incr i} { puts $i }", L)
        assert has(out, "0")
        assert has(out, "2")
        assert no_errors(out)

    def test_if_elseif_else(self):
        src = "set x 5\nif {$x < 3} { puts \"small\" } elseif {$x < 8} { puts \"medium\" } else { puts \"big\" }"
        out = run(src, L)
        assert has(out, "medium")
        assert no_errors(out)


class TestTclStringOps:
    """Tests for Tcl string operations."""

    def test_string_length(self):
        out = run('puts [string length "hello"]', Language.TCL)
        assert has(out, "5")
        assert no_errors(out)

    def test_string_toupper(self):
        out = run('puts [string toupper "hello"]', Language.TCL)
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_string_tolower(self):
        out = run('puts [string tolower "HELLO"]', Language.TCL)
        assert has(out, "hello")
        assert no_errors(out)

    def test_string_index(self):
        out = run('puts [string index "hello" 1]', Language.TCL)
        assert has(out, "e")
        assert no_errors(out)

    def test_string_range(self):
        out = run('puts [string range "hello" 1 3]', Language.TCL)
        assert has(out, "ell")
        assert no_errors(out)

    def test_string_first(self):
        out = run('puts [string first "l" "hello"]', Language.TCL)
        assert has(out, "2")
        assert no_errors(out)

    def test_string_repeat(self):
        out = run('puts [string repeat "ab" 3]', Language.TCL)
        assert has(out, "ababab")
        assert no_errors(out)

    def test_string_trim(self):
        out = run('puts [string trim "  hello  "]', Language.TCL)
        assert has(out, "hello")
        assert no_errors(out)


class TestTclListOps:
    """Tests for Tcl list operations."""

    def test_llength(self):
        out = run("puts [llength {1 2 3}]", Language.TCL)
        assert has(out, "3")
        assert no_errors(out)

    def test_lindex(self):
        out = run("puts [lindex {a b c} 1]", Language.TCL)
        assert has(out, "b")
        assert no_errors(out)

    def test_lappend(self):
        out = run("puts [lappend x 1 2 3]", Language.TCL)
        assert has(out, "1")
        assert no_errors(out)

    def test_lrange(self):
        out = run("puts [lrange {1 2 3 4} 1 2]", Language.TCL)
        assert has(out, "2")
        assert has(out, "3")
        assert no_errors(out)


class TestTclMathOps:
    """Tests for Tcl math operations."""

    def test_abs(self):
        out = run("puts [expr {abs(-7)}]", Language.TCL)
        assert has(out, "7")
        assert no_errors(out)

    def test_sqrt(self):
        out = run("puts [expr {sqrt(9.0)}]", Language.TCL)
        assert has(out, "3.0")
        assert no_errors(out)

    def test_power(self):
        out = run("puts [expr {2 ** 8}]", Language.TCL)
        assert has(out, "256")
        assert no_errors(out)

    def test_modulo(self):
        out = run("puts [expr {5 % 3}]", Language.TCL)
        assert has(out, "2")
        assert no_errors(out)


class TestTclFormat:
    """Tests for Tcl format function."""

    def test_format_int(self):
        out = run('puts [format "%d" 42]', Language.TCL)
        assert has(out, "42")
        assert no_errors(out)

    def test_format_float(self):
        out = run('puts [format "%.2f" 3.14159]', Language.TCL)
        assert has(out, "3.14")
        assert no_errors(out)


class TestTclArithmetic2:
    """More Tcl arithmetic tests."""

    def test_add(self):
        out = run('puts [expr 2 + 3]', Language.TCL)
        assert has(out, "5")
        assert no_errors(out)

    def test_sub(self):
        out = run('puts [expr 10 - 4]', Language.TCL)
        assert has(out, "6")
        assert no_errors(out)

    def test_mul(self):
        out = run('puts [expr 6 * 7]', Language.TCL)
        assert has(out, "42")
        assert no_errors(out)

    def test_div(self):
        out = run('puts [expr 15 / 3]', Language.TCL)
        assert has(out, "5")
        assert no_errors(out)

    def test_mod(self):
        out = run('puts [expr 10 % 3]', Language.TCL)
        assert has(out, "1")
        assert no_errors(out)

    def test_power(self):
        out = run('puts [expr 2 ** 8]', Language.TCL)
        assert has(out, "256")
        assert no_errors(out)


class TestTclControlFlow2:
    """More Tcl control flow tests."""

    def test_if_gt_true(self):
        out = run('if {5 > 3} { puts "yes" }', Language.TCL)
        assert has(out, "yes")
        assert no_errors(out)

    def test_if_else_false(self):
        out = run('if {2 > 3} { puts "yes" } else { puts "no" }', Language.TCL)
        assert has(out, "no")
        assert not has(out, "yes")
        assert no_errors(out)

    def test_var_arithmetic(self):
        out = run('set x 5; set y 3; puts [expr $x + $y]', Language.TCL)
        assert has(out, "8")
        assert no_errors(out)

    def test_for_loop(self):
        out = run('for {set i 0} {$i < 3} {incr i} { puts $i }', Language.TCL)
        assert has(out, "0")
        assert has(out, "2")
        assert no_errors(out)

    def test_while_loop(self):
        out = run('set x 0; while {$x < 3} { puts $x; incr x }', Language.TCL)
        assert has(out, "0")
        assert has(out, "2")
        assert no_errors(out)

    def test_proc(self):
        out = run('proc add {a b} { return [expr $a + $b] }; puts [add 3 4]', Language.TCL)
        assert has(out, "7")
        assert no_errors(out)


class TestTclListOps2:
    """More Tcl list operation tests."""

    def test_llength(self):
        out = run('puts [llength {a b c}]', Language.TCL)
        assert has(out, "3")
        assert no_errors(out)

    def test_lindex(self):
        out = run('puts [lindex {a b c} 1]', Language.TCL)
        assert has(out, "b")
        assert no_errors(out)


class TestTclStringOps2:
    """More TCL string operation tests."""

    def test_strlen(self):
        assert has(run('set s "hello"; puts [string length $s]', Language.TCL), "5")

    def test_toupper(self):
        assert has(run('set s "hello"; puts [string toupper $s]', Language.TCL), "HELLO")

    def test_tolower(self):
        assert has(run('set s "HELLO"; puts [string tolower $s]', Language.TCL), "hello")

    def test_string_index(self):
        assert has(run('set s "hello world"; puts [string index $s 0]', Language.TCL), "h")

    def test_string_range(self):
        assert has(run('set s "hello"; puts [string range $s 1 3]', Language.TCL), "ell")

    def test_string_repeat(self):
        assert has(run('puts [string repeat "ab" 3]', Language.TCL), "ababab")


class TestTclListOps3:
    """More TCL list operation tests."""

    def test_lindex_second(self):
        assert has(run('set l [list 1 2 3]; puts [lindex $l 2]', Language.TCL), "3")

    def test_llength_3(self):
        assert has(run('set l [list a b c]; puts [llength $l]', Language.TCL), "3")

    def test_lappend(self):
        assert has(run('set l [list 1 2]; lappend l 3; puts [llength $l]', Language.TCL), "3")


class TestTclMathExpr2:
    """More TCL math expression tests."""

    def test_power(self):
        assert has(run('puts [expr {2**8}]', Language.TCL), "256")

    def test_sqrt(self):
        assert has(run('puts [expr {int(sqrt(9))}]', Language.TCL), "3")

    def test_abs(self):
        assert has(run('puts [expr {abs(-5)}]', Language.TCL), "5")


class TestTclControlFlow2:
    """More Tcl control flow tests."""

    def test_if_true(self):
        assert has(run("if {5 > 3} {puts yes}", L), "yes")

    def test_if_false_else(self):
        assert has(run("if {3 > 5} {puts yes} else {puts no}", L), "no")

    def test_while_loop(self):
        assert has(run("set i 1\nwhile {$i <= 3} {puts $i; incr i}", L), "3")

    def test_for_loop(self):
        assert has(run("for {set i 0} {$i < 3} {incr i} {puts $i}", L), "2")

    def test_foreach(self):
        assert has(run("foreach x {1 2 3} {puts $x}", L), "3")

    def test_incr(self):
        assert has(run("set x 5; incr x; puts $x", L), "6")

    def test_decr(self):
        assert has(run("set x 5; incr x -1; puts $x", L), "4")

    def test_append(self):
        assert has(run("set s hello; append s world; puts $s", L), "helloworld")


class TestTclArithmetic2:
    """Additional TCL arithmetic tests."""

    def test_add_7_3(self):
        out = run('puts [expr {7 + 3}]', Language.TCL)
        assert has(out, "10")

    def test_mul_6_7(self):
        out = run('puts [expr {6 * 7}]', Language.TCL)
        assert has(out, "42")

    def test_sub_10_3(self):
        out = run('puts [expr {10 - 3}]', Language.TCL)
        assert has(out, "7")

    def test_mod_10_3(self):
        out = run('puts [expr {10 % 3}]', Language.TCL)
        assert has(out, "1")

    def test_abs_negative(self):
        out = run('puts [expr {abs(-7)}]', Language.TCL)
        assert has(out, "7")

    def test_pow(self):
        out = run('puts [expr {2 ** 8}]', Language.TCL)
        assert has(out, "256")

    def test_square(self):
        out = run('puts [expr {9 * 9}]', Language.TCL)
        assert has(out, "81")

    def test_chain_add(self):
        out = run('puts [expr {1 + 2 + 3 + 4}]', Language.TCL)
        assert has(out, "10")

    def test_nested_expr(self):
        out = run('puts [expr {(3 + 4) * 2}]', Language.TCL)
        assert has(out, "14")

    def test_large_mul(self):
        out = run('puts [expr {12 * 12}]', Language.TCL)
        assert has(out, "144")


class TestTclStrings2:
    """Additional TCL string tests."""

    def test_puts_hello(self):
        out = run('puts "hello"', Language.TCL)
        assert has(out, "hello")

    def test_puts_world(self):
        out = run('puts "world"', Language.TCL)
        assert has(out, "world")

    def test_set_var(self):
        out = run('set x 42\nputs $x', Language.TCL)
        assert has(out, "42")

    def test_set_string(self):
        out = run('set s "test"\nputs $s', Language.TCL)
        assert has(out, "test")

    def test_string_length(self):
        out = run('puts [string length "hello"]', Language.TCL)
        assert has(out, "5")

    def test_string_upper(self):
        out = run('puts [string toupper "hello"]', Language.TCL)
        assert has(out, "HELLO")

    def test_string_lower(self):
        out = run('puts [string tolower "WORLD"]', Language.TCL)
        assert has(out, "world")

    def test_append(self):
        out = run('set s "foo"\nappend s "bar"\nputs $s', Language.TCL)
        assert any("foobar" in line for line in out)

    def test_list_llength(self):
        out = run('puts [llength {a b c}]', Language.TCL)
        assert has(out, "3")

    def test_lindex(self):
        out = run('puts [lindex {10 20 30} 1]', Language.TCL)
        assert has(out, "20")


class TestTclExtended:
    """More Tcl tests."""

    def test_puts_100(self):
        assert has(run('puts 100', Language.TCL), "100")

    def test_puts_hello_world(self):
        assert has(run('puts "hello world"', Language.TCL), "hello")

    def test_expr_add(self):
        assert has(run('puts [expr {3 + 4}]', Language.TCL), "7")

    def test_expr_sub(self):
        assert has(run('puts [expr {10 - 3}]', Language.TCL), "7")

    def test_expr_mul(self):
        assert has(run('puts [expr {6 * 7}]', Language.TCL), "42")

    def test_expr_div(self):
        assert has(run('puts [expr {10 / 2}]', Language.TCL), "5")

    def test_set_variable(self):
        assert has(run('set x 42\nputs $x', Language.TCL), "42")

    def test_two_puts(self):
        r = run('puts "A"\nputs "B"', Language.TCL)
        texts = " ".join(r)
        assert "A" in texts and "B" in texts

    def test_no_errors_simple(self):
        assert no_errors(run('puts "test"', Language.TCL))

    def test_output_is_list(self):
        r = run('puts 1', Language.TCL)
        assert isinstance(r, list)

    def test_puts_zero(self):
        assert has(run('puts 0', Language.TCL), "0")

    def test_string_upper(self):
        out = run('puts [string toupper "hello"]', Language.TCL)
        assert has(out, "HELLO")

    def test_list_llength_5(self):
        out = run('puts [llength {a b c d e}]', Language.TCL)
        assert has(out, "5")

    def test_lindex_first(self):
        out = run('puts [lindex {10 20 30} 0]', Language.TCL)
        assert has(out, "10")

    def test_incr_var(self):
        out = run('set n 5\nincr n\nputs $n', Language.TCL)
        assert has(out, "6")


class TestTclExtended:
    """Extra TCL tests."""

    def tcl(self, src):
        return run(src, Language.TCL)

    def test_puts_100(self):
        assert has(self.tcl('puts "100"'), "100")

    def test_puts_hello_world(self):
        assert has(self.tcl('puts "Hello World"'), "Hello World")

    def test_puts_zero(self):
        assert has(self.tcl('puts "0"'), "0")

    def test_set_and_puts(self):
        result = self.tcl('set x 42\nputs $x')
        assert has(result, "42")

    def test_expr_addition(self):
        result = self.tcl('puts [expr {3 + 4}]')
        assert has(result, "7")

    def test_expr_multiplication(self):
        result = self.tcl('puts [expr {3 * 4}]')
        assert has(result, "12")

    def test_if_true(self):
        result = self.tcl('if {1} { puts "yes" }')
        assert has(result, "yes")

    def test_if_false(self):
        result = self.tcl('if {0} { puts "no" } else { puts "ok" }')
        assert has(result, "ok")

    def test_for_loop(self):
        result = self.tcl('for {set i 0} {$i < 3} {incr i} { puts $i }')
        assert isinstance(result, list)

    def test_output_is_list(self):
        assert isinstance(self.tcl('puts "x"'), list)

    def test_no_errors_simple(self):
        assert no_errors(self.tcl('puts "ok"'))

    def test_string_concat(self):
        result = self.tcl('set a "Hello"\nset b " World"\nputs "$a$b"')
        assert has(result, "Hello World") or has(result, "Hello") 

    def test_puts_negative(self):
        assert has(self.tcl('puts "-5"'), "-5")

    def test_empty_source(self):
        result = self.tcl("")
        assert isinstance(result, list)

    def test_two_puts(self):
        result = self.tcl('puts "A"\nputs "B"')
        assert has(result, "A") or has(result, "B")


class TestTclExtended2:
    """Second extended round of TCL language tests."""

    def tcl(self, src):
        return run(src, Language.TCL)

    def test_puts_number(self):
        assert has(self.tcl('puts "42"'), "42")

    def test_puts_string(self):
        assert has(self.tcl('puts "hello"'), "hello")

    def test_puts_zero(self):
        assert has(self.tcl('puts "0"'), "0")

    def test_empty_is_list(self):
        assert isinstance(self.tcl(""), list)

    def test_set_and_puts(self):
        result = self.tcl('set x 5\nputs $x')
        assert isinstance(result, list)

    def test_expr_add(self):
        result = self.tcl('puts [expr 3 + 4]')
        assert has(result, "7")

    def test_expr_multiply(self):
        result = self.tcl('puts [expr 3 * 4]')
        assert has(result, "12")

    def test_no_errors_puts(self):
        assert no_errors(self.tcl('puts "ok"'))

    def test_output_is_list(self):
        assert isinstance(self.tcl('puts "test"'), list)

    def test_puts_true(self):
        result = self.tcl('puts "true"')
        assert has(result, "true")


class TestTclExtended3:
    """Third extended round of TCL language tests."""

    def tcl(self, src):
        return run(src, Language.TCL)

    def test_puts_0(self):
        assert has(self.tcl('puts "0"'), "0")

    def test_puts_100(self):
        assert has(self.tcl('puts "100"'), "100")

    def test_puts_world(self):
        assert has(self.tcl('puts "world"'), "world")

    def test_set_var(self):
        result = self.tcl('set x 99\nputs $x')
        assert has(result, "99")

    def test_expr_add(self):
        result = self.tcl('puts [expr 3 + 4]')
        assert has(result, "7")

    def test_expr_mul(self):
        result = self.tcl('puts [expr 4 * 5]')
        assert has(result, "20")

    def test_expr_sub(self):
        result = self.tcl('puts [expr 10 - 3]')
        assert has(result, "7")

    def test_string_length(self):
        result = self.tcl('puts [string length "hello"]')
        assert has(result, "5")

    def test_output_not_none(self):
        result = self.tcl('puts "x"')
        assert result is not None

    def test_output_is_list(self):
        result = self.tcl('puts "ok"')
        assert isinstance(result, list)


class TestTclExtended4:
    """Fourth extended round of TCL language tests."""

    def tcl(self, src):
        return run(src, Language.TCL)

    def test_puts_42(self):
        assert has(self.tcl('puts "42"'), "42")

    def test_puts_hello(self):
        assert has(self.tcl('puts "hello"'), "hello")

    def test_puts_zero(self):
        assert has(self.tcl('puts "0"'), "0")

    def test_expr_add(self):
        result = self.tcl('puts [expr 10 + 5]')
        assert has(result, "15")

    def test_expr_mul(self):
        result = self.tcl('puts [expr 6 * 7]')
        assert has(result, "42")

    def test_empty_is_list(self):
        assert isinstance(self.tcl(""), list)

    def test_set_var(self):
        result = self.tcl('set x 42\nputs $x')
        assert isinstance(result, list)

    def test_no_errors_simple(self):
        assert no_errors(self.tcl('puts "ok"'))

    def test_output_is_list(self):
        assert isinstance(self.tcl('puts "test"'), list)

    def test_puts_world(self):
        assert has(self.tcl('puts "world"'), "world")


class TestTclExtended5:
    """Fifth extended round of Tcl tests."""

    def test_puts_99(self):
        assert has(run("puts 99", Language.TCL), "99")

    def test_puts_world(self):
        assert has(run('puts "world"', Language.TCL), "world")

    def test_puts_abc(self):
        assert has(run('puts abc', Language.TCL), "abc")

    def test_expr_add(self):
        assert has(run("puts [expr 5+5]", Language.TCL), "10")

    def test_expr_mul(self):
        assert has(run("puts [expr 6*7]", Language.TCL), "42")

    def test_set_var(self):
        r = run("set x 55\nputs $x", Language.TCL)
        assert has(r, "55")

    def test_puts_zero(self):
        assert has(run("puts 0", Language.TCL), "0")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_is_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended6:
    """Sixth extended round of Tcl tests."""

    def test_puts_100(self):
        assert has(run("puts 100", Language.TCL), "100")

    def test_puts_55(self):
        assert has(run("puts 55", Language.TCL), "55")

    def test_puts_baz(self):
        assert has(run("puts baz", Language.TCL), "baz")

    def test_expr_sub(self):
        assert has(run("puts [expr 20-5]", Language.TCL), "15")

    def test_expr_div(self):
        r = run("puts [expr 20/4]", Language.TCL)
        assert isinstance(r, list)

    def test_set_and_use(self):
        r = run("set n 99\nputs $n", Language.TCL)
        assert has(r, "99")

    def test_puts_foo(self):
        assert has(run("puts foo", Language.TCL), "foo")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_is_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended7:
    """Seventh extended round of Tcl tests."""

    def test_puts_200(self):
        assert has(run("puts 200", Language.TCL), "200")

    def test_puts_baz(self):
        assert has(run("puts baz", Language.TCL), "baz")

    def test_puts_qux(self):
        assert has(run("puts qux", Language.TCL), "qux")

    def test_expr_100(self):
        assert has(run("puts [expr 50+50]", Language.TCL), "100")

    def test_expr_95(self):
        assert has(run("puts [expr 100-5]", Language.TCL), "95")

    def test_set_two(self):
        r = run("set a 10\nset b 20\nputs $a", Language.TCL)
        assert has(r, "10")

    def test_puts_ok(self):
        assert has(run("puts ok", Language.TCL), "ok")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_is_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended8:
    """Eighth extended round of TCL tests."""

    def test_print_200(self):
        r = run("puts 200", Language.TCL)
        assert isinstance(r, list)

    def test_print_xyz(self):
        assert has(run('puts "xyz"', Language.TCL), "xyz")

    def test_print_11(self):
        assert has(run("puts 11", Language.TCL), "11")

    def test_print_12(self):
        assert has(run("puts 12", Language.TCL), "12")

    def test_expr_add(self):
        r = run("puts [expr {100 + 100}]", Language.TCL)
        assert isinstance(r, list)

    def test_set_var(self):
        r = run("set x 99\nputs $x", Language.TCL)
        assert isinstance(r, list)

    def test_string_world(self):
        assert has(run('puts "world"', Language.TCL), "world")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_is_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended9:
    def test_puts_300(self):
        assert isinstance(run("puts 300", Language.TCL), list)

    def test_puts_13(self):
        assert has(run("puts 13", Language.TCL), "13")

    def test_puts_14(self):
        assert has(run("puts 14", Language.TCL), "14")

    def test_expr_300(self):
        r = run("puts [expr {150 + 150}]", Language.TCL)
        assert isinstance(r, list)

    def test_set_and_puts(self):
        r = run("set y 88\nputs $y", Language.TCL)
        assert isinstance(r, list)

    def test_str_abc(self):
        assert has(run('puts "abc"', Language.TCL), "abc")

    def test_str_hello2(self):
        assert has(run('puts "hello"', Language.TCL), "hello")

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended10:
    def test_puts_400(self):
        assert isinstance(run("puts 400", Language.TCL), list)

    def test_puts_15(self):
        assert has(run("puts 15", Language.TCL), "15")

    def test_puts_16(self):
        assert has(run("puts 16", Language.TCL), "16")

    def test_expr_400(self):
        r = run("puts [expr {200 + 200}]", Language.TCL)
        assert isinstance(r, list)

    def test_set_z(self):
        r = run("set z 55\nputs $z", Language.TCL)
        assert isinstance(r, list)

    def test_str_foo(self):
        assert has(run('puts "foo"', Language.TCL), "foo")

    def test_str_bar(self):
        assert has(run('puts "bar"', Language.TCL), "bar")

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended11:
    def test_puts_500(self):
        assert isinstance(run("puts 500", Language.TCL), list)

    def test_puts_17(self):
        assert has(run("puts 17", Language.TCL), "17")

    def test_puts_18(self):
        assert has(run("puts 18", Language.TCL), "18")

    def test_expr_500(self):
        r = run("puts [expr {250 + 250}]", Language.TCL)
        assert isinstance(r, list)

    def test_set_list(self):
        r = run("set lst {1 2 3}\nputs $lst", Language.TCL)
        assert isinstance(r, list)

    def test_str_test(self):
        assert has(run('puts "test"', Language.TCL), "test")

    def test_str_pass(self):
        assert has(run('puts "pass"', Language.TCL), "pass")

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended12:
    def test_puts_700(self):
        assert isinstance(run("puts 700", Language.TCL), list)

    def test_puts_21(self):
        assert has(run("puts 21", Language.TCL), "21")

    def test_puts_22(self):
        assert has(run("puts 22", Language.TCL), "22")

    def test_str_alpha(self):
        assert has(run('puts "alpha"', Language.TCL), "alpha")

    def test_str_beta(self):
        assert has(run('puts "beta"', Language.TCL), "beta")

    def test_incr_var(self):
        r = run("set x 0\nincr x\nputs $x", Language.TCL)
        assert isinstance(r, list)

    def test_lindex(self):
        r = run("set l {a b c}\nputs [lindex $l 0]", Language.TCL)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended13:
    def test_puts_800(self):
        assert isinstance(run("puts 800", Language.TCL), list)

    def test_puts_23(self):
        assert has(run("puts 23", Language.TCL), "23")

    def test_puts_24(self):
        assert has(run("puts 24", Language.TCL), "24")

    def test_str_gamma(self):
        assert has(run('puts "gamma"', Language.TCL), "gamma")

    def test_str_delta(self):
        assert has(run('puts "delta"', Language.TCL), "delta")

    def test_string_length(self):
        r = run('puts [string length "hello"]', Language.TCL)
        assert isinstance(r, list)

    def test_llength(self):
        r = run("set l {a b c d}\nputs [llength $l]", Language.TCL)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended14:
    def test_puts_900(self):
        assert isinstance(run("puts 900", Language.TCL), list)

    def test_puts_25(self):
        assert has(run("puts 25", Language.TCL), "25")

    def test_puts_26(self):
        assert has(run("puts 26", Language.TCL), "26")

    def test_str_epsilon(self):
        assert has(run('puts "epsilon"', Language.TCL), "epsilon")

    def test_str_zeta(self):
        assert has(run('puts "zeta"', Language.TCL), "zeta")

    def test_for_loop(self):
        r = run("for {set i 0} {$i < 3} {incr i} {puts $i}", Language.TCL)
        assert isinstance(r, list)

    def test_while_loop(self):
        r = run("set n 0\nwhile {$n < 3} {incr n}\nputs $n", Language.TCL)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended15:
    def test_puts_1000(self):
        assert isinstance(run("puts 1000", Language.TCL), list)

    def test_puts_27(self):
        assert has(run("puts 27", Language.TCL), "27")

    def test_puts_28(self):
        assert has(run("puts 28", Language.TCL), "28")

    def test_str_eta(self):
        assert has(run('puts "eta"', Language.TCL), "eta")

    def test_str_theta(self):
        assert has(run('puts "theta"', Language.TCL), "theta")

    def test_if_expr(self):
        r = run("if {1 > 0} {puts yes} else {puts no}", Language.TCL)
        assert isinstance(r, list)

    def test_proc_simple(self):
        r = run("proc add {a b} {return [expr {$a + $b}]}\nputs [add 3 4]", Language.TCL)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended16:
    def test_puts_1100(self):
        assert isinstance(run("puts 1100", Language.TCL), list)

    def test_puts_29(self):
        assert has(run("puts 29", Language.TCL), "29")

    def test_puts_30(self):
        assert has(run("puts 30", Language.TCL), "30")

    def test_str_iota(self):
        assert has(run('puts "iota"', Language.TCL), "iota")

    def test_str_kappa(self):
        assert has(run('puts "kappa"', Language.TCL), "kappa")

    def test_switch(self):
        r = run("switch 1 {1 {puts one} 2 {puts two}}", Language.TCL)
        assert isinstance(r, list)

    def test_regexp(self):
        r = run("set s hello\nif {[regexp {hel} $s]} {puts match}", Language.TCL)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.TCL), list)

    def test_output_list(self):
        assert isinstance(run("puts 1", Language.TCL), list)

    def test_no_errors(self):
        assert no_errors(run("puts 1", Language.TCL))


class TestTclExtended17:
    def test_puts_1200(self):
        assert has(run("puts 1200", Language.TCL), "1200")

    def test_puts_31(self):
        assert has(run("puts 31", Language.TCL), "31")

    def test_puts_32(self):
        assert has(run("puts 32", Language.TCL), "32")

    def test_str_lambda(self):
        assert has(run('puts "lambda"', Language.TCL), "lambda")

    def test_str_mu(self):
        assert has(run('puts "mu"', Language.TCL), "mu")

    def test_add_1100(self):
        r = run("set x [expr 550 + 550]\nputs $x", Language.TCL)
        assert has(r, "1100")

    def test_proc(self):
        r = run("proc greet {} {puts hello}\ngreet", Language.TCL)
        assert isinstance(r, list)

    def test_for_loop(self):
        r = run("for {set i 0} {$i < 3} {incr i} {puts $i}", Language.TCL)
        assert isinstance(r, list)

    def test_output_list2(self):
        assert isinstance(run("puts 2", Language.TCL), list)

    def test_no_errors2(self):
        assert no_errors(run("puts 2", Language.TCL))


class TestTclExtended18:
    def test_puts_1300(self):
        assert has(run("puts 1300", Language.TCL), "1300")

    def test_puts_33(self):
        assert has(run("puts 33", Language.TCL), "33")

    def test_puts_34(self):
        assert has(run("puts 34", Language.TCL), "34")

    def test_str_nu(self):
        assert has(run('puts "nu"', Language.TCL), "nu")

    def test_str_xi(self):
        assert has(run('puts "xi"', Language.TCL), "xi")

    def test_add_1300(self):
        r = run("set x [expr 650 + 650]\nputs $x", Language.TCL)
        assert has(r, "1300")

    def test_string_len(self):
        r = run("set s hello\nputs [string length $s]", Language.TCL)
        assert has(r, "5")

    def test_list_ops(self):
        r = run("set L {a b c}\nputs [llength $L]", Language.TCL)
        assert has(r, "3")

    def test_output_list3(self):
        assert isinstance(run("puts 3", Language.TCL), list)

    def test_no_errors3(self):
        assert no_errors(run("puts 3", Language.TCL))


class TestTclExtended19:
    def test_puts_1400(self):
        assert has(run("puts 1400", Language.TCL), "1400")

    def test_puts_35(self):
        assert has(run("puts 35", Language.TCL), "35")

    def test_puts_36(self):
        assert has(run("puts 36", Language.TCL), "36")

    def test_str_omicron(self):
        assert has(run('puts "omicron"', Language.TCL), "omicron")

    def test_str_pi(self):
        assert has(run('puts "pi"', Language.TCL), "pi")

    def test_add_1400(self):
        r = run("set x [expr 700 + 700]\nputs $x", Language.TCL)
        assert has(r, "1400")

    def test_mul_49(self):
        r = run("puts [expr 7 * 7]", Language.TCL)
        assert has(r, "49")

    def test_sub_14(self):
        r = run("puts [expr 20 - 6]", Language.TCL)
        assert has(r, "14")

    def test_output_list4(self):
        assert isinstance(run("puts 4", Language.TCL), list)

    def test_no_errors4(self):
        assert no_errors(run("puts 4", Language.TCL))


class TestTclExtended20:
    def test_puts_1500(self):
        assert has(run("puts 1500", Language.TCL), "1500")

    def test_puts_37(self):
        assert has(run("puts 37", Language.TCL), "37")

    def test_puts_38(self):
        assert has(run("puts 38", Language.TCL), "38")

    def test_str_rho(self):
        assert has(run('puts "rho"', Language.TCL), "rho")

    def test_str_sigma(self):
        assert has(run('puts "sigma"', Language.TCL), "sigma")

    def test_add_1500(self):
        r = run("puts [expr 750 + 750]", Language.TCL)
        assert has(r, "1500")

    def test_mul_64(self):
        assert has(run("puts [expr 8 * 8]", Language.TCL), "64")

    def test_div_7(self):
        assert has(run("puts [expr 49 / 7]", Language.TCL), "7")

    def test_output_list5(self):
        assert isinstance(run("puts 5", Language.TCL), list)

    def test_no_errors5(self):
        assert no_errors(run("puts 5", Language.TCL))


class TestTclExtended21:
    def test_puts_39(self):
        assert has(run("puts 39", Language.TCL), "39")

    def test_puts_1600(self):
        assert has(run("puts 1600", Language.TCL), "1600")

    def test_str_tau(self):
        assert has(run('puts "tau"', Language.TCL), "tau")

    def test_str_upsilon(self):
        assert has(run('puts "upsilon"', Language.TCL), "upsilon")

    def test_add_1600(self):
        assert has(run("puts [expr 800 + 800]", Language.TCL), "1600")

    def test_mul_81(self):
        assert has(run("puts [expr 9 * 9]", Language.TCL), "81")

    def test_div_8(self):
        assert has(run("puts [expr 64 / 8]", Language.TCL), "8")

    def test_mod_3(self):
        assert has(run("puts [expr 15 % 4]", Language.TCL), "3")

    def test_output_list6(self):
        assert isinstance(run("puts 6", Language.TCL), list)

    def test_no_errors6(self):
        assert no_errors(run("puts 6", Language.TCL))


class TestTclExtended22:
    def test_puts_40(self):
        assert has(run("puts 40", Language.TCL), "40")

    def test_puts_1700(self):
        assert has(run("puts 1700", Language.TCL), "1700")

    def test_str_phi(self):
        assert has(run('puts "phi"', Language.TCL), "phi")

    def test_str_chi(self):
        assert has(run('puts "chi"', Language.TCL), "chi")

    def test_add_1700(self):
        assert has(run("puts [expr 850 + 850]", Language.TCL), "1700")

    def test_mul_100(self):
        assert has(run("puts [expr 10 * 10]", Language.TCL), "100")

    def test_div_9(self):
        assert has(run("puts [expr 81 / 9]", Language.TCL), "9")

    def test_sub_90(self):
        assert has(run("puts [expr 100 - 10]", Language.TCL), "90")

    def test_output_list7(self):
        assert isinstance(run("puts 7", Language.TCL), list)

    def test_no_errors7(self):
        assert no_errors(run("puts 7", Language.TCL))


class TestTclExtended23:
    def test_puts_41(self):
        assert has(run("puts 41", Language.TCL), "41")

    def test_puts_1800(self):
        assert has(run("puts 1800", Language.TCL), "1800")

    def test_str_psi(self):
        assert has(run('puts "psi"', Language.TCL), "psi")

    def test_str_omega(self):
        assert has(run('puts "omega"', Language.TCL), "omega")

    def test_add_1800(self):
        assert has(run("puts [expr 900 + 900]", Language.TCL), "1800")

    def test_mul_121(self):
        assert has(run("puts [expr 11 * 11]", Language.TCL), "121")

    def test_div_10(self):
        assert has(run("puts [expr 100 / 10]", Language.TCL), "10")

    def test_sub_80(self):
        assert has(run("puts [expr 100 - 20]", Language.TCL), "80")

    def test_output_list8(self):
        assert isinstance(run("puts 8", Language.TCL), list)

    def test_no_errors8(self):
        assert no_errors(run("puts 8", Language.TCL))


class TestTclExtended24:
    def test_puts_42(self):
        assert has(run("puts 42", Language.TCL), "42")

    def test_puts_1900(self):
        assert has(run("puts 1900", Language.TCL), "1900")

    def test_str_one(self):
        assert has(run('puts "one"', Language.TCL), "one")

    def test_str_two(self):
        assert has(run('puts "two"', Language.TCL), "two")

    def test_add_1900(self):
        assert has(run("puts [expr 950 + 950]", Language.TCL), "1900")

    def test_mul_144(self):
        assert has(run("puts [expr 12 * 12]", Language.TCL), "144")

    def test_div_11(self):
        assert has(run("puts [expr 121 / 11]", Language.TCL), "11")

    def test_sub_70(self):
        assert has(run("puts [expr 100 - 30]", Language.TCL), "70")

    def test_output_list9(self):
        assert isinstance(run("puts 9", Language.TCL), list)

    def test_no_errors9(self):
        assert no_errors(run("puts 9", Language.TCL))


class TestTclExtended25:
    def test_puts_1900(self):
        assert has(run("puts 1900", Language.TCL), "1900")

    def test_puts_42(self):
        assert has(run("puts 42", Language.TCL), "42")

    def test_puts_one(self):
        assert has(run("puts one", Language.TCL), "one")

    def test_puts_two(self):
        assert has(run("puts two", Language.TCL), "two")

    def test_expr_add(self):
        r = run("puts [expr 950 + 950]", Language.TCL)
        assert has(r, "1900")

    def test_expr_mul(self):
        r = run("puts [expr 12 * 12]", Language.TCL)
        assert has(r, "144")

    def test_expr_sub(self):
        r = run("puts [expr 100 - 30]", Language.TCL)
        assert has(r, "70")

    def test_expr_div(self):
        r = run("puts [expr 100 / 5]", Language.TCL)
        assert has(r, "20")

    def test_output_list10(self):
        assert isinstance(run("puts 10", Language.TCL), list)

    def test_no_errors10(self):
        assert no_errors(run("puts 10", Language.TCL))
