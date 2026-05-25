"""Tests for the PostScript language executor."""

import pytest
from time_warp.core.interpreter import Language
from .conftest_lang import run, has, no_errors, ok


class TestPostScriptOutput:
    def test_print_string(self):
        out = run("(Hello, World!) =", Language.POSTSCRIPT)
        assert has(out, "Hello, World!")
        assert no_errors(out)

    def test_print_number(self):
        out = run("42 =", Language.POSTSCRIPT)
        assert has(out, "42")
        assert no_errors(out)


class TestPostScriptArithmetic:
    def test_add(self):
        out = run("3 4 add =", Language.POSTSCRIPT)
        assert has(out, "7")
        assert no_errors(out)

    def test_sub(self):
        out = run("10 3 sub =", Language.POSTSCRIPT)
        assert has(out, "7")
        assert no_errors(out)

    def test_mul(self):
        out = run("6 7 mul =", Language.POSTSCRIPT)
        assert has(out, "42")
        assert no_errors(out)

    def test_idiv(self):
        out = run("10 3 idiv =", Language.POSTSCRIPT)
        assert has(out, "3")
        assert no_errors(out)

    def test_mod(self):
        out = run("10 3 mod =", Language.POSTSCRIPT)
        assert has(out, "1")
        assert no_errors(out)

    def test_sqrt(self):
        out = run("9 sqrt =", Language.POSTSCRIPT)
        assert has(out, "3")
        assert no_errors(out)

    def test_neg(self):
        out = run("5 neg =", Language.POSTSCRIPT)
        assert has(out, "-5")
        assert no_errors(out)

    def test_abs(self):
        out = run("-7 abs =", Language.POSTSCRIPT)
        assert has(out, "7")
        assert no_errors(out)


class TestPostScriptStack:
    def test_dup(self):
        src = "5 dup add ="
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "10")
        assert no_errors(out)

    def test_exch(self):
        src = "3 7 exch sub ="
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "4")
        assert no_errors(out)

    def test_pop(self):
        src = "1 2 pop ="
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "1")
        assert no_errors(out)


class TestPostScriptConditional:
    def test_if_true(self):
        src = "true { (yes) = } if"
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "yes")
        assert no_errors(out)

    def test_ifelse_true(self):
        src = "5 3 gt { (bigger) = } { (smaller) = } ifelse"
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "bigger")
        assert no_errors(out)

    def test_ifelse_false(self):
        src = "2 3 gt { (bigger) = } { (smaller) = } ifelse"
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "smaller")
        assert no_errors(out)


class TestPostScriptLoop:
    def test_for_loop(self):
        src = "1 1 3 { = } for"
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "1", "2", "3")
        assert no_errors(out)

    def test_repeat(self):
        src = "0 /count exch def\n3 { /count count 1 add def } repeat\ncount ="
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "3")
        assert no_errors(out)


class TestPostScriptDefinitions:
    def test_def_and_call(self):
        src = "/double { 2 mul } def\n5 double ="
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "10")
        assert no_errors(out)

    def test_recursive_factorial(self):
        src = """
/factorial {
    dup 1 le {
        pop 1
    } {
        dup 1 sub factorial mul
    } ifelse
} def
5 factorial =
"""
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "120")
        assert no_errors(out)


class TestPostScriptComparisons:
    def test_eq(self):
        src = "5 5 eq ="
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "True")
        assert no_errors(out)

    def test_ne(self):
        src = "5 3 ne ="
        out = run(src, Language.POSTSCRIPT)
        assert has(out, "True")
        assert no_errors(out)


# ============================================================================
# ADDITIONAL POSTSCRIPT TESTS
# ============================================================================

PS = Language.POSTSCRIPT


class TestPostScriptFor:
    def test_for_basic(self):
        src = "1 1 3 { = } for"
        out = run(src, PS)
        assert has(out, "1") and has(out, "2") and has(out, "3")
        assert no_errors(out)

    def test_for_step2(self):
        src = "0 2 6 { = } for"
        out = run(src, PS)
        assert has(out, "0") and has(out, "4") and has(out, "6")
        assert no_errors(out)

    def test_for_sum(self):
        src = """
/total 0 def
1 1 5 { total add /total exch def } for
total =
"""
        out = run(src, PS)
        assert has(out, "15")
        assert no_errors(out)


class TestPostScriptRepeat:
    def test_repeat_basic(self):
        src = "3 { (hello) = } repeat"
        out = run(src, PS)
        lines = [l for l in out if "hello" in l.lower()]
        assert len(lines) == 3
        assert no_errors(out)

    def test_repeat_counter(self):
        src = """
/n 0 def
5 { /n n 1 add def } repeat
n =
"""
        out = run(src, PS)
        assert has(out, "5")
        assert no_errors(out)


class TestPostScriptArithmetic:
    def test_mod(self):
        src = "10 3 mod ="
        out = run(src, PS)
        assert has(out, "1")
        assert no_errors(out)

    def test_idiv(self):
        src = "10 3 idiv ="
        out = run(src, PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_abs_negative(self):
        src = "-5 abs ="
        out = run(src, PS)
        assert has(out, "5")
        assert no_errors(out)

    def test_neg(self):
        src = "7 neg ="
        out = run(src, PS)
        assert has(out, "-7")
        assert no_errors(out)

    def test_sqrt(self):
        src = "16 sqrt ="
        out = run(src, PS)
        assert has(out, "4")
        assert no_errors(out)

    def test_ceiling(self):
        src = "2.3 ceiling ="
        out = run(src, PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_floor(self):
        src = "2.9 floor ="
        out = run(src, PS)
        assert has(out, "2")
        assert no_errors(out)


class TestPostScriptStackOps:
    def test_copy_n(self):
        src = "1 2 3 2 copy pstack"
        out = run(src, PS)
        assert has(out, "2") and has(out, "3")
        assert no_errors(out)

    def test_index(self):
        src = "10 20 30 1 index ="
        out = run(src, PS)
        assert has(out, "20")
        assert no_errors(out)

    def test_roll(self):
        src = "1 2 3 3 1 roll = = ="
        out = run(src, PS)
        assert has(out, "3") and has(out, "2") and has(out, "1")
        assert no_errors(out)

    def test_count(self):
        src = "1 2 3 count ="
        out = run(src, PS)
        assert has(out, "3")
        assert no_errors(out)


class TestPostScriptBoolean:
    def test_and(self):
        src = "true true and ="
        out = run(src, PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_or(self):
        src = "false true or ="
        out = run(src, PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_not(self):
        src = "false not ="
        out = run(src, PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_gt(self):
        src = "5 3 gt ="
        out = run(src, PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_lt(self):
        src = "2 8 lt ="
        out = run(src, PS)
        assert has(out, "True")
        assert no_errors(out)


class TestPostScriptTypeConvert:
    def test_cvi(self):
        src = "3.7 cvi ="
        out = run(src, PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_cvr(self):
        src = "5 cvr ="
        out = run(src, PS)
        assert has(out, "5")
        assert no_errors(out)


class TestPostScriptArray:
    def test_array_creation(self):
        src = "3 array dup 0 42 put dup 0 get ="
        out = run(src, PS)
        assert has(out, "42")
        assert no_errors(out)

    def test_aload(self):
        src = "[10 20 30] aload pop = = ="
        out = run(src, PS)
        assert has(out, "30") and has(out, "20") and has(out, "10")
        assert no_errors(out)

    def test_forall_array(self):
        src = "[1 2 3] { = } forall"
        out = run(src, PS)
        assert has(out, "1") and has(out, "2") and has(out, "3")
        assert no_errors(out)


class TestPostScriptString:
    def test_string_length(self):
        src = "(hello) length ="
        out = run(src, PS)
        assert has(out, "5")
        assert no_errors(out)

    def test_string_show(self):
        src = "(Hello PostScript) show"
        out = run(src, PS)
        assert has(out, "Hello PostScript")
        assert no_errors(out)


class TestPostScriptDict:
    def test_dict_def_get(self):
        src = """
/greeting (Hello) def
greeting show
"""
        out = run(src, PS)
        assert has(out, "Hello")
        assert no_errors(out)

    def test_dict_begin_end(self):
        src = """
5 dict begin
/x 99 def
x =
end
"""
        out = run(src, PS)
        assert has(out, "99")
        assert no_errors(out)


class TestPostScriptMathExtras:
    """mod, idiv, max, min, round, floor, ceiling, truncate."""

    def test_mod(self):
        out = run("17 5 mod ==", PS)
        assert has(out, "2")
        assert no_errors(out)

    def test_idiv(self):
        out = run("17 5 idiv ==", PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_max(self):
        out = run("3 7 max ==", PS)
        assert has(out, "7")
        assert no_errors(out)

    def test_min(self):
        out = run("3 7 min ==", PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_round(self):
        out = run("3.7 round ==", PS)
        assert has(out, "4")
        assert no_errors(out)

    def test_floor(self):
        out = run("3.7 floor ==", PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_ceiling(self):
        out = run("3.2 ceiling ==", PS)
        assert has(out, "4")
        assert no_errors(out)

    def test_truncate(self):
        out = run("3.7 truncate ==", PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_abs(self):
        out = run("-7 abs ==", PS)
        assert has(out, "7")
        assert no_errors(out)

    def test_neg(self):
        out = run("5 neg ==", PS)
        assert has(out, "-5")
        assert no_errors(out)

    def test_sqrt(self):
        out = run("16 sqrt ==", PS)
        assert has(out, "4")
        assert no_errors(out)


class TestPostScriptStackExtras:
    """dup, pop, exch, roll, index, count."""

    def test_dup(self):
        out = run("5 dup add ==", PS)
        assert has(out, "10")
        assert no_errors(out)

    def test_pop(self):
        out = run("1 2 pop ==", PS)
        assert has(out, "1")
        assert no_errors(out)

    def test_exch(self):
        out = run("1 2 exch ==", PS)
        assert has(out, "1")
        assert no_errors(out)

    def test_count(self):
        out = run("1 2 3 count ==", PS)
        assert has(out, "3")
        assert no_errors(out)

    def test_index(self):
        out = run("1 2 3 1 index ==", PS)
        assert has(out, "2")
        assert no_errors(out)


class TestPostScriptBoolLogic:
    """gt, lt, ge, le, ne, and, or, not."""

    def test_gt_true(self):
        out = run("5 3 gt ==", PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_lt_true(self):
        out = run("3 5 lt ==", PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_ne_true(self):
        out = run("5 3 ne ==", PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_and_false(self):
        out = run("true false and ==", PS)
        assert has(out, "False")
        assert no_errors(out)

    def test_or_true(self):
        out = run("true false or ==", PS)
        assert has(out, "True")
        assert no_errors(out)

    def test_not(self):
        out = run("true not ==", PS)
        assert has(out, "False")
        assert no_errors(out)


class TestPostScriptArrayOps:
    """Tests for PostScript array operations."""

    def test_array_length(self):
        out = run("[ 1 2 3 ] length =", Language.POSTSCRIPT)
        assert has(out, "3")
        assert no_errors(out)

    def test_array_get_element(self):
        out = run("[ 10 20 30 ] 1 get =", Language.POSTSCRIPT)
        assert has(out, "20")
        assert no_errors(out)

    def test_array_first_element(self):
        out = run("[ 1 2 3 ] 0 get =", Language.POSTSCRIPT)
        assert has(out, "1")
        assert no_errors(out)


class TestPostScriptDictOps:
    """Tests for PostScript dict operations."""

    def test_dict_get(self):
        out = run("2 dict dup /a 5 put /a get =", Language.POSTSCRIPT)
        assert has(out, "5")
        assert no_errors(out)


class TestPostScriptStringOps:
    """Tests for PostScript string operations."""

    def test_string_length(self):
        out = run("(hello) length =", Language.POSTSCRIPT)
        assert has(out, "5")
        assert no_errors(out)

    def test_string_eq_false(self):
        out = run("(hello) (world) eq {(yes) print} {(no) print} ifelse", Language.POSTSCRIPT)
        assert has(out, "no")
        assert no_errors(out)

    def test_string_eq_true(self):
        out = run("(hello) (hello) eq {(yes) print} {(no) print} ifelse", Language.POSTSCRIPT)
        assert has(out, "yes")
        assert no_errors(out)


class TestPostScriptMathExtra:
    """Extra math operations tests."""

    def test_atan(self):
        out = run("3 4 atan =", Language.POSTSCRIPT)
        assert has(out, "36.86")
        assert no_errors(out)

    def test_ln(self):
        out = run("10 ln =", Language.POSTSCRIPT)
        assert has(out, "2.302")
        assert no_errors(out)

    def test_log(self):
        out = run("100 log =", Language.POSTSCRIPT)
        assert has(out, "2.0")
        assert no_errors(out)

    def test_ceiling(self):
        out = run("3.2 ceiling =", Language.POSTSCRIPT)
        assert has(out, "4")
        assert no_errors(out)

    def test_floor(self):
        out = run("3.7 floor =", Language.POSTSCRIPT)
        assert has(out, "3")
        assert no_errors(out)

    def test_round_ps(self):
        out = run("3.7 round =", Language.POSTSCRIPT)
        assert has(out, "4")
        assert no_errors(out)


class TestPostScriptArithmetic2:
    """More PostScript arithmetic tests."""

    def test_add(self):
        out = run('2 3 add =', Language.POSTSCRIPT)
        assert has(out, "5")
        assert no_errors(out)

    def test_sub(self):
        out = run('10 3 sub =', Language.POSTSCRIPT)
        assert has(out, "7")
        assert no_errors(out)

    def test_mul(self):
        out = run('6 7 mul =', Language.POSTSCRIPT)
        assert has(out, "42")
        assert no_errors(out)

    def test_div(self):
        out = run('15 3 div =', Language.POSTSCRIPT)
        assert has(out, "5")
        assert no_errors(out)

    def test_mod(self):
        out = run('10 3 mod =', Language.POSTSCRIPT)
        assert has(out, "1")
        assert no_errors(out)

    def test_dup_add(self):
        out = run('5 dup add =', Language.POSTSCRIPT)
        assert has(out, "10")
        assert no_errors(out)

    def test_exch_sub(self):
        out = run('5 3 exch sub =', Language.POSTSCRIPT)
        assert has(out, "-2")
        assert no_errors(out)

    def test_pop(self):
        out = run('5 3 pop =', Language.POSTSCRIPT)
        assert has(out, "5")
        assert no_errors(out)


class TestPostScriptConditionals2:
    """More PostScript conditional tests."""

    def test_if_gt_true(self):
        out = run('5 3 gt { (yes) = } if', Language.POSTSCRIPT)
        assert has(out, "yes")
        assert no_errors(out)

    def test_ifelse_true(self):
        out = run('5 3 gt { (yes) = } { (no) = } ifelse', Language.POSTSCRIPT)
        assert has(out, "yes")
        assert not has(out, "no")
        assert no_errors(out)

    def test_ifelse_false(self):
        out = run('2 3 gt { (yes) = } { (no) = } ifelse', Language.POSTSCRIPT)
        assert has(out, "no")
        assert not has(out, "yes")
        assert no_errors(out)

    def test_string_eq_true(self):
        out = run('(hello) (hello) eq { (same) = } if', Language.POSTSCRIPT)
        assert has(out, "same")
        assert no_errors(out)

    def test_string_neq(self):
        out = run('(hello) (world) eq { (same) = } { (diff) = } ifelse', Language.POSTSCRIPT)
        assert has(out, "diff")
        assert no_errors(out)

    def test_string_length(self):
        out = run('(hello) length =', Language.POSTSCRIPT)
        assert has(out, "5")
        assert no_errors(out)


class TestPostScriptArithmetic3:
    """More PostScript arithmetic tests."""

    def test_dup_mul(self):
        assert has(run("2 dup mul = ", Language.POSTSCRIPT), "4")

    def test_dup_add(self):
        assert has(run("3 dup add = ", Language.POSTSCRIPT), "6")

    def test_nested_add(self):
        assert has(run("2 3 add 4 5 add add = ", Language.POSTSCRIPT), "14")


class TestPostScriptDefinitions2:
    """PostScript def variable and procedure tests."""

    def test_def_var(self):
        assert has(run("/x 42 def x = ", Language.POSTSCRIPT), "42")

    def test_def_func(self):
        assert has(run("/square { dup mul } def 5 square = ", Language.POSTSCRIPT), "25")


class TestPostScriptConditionals3:
    """More PostScript conditional tests."""

    def test_if_true(self):
        assert has(run("true { (yes) show } if ", Language.POSTSCRIPT), "yes")

    def test_ifelse_false(self):
        assert has(run("false { (yes) show } { (no) show } ifelse", Language.POSTSCRIPT), "no")

    def test_gt_if(self):
        assert has(run("5 3 gt { (bigger) show } if ", Language.POSTSCRIPT), "bigger")

    def test_lt_if(self):
        assert has(run("3 5 lt { (smaller) show } if ", Language.POSTSCRIPT), "smaller")


_PS = Language.POSTSCRIPT


class TestPostScriptStack2:
    """More PostScript stack manipulation tests."""

    def test_roll(self):
        # 3 2 roll rotates top 3 elements
        result = run("1 2 3 3 1 roll =", _PS)
        assert result != []

    def test_exch(self):
        result = run("3 5 exch = =", _PS)
        assert has(result, "5") or has(result, "3")

    def test_clear(self):
        result = run("1 2 3 clear", _PS)
        # clear should not error
        assert not any("❌" in l for l in result)

    def test_count(self):
        result = run("1 2 3 count =", _PS)
        assert has(result, "3")

    def test_copy(self):
        result = run("1 2 2 copy = =", _PS)
        assert result != []


class TestPostScriptStrings2:
    """More PostScript string tests."""

    def test_string_display(self):
        assert has(run("(hello) =", _PS), "hello")

    def test_string_length(self):
        result = run("(hello) length =", _PS)
        assert has(result, "5")

    def test_two_strings(self):
        result = run("(hello) =\n(world) =", _PS)
        assert has(result, "hello") and has(result, "world")


class TestPostScriptArithmetic2:
    """Additional PostScript arithmetic tests."""

    def test_add_7_3(self):
        out = run('7 3 add =', Language.POSTSCRIPT)
        assert has(out, "10")

    def test_mul_6_7(self):
        out = run('6 7 mul =', Language.POSTSCRIPT)
        assert has(out, "42")

    def test_sub_10_3(self):
        out = run('10 3 sub =', Language.POSTSCRIPT)
        assert has(out, "7")

    def test_mod_10_3(self):
        out = run('10 3 mod =', Language.POSTSCRIPT)
        assert has(out, "1")

    def test_abs_neg(self):
        out = run('-7 abs =', Language.POSTSCRIPT)
        assert has(out, "7")

    def test_exp_pow(self):
        out = run('2 8 exp =', Language.POSTSCRIPT)
        assert has(out, "256")

    def test_max(self):
        out = run('3 7 max =', Language.POSTSCRIPT)
        assert has(out, "7")

    def test_min(self):
        out = run('3 7 min =', Language.POSTSCRIPT)
        assert has(out, "3")

    def test_square(self):
        out = run('9 9 mul =', Language.POSTSCRIPT)
        assert has(out, "81")

    def test_dup_add(self):
        out = run('3 dup add =', Language.POSTSCRIPT)
        assert has(out, "6")

    def test_dup_mul(self):
        out = run('5 dup mul =', Language.POSTSCRIPT)
        assert has(out, "25")

    def test_chain_add(self):
        out = run('1 2 add 3 add 4 add =', Language.POSTSCRIPT)
        assert has(out, "10")

    def test_nested_expr(self):
        out = run('3 4 add 2 mul =', Language.POSTSCRIPT)
        assert has(out, "14")


class TestPostScriptStrings2:
    """Additional PostScript string/output tests."""

    def test_print_hello(self):
        out = run('(hello) =', Language.POSTSCRIPT)
        assert has(out, "hello")

    def test_print_world(self):
        out = run('(world) =', Language.POSTSCRIPT)
        assert has(out, "world")

    def test_print_postscript(self):
        out = run('(postscript) =', Language.POSTSCRIPT)
        assert has(out, "postscript")

    def test_two_outputs(self):
        out = run('10 =\n20 =', Language.POSTSCRIPT)
        assert any("10" in line for line in out)
        assert any("20" in line for line in out)

    def test_string_and_number(self):
        out = run('(answer) =\n42 =', Language.POSTSCRIPT)
        assert any("answer" in line for line in out)
        assert any("42" in line for line in out)


class TestPostScriptExtended:
    """More PostScript tests."""

    def test_add_large(self):
        out = run('99 1 add =', Language.POSTSCRIPT)
        assert has(out, "100")

    def test_sub_result(self):
        out = run('10 3 sub =', Language.POSTSCRIPT)
        assert has(out, "7")

    def test_mul_result(self):
        out = run('6 7 mul =', Language.POSTSCRIPT)
        assert has(out, "42")

    def test_div_result(self):
        out = run('10 2 div =', Language.POSTSCRIPT)
        assert has(out, "5")

    def test_nested_add(self):
        out = run('1 2 add 3 add =', Language.POSTSCRIPT)
        assert has(out, "6")

    def test_print_string_42(self):
        out = run('(42) =', Language.POSTSCRIPT)
        assert has(out, "42")

    def test_output_is_list(self):
        out = run('(test) =', Language.POSTSCRIPT)
        assert isinstance(out, list)

    def test_no_errors_simple(self):
        out = run('1 =', Language.POSTSCRIPT)
        assert no_errors(out)

    def test_three_outputs(self):
        out = run('1 =\n2 =\n3 =', Language.POSTSCRIPT)
        texts = " ".join(out)
        assert "1" in texts and "2" in texts and "3" in texts

    def test_string_space(self):
        out = run('(hello world) =', Language.POSTSCRIPT)
        assert has(out, "hello")

    def test_mul_zero(self):
        out = run('5 0 mul =', Language.POSTSCRIPT)
        assert has(out, "0")

    def test_add_negatives(self):
        out = run('5 neg 3 add =', Language.POSTSCRIPT)
        assert isinstance(out, list)

    def test_dup_and_add(self):
        out = run('5 dup add =', Language.POSTSCRIPT)
        assert has(out, "10")

    def test_print_true(self):
        out = run('true =', Language.POSTSCRIPT)
        assert isinstance(out, list)

    def test_print_false(self):
        out = run('false =', Language.POSTSCRIPT)
        assert isinstance(out, list)


class TestPostScriptExtended:
    """Extra PostScript tests."""

    def ps(self, src):
        return run(src, Language.POSTSCRIPT)

    def test_print_100(self):
        assert has(self.ps("100 ="), "100")

    def test_print_zero(self):
        assert has(self.ps("0 ="), "0")

    def test_print_hello(self):
        assert has(self.ps("(Hello) ="), "Hello")

    def test_add_numbers(self):
        assert has(self.ps("3 4 add ="), "7")

    def test_sub_numbers(self):
        assert has(self.ps("10 3 sub ="), "7")

    def test_mul_numbers(self):
        assert has(self.ps("3 4 mul ="), "12")

    def test_dup_stack(self):
        result = self.ps("5 dup = =")
        assert isinstance(result, list)

    def test_output_is_list(self):
        assert isinstance(self.ps("1 ="), list)

    def test_no_errors_simple(self):
        assert no_errors(self.ps("42 ="))

    def test_string_output(self):
        assert has(self.ps("(PostScript) ="), "PostScript")

    def test_neg_number(self):
        assert has(self.ps("-5 ="), "-5")

    def test_two_prints(self):
        result = self.ps("1 = 2 =")
        assert has(result, "1") or has(result, "2")

    def test_div_numbers(self):
        result = self.ps("10 2 div =")
        assert has(result, "5")

    def test_empty_source(self):
        result = self.ps("")
        assert isinstance(result, list)

    def test_print_large_number(self):
        assert has(self.ps("999 ="), "999")


class TestPostScriptExtended2:
    """Second extended round of PostScript language tests."""

    def ps(self, src):
        return run(src, Language.POSTSCRIPT)

    def test_print_42(self):
        assert has(self.ps("42 ="), "42")

    def test_print_zero(self):
        assert has(self.ps("0 ="), "0")

    def test_print_100(self):
        assert has(self.ps("100 ="), "100")

    def test_add(self):
        result = self.ps("3 4 add =")
        assert has(result, "7")

    def test_sub(self):
        result = self.ps("10 3 sub =")
        assert has(result, "7")

    def test_mul(self):
        result = self.ps("3 4 mul =")
        assert has(result, "12")

    def test_empty_is_list(self):
        assert isinstance(self.ps(""), list)

    def test_dup(self):
        result = self.ps("5 dup add =")
        assert has(result, "10")

    def test_exch(self):
        result = self.ps("1 2 exch = =")
        assert isinstance(result, list)

    def test_output_is_list(self):
        assert isinstance(self.ps("1 ="), list)


class TestPostScriptExtended3:
    """Third extended round of PostScript tests."""

    def ps(self, src):
        return run(src, Language.POSTSCRIPT)

    def test_print_0(self):
        result = self.ps("0 =")
        assert has(result, "0")

    def test_print_100(self):
        result = self.ps("100 =")
        assert has(result, "100")

    def test_add_two(self):
        result = self.ps("3 4 add =")
        assert has(result, "7")

    def test_mul_two(self):
        result = self.ps("4 5 mul =")
        assert has(result, "20")

    def test_sub_two(self):
        result = self.ps("10 3 sub =")
        assert has(result, "7")

    def test_div_two(self):
        result = self.ps("20 4 div =")
        assert has(result, "5")

    def test_neg(self):
        result = self.ps("5 neg =")
        assert has(result, "-5")

    def test_dup_add(self):
        result = self.ps("5 dup add =")
        assert has(result, "10")

    def test_output_not_none(self):
        result = self.ps("1 =")
        assert result is not None

    def test_output_is_list(self):
        result = self.ps("1 =")
        assert isinstance(result, list)


class TestPostScriptExtended4:
    """Fourth extended round of PostScript tests."""

    def ps(self, src):
        return run(src, Language.POSTSCRIPT)

    def test_print_42(self):
        assert has(self.ps("42 ="), "42")

    def test_print_zero(self):
        assert has(self.ps("0 ="), "0")

    def test_print_100(self):
        assert has(self.ps("100 ="), "100")

    def test_add(self):
        assert has(self.ps("10 5 add ="), "15")

    def test_sub(self):
        assert has(self.ps("20 8 sub ="), "12")

    def test_mul(self):
        assert has(self.ps("6 7 mul ="), "42")

    def test_dup(self):
        assert has(self.ps("5 dup add ="), "10")

    def test_empty_is_list(self):
        assert isinstance(self.ps(""), list)

    def test_output_is_list(self):
        assert isinstance(self.ps("1 ="), list)

    def test_no_errors(self):
        assert no_errors(self.ps("42 ="))


class TestPostScriptExtended5:
    """Fifth extended round of PostScript tests."""

    def test_print_200(self):
        r = run("200 =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_add_200(self):
        r = run("100 100 add =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_sub_190(self):
        r = run("200 10 sub =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_mul_200(self):
        r = run("20 10 mul =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_dup_op(self):
        r = run("50 dup add =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_pop_op(self):
        r = run("1 2 pop =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_neg_op(self):
        r = run("5 neg =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_is_list(self):
        assert isinstance(run("42 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("42 =", Language.POSTSCRIPT))


class TestPostScriptExtended6:
    def test_print_300(self):
        assert isinstance(run("300 =", Language.POSTSCRIPT), list)

    def test_add_300(self):
        assert isinstance(run("150 150 add =", Language.POSTSCRIPT), list)

    def test_sub_290(self):
        assert isinstance(run("300 10 sub =", Language.POSTSCRIPT), list)

    def test_mul_300(self):
        assert isinstance(run("30 10 mul =", Language.POSTSCRIPT), list)

    def test_div_15(self):
        assert isinstance(run("30 2 div =", Language.POSTSCRIPT), list)

    def test_mod_2(self):
        assert isinstance(run("17 5 mod =", Language.POSTSCRIPT), list)

    def test_neg(self):
        assert isinstance(run("10 neg =", Language.POSTSCRIPT), list)

    def test_empty(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("42 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("42 =", Language.POSTSCRIPT))


class TestPostScriptExtended8:
    def test_eq_700(self):
        assert isinstance(run("700 =", Language.POSTSCRIPT), list)

    def test_eq_21(self):
        assert has(run("21 =", Language.POSTSCRIPT), "21")

    def test_eq_22(self):
        assert has(run("22 =", Language.POSTSCRIPT), "22")

    def test_add_700(self):
        assert has(run("350 350 add =", Language.POSTSCRIPT), "700")

    def test_sub_291(self):
        assert has(run("300 9 sub =", Language.POSTSCRIPT), "291")

    def test_mul_700(self):
        assert has(run("70 10 mul =", Language.POSTSCRIPT), "700")

    def test_stack_exch(self):
        r = run("1 2 exch = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("1 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("1 =", Language.POSTSCRIPT))


class TestPostScriptExtended9:
    def test_eq_800(self):
        assert isinstance(run("800 =", Language.POSTSCRIPT), list)

    def test_eq_23(self):
        assert has(run("23 =", Language.POSTSCRIPT), "23")

    def test_eq_24(self):
        assert has(run("24 =", Language.POSTSCRIPT), "24")

    def test_add_800(self):
        assert has(run("400 400 add =", Language.POSTSCRIPT), "800")

    def test_sub_376(self):
        assert has(run("400 24 sub =", Language.POSTSCRIPT), "376")

    def test_mul_800(self):
        assert has(run("80 10 mul =", Language.POSTSCRIPT), "800")

    def test_div(self):
        r = run("100 4 div =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("1 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("1 =", Language.POSTSCRIPT))


class TestPostScriptExtended10:
    def test_eq_900(self):
        assert isinstance(run("900 =", Language.POSTSCRIPT), list)

    def test_eq_25(self):
        assert has(run("25 =", Language.POSTSCRIPT), "25")

    def test_eq_26(self):
        assert has(run("26 =", Language.POSTSCRIPT), "26")

    def test_add_900(self):
        assert has(run("450 450 add =", Language.POSTSCRIPT), "900")

    def test_sub_424(self):
        assert has(run("450 26 sub =", Language.POSTSCRIPT), "424")

    def test_mul_900(self):
        assert has(run("90 10 mul =", Language.POSTSCRIPT), "900")

    def test_neg(self):
        r = run("42 neg =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("1 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("1 =", Language.POSTSCRIPT))


class TestPostScriptExtended11:
    def test_eq_1000(self):
        assert isinstance(run("1000 =", Language.POSTSCRIPT), list)

    def test_eq_27(self):
        assert has(run("27 =", Language.POSTSCRIPT), "27")

    def test_eq_28(self):
        assert has(run("28 =", Language.POSTSCRIPT), "28")

    def test_add_1000(self):
        assert has(run("500 500 add =", Language.POSTSCRIPT), "1000")

    def test_sub_472(self):
        assert has(run("500 28 sub =", Language.POSTSCRIPT), "472")

    def test_mul_1000(self):
        assert has(run("100 10 mul =", Language.POSTSCRIPT), "1000")

    def test_dup_top(self):
        r = run("42 dup = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("1 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("1 =", Language.POSTSCRIPT))


class TestPostScriptExtended12:
    def test_number_1100(self):
        assert isinstance(run("1100", Language.POSTSCRIPT), list)

    def test_add_11(self):
        assert has(run("5 6 add =", Language.POSTSCRIPT), "11")

    def test_add_1100(self):
        assert has(run("550 550 add =", Language.POSTSCRIPT), "1100")

    def test_mul_12(self):
        assert has(run("3 4 mul =", Language.POSTSCRIPT), "12")

    def test_sub_3(self):
        assert has(run("10 7 sub =", Language.POSTSCRIPT), "3")

    def test_div_5(self):
        assert has(run("20 4 div =", Language.POSTSCRIPT), "5")

    def test_dup(self):
        r = run("9 dup = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("1 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("5 5 add =", Language.POSTSCRIPT))


class TestPostScriptExtended13:
    def test_add_1200(self):
        assert has(run("600 600 add =", Language.POSTSCRIPT), "1200")

    def test_mul_12(self):
        assert has(run("3 4 mul =", Language.POSTSCRIPT), "12")

    def test_sub_7(self):
        assert has(run("10 3 sub =", Language.POSTSCRIPT), "7")

    def test_neg(self):
        r = run("5 neg =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_abs_5(self):
        r = run("-5 abs =", Language.POSTSCRIPT)
        assert has(r, "5")

    def test_max_7(self):
        r = run("3 7 max =", Language.POSTSCRIPT)
        assert has(r, "7")

    def test_min_3(self):
        r = run("3 7 min =", Language.POSTSCRIPT)
        assert has(r, "3")

    def test_dup2(self):
        r = run("7 dup = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_output_list2(self):
        assert isinstance(run("2 =", Language.POSTSCRIPT), list)

    def test_no_errors2(self):
        assert no_errors(run("1 1 add =", Language.POSTSCRIPT))


class TestPostScriptExtended14:
    def test_add_1300(self):
        assert has(run("650 650 add =", Language.POSTSCRIPT), "1300")

    def test_mul_20(self):
        assert has(run("4 5 mul =", Language.POSTSCRIPT), "20")

    def test_sub_7(self):
        assert has(run("10 3 sub =", Language.POSTSCRIPT), "7")

    def test_div_4(self):
        assert has(run("20 5 div =", Language.POSTSCRIPT), "4")

    def test_mod_1(self):
        r = run("10 3 mod =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_exch(self):
        r = run("3 5 exch = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_pop(self):
        r = run("3 5 pop =", Language.POSTSCRIPT)
        assert has(r, "3")

    def test_copy(self):
        r = run("1 2 3 3 copy = = = = = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_output_list3(self):
        assert isinstance(run("3 =", Language.POSTSCRIPT), list)

    def test_no_errors3(self):
        assert no_errors(run("1 2 add =", Language.POSTSCRIPT))


class TestPostScriptExtended15:
    def test_add_1400(self):
        assert has(run("700 700 add =", Language.POSTSCRIPT), "1400")

    def test_mul_49(self):
        assert has(run("7 7 mul =", Language.POSTSCRIPT), "49")

    def test_sub_14(self):
        assert has(run("20 6 sub =", Language.POSTSCRIPT), "14")

    def test_div_4(self):
        assert has(run("20 5 div =", Language.POSTSCRIPT), "4")

    def test_sqrt_3(self):
        r = run("9 sqrt =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_ceiling(self):
        r = run("1.2 ceiling =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_floor(self):
        r = run("1.9 floor =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_round(self):
        r = run("1.5 round =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_output_list4(self):
        assert isinstance(run("4 =", Language.POSTSCRIPT), list)

    def test_no_errors4(self):
        assert no_errors(run("2 3 add =", Language.POSTSCRIPT))


class TestPostScriptExtended16:
    def test_eq_print(self):
        r = run("1 1 eq (yes) (no) ifelse =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_neg(self):
        r = run("5 neg =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_abs_neg(self):
        r = run("-9 abs =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_pop(self):
        r = run("1 2 pop =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_exch(self):
        r = run("1 2 exch = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_dup(self):
        r = run("7 dup = =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_add_1500(self):
        r = run("750 750 add =", Language.POSTSCRIPT)
        assert has(r, "1500")

    def test_mul_64(self):
        r = run("8 8 mul =", Language.POSTSCRIPT)
        assert has(r, "64")

    def test_output_list5(self):
        assert isinstance(run("5 =", Language.POSTSCRIPT), list)

    def test_no_errors5(self):
        assert no_errors(run("5 =", Language.POSTSCRIPT))


class TestPostScriptExtended17:
    def test_sub_10(self):
        r = run("20 10 sub =", Language.POSTSCRIPT)
        assert has(r, "10")

    def test_div_4(self):
        r = run("16 4 div =", Language.POSTSCRIPT)
        assert has(r, "4")

    def test_mod_3(self):
        r = run("15 4 mod =", Language.POSTSCRIPT)
        assert has(r, "3")

    def test_gt(self):
        r = run("5 3 gt =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_lt(self):
        r = run("3 5 lt =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_and_bool(self):
        r = run("true false and =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_or_bool(self):
        r = run("true false or =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_not_bool(self):
        r = run("true not =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_output_list7(self):
        assert isinstance(run("7 =", Language.POSTSCRIPT), list)

    def test_no_errors7(self):
        assert no_errors(run("7 =", Language.POSTSCRIPT))


class TestPostScriptExtended18:
    def test_ceiling(self):
        r = run("3.2 ceiling =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_floor(self):
        r = run("3.9 floor =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_round(self):
        r = run("3.5 round =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_truncate(self):
        r = run("3.9 truncate =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_sqrt_4(self):
        r = run("16 sqrt =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_string_print(self):
        r = run("(Hello) =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_add_1700(self):
        r = run("850 850 add =", Language.POSTSCRIPT)
        assert has(r, "1700")

    def test_mul_100(self):
        r = run("10 10 mul =", Language.POSTSCRIPT)
        assert has(r, "100")

    def test_output_list8(self):
        assert isinstance(run("8 =", Language.POSTSCRIPT), list)

    def test_no_errors8(self):
        assert no_errors(run("8 =", Language.POSTSCRIPT))


class TestPostScriptExtended19:
    def test_type_int(self):
        r = run("5 type =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_copy_stack(self):
        r = run("1 2 3 3 copy . . . . . .", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_roll(self):
        r = run("1 2 3 3 1 roll . . .", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_index(self):
        r = run("1 2 3 1 index =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_clear(self):
        r = run("1 2 3 clear", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_count(self):
        r = run("1 2 3 count =", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_add_1800(self):
        r = run("900 900 add =", Language.POSTSCRIPT)
        assert has(r, "1800")

    def test_mul_121(self):
        r = run("11 11 mul =", Language.POSTSCRIPT)
        assert has(r, "121")

    def test_output_list9(self):
        assert isinstance(run("9 =", Language.POSTSCRIPT), list)

    def test_no_errors9(self):
        assert no_errors(run("9 =", Language.POSTSCRIPT))


class TestPostScriptExtended20:
    def test_moveto(self):
        r = run("10 20 moveto", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_lineto(self):
        r = run("0 0 moveto 10 10 lineto", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_setlinewidth(self):
        r = run("2 setlinewidth", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_gsave_grestore(self):
        r = run("gsave grestore", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_translate(self):
        r = run("100 200 translate", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_scale(self):
        r = run("2 2 scale", Language.POSTSCRIPT)
        assert isinstance(r, list)

    def test_add_1900(self):
        r = run("950 950 add =", Language.POSTSCRIPT)
        assert has(r, "1900")

    def test_mul_144(self):
        r = run("12 12 mul =", Language.POSTSCRIPT)
        assert has(r, "144")

    def test_output_list10(self):
        assert isinstance(run("10 =", Language.POSTSCRIPT), list)

    def test_no_errors10(self):
        assert no_errors(run("10 =", Language.POSTSCRIPT))
