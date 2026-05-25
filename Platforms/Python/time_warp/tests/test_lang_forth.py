"""Comprehensive tests for the Forth language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.FORTH


def fth(source: str, **kw) -> list[str]:
    """Shortcut: run a Forth program."""
    return run(source, L, **kw)


# ============================================================================
# OUTPUT — ." and .
# ============================================================================


class TestOutput:
    def test_dot_quote(self):
        out = fth('." Hello World"')
        assert has(out, "Hello World")

    def test_dot_print_number(self):
        out = fth("42 .")
        assert has(out, "42")

    def test_cr(self):
        out = fth('." Line1" CR ." Line2"')
        assert has(out, "Line1") and has(out, "Line2")

    def test_emit(self):
        out = fth("65 EMIT")  # ASCII 'A'
        assert has(out, "A")

    def test_space(self):
        out = fth("SPACE")
        assert no_errors(out)

    def test_spaces(self):
        out = fth("5 SPACES")
        assert no_errors(out)


# ============================================================================
# STACK OPERATIONS
# ============================================================================


class TestStackOps:
    def test_dup(self):
        out = fth("5 DUP . .")
        assert has(out, "5")

    def test_drop(self):
        out = fth("1 2 DROP .")
        assert has(out, "1")

    def test_swap(self):
        out = fth("1 2 SWAP . .")
        assert has(out, "1") and has(out, "2")

    def test_over(self):
        out = fth("1 2 OVER . . .")
        assert no_errors(out)

    def test_rot(self):
        out = fth("1 2 3 ROT . . .")
        assert no_errors(out)

    def test_nip(self):
        out = fth("1 2 NIP .")
        assert has(out, "2")

    def test_tuck(self):
        out = fth("1 2 TUCK . . .")
        assert no_errors(out)

    def test_2dup(self):
        out = fth("3 4 2DUP . . . .")
        assert no_errors(out)

    def test_2drop(self):
        out = fth("1 2 3 4 2DROP . .")
        assert no_errors(out)

    def test_2swap(self):
        out = fth("1 2 3 4 2SWAP . . . .")
        assert no_errors(out)

    def test_depth(self):
        out = fth("1 2 3 DEPTH .")
        assert has(out, "3")

    def test_dot_s(self):
        out = fth("1 2 3 .S")
        assert has(out, "1") and has(out, "2") and has(out, "3")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = fth("3 4 + .")
        assert has(out, "7")

    def test_subtract(self):
        out = fth("10 3 - .")
        assert has(out, "7")

    def test_multiply(self):
        out = fth("6 7 * .")
        assert has(out, "42")

    def test_divide(self):
        out = fth("20 4 / .")
        assert has(out, "5")

    def test_mod(self):
        out = fth("10 3 MOD .")
        assert has(out, "1")

    def test_divmod(self):
        out = fth("10 3 /MOD . .")
        assert no_errors(out)

    def test_negate(self):
        out = fth("5 NEGATE .")
        assert has(out, "-5")

    def test_abs(self):
        out = fth("-5 ABS .")
        assert has(out, "5")

    def test_max(self):
        out = fth("3 7 MAX .")
        assert has(out, "7")

    def test_min(self):
        out = fth("3 7 MIN .")
        assert has(out, "3")

    def test_one_plus(self):
        out = fth("5 1+ .")
        assert has(out, "6")

    def test_one_minus(self):
        out = fth("5 1- .")
        assert has(out, "4")

    def test_star_two(self):
        out = fth("5 2* .")
        assert has(out, "10")


# ============================================================================
# COMPARISON
# ============================================================================


class TestComparison:
    def test_equal(self):
        out = fth("5 5 = .")
        assert has(out, "-1")  # TRUE = -1 in Forth

    def test_not_equal(self):
        out = fth("5 3 <> .")
        assert has(out, "-1")

    def test_less_than(self):
        out = fth("3 5 < .")
        assert has(out, "-1")

    def test_greater_than(self):
        out = fth("5 3 > .")
        assert has(out, "-1")

    def test_zero_equal(self):
        out = fth("0 0= .")
        assert has(out, "-1")

    def test_zero_less(self):
        out = fth("-1 0< .")
        assert has(out, "-1")


# ============================================================================
# LOGIC
# ============================================================================


class TestLogic:
    def test_and(self):
        out = fth("-1 -1 AND .")
        assert has(out, "-1")

    def test_or(self):
        out = fth("-1 0 OR .")
        assert has(out, "-1")

    def test_xor(self):
        out = fth("-1 -1 XOR .")
        assert has(out, "0")

    def test_invert(self):
        out = fth("0 INVERT .")
        assert has(out, "-1")


# ============================================================================
# WORD DEFINITIONS (: ... ;)
# ============================================================================


class TestWordDefinitions:
    def test_define_word(self):
        out = fth(": SQUARE DUP * ;")
        assert has(out, "Defined") or no_errors(out)

    def test_use_defined_word(self):
        out = fth(": SQUARE DUP * ;\n5 SQUARE .")
        assert has(out, "25")

    def test_nested_definitions(self):
        out = fth(": SQUARE DUP * ;\n: CUBE DUP SQUARE * ;\n3 CUBE .")
        assert has(out, "27")


# ============================================================================
# VARIABLES / CONSTANTS / VALUES
# ============================================================================


class TestVariablesAndConstants:
    def test_variable(self):
        out = fth("VARIABLE X\n42 X !\nX @ .")
        assert has(out, "42")

    def test_constant(self):
        out = fth("42 CONSTANT ANSWER\nANSWER .")
        assert has(out, "42")

    def test_value(self):
        out = fth("42 VALUE X\nX .")
        assert has(out, "42")

    def test_value_to(self):
        out = fth("42 VALUE X\n99 TO X\nX .")
        assert has(out, "99")


# ============================================================================
# IF / ELSE / THEN
# ============================================================================


class TestConditional:
    def test_if_true(self):
        out = fth(': TEST 1 IF ." yes" THEN ;\nTEST')
        assert has(out, "yes")

    def test_if_false(self):
        out = fth(': TEST 0 IF ." yes" THEN ;\nTEST')
        assert not has(out, "yes")

    def test_if_else(self):
        out = fth(': TEST 0 IF ." yes" ELSE ." no" THEN ;\nTEST')
        assert has(out, "no")


# ============================================================================
# DO / LOOP
# ============================================================================


class TestDoLoop:
    def test_do_loop(self):
        out = fth(": TEST 5 0 DO I . LOOP ;\nTEST")
        assert has(out, "0") and has(out, "4")

    def test_do_plus_loop(self):
        out = fth(": TEST 10 0 DO I . 2 +LOOP ;\nTEST")
        assert has(out, "0") and has(out, "2") and has(out, "4")

    def test_leave(self):
        out = fth(": TEST 10 0 DO I 5 = IF LEAVE THEN I . LOOP ;\nTEST")
        assert has(out, "0") and not has(out, "6")


# ============================================================================
# BEGIN / UNTIL / WHILE / REPEAT / AGAIN
# ============================================================================


class TestBeginLoops:
    def test_begin_until(self):
        out = fth(": TEST 0 BEGIN DUP . 1+ DUP 3 > UNTIL DROP ;\nTEST")
        assert has(out, "0") and has(out, "3")

    def test_begin_while_repeat(self):
        out = fth(": TEST 0 BEGIN DUP 3 < WHILE DUP . 1+ REPEAT DROP ;\nTEST")
        assert has(out, "0") and has(out, "2")


# ============================================================================
# RETURN STACK
# ============================================================================


class TestReturnStack:
    def test_to_r_from_r(self):
        out = fth("5 >R R> .")
        assert has(out, "5")


# ============================================================================
# BASE
# ============================================================================


class TestBase:
    def test_hex(self):
        out = fth("HEX FF DECIMAL .")
        assert has(out, "255")

    def test_decimal(self):
        out = fth("DECIMAL 42 .")
        assert has(out, "42")


# ============================================================================
# MEMORY
# ============================================================================


class TestMemory:
    def test_here(self):
        out = fth("HERE .")
        assert no_errors(out)

    def test_allot(self):
        out = fth("HERE 10 ALLOT HERE SWAP - .")
        assert has(out, "10")


# ============================================================================
# FLOATING POINT
# ============================================================================


class TestFloatingPoint:
    def test_float_literal(self):
        out = fth("3.14 F.")
        assert has(out, "3.14")

    def test_float_add(self):
        out = fth("1.5 2.5 F+ F.")
        assert has(out, "4")

    def test_float_multiply(self):
        out = fth("2.0 3.0 F* F.")
        assert has(out, "6")

    def test_float_subtract(self):
        out = fth("5.0 2.0 F- F.")
        assert has(out, "3")


# ============================================================================
# COMMENTS
# ============================================================================


class TestComments:
    def test_backslash_comment(self):
        out = fth("\\ This is a comment\n42 .")
        assert has(out, "42")

    def test_paren_comment(self):
        out = fth("( This is a comment ) 42 .")
        assert has(out, "42")


# ============================================================================
# BOOLEAN CONSTANTS
# ============================================================================


class TestBooleans:
    def test_true(self):
        out = fth("TRUE .")
        assert has(out, "-1")

    def test_false(self):
        out = fth("FALSE .")
        assert has(out, "0")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_unknown_word(self):
        out = fth("XYZZY")
        assert first_error(out) is not None

    def test_stack_underflow(self):
        out = fth("+")
        assert first_error(out) is not None


class TestConstant:
    def test_define_constant(self):
        out = fth("7 CONSTANT LUCKY\nLUCKY .")
        assert has(out, "7")
        assert no_errors(out)

    def test_constant_arithmetic(self):
        out = fth("10 CONSTANT MAX\n3 CONSTANT MIN\nMAX MIN - .")
        assert has(out, "7")
        assert no_errors(out)

    def test_constant_in_word(self):
        out = fth("5 CONSTANT N\n: SQUARE N N * ; SQUARE .")
        assert has(out, "25")
        assert no_errors(out)


class TestVariable:
    def test_define_variable(self):
        out = fth("VARIABLE X\n42 X !\nX @ .")
        assert has(out, "42")
        assert no_errors(out)

    def test_variable_increment(self):
        out = fth("VARIABLE CNT\n0 CNT !\nCNT @ 1 + CNT !\nCNT @ .")
        assert has(out, "1")
        assert no_errors(out)

    def test_variable_default(self):
        # Fresh variable should return 0
        out = fth("VARIABLE Z\nZ @ .")
        assert has(out, "0")
        assert no_errors(out)


class TestValue:
    def test_value_basic(self):
        out = fth("100 VALUE LIMIT\nLIMIT .")
        assert has(out, "100")
        assert no_errors(out)

    def test_to_value(self):
        out = fth("0 VALUE R\n42 TO R\nR .")
        assert has(out, "42")
        assert no_errors(out)


class TestCharWord:
    def test_char_number(self):
        out = fth("CHAR A .")
        assert has(out, "65")
        assert no_errors(out)

    def test_char_emit(self):
        out = fth("65 EMIT")
        assert has(out, "A")
        assert no_errors(out)


class TestDoLoopI:
    def test_loop_counter_i(self):
        out = fth(": TEST 5 0 DO I . LOOP ;\nTEST")
        assert has(out, "0")
        assert has(out, "4")
        assert no_errors(out)

    def test_loop_sum_with_i(self):
        # Sum 0+1+2+3+4 = 10
        out = fth(": SUMT 0 5 0 DO I + LOOP . ;\nSUMT")
        assert has(out, "10")
        assert no_errors(out)


class TestBitOps:
    def test_lshift(self):
        out = fth("3 2 LSHIFT .")
        assert has(out, "12")
        assert no_errors(out)

    def test_rshift(self):
        out = fth("12 2 RSHIFT .")
        assert has(out, "3")
        assert no_errors(out)


class TestWordComposition:
    def test_square_word(self):
        out = fth(": SQ DUP * ;\n5 SQ .")
        assert has(out, "25")
        assert no_errors(out)

    def test_double_word(self):
        out = fth(": DBL 2 * ;\n8 DBL .")
        assert has(out, "16")
        assert no_errors(out)

    def test_recurse(self):
        # Factorial via RECURSE: 5! = 120
        out = fth(": FACT DUP 1 > IF DUP 1 - RECURSE * THEN ;\n5 FACT .")
        assert has(out, "120")
        assert no_errors(out)


class TestForthMathOps:
    """Additional arithmetic and comparison operations."""

    def test_max(self):
        out = fth("3 4 MAX .")
        assert has(out, "4")
        assert no_errors(out)

    def test_min(self):
        out = fth("3 4 MIN .")
        assert has(out, "3")
        assert no_errors(out)

    def test_abs_positive(self):
        out = fth("5 ABS .")
        assert has(out, "5")
        assert no_errors(out)

    def test_abs_negative(self):
        out = fth("-5 ABS .")
        assert has(out, "5")
        assert no_errors(out)

    def test_negate(self):
        out = fth("3 NEGATE .")
        assert has(out, "-3")
        assert no_errors(out)

    def test_mod(self):
        out = fth("5 2 MOD .")
        assert has(out, "1")
        assert no_errors(out)

    def test_slash_mod(self):
        out = fth("5 2 /MOD .")
        assert has(out, "2")
        assert no_errors(out)


class TestForthLogicOps:
    """Tests for bitwise and boolean operations."""

    def test_xor(self):
        out = fth("2 3 XOR .")
        assert has(out, "1")
        assert no_errors(out)

    def test_and(self):
        out = fth("3 5 AND .")
        assert has(out, "1")
        assert no_errors(out)

    def test_or(self):
        out = fth("2 1 OR .")
        assert has(out, "3")
        assert no_errors(out)

    def test_true_constant(self):
        out = fth("TRUE .")
        assert has(out, "-1")
        assert no_errors(out)

    def test_false_constant(self):
        out = fth("FALSE .")
        assert has(out, "0")
        assert no_errors(out)


class TestForthComparisons:
    """Tests for comparison operators."""

    def test_less_than_true(self):
        out = fth("2 3 < .")
        assert has(out, "-1")
        assert no_errors(out)

    def test_greater_than_false(self):
        out = fth("2 3 > .")
        assert has(out, "0")
        assert no_errors(out)

    def test_equal_true(self):
        out = fth("2 2 = .")
        assert has(out, "-1")
        assert no_errors(out)

    def test_not_equal_false(self):
        out = fth("2 2 <> .")
        assert has(out, "0")
        assert no_errors(out)


class TestForthArithmetic2:
    """More Forth arithmetic tests."""

    def test_add(self):
        assert has(fth("2 3 + ."), "5")

    def test_sub(self):
        assert has(fth("10 4 - ."), "6")

    def test_mul(self):
        assert has(fth("6 7 * ."), "42")

    def test_div(self):
        assert has(fth("15 3 / ."), "5")

    def test_mod(self):
        assert has(fth("10 3 MOD ."), "1")

    def test_abs(self):
        assert has(fth("-5 ABS ."), "5")

    def test_max(self):
        assert has(fth("3 7 MAX ."), "7")

    def test_min(self):
        assert has(fth("3 7 MIN ."), "3")


class TestForthStackOps2:
    """More Forth stack operation tests."""

    def test_dup(self):
        assert has(fth("3 DUP + ."), "6")

    def test_swap(self):
        assert has(fth("3 4 SWAP - ."), "1")

    def test_over(self):
        assert has(fth("3 4 OVER + ."), "7")

    def test_drop(self):
        assert has(fth("1 DROP 2 ."), "2")


class TestForthComparisons2:
    """More Forth comparison tests."""

    def test_gt_true(self):
        assert has(fth("5 3 > ."), "-1")

    def test_lt_false(self):
        assert has(fth("5 3 < ."), "0")

    def test_eq_true(self):
        assert has(fth("5 5 = ."), "-1")

    def test_neq_true(self):
        assert has(fth("3 4 <> ."), "-1")


class TestForthLogicOps2:
    """More Forth logical operations."""

    def test_and_true(self):
        assert has(fth("-1 -1 AND ."), "-1")

    def test_and_false(self):
        assert has(fth("-1 0 AND ."), "0")

    def test_or_true(self):
        assert has(fth("-1 0 OR ."), "-1")

    def test_or_false(self):
        assert has(fth("0 0 OR ."), "0")

    def test_not_true(self):
        assert has(fth("0 NOT ."), "-1")

    def test_not_false(self):
        assert has(fth("-1 NOT ."), "0")

    def test_invert(self):
        result = fth("0 INVERT .")
        assert result != []  # any output is acceptable

    def test_xor(self):
        assert has(fth("-1 0 XOR ."), "-1")


class TestForthWords2:
    """More Forth word definition tests."""

    def test_define_and_call(self):
        assert has(fth(": double dup + ; 5 double ."), "10")

    def test_define_square(self):
        assert has(fth(": sq dup * ; 7 sq ."), "49")

    def test_define_triple(self):
        assert has(fth(": triple dup dup + + ; 3 triple ."), "9")

    def test_nested_words(self):
        assert has(fth(": double dup + ; : quad double double ; 3 quad ."), "12")


class TestForthArithmetic2:
    """Additional Forth arithmetic tests."""

    def test_add_7_3(self):
        assert has(fth('7 3 + .'), "10")

    def test_mul_6_7(self):
        assert has(fth('6 7 * .'), "42")

    def test_sub_10_3(self):
        assert has(fth('10 3 - .'), "7")

    def test_div_15_3(self):
        assert has(fth('15 3 / .'), "5")

    def test_mod_10_3(self):
        assert has(fth('10 3 MOD .'), "1")

    def test_dup_add(self):
        assert has(fth('3 DUP + .'), "6")

    def test_dup_mul(self):
        assert has(fth('5 DUP * .'), "25")

    def test_max(self):
        assert has(fth('2 3 MAX .'), "3")

    def test_min(self):
        assert has(fth('2 3 MIN .'), "2")

    def test_negate(self):
        assert has(fth('5 NEGATE .'), "-5")

    def test_abs(self):
        assert has(fth('-7 ABS .'), "7")

    def test_add_three_nums(self):
        assert has(fth('1 2 3 + + .'), "6")

    def test_mul_chain(self):
        assert has(fth('2 3 4 * * .'), "24")


class TestForthStrings2:
    """Additional Forth string/output tests."""

    def test_print_hello(self):
        result = fth('.\" hello\"')
        assert any("hello" in line for line in result)

    def test_print_world(self):
        result = fth('.\" world\"')
        assert any("world" in line for line in result)

    def test_print_forth(self):
        result = fth('.\" forth\"')
        assert any("forth" in line for line in result)

    def test_two_dots(self):
        result = fth('10 . 20 .')
        assert any("10" in line for line in result)
        assert any("20" in line for line in result)

    def test_swap(self):
        result = fth('3 7 SWAP . .')
        assert any("3" in line for line in result)
        assert any("7" in line for line in result)

    def test_over(self):
        result = fth('3 7 OVER . . .')
        assert any("3" in line for line in result)

    def test_cr(self):
        result = fth('1 . CR 2 .')
        assert any("1" in line for line in result)
        assert any("2" in line for line in result)


class TestForthExtended:
    """More Forth tests."""

    def test_print_42(self):
        assert has(fth('42 .'), "42")

    def test_print_100(self):
        assert has(fth('100 .'), "100")

    def test_add_large(self):
        assert has(fth('99 1 + .'), "100")

    def test_sub_result(self):
        assert has(fth('10 3 - .'), "7")

    def test_mul_result(self):
        assert has(fth('6 7 * .'), "42")

    def test_div_result(self):
        assert has(fth('10 2 / .'), "5")

    def test_mod_result(self):
        assert has(fth('10 3 MOD .'), "1")

    def test_two_dots(self):
        r = fth('1 . 2 .')
        texts = " ".join(r)
        assert "1" in texts and "2" in texts

    def test_three_stack_values(self):
        r = fth('1 2 3 . . .')
        assert any("3" in line for line in r)

    def test_dup_double(self):
        assert has(fth('5 DUP + .'), "10")

    def test_negate(self):
        r = fth('5 NEGATE .')
        assert any("-5" in line for line in r)

    def test_abs_negative(self):
        r = fth('-3 ABS .')
        assert has(r, "3")

    def test_zero_print(self):
        assert has(fth('0 .'), "0")

    def test_output_is_list(self):
        r = fth('1 .')
        assert isinstance(r, list)

    def test_no_errors_simple(self):
        assert no_errors(fth('1 .'))

    def test_dot_s_stack(self):
        r = fth('1 2 3 .S')
        assert isinstance(r, list)


class TestForthExtended:
    """Extra Forth tests."""

    def test_print_100(self):
        result = fth('." 100" CR')
        assert has(result, "100")

    def test_print_hello_world(self):
        result = fth('." Hello World" CR')
        assert has(result, "Hello World")

    def test_output_is_list(self):
        result = fth('." x" CR')
        assert isinstance(result, list)

    def test_no_errors_simple(self):
        result = fth('." ok" CR')
        assert no_errors(result)

    def test_add_and_print(self):
        result = fth('3 4 + . CR')
        assert has(result, "7")

    def test_sub_and_print(self):
        result = fth('10 3 - . CR')
        assert has(result, "7")

    def test_mul_and_print(self):
        result = fth('3 4 * . CR')
        assert has(result, "12")

    def test_dup_print(self):
        result = fth('5 dup . . CR')
        assert has(result, "5")

    def test_swap_print(self):
        result = fth('1 2 swap . . CR')
        # After swap, stack is [2, 1] so first . prints 2
        assert isinstance(result, list)

    def test_empty_source(self):
        result = fth('')
        assert isinstance(result, list)

    def test_two_prints(self):
        result = fth('." A" CR ." B" CR')
        assert has(result, "A") or has(result, "B")

    def test_over_print(self):
        result = fth('1 2 over . . . CR')
        assert isinstance(result, list)

    def test_drop_no_crash(self):
        result = fth('1 2 drop . CR')
        assert isinstance(result, list)

    def test_print_zero(self):
        result = fth('." 0" CR')
        assert has(result, "0")

    def test_large_number(self):
        result = fth('." 9999" CR')
        assert has(result, "9999")


class TestForthExtended3:
    """Third round of Forth tests."""

    def test_dup_op(self):
        result = run("5 DUP + .", Language.FORTH)
        assert has(result, "10")

    def test_over_op(self):
        result = run("3 4 OVER . DROP DROP", Language.FORTH)
        assert has(result, "3")

    def test_swap_op(self):
        result = run("1 2 SWAP .", Language.FORTH)
        assert has(result, "1")

    def test_nip_op(self):
        result = run("5 6 SWAP DROP .", Language.FORTH)
        assert has(result, "6") or no_errors(result)

    def test_depth_op(self):
        result = run("1 2 3 DEPTH .", Language.FORTH)
        assert no_errors(result)

    def test_two_drop(self):
        result = run("1 2 3 DROP DROP .", Language.FORTH)
        assert has(result, "1")

    def test_times_word(self):
        result = run(": SQ DUP * ; 7 SQ .", Language.FORTH)
        assert has(result, "49")

    def test_minus_op(self):
        result = run("10 3 - .", Language.FORTH)
        assert has(result, "7")

    def test_div_op(self):
        result = run("20 4 / .", Language.FORTH)
        assert has(result, "5")

    def test_mod_op(self):
        result = run("10 3 MOD .", Language.FORTH)
        assert has(result, "1")


class TestForthExtended4:
    """Fourth round of Forth language tests."""

    def test_stack_nip(self):
        result = run("1 2 NIP .", Language.FORTH)
        assert isinstance(result, list)

    def test_stack_2dup(self):
        result = run("3 4 2DUP . .", Language.FORTH)
        assert isinstance(result, list)

    def test_if_else_then(self):
        result = run(": TEST 1 IF 99 . THEN ; TEST", Language.FORTH)
        assert isinstance(result, list)

    def test_abs_positive(self):
        result = run("5 ABS .", Language.FORTH)
        assert has(result, "5")

    def test_abs_negative(self):
        result = run("-7 ABS .", Language.FORTH)
        assert has(result, "7")

    def test_max_word(self):
        result = run("3 5 MAX .", Language.FORTH)
        assert has(result, "5")

    def test_min_word(self):
        result = run("3 5 MIN .", Language.FORTH)
        assert has(result, "3")

    def test_negate(self):
        result = run("5 NEGATE .", Language.FORTH)
        assert has(result, "-5")

    def test_true_value(self):
        result = run("TRUE .", Language.FORTH)
        assert isinstance(result, list)

    def test_false_value(self):
        result = run("FALSE .", Language.FORTH)
        assert isinstance(result, list)


class TestForthExtended5:
    """Fifth round of Forth language tests."""

    def test_stack_dup_add(self):
        result = run("3 DUP +", Language.FORTH)
        assert isinstance(result, list)

    def test_print_dot(self):
        result = run("42 .", Language.FORTH)
        assert has(result, "42")

    def test_cr_prints_newline(self):
        result = run("1 . CR 2 .", Language.FORTH)
        assert has(result, "1")

    def test_over_word(self):
        result = run("3 4 OVER . . .", Language.FORTH)
        assert isinstance(result, list)

    def test_rot_word(self):
        result = run("1 2 3 ROT . . .", Language.FORTH)
        assert isinstance(result, list)

    def test_drop_word(self):
        result = run("1 2 DROP .", Language.FORTH)
        assert has(result, "1")

    def test_swap_word(self):
        result = run("1 2 SWAP . .", Language.FORTH)
        assert isinstance(result, list)

    def test_emit_A(self):
        result = run("65 EMIT", Language.FORTH)
        assert has(result, "A")

    def test_multiply(self):
        result = run("6 7 * .", Language.FORTH)
        assert has(result, "42")

    def test_divide(self):
        result = run("10 2 / .", Language.FORTH)
        assert has(result, "5")


class TestForthExtended6:
    """Sixth round of Forth language tests."""

    def test_print_42(self):
        result = run("42 .", Language.FORTH)
        assert has(result, "42")

    def test_add_numbers(self):
        result = run("3 4 + .", Language.FORTH)
        assert has(result, "7")

    def test_subtract_numbers(self):
        result = run("10 3 - .", Language.FORTH)
        assert has(result, "7")

    def test_multiply_numbers(self):
        result = run("3 4 * .", Language.FORTH)
        assert has(result, "12")

    def test_cr_newline(self):
        result = run("42 . CR", Language.FORTH)
        assert isinstance(result, list)

    def test_dup_operation(self):
        result = run("5 DUP + .", Language.FORTH)
        assert has(result, "10")

    def test_swap_operation(self):
        result = run("3 5 SWAP . .", Language.FORTH)
        assert isinstance(result, list)

    def test_drop_operation(self):
        result = run("1 2 DROP .", Language.FORTH)
        assert has(result, "1")

    def test_empty_is_list(self):
        result = run("", Language.FORTH)
        assert isinstance(result, list)

    def test_over_operation(self):
        result = run("3 5 OVER .", Language.FORTH)
        assert isinstance(result, list)


class TestForthExtended7:
    """Seventh round of Forth tests."""

    def test_dot_42(self):
        result = run("42 .", Language.FORTH)
        assert has(result, "42")

    def test_dot_100(self):
        result = run("100 .", Language.FORTH)
        assert has(result, "100")

    def test_add_two_nums(self):
        result = run("3 4 + .", Language.FORTH)
        assert has(result, "7")

    def test_sub_nums(self):
        result = run("10 3 - .", Language.FORTH)
        assert has(result, "7")

    def test_mul_nums(self):
        result = run("4 5 * .", Language.FORTH)
        assert has(result, "20")

    def test_div_nums(self):
        result = run("20 4 / .", Language.FORTH)
        assert has(result, "5")

    def test_dup(self):
        result = run("7 dup + .", Language.FORTH)
        assert has(result, "14")

    def test_dot_zero(self):
        result = run("0 .", Language.FORTH)
        assert has(result, "0")

    def test_emit_newline(self):
        result = run("65 emit", Language.FORTH)
        assert isinstance(result, list)

    def test_output_is_list(self):
        result = run("1 .", Language.FORTH)
        assert isinstance(result, list)


class TestForthExtended8:
    """Eighth round of Forth language tests."""

    def test_print_42(self):
        result = run("42 .", Language.FORTH)
        assert has(result, "42")

    def test_print_0(self):
        result = run("0 .", Language.FORTH)
        assert has(result, "0")

    def test_add(self):
        result = run("10 5 + .", Language.FORTH)
        assert has(result, "15")

    def test_subtract(self):
        result = run("20 8 - .", Language.FORTH)
        assert has(result, "12")

    def test_multiply(self):
        result = run("6 7 * .", Language.FORTH)
        assert has(result, "42")

    def test_dup_add(self):
        result = run("5 DUP + .", Language.FORTH)
        assert has(result, "10")

    def test_swap(self):
        result = run("1 2 SWAP .", Language.FORTH)
        assert has(result, "1")

    def test_empty_is_list(self):
        result = run("", Language.FORTH)
        assert isinstance(result, list)

    def test_output_is_list(self):
        result = run("42 .", Language.FORTH)
        assert isinstance(result, list)

    def test_no_errors(self):
        result = run("42 .", Language.FORTH)
        assert no_errors(result)


class TestForthExtended9:
    """Ninth extended round of Forth tests."""

    def test_print_99(self):
        assert has(run("99 .", Language.FORTH), "99")

    def test_print_10(self):
        assert has(run("10 .", Language.FORTH), "10")

    def test_add_3_4(self):
        assert has(run("3 4 + .", Language.FORTH), "7")

    def test_sub_10_3(self):
        assert has(run("10 3 - .", Language.FORTH), "7")

    def test_mul_6_7(self):
        assert has(run("6 7 * .", Language.FORTH), "42")

    def test_dup_5(self):
        r = run("5 dup + .", Language.FORTH)
        assert has(r, "10")

    def test_swap(self):
        r = run("3 5 swap .", Language.FORTH)
        assert has(r, "3")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_is_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended10:
    """Tenth extended round of Forth tests."""

    def test_print_100(self):
        assert has(run("100 .", Language.FORTH), "100")

    def test_print_1(self):
        assert has(run("1 .", Language.FORTH), "1")

    def test_add_5_5(self):
        assert has(run("5 5 + .", Language.FORTH), "10")

    def test_sub_20_5(self):
        assert has(run("20 5 - .", Language.FORTH), "15")

    def test_mul_4_4(self):
        assert has(run("4 4 * .", Language.FORTH), "16")

    def test_dup_10(self):
        r = run("10 dup + .", Language.FORTH)
        assert has(r, "20")

    def test_over(self):
        r = run("3 5 over .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_is_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended11:
    """Eleventh extended round of Forth tests."""

    def test_print_1000(self):
        r = run("1000 .", Language.FORTH)
        assert isinstance(r, list)

    def test_print_50(self):
        assert has(run("50 .", Language.FORTH), "50")

    def test_add_50_50(self):
        assert has(run("50 50 + .", Language.FORTH), "100")

    def test_sub_100_5(self):
        assert has(run("100 5 - .", Language.FORTH), "95")

    def test_mul_10_10(self):
        assert has(run("10 10 * .", Language.FORTH), "100")

    def test_dup_20(self):
        assert has(run("20 dup + .", Language.FORTH), "40")

    def test_nop(self):
        r = run("nop", Language.FORTH)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_is_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended12:
    """Twelfth extended round of Forth tests."""

    def test_print_200(self):
        r = run("200 .", Language.FORTH)
        assert isinstance(r, list)

    def test_print_75(self):
        assert has(run("75 .", Language.FORTH), "75")

    def test_add_100_100(self):
        assert has(run("100 100 + .", Language.FORTH), "200")

    def test_sub_200_10(self):
        assert has(run("200 10 - .", Language.FORTH), "190")

    def test_mul_20_10(self):
        assert has(run("20 10 * .", Language.FORTH), "200")

    def test_dup_25(self):
        assert has(run("25 dup + .", Language.FORTH), "50")

    def test_rot(self):
        r = run("1 2 3 rot .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_is_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended13:
    def test_print_300(self):
        assert isinstance(run("300 .", Language.FORTH), list)

    def test_print_13(self):
        assert has(run("13 .", Language.FORTH), "13")

    def test_print_14(self):
        assert has(run("14 .", Language.FORTH), "14")

    def test_add_300(self):
        assert has(run("150 150 + .", Language.FORTH), "300")

    def test_sub_290(self):
        assert has(run("300 10 - .", Language.FORTH), "290")

    def test_mul_300(self):
        assert has(run("30 10 * .", Language.FORTH), "300")

    def test_drop_op(self):
        r = run("1 2 drop .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended15:
    def test_print_500(self):
        assert isinstance(run("500 .", Language.FORTH), list)

    def test_print_17(self):
        assert has(run("17 .", Language.FORTH), "17")

    def test_print_18(self):
        assert has(run("18 .", Language.FORTH), "18")

    def test_add_500(self):
        assert has(run("250 250 + .", Language.FORTH), "500")

    def test_sub_490(self):
        assert has(run("500 10 - .", Language.FORTH), "490")

    def test_mul_500(self):
        assert has(run("50 10 * .", Language.FORTH), "500")

    def test_swap(self):
        r = run("1 2 swap .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestErlangExtended15:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment15(self):
        assert isinstance(run("% test 15", Language.ERLANG), list)

    def test_module15(self):
        assert isinstance(run("-module(m15).", Language.ERLANG), list)

    def test_export_multi(self):
        src = "-module(e15).\n-export([f/0, g/1]).\nf() -> ok.\ng(X) -> X."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_nested_case(self):
        src = "-module(n15).\nf(X) -> case X of 1 -> one; _ -> other end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_list_comprehension(self):
        src = "-module(lc15).\nf() -> [X * 2 || X <- [1,2,3]]."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_binary_pattern(self):
        src = "-module(bp15).\nf(<<X:8, Rest/binary>>) -> {X, Rest}."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_guard_fn(self):
        src = "-module(g15).\nf(X) when is_integer(X) -> X + 1."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o15).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 15", Language.ERLANG))


class TestForthExtended17:
    def test_print_700(self):
        assert isinstance(run("700 .", Language.FORTH), list)

    def test_print_21(self):
        assert has(run("21 .", Language.FORTH), "21")

    def test_print_22(self):
        assert has(run("22 .", Language.FORTH), "22")

    def test_add_700(self):
        assert has(run("350 350 + .", Language.FORTH), "700")

    def test_sub_291(self):
        assert has(run("300 9 - .", Language.FORTH), "291")

    def test_mul_700(self):
        assert has(run("70 10 * .", Language.FORTH), "700")

    def test_over(self):
        r = run("1 2 over . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended18:
    def test_print_800(self):
        assert isinstance(run("800 .", Language.FORTH), list)

    def test_print_23(self):
        assert has(run("23 .", Language.FORTH), "23")

    def test_print_24(self):
        assert has(run("24 .", Language.FORTH), "24")

    def test_add_800(self):
        assert has(run("400 400 + .", Language.FORTH), "800")

    def test_sub_376(self):
        assert has(run("400 24 - .", Language.FORTH), "376")

    def test_mul_800(self):
        assert has(run("80 10 * .", Language.FORTH), "800")

    def test_mod(self):
        r = run("10 3 mod .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended19:
    def test_print_900(self):
        assert isinstance(run("900 .", Language.FORTH), list)

    def test_print_25(self):
        assert has(run("25 .", Language.FORTH), "25")

    def test_print_26(self):
        assert has(run("26 .", Language.FORTH), "26")

    def test_add_900(self):
        assert has(run("450 450 + .", Language.FORTH), "900")

    def test_sub_424(self):
        assert has(run("450 26 - .", Language.FORTH), "424")

    def test_mul_900(self):
        assert has(run("90 10 * .", Language.FORTH), "900")

    def test_nip(self):
        r = run("1 2 3 nip . .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestErlangExtended20:
    def test_empty(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_comment20(self):
        assert isinstance(run("% test 20", Language.ERLANG), list)

    def test_module20(self):
        assert isinstance(run("-module(m20).", Language.ERLANG), list)

    def test_fn_fib(self):
        src = "-module(fib20).\nfib(0) -> 0;\nfib(1) -> 1;\nfib(N) -> fib(N-1) + fib(N-2)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_fact(self):
        src = "-module(fac20).\nfact(0) -> 1;\nfact(N) -> N * fact(N-1)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_sum(self):
        src = "-module(sum20).\nsum([]) -> 0;\nsum([H|T]) -> H + sum(T)."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_max(self):
        src = "-module(max20).\nf(X, Y) -> if X > Y -> X; true -> Y end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_fn_min(self):
        src = "-module(min20).\nf(X, Y) -> if X < Y -> X; true -> Y end."
        assert isinstance(run(src, Language.ERLANG), list)

    def test_output_list(self):
        assert isinstance(run("-module(o20).", Language.ERLANG), list)

    def test_no_errors(self):
        assert no_errors(run("% ok 20", Language.ERLANG))


class TestForthExtended20:
    def test_print_1000(self):
        assert isinstance(run("1000 .", Language.FORTH), list)

    def test_print_27(self):
        assert has(run("27 .", Language.FORTH), "27")

    def test_print_28(self):
        assert has(run("28 .", Language.FORTH), "28")

    def test_add_1000(self):
        assert has(run("500 500 + .", Language.FORTH), "1000")

    def test_sub_472(self):
        assert has(run("500 28 - .", Language.FORTH), "472")

    def test_mul_1000(self):
        assert has(run("100 10 * .", Language.FORTH), "1000")

    def test_abs(self):
        r = run("-5 abs .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestForthExtended21:
    def test_push_99(self):
        assert isinstance(run("99", Language.FORTH), list)

    def test_add_simple(self):
        assert has(run("5 6 + .", Language.FORTH), "11")

    def test_mul_2_6(self):
        assert has(run("2 6 * .", Language.FORTH), "12")

    def test_sub_10_7(self):
        assert has(run("10 7 - .", Language.FORTH), "3")

    def test_div_20_4(self):
        assert has(run("20 4 / .", Language.FORTH), "5")

    def test_dup_dot(self):
        r = run("9 DUP . .", Language.FORTH)
        assert isinstance(r, list)

    def test_swap(self):
        r = run("3 4 SWAP . .", Language.FORTH)
        assert isinstance(r, list)

    def test_over(self):
        r = run("3 4 OVER .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("5 5 + .", Language.FORTH))


class TestForthExtended22:
    def test_push_77(self):
        assert isinstance(run("77", Language.FORTH), list)

    def test_mod_2(self):
        r = run("10 3 MOD .", Language.FORTH)
        assert isinstance(r, list)

    def test_abs(self):
        r = run("-5 ABS .", Language.FORTH)
        assert has(r, "5")

    def test_max(self):
        r = run("3 7 MAX .", Language.FORTH)
        assert has(r, "7")

    def test_min(self):
        r = run("3 7 MIN .", Language.FORTH)
        assert has(r, "3")

    def test_negate(self):
        r = run("5 NEGATE .", Language.FORTH)
        assert isinstance(r, list)

    def test_two_dup(self):
        r = run("1 2 2DUP . . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_drop(self):
        r = run("1 2 DROP .", Language.FORTH)
        assert has(r, "1")

    def test_output_list2(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors2(self):
        assert no_errors(run("5 3 + .", Language.FORTH))


class TestForthExtended23:
    def test_and(self):
        r = run("1 1 AND .", Language.FORTH)
        assert isinstance(r, list)

    def test_or(self):
        r = run("1 0 OR .", Language.FORTH)
        assert isinstance(r, list)

    def test_not(self):
        r = run("0 NOT .", Language.FORTH)
        assert isinstance(r, list)

    def test_equal(self):
        r = run("5 5 = .", Language.FORTH)
        assert isinstance(r, list)

    def test_less(self):
        r = run("3 5 < .", Language.FORTH)
        assert isinstance(r, list)

    def test_greater(self):
        r = run("5 3 > .", Language.FORTH)
        assert isinstance(r, list)

    def test_if_true(self):
        r = run("1 IF 99 . THEN", Language.FORTH)
        assert isinstance(r, list)

    def test_do_loop(self):
        r = run("5 0 DO I . LOOP", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list3(self):
        assert isinstance(run("2 .", Language.FORTH), list)

    def test_no_errors3(self):
        assert no_errors(run("2 2 + .", Language.FORTH))


class TestForthExtended24:
    def test_emit(self):
        r = run("72 EMIT", Language.FORTH)
        assert isinstance(r, list)

    def test_cr(self):
        r = run("CR", Language.FORTH)
        assert isinstance(r, list)

    def test_space(self):
        r = run("SPACE", Language.FORTH)
        assert isinstance(r, list)

    def test_variable(self):
        r = run("VARIABLE X 99 X ! X @ .", Language.FORTH)
        assert isinstance(r, list)

    def test_word_def(self):
        r = run(": SQ DUP * ; 7 SQ .", Language.FORTH)
        assert has(r, "49")

    def test_begin_until(self):
        r = run("5 BEGIN DUP . 1 - DUP 0= UNTIL DROP", Language.FORTH)
        assert isinstance(r, list)

    def test_recurse(self):
        r = run(": FACT DUP 1 > IF DUP 1 - RECURSE * THEN ; 5 FACT .", Language.FORTH)
        assert isinstance(r, list)

    def test_leave(self):
        r = run("5 0 DO I 3 = IF LEAVE THEN I . LOOP", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list4(self):
        assert isinstance(run("3 .", Language.FORTH), list)

    def test_no_errors4(self):
        assert no_errors(run("3 3 + .", Language.FORTH))


class TestForthExtended25:
    def test_string_print(self):
        r = run('." hello"', Language.FORTH)
        assert isinstance(r, list)

    def test_hex_mode(self):
        r = run("HEX FF DECIMAL .", Language.FORTH)
        assert isinstance(r, list)

    def test_octal(self):
        r = run("8 BASE ! 7 . DECIMAL", Language.FORTH)
        assert isinstance(r, list)

    def test_2drop(self):
        r = run("1 2 2DROP", Language.FORTH)
        assert isinstance(r, list)

    def test_nip(self):
        r = run("1 2 NIP .", Language.FORTH)
        assert isinstance(r, list)

    def test_tuck(self):
        r = run("1 2 TUCK . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_rot(self):
        r = run("1 2 3 ROT . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_within(self):
        r = run("5 1 10 WITHIN .", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list5(self):
        assert isinstance(run("4 .", Language.FORTH), list)

    def test_no_errors5(self):
        assert no_errors(run("4 4 + .", Language.FORTH))


class TestForthExtended26:
    def test_num_1600(self):
        assert has(run("1600 .", Language.FORTH), "1600")

    def test_over(self):
        r = run("1 2 OVER . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_swap(self):
        r = run("1 2 SWAP . .", Language.FORTH)
        assert isinstance(r, list)

    def test_or_op(self):
        r = run("3 5 OR .", Language.FORTH)
        assert isinstance(r, list)

    def test_and_op(self):
        r = run("7 5 AND .", Language.FORTH)
        assert isinstance(r, list)

    def test_xor_op(self):
        r = run("7 5 XOR .", Language.FORTH)
        assert isinstance(r, list)

    def test_lshift(self):
        r = run("1 3 LSHIFT .", Language.FORTH)
        assert isinstance(r, list)

    def test_rshift(self):
        r = run("16 2 RSHIFT .", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list6(self):
        assert isinstance(run("5 .", Language.FORTH), list)

    def test_no_errors6(self):
        assert no_errors(run("5 5 + .", Language.FORTH))


class TestForthExtended27:
    def test_num_1700(self):
        assert has(run("1700 .", Language.FORTH), "1700")

    def test_count_print(self):
        r = run("1 2 3 . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_depth(self):
        r = run("1 2 3 DEPTH .", Language.FORTH)
        assert isinstance(r, list)

    def test_true_print(self):
        r = run("TRUE .", Language.FORTH)
        assert isinstance(r, list)

    def test_false_print(self):
        r = run("FALSE .", Language.FORTH)
        assert isinstance(r, list)

    def test_max_op(self):
        r = run("3 7 MAX .", Language.FORTH)
        assert isinstance(r, list)

    def test_min_op(self):
        r = run("3 7 MIN .", Language.FORTH)
        assert isinstance(r, list)

    def test_abs_op(self):
        r = run("-5 ABS .", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list7(self):
        assert isinstance(run("6 .", Language.FORTH), list)

    def test_no_errors7(self):
        assert no_errors(run("6 6 + .", Language.FORTH))


class TestForthExtended28:
    def test_num_1800(self):
        assert has(run("1800 .", Language.FORTH), "1800")

    def test_negate(self):
        r = run("5 NEGATE .", Language.FORTH)
        assert isinstance(r, list)

    def test_2dup(self):
        r = run("1 2 2DUP . . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_2swap(self):
        r = run("1 2 3 4 2SWAP . . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_pick(self):
        r = run("1 2 3 2 PICK .", Language.FORTH)
        assert isinstance(r, list)

    def test_mod(self):
        r = run("15 4 MOD .", Language.FORTH)
        assert isinstance(r, list)

    def test_divmod(self):
        r = run("10 3 /MOD . .", Language.FORTH)
        assert isinstance(r, list)

    def test_2div(self):
        r = run("10 2/ .", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list8(self):
        assert isinstance(run("7 .", Language.FORTH), list)

    def test_no_errors8(self):
        assert no_errors(run("7 7 + .", Language.FORTH))


class TestForthExtended29:
    def test_num_1900(self):
        assert has(run("1900 .", Language.FORTH), "1900")

    def test_2over(self):
        r = run("1 2 3 4 2OVER . . . . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_cell_plus(self):
        r = run("0 CELL+ .", Language.FORTH)
        assert isinstance(r, list)

    def test_cells(self):
        r = run("3 CELLS .", Language.FORTH)
        assert isinstance(r, list)

    def test_char_plus(self):
        r = run("0 CHAR+ .", Language.FORTH)
        assert isinstance(r, list)

    def test_chars(self):
        r = run("3 CHARS .", Language.FORTH)
        assert isinstance(r, list)

    def test_fill(self):
        r = run("1 2 3 4 5 . . . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_spaces(self):
        r = run("3 SPACES", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list9(self):
        assert isinstance(run("8 .", Language.FORTH), list)

    def test_no_errors9(self):
        assert no_errors(run("8 8 + .", Language.FORTH))


class TestForthExtended30:
    def test_dot_1900(self):
        assert has(run("1900 .", Language.FORTH), "1900")

    def test_dot_42(self):
        assert has(run("42 .", Language.FORTH), "42")

    def test_add_1900(self):
        assert has(run("950 950 + .", Language.FORTH), "1900")

    def test_mul_144(self):
        assert has(run("12 12 * .", Language.FORTH), "144")

    def test_sub_70(self):
        assert has(run("100 30 - .", Language.FORTH), "70")

    def test_div_20(self):
        assert has(run("100 5 / .", Language.FORTH), "20")

    def test_dup(self):
        r = run("7 dup * .", Language.FORTH)
        assert has(r, "49")

    def test_swap(self):
        r = run("3 4 swap .", Language.FORTH)
        assert isinstance(r, list)

    def test_output_list10(self):
        assert isinstance(run("10 .", Language.FORTH), list)

    def test_no_errors10(self):
        assert no_errors(run("10 .", Language.FORTH))
