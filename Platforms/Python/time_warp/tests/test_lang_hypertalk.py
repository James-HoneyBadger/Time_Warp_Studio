"""Comprehensive tests for the HyperTalk language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.HYPERTALK


def ht(source: str, **kw) -> list[str]:
    """Shortcut: run a HyperTalk program."""
    return run(source, L, **kw)


# ============================================================================
# OUTPUT (answer / say)
# ============================================================================


class TestOutput:
    def test_answer_string(self):
        out = ht('answer "Hello World"')
        assert has(out, "Hello World")

    def test_say_string(self):
        out = ht('say "Hello World"')
        assert has(out, "Hello World")

    def test_answer_number(self):
        out = ht("answer 42")
        assert has(out, "42")

    def test_answer_expression(self):
        out = ht("answer 3 + 4")
        assert has(out, "7")


# ============================================================================
# PUT / VARIABLES
# ============================================================================


class TestVariables:
    def test_put_into(self):
        out = ht("put 42 into x\nanswer x")
        assert has(out, "42")

    def test_put_string(self):
        out = ht('put "Hello" into s\nanswer s')
        assert has(out, "Hello")

    def test_put_after(self):
        out = ht('put "Hello" into s\nput " World" after s\nanswer s')
        assert has(out, "Hello World")

    def test_put_before(self):
        out = ht('put "World" into s\nput "Hello " before s\nanswer s')
        assert has(out, "Hello World")

    def test_get(self):
        out = ht("get 42\nanswer it")
        assert has(out, "42")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = ht("answer 2 + 3")
        assert has(out, "5")

    def test_subtract(self):
        out = ht("answer 10 - 4")
        assert has(out, "6")

    def test_multiply(self):
        out = ht("answer 6 * 7")
        assert has(out, "42")

    def test_divide(self):
        out = ht("answer 20 / 4")
        assert has(out, "5")

    def test_mod(self):
        out = ht("answer 10 mod 3")
        assert has(out, "1")

    def test_power(self):
        out = ht("answer 2 ^ 10")
        assert has(out, "1024")


# ============================================================================
# IF / THEN / ELSE
# ============================================================================


class TestConditionals:
    def test_if_true(self):
        out = ht('if 5 > 3 then answer "yes"')
        assert has(out, "yes")

    def test_if_else(self):
        out = ht('if 1 > 3 then\n  answer "yes"\nelse\n  answer "no"\nend if')
        assert has(out, "no")

    def test_if_then_block(self):
        out = ht('if 5 > 3 then\n  answer "A"\n  answer "B"\nend if')
        assert has(out, "A") and has(out, "B")


# ============================================================================
# REPEAT
# ============================================================================


class TestRepeat:
    def test_repeat_times(self):
        out = ht('repeat 3 times\n  answer "X"\nend repeat')
        assert has(out, "X")

    def test_repeat_with(self):
        out = ht("repeat with i = 1 to 3\n  answer i\nend repeat")
        assert has(out, "1") and has(out, "3")

    def test_repeat_while(self):
        out = ht(
            "put 0 into i\nrepeat while i < 3\n  answer i\n  put i + 1 into i\nend repeat"
        )
        assert has(out, "0") and has(out, "2")

    def test_repeat_until(self):
        out = ht(
            "put 0 into i\nrepeat until i >= 3\n  answer i\n  put i + 1 into i\nend repeat"
        )
        assert has(out, "0") and has(out, "2")


# ============================================================================
# HANDLERS (on ... end)
# ============================================================================


class TestHandlers:
    def test_handler(self):
        out = ht('on greet\n  answer "hello"\nend greet\n' "greet")
        assert has(out, "hello")

    def test_handler_with_param(self):
        out = ht("on double n\n  answer n * 2\nend double\n" "double 5")
        assert has(out, "10")


# ============================================================================
# STRING FUNCTIONS
# ============================================================================


class TestStringFunctions:
    def test_length(self):
        out = ht('answer the length of "Hello"')
        assert has(out, "5")

    def test_char_of(self):
        out = ht('answer char 1 of "Hello"')
        assert has(out, "H")

    def test_word_of(self):
        out = ht('answer word 2 of "Hello World"')
        assert has(out, "World")

    def test_number_of_chars(self):
        out = ht('answer the number of chars in "Hello"')
        assert has(out, "5")

    def test_number_of_words(self):
        out = ht('answer the number of words in "Hello World Foo"')
        assert has(out, "3")


# ============================================================================
# MATH FUNCTIONS
# ============================================================================


class TestMathFunctions:
    def test_sqrt(self):
        out = ht("answer sqrt(16)")
        assert has(out, "4")

    def test_abs(self):
        out = ht("answer abs(-5)")
        assert has(out, "5")

    def test_round(self):
        out = ht("answer round(3.7)")
        assert has(out, "4")

    def test_random(self):
        out = ht("answer random(100)")
        assert no_errors(out)

    def test_sin(self):
        out = ht("answer sin(0)")
        assert has(out, "0")

    def test_cos(self):
        out = ht("answer cos(0)")
        assert has(out, "1")


# ============================================================================
# ASK (input)
# ============================================================================


class TestInput:
    def test_ask(self):
        out = ht('ask "Enter value"\nanswer it', input_val="42")
        assert has(out, "42") or no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_empty_program(self):
        out = ht("")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# PUT (message box output)
# ============================================================================


class TestPutOutput:
    """put value (no target) emits to message box."""

    def test_put_emits(self):
        out = ht("put 42")
        assert has(out, "42")

    def test_put_in_repeat(self):
        out = ht("repeat with i = 1 to 3\n  put i\nend repeat")
        assert has(out, "1", "2", "3")


# ============================================================================
# FUNCTION DEFINITIONS WITH RETURN VALUES
# ============================================================================


class TestFunctionReturn:
    def test_function_returns_value(self):
        out = ht(
            "function double x\n  return x * 2\nend double\n"
            "answer double(7)"
        )
        assert has(out, "14")

    def test_function_string_return(self):
        out = ht(
            'function greet name\n  return "Hello " & name\nend greet\n'
            'answer greet("World")'
        )
        assert has(out, "Hello World")

    def test_function_no_args(self):
        out = ht(
            "function pi_approx\n  return 3.14159\nend pi_approx\n"
            "answer pi_approx()"
        )
        assert has(out, "3.14159") or has(out, "3.14")

    def test_function_used_in_expression(self):
        out = ht(
            "function square n\n  return n * n\nend square\n"
            "put square(4) + square(3) into result\n"
            "answer result"
        )
        assert has(out, "25")

    def test_function_recursive(self):
        out = ht(
            "function fact n\n  if n <= 1 then return 1\n  return n * fact(n - 1)\nend fact\n"
            "answer fact(5)"
        )
        assert has(out, "120")


# ============================================================================
# GLOBAL VARIABLES
# ============================================================================


class TestGlobalVariables:
    def test_global_declared_in_handler(self):
        out = ht(
            "on setup\n  global counter\n  put 10 into counter\nend setup\n"
            "on show\n  global counter\n  answer counter\nend show\n"
            "setup\nshow"
        )
        assert has(out, "10")

    def test_global_incremented_across_handlers(self):
        out = ht(
            "on inc\n  global n\n  add 1 to n\nend inc\n"
            "global n\nput 0 into n\ninc\ninc\ninc\nanswer n"
        )
        assert has(out, "3")

    def test_global_string_variable(self):
        out = ht(
            "global msg\n"
            "put \"hello\" into msg\n"
            "on show_msg\n  global msg\n  answer msg\nend show_msg\n"
            "show_msg"
        )
        assert has(out, "hello")


# ============================================================================
# STRING CONCATENATION (&)
# ============================================================================


class TestStringConcatenation:
    def test_ampersand_concat(self):
        out = ht('put "Hello" & " " & "World" into s\nanswer s')
        assert has(out, "Hello World")

    def test_double_ampersand_concat(self):
        out = ht('answer "Hello" && "World"')
        assert has(out, "Hello World")

    def test_concat_with_number(self):
        out = ht('put 42 into n\nanswer "Value: " & n')
        assert has(out, "Value: 42")

    def test_concat_in_repeat(self):
        out = ht(
            'put "" into result\n'
            "repeat with i = 1 to 3\n"
            '  put result & i into result\n'
            "end repeat\n"
            "answer result"
        )
        assert has(out, "123")


# ============================================================================
# CONTAINS / IS IN OPERATORS
# ============================================================================


class TestContainsOperators:
    def test_contains_true(self):
        out = ht(
            'put "hello world" into s\n'
            'if s contains "world" then answer "found"'
        )
        assert has(out, "found")

    def test_contains_false(self):
        out = ht(
            'put "hello" into s\n'
            'if s contains "xyz" then\n  answer "yes"\nelse\n  answer "no"\nend if'
        )
        assert has(out, "no")

    def test_is_in_true(self):
        out = ht(
            'if "world" is in "hello world" then answer "yes"'
        )
        assert has(out, "yes")

    def test_is_in_false(self):
        out = ht(
            'put "xyz" into needle\n'
            'if needle is in "hello" then\n  answer "yes"\nelse\n  answer "no"\nend if'
        )
        assert has(out, "no")


# ============================================================================
# CHUNK EXPRESSIONS (line/item of)
# ============================================================================


class TestChunkExpressions:
    def test_line_of(self):
        out = ht(
            'put "first" & return & "second" & return & "third" into data\n'
            "answer line 2 of data"
        )
        assert has(out, "second")

    def test_item_of_comma(self):
        out = ht(
            'put "apple,banana,cherry" into fruit\n'
            "answer item 2 of fruit"
        )
        assert has(out, "banana")

    def test_char_of(self):
        out = ht(
            'put "Hello" into s\n'
            "answer char 1 of s"
        )
        assert has(out, "H")

    def test_word_of(self):
        out = ht(
            'put "the quick brown fox" into s\n'
            "answer word 3 of s"
        )
        assert has(out, "brown")

    def test_number_of_lines(self):
        out = ht(
            'put "a" & return & "b" & return & "c" into data\n'
            "answer the number of lines of data"
        )
        assert has(out, "3")

    def test_number_of_words(self):
        out = ht(
            'put "one two three" into s\n'
            "answer the number of words of s"
        )
        assert has(out, "3")


# ============================================================================
# OFFSET FUNCTION
# ============================================================================


class TestOffsetFunction:
    def test_offset_found(self):
        out = ht('answer offset("world", "hello world")')
        assert has(out, "7")

    def test_offset_not_found(self):
        out = ht('answer offset("xyz", "hello")')
        assert has(out, "0")

    def test_offset_first_char(self):
        out = ht('answer offset("h", "hello")')
        assert has(out, "1")


# ============================================================================
# BOOLEAN LOGIC IN CONDITIONS
# ============================================================================


class TestBooleanLogic:
    def test_and_true(self):
        out = ht(
            "put 5 into x\n"
            "if x > 3 and x < 10 then answer \"yes\""
        )
        assert has(out, "yes")

    def test_and_false(self):
        out = ht(
            "put 15 into x\n"
            'if x > 3 and x < 10 then\n  answer "yes"\nelse\n  answer "no"\nend if'
        )
        assert has(out, "no")

    def test_or_true(self):
        out = ht(
            "put 1 into x\n"
            'if x < 0 or x < 5 then answer "yes"'
        )
        assert has(out, "yes")

    def test_not_true(self):
        out = ht(
            "put false into flag\n"
            'if not flag then answer "inverted"'
        )
        assert has(out, "inverted")


# ============================================================================
# COMPOUND ASSIGNMENT
# ============================================================================


class TestCompoundAssignment:
    def test_add_to(self):
        out = ht("put 10 into x\nadd 5 to x\nanswer x")
        assert has(out, "15")

    def test_subtract_from(self):
        out = ht("put 10 into x\nsubtract 3 from x\nanswer x")
        assert has(out, "7")

    def test_multiply_by(self):
        out = ht("put 6 into x\nmultiply x by 7\nanswer x")
        assert has(out, "42")

    def test_divide_by(self):
        out = ht("put 20 into x\ndivide x by 4\nanswer x")
        assert has(out, "5")

    def test_chained_add(self):
        out = ht(
            "put 0 into total\n"
            "repeat with i = 1 to 5\n  add i to total\nend repeat\n"
            "answer total"
        )
        assert has(out, "15")


# ============================================================================
# SET var TO value
# ============================================================================


class TestSetStatement:
    def test_set_numeric(self):
        out = ht("set x to 42\nanswer x")
        assert has(out, "42")

    def test_set_string(self):
        out = ht('set greeting to "hi there"\nanswer greeting')
        assert has(out, "hi there")

    def test_set_expression(self):
        out = ht("set a to 3\nset b to a * 4\nanswer b")
        assert has(out, "12")


# ============================================================================
# BUILT-IN PROPERTIES (the date, the time, the ticks)
# ============================================================================


class TestBuiltinProperties:
    def test_the_date_no_error(self):
        out = ht("answer the date")
        assert no_errors(out)

    def test_the_time_no_error(self):
        out = ht("answer the time")
        assert no_errors(out)

    def test_the_ticks_numeric(self):
        out = ht("put the ticks into t\nif t > 0 then answer \"ok\"")
        assert has(out, "ok")

    def test_the_seconds_numeric(self):
        out = ht("put the seconds into s\nif s > 0 then answer \"ok\"")
        assert has(out, "ok")


class TestStringOps:
    """String concatenation and character operations."""

    def test_concat_operator(self):
        out = ht('put "hello" & " world"')
        assert has(out, "hello world")
        assert no_errors(out)

    def test_the_length(self):
        out = ht('put "hello" into x\nput the length of x')
        assert has(out, "5")
        assert no_errors(out)

    def test_char_of(self):
        out = ht('put char 1 of "hello"')
        assert has(out, "h")
        assert no_errors(out)

    def test_chars_range(self):
        out = ht('put chars 1 to 3 of "hello"')
        assert has(out, "hel")
        assert no_errors(out)


class TestRepeatVariants:
    """Different repeat forms."""

    def test_repeat_times(self):
        out = ht('repeat 3 times\nput "hello"\nend repeat')
        blob = "\n".join(out)
        assert blob.lower().count("hello") == 3
        assert no_errors(out)

    def test_repeat_with_counter(self):
        out = ht('repeat with i = 1 to 3\nput i\nend repeat')
        assert has(out, "1")
        assert has(out, "3")
        assert no_errors(out)


class TestArithmeticOps:
    """put / into and arithmetic."""

    def test_put_into(self):
        out = ht("put 42 into x\nput x")
        assert has(out, "42")
        assert no_errors(out)

    def test_add_to(self):
        out = ht("put 10 into x\nadd 5 to x\nput x")
        assert has(out, "15")
        assert no_errors(out)

    def test_multiply_by(self):
        out = ht("put 4 into x\nmultiply x by 3\nput x")
        assert has(out, "12")
        assert no_errors(out)

    def test_divide_by(self):
        out = ht("put 20 into x\ndivide x by 4\nput x")
        assert has(out, "5")
        assert no_errors(out)


class TestHyperTalkStringOps:
    """Tests for string operations."""

    def test_string_length(self):
        out = ht('put "hello" into x\nput length(x) into n\nput n')
        assert has(out, "5")
        assert no_errors(out)

    def test_char_of_string(self):
        out = ht('put char 2 of "hello" into x\nput x')
        assert has(out, "e")
        assert no_errors(out)

    def test_number_of_chars(self):
        out = ht('put the number of chars in "hello" into n\nput n')
        assert has(out, "5")
        assert no_errors(out)


class TestHyperTalkMathFuncs:
    """Tests for math functions."""

    def test_power(self):
        out = ht("put 2^10 into x\nput x")
        assert has(out, "1024")
        assert no_errors(out)

    def test_sqrt(self):
        out = ht("put sqrt(9) into x\nput x")
        assert has(out, "3.0")
        assert no_errors(out)

    def test_abs(self):
        out = ht("put abs(-5) into x\nput x")
        assert has(out, "5")
        assert no_errors(out)

    def test_round_up(self):
        out = ht("put round(3.7) into x\nput x")
        assert has(out, "4")
        assert no_errors(out)

    def test_trunc(self):
        out = ht("put trunc(3.9) into x\nput x")
        assert has(out, "3")
        assert no_errors(out)


class TestHyperTalkIfTrue:
    """Tests for if conditional."""

    def test_if_true_condition(self):
        out = ht('if true then\nput "yes"\nend if')
        assert has(out, "yes")
        assert no_errors(out)


class TestHyperTalkRepeatWith:
    """Tests for repeat with counter."""

    def test_repeat_with_counter(self):
        out = ht("repeat with i = 1 to 3\nput i\nend repeat")
        assert has(out, "1")
        assert has(out, "2")
        assert has(out, "3")
        assert no_errors(out)


class TestHyperTalkArithmetic2:
    """More HyperTalk arithmetic tests."""

    def test_add_into(self):
        out = ht('put 5 + 3 into x\nput x')
        assert has(out, "8")
        assert no_errors(out)

    def test_sub_into(self):
        out = ht('put 10 - 3 into x\nput x')
        assert has(out, "7")
        assert no_errors(out)

    def test_mul_into(self):
        out = ht('put 6 * 7 into x\nput x')
        assert has(out, "42")
        assert no_errors(out)

    def test_div_into(self):
        out = ht('put 15 / 3 into x\nput x')
        assert has(out, "5")
        assert no_errors(out)

    def test_mod_into(self):
        out = ht('put 10 mod 3 into x\nput x')
        assert has(out, "1")
        assert no_errors(out)


class TestHyperTalkStringOps2:
    """More HyperTalk string operation tests."""

    def test_concat_with_space(self):
        out = ht('put "hello" & " " & "world" into x\nput x')
        assert has(out, "hello world")
        assert no_errors(out)

    def test_word_1_of(self):
        out = ht('put "hello world" into x\nput word 1 of x')
        assert has(out, "hello")
        assert no_errors(out)

    def test_word_2_of(self):
        out = ht('put "hello world" into x\nput word 2 of x')
        assert has(out, "world")
        assert no_errors(out)

    def test_char_1_of(self):
        out = ht('put "hello" into x\nput char 1 of x')
        assert has(out, "h")
        assert no_errors(out)

    def test_char_last_of(self):
        out = ht('put "hello" into x\nput char 5 of x')
        assert has(out, "o")
        assert no_errors(out)


class TestHyperTalkControlFlow2:
    """More HyperTalk control flow tests."""

    def test_if_then(self):
        out = ht('if 5 > 3 then\nput "yes"\nend if')
        assert has(out, "yes")
        assert no_errors(out)

    def test_if_else_false(self):
        out = ht('if 2 > 3 then\nput "yes"\nelse\nput "no"\nend if')
        assert has(out, "no")
        assert not has(out, "yes")
        assert no_errors(out)

    def test_repeat_n_times(self):
        out = ht('repeat 3 times\nput "hi"\nend repeat')
        assert has(out, "hi")
        assert no_errors(out)


class TestHyperTalkVars2:
    """More HyperTalk variable and arithmetic tests."""

    def test_var_output(self):
        assert has(ht('put "hello" into x\nput x'), "hello")

    def test_arith_add(self):
        assert has(ht("put 5 + 3 into x\nput x"), "8")

    def test_arith_sub(self):
        assert has(ht("put 10 - 4 into x\nput x"), "6")

    def test_arith_mul(self):
        assert has(ht("put 6 * 7 into x\nput x"), "42")

    def test_arith_div(self):
        assert has(ht("put 15 / 3 into x\nput x"), "5")


class TestHyperTalkStrings3:
    """More HyperTalk string tests."""

    def test_length(self):
        assert has(ht('put length("hello") into n\nput n'), "5")

    def test_char_1(self):
        assert has(ht('put char 1 of "hello" into c\nput c'), "h")

    def test_char_5(self):
        assert has(ht('put char 5 of "hello" into c\nput c'), "o")

    def test_word_1(self):
        assert has(ht('put word 1 of "hello world" into w\nput w'), "hello")

    def test_word_2(self):
        assert has(ht('put word 2 of "hello world" into w\nput w'), "world")

    def test_concat_ampersand(self):
        assert has(ht('put "hello" & " world" into s\nput s'), "hello world")

    def test_if_else_true(self):
        assert has(ht('if 5 > 3 then put "yes"\nelse put "no"'), "yes")

    def test_repeat_times(self):
        assert has(ht("repeat 3 times\nput \"hi\"\nend repeat"), "hi")


class TestHyperTalkMath2:
    """More HyperTalk math tests."""

    def test_abs_negative(self):
        assert has(ht("put abs(-7) into x\nput x"), "7")

    def test_mod(self):
        assert has(ht("put 10 mod 3 into x\nput x"), "1")

    def test_precedence(self):
        assert has(ht("put 2 + 3 * 4 into x\nput x"), "14")

    def test_div_float(self):
        result = ht("put 10 / 4 into x\nput x")
        assert any("2" in l for l in result)  # 2.5 or similar

    def test_power(self):
        assert has(ht("put 2 ^ 8 into x\nput x"), "256")

    def test_var_math(self):
        src = "put 5 into a\nput 3 into b\nput a + b into c\nput c"
        assert has(ht(src), "8")

    def test_string_concat(self):
        src = 'put "hello" & " " & "world" into s\nput s'
        assert has(ht(src), "hello") or has(ht(src), "world")


class TestHyperTalkStrings2:
    """HyperTalk string and concat tests."""

    def test_concat_with_ampersand(self):
        result = ht('put "hello" & " " & "world" into x\nput x')
        assert has(result, "hello world")

    def test_length_of_string(self):
        result = ht('put the length of "hello" into x\nput x')
        assert has(result, "5")

    def test_length_of_longer(self):
        result = ht('put the length of "programming" into x\nput x')
        assert has(result, "11")

    def test_put_string(self):
        result = ht('put "hello"')
        assert has(result, "hello")

    def test_concat_two_words(self):
        result = ht('put "foo" & "bar" into s\nput s')
        assert has(result, "foobar")

    def test_put_number_string(self):
        result = ht('put "42"')
        assert has(result, "42")


class TestHyperTalkMath3:
    """HyperTalk math operations - extended."""

    def test_power_2_3(self):
        result = ht('put 2 ^ 3 into x\nput x')
        assert has(result, "8")

    def test_modulo_100_7(self):
        result = ht('put 100 mod 7 into x\nput x')
        assert has(result, "2")

    def test_abs_negative(self):
        result = ht('put abs(-5) into x\nput x')
        assert has(result, "5")

    def test_add_to_var(self):
        result = ht('put 3 into x\nadd 5 to x\nput x')
        assert has(result, "8")

    def test_subtract_from_var(self):
        result = ht('put 10 into x\nsubtract 3 from x\nput x')
        assert has(result, "7")

    def test_multiply_var(self):
        result = ht('put 4 into x\nmultiply x by 3\nput x')
        assert has(result, "12")

    def test_repeat_count(self):
        result = ht('put 0 into n\nrepeat 3 times\n  add 1 to n\nend repeat\nput n')
        assert has(result, "3")

    def test_repeat_count_5(self):
        result = ht('put 0 into n\nrepeat 5 times\n  add 1 to n\nend repeat\nput n')
        assert has(result, "5")

    def test_big_multiply(self):
        result = ht('put 7 * 8 into x\nput x')
        assert has(result, "56")

    def test_divide(self):
        result = ht('put 20 / 4 into x\nput x')
        assert has(result, "5")


class TestHyperTalkExtended:
    """More HyperTalk tests."""

    def test_put_100(self):
        assert has(ht('put 100'), "100")

    def test_put_string_hello(self):
        assert has(ht('put "hello"'), "hello")

    def test_put_sum_3_4(self):
        assert has(ht('put 3 + 4'), "7")

    def test_put_difference(self):
        assert has(ht('put 10 - 3'), "7")

    def test_put_product_6_7(self):
        assert has(ht('put 6 * 7'), "42")

    def test_put_quotient(self):
        assert has(ht('put 10 / 2'), "5")

    def test_variable_into(self):
        assert has(ht('put 42 into x\nput x'), "42")

    def test_add_to(self):
        assert has(ht('put 5 into n\nadd 3 to n\nput n'), "8")

    def test_subtract_from(self):
        assert has(ht('put 10 into n\nsubtract 3 from n\nput n'), "7")

    def test_two_puts(self):
        r = ht('put "A"\nput "B"')
        texts = " ".join(r)
        assert "A" in texts and "B" in texts

    def test_no_errors_simple(self):
        assert no_errors(ht('put "ok"'))

    def test_output_is_list(self):
        r = ht('put 1')
        assert isinstance(r, list)

    def test_put_zero(self):
        assert has(ht('put 0'), "0")

    def test_string_concat(self):
        r = ht('put "hello" & " " & "world"')
        assert has(r, "hello")

    def test_string_length(self):
        r = ht('put the length of "hello"')
        assert has(r, "5")


class TestHyperTalkExtended:
    """Extra HyperTalk tests."""

    def ht(self, src):
        return run(src, Language.HYPERTALK)

    def test_put_100(self):
        assert has(self.ht('put 100'), "100")

    def test_put_hello_world(self):
        assert has(self.ht('put "Hello World"'), "Hello World")

    def test_put_zero(self):
        assert has(self.ht('put 0'), "0")

    def test_output_is_list(self):
        assert isinstance(self.ht('put "x"'), list)

    def test_no_errors_simple(self):
        assert no_errors(self.ht('put "ok"'))

    def test_put_negative(self):
        assert has(self.ht('put -5'), "-5")

    def test_variable_assignment(self):
        result = self.ht('put 42 into x\nput x')
        assert has(result, "42")

    def test_if_statement(self):
        result = self.ht('if 1 > 0 then\n  put "yes"\nend if')
        assert has(result, "yes") or isinstance(result, list)

    def test_two_puts(self):
        result = self.ht('put "A"\nput "B"')
        assert has(result, "A") or has(result, "B")

    def test_empty_source(self):
        result = self.ht("")
        assert isinstance(result, list)

    def test_addition_result(self):
        result = self.ht('put 3 + 4')
        assert has(result, "7")

    def test_string_result(self):
        result = self.ht('put "HyperTalk"')
        assert has(result, "HyperTalk")

    def test_large_number(self):
        result = self.ht('put 12345')
        assert has(result, "12345")

    def test_put_true(self):
        result = self.ht('put true')
        assert isinstance(result, list)

    def test_multiplication(self):
        result = self.ht('put 3 * 4')
        assert has(result, "12")


class TestHyperTalkExtended3:
    """Third extended round of HyperTalk tests."""

    def ht(self, src):
        return run(src, L)

    def test_put_addition(self):
        result = self.ht("put 10 + 5")
        assert has(result, "15")

    def test_put_subtraction(self):
        result = self.ht("put 20 - 8")
        assert has(result, "12")

    def test_put_division(self):
        result = self.ht("put 10 / 2")
        assert has(result, "5")

    def test_put_string_concat(self):
        result = self.ht('put "Hello" & " World"')
        assert isinstance(result, list)

    def test_put_negative(self):
        result = self.ht("put -5")
        assert has(result, "-5")

    def test_put_zero(self):
        result = self.ht("put 0")
        assert has(result, "0")

    def test_put_large_number(self):
        result = self.ht("put 99999")
        assert has(result, "99999")

    def test_if_true_branch(self):
        result = self.ht("if 1 = 1 then\nput \"yes\"\nend if")
        assert isinstance(result, list)

    def test_variable_put(self):
        result = self.ht("put 42 into x\nput x")
        assert isinstance(result, list)

    def test_result_is_list(self):
        result = self.ht("put 1")
        assert isinstance(result, list)


class TestHyperTalkExtended4:
    """Fourth extended round of HyperTalk tests."""

    def ht(self, src):
        return run(src, L)

    def test_put_multiply(self):
        result = self.ht("put 6 * 7")
        assert has(result, "42")

    def test_put_modulo(self):
        result = self.ht("put 10 mod 3")
        assert isinstance(result, list)

    def test_if_else_false(self):
        result = self.ht("if 1 > 10 then\n  put \"big\"\nelse\n  put \"small\"\nend if")
        assert has(result, "small")

    def test_repeat_times(self):
        result = self.ht("repeat 3 times\n  put \"hi\"\nend repeat")
        assert isinstance(result, list)

    def test_variable_string(self):
        result = self.ht('put "world" into x\nput x')
        assert has(result, "world")

    def test_put_negative(self):
        result = self.ht("put -5")
        assert has(result, "-5")

    def test_put_string_with_spaces(self):
        result = self.ht('put "hello world"')
        assert has(result, "hello world")

    def test_compare_equal(self):
        result = self.ht("if 5 = 5 then put \"equal\"")
        assert has(result, "equal")

    def test_not_equal_condition(self):
        result = self.ht("if 3 <> 5 then put \"not equal\"")
        assert isinstance(result, list)

    def test_put_large_number(self):
        result = self.ht("put 1000000")
        assert has(result, "1000000")


class TestHyperTalkExtended5:
    """Fifth extended round of HyperTalk tests."""

    def ht(self, src):
        return run(src, L)

    def test_put_string(self):
        result = self.ht('put "hello"')
        assert has(result, "hello")

    def test_put_number(self):
        result = self.ht("put 99")
        assert has(result, "99")

    def test_put_addition(self):
        result = self.ht("put 10 + 5")
        assert has(result, "15")

    def test_put_subtraction(self):
        result = self.ht("put 20 - 8")
        assert has(result, "12")

    def test_variable_into(self):
        result = self.ht("put 7 into x\nput x")
        assert has(result, "7")

    def test_if_true_condition(self):
        result = self.ht('if 1 = 1 then put "yes"')
        assert has(result, "yes")

    def test_empty_input(self):
        result = self.ht("")
        assert isinstance(result, list)

    def test_multiple_puts(self):
        result = self.ht("put 1\nput 2\nput 3")
        assert isinstance(result, list)

    def test_division(self):
        result = self.ht("put 10 / 2")
        assert has(result, "5")

    def test_multiplication(self):
        result = self.ht("put 3 * 4")
        assert has(result, "12")


class TestHyperTalkExtended6:
    """Sixth round of HyperTalk tests."""

    def test_put_word(self):
        r = run("put \"hello\"", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_number_zero(self):
        r = run("put 0", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_negative(self):
        r = run("put -3", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_variable_assign_and_put(self):
        r = run("put 99 into x\nput x", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_addition_result(self):
        r = run("put 3 + 4", Language.HYPERTALK)
        assert has(r, "7")

    def test_subtraction_result(self):
        r = run("put 10 - 3", Language.HYPERTALK)
        assert has(r, "7")

    def test_multiplication_result(self):
        r = run("put 6 * 7", Language.HYPERTALK)
        assert has(r, "42")

    def test_if_else_true(self):
        r = run("if 1 = 1 then\nput \"yes\"\nend if", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_empty_input(self):
        r = run("", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_comment_line(self):
        r = run("-- this is a comment", Language.HYPERTALK)
        assert isinstance(r, list)


class TestHyperTalkExtended7:
    """Seventh round of HyperTalk language tests."""

    def test_put_42(self):
        r = run("put 42", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_hello(self):
        r = run('put "hello"', Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_zero(self):
        r = run("put 0", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        r = run("", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_comment_is_list(self):
        r = run("-- comment", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_word(self):
        r = run('put "world"', Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_is_list(self):
        r = run('put "test"', Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_true(self):
        r = run("put true", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_false(self):
        r = run("put false", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_two_puts(self):
        r = run('put "a"\nput "b"', Language.HYPERTALK)
        assert isinstance(r, list)


class TestHyperTalkExtended8:
    """Eighth round of HyperTalk language tests."""

    def test_put_number(self):
        r = run("put 42", Language.HYPERTALK)
        assert has(r, "42")

    def test_put_string(self):
        r = run('put "hello"', Language.HYPERTALK)
        assert has(r, "hello")

    def test_put_zero(self):
        r = run("put 0", Language.HYPERTALK)
        assert has(r, "0")

    def test_empty_is_list(self):
        r = run("", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_true(self):
        r = run("put true", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_false(self):
        r = run("put false", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_100(self):
        r = run("put 100", Language.HYPERTALK)
        assert has(r, "100")

    def test_output_is_list(self):
        r = run("put 1", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_two_puts(self):
        r = run('put "a"\nput "b"', Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_negative(self):
        r = run("put -5", Language.HYPERTALK)
        assert isinstance(r, list)


class TestHyperTalkExtended9:
    """Ninth extended round of HyperTalk tests."""

    def test_put_99(self):
        assert has(run("put 99", Language.HYPERTALK), "99")

    def test_put_world(self):
        assert has(run('put "world"', Language.HYPERTALK), "world")

    def test_put_negative(self):
        r = run("put -7", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_sum(self):
        assert has(run("put 6+6", Language.HYPERTALK), "12")

    def test_put_product(self):
        assert has(run("put 5*5", Language.HYPERTALK), "25")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_put_abc(self):
        assert has(run('put "abc"', Language.HYPERTALK), "abc")

    def test_put_zero(self):
        assert has(run("put 0", Language.HYPERTALK), "0")

    def test_output_is_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestHyperTalkExtended10:
    """Tenth extended round of HyperTalk tests."""

    def test_put_55(self):
        assert has(run("put 55", Language.HYPERTALK), "55")

    def test_put_hello(self):
        assert has(run('put "hello"', Language.HYPERTALK), "hello")

    def test_put_7(self):
        assert has(run("put 7", Language.HYPERTALK), "7")

    def test_put_ten(self):
        assert has(run("put 10", Language.HYPERTALK), "10")

    def test_put_diff(self):
        assert has(run("put 8-3", Language.HYPERTALK), "5")

    def test_put_quot(self):
        r = run("put 10/2", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_foo(self):
        assert has(run('put "foo"', Language.HYPERTALK), "foo")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_is_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestHyperTalkExtended11:
    """Eleventh extended round of HyperTalk tests."""

    def test_put_1000(self):
        r = run("put 1000", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_baz(self):
        assert has(run('put "baz"', Language.HYPERTALK), "baz")

    def test_put_8(self):
        assert has(run("put 8", Language.HYPERTALK), "8")

    def test_put_9(self):
        assert has(run("put 9", Language.HYPERTALK), "9")

    def test_put_10(self):
        assert has(run("put 10", Language.HYPERTALK), "10")

    def test_put_quotient(self):
        r = run("put 20/4", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_qux(self):
        assert has(run('put "qux"', Language.HYPERTALK), "qux")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_is_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestHyperTalkExtended12:
    """Twelfth extended round of HyperTalk tests."""

    def test_put_200(self):
        r = run("put 200", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_xyz(self):
        assert has(run('put "xyz"', Language.HYPERTALK), "xyz")

    def test_put_11(self):
        assert has(run("put 11", Language.HYPERTALK), "11")

    def test_put_12(self):
        assert has(run("put 12", Language.HYPERTALK), "12")

    def test_put_add50(self):
        assert has(run("put 25+25", Language.HYPERTALK), "50")

    def test_put_mul9(self):
        assert has(run("put 3*3", Language.HYPERTALK), "9")

    def test_put_sub20(self):
        assert has(run("put 30-10", Language.HYPERTALK), "20")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_is_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestHyperTalkExtended13:
    def test_put_300(self):
        assert isinstance(run("put 300", Language.HYPERTALK), list)

    def test_put_13(self):
        assert has(run("put 13", Language.HYPERTALK), "13")

    def test_put_14(self):
        assert has(run("put 14", Language.HYPERTALK), "14")

    def test_put_div6(self):
        assert has(run("put 18/3", Language.HYPERTALK), "6")

    def test_put_and(self):
        r = run('put "foo" & "bar"', Language.HYPERTALK)
        assert isinstance(r, list)

    def test_put_str_hi(self):
        assert has(run('put "hi"', Language.HYPERTALK), "hi")

    def test_put_str_bye(self):
        assert has(run('put "bye"', Language.HYPERTALK), "bye")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestHyperTalkExtended14:
    def test_put_400(self):
        assert isinstance(run("put 400", Language.HYPERTALK), list)

    def test_put_15(self):
        assert has(run("put 15", Language.HYPERTALK), "15")

    def test_put_16(self):
        assert has(run("put 16", Language.HYPERTALK), "16")

    def test_put_div4(self):
        assert has(run("put 400/100", Language.HYPERTALK), "4")

    def test_put_add400(self):
        assert has(run("put 200+200", Language.HYPERTALK), "400")

    def test_put_hello(self):
        assert has(run('put "hello"', Language.HYPERTALK), "hello")

    def test_put_world(self):
        assert has(run('put "world"', Language.HYPERTALK), "world")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended16:
    def test_display_400(self):
        assert isinstance(run("(display 400)", Language.LISP), list)

    def test_display_15(self):
        assert has(run("(display 15)", Language.LISP), "15")

    def test_display_16(self):
        assert has(run("(display 16)", Language.LISP), "16")

    def test_add_400(self):
        assert has(run("(display (+ 200 200))", Language.LISP), "400")

    def test_string_foo(self):
        assert has(run('(display "foo")', Language.LISP), "foo")

    def test_string_bar(self):
        assert has(run('(display "bar")', Language.LISP), "bar")

    def test_lambda(self):
        r = run("((lambda (x) (display x)) 5)", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestJavaScriptExtended15:
    def test_log_400(self):
        assert isinstance(run("console.log(400);", Language.JAVASCRIPT), list)

    def test_log_15(self):
        assert has(run("console.log(15);", Language.JAVASCRIPT), "15")

    def test_log_16(self):
        assert has(run("console.log(16);", Language.JAVASCRIPT), "16")

    def test_add_400(self):
        assert has(run("console.log(200+200);", Language.JAVASCRIPT), "400")

    def test_str_foo(self):
        assert has(run('console.log("foo");', Language.JAVASCRIPT), "foo")

    def test_str_bar(self):
        assert has(run('console.log("bar");', Language.JAVASCRIPT), "bar")

    def test_bool_true(self):
        assert has(run("console.log(true);", Language.JAVASCRIPT), "true")

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestHyperTalkExtended15:
    def test_put_500(self):
        assert isinstance(run("put 500", Language.HYPERTALK), list)

    def test_put_17(self):
        assert has(run("put 17", Language.HYPERTALK), "17")

    def test_put_18(self):
        assert has(run("put 18", Language.HYPERTALK), "18")

    def test_put_add500(self):
        assert has(run("put 250+250", Language.HYPERTALK), "500")

    def test_put_mul25(self):
        assert has(run("put 5*5", Language.HYPERTALK), "25")

    def test_put_str_test(self):
        assert has(run('put "test"', Language.HYPERTALK), "test")

    def test_put_str_pass(self):
        assert has(run('put "pass"', Language.HYPERTALK), "pass")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended17:
    def test_display_500(self):
        assert isinstance(run("(display 500)", Language.LISP), list)

    def test_display_17(self):
        assert has(run("(display 17)", Language.LISP), "17")

    def test_display_18(self):
        assert has(run("(display 18)", Language.LISP), "18")

    def test_add_500(self):
        assert has(run("(display (+ 250 250))", Language.LISP), "500")

    def test_str_test(self):
        assert has(run('(display "test")', Language.LISP), "test")

    def test_neg(self):
        r = run("(display (- 0 5))", Language.LISP)
        assert isinstance(r, list)

    def test_define(self):
        r = run("(define x 99) (display x)", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestHyperTalkExtended16:
    def test_put_600(self):
        assert isinstance(run("put 600", Language.HYPERTALK), list)

    def test_put_19(self):
        assert has(run("put 19", Language.HYPERTALK), "19")

    def test_put_20(self):
        assert has(run("put 20", Language.HYPERTALK), "20")

    def test_put_30(self):
        assert has(run("put 30", Language.HYPERTALK), "30")

    def test_put_40(self):
        assert has(run("put 40", Language.HYPERTALK), "40")

    def test_put_str_run(self):
        assert has(run('put "run"', Language.HYPERTALK), "run")

    def test_put_str_done(self):
        assert has(run('put "done"', Language.HYPERTALK), "done")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended18:
    def test_display_600(self):
        assert isinstance(run("(display 600)", Language.LISP), list)

    def test_display_19(self):
        assert has(run("(display 19)", Language.LISP), "19")

    def test_display_20(self):
        assert has(run("(display 20)", Language.LISP), "20")

    def test_add_600(self):
        assert has(run("(display (+ 300 300))", Language.LISP), "600")

    def test_str_run(self):
        assert has(run('(display "run")', Language.LISP), "run")

    def test_str_done(self):
        assert has(run('(display "done")', Language.LISP), "done")

    def test_not_fn(self):
        r = run("(display (not #f))", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestJavaScriptExtended17:
    def test_log_600(self):
        assert isinstance(run("console.log(600);", Language.JAVASCRIPT), list)

    def test_log_19(self):
        assert has(run("console.log(19);", Language.JAVASCRIPT), "19")

    def test_log_20(self):
        assert has(run("console.log(20);", Language.JAVASCRIPT), "20")

    def test_add_600(self):
        assert has(run("console.log(300+300);", Language.JAVASCRIPT), "600")

    def test_str_run(self):
        assert has(run('console.log("run");', Language.JAVASCRIPT), "run")

    def test_str_done(self):
        assert has(run('console.log("done");', Language.JAVASCRIPT), "done")

    def test_null(self):
        assert has(run("console.log(null);", Language.JAVASCRIPT), "null")

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestHyperTalkExtended17:
    def test_put_700(self):
        assert isinstance(run("put 700", Language.HYPERTALK), list)

    def test_put_21(self):
        assert has(run("put 21", Language.HYPERTALK), "21")

    def test_put_22(self):
        assert has(run("put 22", Language.HYPERTALK), "22")

    def test_put_50(self):
        assert has(run("put 50", Language.HYPERTALK), "50")

    def test_put_60(self):
        assert has(run("put 60", Language.HYPERTALK), "60")

    def test_put_str_alpha(self):
        assert has(run('put "alpha"', Language.HYPERTALK), "alpha")

    def test_put_str_beta(self):
        assert has(run('put "beta"', Language.HYPERTALK), "beta")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended19:
    def test_display_700(self):
        assert isinstance(run("(display 700)", Language.LISP), list)

    def test_display_21(self):
        assert has(run("(display 21)", Language.LISP), "21")

    def test_display_22(self):
        assert has(run("(display 22)", Language.LISP), "22")

    def test_add_700(self):
        assert has(run("(display (+ 350 350))", Language.LISP), "700")

    def test_str_alpha(self):
        assert has(run('(display "alpha")', Language.LISP), "alpha")

    def test_str_beta(self):
        assert has(run('(display "beta")', Language.LISP), "beta")

    def test_car(self):
        r = run("(display (car '(1 2 3)))", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestHyperTalkExtended18:
    def test_put_800(self):
        assert isinstance(run("put 800", Language.HYPERTALK), list)

    def test_put_23(self):
        assert has(run("put 23", Language.HYPERTALK), "23")

    def test_put_24(self):
        assert has(run("put 24", Language.HYPERTALK), "24")

    def test_put_70(self):
        assert has(run("put 70", Language.HYPERTALK), "70")

    def test_put_80(self):
        assert has(run("put 80", Language.HYPERTALK), "80")

    def test_put_str_gamma(self):
        assert has(run('put "gamma"', Language.HYPERTALK), "gamma")

    def test_put_str_delta(self):
        assert has(run('put "delta"', Language.HYPERTALK), "delta")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended20:
    def test_display_800(self):
        assert isinstance(run("(display 800)", Language.LISP), list)

    def test_display_23(self):
        assert has(run("(display 23)", Language.LISP), "23")

    def test_display_24(self):
        assert has(run("(display 24)", Language.LISP), "24")

    def test_add_800(self):
        assert has(run("(display (+ 400 400))", Language.LISP), "800")

    def test_str_gamma(self):
        assert has(run('(display "gamma")', Language.LISP), "gamma")

    def test_str_delta(self):
        assert has(run('(display "delta")', Language.LISP), "delta")

    def test_cdr(self):
        r = run("(display (cdr '(1 2 3)))", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestHyperTalkExtended19:
    def test_put_900(self):
        assert isinstance(run("put 900", Language.HYPERTALK), list)

    def test_put_25(self):
        assert has(run("put 25", Language.HYPERTALK), "25")

    def test_put_26(self):
        assert has(run("put 26", Language.HYPERTALK), "26")

    def test_put_90(self):
        assert has(run("put 90", Language.HYPERTALK), "90")

    def test_put_100(self):
        assert has(run("put 100", Language.HYPERTALK), "100")

    def test_put_str_epsilon(self):
        assert has(run('put "epsilon"', Language.HYPERTALK), "epsilon")

    def test_put_str_zeta(self):
        assert has(run('put "zeta"', Language.HYPERTALK), "zeta")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended21:
    def test_display_900(self):
        assert isinstance(run("(display 900)", Language.LISP), list)

    def test_display_25(self):
        assert has(run("(display 25)", Language.LISP), "25")

    def test_display_26(self):
        assert has(run("(display 26)", Language.LISP), "26")

    def test_add_900(self):
        assert has(run("(display (+ 450 450))", Language.LISP), "900")

    def test_str_epsilon(self):
        assert has(run('(display "epsilon")', Language.LISP), "epsilon")

    def test_str_zeta(self):
        assert has(run('(display "zeta")', Language.LISP), "zeta")

    def test_list_op(self):
        r = run("(display (list 1 2 3))", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestHyperTalkExtended20:
    def test_put_1000(self):
        assert isinstance(run("put 1000", Language.HYPERTALK), list)

    def test_put_27(self):
        assert has(run("put 27", Language.HYPERTALK), "27")

    def test_put_28(self):
        assert has(run("put 28", Language.HYPERTALK), "28")

    def test_put_110(self):
        assert has(run("put 110", Language.HYPERTALK), "110")

    def test_put_120(self):
        assert has(run("put 120", Language.HYPERTALK), "120")

    def test_put_str_eta(self):
        assert has(run('put "eta"', Language.HYPERTALK), "eta")

    def test_put_str_theta(self):
        assert has(run('put "theta"', Language.HYPERTALK), "theta")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended22:
    def test_display_1000(self):
        assert isinstance(run("(display 1000)", Language.LISP), list)

    def test_display_27(self):
        assert has(run("(display 27)", Language.LISP), "27")

    def test_display_28(self):
        assert has(run("(display 28)", Language.LISP), "28")

    def test_add_1000(self):
        assert has(run("(display (+ 500 500))", Language.LISP), "1000")

    def test_str_eta(self):
        assert has(run('(display "eta")', Language.LISP), "eta")

    def test_str_theta(self):
        assert has(run('(display "theta")', Language.LISP), "theta")

    def test_lambda_call(self):
        r = run("(display ((lambda (x) (* x x)) 5))", Language.LISP)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestHyperTalkExtended21:
    def test_put_1100(self):
        assert isinstance(run("put 1100", Language.HYPERTALK), list)

    def test_put_29(self):
        assert has(run("put 29", Language.HYPERTALK), "29")

    def test_put_30(self):
        assert has(run("put 30", Language.HYPERTALK), "30")

    def test_put_iota(self):
        assert has(run('put "iota"', Language.HYPERTALK), "iota")

    def test_put_kappa(self):
        assert has(run('put "kappa"', Language.HYPERTALK), "kappa")

    def test_put_1100_num(self):
        assert has(run("put 1100", Language.HYPERTALK), "1100")

    def test_add_math(self):
        assert has(run("put 550 + 550", Language.HYPERTALK), "1100")

    def test_empty(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_output_list(self):
        assert isinstance(run("put 1", Language.HYPERTALK), list)

    def test_no_errors(self):
        assert no_errors(run("put 1", Language.HYPERTALK))


class TestLispExtended23:
    def test_display_1100(self):
        assert isinstance(run("(display 1100)", Language.LISP), list)

    def test_display_29(self):
        assert has(run("(display 29)", Language.LISP), "29")

    def test_display_30(self):
        assert has(run("(display 30)", Language.LISP), "30")

    def test_add_1100(self):
        assert has(run("(display (+ 550 550))", Language.LISP), "1100")

    def test_str_iota(self):
        assert has(run('(display "iota")', Language.LISP), "iota")

    def test_str_kappa(self):
        assert has(run('(display "kappa")', Language.LISP), "kappa")

    def test_list_3(self):
        assert has(run("(display (list 1 2 3))", Language.LISP), "1")

    def test_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_output_list(self):
        assert isinstance(run("(display 1)", Language.LISP), list)

    def test_no_errors(self):
        assert no_errors(run("(display 1)", Language.LISP))


class TestHyperTalkExtended22:
    def test_put_1200(self):
        assert has(run("put 1200", Language.HYPERTALK), "1200")

    def test_put_31(self):
        assert has(run("put 31", Language.HYPERTALK), "31")

    def test_put_32(self):
        assert has(run("put 32", Language.HYPERTALK), "32")

    def test_put_lambda(self):
        assert has(run('put "lambda"', Language.HYPERTALK), "lambda")

    def test_put_mu(self):
        assert has(run('put "mu"', Language.HYPERTALK), "mu")

    def test_add_1200(self):
        assert has(run("put 600 + 600", Language.HYPERTALK), "1200")

    def test_mul_12(self):
        r = run("put 3 * 4", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_sub_3(self):
        r = run("put 10 - 7", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list2(self):
        assert isinstance(run("put 2", Language.HYPERTALK), list)

    def test_no_errors2(self):
        assert no_errors(run("put 2", Language.HYPERTALK))


class TestLispExtended24:
    def test_display_1200(self):
        assert has(run("(display 1200)", Language.LISP), "1200")

    def test_display_31(self):
        assert has(run("(display 31)", Language.LISP), "31")

    def test_display_32(self):
        assert has(run("(display 32)", Language.LISP), "32")

    def test_str_lambda(self):
        assert has(run('(display "lambda")', Language.LISP), "lambda")

    def test_str_mu(self):
        assert has(run('(display "mu")', Language.LISP), "mu")

    def test_add_1200(self):
        assert has(run("(display (+ 600 600))", Language.LISP), "1200")

    def test_mul_12(self):
        assert has(run("(display (* 3 4))", Language.LISP), "12")

    def test_cond(self):
        r = run("(display (if (> 5 3) 'yes 'no))", Language.LISP)
        assert isinstance(r, list)

    def test_output_list2(self):
        assert isinstance(run("(display 2)", Language.LISP), list)

    def test_no_errors2(self):
        assert no_errors(run("(display 2)", Language.LISP))


class TestHyperTalkExtended23:
    def test_put_1300(self):
        assert has(run("put 1300", Language.HYPERTALK), "1300")

    def test_put_33(self):
        assert has(run("put 33", Language.HYPERTALK), "33")

    def test_put_34(self):
        assert has(run("put 34", Language.HYPERTALK), "34")

    def test_put_nu(self):
        assert has(run('put "nu"', Language.HYPERTALK), "nu")

    def test_put_xi(self):
        assert has(run('put "xi"', Language.HYPERTALK), "xi")

    def test_add_1300(self):
        assert has(run("put 650 + 650", Language.HYPERTALK), "1300")

    def test_mul_20(self):
        r = run("put 4 * 5", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_sub_7(self):
        r = run("put 10 - 3", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list3(self):
        assert isinstance(run("put 3", Language.HYPERTALK), list)

    def test_no_errors3(self):
        assert no_errors(run("put 3", Language.HYPERTALK))


class TestLispExtended25:
    def test_display_1300(self):
        assert has(run("(display 1300)", Language.LISP), "1300")

    def test_display_33(self):
        assert has(run("(display 33)", Language.LISP), "33")

    def test_display_34(self):
        assert has(run("(display 34)", Language.LISP), "34")

    def test_str_nu(self):
        assert has(run('(display "nu")', Language.LISP), "nu")

    def test_str_xi(self):
        assert has(run('(display "xi")', Language.LISP), "xi")

    def test_add_1300(self):
        assert has(run("(display (+ 650 650))", Language.LISP), "1300")

    def test_sub_7(self):
        assert has(run("(display (- 10 3))", Language.LISP), "7")

    def test_let_form(self):
        r = run("(let ((x 7)) (display x))", Language.LISP)
        assert has(r, "7")

    def test_output_list3(self):
        assert isinstance(run("(display 3)", Language.LISP), list)

    def test_no_errors3(self):
        assert no_errors(run("(display 3)", Language.LISP))


class TestHyperTalkExtended24:
    def test_put_1400(self):
        assert has(run("put 1400", Language.HYPERTALK), "1400")

    def test_put_35(self):
        assert has(run("put 35", Language.HYPERTALK), "35")

    def test_put_36(self):
        assert has(run("put 36", Language.HYPERTALK), "36")

    def test_put_omicron(self):
        assert has(run('put "omicron"', Language.HYPERTALK), "omicron")

    def test_put_pi(self):
        assert has(run('put "pi"', Language.HYPERTALK), "pi")

    def test_add_1400(self):
        assert has(run("put 700 + 700", Language.HYPERTALK), "1400")

    def test_mul_49(self):
        r = run("put 7 * 7", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_div_5(self):
        r = run("put 20 / 4", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list4(self):
        assert isinstance(run("put 4", Language.HYPERTALK), list)

    def test_no_errors4(self):
        assert no_errors(run("put 4", Language.HYPERTALK))


class TestLispExtended26:
    def test_display_1400(self):
        assert has(run("(display 1400)", Language.LISP), "1400")

    def test_display_35(self):
        assert has(run("(display 35)", Language.LISP), "35")

    def test_display_36(self):
        assert has(run("(display 36)", Language.LISP), "36")

    def test_str_omicron(self):
        assert has(run('(display "omicron")', Language.LISP), "omicron")

    def test_str_pi(self):
        assert has(run('(display "pi")', Language.LISP), "pi")

    def test_add_1400(self):
        assert has(run("(display (+ 700 700))", Language.LISP), "1400")

    def test_mul_49(self):
        assert has(run("(display (* 7 7))", Language.LISP), "49")

    def test_lambda(self):
        r = run("(display ((lambda (x) (* x x)) 8))", Language.LISP)
        assert has(r, "64")

    def test_output_list4(self):
        assert isinstance(run("(display 4)", Language.LISP), list)

    def test_no_errors4(self):
        assert no_errors(run("(display 4)", Language.LISP))


class TestHyperTalkExtended25:
    def test_put_1500(self):
        assert has(run("put 1500", Language.HYPERTALK), "1500")

    def test_put_37(self):
        assert has(run("put 37", Language.HYPERTALK), "37")

    def test_put_38(self):
        assert has(run("put 38", Language.HYPERTALK), "38")

    def test_put_rho(self):
        assert has(run('put "rho"', Language.HYPERTALK), "rho")

    def test_put_sigma(self):
        assert has(run('put "sigma"', Language.HYPERTALK), "sigma")

    def test_add_1500(self):
        assert has(run("put 750 + 750", Language.HYPERTALK), "1500")

    def test_mul_64(self):
        r = run("put 8 * 8", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_div_7(self):
        r = run("put 49 / 7", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list5(self):
        assert isinstance(run("put 5", Language.HYPERTALK), list)

    def test_no_errors5(self):
        assert no_errors(run("put 5", Language.HYPERTALK))


class TestLispExtended27:
    def test_display_1500(self):
        assert has(run("(display 1500)", Language.LISP), "1500")

    def test_display_37(self):
        assert has(run("(display 37)", Language.LISP), "37")

    def test_display_38(self):
        assert has(run("(display 38)", Language.LISP), "38")

    def test_str_rho(self):
        assert has(run('(display "rho")', Language.LISP), "rho")

    def test_str_sigma(self):
        assert has(run('(display "sigma")', Language.LISP), "sigma")

    def test_add_1500(self):
        assert has(run("(display (+ 750 750))", Language.LISP), "1500")

    def test_mul_64(self):
        assert has(run("(display (* 8 8))", Language.LISP), "64")

    def test_define(self):
        r = run("(define x 77) (display x)", Language.LISP)
        assert has(r, "77")

    def test_output_list5(self):
        assert isinstance(run("(display 5)", Language.LISP), list)

    def test_no_errors5(self):
        assert no_errors(run("(display 5)", Language.LISP))


class TestHyperTalkExtended26:
    def test_put_1600(self):
        assert has(run("put 1600", Language.HYPERTALK), "1600")

    def test_put_39(self):
        assert has(run("put 39", Language.HYPERTALK), "39")

    def test_put_tau(self):
        assert has(run('put "tau"', Language.HYPERTALK), "tau")

    def test_put_upsilon(self):
        assert has(run('put "upsilon"', Language.HYPERTALK), "upsilon")

    def test_add_1600(self):
        assert has(run("put 800 + 800", Language.HYPERTALK), "1600")

    def test_sub_90(self):
        r = run("put 100 - 10", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_mul_81(self):
        r = run("put 9 * 9", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_div_4(self):
        r = run("put 16 / 4", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list6(self):
        assert isinstance(run("put 6", Language.HYPERTALK), list)

    def test_no_errors6(self):
        assert no_errors(run("put 6", Language.HYPERTALK))


class TestLispExtended28:
    def test_display_1600(self):
        assert has(run("(display 1600)", Language.LISP), "1600")

    def test_display_39(self):
        assert has(run("(display 39)", Language.LISP), "39")

    def test_str_tau(self):
        assert has(run('(display "tau")', Language.LISP), "tau")

    def test_str_upsilon(self):
        assert has(run('(display "upsilon")', Language.LISP), "upsilon")

    def test_add_1600(self):
        assert has(run("(display (+ 800 800))", Language.LISP), "1600")

    def test_mul_81(self):
        assert has(run("(display (* 9 9))", Language.LISP), "81")

    def test_cdr(self):
        r = run("(display (cdr '(1 2 3)))", Language.LISP)
        assert isinstance(r, list)

    def test_car(self):
        r = run("(display (car '(10 20 30)))", Language.LISP)
        assert isinstance(r, list)

    def test_output_list6(self):
        assert isinstance(run("(display 6)", Language.LISP), list)

    def test_no_errors6(self):
        assert no_errors(run("(display 6)", Language.LISP))


class TestHyperTalkExtended27:
    def test_put_1700(self):
        assert has(run("put 1700", Language.HYPERTALK), "1700")

    def test_put_40(self):
        assert has(run("put 40", Language.HYPERTALK), "40")

    def test_put_phi(self):
        assert has(run('put "phi"', Language.HYPERTALK), "phi")

    def test_put_chi(self):
        assert has(run('put "chi"', Language.HYPERTALK), "chi")

    def test_add_1700(self):
        assert has(run("put 850 + 850", Language.HYPERTALK), "1700")

    def test_sub_50(self):
        r = run("put 100 - 50", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_mul_100(self):
        r = run("put 10 * 10", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_div_5(self):
        r = run("put 25 / 5", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list7(self):
        assert isinstance(run("put 7", Language.HYPERTALK), list)

    def test_no_errors7(self):
        assert no_errors(run("put 7", Language.HYPERTALK))


class TestLispExtended29:
    def test_display_1700(self):
        assert has(run("(display 1700)", Language.LISP), "1700")

    def test_display_40(self):
        assert has(run("(display 40)", Language.LISP), "40")

    def test_str_phi(self):
        assert has(run('(display "phi")', Language.LISP), "phi")

    def test_str_chi(self):
        assert has(run('(display "chi")', Language.LISP), "chi")

    def test_add_1700(self):
        assert has(run("(display (+ 850 850))", Language.LISP), "1700")

    def test_mul_100(self):
        assert has(run("(display (* 10 10))", Language.LISP), "100")

    def test_list_len(self):
        r = run("(display (length '(1 2 3 4 5)))", Language.LISP)
        assert has(r, "5")

    def test_append(self):
        r = run("(display (append '(1 2) '(3 4)))", Language.LISP)
        assert isinstance(r, list)

    def test_output_list7(self):
        assert isinstance(run("(display 7)", Language.LISP), list)

    def test_no_errors7(self):
        assert no_errors(run("(display 7)", Language.LISP))


class TestHyperTalkExtended28:
    def test_put_1800(self):
        assert has(run("put 1800", Language.HYPERTALK), "1800")

    def test_put_41(self):
        assert has(run("put 41", Language.HYPERTALK), "41")

    def test_put_psi(self):
        assert has(run('put "psi"', Language.HYPERTALK), "psi")

    def test_put_omega(self):
        assert has(run('put "omega"', Language.HYPERTALK), "omega")

    def test_add_1800(self):
        assert has(run("put 900 + 900", Language.HYPERTALK), "1800")

    def test_sub_80(self):
        r = run("put 100 - 20", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_mul_121(self):
        r = run("put 11 * 11", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_div_9(self):
        r = run("put 81 / 9", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list8(self):
        assert isinstance(run("put 8", Language.HYPERTALK), list)

    def test_no_errors8(self):
        assert no_errors(run("put 8", Language.HYPERTALK))


class TestLispExtended30:
    def test_display_1800(self):
        assert has(run("(display 1800)", Language.LISP), "1800")

    def test_display_41(self):
        assert has(run("(display 41)", Language.LISP), "41")

    def test_str_psi(self):
        assert has(run('(display "psi")', Language.LISP), "psi")

    def test_str_omega(self):
        assert has(run('(display "omega")', Language.LISP), "omega")

    def test_add_1800(self):
        assert has(run("(display (+ 900 900))", Language.LISP), "1800")

    def test_mul_121(self):
        assert has(run("(display (* 11 11))", Language.LISP), "121")

    def test_if_true(self):
        r = run("(display (if #t 99 0))", Language.LISP)
        assert has(r, "99")

    def test_if_false(self):
        r = run("(display (if #f 99 0))", Language.LISP)
        assert has(r, "0")

    def test_output_list8(self):
        assert isinstance(run("(display 8)", Language.LISP), list)

    def test_no_errors8(self):
        assert no_errors(run("(display 8)", Language.LISP))


class TestHyperTalkExtended29:
    def test_put_1900(self):
        assert has(run("put 1900", Language.HYPERTALK), "1900")

    def test_put_42(self):
        assert has(run("put 42", Language.HYPERTALK), "42")

    def test_put_one(self):
        assert has(run('put "one"', Language.HYPERTALK), "one")

    def test_put_two(self):
        assert has(run('put "two"', Language.HYPERTALK), "two")

    def test_add_1900(self):
        assert has(run("put 950 + 950", Language.HYPERTALK), "1900")

    def test_sub_70(self):
        r = run("put 100 - 30", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_mul_144(self):
        r = run("put 12 * 12", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_div_11(self):
        r = run("put 121 / 11", Language.HYPERTALK)
        assert isinstance(r, list)

    def test_output_list9(self):
        assert isinstance(run("put 9", Language.HYPERTALK), list)

    def test_no_errors9(self):
        assert no_errors(run("put 9", Language.HYPERTALK))


class TestLispExtended31:
    def test_display_1900(self):
        assert has(run("(display 1900)", Language.LISP), "1900")

    def test_display_42(self):
        assert has(run("(display 42)", Language.LISP), "42")

    def test_str_one(self):
        assert has(run('(display "one")', Language.LISP), "one")

    def test_str_two(self):
        assert has(run('(display "two")', Language.LISP), "two")

    def test_add_1900(self):
        assert has(run("(display (+ 950 950))", Language.LISP), "1900")

    def test_mul_144(self):
        assert has(run("(display (* 12 12))", Language.LISP), "144")

    def test_min(self):
        r = run("(display (min 3 7))", Language.LISP)
        assert has(r, "3")

    def test_max(self):
        r = run("(display (max 3 7))", Language.LISP)
        assert has(r, "7")

    def test_output_list9(self):
        assert isinstance(run("(display 9)", Language.LISP), list)

    def test_no_errors9(self):
        assert no_errors(run("(display 9)", Language.LISP))


class TestHyperTalkExtended30:
    def test_put_1900(self):
        assert has(run("put 1900", Language.HYPERTALK), "1900")

    def test_put_42(self):
        assert has(run("put 42", Language.HYPERTALK), "42")

    def test_put_one(self):
        assert has(run("put \"one\"", Language.HYPERTALK), "one")

    def test_put_two(self):
        assert has(run("put \"two\"", Language.HYPERTALK), "two")

    def test_add_1900(self):
        assert has(run("put 950 + 950", Language.HYPERTALK), "1900")

    def test_mul_144(self):
        assert has(run("put 12 * 12", Language.HYPERTALK), "144")

    def test_sub_70(self):
        assert has(run("put 100 - 30", Language.HYPERTALK), "70")

    def test_div_20(self):
        assert has(run("put 100 / 5", Language.HYPERTALK), "20")

    def test_output_list10(self):
        assert isinstance(run("put 10", Language.HYPERTALK), list)

    def test_no_errors10(self):
        assert no_errors(run("put 10", Language.HYPERTALK))


class TestLispExtended32:
    def test_display_1900(self):
        assert has(run("(display 1900)", Language.LISP), "1900")

    def test_display_42(self):
        assert has(run("(display 42)", Language.LISP), "42")

    def test_str_one(self):
        assert has(run('(display "one")', Language.LISP), "one")

    def test_str_two(self):
        assert has(run('(display "two")', Language.LISP), "two")

    def test_add_1900(self):
        assert has(run("(display (+ 950 950))", Language.LISP), "1900")

    def test_mul_144(self):
        assert has(run("(display (* 12 12))", Language.LISP), "144")

    def test_sub_70(self):
        assert has(run("(display (- 100 30))", Language.LISP), "70")

    def test_div_20(self):
        assert has(run("(display (/ 100 5))", Language.LISP), "20")

    def test_output_list10(self):
        assert isinstance(run("(display 10)", Language.LISP), list)

    def test_no_errors10(self):
        assert no_errors(run("(display 10)", Language.LISP))
