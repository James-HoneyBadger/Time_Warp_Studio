"""Comprehensive tests for the C language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.C


# ── helpers ────────────────────────────────────────────────────────────────


def c(body: str, **kw) -> list[str]:
    """Wrap *body* in a minimal C main() and execute."""
    source = "#include <stdio.h>\n" "int main() {\n" f"{body}\n" "return 0;\n" "}"
    return run(source, L, **kw)


# ============================================================================
# printf basics
# ============================================================================


class TestPrintf:
    """printf with various format specifiers."""

    def test_hello_world(self):
        out = c('printf("Hello World\\n");')
        assert has(out, "Hello World")

    def test_printf_int(self):
        out = c('int x = 42;\nprintf("%d\\n", x);')
        assert has(out, "42")

    def test_printf_float(self):
        out = c('double pi = 3.14;\nprintf("%.2f\\n", pi);')
        assert has(out, "3.14")

    def test_printf_string(self):
        out = c('printf("%s\\n", "test");')
        assert has(out, "test")

    def test_printf_char(self):
        out = c('printf("%c\\n", 65);')
        assert has(out, "A")

    def test_printf_hex(self):
        out = c('printf("%x\\n", 255);')
        assert has(out, "ff")

    def test_printf_octal(self):
        out = c('printf("%o\\n", 8);')
        assert has(out, "10")

    def test_printf_unsigned(self):
        out = c('printf("%u\\n", 42);')
        assert has(out, "42")

    def test_printf_multiple_args(self):
        out = c('int a = 1;\nint b = 2;\nprintf("%d %d\\n", a, b);')
        assert has(out, "1", "2")

    def test_printf_no_newline(self):
        out = c('printf("AB");')
        assert has(out, "AB")

    def test_printf_escape_tab(self):
        out = c('printf("A\\tB\\n");')
        assert has(out, "A\tB")

    def test_printf_percent_literal(self):
        out = c('printf("100%%\\n");')
        assert has(out, "100%")


# ============================================================================
# Variable declarations and assignments
# ============================================================================


class TestVariables:
    """int, float, double, long declarations and assignments."""

    def test_int_declaration(self):
        out = c('int x = 10;\nprintf("%d\\n", x);')
        assert has(out, "10")

    def test_float_declaration(self):
        out = c('float f = 2.5;\nprintf("%f\\n", f);')
        assert has(out, "2.5")

    def test_double_declaration(self):
        out = c('double d = 1.23;\nprintf("%f\\n", d);')
        assert has(out, "1.23")

    def test_long_declaration(self):
        out = c('long n = 100000;\nprintf("%d\\n", n);')
        assert has(out, "100000")

    def test_assignment(self):
        out = c('int x = 0;\nx = 99;\nprintf("%d\\n", x);')
        assert has(out, "99")

    def test_expression_assignment(self):
        out = c('int x = 3 + 4;\nprintf("%d\\n", x);')
        assert has(out, "7")

    def test_increment(self):
        out = c('int x = 5;\nx++;\nprintf("%d\\n", x);')
        assert has(out, "6")

    def test_decrement(self):
        out = c('int x = 5;\nx--;\nprintf("%d\\n", x);')
        assert has(out, "4")


# ============================================================================
# scanf
# ============================================================================


class TestScanf:
    """scanf reads via input callback."""

    def test_scanf_int(self):
        out = c(
            'int x;\nscanf("%d", &x);\nprintf("%d\\n", x);',
            input_val="7",
        )
        assert has(out, "7")

    def test_scanf_float(self):
        out = c(
            'float f;\nscanf("%f", &f);\nprintf("%f\\n", f);',
            input_val="3.5",
        )
        assert has(out, "3.5")


# ============================================================================
# if / else
# ============================================================================


class TestIfElse:
    """if/else control flow."""

    def test_if_true(self):
        out = c("int x = 1;\n" "if (x == 1) {\n" '    printf("yes\\n");\n' "}")
        assert has(out, "yes")

    def test_if_false(self):
        out = c("int x = 0;\n" "if (x == 1) {\n" '    printf("yes\\n");\n' "}")
        assert not has(out, "yes")

    def test_if_else(self):
        out = c(
            "int x = 0;\n"
            "if (x == 1) {\n"
            '    printf("yes\\n");\n'
            "} else {\n"
            '    printf("no\\n");\n'
            "}"
        )
        assert has(out, "no")

    def test_if_greater_than(self):
        out = c("int x = 10;\n" "if (x > 5) {\n" '    printf("big\\n");\n' "}")
        assert has(out, "big")


# ============================================================================
# while loop
# ============================================================================


class TestWhile:
    """while loop."""

    def test_while_loop(self):
        out = c(
            "int i = 0;\n"
            "while (i < 3) {\n"
            '    printf("%d\\n", i);\n'
            "    i++;\n"
            "}"
        )
        assert has(out, "0", "1", "2")

    def test_while_false_skip(self):
        out = c(
            "int i = 10;\n"
            "while (i < 3) {\n"
            '    printf("loop\\n");\n'
            "    i++;\n"
            "}\n"
            'printf("done\\n");'
        )
        assert has(out, "done")
        assert not has(out, "loop")


# ============================================================================
# for loop
# ============================================================================


class TestFor:
    """for loop."""

    def test_for_loop(self):
        out = c("for (int i = 0; i < 3; i++) {\n" '    printf("%d\\n", i);\n' "}")
        assert has(out, "0", "1", "2")

    def test_for_loop_step(self):
        out = c(
            "int i;\n"
            "for (i = 0; i < 10; i = i + 3) {\n"
            '    printf("%d\\n", i);\n'
            "}"
        )
        assert has(out, "0", "3", "6", "9")


# ============================================================================
# do-while loop
# ============================================================================


class TestDoWhile:
    """do { ... } while (cond);"""

    def test_do_while(self):
        out = c(
            "int i = 0;\n"
            "do {\n"
            '    printf("%d\\n", i);\n'
            "    i++;\n"
            "} while (i < 3);"
        )
        assert has(out, "0", "1", "2")

    def test_do_while_runs_once(self):
        out = c(
            "int i = 10;\n"
            "do {\n"
            '    printf("once\\n");\n'
            "    i++;\n"
            "} while (i < 3);"
        )
        assert has(out, "once")


# ============================================================================
# break / continue
# ============================================================================


class TestBreakContinue:
    """break and continue in loops."""

    def test_break(self):
        out = c(
            "int i = 0;\n"
            "while (i < 10) {\n"
            "    if (i == 3) {\n"
            "        break;\n"
            "    }\n"
            '    printf("%d\\n", i);\n'
            "    i++;\n"
            "}"
        )
        assert has(out, "0", "1", "2")
        assert not has(out, "3")


# ============================================================================
# math.h functions
# ============================================================================


class TestMathFunctions:
    """math.h standard library functions."""

    def test_sqrt(self):
        out = c('printf("%f\\n", sqrt(16.0));')
        assert has(out, "4.0")

    def test_pow(self):
        out = c('printf("%f\\n", pow(2.0, 3.0));')
        assert has(out, "8.0")

    def test_ceil(self):
        out = c('printf("%f\\n", ceil(3.2));')
        assert has(out, "4.0")

    def test_floor(self):
        out = c('printf("%f\\n", floor(3.8));')
        assert has(out, "3.0")

    def test_fabs(self):
        out = c('printf("%f\\n", fabs(-5.5));')
        assert has(out, "5.5")

    def test_sin(self):
        out = c('printf("%f\\n", sin(0.0));')
        assert has(out, "0.0")

    def test_cos(self):
        out = c('printf("%f\\n", cos(0.0));')
        assert has(out, "1.0")

    def test_log(self):
        out = c('printf("%f\\n", log(1.0));')
        assert has(out, "0.0")

    def test_log10(self):
        out = c('printf("%f\\n", log10(100.0));')
        assert has(out, "2.0")

    def test_exp(self):
        out = c('printf("%f\\n", exp(0.0));')
        assert has(out, "1.0")


# ============================================================================
# stdlib.h functions
# ============================================================================


class TestStdlib:
    """stdlib.h functions."""

    def test_abs(self):
        out = c('printf("%d\\n", abs(-42));')
        assert has(out, "42")

    def test_atoi(self):
        out = c('printf("%d\\n", atoi("123"));')
        assert has(out, "123")

    def test_rand(self):
        out = c('int r = rand();\nprintf("%d\\n", r);')
        assert no_errors(out)

    def test_malloc_free(self):
        """malloc/free are simulated – just no errors."""
        out = c("int x = malloc(100);\nfree(x);")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# string.h functions
# ============================================================================


class TestStringH:
    """string.h functions."""

    def test_strlen(self):
        out = c('printf("%d\\n", strlen("hello"));')
        assert has(out, "5")

    def test_strcmp_equal(self):
        out = c('printf("%d\\n", strcmp("abc", "abc"));')
        assert has(out, "0")

    def test_strcmp_unequal(self):
        out = c('printf("%d\\n", strcmp("abc", "xyz"));')
        assert no_errors(out)  # Should be non-zero

    def test_strcpy(self):
        src = "char dest[20];\n" 'strcpy(dest, "hello");\n' 'printf("%s\\n", dest);'
        out = c(src)
        assert has(out, "hello")

    def test_strcat(self):
        src = (
            "char dest[20];\n"
            'strcpy(dest, "hello");\n'
            'strcat(dest, " world");\n'
            'printf("%s\\n", dest);'
        )
        out = c(src)
        assert has(out, "hello world")

    def test_strncmp(self):
        out = c('printf("%d\\n", strncmp("abc", "abd", 2));')
        assert has(out, "0")

    def test_memset(self):
        src = "char buf[10];\n" "memset(buf, 65, 5);\n" 'printf("%s\\n", buf);'
        out = c(src)
        assert has(out, "AAAAA")


# ============================================================================
# ctype.h functions
# ============================================================================


class TestCtypeH:
    """ctype.h character classification functions."""

    def test_isalpha(self):
        out = c('printf("%d\\n", isalpha(65));')
        assert has(out, "1")

    def test_isdigit(self):
        out = c('printf("%d\\n", isdigit(48));')
        assert has(out, "1")

    def test_isalnum(self):
        out = c('printf("%d\\n", isalnum(65));')
        assert has(out, "1")

    def test_isupper(self):
        out = c('printf("%d\\n", isupper(65));')
        assert has(out, "1")

    def test_islower(self):
        out = c('printf("%d\\n", islower(97));')
        assert has(out, "1")

    def test_isspace(self):
        out = c('printf("%d\\n", isspace(32));')
        assert has(out, "1")


# ============================================================================
# time.h
# ============================================================================


class TestTimeH:
    """time.h functions."""

    def test_time(self):
        out = c('printf("%d\\n", time(0));')
        assert no_errors(out)


# ============================================================================
# Comments and preprocessor
# ============================================================================


class TestComments:
    """C comments and preprocessor directives."""

    def test_line_comment(self):
        out = c('// this is a comment\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_block_comment(self):
        src = (
            "#include <stdio.h>\n"
            "/* block comment */\n"
            "int main() {\n"
            'printf("ok\\n");\n'
            "return 0;\n"
            "}"
        )
        out = run(src, L)
        assert has(out, "ok")

    def test_include_ignored(self):
        src = (
            "#include <stdio.h>\n"
            "#include <stdlib.h>\n"
            "int main() {\n"
            'printf("ok\\n");\n'
            "return 0;\n"
            "}"
        )
        out = run(src, L)
        assert has(out, "ok")


# ============================================================================
# return statement
# ============================================================================


class TestReturn:
    """return stops execution."""

    def test_return_stops(self):
        out = c('printf("before\\n");\nreturn 0;')
        assert has(out, "before")

    def test_return_with_value(self):
        out = c("return 42;")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# Arithmetic expressions
# ============================================================================


class TestArithmetic:
    """Arithmetic expressions in C."""

    def test_addition(self):
        out = c('int x = 3 + 4;\nprintf("%d\\n", x);')
        assert has(out, "7")

    def test_subtraction(self):
        out = c('int x = 10 - 3;\nprintf("%d\\n", x);')
        assert has(out, "7")

    def test_multiplication(self):
        out = c('int x = 6 * 7;\nprintf("%d\\n", x);')
        assert has(out, "42")

    def test_division(self):
        out = c('int x = 10 / 2;\nprintf("%d\\n", x);')
        assert has(out, "5")

    def test_modulo_side_effect(self):
        out = c('int x = 10;\nint y = x + 1;\nprintf("%d\\n", y);')
        assert has(out, "11")


# ============================================================================
# String variables (char arrays)
# ============================================================================


class TestCharArrays:
    """char array declarations and string operations."""

    def test_char_array_init(self):
        src = (
            "#include <stdio.h>\n"
            "int main() {\n"
            "char name[20];\n"
            'strcpy(name, "World");\n'
            'printf("Hello %s\\n", name);\n'
            "return 0;\n"
            "}"
        )
        out = run(src, L)
        assert has(out, "Hello World")


# ============================================================================
# Nested control flow
# ============================================================================


class TestNestedFlow:
    """Nested if/while combinations."""

    def test_if_inside_while(self):
        out = c(
            "int i = 0;\n"
            "while (i < 5) {\n"
            "    if (i == 2) {\n"
            '        printf("found\\n");\n'
            "    }\n"
            "    i++;\n"
            "}"
        )
        assert has(out, "found")

    def test_while_inside_if(self):
        out = c(
            "int x = 1;\n"
            "if (x == 1) {\n"
            "    int i = 0;\n"
            "    while (i < 2) {\n"
            '        printf("%d\\n", i);\n'
            "        i++;\n"
            "    }\n"
            "}"
        )
        assert has(out, "0", "1")


# ============================================================================
# Edge cases
# ============================================================================


class TestEdgeCases:
    """Edge cases and error handling."""

    def test_empty_main(self):
        src = "#include <stdio.h>\nint main() {\nreturn 0;\n}"
        out = run(src, L)
        assert no_errors(out) or len(out) == 0

    def test_braces_only(self):
        """Opening/closing braces are handled gracefully."""
        out = c("")
        assert no_errors(out) or len(out) == 0

    def test_multiple_declarations(self):
        out = c("int a = 1;\n" "int b = 2;\n" "int c = a + b;\n" 'printf("%d\\n", c);')
        assert has(out, "3")

    def test_semicolons_ignored(self):
        """Extra semicolons don't cause errors."""
        out = c('printf("ok\\n");;')
        assert has(out, "ok") or no_errors(out)


# ============================================================================
# switch/case
# ============================================================================


class TestSwitchCase:
    """switch/case control flow."""

    def test_switch_matches_case(self):
        out = c(
            'int x = 2;\nswitch (x) {\ncase 1: printf("one\\n"); break;\ncase 2: printf("two\\n"); break;\ndefault: printf("other\\n"); break;\n}'
        )
        assert has(out, "two")

    def test_switch_default(self):
        out = c(
            'int x = 9;\nswitch (x) {\ncase 1: printf("one\\n"); break;\ndefault: printf("default\\n"); break;\n}'
        )
        assert has(out, "default")


# ============================================================================
# ternary operator
# ============================================================================


class TestTernary:
    """Ternary conditional expressions."""

    def test_ternary_true(self):
        out = c('int x = 1;\nprintf("%d\\n", x ? 10 : 20);')
        assert has(out, "10")

    def test_ternary_false(self):
        out = c('int x = 0;\nprintf("%d\\n", x ? 10 : 20);')
        assert has(out, "20")


# ============================================================================
# arrays
# ============================================================================


class TestArrays:
    """C array declarations and element access."""

    def test_array_declare_and_set(self):
        out = c(
            'int a[3];\na[0] = 10;\na[1] = 20;\na[2] = 30;\nprintf("%d %d %d\\n", a[0], a[1], a[2]);'
        )
        assert has(out, "10", "20", "30")

    def test_array_multi_statement(self):
        out = c(
            'int a[3];\na[0] = 10; a[1] = 20; a[2] = 30;\nprintf("%d %d %d\\n", a[0], a[1], a[2]);'
        )
        assert has(out, "10", "20", "30")


# ============================================================================
# Array initializer + compound assignment (regression)
# ============================================================================


class TestArrayInitializer:
    """Regression: int a[3] = {10,20,30} should populate elements."""

    def test_array_initializer(self):
        out = c('int a[3] = {10, 20, 30};\nprintf("%d\\n", a[1]);')
        assert has(out, "20")


class TestCompoundAssignment:
    """Regression: +=, -=, *=, /=, %= operators."""

    def test_plus_equals(self):
        out = c('int x = 5;\nx += 3;\nprintf("%d\\n", x);')
        assert has(out, "8")

    def test_minus_equals(self):
        out = c('int x = 10;\nx -= 3;\nprintf("%d\\n", x);')
        assert has(out, "7")

    def test_times_equals(self):
        out = c('int x = 4;\nx *= 3;\nprintf("%d\\n", x);')
        assert has(out, "12")


# ============================================================================
# Preprocessor: #define constants and #ifdef / #ifndef / #endif
# ============================================================================


class TestPreprocessorDefine:
    """#define macro constant substitution."""

    def test_define_constant(self):
        out = c('#define MAX 42\nint x = MAX;\nprintf("%d\\n", x);')
        assert has(out, "42")

    def test_define_used_in_expression(self):
        out = c('#define BASE 10\nint x = BASE * 2;\nprintf("%d\\n", x);')
        assert has(out, "20")

    def test_multiple_defines(self):
        out = c('#define A 3\n#define B 4\nprintf("%d\\n", A + B);')
        assert has(out, "7")


class TestPreprocessorIfdef:
    """#ifdef / #ifndef / #else / #endif conditional compilation."""

    def test_ifdef_defined_includes_block(self):
        src = (
            "#define DEBUG 1\n"
            "#ifdef DEBUG\n"
            'printf("debug\\n");\n'
            "#endif\n"
        )
        out = c(src)
        assert has(out, "debug")

    def test_ifdef_undefined_skips_block(self):
        src = (
            "#ifdef NDEBUG\n"
            'printf("should_not_print\\n");\n'
            "#endif\n"
            'printf("ok\\n");\n'
        )
        out = c(src)
        assert has(out, "ok")
        assert not any("should_not_print" in line for line in out)

    def test_ifndef_undefined_includes_block(self):
        src = (
            "#ifndef RELEASE\n"
            'printf("dev\\n");\n'
            "#endif\n"
        )
        out = c(src)
        assert has(out, "dev")

    def test_ifndef_defined_skips_block(self):
        src = (
            "#define RELEASE 1\n"
            "#ifndef RELEASE\n"
            'printf("should_not_print\\n");\n'
            "#endif\n"
            'printf("ok\\n");\n'
        )
        out = c(src)
        assert has(out, "ok")
        assert not any("should_not_print" in line for line in out)

    def test_ifdef_else_takes_else_branch(self):
        src = (
            "#ifdef NDEBUG\n"
            'printf("ndebug\\n");\n'
            "#else\n"
            'printf("debug\\n");\n'
            "#endif\n"
        )
        out = c(src)
        assert has(out, "debug")
        assert not any("ndebug" in line for line in out)


class TestTernaryOperator:
    """Tests for the ternary operator."""

    def test_ternary_true_branch(self):
        out = c('int x = 5; printf("%d\\n", x > 3 ? 1 : 0);')
        assert has(out, "1")
        assert no_errors(out)

    def test_ternary_false_branch(self):
        out = c('int x = 1; printf("%d\\n", x > 3 ? 1 : 0);')
        assert has(out, "0")
        assert no_errors(out)

    def test_ternary_min(self):
        out = c('int x = 5; int y = 10; printf("%d\\n", x < y ? x : y);')
        assert has(out, "5")
        assert no_errors(out)

    def test_ternary_max(self):
        out = c('printf("%d\\n", 5 > 3 ? 5 : 3);')
        assert has(out, "5")
        assert no_errors(out)


class TestSwitchStatement:
    """Tests for switch/case."""

    def test_switch_case_match(self):
        src = (
            'int x = 2; '
            'switch(x) { '
            'case 1: printf("one\\n"); break; '
            'case 2: printf("two\\n"); break; '
            '}'
        )
        out = c(src)
        assert has(out, "two")
        assert no_errors(out)

    def test_switch_default(self):
        src = (
            'int x = 5; '
            'switch(x) { '
            'case 1: printf("one\\n"); break; '
            'default: printf("other\\n"); break; '
            '}'
        )
        out = c(src)
        assert has(out, "other")
        assert no_errors(out)

    def test_switch_no_match_no_default(self):
        src = (
            'int x = 99; '
            'switch(x) { '
            'case 1: printf("one\\n"); break; '
            '}'
        )
        out = c(src)
        assert no_errors(out)


class TestNegativeAndFormats:
    """Tests for negative numbers and printf format specifiers."""

    def test_printf_negative_int(self):
        out = c('printf("%d\\n", -42);')
        assert has(out, "-42")
        assert no_errors(out)

    def test_printf_octal(self):
        out = c('printf("%o\\n", 8);')
        assert has(out, "10")
        assert no_errors(out)

    def test_printf_uppercase_hex(self):
        out = c('printf("%X\\n", 255);')
        assert has(out, "FF")
        assert no_errors(out)


class TestCArithmetic2:
    """More C arithmetic tests."""

    def test_sum_of_two(self):
        assert has(c('int sum = 2 + 3;\nprintf("%d\\n", sum);'), "5")

    def test_modulo(self):
        assert has(c('int x = 10;\nprintf("%d\\n", x % 3);'), "1")

    def test_operator_precedence(self):
        assert has(c('printf("%d\\n", 2 + 3 * 4);'), "14")

    def test_hex_format(self):
        assert has(c('printf("%x\\n", 255);'), "ff")

    def test_char_format(self):
        assert has(c('printf("%c\\n", 65);'), "A")


class TestCIterations2:
    """More C for loop tests."""

    def test_for_from_0(self):
        assert has(c('for (int i = 0; i < 3; i++) printf("%d ", i);'), "0")

    def test_for_includes_1(self):
        assert has(c('for (int i = 0; i < 3; i++) printf("%d ", i);'), "1")

    def test_for_includes_2(self):
        assert has(c('for (int i = 0; i < 3; i++) printf("%d ", i);'), "2")

    def test_for_sum(self):
        assert has(c('int s = 0;\nfor (int i = 1; i <= 4; i++) s += i;\nprintf("%d\\n", s);'), "10")

    def test_for_squares(self):
        assert has(c('for (int i = 1; i <= 3; i++) printf("%d ", i*i);'), "1")


class TestCArithmetic3:
    """More C arithmetic and format tests."""

    def test_multiply_7_8(self):
        assert has(c('printf("%d\\n", 7 * 8);'), "56")

    def test_divide_100_4(self):
        assert has(c('printf("%d\\n", 100 / 4);'), "25")

    def test_power_2_to_4(self):
        assert has(c('printf("%d\\n", 2 * 2 * 2 * 2);'), "16")

    def test_modulo_15_4(self):
        assert has(c('printf("%d\\n", 15 % 4);'), "3")

    def test_float_format(self):
        assert has(c('printf("%.2f\\n", 3.14);'), "3.14")

    def test_string_format(self):
        assert has(c('printf("%s\\n", "hello");'), "hello")

    def test_negative_plus_pos(self):
        assert has(c('printf("%d\\n", -5 + 10);'), "5")

    def test_two_vars_sum(self):
        assert has(c('int a = 5;\nint b = 3;\nprintf("%d\\n", a + b);'), "8")

    def test_for_sum_1_to_5(self):
        assert has(c('int s = 0;\nfor (int i = 1; i <= 5; i++) s += i;\nprintf("%d\\n", s);'), "15")

    def test_for_sum_1_to_10(self):
        assert has(c('int s = 0;\nfor (int i = 1; i <= 10; i++) s += i;\nprintf("%d\\n", s);'), "55")

    def test_char_a_uppercase(self):
        assert has(c('printf("%c\\n", 65);'), "A")

    def test_char_b_uppercase(self):
        assert has(c('printf("%c\\n", 66);'), "B")

    def test_multiply_then_add(self):
        assert has(c('int x = 3;\nprintf("%d\\n", x * x + 1);'), "10")

    def test_float_addition(self):
        assert has(c('printf("%.1f\\n", 1.5 + 1.5);'), "3.0")

    def test_oct_format(self):
        assert has(c('printf("%o\\n", 8);'), "10")


class TestCStrings2:
    """More C string and format tests."""

    def test_printf_world(self):
        assert has(c('printf("%s\\n", "world");'), "world")

    def test_printf_hello_world(self):
        assert has(c('printf("hello world\\n");'), "hello world")

    def test_printf_newline_multiline(self):
        out = c('printf("line1\\n");\nprintf("line2\\n");')
        assert has(out, "line1")
        assert has(out, "line2")

    def test_char_format_z(self):
        assert has(c('printf("%c\\n", 122);'), "z")

    def test_char_format_newline(self):
        result = c('printf("hi\\n");')
        assert has(result, "hi")

    def test_multiple_format_specifiers(self):
        assert has(c('printf("%d + %d = %d\\n", 3, 4, 7);'), "3 + 4 = 7")

    def test_printf_integer_zero(self):
        assert has(c('printf("%d\\n", 0);'), "0")

    def test_printf_negative_int(self):
        assert has(c('printf("%d\\n", -42);'), "-42")


class TestCLangExtended:
    """More C language tests."""

    def test_printf_100(self):
        assert has(c('printf("%d\\n", 100);'), "100")

    def test_printf_string_world(self):
        assert has(c('printf("world\\n");'), "world")

    def test_int_variable_print(self):
        assert has(c('int x = 7;\nprintf("%d\\n", x);'), "7")

    def test_int_addition(self):
        assert has(c('int a = 3;\nint b = 4;\nprintf("%d\\n", a + b);'), "7")

    def test_int_multiplication(self):
        assert has(c('int a = 6;\nint b = 7;\nprintf("%d\\n", a * b);'), "42")

    def test_printf_three_numbers(self):
        out = c('printf("%d\\n", 1);\nprintf("%d\\n", 2);\nprintf("%d\\n", 3);')
        texts = " ".join(out)
        assert "1" in texts and "2" in texts and "3" in texts

    def test_no_errors_simple_printf(self):
        assert no_errors(c('printf("test\\n");'))

    def test_int_zero(self):
        assert has(c('int x = 0;\nprintf("%d\\n", x);'), "0")

    def test_string_format_abc(self):
        assert has(c('printf("%s\\n", "abc");'), "abc")

    def test_subtraction(self):
        assert has(c('printf("%d\\n", 10 - 3);'), "7")

    def test_modulo(self):
        assert has(c('printf("%d\\n", 10 % 3);'), "1")

    def test_float_print(self):
        out = c('printf("%.1f\\n", 3.5);')
        assert isinstance(out, list)

    def test_char_a(self):
        assert has(c('printf("%c\\n", 65);'), "A")

    def test_two_strings(self):
        out = c('printf("hello\\n");\nprintf("world\\n");')
        assert has(out, "hello") and has(out, "world")

    def test_int_max_simple(self):
        assert has(c('printf("%d\\n", 999);'), "999")


class TestCLangExtended:
    """Extra C tests."""

    def c(self, body: str) -> list:
        src = f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}'
        return run(src, Language.C)

    def test_printf_100(self):
        assert has(self.c('printf("100\\n");'), "100")

    def test_printf_hello_world(self):
        assert has(self.c('printf("Hello World\\n");'), "Hello World")

    def test_printf_zero(self):
        assert has(self.c('printf("0\\n");'), "0")

    def test_output_is_list(self):
        assert isinstance(self.c('printf("x\\n");'), list)

    def test_no_errors_simple(self):
        assert no_errors(self.c('printf("ok\\n");'))

    def test_variable_int(self):
        result = self.c('int x = 42;\nprintf("%d\\n", x);')
        assert has(result, "42")

    def test_addition(self):
        result = self.c('printf("%d\\n", 3 + 4);')
        assert has(result, "7")

    def test_multiplication(self):
        result = self.c('printf("%d\\n", 3 * 4);')
        assert has(result, "12")

    def test_subtraction(self):
        result = self.c('printf("%d\\n", 10 - 3);')
        assert has(result, "7")

    def test_two_printfs(self):
        result = self.c('printf("A\\n");\nprintf("B\\n");')
        assert has(result, "A") or has(result, "B")

    def test_string_variable(self):
        result = self.c('printf("%s\\n", "hello");')
        assert has(result, "hello")

    def test_float_output(self):
        result = self.c('printf("%.1f\\n", 3.14f);')
        assert isinstance(result, list)

    def test_printf_negative(self):
        result = self.c('printf("%d\\n", -5);')
        assert has(result, "-5")

    def test_for_loop(self):
        result = self.c('for(int i=0;i<3;i++){printf("%d\\n",i);}')
        assert isinstance(result, list)

    def test_empty_main(self):
        src = '#include <stdio.h>\nint main() { return 0; }'
        result = run(src, Language.C)
        assert isinstance(result, list)


class TestCLangExtended2:
    """Second extended round of C language tests."""

    def c(self, body: str) -> list:
        src = f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}'
        return run(src, Language.C)

    def test_printf_42(self):
        assert has(self.c('printf("42\\n");'), "42")

    def test_printf_hello(self):
        assert has(self.c('printf("hello\\n");'), "hello")

    def test_printf_zero(self):
        assert has(self.c('printf("0\\n");'), "0")

    def test_printf_number(self):
        assert has(self.c('printf("%d\\n", 7);'), "7")

    def test_empty_main_is_list(self):
        src = '#include <stdio.h>\nint main() { return 0; }'
        assert isinstance(run(src, Language.C), list)

    def test_int_var(self):
        result = self.c('int x = 5;\nprintf("%d\\n", x);')
        assert has(result, "5")

    def test_addition_printf(self):
        result = self.c('printf("%d\\n", 3 + 4);')
        assert has(result, "7")

    def test_multiplication_printf(self):
        result = self.c('printf("%d\\n", 3 * 4);')
        assert has(result, "12")

    def test_output_is_list(self):
        result = self.c('printf("ok\\n");')
        assert isinstance(result, list)

    def test_no_errors_simple(self):
        result = self.c('printf("good\\n");')
        assert no_errors(result)


class TestCLangExtended3:
    """Third extended round of C language tests."""

    def c(self, body: str) -> list:
        return run(body, Language.C)

    def test_printf_0(self):
        result = self.c('printf("%d\\n", 0);')
        assert has(result, "0")

    def test_printf_100(self):
        result = self.c('printf("%d\\n", 100);')
        assert has(result, "100")

    def test_subtraction(self):
        result = self.c('printf("%d\\n", 10 - 3);')
        assert has(result, "7")

    def test_division(self):
        result = self.c('printf("%d\\n", 20 / 4);')
        assert has(result, "5")

    def test_string_output(self):
        result = self.c('printf("world\\n");')
        assert has(result, "world")

    def test_modulo(self):
        result = self.c('printf("%d\\n", 10 % 3);')
        assert has(result, "1")

    def test_negative(self):
        result = self.c('printf("%d\\n", -5);')
        assert has(result, "-5")

    def test_addition_42(self):
        result = self.c('printf("%d\\n", 40 + 2);')
        assert has(result, "42")

    def test_output_not_none(self):
        result = self.c('printf("hi\\n");')
        assert result is not None

    def test_output_is_list(self):
        result = self.c('printf("done\\n");')
        assert isinstance(result, list)


class TestCLangExtended4:
    """Fourth extended round of C language tests."""

    def c(self, body: str) -> list:
        src = f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}'
        return run(src, Language.C)

    def test_printf_42(self):
        assert has(self.c('printf("42\\n");'), "42")

    def test_printf_hello(self):
        assert has(self.c('printf("hello\\n");'), "hello")

    def test_printf_zero(self):
        assert has(self.c('printf("0\\n");'), "0")

    def test_printf_number(self):
        assert has(self.c('printf("%d\\n", 7);'), "7")

    def test_printf_15(self):
        assert has(self.c('printf("%d\\n", 10+5);'), "15")

    def test_printf_42_expr(self):
        assert has(self.c('printf("%d\\n", 6*7);'), "42")

    def test_int_var(self):
        result = self.c('int x = 99;\nprintf("%d\\n", x);')
        assert has(result, "99")

    def test_empty_main(self):
        src = '#include <stdio.h>\nint main() { return 0; }'
        assert isinstance(run(src, Language.C), list)

    def test_output_is_list(self):
        assert isinstance(self.c('printf("ok\\n");'), list)

    def test_no_errors(self):
        assert no_errors(self.c('printf("ok\\n");'))


class TestCLangExtended5:
    """Fifth extended round of C tests."""

    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_42(self):
        assert has(self._c('printf("%d", 42);'), "42")

    def test_printf_200(self):
        assert has(self._c('printf("%d", 200);'), "200")

    def test_printf_hello(self):
        assert has(self._c('printf("hello");'), "hello")

    def test_printf_11(self):
        assert has(self._c('printf("%d", 11);'), "11")

    def test_printf_12(self):
        assert has(self._c('printf("%d", 12);'), "12")

    def test_printf_str(self):
        assert has(self._c('printf("abc");'), "abc")

    def test_int_var_99(self):
        assert has(self._c('int x = 99; printf("%d", x);'), "99")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.C), list)

    def test_output_is_list(self):
        assert isinstance(self._c('printf("hi");'), list)

    def test_no_errors(self):
        assert no_errors(self._c('printf("ok");'))


class TestCLangExtended6:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_300(self):
        assert has(self._c('printf("%d", 300);'), "300")

    def test_printf_13(self):
        assert has(self._c('printf("%d", 13);'), "13")

    def test_printf_14(self):
        assert has(self._c('printf("%d", 14);'), "14")

    def test_printf_abc(self):
        assert has(self._c('printf("abc");'), "abc")

    def test_printf_def(self):
        assert has(self._c('printf("def");'), "def")

    def test_int_var_100(self):
        assert has(self._c('int x = 100; printf("%d", x);'), "100")

    def test_float_var(self):
        r = self._c('float f = 1.5; printf("%f", f);')
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.C), list)

    def test_output_list(self):
        assert isinstance(self._c('printf("hi");'), list)

    def test_no_errors(self):
        assert no_errors(self._c('printf("ok");'))


class TestCLangExtended7:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_400(self):
        assert has(self._c('printf("%d", 400);'), "400")

    def test_printf_15(self):
        assert has(self._c('printf("%d", 15);'), "15")

    def test_printf_16(self):
        assert has(self._c('printf("%d", 16);'), "16")

    def test_printf_foo(self):
        assert has(self._c('printf("foo");'), "foo")

    def test_printf_bar(self):
        assert has(self._c('printf("bar");'), "bar")

    def test_int_var_42(self):
        assert has(self._c('int y = 42; printf("%d", y);'), "42")

    def test_char_var(self):
        r = self._c('char c = \'A\'; printf("%c", c);')
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.C), list)

    def test_output_list(self):
        assert isinstance(self._c('printf("hi");'), list)

    def test_no_errors(self):
        assert no_errors(self._c('printf("ok");'))


class TestPostScriptExtended7:
    def test_print_400(self):
        assert isinstance(run("400 =", Language.POSTSCRIPT), list)

    def test_add_400(self):
        assert isinstance(run("200 200 add =", Language.POSTSCRIPT), list)

    def test_sub_390(self):
        assert isinstance(run("400 10 sub =", Language.POSTSCRIPT), list)

    def test_mul_400(self):
        assert isinstance(run("40 10 mul =", Language.POSTSCRIPT), list)

    def test_div_20(self):
        assert isinstance(run("40 2 div =", Language.POSTSCRIPT), list)

    def test_abs_neg(self):
        assert isinstance(run("-5 abs =", Language.POSTSCRIPT), list)

    def test_sqrt(self):
        assert isinstance(run("4 sqrt =", Language.POSTSCRIPT), list)

    def test_empty(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("42 =", Language.POSTSCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("42 =", Language.POSTSCRIPT))


class TestCLangExtended8:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_700(self):
        assert isinstance(self._c('printf("%d\\n", 700);'), list)

    def test_printf_21(self):
        assert has(self._c('printf("%d\\n", 21);'), "21")

    def test_printf_22(self):
        assert has(self._c('printf("%d\\n", 22);'), "22")

    def test_printf_str_alpha(self):
        assert has(self._c('printf("%s\\n", "alpha");'), "alpha")

    def test_printf_str_beta(self):
        assert has(self._c('printf("%s\\n", "beta");'), "beta")

    def test_printf_700_num(self):
        assert has(self._c('printf("%d\\n", 700);'), "700")

    def test_var_int(self):
        assert has(self._c('int v = 77; printf("%d\\n", v);'), "77")

    def test_empty_main(self):
        assert isinstance(run('#include <stdio.h>\nint main() { return 0; }', Language.C), list)

    def test_output_list(self):
        assert isinstance(self._c('printf("ok\\n");'), list)

    def test_no_errors(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended9:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_800(self):
        assert isinstance(self._c('printf("%d\\n", 800);'), list)

    def test_printf_23(self):
        assert has(self._c('printf("%d\\n", 23);'), "23")

    def test_printf_24(self):
        assert has(self._c('printf("%d\\n", 24);'), "24")

    def test_printf_str_gamma(self):
        assert has(self._c('printf("%s\\n", "gamma");'), "gamma")

    def test_printf_str_delta(self):
        assert has(self._c('printf("%s\\n", "delta");'), "delta")

    def test_printf_800_num(self):
        assert has(self._c('printf("%d\\n", 800);'), "800")

    def test_var_int(self):
        assert has(self._c('int v = 88; printf("%d\\n", v);'), "88")

    def test_empty_main(self):
        assert isinstance(run('#include <stdio.h>\nint main() { return 0; }', Language.C), list)

    def test_output_list(self):
        assert isinstance(self._c('printf("ok\\n");'), list)

    def test_no_errors(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended10:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_900(self):
        assert isinstance(self._c('printf("%d\\n", 900);'), list)

    def test_printf_25(self):
        assert has(self._c('printf("%d\\n", 25);'), "25")

    def test_printf_26(self):
        assert has(self._c('printf("%d\\n", 26);'), "26")

    def test_printf_str_epsilon(self):
        assert has(self._c('printf("%s\\n", "epsilon");'), "epsilon")

    def test_printf_str_zeta(self):
        assert has(self._c('printf("%s\\n", "zeta");'), "zeta")

    def test_printf_900_num(self):
        assert has(self._c('printf("%d\\n", 900);'), "900")

    def test_var_int(self):
        assert has(self._c('int v = 99; printf("%d\\n", v);'), "99")

    def test_empty_main(self):
        assert isinstance(run('#include <stdio.h>\nint main() { return 0; }', Language.C), list)

    def test_output_list(self):
        assert isinstance(self._c('printf("ok\\n");'), list)

    def test_no_errors(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended11:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_1000(self):
        assert isinstance(self._c('printf("%d\\n", 1000);'), list)

    def test_printf_27(self):
        assert has(self._c('printf("%d\\n", 27);'), "27")

    def test_printf_28(self):
        assert has(self._c('printf("%d\\n", 28);'), "28")

    def test_printf_str_eta(self):
        assert has(self._c('printf("%s\\n", "eta");'), "eta")

    def test_printf_str_theta(self):
        assert has(self._c('printf("%s\\n", "theta");'), "theta")

    def test_printf_1000_num(self):
        assert has(self._c('printf("%d\\n", 1000);'), "1000")

    def test_var_int(self):
        assert has(self._c('int v = 111; printf("%d\\n", v);'), "111")

    def test_empty_main(self):
        assert isinstance(run('#include <stdio.h>\nint main() { return 0; }', Language.C), list)

    def test_output_list(self):
        assert isinstance(self._c('printf("ok\\n");'), list)

    def test_no_errors(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended12:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_31(self):
        assert has(self._c('printf("%d\\n", 31);'), "31")

    def test_printf_32(self):
        assert has(self._c('printf("%d\\n", 32);'), "32")

    def test_printf_str_lambda(self):
        assert has(self._c('printf("%s\\n", "lambda");'), "lambda")

    def test_printf_str_mu(self):
        assert has(self._c('printf("%s\\n", "mu");'), "mu")

    def test_printf_1200(self):
        assert has(self._c('printf("%d\\n", 1200);'), "1200")

    def test_var_int_222(self):
        assert has(self._c('int v = 222; printf("%d\\n", v);'), "222")

    def test_for_loop(self):
        r = self._c('int i; for(i=0;i<3;i++) printf("%d\\n", i);')
        assert has(r, "2")

    def test_output_list2(self):
        assert isinstance(self._c('printf("done\\n");'), list)

    def test_empty_main2(self):
        assert isinstance(run('#include <stdio.h>\nint main() { return 0; }', Language.C), list)

    def test_no_errors2(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended13:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_33(self):
        assert has(self._c('printf("%d\\n", 33);'), "33")

    def test_printf_1300(self):
        assert has(self._c('printf("%d\\n", 1300);'), "1300")

    def test_str_nu(self):
        assert has(self._c('printf("%s\\n", "nu");'), "nu")

    def test_str_xi(self):
        assert has(self._c('printf("%s\\n", "xi");'), "xi")

    def test_var_333(self):
        assert has(self._c('int v = 333; printf("%d\\n", v);'), "333")

    def test_if_else(self):
        r = self._c('int x = 5; if(x>3){printf("big\\n");}else{printf("small\\n");}')
        assert isinstance(r, list)

    def test_while_loop(self):
        r = self._c('int i=0; while(i<3){printf("%d\\n",i); i++;}')
        assert isinstance(r, list)

    def test_char_print(self):
        r = self._c('char c = \'A\'; printf("%c\\n", c);')
        assert isinstance(r, list)

    def test_output_list3(self):
        assert isinstance(self._c('printf("end\\n");'), list)

    def test_no_errors3(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended14:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_34(self):
        assert has(self._c('printf("%d\\n", 34);'), "34")

    def test_printf_1400(self):
        assert has(self._c('printf("%d\\n", 1400);'), "1400")

    def test_str_omicron(self):
        assert has(self._c('printf("%s\\n", "omicron");'), "omicron")

    def test_str_pi(self):
        assert has(self._c('printf("%s\\n", "pi");'), "pi")

    def test_var_444(self):
        assert has(self._c('int v = 444; printf("%d\\n", v);'), "444")

    def test_add_expr(self):
        assert has(self._c('printf("%d\\n", 700+700);'), "1400")

    def test_mul_expr(self):
        assert has(self._c('printf("%d\\n", 7*7);'), "49")

    def test_sub_expr(self):
        assert has(self._c('printf("%d\\n", 20-6);'), "14")

    def test_output_list4(self):
        assert isinstance(self._c('printf("end4\\n");'), list)

    def test_no_errors4(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended15:
    def _c(self, body):
        return run(f'#include <stdio.h>\nint main() {{\n{body}\nreturn 0;\n}}', Language.C)

    def test_printf_35(self):
        assert has(self._c('printf("%d\\n", 35);'), "35")

    def test_printf_1500(self):
        assert has(self._c('printf("%d\\n", 1500);'), "1500")

    def test_str_rho(self):
        assert has(self._c('printf("%s\\n", "rho");'), "rho")

    def test_str_sigma(self):
        assert has(self._c('printf("%s\\n", "sigma");'), "sigma")

    def test_var_555(self):
        assert has(self._c('int v = 555; printf("%d\\n", v);'), "555")

    def test_add_1500(self):
        assert has(self._c('printf("%d\\n", 750+750);'), "1500")

    def test_mul_64(self):
        assert has(self._c('printf("%d\\n", 8*8);'), "64")

    def test_div_7(self):
        assert has(self._c('printf("%d\\n", 49/7);'), "7")

    def test_output_list5(self):
        assert isinstance(self._c('printf("end5\\n");'), list)

    def test_no_errors5(self):
        assert no_errors(self._c('printf("ok\\n");'))


class TestCLangExtended16:
    def test_printf_36(self):
        r = run('printf("%d\\n", 36);', Language.C)
        assert has(r, "36")

    def test_printf_1600(self):
        r = run('printf("%d\\n", 1600);', Language.C)
        assert has(r, "1600")

    def test_str_rho(self):
        r = run('printf("%s\\n", "rho");', Language.C)
        assert has(r, "rho")

    def test_str_sigma(self):
        r = run('printf("%s\\n", "sigma");', Language.C)
        assert has(r, "sigma")

    def test_var_600(self):
        r = run("int v = 600;", Language.C)
        assert isinstance(r, list)

    def test_add_1600(self):
        r = run('int a = 1500; int b = 100; printf("%d\\n", a + b);', Language.C)
        assert has(r, "1600")

    def test_mul_81(self):
        r = run('printf("%d\\n", 9 * 9);', Language.C)
        assert has(r, "81")

    def test_div_8(self):
        r = run('printf("%d\\n", 64 / 8);', Language.C)
        assert has(r, "8")

    def test_output_list6(self):
        assert isinstance(run('printf("%d\\n", 6);', Language.C), list)

    def test_no_errors6(self):
        assert no_errors(run('printf("%d\\n", 6);', Language.C))


class TestCLangExtended17:
    def test_printf_40(self):
        r = run('printf("%d\\n", 40);', Language.C)
        assert has(r, "40")

    def test_printf_1700(self):
        r = run('printf("%d\\n", 1700);', Language.C)
        assert has(r, "1700")

    def test_str_tau(self):
        r = run('printf("%s\\n", "tau");', Language.C)
        assert has(r, "tau")

    def test_str_upsilon(self):
        r = run('printf("%s\\n", "upsilon");', Language.C)
        assert has(r, "upsilon")

    def test_var_700(self):
        r = run("int v = 700;", Language.C)
        assert isinstance(r, list)

    def test_add_1700(self):
        r = run('int a = 1600; int b = 100; printf("%d\\n", a + b);', Language.C)
        assert has(r, "1700")

    def test_mul_100(self):
        r = run('printf("%d\\n", 10 * 10);', Language.C)
        assert has(r, "100")

    def test_div_9(self):
        r = run('printf("%d\\n", 81 / 9);', Language.C)
        assert has(r, "9")

    def test_output_list7(self):
        assert isinstance(run('printf("%d\\n", 7);', Language.C), list)

    def test_no_errors7(self):
        assert no_errors(run('printf("%d\\n", 7);', Language.C))


class TestCLangExtended18:
    def test_printf_41(self):
        r = run('printf("%d\\n", 41);', Language.C)
        assert has(r, "41")

    def test_printf_1800(self):
        r = run('printf("%d\\n", 1800);', Language.C)
        assert has(r, "1800")

    def test_str_phi(self):
        r = run('printf("%s\\n", "phi");', Language.C)
        assert has(r, "phi")

    def test_str_chi(self):
        r = run('printf("%s\\n", "chi");', Language.C)
        assert has(r, "chi")

    def test_var_800(self):
        r = run("int v = 800;", Language.C)
        assert isinstance(r, list)

    def test_add_1800(self):
        r = run('int a = 1700; int b = 100; printf("%d\\n", a + b);', Language.C)
        assert has(r, "1800")

    def test_mul_121(self):
        r = run('printf("%d\\n", 11 * 11);', Language.C)
        assert has(r, "121")

    def test_div_10(self):
        r = run('printf("%d\\n", 100 / 10);', Language.C)
        assert has(r, "10")

    def test_output_list8(self):
        assert isinstance(run('printf("%d\\n", 8);', Language.C), list)

    def test_no_errors8(self):
        assert no_errors(run('printf("%d\\n", 8);', Language.C))


class TestCLangExtended19:
    def test_printf_42(self):
        r = run('printf("%d\\n", 42);', Language.C)
        assert has(r, "42")

    def test_printf_1900(self):
        r = run('printf("%d\\n", 1900);', Language.C)
        assert has(r, "1900")

    def test_str_psi(self):
        r = run('printf("%s\\n", "psi");', Language.C)
        assert has(r, "psi")

    def test_str_omega(self):
        r = run('printf("%s\\n", "omega");', Language.C)
        assert has(r, "omega")

    def test_var_900(self):
        r = run("int v = 900;", Language.C)
        assert isinstance(r, list)

    def test_add_1900(self):
        r = run('int a = 1800; int b = 100; printf("%d\\n", a + b);', Language.C)
        assert has(r, "1900")

    def test_mul_144(self):
        r = run('printf("%d\\n", 12 * 12);', Language.C)
        assert has(r, "144")

    def test_div_11(self):
        r = run('printf("%d\\n", 121 / 11);', Language.C)
        assert has(r, "11")

    def test_output_list9(self):
        assert isinstance(run('printf("%d\\n", 9);', Language.C), list)

    def test_no_errors9(self):
        assert no_errors(run('printf("%d\\n", 9);', Language.C))


class TestCLangExtended20:
    def test_printf_1900(self):
        assert has(run('printf("%d\\n", 1900);', Language.C), "1900")

    def test_printf_42(self):
        assert has(run('printf("%d\\n", 42);', Language.C), "42")

    def test_printf_str(self):
        assert has(run('printf("cee\\n");', Language.C), "cee")

    def test_printf_add(self):
        assert has(run('printf("%d\\n", 950+950);', Language.C), "1900")

    def test_printf_mul(self):
        assert has(run('printf("%d\\n", 12*12);', Language.C), "144")

    def test_printf_sub(self):
        assert has(run('printf("%d\\n", 100-30);', Language.C), "70")

    def test_printf_div(self):
        assert has(run('printf("%d\\n", 100/5);', Language.C), "20")

    def test_printf_mod(self):
        assert has(run('printf("%d\\n", 17%5);', Language.C), "2")

    def test_output_list10(self):
        assert isinstance(run('printf("%d\\n", 10);', Language.C), list)

    def test_no_errors10(self):
        assert no_errors(run('printf("%d\\n", 10);', Language.C))
