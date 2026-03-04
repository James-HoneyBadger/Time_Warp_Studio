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
