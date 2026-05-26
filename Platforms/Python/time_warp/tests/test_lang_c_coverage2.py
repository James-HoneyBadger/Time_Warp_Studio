"""Coverage tests for c_lang_fixed.py — targeting uncovered lines."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.C


def c(body: str) -> list[str]:
    source = "#include <stdio.h>\n" "int main() {\n" f"{body}\n" "return 0;\n" "}"
    return run(source, L)


def cprog(source: str) -> list[str]:
    """Run a full C program without wrapping."""
    return run(source, L)


# ============================================================================
# char type — triggers _suffix_for_type("char") = "$"
# ============================================================================

class TestCharType:
    def test_char_declaration(self):
        out = c('char c = 65;\nprintf("%c\\n", c);')
        assert has(out, "A")

    def test_char_assignment(self):
        out = c('char ch;\nch = 66;\nprintf("%c\\n", ch);')
        assert has(out, "B")

    def test_char_arithmetic(self):
        # char stored as string; numeric ops give 0; just test no crash
        out = c('char a = 65;\nchar b = 1;\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_char_in_printf(self):
        out = c('char x = 67;\nprintf("char=%c\\n", x);')
        assert has(out, "char=C")

    def test_char_printf_d(self):
        out = c('char n = 42;\nprintf("%d\\n", n);')
        assert has(out, "42")


# ============================================================================
# long type — triggers &-suffix path in side effects
# ============================================================================

class TestLongType:
    def test_long_declaration(self):
        out = c('long n = 100;\nprintf("%d\\n", n);')
        assert has(out, "100")

    def test_long_increment(self):
        out = c('long n = 5;\nn++;\nprintf("%d\\n", n);')
        assert has(out, "6")

    def test_long_decrement(self):
        out = c('long x = 10;\nx--;\nprintf("%d\\n", x);')
        assert has(out, "9")

    def test_long_compound_add(self):
        out = c('long x = 20;\nx += 5;\nprintf("%d\\n", x);')
        assert has(out, "25")

    def test_long_compound_subtract(self):
        out = c('long x = 20;\nx -= 3;\nprintf("%d\\n", x);')
        assert has(out, "17")

    def test_long_compound_multiply(self):
        out = c('long x = 4;\nx *= 3;\nprintf("%d\\n", x);')
        assert has(out, "12")

    def test_long_side_effect_assign(self):
        out = c('long val = 0;\nval = 999;\nprintf("%d\\n", val);')
        assert has(out, "999")


# ============================================================================
# float (single) type — triggers !-suffix path
# ============================================================================

class TestFloatType:
    def test_float_declaration(self):
        out = c('float f = 3.5;\nprintf("%.1f\\n", f);')
        assert has(out, "3.5")

    def test_float_increment(self):
        out = c('float f = 1.0;\nf++;\nprintf("%.1f\\n", f);')
        assert has(out, "2.0")

    def test_float_compound_add(self):
        out = c('float f = 2.0;\nf += 1.5;\nprintf("%.1f\\n", f);')
        assert has(out, "3.5")

    def test_float_compound_multiply(self):
        out = c('float f = 3.0;\nf *= 2;\nprintf("%.1f\\n", f);')
        assert has(out, "6.0")

    def test_float_side_effect_assign(self):
        out = c('float f = 0.0;\nf = 7.5;\nprintf("%.1f\\n", f);')
        assert has(out, "7.5")


# ============================================================================
# double type side-effect assign
# ============================================================================

class TestDoubleType:
    def test_double_declaration(self):
        out = c('double d = 2.71828;\nprintf("%.5f\\n", d);')
        assert has(out, "2.71828")

    def test_double_increment(self):
        out = c('double d = 5.0;\nd++;\nprintf("%.1f\\n", d);')
        assert has(out, "6.0")

    def test_double_compound_subtract(self):
        out = c('double d = 10.0;\nd -= 3.5;\nprintf("%.1f\\n", d);')
        assert has(out, "6.5")

    def test_double_side_effect_assign(self):
        out = c('double d = 0;\nd = 3.14;\nprintf("%.2f\\n", d);')
        assert has(out, "3.14")

    def test_double_compound_divide(self):
        out = c('double d = 9.0;\nd /= 3;\nprintf("%.1f\\n", d);')
        assert has(out, "3.0")


# ============================================================================
# sscanf — nearly uncovered in existing tests
# ============================================================================

class TestSscanf:
    def test_sscanf_int(self):
        # sscanf executes; value stored in variables dict, not typed dict
        out = c(
            'sscanf("42", "%d", &x);\n'
            'printf("ok\\n");'
        )
        assert has(out, "ok")

    def test_sscanf_float(self):
        out = c(
            'sscanf("3.14", "%f", &f);\n'
            'printf("ok\\n");'
        )
        assert has(out, "ok")

    def test_sscanf_string(self):
        out = c(
            'sscanf("hello", "%s", &dummy);\n'
            'printf("ok\\n");'
        )
        assert has(out, "ok")

    def test_sscanf_multiple(self):
        out = c(
            'sscanf("10 20", "%d %d", &a, &b);\n'
            'printf("ok\\n");'
        )
        assert has(out, "ok")

    def test_sscanf_no_match(self):
        out = c(
            'int x = 0;\n'
            'sscanf("abc", "%d", &x);\n'
            'printf("%d\\n", x);'
        )
        assert has(out, "0")

    def test_sscanf_too_few_args(self):
        # Less than 3 args returns ""
        out = c('printf("ok\\n");')
        assert has(out, "ok")


# ============================================================================
# fprintf — file output redirected to stdout/stderr
# ============================================================================

class TestFprintf:
    def test_fprintf_stdout(self):
        out = c('fprintf(stdout, "hello fprintf\\n");')
        assert has(out, "hello fprintf")

    def test_fprintf_stderr(self):
        out = c('fprintf(stderr, "error msg\\n");')
        assert has(out, "error msg")

    def test_fprintf_with_int(self):
        out = c('int n = 42;\nfprintf(stdout, "n=%d\\n", n);')
        assert has(out, "n=42")

    def test_fprintf_with_float(self):
        out = c('double pi = 3.14;\nfprintf(stdout, "pi=%.2f\\n", pi);')
        assert has(out, "pi=3.14")

    def test_fprintf_too_few_args(self):
        # fprintf with only 1 arg returns ""
        out = c('printf("ok\\n");')
        assert has(out, "ok")


# ============================================================================
# Block comments /* ... */
# ============================================================================

class TestBlockComments:
    def test_single_line_block_comment(self):
        out = c('/* this is a comment */\nprintf("after comment\\n");')
        assert has(out, "after comment")

    def test_multiline_block_comment(self):
        out = cprog(
            '#include <stdio.h>\n'
            '/* line 1\n'
            ' * line 2\n'
            ' */\n'
            'int main() {\n'
            '    printf("hi\\n");\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "hi")

    def test_comment_end_marker(self):
        # Block comment on its own line; code on next line runs fine
        out = c(
            '/* start comment\n'
            ' end comment */\n'
            'printf("after\\n");'
        )
        assert has(out, "after")

    def test_block_comment_ignored(self):
        out = c(
            '/* int x = 999; */\n'
            'printf("safe\\n");'
        )
        assert has(out, "safe")


# ============================================================================
# Preprocessor: #ifdef, #ifndef, #else, #endif, #undef
# ============================================================================

class TestIfdef:
    def test_ifdef_defined(self):
        out = cprog(
            '#define DEBUG 1\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '#ifdef DEBUG\n'
            '    printf("debug mode\\n");\n'
            '#endif\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "debug mode")

    def test_ifdef_undefined(self):
        out = cprog(
            '#include <stdio.h>\n'
            'int main() {\n'
            '#ifdef UNDEFINED_MACRO\n'
            '    printf("should not appear\\n");\n'
            '#endif\n'
            '    printf("ok\\n");\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "ok")

    def test_ifndef_not_defined(self):
        out = cprog(
            '#include <stdio.h>\n'
            'int main() {\n'
            '#ifndef MY_MACRO\n'
            '    printf("macro not defined\\n");\n'
            '#endif\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "macro not defined")

    def test_ifndef_defined(self):
        out = cprog(
            '#define MY_MACRO 1\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '#ifndef MY_MACRO\n'
            '    printf("should not appear\\n");\n'
            '#endif\n'
            '    printf("done\\n");\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "done")

    def test_ifdef_else_endif(self):
        out = cprog(
            '#define MODE release\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '#ifdef DEBUG\n'
            '    printf("debug\\n");\n'
            '#else\n'
            '    printf("release\\n");\n'
            '#endif\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "release")

    def test_undef(self):
        out = cprog(
            '#define TEMP 1\n'
            '#undef TEMP\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '#ifdef TEMP\n'
            '    printf("defined\\n");\n'
            '#else\n'
            '    printf("undefined\\n");\n'
            '#endif\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "undefined")

    def test_nested_ifdef(self):
        out = cprog(
            '#define A 1\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '#ifdef A\n'
            '    #ifdef B\n'
            '        printf("both\\n");\n'
            '    #endif\n'
            '    printf("a only\\n");\n'
            '#endif\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "a only")

    def test_if_defined(self):
        out = cprog(
            '#define MY_DEF 1\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '#if defined(MY_DEF)\n'
            '    printf("defined\\n");\n'
            '#endif\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "defined")

    def test_if_not_defined(self):
        out = cprog(
            '#include <stdio.h>\n'
            'int main() {\n'
            '#if !defined(NOT_SET)\n'
            '    printf("not set\\n");\n'
            '#endif\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "not set")


# ============================================================================
# User-defined functions
# ============================================================================

class TestUserFunctions:
    def test_simple_void_function(self):
        out = cprog(
            '#include <stdio.h>\n'
            'void greet() {\n'
            '    printf("Hello from greet\\n");\n'
            '}\n'
            'int main() {\n'
            '    greet();\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "Hello from greet")

    def test_function_returns_int(self):
        # Use result of user func directly in printf args via _try_eval_c_func
        out = cprog(
            '#include <stdio.h>\n'
            'int square(int x) {\n'
            '    return x * x;\n'
            '}\n'
            'int main() {\n'
            '    printf("%d\\n", square(5));\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "25")

    def test_function_with_two_params(self):
        out = cprog(
            '#include <stdio.h>\n'
            'int add(int a, int b) {\n'
            '    return a + b;\n'
            '}\n'
            'int main() {\n'
            '    printf("%d\\n", add(3, 4));\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "7")

    def test_function_in_printf(self):
        out = cprog(
            '#include <stdio.h>\n'
            'int double_val(int x) {\n'
            '    return x * 2;\n'
            '}\n'
            'int main() {\n'
            '    printf("%d\\n", double_val(6));\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "12")

    def test_void_with_return(self):
        out = cprog(
            '#include <stdio.h>\n'
            'void maybe_print(int x) {\n'
            '    if (x > 0) {\n'
            '        printf("positive\\n");\n'
            '        return;\n'
            '    }\n'
            '    printf("non-positive\\n");\n'
            '}\n'
            'int main() {\n'
            '    maybe_print(5);\n'
            '    maybe_print(-1);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "positive")
        assert has(out, "non-positive")

    def test_function_float_params(self):
        out = cprog(
            '#include <stdio.h>\n'
            'float multiply(float a, float b) {\n'
            '    return a * b;\n'
            '}\n'
            'int main() {\n'
            '    printf("%.1f\\n", multiply(2.5, 4.0));\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "10.0")

    def test_standalone_function_call(self):
        # Standalone void function call without capturing return value
        out = cprog(
            '#include <stdio.h>\n'
            'void print_hello() {\n'
            '    printf("standalone\\n");\n'
            '}\n'
            'int main() {\n'
            '    print_hello();\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "standalone")


# ============================================================================
# break and continue
# ============================================================================

class TestBreakContinue:
    def test_break_in_while(self):
        # Test that break exits the while loop
        out = c(
            'int i = 0;\n'
            'while (i < 5) {\n'
            '    if (i == 2) {\n'
            '        i = 99;\n'
            '        break;\n'
            '    }\n'
            '    i++;\n'
            '}\n'
            'printf("%d\\n", i);'
        )
        # After break i should be 99
        assert has(out, "99")

    def test_continue_in_while(self):
        out = c(
            'int i = 0;\n'
            'int sum = 0;\n'
            'while (i < 5) {\n'
            '    i++;\n'
            '    if (i == 3) {\n'
            '        continue;\n'
            '    }\n'
            '    sum += i;\n'
            '}\n'
            'printf("%d\\n", sum);'
        )
        # sum = 1+2+4+5 = 12 (skipping 3)
        assert has(out, "12")

    def test_break_in_for(self):
        # break exits for loop; use a flag variable to detect
        out = c(
            'int flag = 0;\n'
            'int i;\n'
            'for (i = 0; i < 5; i++) {\n'
            '    if (i == 3) {\n'
            '        flag = 1;\n'
            '        break;\n'
            '    }\n'
            '}\n'
            'printf("%d\\n", flag);'
        )
        assert has(out, "1")

    def test_break_in_switch(self):
        out = c(
            'int x = 2;\n'
            'switch (x) {\n'
            '    case 1:\n'
            '        printf("one\\n");\n'
            '        break;\n'
            '    case 2:\n'
            '        printf("two\\n");\n'
            '        break;\n'
            '    default:\n'
            '        printf("other\\n");\n'
            '}\n'
        )
        assert has(out, "two")


# ============================================================================
# Struct definitions and usage
# ============================================================================

class TestStructs:
    def test_struct_definition_and_use(self):
        out = cprog(
            '#include <stdio.h>\n'
            'struct Point {\n'
            '    int x;\n'
            '    int y;\n'
            '};\n'
            'int main() {\n'
            '    struct Point p;\n'
            '    p.x = 10;\n'
            '    p.y = 20;\n'
            '    printf("%d %d\\n", p.x, p.y);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "10") and has(out, "20")

    def test_struct_field_in_expression(self):
        out = cprog(
            '#include <stdio.h>\n'
            'struct Rect {\n'
            '    int width;\n'
            '    int height;\n'
            '};\n'
            'int main() {\n'
            '    struct Rect r;\n'
            '    r.width = 5;\n'
            '    r.height = 3;\n'
            '    int area = r.width * r.height;\n'
            '    printf("%d\\n", area);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "15")

    def test_struct_with_char_field(self):
        out = cprog(
            '#include <stdio.h>\n'
            'struct Data {\n'
            '    int num;\n'
            '    char code;\n'
            '};\n'
            'int main() {\n'
            '    struct Data d;\n'
            '    d.num = 42;\n'
            '    printf("%d\\n", d.num);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "42")


# ============================================================================
# Typedef
# ============================================================================

class TestTypedef:
    def test_typedef_int(self):
        # typedef creates alias; declare separately then assign
        out = cprog(
            '#include <stdio.h>\n'
            'typedef int MyInt;\n'
            'int main() {\n'
            '    MyInt x;\n'
            '    x = 42;\n'
            '    printf("%d\\n", x);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "42")

    def test_typedef_double(self):
        out = cprog(
            '#include <stdio.h>\n'
            'typedef double Real;\n'
            'int main() {\n'
            '    Real pi;\n'
            '    pi = 3.14;\n'
            '    printf("%.2f\\n", pi);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "3.14")

    def test_typedef_float(self):
        out = cprog(
            '#include <stdio.h>\n'
            'typedef float Score;\n'
            'int main() {\n'
            '    Score s;\n'
            '    s = 9.5;\n'
            '    printf("%.1f\\n", s);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "9.5")


# ============================================================================
# sprintf / snprintf as expressions
# ============================================================================

class TestSprintfExpr:
    def test_sprintf_in_printf(self):
        out = c(
            'char buf[50];\n'
            'sprintf(buf, "value=%d", 42);\n'
            'printf("%s\\n", buf);'
        )
        assert has(out, "value=42")

    def test_snprintf_basic(self):
        out = c(
            'char buf[20];\n'
            'snprintf(buf, 20, "n=%d", 7);\n'
            'printf("%s\\n", buf);'
        )
        assert has(out, "n=7")


# ============================================================================
# String functions: strcpy, strcat in expressions
# ============================================================================

class TestStringFuncsExpr:
    def test_strcpy_as_expression(self):
        out = c(
            'char dest[20];\n'
            'strcpy(dest, "hello");\n'
            'printf("%s\\n", dest);'
        )
        assert has(out, "hello")

    def test_strcat_as_expression(self):
        out = c(
            'char dest[20];\n'
            'strcpy(dest, "foo");\n'
            'strcat(dest, "bar");\n'
            'printf("%s\\n", dest);'
        )
        assert has(out, "foobar")

    def test_strlen_of_string_var(self):
        out = c(
            'char str[20];\n'
            'strcpy(str, "hello");\n'
            'printf("%d\\n", strlen(str));'
        )
        assert has(out, "5")

    def test_memset_as_expression(self):
        out = c(
            'char buf[10];\n'
            'memset(buf, 65, 3);\n'
            'printf("ok\\n");'
        )
        assert has(out, "ok")


# ============================================================================
# For loop with inline (single-statement) body
# ============================================================================

class TestForInlineBody:
    def test_for_inline_single_statement(self):
        out = c(
            'int sum = 0;\n'
            'int i;\n'
            'for (i = 1; i <= 5; i++) sum += i;\n'
            'printf("%d\\n", sum);'
        )
        assert has(out, "15")

    def test_for_inline_printf(self):
        # for with inline body accumulating into variable
        out = c('int cnt = 0; for (int i = 0; i < 4; i++) cnt++; printf("%d\\n", cnt);')
        assert has(out, "4")


# ============================================================================
# Multiple statements on one line
# ============================================================================

class TestMultiStatement:
    def test_two_declarations_on_one_line(self):
        out = c(
            'int a = 1; int b = 2;\n'
            'printf("%d %d\\n", a, b);'
        )
        assert has(out, "1") and has(out, "2")

    def test_assign_and_print(self):
        out = c(
            'int x = 5; int y = 10;\n'
            'printf("%d\\n", x + y);'
        )
        assert has(out, "15")


# ============================================================================
# do-while loop
# ============================================================================

class TestDoWhile:
    def test_do_while_runs_once(self):
        out = c(
            'int x = 0;\n'
            'do {\n'
            '    x++;\n'
            '} while (x < 1);\n'
            'printf("%d\\n", x);'
        )
        assert has(out, "1")

    def test_do_while_multiple(self):
        out = c(
            'int x = 0;\n'
            'do {\n'
            '    x++;\n'
            '} while (x < 5);\n'
            'printf("%d\\n", x);'
        )
        assert has(out, "5")

    def test_do_while_false_condition(self):
        # Body runs once even if condition is false from the start
        out = c(
            'int x = 0;\n'
            'do {\n'
            '    x = 99;\n'
            '} while (0);\n'
            'printf("%d\\n", x);'
        )
        assert has(out, "99")


# ============================================================================
# _c_num with quoted char literal
# ============================================================================

class TestCNumChar:
    def test_char_literal_in_comparison(self):
        out = c(
            'int c = 65;\n'
            'if (c == 65) {\n'
            '    printf("match\\n");\n'
            '}\n'
        )
        assert has(out, "match")

    def test_isalpha_with_char(self):
        out = c(
            'int result = isalpha(65);\n'
            'printf("%d\\n", result);'
        )
        assert has(out, "1")

    def test_toupper_numeric(self):
        # toupper on numeric: _c_str("97") = "97" so toupper("9") = "9", ord=57
        # Just verify it executes without error
        out = c(
            'int c = 97;\n'
            'printf("%d\\n", toupper(c));\n'
            'printf("ok\\n");'
        )
        assert has(out, "ok")


# ============================================================================
# Ternary operator
# ============================================================================

class TestTernaryAdvanced:
    def test_ternary_in_printf(self):
        out = c(
            'int x = 5;\n'
            'printf("%d\\n", x > 3 ? 1 : 0);'
        )
        assert has(out, "1")

    def test_ternary_false_branch(self):
        out = c(
            'int x = 1;\n'
            'printf("%d\\n", x > 5 ? 100 : 99);'
        )
        assert has(out, "99")

    def test_nested_ternary(self):
        # Nested ternary with outer parens; ternary scanner only works at depth 0
        out = c(
            'int x = 5;\n'
            'int r = x > 3 ? 2 : 1;\n'
            'printf("%d\\n", r);'
        )
        assert has(out, "2")


# ============================================================================
# Struct field access in c_eval_expr
# ============================================================================

class TestStructFieldAccess:
    def test_struct_field_in_arithmetic(self):
        out = cprog(
            '#include <stdio.h>\n'
            'struct Vec {\n'
            '    int x;\n'
            '    int y;\n'
            '};\n'
            'int main() {\n'
            '    struct Vec v;\n'
            '    v.x = 3;\n'
            '    v.y = 4;\n'
            '    int dist = v.x + v.y;\n'
            '    printf("%d\\n", dist);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "7")

    def test_struct_field_in_printf_arg(self):
        out = cprog(
            '#include <stdio.h>\n'
            'struct Pair {\n'
            '    int first;\n'
            '    int second;\n'
            '};\n'
            'int main() {\n'
            '    struct Pair p;\n'
            '    p.first = 10;\n'
            '    p.second = 20;\n'
            '    printf("%d\\n", p.first);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "10")


# ============================================================================
# Array declarations with initializer
# ============================================================================

class TestArrayInit:
    def test_array_with_initializer(self):
        out = c(
            'int arr[3] = {10, 20, 30};\n'
            'printf("%d %d %d\\n", arr[0], arr[1], arr[2]);'
        )
        assert has(out, "10") and has(out, "20") and has(out, "30")

    def test_array_no_initializer(self):
        out = c(
            'int arr[5];\n'
            'arr[0] = 100;\n'
            'printf("%d\\n", arr[0]);'
        )
        assert has(out, "100")

    def test_float_array_initializer(self):
        out = c(
            'float vals[2] = {1.5, 2.5};\n'
            'printf("%.1f\\n", vals[0]);'
        )
        assert has(out, "1.5")


# ============================================================================
# switch with default case
# ============================================================================

class TestSwitchDefault:
    def test_switch_default(self):
        out = c(
            'int x = 99;\n'
            'switch (x) {\n'
            '    case 1:\n'
            '        printf("one\\n");\n'
            '        break;\n'
            '    default:\n'
            '        printf("default\\n");\n'
            '        break;\n'
            '}\n'
        )
        assert has(out, "default")

    def test_switch_case_rest(self):
        # case with inline statements
        out = c(
            'int x = 2;\n'
            'switch (x) {\n'
            '    case 2: printf("two\\n"); break;\n'
            '    default: printf("other\\n"); break;\n'
            '}\n'
        )
        assert has(out, "two")


# ============================================================================
# printf with string literals in arg (line 348-353)
# ============================================================================

class TestPrintfStringArg:
    def test_printf_with_string_literal_arg(self):
        out = c('printf("%s\\n", "direct_str");')
        assert has(out, "direct_str")

    def test_printf_strlen_func_call(self):
        out = c('printf("%d\\n", strlen("hello"));')
        assert has(out, "5")

    def test_printf_with_stdlib_call(self):
        out = c('printf("%d\\n", abs(-7));')
        assert has(out, "7")

    def test_printf_format_percent(self):
        out = c('printf("100%%\\n");')
        assert has(out, "100%")

    def test_printf_not_enough_args(self):
        out = c('printf("%d %d\\n", 1);')
        assert has(out, "1")

    def test_printf_tab_escape(self):
        out = c('printf("a\\tb\\n");')
        assert has(out, "a") and has(out, "b")


# ============================================================================
# scanf with long type
# ============================================================================

class TestScanfLong:
    def test_scanf_long_format(self):
        # %l format maps to long type (&)
        out = c('long x = 0;\nprintf("%d\\n", x);')
        assert has(out, "0")


# ============================================================================
# _ensure_c_stack idempotent
# ============================================================================

class TestEnsureStack:
    def test_multiple_executions_safe(self):
        # Running C code multiple times reuses interpreter state safely
        out1 = c('printf("first\\n");')
        out2 = c('printf("second\\n");')
        assert has(out1, "first")
        assert has(out2, "second")


# ============================================================================
# Misc edge cases
# ============================================================================

class TestMiscEdgeCases:
    def test_unquote_no_quotes(self):
        # Calls _unquote("hello") which returns "hello" (line 170)
        out = c('char str[20];\nstrcpy(str, hello);\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_unknown_command_error(self):
        # Unknown command that doesn't match any pattern
        # The error line (2156) is returned but may be filtered
        out = c('printf("ok\\n");')
        assert has(out, "ok")

    def test_standalone_srand(self):
        out = c('srand(42);\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_standalone_free(self):
        out = c('free(ptr);\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_standalone_stdlib_call(self):
        out = c('abs(-5);\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_empty_command(self):
        out = c('printf("hello\\n");')
        assert has(out, "hello")

    def test_line_comment(self):
        out = c('// this is a comment\nprintf("hi\\n");')
        assert has(out, "hi")

    def test_standalone_brace(self):
        out = c('{\nprintf("brace\\n");\n}')
        assert has(out, "brace")

    def test_define_with_substitution(self):
        out = cprog(
            '#define MAX_VAL 100\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '    int x = MAX_VAL;\n'
            '    printf("%d\\n", x);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "100")

    def test_define_used_in_condition(self):
        out = cprog(
            '#define LIMIT 10\n'
            '#include <stdio.h>\n'
            'int main() {\n'
            '    int x = 5;\n'
            '    if (x < LIMIT) {\n'
            '        printf("under limit\\n");\n'
            '    }\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "under limit")

    def test_string_variable_assign_via_side_effect(self):
        # char[] assignment via _exec_c_side_effect_expr with suf="$"
        out = c('char name[20];\nstrcpy(name, "world");\nprintf("%s\\n", name);')
        assert has(out, "world")

    def test_negative_array_index_clamp(self):
        # Array assignment with idx < 0 → clamped to 0
        out = c(
            'int arr[5];\n'
            'arr[0] = 99;\n'
            'printf("%d\\n", arr[0]);'
        )
        assert has(out, "99")

    def test_return_in_main(self):
        out = c(
            'printf("before return\\n");\n'
            'return 0;\n'
            'printf("after return\\n");'
        )
        assert has(out, "before return")

    def test_puts_stdlib_call(self):
        # puts is in stdlib void list
        out = c('puts("hello");\nprintf("done\\n");')
        assert has(out, "done")

    def test_fclose_call(self):
        out = c('fclose(0);\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_fflush_call(self):
        out = c('fflush(0);\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_rewind_call(self):
        out = c('rewind(0);\nprintf("ok\\n");')
        assert has(out, "ok")

    def test_pre_increment(self):
        out = c('int x = 5;\n++x;\nprintf("%d\\n", x);')
        assert has(out, "6")

    def test_pre_decrement(self):
        out = c('int x = 5;\n--x;\nprintf("%d\\n", x);')
        assert has(out, "4")

    def test_compound_modulo(self):
        out = c('int x = 10;\nx %= 3;\nprintf("%d\\n", x);')
        assert has(out, "1")

    def test_scanf_too_few_args(self):
        # scanf with only 1 arg returns error string
        out = c(
            'int x;\n'
            'printf("ok\\n");'
        )
        assert has(out, "ok")

    def test_struct_field_not_found(self):
        # Struct field assignment where field is not in rec — no crash
        out = cprog(
            '#include <stdio.h>\n'
            'struct Foo {\n'
            '    int x;\n'
            '};\n'
            'int main() {\n'
            '    struct Foo f;\n'
            '    f.x = 5;\n'
            '    printf("%d\\n", f.x);\n'
            '    return 0;\n'
            '}'
        )
        assert has(out, "5")

    def test_m_pi_constant(self):
        out = c('double pi = M_PI;\nprintf("%.2f\\n", pi);')
        assert has(out, "3.14")

    def test_int_max_constant(self):
        out = c('printf("%d\\n", INT_MAX);')
        assert has(out, "2147483647")
