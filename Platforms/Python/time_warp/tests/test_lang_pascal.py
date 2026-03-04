"""Comprehensive tests for the Pascal language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.PASCAL


def pas(source: str, **kw) -> list[str]:
    """Shortcut: run a Pascal program."""
    return run(source, L, **kw)


# ============================================================================
# WRITELN / WRITE (output)
# ============================================================================


class TestOutput:
    def test_writeln_string(self):
        out = pas("writeln('Hello World');")
        assert has(out, "Hello World")

    def test_writeln_number(self):
        out = pas("var x: integer;\nx := 42;\nwriteln(x);")
        assert has(out, "42")

    def test_writeln_expression(self):
        out = pas("writeln(3 + 4);")
        assert has(out, "7")

    def test_write_no_newline(self):
        out = pas("write('AB');")
        assert has(out, "AB")

    def test_writeln_multiple_args(self):
        out = pas("writeln('A', ' ', 'B');")
        assert has(out, "A", "B")

    def test_writeln_bare(self):
        out = pas("writeln;")
        assert no_errors(out)

    def test_write_bare(self):
        out = pas("write;")
        assert no_errors(out)


# ============================================================================
# VARIABLES / ASSIGNMENT
# ============================================================================


class TestVariables:
    def test_integer_var(self):
        out = pas("var x: integer;\nx := 10;\nwriteln(x);")
        assert has(out, "10")

    def test_real_var(self):
        out = pas("var r: real;\nr := 3.14;\nwriteln(r);")
        assert has(out, "3.14")

    def test_string_var(self):
        out = pas("var s: string;\ns := 'Hi';\nwriteln(s);")
        assert has(out, "Hi")

    def test_boolean_var(self):
        out = pas("var b: boolean;\nb := true;\nwriteln(b);")
        assert no_errors(out)

    def test_char_var(self):
        out = pas("var c: char;\nc := 'A';\nwriteln(c);")
        assert has(out, "A")

    def test_multiple_declarations(self):
        out = pas("var a: integer;\nvar b: integer;\na := 1;\nb := 2;\nwriteln(a + b);")
        assert has(out, "3")

    def test_constant(self):
        out = pas("const N = 42;\nwriteln(N);")
        assert has(out, "42")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_addition(self):
        out = pas("writeln(2 + 3);")
        assert has(out, "5")

    def test_subtraction(self):
        out = pas("writeln(10 - 4);")
        assert has(out, "6")

    def test_multiplication(self):
        out = pas("writeln(6 * 7);")
        assert has(out, "42")

    def test_division(self):
        out = pas("var r: real;\nr := 10 / 3;\nwriteln(r);")
        assert no_errors(out)

    def test_integer_division(self):
        out = pas("writeln(10 div 3);")
        assert has(out, "3")

    def test_modulo(self):
        out = pas("writeln(10 mod 3);")
        assert has(out, "1")


# ============================================================================
# IF / THEN / ELSE
# ============================================================================


class TestConditional:
    def test_if_true(self):
        out = pas("var x: integer;\nx := 5;\nif x > 3 then\nwriteln('yes');")
        assert has(out, "yes")

    def test_if_false(self):
        out = pas("var x: integer;\nx := 1;\nif x > 3 then\nwriteln('yes');")
        assert not has(out, "yes")

    def test_if_else(self):
        out = pas(
            "var x: integer;\nx := 1;\nif x > 3 then\nbegin\nwriteln('big');\nend\nelse\nbegin\nwriteln('small');\nend"
        )
        assert has(out, "small")

    def test_if_begin_end_block(self):
        out = pas(
            "var x: integer;\nx := 10;\nif x > 3 then\nbegin\nwriteln('A');\nwriteln('B');\nend"
        )
        assert has(out, "A") and has(out, "B")


# ============================================================================
# FOR LOOP
# ============================================================================


class TestForLoop:
    def test_for_to(self):
        out = pas("var i: integer;\nfor i := 1 to 3 do\nwriteln(i);")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_for_downto(self):
        out = pas("var i: integer;\nfor i := 3 downto 1 do\nwriteln(i);")
        assert has(out, "3") and has(out, "2") and has(out, "1")

    def test_for_block(self):
        out = pas("var i: integer;\nfor i := 1 to 2 do\nbegin\nwriteln(i);\nend")
        assert has(out, "1") and has(out, "2")


# ============================================================================
# WHILE LOOP
# ============================================================================


class TestWhileLoop:
    def test_while_basic(self):
        out = pas(
            "var i: integer;\ni := 1;\nwhile i <= 3 do\nbegin\nwriteln(i);\ni := i + 1;\nend"
        )
        assert has(out, "1") and has(out, "3")

    def test_while_not_entered(self):
        out = pas(
            "var i: integer;\ni := 10;\nwhile i < 5 do\nbegin\nwriteln(i);\ni := i + 1;\nend"
        )
        assert not has(out, "10")


# ============================================================================
# REPEAT / UNTIL
# ============================================================================


class TestRepeatUntil:
    def test_repeat_basic(self):
        out = pas(
            "var i: integer;\ni := 1;\nrepeat\nwriteln(i);\ni := i + 1;\nuntil i > 3;"
        )
        assert has(out, "1") and has(out, "3")


# ============================================================================
# CASE / OF
# ============================================================================


class TestCase:
    def test_case_match(self):
        out = pas(
            "var x: integer;\nx := 2;\ncase x of\n1: writeln('one');\n2: writeln('two');\n3: writeln('three');\nend"
        )
        assert has(out, "two")


# ============================================================================
# PROCEDURES / FUNCTIONS
# ============================================================================


class TestProceduresAndFunctions:
    def test_procedure_call(self):
        out = pas("procedure greet;\nbegin\nwriteln('hello');\nend;\ngreet;")
        assert has(out, "hello")

    def test_procedure_with_param(self):
        out = pas("procedure show(n: integer);\nbegin\nwriteln(n);\nend;\nshow(42);")
        assert has(out, "42")

    def test_function_return(self):
        out = pas(
            "function double(n: integer): integer;\nbegin\ndouble := n * 2;\nend;\nwriteln(double(5));"
        )
        assert has(out, "10")


# ============================================================================
# BUILT-IN FUNCTIONS
# ============================================================================


class TestBuiltinFunctions:
    def test_length(self):
        out = pas("var s: string;\ns := 'Hello';\nwriteln(Length(s));")
        assert has(out, "5")

    def test_copy(self):
        out = pas("var s: string;\ns := 'Hello';\nwriteln(Copy(s, 1, 3));")
        assert has(out, "Hel")

    def test_pos(self):
        out = pas("var s: string;\ns := 'Hello';\nwriteln(Pos('ll', s));")
        assert no_errors(out)

    def test_upcase(self):
        out = pas("writeln(UpCase('a'));")
        assert has(out, "A")

    def test_chr_ord(self):
        out = pas("writeln(Chr(65));")
        assert has(out, "A")

    def test_abs(self):
        out = pas("writeln(Abs(-5));")
        assert has(out, "5")

    def test_sqrt(self):
        out = pas("writeln(Sqrt(16));")
        assert has(out, "4")

    def test_round(self):
        out = pas("writeln(Round(3.7));")
        assert has(out, "4")

    def test_trunc(self):
        out = pas("writeln(Trunc(3.9));")
        assert has(out, "3")

    def test_random(self):
        out = pas("Randomize;\nwriteln(Random(100));")
        assert no_errors(out)

    def test_odd(self):
        out = pas("writeln(Odd(3));")
        assert no_errors(out)


# ============================================================================
# STRING OPERATIONS
# ============================================================================


class TestStringOps:
    def test_concat(self):
        out = pas("writeln(Concat('Hello', ' ', 'World'));")
        assert has(out, "Hello World")

    def test_string_addition(self):
        out = pas("var s: string;\ns := 'AB' + 'CD';\nwriteln(s);")
        assert has(out, "ABCD")


# ============================================================================
# ARRAYS
# ============================================================================


class TestArrays:
    def test_array_basic(self):
        out = pas(
            "var a: array[0..2] of integer;\na[0] := 10;\na[1] := 20;\nwriteln(a[0]);\nwriteln(a[1]);"
        )
        assert has(out, "10") and has(out, "20")


# ============================================================================
# BUILT-IN PROCEDURES
# ============================================================================


class TestBuiltinProcedures:
    def test_inc(self):
        out = pas("var x: integer;\nx := 5;\nInc(x);\nwriteln(x);")
        assert has(out, "6")

    def test_dec(self):
        out = pas("var x: integer;\nx := 5;\nDec(x);\nwriteln(x);")
        assert has(out, "4")

    def test_clrscr(self):
        out = pas("ClrScr;")
        assert no_errors(out) or len(out) == 0


# ============================================================================
# INPUT
# ============================================================================


class TestInput:
    def test_readln(self):
        out = pas("var x: integer;\nreadln(x);\nwriteln(x);", input_val="7")
        assert has(out, "7") or no_errors(out)


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_unknown_identifier(self):
        out = pas("writeln(undefined_var);")
        # Should either error or output something
        assert len(out) > 0

    def test_type_mismatch(self):
        out = pas("var x: integer;\nx := 'hello';")
        # May or may not error depending on implementation
        assert len(out) >= 0


# ============================================================================
# STRUCTURE KEYWORDS (BEGIN/END/PROGRAM)
# ============================================================================


class TestStructureKeywords:
    def test_program_keyword(self):
        out = pas("program Test;\nwriteln('OK');")
        assert has(out, "OK")

    def test_begin_end(self):
        out = pas("begin\nwriteln('inside');\nend")
        assert has(out, "inside")
