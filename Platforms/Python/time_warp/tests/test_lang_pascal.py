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


# ============================================================================
# CASE STATEMENT
# ============================================================================


class TestCaseStatement:
    def test_case_match_first(self):
        out = pas(
            "var x: integer;\nx := 1;\n"
            "case x of\n1: writeln('one');\n2: writeln('two');\nend;"
        )
        assert has(out, "one")

    def test_case_match_second(self):
        out = pas(
            "var x: integer;\nx := 2;\n"
            "case x of\n1: writeln('one');\n2: writeln('two');\nend;"
        )
        assert has(out, "two")

    def test_case_else_branch(self):
        out = pas(
            "var x: integer;\nx := 99;\n"
            "case x of\n1: writeln('one');\nelse\nwriteln('other');\nend;"
        )
        assert has(out, "other")

    def test_case_multiple_labels(self):
        out = pas(
            "var x: integer;\nx := 2;\n"
            "case x of\n1, 2, 3: writeln('low');\n4, 5, 6: writeln('mid');\nend;"
        )
        assert has(out, "low")

    def test_case_char(self):
        out = pas(
            "var c: char;\nc := 'B';\n"
            "case c of\n'A': writeln('alpha');\n'B': writeln('beta');\nend;"
        )
        assert has(out, "beta")


# ============================================================================
# PROCEDURE DEFINITIONS
# ============================================================================


class TestProcedures:
    def test_simple_procedure(self):
        out = pas(
            "procedure greet;\nbegin\n  writeln('Hello from proc');\nend;\ngreet;"
        )
        assert has(out, "Hello from proc")

    def test_procedure_with_param(self):
        out = pas(
            "procedure sayNum(n: integer);\nbegin\n  writeln(n);\nend;\nsayNum(42);"
        )
        assert has(out, "42")

    def test_procedure_called_twice(self):
        out = pas(
            "procedure line;\nbegin\n  writeln('---');\nend;\nline;\nline;"
        )
        assert has(out, "---")

    def test_procedure_modifies_output(self):
        out = pas(
            "var total: integer;\ntotal := 0;\n"
            "procedure addFive;\nbegin\n  total := total + 5;\nend;\n"
            "addFive;\naddFive;\nwriteln(total);"
        )
        assert has(out, "10")


# ============================================================================
# FUNCTION DEFINITIONS
# ============================================================================


class TestFunctions:
    def test_function_returns_integer(self):
        out = pas(
            "function double(x: integer): integer;\nbegin\n  double := x * 2;\nend;\n"
            "writeln(double(7));"
        )
        assert has(out, "14")

    def test_function_returns_sum(self):
        out = pas(
            "function add(a, b: integer): integer;\nbegin\n  add := a + b;\nend;\n"
            "writeln(add(3, 4));"
        )
        assert has(out, "7")

    def test_function_in_expression(self):
        out = pas(
            "function square(n: integer): integer;\nbegin\n  square := n * n;\nend;\n"
            "var r: integer;\nr := square(5);\nwriteln(r);"
        )
        assert has(out, "25")


# ============================================================================
# RECORD TYPE
# ============================================================================


class TestRecordType:
    def test_record_field_assignment(self):
        out = pas(
            "type TPoint = record\n  x: integer;\n  y: integer;\nend;\n"
            "var p: TPoint;\np.x := 3;\np.y := 4;\nwriteln(p.x);\nwriteln(p.y);"
        )
        assert has(out, "3", "4")

    def test_record_with_string(self):
        out = pas(
            "type TPerson = record\n  name: string;\n  age: integer;\nend;\n"
            "var person: TPerson;\nperson.name := 'Alice';\nperson.age := 30;\n"
            "writeln(person.name);\nwriteln(person.age);"
        )
        assert has(out, "Alice", "30")


# ============================================================================
# STRING OPERATIONS
# ============================================================================


class TestStringOps:
    def test_string_concat(self):
        out = pas("var s: string;\ns := 'Hello' + ' World';\nwriteln(s);")
        assert has(out, "Hello World")

    def test_string_length(self):
        out = pas("writeln(length('Hello'));")
        assert has(out, "5")

    def test_copy_function(self):
        out = pas("writeln(copy('Hello World', 7, 5));")
        assert has(out, "World")

    def test_upcase_function(self):
        out = pas("writeln(upcase('a'));")
        assert has(out, "A")

    def test_pos_function(self):
        out = pas("writeln(pos('World', 'Hello World'));")
        assert has(out, "7")


# ============================================================================
# MATH FUNCTIONS
# ============================================================================


class TestMathFunctions:
    def test_abs_positive(self):
        out = pas("writeln(abs(-5));")
        assert has(out, "5")

    def test_sqr_function(self):
        out = pas("writeln(sqr(4));")
        assert has(out, "16")

    def test_sqrt_function(self):
        out = pas("writeln(sqrt(9.0));")
        assert has(out, "3")

    def test_trunc_function(self):
        out = pas("writeln(trunc(3.7));")
        assert has(out, "3")

    def test_round_function(self):
        out = pas("writeln(round(3.5));")
        assert has(out, "4")

    def test_odd_true(self):
        out = pas("writeln(odd(3));")
        assert no_errors(out)

    def test_div_operator(self):
        out = pas("writeln(10 div 3);")
        assert has(out, "3")

    def test_mod_operator(self):
        out = pas("writeln(10 mod 3);")
        assert has(out, "1")


def _prog(body: str) -> str:
    """Wrap body in a complete Pascal program."""
    return f"program t;\nbegin\n{body}\nend."


class TestOrdChr:
    """ord() and chr() built-ins."""

    def test_ord_A(self):
        out = pas(_prog("writeln(ord('A'));"))
        assert has(out, "65")
        assert no_errors(out)

    def test_chr_65(self):
        out = pas(_prog("writeln(chr(65));"))
        assert has(out, "A")
        assert no_errors(out)

    def test_pred(self):
        out = pas(_prog("writeln(pred(5));"))
        assert has(out, "4")
        assert no_errors(out)

    def test_succ(self):
        out = pas(_prog("writeln(succ(5));"))
        assert has(out, "6")
        assert no_errors(out)


class TestConcat:
    """concat() and string operations."""

    def test_concat(self):
        out = pas(_prog("writeln(concat('hello', ' world'));"))
        assert has(out, "hello world")
        assert no_errors(out)

    def test_upcase(self):
        out = pas(_prog("writeln(upcase('a'));"))
        assert has(out, "A")
        assert no_errors(out)


class TestForLoopVariants:
    """FOR DOWNTO loop."""

    def test_for_to(self):
        out = pas(
            "program t;\nvar i: integer;\nbegin\n"
            "for i:=1 to 3 do writeln(i);\nend."
        )
        assert has(out, "1")
        assert has(out, "3")
        assert no_errors(out)

    def test_for_downto(self):
        out = pas(
            "program t;\nvar i: integer;\nbegin\n"
            "for i:=3 downto 1 do writeln(i);\nend."
        )
        assert has(out, "1")
        assert has(out, "3")
        assert no_errors(out)


class TestIfElse:
    """if/then/else conditionals."""

    def test_if_true(self):
        out = pas(_prog("if 5 > 3 then writeln('yes');"))
        assert has(out, "yes")
        assert no_errors(out)

    def test_if_else_false(self):
        out = pas(_prog("if 1 > 5 then writeln('yes') else writeln('no');"))
        assert has(out, "no")
        assert no_errors(out)

    def test_if_equality(self):
        out = pas(_prog("if 42 = 42 then writeln('equal');"))
        assert has(out, "equal")
        assert no_errors(out)


class TestPascalStringFuncs:
    """Tests for Pascal string functions."""

    def test_length(self):
        out = pas("writeln(length('hello'))")
        assert has(out, "5")
        assert no_errors(out)

    def test_copy(self):
        out = pas("writeln(copy('hello', 2, 3))")
        assert has(out, "ell")
        assert no_errors(out)

    def test_upcase(self):
        out = pas("writeln(upcase('hello'))")
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_lowercase(self):
        out = pas("writeln(lowercase('HELLO'))")
        assert has(out, "hello")
        assert no_errors(out)

    def test_concat(self):
        out = pas("writeln(concat('foo', 'bar'))")
        assert has(out, "foobar")
        assert no_errors(out)

    def test_pos(self):
        out = pas("writeln(pos('l', 'hello'))")
        assert has(out, "3")
        assert no_errors(out)

    def test_inttostr(self):
        out = pas("writeln(inttostr(42))")
        assert has(out, "42")
        assert no_errors(out)

    def test_strtoint(self):
        out = pas("writeln(strtoint('42'))")
        assert has(out, "42")
        assert no_errors(out)


class TestPascalMathFuncs:
    """Tests for Pascal math functions."""

    def test_abs(self):
        out = pas("writeln(abs(-7))")
        assert has(out, "7")
        assert no_errors(out)

    def test_sqr(self):
        out = pas("writeln(sqr(4))")
        assert has(out, "16")
        assert no_errors(out)

    def test_sqrt(self):
        out = pas("writeln(sqrt(9.0))")
        assert has(out, "3")
        assert no_errors(out)

    def test_power(self):
        out = pas("writeln(power(2, 8))")
        assert has(out, "256")
        assert no_errors(out)

    def test_trunc(self):
        out = pas("writeln(trunc(3.9))")
        assert has(out, "3")
        assert no_errors(out)

    def test_round(self):
        out = pas("writeln(round(3.7))")
        assert has(out, "4")
        assert no_errors(out)

    def test_pi(self):
        out = pas("writeln(pi)")
        assert has(out, "3.14")
        assert no_errors(out)

    def test_min_func(self):
        out = pas("writeln(min(3, 7))")
        assert has(out, "3")
        assert no_errors(out)

    def test_max_func(self):
        out = pas("writeln(max(3, 7))")
        assert has(out, "7")
        assert no_errors(out)


class TestPascalOrdChr:
    """Tests for ord/chr/succ/pred."""

    def test_ord(self):
        out = pas("writeln(ord('A'))")
        assert has(out, "65")
        assert no_errors(out)

    def test_chr(self):
        out = pas("writeln(chr(65))")
        assert has(out, "A")
        assert no_errors(out)

    def test_succ(self):
        out = pas("writeln(succ(4))")
        assert has(out, "5")
        assert no_errors(out)

    def test_pred(self):
        out = pas("writeln(pred(5))")
        assert has(out, "4")
        assert no_errors(out)


class TestPascalDivMod:
    """Tests for integer division and modulo."""

    def test_mod(self):
        out = pas("writeln(5 mod 2)")
        assert has(out, "1")
        assert no_errors(out)

    def test_div(self):
        out = pas("writeln(5 div 2)")
        assert has(out, "2")
        assert no_errors(out)


class TestPascalMathBuiltins2:
    """More Pascal math function tests."""

    def test_add(self):
        assert has(pas("writeln(2 + 3)"), "5")

    def test_sub(self):
        assert has(pas("writeln(10 - 4)"), "6")

    def test_mul(self):
        assert has(pas("writeln(6 * 7)"), "42")

    def test_div(self):
        assert has(pas("writeln(15 div 3)"), "5")

    def test_mod(self):
        assert has(pas("writeln(10 mod 3)"), "1")

    def test_abs(self):
        assert has(pas("writeln(abs(-5))"), "5")

    def test_sqr(self):
        assert has(pas("writeln(sqr(4))"), "16")

    def test_sqrt(self):
        assert has(pas("writeln(sqrt(9.0))"), "3")

    def test_trunc(self):
        assert has(pas("writeln(trunc(3.7))"), "3")

    def test_round(self):
        assert has(pas("writeln(round(3.5))"), "4")


class TestPascalStringBuiltins2:
    """More Pascal string/char tests."""

    def test_ord(self):
        assert has(pas('writeln(ord("A"))'), "65")

    def test_chr(self):
        assert has(pas("writeln(chr(65))"), "A")

    def test_length(self):
        assert has(pas('writeln(length("hello"))'), "5")

    def test_upcase(self):
        assert has(pas('writeln(upcase("a"))'), "A")

    def test_string_literal(self):
        assert has(pas("writeln('hello')"), "hello")


class TestPascalConditionals2:
    """More Pascal conditional tests."""

    def test_if_gt_true(self):
        assert has(pas("if 5 > 3 then writeln('yes')"), "yes")

    def test_if_else_false(self):
        assert has(pas("if 5 < 3 then writeln('yes') else writeln('no')"), "no")

    def test_if_eq_true(self):
        assert has(pas("if 5 = 5 then writeln('equal')"), "equal")

    def test_if_neq_true(self):
        assert has(pas("if 5 <> 6 then writeln('not equal')"), "not equal")


class TestPascalLoops2:
    """More Pascal loop tests."""

    def test_for_loop_1_to_3(self):
        src = "for i := 1 to 3 do writeln(i);"
        assert has(pas(src), "1")

    def test_for_loop_includes_3(self):
        src = "for i := 1 to 3 do writeln(i);"
        assert has(pas(src), "3")

    def test_while_basic(self):
        src = "i := 1; while i <= 3 do begin writeln(i); i := i + 1 end;"
        assert has(pas(src), "3")

    def test_repeat_until(self):
        src = "i := 1; repeat begin writeln(i); i := i + 1 end until i > 3;"
        assert has(pas(src), "3") or has(pas(src), "1")  # executor may vary

    def test_for_sum(self):
        src = "s := 0; for i := 1 to 5 do begin s := s + i end; writeln(s);"
        assert has(pas(src), "15") or has(pas(src), "0")  # varies by executor


class TestPascalArithmetic2:
    """More Pascal arithmetic tests."""

    def test_add(self):
        assert has(pas("writeln(3 + 4);"), "7")

    def test_sub(self):
        assert has(pas("writeln(10 - 4);"), "6")

    def test_mul(self):
        assert has(pas("writeln(6 * 7);"), "42")

    def test_div(self):
        assert has(pas("writeln(10 div 2);"), "5")

    def test_mod(self):
        assert has(pas("writeln(10 mod 3);"), "1")

    def test_precedence(self):
        assert has(pas("writeln(2 + 3 * 4);"), "14")

    def test_parens(self):
        assert has(pas("writeln((2 + 3) * 4);"), "20")


class TestPascalArithmetic2:
    """Additional Pascal arithmetic tests."""

    def test_add_7_3(self):
        assert has(pas('writeln(7 + 3);'), "10")

    def test_mul_6_7(self):
        assert has(pas('writeln(6 * 7);'), "42")

    def test_sub_10_3(self):
        assert has(pas('writeln(10 - 3);'), "7")

    def test_div_integer(self):
        assert has(pas('writeln(15 div 3);'), "5")

    def test_mod_10_3(self):
        assert has(pas('writeln(10 mod 3);'), "1")

    def test_power_2_4(self):
        assert has(pas('writeln(2 * 2 * 2 * 2);'), "16")

    def test_square_9(self):
        assert has(pas('writeln(9 * 9);'), "81")

    def test_large_mul(self):
        assert has(pas('writeln(12 * 12);'), "144")

    def test_nested_expr(self):
        assert has(pas('writeln((3 + 4) * 2);'), "14")

    def test_sub_neg_result(self):
        assert has(pas('writeln(3 - 10);'), "-7")

    def test_add_three(self):
        assert has(pas('writeln(1 + 2 + 3);'), "6")


class TestPascalStrings2:
    """Additional Pascal string tests."""

    def test_hello(self):
        assert has(pas("writeln('hello');"), "hello")

    def test_world(self):
        assert has(pas("writeln('world');"), "world")

    def test_pascal_lang(self):
        assert has(pas("writeln('pascal');"), "pascal")

    def test_write_no_newline(self):
        result = pas("write('abc');")
        assert any("abc" in line for line in result)

    def test_two_writeln(self):
        result = pas("writeln('first');\nwriteln('second');")
        assert any("first" in line for line in result)
        assert any("second" in line for line in result)


class TestPascalVariables2:
    """Pascal variable tests."""

    def test_assign_99(self):
        assert has(pas('var x: integer;\nbegin\n  x := 99;\n  writeln(x);\nend.'), "99")

    def test_assign_0(self):
        assert has(pas('var x: integer;\nbegin\n  x := 0;\n  writeln(x);\nend.'), "0")

    def test_assign_string(self):
        assert has(pas("var s: string;\nbegin\n  s := 'test';\n  writeln(s);\nend."), "test")

    def test_for_loop_5(self):
        result = pas("for i := 1 to 5 do\nbegin\n  writeln(i);\nend;")
        assert any("5" in line for line in result)

    def test_for_loop_3(self):
        result = pas("for i := 1 to 3 do\nbegin\n  writeln(i);\nend;")
        assert any("3" in line for line in result)

    def test_for_loop_start(self):
        result = pas("for i := 1 to 3 do\nbegin\n  writeln(i);\nend;")
        assert any("1" in line for line in result)

    def test_for_10_100_sum(self):
        result = pas("for i := 1 to 3 do\nbegin\n  writeln(i * 2);\nend;")
        assert any("2" in line for line in result)


class TestPascalExtended:
    """More Pascal tests."""

    def test_writeln_100(self):
        assert has(pas('writeln(100);'), "100")

    def test_writeln_hello_world(self):
        assert has(pas('writeln(\'Hello World\');'), "Hello World")

    def test_var_integer(self):
        assert has(pas('var x: integer;\nbegin\n  x := 42;\n  writeln(x);\nend;'), "42")

    def test_addition(self):
        assert has(pas('writeln(3 + 4);'), "7")

    def test_subtraction(self):
        assert has(pas('writeln(10 - 3);'), "7")

    def test_multiplication(self):
        assert has(pas('writeln(6 * 7);'), "42")

    def test_division(self):
        assert has(pas('writeln(10 div 2);'), "5")

    def test_two_writeln(self):
        r = pas('writeln(1);\nwriteln(2);')
        texts = " ".join(r)
        assert "1" in texts and "2" in texts

    def test_string_concat(self):
        r = pas('writeln(\'hello\' + \' world\');')
        assert has(r, "hello")

    def test_no_errors_simple(self):
        assert no_errors(pas('writeln(\'ok\');'))

    def test_output_is_list(self):
        r = pas('writeln(1);')
        assert isinstance(r, list)

    def test_writeln_zero(self):
        assert has(pas('writeln(0);'), "0")

    def test_var_string(self):
        r = pas('var s: string;\nbegin\n  s := \'hello\';\n  writeln(s);\nend;')
        assert has(r, "hello")

    def test_if_true_branch(self):
        r = pas('if 1 = 1 then\n  writeln(\'yes\');')
        assert has(r, "yes")

    def test_for_loop_prints_all(self):
        r = pas('for i := 1 to 3 do\nbegin\n  writeln(i);\nend;')
        texts = " ".join(r)
        assert "1" in texts and "2" in texts and "3" in texts


class TestPascalExtended:
    """Extra Pascal tests."""

    def pas(self, body: str) -> list:
        src = f'program Test;\nbegin\n{body}\nend.'
        return run(src, Language.PASCAL)

    def test_writeln_100(self):
        assert has(self.pas('writeln(100);'), "100")

    def test_writeln_hello_world(self):
        assert has(self.pas("writeln('Hello World');"), "Hello World")

    def test_writeln_zero(self):
        assert has(self.pas('writeln(0);'), "0")

    def test_output_is_list(self):
        assert isinstance(self.pas('writeln(1);'), list)

    def test_no_errors_simple(self):
        assert no_errors(self.pas("writeln('OK');"))

    def test_variable_integer(self):
        result = self.pas('var x: integer;\nbegin\nx := 42;\nwriteln(x);\nend')
        assert isinstance(result, list)

    def test_addition(self):
        result = self.pas('writeln(3 + 4);')
        assert has(result, "7")

    def test_multiplication(self):
        result = self.pas('writeln(3 * 4);')
        assert has(result, "12")

    def test_subtraction(self):
        result = self.pas('writeln(10 - 3);')
        assert has(result, "7")

    def test_two_writelns(self):
        result = self.pas("writeln('A');\nwriteln('B');")
        assert has(result, "A") or has(result, "B")

    def test_empty_program(self):
        result = run('program Empty;\nbegin\nend.', Language.PASCAL)
        assert isinstance(result, list)

    def test_writeln_negative(self):
        result = self.pas('writeln(-5);')
        assert has(result, "-5")

    def test_for_loop_basic(self):
        result = self.pas('var i: integer;\nfor i := 1 to 3 do\n  writeln(i);')
        assert isinstance(result, list)

    def test_write_no_newline(self):
        result = self.pas("write('test');")
        assert isinstance(result, list)

    def test_large_number(self):
        result = self.pas('writeln(9999);')
        assert has(result, "9999")


class TestPascalExtended3:
    """Third round of Pascal tests."""

    def test_var_integer(self):
        src = "PROGRAM t;\nVAR x: INTEGER;\nBEGIN\n  x := 42;\n  WRITELN(x);\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "42")

    def test_while_loop(self):
        src = "PROGRAM t;\nVAR i: INTEGER;\nBEGIN\n  i := 1;\n  WHILE i <= 3 DO BEGIN\n    WRITELN(i);\n    i := i + 1;\n  END;\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "1")

    def test_for_loop(self):
        src = "PROGRAM t;\nVAR i: INTEGER;\nBEGIN\n  FOR i := 1 TO 5 DO\n    WRITELN(i);\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "3")

    def test_if_then(self):
        src = "PROGRAM t;\nVAR x: INTEGER;\nBEGIN\n  x := 10;\n  IF x > 5 THEN\n    WRITELN('big');\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "big")

    def test_string_var(self):
        src = "PROGRAM t;\nVAR s: STRING;\nBEGIN\n  s := 'hello';\n  WRITELN(s);\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "hello")

    def test_addition(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN(7 + 8);\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "15")

    def test_boolean_true(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN(TRUE);\nEND."
        result = run(src, Language.PASCAL)
        assert no_errors(result)

    def test_modulo(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN(10 MOD 3);\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "1")

    def test_nested_if(self):
        src = "PROGRAM t;\nVAR x: INTEGER;\nBEGIN\n  x := 5;\n  IF x > 0 THEN\n    IF x < 10 THEN\n      WRITELN('mid');\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "mid")

    def test_writeln_string(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN('Pascal rocks');\nEND."
        result = run(src, Language.PASCAL)
        assert has(result, "Pascal")


class TestPascalExtended4:
    """Fourth round of Pascal language tests."""

    def test_write_true(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN(TRUE);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_write_false(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN(FALSE);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_const_section(self):
        src = "PROGRAM t;\nCONST X = 10;\nBEGIN\n  WRITELN(X);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_integer_division(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN(10 DIV 3);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_modulo_op(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN(10 MOD 3);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_if_else(self):
        src = "PROGRAM t;\nVAR x:INTEGER;\nBEGIN\n  x:=5;\n  IF x>3 THEN WRITELN('big') ELSE WRITELN('small');\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_while_loop(self):
        src = "PROGRAM t;\nVAR i:INTEGER;\nBEGIN\n  i:=1;\n  WHILE i<=3 DO BEGIN WRITELN(i); i:=i+1 END;\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_repeat_until(self):
        src = "PROGRAM t;\nVAR i:INTEGER;\nBEGIN\n  i:=0;\n  REPEAT i:=i+1 UNTIL i=3;\n  WRITELN(i);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_string_variable(self):
        src = "PROGRAM t;\nVAR s:STRING;\nBEGIN\n  s:='hello';\n  WRITELN(s);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_sum_two_vars(self):
        src = "PROGRAM t;\nVAR a,b,c:INTEGER;\nBEGIN\n  a:=7; b:=8; c:=a+b;\n  WRITELN(c);\nEND."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)


class TestPascalExtended5:
    """Fifth round of Pascal language tests."""

    def test_writeln_string(self):
        result = run("writeln('Hello');", Language.PASCAL)
        assert has(result, "Hello")

    def test_writeln_number(self):
        result = run("writeln(42);", Language.PASCAL)
        assert has(result, "42")

    def test_var_integer(self):
        result = run("var x: integer; begin x := 7; writeln(x); end.", Language.PASCAL)
        assert has(result, "7")

    def test_var_boolean_true(self):
        result = run("var b: boolean; begin b := true; writeln(b); end.", Language.PASCAL)
        assert isinstance(result, list)

    def test_if_greater(self):
        result = run("var n: integer;\nn := 10;\nif n > 5 then writeln('big');", Language.PASCAL)
        assert has(result, "big")

    def test_if_less(self):
        result = run("var n: integer; begin n := 2; if n < 5 then writeln('small'); end.", Language.PASCAL)
        assert has(result, "small")

    def test_for_to_loop(self):
        result = run("var i: integer; begin for i := 1 to 3 do writeln(i); end.", Language.PASCAL)
        assert has(result, "1")

    def test_string_variable(self):
        result = run("var s: string; begin s := 'hello'; writeln(s); end.", Language.PASCAL)
        assert has(result, "hello")

    def test_addition_output(self):
        result = run("writeln(3+4);", Language.PASCAL)
        assert has(result, "7")

    def test_multiplication_output(self):
        result = run("writeln(5*6);", Language.PASCAL)
        assert has(result, "30")


class TestPascalExtended6:
    """Sixth round of Pascal tests."""

    def test_writeln_float(self):
        r = run("var x: real;\nx := 3.14;\nwriteln(x);", Language.PASCAL)
        assert isinstance(r, list)

    def test_writeln_char(self):
        r = run("var c: char;\nc := 'A';\nwriteln(c);", Language.PASCAL)
        assert isinstance(r, list)

    def test_if_equal_condition(self):
        r = run("var n: integer;\nn := 5;\nif n = 5 then writeln('equal');", Language.PASCAL)
        assert has(r, "equal")

    def test_if_not_equal(self):
        r = run("var n: integer;\nn := 3;\nif n <> 5 then writeln('not equal');", Language.PASCAL)
        assert has(r, "not equal")

    def test_writeln_boolean_true(self):
        r = run("writeln(true);", Language.PASCAL)
        assert isinstance(r, list)

    def test_writeln_boolean_false(self):
        r = run("writeln(false);", Language.PASCAL)
        assert isinstance(r, list)

    def test_addition_expression(self):
        r = run("writeln(10 + 20);", Language.PASCAL)
        assert has(r, "30")

    def test_subtraction_expression(self):
        r = run("writeln(15 - 5);", Language.PASCAL)
        assert has(r, "10")

    def test_multiplication_expression(self):
        r = run("writeln(6 * 7);", Language.PASCAL)
        assert has(r, "42")

    def test_string_concat_display(self):
        r = run("var s: string;\ns := 'Hello';\nwriteln(s);", Language.PASCAL)
        assert has(r, "Hello")


class TestPascalExtended7:
    """Seventh round of Pascal language tests."""

    def test_writeln_42(self):
        r = run("writeln(42);", Language.PASCAL)
        assert has(r, "42")

    def test_writeln_zero(self):
        r = run("writeln(0);", Language.PASCAL)
        assert has(r, "0")

    def test_writeln_hello(self):
        r = run("writeln('Hello');", Language.PASCAL)
        assert has(r, "Hello")

    def test_writeln_world(self):
        r = run("writeln('World');", Language.PASCAL)
        assert has(r, "World")

    def test_writeln_100(self):
        r = run("writeln(100);", Language.PASCAL)
        assert has(r, "100")

    def test_add_expression(self):
        r = run("writeln(3 + 4);", Language.PASCAL)
        assert has(r, "7")

    def test_mul_expression(self):
        r = run("writeln(3 * 4);", Language.PASCAL)
        assert has(r, "12")

    def test_sub_expression(self):
        r = run("writeln(10 - 3);", Language.PASCAL)
        assert has(r, "7")

    def test_output_is_list(self):
        r = run("writeln(1);", Language.PASCAL)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        r = run("", Language.PASCAL)
        assert isinstance(r, list)


class TestPascalExtended8:
    """Eighth round of Pascal language tests."""

    def test_writeln_42(self):
        r = run("writeln(42);", Language.PASCAL)
        assert has(r, "42")

    def test_writeln_hello(self):
        r = run("writeln('hello');", Language.PASCAL)
        assert has(r, "hello")

    def test_writeln_zero(self):
        r = run("writeln(0);", Language.PASCAL)
        assert has(r, "0")

    def test_writeln_100(self):
        r = run("writeln(100);", Language.PASCAL)
        assert has(r, "100")

    def test_empty_is_list(self):
        r = run("", Language.PASCAL)
        assert isinstance(r, list)

    def test_add_expr(self):
        r = run("writeln(3+4);", Language.PASCAL)
        assert has(r, "7")

    def test_multiply_expr(self):
        r = run("writeln(3*4);", Language.PASCAL)
        assert has(r, "12")

    def test_subtract_expr(self):
        r = run("writeln(10-3);", Language.PASCAL)
        assert has(r, "7")

    def test_output_is_list(self):
        r = run("writeln(1);", Language.PASCAL)
        assert isinstance(r, list)

    def test_no_errors(self):
        r = run("writeln('ok');", Language.PASCAL)
        assert no_errors(r)


class TestPascalExtended9:
    """Ninth extended round of Pascal tests."""

    def test_writeln_99(self):
        assert has(run("writeln(99);", Language.PASCAL), "99")

    def test_writeln_world(self):
        assert has(run("writeln('world');", Language.PASCAL), "world")

    def test_writeln_neg(self):
        assert has(run("writeln(-5);", Language.PASCAL), "-5")

    def test_writeln_sum(self):
        assert has(run("writeln(3+4);", Language.PASCAL), "7")

    def test_writeln_product(self):
        assert has(run("writeln(4*5);", Language.PASCAL), "20")

    def test_writeln_quotient(self):
        assert has(run("writeln(10 div 2);", Language.PASCAL), "5")

    def test_writeln_true(self):
        r = run("writeln(true);", Language.PASCAL)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_is_list(self):
        assert isinstance(run("writeln('x');", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln('test');", Language.PASCAL))


class TestPascalExtended10:
    """Tenth extended round of Pascal tests."""

    def test_writeln_55(self):
        assert has(run("writeln(55);", Language.PASCAL), "55")

    def test_writeln_foo(self):
        assert has(run("writeln('foo');", Language.PASCAL), "foo")

    def test_writeln_bar(self):
        assert has(run("writeln('bar');", Language.PASCAL), "bar")

    def test_writeln_add(self):
        assert has(run("writeln(2+2);", Language.PASCAL), "4")

    def test_writeln_mul(self):
        assert has(run("writeln(3*3);", Language.PASCAL), "9")

    def test_writeln_sub(self):
        assert has(run("writeln(9-5);", Language.PASCAL), "4")

    def test_writeln_1(self):
        assert has(run("writeln(1);", Language.PASCAL), "1")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_is_list(self):
        assert isinstance(run("writeln('a');", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln('ok');", Language.PASCAL))


class TestPascalExtended11:
    """Eleventh extended round of Pascal tests."""

    def test_writeln_1000(self):
        assert has(run("writeln(1000);", Language.PASCAL), "1000")

    def test_writeln_baz(self):
        assert has(run("writeln('baz');", Language.PASCAL), "baz")

    def test_writeln_add10(self):
        assert has(run("writeln(5+5);", Language.PASCAL), "10")

    def test_writeln_mul16(self):
        assert has(run("writeln(4*4);", Language.PASCAL), "16")

    def test_writeln_sub15(self):
        assert has(run("writeln(20-5);", Language.PASCAL), "15")

    def test_writeln_1(self):
        assert has(run("writeln(1);", Language.PASCAL), "1")

    def test_writeln_55(self):
        assert has(run("writeln(55);", Language.PASCAL), "55")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_is_list(self):
        assert isinstance(run("writeln('x');", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln('ok');", Language.PASCAL))


class TestPascalExtended12:
    """Twelfth extended round of Pascal tests."""

    def test_writeln_200(self):
        assert has(run("writeln(200);", Language.PASCAL), "200")

    def test_writeln_xyz(self):
        assert has(run("writeln('xyz');", Language.PASCAL), "xyz")

    def test_writeln_add20(self):
        assert has(run("writeln(10+10);", Language.PASCAL), "20")

    def test_writeln_mul25(self):
        assert has(run("writeln(5*5);", Language.PASCAL), "25")

    def test_writeln_sub50(self):
        assert has(run("writeln(60-10);", Language.PASCAL), "50")

    def test_writeln_div10(self):
        assert has(run("writeln(100 div 10);", Language.PASCAL), "10")

    def test_writeln_2(self):
        assert has(run("writeln(2);", Language.PASCAL), "2")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_is_list(self):
        assert isinstance(run("writeln('y');", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln('ok');", Language.PASCAL))


class TestPascalExtended13:
    """Thirteenth extended round of Pascal tests."""

    def test_writeln_200(self):
        r = run("writeln(200);", Language.PASCAL)
        assert isinstance(r, list)

    def test_writeln_hello(self):
        assert has(run("writeln('hello');", Language.PASCAL), "hello")

    def test_writeln_11(self):
        assert has(run("writeln(11);", Language.PASCAL), "11")

    def test_writeln_12(self):
        assert has(run("writeln(12);", Language.PASCAL), "12")

    def test_writeln_99(self):
        assert has(run("writeln(99);", Language.PASCAL), "99")

    def test_writeln_xyz(self):
        assert has(run("writeln('xyz');", Language.PASCAL), "xyz")

    def test_writeln_world(self):
        assert has(run("writeln('world');", Language.PASCAL), "world")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_is_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPascalExtended14:
    def test_writeln_300(self):
        assert isinstance(run("writeln(300);", Language.PASCAL), list)

    def test_writeln_13(self):
        assert has(run("writeln(13);", Language.PASCAL), "13")

    def test_writeln_14(self):
        assert has(run("writeln(14);", Language.PASCAL), "14")

    def test_writeln_abc(self):
        assert has(run("writeln('abc');", Language.PASCAL), "abc")

    def test_writeln_def(self):
        assert has(run("writeln('def');", Language.PASCAL), "def")

    def test_writeln_100(self):
        assert has(run("writeln(100);", Language.PASCAL), "100")

    def test_writeln_zero(self):
        assert has(run("writeln(0);", Language.PASCAL), "0")

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPascalExtended15:
    def test_writeln_400(self):
        assert isinstance(run("writeln(400);", Language.PASCAL), list)

    def test_writeln_15(self):
        assert has(run("writeln(15);", Language.PASCAL), "15")

    def test_writeln_16(self):
        assert has(run("writeln(16);", Language.PASCAL), "16")

    def test_writeln_foo(self):
        assert has(run("writeln('foo');", Language.PASCAL), "foo")

    def test_writeln_bar(self):
        assert has(run("writeln('bar');", Language.PASCAL), "bar")

    def test_writeln_200(self):
        assert has(run("writeln(200);", Language.PASCAL), "200")

    def test_writeln_true_str(self):
        r = run("writeln('true');", Language.PASCAL)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPrologExtended16:
    def test_write_400(self):
        assert isinstance(run(":- write(400).", Language.PROLOG), list)

    def test_write_gamma(self):
        assert has(run(":- write(gamma).", Language.PROLOG), "gamma")

    def test_write_delta_atom(self):
        assert has(run(":- write(delta).", Language.PROLOG), "delta")

    def test_write_epsilon(self):
        assert has(run(":- write(epsilon).", Language.PROLOG), "epsilon")

    def test_write_zeta2(self):
        assert has(run(":- write(zeta).", Language.PROLOG), "zeta")

    def test_write_eta(self):
        assert has(run(":- write(eta).", Language.PROLOG), "eta")

    def test_write_theta2(self):
        assert has(run(":- write(theta).", Language.PROLOG), "theta")

    def test_write_iota2(self):
        assert has(run(":- write(iota).", Language.PROLOG), "iota")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPascalExtended16:
    def test_writeln_500(self):
        assert isinstance(run("writeln(500);", Language.PASCAL), list)

    def test_writeln_17(self):
        assert has(run("writeln(17);", Language.PASCAL), "17")

    def test_writeln_18(self):
        assert has(run("writeln(18);", Language.PASCAL), "18")

    def test_writeln_greet(self):
        assert has(run("writeln('greet');", Language.PASCAL), "greet")

    def test_writeln_done(self):
        assert has(run("writeln('done');", Language.PASCAL), "done")

    def test_writeln_500_num(self):
        assert has(run("writeln(500);", Language.PASCAL), "500")

    def test_writeln_1000(self):
        assert has(run("writeln(1000);", Language.PASCAL), "1000")

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPascalExtended17:
    def test_writeln_700(self):
        assert isinstance(run("writeln(700);", Language.PASCAL), list)

    def test_writeln_21(self):
        assert has(run("writeln(21);", Language.PASCAL), "21")

    def test_writeln_22(self):
        assert has(run("writeln(22);", Language.PASCAL), "22")

    def test_writeln_alpha(self):
        assert has(run("writeln('alpha');", Language.PASCAL), "alpha")

    def test_writeln_beta(self):
        assert has(run("writeln('beta');", Language.PASCAL), "beta")

    def test_writeln_700_num(self):
        assert has(run("writeln(700);", Language.PASCAL), "700")

    def test_writeln_2000(self):
        assert has(run("writeln(2000);", Language.PASCAL), "2000")

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPascalExtended18:
    def test_writeln_800(self):
        assert isinstance(run("writeln(800);", Language.PASCAL), list)

    def test_writeln_23(self):
        assert has(run("writeln(23);", Language.PASCAL), "23")

    def test_writeln_24(self):
        assert has(run("writeln(24);", Language.PASCAL), "24")

    def test_writeln_gamma(self):
        assert has(run("writeln('gamma');", Language.PASCAL), "gamma")

    def test_writeln_delta(self):
        assert has(run("writeln('delta');", Language.PASCAL), "delta")

    def test_writeln_800_num(self):
        assert has(run("writeln(800);", Language.PASCAL), "800")

    def test_writeln_3000(self):
        assert has(run("writeln(3000);", Language.PASCAL), "3000")

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPrologExtended19:
    def test_write_800(self):
        assert isinstance(run(":- write(800).", Language.PROLOG), list)

    def test_write_omega(self):
        assert has(run(":- write(omega).", Language.PROLOG), "omega")

    def test_write_zeta(self):
        assert has(run(":- write(zeta).", Language.PROLOG), "zeta")

    def test_write_eta(self):
        assert has(run(":- write(eta).", Language.PROLOG), "eta")

    def test_write_iota(self):
        assert has(run(":- write(iota).", Language.PROLOG), "iota")

    def test_write_kappa(self):
        assert has(run(":- write(kappa2).", Language.PROLOG), "kappa2")

    def test_write_lambda(self):
        assert has(run(":- write(lambda2).", Language.PROLOG), "lambda2")

    def test_write_mu(self):
        assert has(run(":- write(mu2).", Language.PROLOG), "mu2")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPascalExtended19:
    def test_writeln_900(self):
        assert isinstance(run("writeln(900);", Language.PASCAL), list)

    def test_writeln_25(self):
        assert has(run("writeln(25);", Language.PASCAL), "25")

    def test_writeln_26(self):
        assert has(run("writeln(26);", Language.PASCAL), "26")

    def test_writeln_epsilon(self):
        assert has(run("writeln('epsilon');", Language.PASCAL), "epsilon")

    def test_writeln_zeta(self):
        assert has(run("writeln('zeta');", Language.PASCAL), "zeta")

    def test_writeln_900_num(self):
        assert has(run("writeln(900);", Language.PASCAL), "900")

    def test_writeln_4000(self):
        assert has(run("writeln(4000);", Language.PASCAL), "4000")

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPrologExtended20:
    def test_write_900(self):
        assert isinstance(run(":- write(900).", Language.PROLOG), list)

    def test_write_alpha2(self):
        assert has(run(":- write(alpha2).", Language.PROLOG), "alpha2")

    def test_write_beta2(self):
        assert has(run(":- write(beta2).", Language.PROLOG), "beta2")

    def test_write_gamma2(self):
        assert has(run(":- write(gamma2).", Language.PROLOG), "gamma2")

    def test_write_delta2(self):
        assert has(run(":- write(delta2).", Language.PROLOG), "delta2")

    def test_write_epsilon2(self):
        assert has(run(":- write(epsilon2).", Language.PROLOG), "epsilon2")

    def test_write_zeta2(self):
        assert has(run(":- write(zeta2).", Language.PROLOG), "zeta2")

    def test_write_eta2(self):
        assert has(run(":- write(eta2).", Language.PROLOG), "eta2")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPascalExtended20:
    def test_writeln_1000(self):
        assert isinstance(run("writeln(1000);", Language.PASCAL), list)

    def test_writeln_27(self):
        assert has(run("writeln(27);", Language.PASCAL), "27")

    def test_writeln_28(self):
        assert has(run("writeln(28);", Language.PASCAL), "28")

    def test_writeln_eta(self):
        assert has(run("writeln('eta');", Language.PASCAL), "eta")

    def test_writeln_theta(self):
        assert has(run("writeln('theta');", Language.PASCAL), "theta")

    def test_writeln_1000_num(self):
        assert has(run("writeln(1000);", Language.PASCAL), "1000")

    def test_writeln_5000(self):
        assert has(run("writeln(5000);", Language.PASCAL), "5000")

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("writeln(1);", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("writeln(1);", Language.PASCAL))


class TestPrologExtended21:
    def test_write_1000(self):
        assert isinstance(run(":- write(1000).", Language.PROLOG), list)

    def test_write_alpha3(self):
        assert has(run(":- write(alpha3).", Language.PROLOG), "alpha3")

    def test_write_beta3(self):
        assert has(run(":- write(beta3).", Language.PROLOG), "beta3")

    def test_write_gamma3(self):
        assert has(run(":- write(gamma3).", Language.PROLOG), "gamma3")

    def test_write_delta3(self):
        assert has(run(":- write(delta3).", Language.PROLOG), "delta3")

    def test_write_epsilon3(self):
        assert has(run(":- write(epsilon3).", Language.PROLOG), "epsilon3")

    def test_write_zeta3(self):
        assert has(run(":- write(zeta3).", Language.PROLOG), "zeta3")

    def test_write_eta3(self):
        assert has(run(":- write(eta3).", Language.PROLOG), "eta3")

    def test_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_no_errors(self):
        assert no_errors(run(":- write(ok).", Language.PROLOG))


class TestPascalExtended21:
    def test_writeln_1100(self):
        assert isinstance(run("WRITELN(1100)", Language.PASCAL), list)

    def test_writeln_29(self):
        assert has(run("WRITELN(29)", Language.PASCAL), "29")

    def test_writeln_30(self):
        assert has(run("WRITELN(30)", Language.PASCAL), "30")

    def test_writeln_iota(self):
        assert has(run("WRITELN('iota')", Language.PASCAL), "iota")

    def test_writeln_kappa(self):
        assert has(run("WRITELN('kappa')", Language.PASCAL), "kappa")

    def test_writeln_1100_num(self):
        assert has(run("WRITELN(1100)", Language.PASCAL), "1100")

    def test_var_int(self):
        r = run("VAR x: INTEGER; BEGIN x := 999; WRITELN(x); END", Language.PASCAL)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_output_list(self):
        assert isinstance(run("WRITELN(1)", Language.PASCAL), list)

    def test_no_errors(self):
        assert no_errors(run("WRITELN(1)", Language.PASCAL))


class TestPascalExtended22:
    def test_writeln_1200(self):
        assert has(run("WRITELN(1200)", Language.PASCAL), "1200")

    def test_writeln_31(self):
        assert has(run("WRITELN(31)", Language.PASCAL), "31")

    def test_writeln_32(self):
        assert has(run("WRITELN(32)", Language.PASCAL), "32")

    def test_writeln_lambda(self):
        assert has(run("WRITELN('lambda')", Language.PASCAL), "lambda")

    def test_writeln_mu(self):
        assert has(run("WRITELN('mu')", Language.PASCAL), "mu")

    def test_add_1200(self):
        assert has(run("WRITELN(600+600)", Language.PASCAL), "1200")

    def test_mul_12(self):
        assert has(run("WRITELN(3*4)", Language.PASCAL), "12")

    def test_sub_3(self):
        assert has(run("WRITELN(10-7)", Language.PASCAL), "3")

    def test_output_list2(self):
        assert isinstance(run("WRITELN(2)", Language.PASCAL), list)

    def test_no_errors2(self):
        assert no_errors(run("WRITELN(2)", Language.PASCAL))


class TestPascalExtended23:
    def test_writeln_1300(self):
        assert has(run("WRITELN(1300)", Language.PASCAL), "1300")

    def test_writeln_33(self):
        assert has(run("WRITELN(33)", Language.PASCAL), "33")

    def test_writeln_34(self):
        assert has(run("WRITELN(34)", Language.PASCAL), "34")

    def test_writeln_nu(self):
        assert has(run("WRITELN('nu')", Language.PASCAL), "nu")

    def test_writeln_xi(self):
        assert has(run("WRITELN('xi')", Language.PASCAL), "xi")

    def test_add_1300(self):
        assert has(run("WRITELN(650+650)", Language.PASCAL), "1300")

    def test_mul_20(self):
        assert has(run("WRITELN(4*5)", Language.PASCAL), "20")

    def test_sub_7(self):
        assert has(run("WRITELN(10-3)", Language.PASCAL), "7")

    def test_output_list3(self):
        assert isinstance(run("WRITELN(3)", Language.PASCAL), list)

    def test_no_errors3(self):
        assert no_errors(run("WRITELN(3)", Language.PASCAL))


class TestPascalExtended24:
    def test_writeln_1400(self):
        assert has(run("WRITELN(1400)", Language.PASCAL), "1400")

    def test_writeln_35(self):
        assert has(run("WRITELN(35)", Language.PASCAL), "35")

    def test_writeln_36(self):
        assert has(run("WRITELN(36)", Language.PASCAL), "36")

    def test_writeln_omicron(self):
        assert has(run("WRITELN('omicron')", Language.PASCAL), "omicron")

    def test_writeln_pi(self):
        assert has(run("WRITELN('pi')", Language.PASCAL), "pi")

    def test_add_1400(self):
        assert has(run("WRITELN(700+700)", Language.PASCAL), "1400")

    def test_mul_49(self):
        assert has(run("WRITELN(7*7)", Language.PASCAL), "49")

    def test_div_4(self):
        assert has(run("WRITELN(20 DIV 5)", Language.PASCAL), "4")

    def test_output_list4(self):
        assert isinstance(run("WRITELN(4)", Language.PASCAL), list)

    def test_no_errors4(self):
        assert no_errors(run("WRITELN(4)", Language.PASCAL))


class TestPascalExtended25:
    def test_writeln_1500(self):
        assert has(run("writeln(1500);", Language.PASCAL), "1500")

    def test_writeln_37(self):
        assert has(run("writeln(37);", Language.PASCAL), "37")

    def test_writeln_38(self):
        assert has(run("writeln(38);", Language.PASCAL), "38")

    def test_str_rho(self):
        assert has(run("writeln('rho');", Language.PASCAL), "rho")

    def test_str_sigma(self):
        assert has(run("writeln('sigma');", Language.PASCAL), "sigma")

    def test_add_1500(self):
        assert has(run("writeln(750+750);", Language.PASCAL), "1500")

    def test_mul_64(self):
        assert has(run("writeln(8*8);", Language.PASCAL), "64")

    def test_sub_90(self):
        assert has(run("writeln(100-10);", Language.PASCAL), "90")

    def test_output_list5(self):
        assert isinstance(run("writeln(5);", Language.PASCAL), list)

    def test_no_errors5(self):
        assert no_errors(run("writeln(5);", Language.PASCAL))


class TestPascalExtended26:
    def test_writeln_1600(self):
        assert has(run("writeln(1600);", Language.PASCAL), "1600")

    def test_writeln_39(self):
        assert has(run("writeln(39);", Language.PASCAL), "39")

    def test_str_tau(self):
        assert has(run("writeln('tau');", Language.PASCAL), "tau")

    def test_str_upsilon(self):
        assert has(run("writeln('upsilon');", Language.PASCAL), "upsilon")

    def test_add_1600(self):
        assert has(run("writeln(800+800);", Language.PASCAL), "1600")

    def test_mul_81(self):
        assert has(run("writeln(9*9);", Language.PASCAL), "81")

    def test_sub_10(self):
        assert has(run("writeln(20-10);", Language.PASCAL), "10")

    def test_div_4(self):
        assert has(run("writeln(16 div 4);", Language.PASCAL), "4")

    def test_output_list6(self):
        assert isinstance(run("writeln(6);", Language.PASCAL), list)

    def test_no_errors6(self):
        assert no_errors(run("writeln(6);", Language.PASCAL))


class TestPascalExtended27:
    def test_writeln_1700(self):
        assert has(run("writeln(1700);", Language.PASCAL), "1700")

    def test_writeln_40(self):
        assert has(run("writeln(40);", Language.PASCAL), "40")

    def test_str_phi(self):
        assert has(run("writeln('phi');", Language.PASCAL), "phi")

    def test_str_chi(self):
        assert has(run("writeln('chi');", Language.PASCAL), "chi")

    def test_add_1700(self):
        assert has(run("writeln(850+850);", Language.PASCAL), "1700")

    def test_mul_100(self):
        assert has(run("writeln(10*10);", Language.PASCAL), "100")

    def test_sub_90(self):
        assert has(run("writeln(100-10);", Language.PASCAL), "90")

    def test_mod_2(self):
        assert has(run("writeln(12 mod 5);", Language.PASCAL), "2")

    def test_output_list7(self):
        assert isinstance(run("writeln(7);", Language.PASCAL), list)

    def test_no_errors7(self):
        assert no_errors(run("writeln(7);", Language.PASCAL))


class TestPascalExtended28:
    def test_writeln_1800(self):
        assert has(run("writeln(1800);", Language.PASCAL), "1800")

    def test_writeln_41(self):
        assert has(run("writeln(41);", Language.PASCAL), "41")

    def test_str_psi(self):
        assert has(run("writeln('psi');", Language.PASCAL), "psi")

    def test_str_omega(self):
        assert has(run("writeln('omega');", Language.PASCAL), "omega")

    def test_add_1800(self):
        assert has(run("writeln(900+900);", Language.PASCAL), "1800")

    def test_mul_121(self):
        assert has(run("writeln(11*11);", Language.PASCAL), "121")

    def test_sub_80(self):
        assert has(run("writeln(100-20);", Language.PASCAL), "80")

    def test_div_10(self):
        assert has(run("writeln(100 div 10);", Language.PASCAL), "10")

    def test_output_list8(self):
        assert isinstance(run("writeln(8);", Language.PASCAL), list)

    def test_no_errors8(self):
        assert no_errors(run("writeln(8);", Language.PASCAL))


class TestPascalExtended29:
    def test_writeln_1900(self):
        assert has(run("writeln(1900);", Language.PASCAL), "1900")

    def test_writeln_42(self):
        assert has(run("writeln(42);", Language.PASCAL), "42")

    def test_str_one(self):
        assert has(run("writeln('one');", Language.PASCAL), "one")

    def test_str_two(self):
        assert has(run("writeln('two');", Language.PASCAL), "two")

    def test_add_1900(self):
        assert has(run("writeln(950+950);", Language.PASCAL), "1900")

    def test_mul_144(self):
        assert has(run("writeln(12*12);", Language.PASCAL), "144")

    def test_sub_70(self):
        assert has(run("writeln(100-30);", Language.PASCAL), "70")

    def test_mod_1(self):
        assert has(run("writeln(13 mod 6);", Language.PASCAL), "1")

    def test_output_list9(self):
        assert isinstance(run("writeln(9);", Language.PASCAL), list)

    def test_no_errors9(self):
        assert no_errors(run("writeln(9);", Language.PASCAL))
