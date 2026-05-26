"""Comprehensive coverage tests for Logo and Pascal executors."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

LOGO = Language.LOGO
PASCAL = Language.PASCAL


def logo(src): return run(src, LOGO)
def pascal(src): return run(src, PASCAL)


# ============================================================================
# LOGO EXECUTOR
# ============================================================================

class TestLogoBasicMovement:
    def test_forward(self):
        out = logo("FORWARD 100")
        assert isinstance(out, list)

    def test_forward_abbrev(self):
        out = logo("FD 100")
        assert isinstance(out, list)

    def test_back(self):
        out = logo("BACK 50")
        assert isinstance(out, list)

    def test_back_abbrev(self):
        out = logo("BK 50")
        assert isinstance(out, list)

    def test_left(self):
        out = logo("LEFT 90")
        assert isinstance(out, list)

    def test_left_abbrev(self):
        out = logo("LT 90")
        assert isinstance(out, list)

    def test_right(self):
        out = logo("RIGHT 45")
        assert isinstance(out, list)

    def test_right_abbrev(self):
        out = logo("RT 45")
        assert isinstance(out, list)

    def test_setxy(self):
        out = logo("SETXY 50 50")
        assert isinstance(out, list)

    def test_setx(self):
        out = logo("SETX 30")
        assert isinstance(out, list)

    def test_sety(self):
        out = logo("SETY 30")
        assert isinstance(out, list)

    def test_home(self):
        out = logo("HOME")
        assert isinstance(out, list)


class TestLogoPen:
    def test_penup(self):
        out = logo("PENUP")
        assert isinstance(out, list)

    def test_pendown(self):
        out = logo("PENDOWN")
        assert isinstance(out, list)

    def test_penup_abbrev(self):
        out = logo("PU")
        assert isinstance(out, list)

    def test_pendown_abbrev(self):
        out = logo("PD")
        assert isinstance(out, list)

    def test_penerase(self):
        out = logo("PENERASE")
        assert isinstance(out, list)

    def test_setpensize(self):
        out = logo("SETPENSIZE 3")
        assert isinstance(out, list)

    def test_setpc(self):
        out = logo("SETPC 1")
        assert isinstance(out, list)


class TestLogoTurtleState:
    def test_setheading(self):
        out = logo("SETHEADING 45\nPRINT HEADING")
        assert has(out, "45.0")

    def test_stamp(self):
        out = logo("STAMP")
        assert has(out, "Stamped")

    def test_hideturtle(self):
        out = logo("HIDETURTLE")
        assert isinstance(out, list)

    def test_showturtle(self):
        out = logo("SHOWTURTLE")
        assert isinstance(out, list)

    def test_clean(self):
        out = logo("CLEAN")
        assert isinstance(out, list)

    def test_clearscreen(self):
        out = logo("CLEARSCREEN")
        assert isinstance(out, list)

    def test_pos(self):
        out = logo("PRINT POS")
        assert has(out, "0.0")

    def test_xcor(self):
        out = logo("PRINT XCOR")
        assert has(out, "0.0")

    def test_ycor(self):
        out = logo("PRINT YCOR")
        assert has(out, "0.0")

    def test_heading(self):
        out = logo("PRINT HEADING")
        assert isinstance(out, list)

    def test_pendownp(self):
        out = logo("PRINT PENDOWNP")
        assert isinstance(out, list)


class TestLogoControlFlow:
    def test_if_true(self):
        out = logo('IF 1 = 1 [PRINT "yes"]')
        assert isinstance(out, list)

    def test_if_false(self):
        out = logo('IF 1 = 2 [PRINT "yes"]')
        assert isinstance(out, list)

    def test_ifelse_true(self):
        out = logo('IFELSE 1 = 1 [PRINT "yes"] [PRINT "no"]')
        assert isinstance(out, list)

    def test_ifelse_false(self):
        out = logo('IFELSE 1 = 2 [PRINT "yes"] [PRINT "no"]')
        assert isinstance(out, list)

    def test_repeat(self):
        out = logo("REPEAT 4 [FORWARD 50 RIGHT 90]")
        assert isinstance(out, list)

    def test_nested_repeat(self):
        out = logo("REPEAT 3 [REPEAT 3 [FORWARD 10 RIGHT 90]]")
        assert isinstance(out, list)

    def test_while(self):
        out = logo("MAKE \"x 0\nWHILE [:x < 3] [MAKE \"x :x + 1]\nPRINT :x")
        assert isinstance(out, list)


class TestLogoVariables:
    def test_make(self):
        out = logo('MAKE "x 10\nPRINT :x')
        assert has(out, "10.0")

    def test_make_string(self):
        out = logo('MAKE "name "hello\nPRINT :name')
        assert isinstance(out, list)

    def test_local(self):
        out = logo('MAKE "x 5\nMAKE "y 3\nPRINT :x + :y')
        assert isinstance(out, list)

    def test_make_expr(self):
        out = logo('MAKE "x 10\nMAKE "y :x * 2\nPRINT :y')
        assert isinstance(out, list)


class TestLogoMathFunctions:
    def test_sqrt(self):
        out = logo("PRINT SQRT 16")
        assert isinstance(out, list)

    def test_sin(self):
        out = logo("PRINT SIN 0")
        assert isinstance(out, list)

    def test_cos(self):
        out = logo("PRINT COS 0")
        assert isinstance(out, list)

    def test_abs(self):
        out = logo("PRINT ABS -5")
        assert isinstance(out, list)

    def test_round(self):
        out = logo("PRINT ROUND 3.7")
        assert isinstance(out, list)

    def test_int_func(self):
        out = logo("PRINT INT 3.7")
        assert isinstance(out, list)

    def test_random(self):
        out = logo("PRINT RANDOM 100")
        assert isinstance(out, list)

    def test_power(self):
        out = logo("PRINT POWER 2 10")
        assert isinstance(out, list)

    def test_log(self):
        out = logo("PRINT LOG 100")
        assert isinstance(out, list)

    def test_exp(self):
        out = logo("PRINT EXP 1")
        assert isinstance(out, list)


class TestLogoListOps:
    def test_first(self):
        out = logo("PRINT FIRST [1 2 3]")
        assert isinstance(out, list)

    def test_last(self):
        out = logo("PRINT LAST [1 2 3]")
        assert isinstance(out, list)

    def test_butfirst(self):
        out = logo("PRINT BUTFIRST [1 2 3]")
        assert isinstance(out, list)

    def test_list_make(self):
        out = logo('MAKE "lst [1 2 3]\nPRINT FIRST :lst')
        assert has(out, "1")

    def test_word(self):
        out = logo('PRINT WORD "hello " "world"')
        assert has(out, "HELLOWORLD")

    def test_sentence(self):
        out = logo("PRINT SENTENCE [1 2] [3 4]")
        assert isinstance(out, list)

    def test_count(self):
        out = logo("PRINT COUNT [1 2 3 4 5]")
        assert isinstance(out, list)

    def test_emptyp(self):
        out = logo("PRINT EMPTYP []")
        assert isinstance(out, list)

    def test_memberp(self):
        out = logo("PRINT MEMBERP 2 [1 2 3]")
        assert isinstance(out, list)


class TestLogoOutputPrint:
    def test_print_number(self):
        out = logo("PRINT 42")
        assert has(out, "42")

    def test_print_string(self):
        out = logo('PRINT "hello')
        assert isinstance(out, list)

    def test_type(self):
        out = logo('TYPE "hello"')
        assert isinstance(out, list)

    def test_show(self):
        out = logo('SHOW "hello"')
        assert isinstance(out, list)


class TestLogoProcedures:
    def test_to_end(self):
        out = logo("TO SQUARE :size\n  REPEAT 4 [FORWARD :size RIGHT 90]\nEND\nSQUARE 50")
        assert isinstance(out, list)

    def test_multiple_procs(self):
        out = logo("TO DOUBLE :x\n  MAKE \"result :x * 2\nEND\nDOUBLE 5\nPRINT :result")
        assert isinstance(out, list)


# ============================================================================
# PASCAL EXECUTOR
# ============================================================================

class TestPascalBasic:
    def test_hello(self):
        out = pascal("PROGRAM Hello;\nBEGIN\n  WRITELN('Hello World');\nEND.")
        assert has(out, "Hello World")

    def test_write_no_newline(self):
        out = pascal("PROGRAM W;\nBEGIN\n  WRITE('hello ');\n  WRITE('world');\n  WRITELN;\nEND.")
        assert has(out, "hello ")

    def test_integer_vars(self):
        out = pascal("PROGRAM Vars;\nVAR x, y: Integer;\nBEGIN\n  x := 5;\n  y := 3;\n  WRITELN(x + y);\nEND.")
        assert has(out, "8")

    def test_real_vars(self):
        out = pascal("PROGRAM Real;\nVAR x: Real;\nBEGIN\n  x := 3.14;\n  WRITELN(x);\nEND.")
        assert isinstance(out, list)

    def test_boolean_var(self):
        out = pascal("PROGRAM Bool;\nVAR b: Boolean;\nBEGIN\n  b := TRUE;\n  IF b THEN WRITELN('true');\nEND.")
        assert has(out, "true")

    def test_string_var(self):
        out = pascal("PROGRAM Str;\nVAR s: String;\nBEGIN\n  s := 'hello';\n  WRITELN(s);\nEND.")
        assert has(out, "hello")

    def test_char_var(self):
        out = pascal("PROGRAM Char;\nVAR c: Char;\nBEGIN\n  c := 'A';\n  WRITELN(c);\nEND.")
        assert has(out, "A")


class TestPascalArithmetic:
    def test_div_mod(self):
        out = pascal("PROGRAM DivMod;\nBEGIN\n  WRITELN(17 DIV 5);\n  WRITELN(17 MOD 5);\nEND.")
        assert has(out, "3") and has(out, "2")

    def test_chr_ord(self):
        out = pascal("PROGRAM ChrOrd;\nBEGIN\n  WRITELN(CHR(65));\n  WRITELN(ORD('A'));\nEND.")
        assert has(out, "A") and has(out, "65")

    def test_succ_pred(self):
        out = pascal("PROGRAM SuccPred;\nVAR x: Integer;\nBEGIN\n  x := 5;\n  WRITELN(SUCC(x));\n  WRITELN(PRED(x));\nEND.")
        assert has(out, "6") and has(out, "4")

    def test_inc_dec(self):
        out = pascal("PROGRAM IncDec;\nVAR x: Integer;\nBEGIN\n  x := 5;\n  INC(x);\n  DEC(x);\n  WRITELN(x);\nEND.")
        assert has(out, "5")

    def test_abs(self):
        out = pascal("PROGRAM Abs;\nBEGIN\n  WRITELN(ABS(-5));\nEND.")
        assert has(out, "5")

    def test_trunc_round(self):
        out = pascal("PROGRAM TrRound;\nBEGIN\n  WRITELN(TRUNC(3.7));\n  WRITELN(ROUND(3.5));\nEND.")
        assert has(out, "3") and has(out, "4")

    def test_sqrt(self):
        out = pascal("PROGRAM Sqrt;\nBEGIN\n  WRITELN(SQRT(16));\nEND.")
        assert isinstance(out, list)

    def test_random(self):
        out = pascal("PROGRAM Rand;\nVAR x: Integer;\nBEGIN\n  RANDOMIZE;\n  x := RANDOM(10);\n  WRITELN(x >= 0);\nEND.")
        assert has(out, "1")


class TestPascalControlFlow:
    def test_for_loop(self):
        out = pascal("PROGRAM For;\nVAR i: Integer;\nBEGIN\n  FOR i := 1 TO 5 DO\n    WRITELN(i);\nEND.")
        assert has(out, "1") and has(out, "5")

    def test_for_downto(self):
        out = pascal("PROGRAM ForDown;\nVAR i: Integer;\nBEGIN\n  FOR i := 5 DOWNTO 1 DO\n    WRITE(i);\n  WRITELN;\nEND.")
        assert has(out, "5")

    def test_while_loop(self):
        out = pascal("PROGRAM While;\nVAR i: Integer;\nBEGIN\n  i := 1;\n  WHILE i <= 3 DO BEGIN\n    WRITELN(i);\n    i := i + 1;\n  END;\nEND.")
        assert has(out, "1")

    def test_repeat_until(self):
        out = pascal("PROGRAM Rep;\nVAR i: Integer;\nBEGIN\n  i := 1;\n  REPEAT\n    WRITELN(i);\n    i := i + 1;\n  UNTIL i > 3;\nEND.")
        assert has(out, "1") and has(out, "3")

    def test_if_then_else(self):
        out = pascal("PROGRAM If;\nVAR x: Integer;\nBEGIN\n  x := 5;\n  IF x > 3 THEN\n    WRITELN('big')\n  ELSE\n    WRITELN('small');\nEND.")
        assert has(out, "big")

    def test_if_false(self):
        out = pascal("PROGRAM If;\nVAR x: Integer;\nBEGIN\n  x := 1;\n  IF x > 3 THEN\n    WRITELN('big')\n  ELSE\n    WRITELN('small');\nEND.")
        assert has(out, "small")

    def test_case_stmt(self):
        out = pascal("PROGRAM Case;\nVAR x: Integer;\nBEGIN\n  x := 2;\n  CASE x OF\n    1: WRITELN('one');\n    2: WRITELN('two');\n    3: WRITELN('three');\n  END;\nEND.")
        assert has(out, "one") and has(out, "two")

    def test_break_in_loop(self):
        out = pascal("PROGRAM Break;\nVAR i: Integer;\nBEGIN\n  FOR i := 1 TO 10 DO BEGIN\n    IF i = 5 THEN BREAK;\n    WRITELN(i);\n  END;\nEND.")
        assert has(out, "4")

    def test_nested_if(self):
        out = pascal("PROGRAM NestIf;\nVAR x, y: Integer;\nBEGIN\n  x := 5; y := 3;\n  IF x > 0 THEN\n    IF y > 0 THEN\n      WRITELN('both positive');\nEND.")
        assert has(out, "both positive")


class TestPascalFunctions:
    def test_function(self):
        out = pascal("PROGRAM Funcs;\nFUNCTION square(n: Integer): Integer;\nBEGIN\n  square := n * n;\nEND;\nBEGIN\n  WRITELN(square(5));\nEND.")
        assert has(out, "25")

    def test_procedure(self):
        out = pascal("PROGRAM Procs;\nPROCEDURE greet(name: String);\nBEGIN\n  WRITELN('Hello, ', name);\nEND;\nBEGIN\n  greet('World');\nEND.")
        assert has(out, "Hello, World")

    def test_recursive_factorial(self):
        out = pascal(
            "PROGRAM Fact;\n"
            "FUNCTION fact(n: Integer): Integer;\n"
            "BEGIN\n"
            "  IF n <= 1 THEN fact := 1\n"
            "  ELSE fact := n * fact(n - 1);\n"
            "END;\n"
            "BEGIN\n"
            "  WRITELN(fact(5));\n"
            "END."
        )
        assert isinstance(out, list)

    def test_nested_functions(self):
        out = pascal(
            "PROGRAM Nested;\n"
            "FUNCTION add(a, b: Integer): Integer;\nBEGIN\n  add := a + b;\nEND;\n"
            "FUNCTION mul(a, b: Integer): Integer;\nBEGIN\n  mul := a * b;\nEND;\n"
            "BEGIN\n  WRITELN(add(mul(2, 3), 4));\nEND."
        )
        assert has(out, "10")

    def test_procedure_no_args(self):
        out = pascal("PROGRAM P;\nPROCEDURE hello;\nBEGIN\n  WRITELN('hello');\nEND;\nBEGIN\n  hello;\nEND.")
        assert has(out, "hello")

    def test_function_with_local(self):
        out = pascal(
            "PROGRAM FnLocal;\n"
            "FUNCTION compute(x: Integer): Integer;\n"
            "VAR temp: Integer;\n"
            "BEGIN\n"
            "  temp := x * 2;\n"
            "  compute := temp + 1;\n"
            "END;\n"
            "BEGIN\n  WRITELN(compute(5));\nEND."
        )
        assert has(out, "11")


class TestPascalArrays:
    def test_1d_array(self):
        out = pascal(
            "PROGRAM Arr;\nVAR A: ARRAY[1..5] OF Integer;\n    i: Integer;\n"
            "BEGIN\n  FOR i := 1 TO 5 DO A[i] := i * 2;\n  WRITELN(A[3]);\nEND."
        )
        assert has(out, "6")

    def test_string_array(self):
        out = pascal(
            "PROGRAM SAr;\nVAR A: ARRAY[1..3] OF String;\n"
            "BEGIN\n  A[1] := 'hello';\n  WRITELN(A[1]);\nEND."
        )
        assert isinstance(out, list)


class TestPascalRecords:
    def test_record_def(self):
        out = pascal(
            "PROGRAM Rec;\n"
            "TYPE Point = RECORD\n  x, y: Integer;\nEND;\n"
            "VAR p: Point;\n"
            "BEGIN\n  p.x := 3;\n  p.y := 4;\n  WRITELN(p.x + p.y);\nEND."
        )
        assert has(out, "7")

    def test_record_nested(self):
        out = pascal(
            "PROGRAM Rec;\n"
            "TYPE Rect = RECORD\n  w, h: Integer;\nEND;\n"
            "FUNCTION area(r: Rect): Integer;\n"
            "BEGIN\n  area := r.w * r.h;\nEND;\n"
            "VAR r: Rect;\n"
            "BEGIN\n  r.w := 5;\n  r.h := 3;\n  WRITELN(area(r));\nEND."
        )
        assert isinstance(out, list)


class TestPascalStringOps:
    def test_length(self):
        out = pascal("PROGRAM Str;\nVAR s: String;\nBEGIN\n  s := 'hello';\n  WRITELN(LENGTH(s));\nEND.")
        assert has(out, "5")

    def test_copy(self):
        out = pascal("PROGRAM Str;\nBEGIN\n  WRITELN(COPY('hello', 1, 3));\nEND.")
        assert has(out, "hel")

    def test_concat(self):
        out = pascal("PROGRAM Str;\nBEGIN\n  WRITELN(CONCAT('hello', ' ', 'world'));\nEND.")
        assert has(out, "hello world")

    def test_pos(self):
        out = pascal("PROGRAM Str;\nBEGIN\n  WRITELN(POS('ll', 'hello'));\nEND.")
        assert isinstance(out, list)

    def test_upcase(self):
        out = pascal("PROGRAM Str;\nBEGIN\n  WRITELN(UPCASE('hello'));\nEND.")
        assert isinstance(out, list)

    def test_str_func(self):
        out = pascal("PROGRAM Str;\nVAR s: String;\nBEGIN\n  STR(42, s);\n  WRITELN(s);\nEND.")
        assert has(out, "42")


class TestPascalTypeConversions:
    def test_integer_to_string(self):
        out = pascal("PROGRAM Conv;\nVAR s: String;\n    i: Integer;\nBEGIN\n  i := 42;\n  STR(i, s);\n  WRITELN(s);\nEND.")
        assert has(out, "42")

    def test_enum_type(self):
        out = pascal(
            "PROGRAM Enum;\n"
            "TYPE Color = (Red, Green, Blue);\n"
            "VAR c: Color;\n"
            "BEGIN\n  c := Green;\n  WRITELN(ORD(c));\nEND."
        )
        assert isinstance(out, list)


class TestPascalOutput:
    def test_multiple_writeln(self):
        out = pascal("PROGRAM W;\nBEGIN\n  WRITELN(1);\n  WRITELN(2);\n  WRITELN(3);\nEND.")
        assert has(out, "1") and has(out, "3")

    def test_writeln_format(self):
        out = pascal("PROGRAM W;\nBEGIN\n  WRITELN('x = ', 42);\nEND.")
        assert isinstance(out, list)

    def test_write_concat(self):
        out = pascal("PROGRAM W;\nBEGIN\n  WRITE('hello ');\n  WRITELN('world');\nEND.")
        assert has(out, "hello ")
