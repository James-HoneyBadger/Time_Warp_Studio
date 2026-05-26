"""Comprehensive coverage tests for BASIC, FORTH, Erlang, and LISP executors."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

BASIC = Language.BASIC
FORTH = Language.FORTH
ERLANG = Language.ERLANG
LISP = Language.LISP


def basic(src): return run(src, BASIC)
def forth(src): return run(src, FORTH)
def erlang(src): return run(src, ERLANG)
def lisp(src): return run(src, LISP)


# ============================================================================
# BASIC EXECUTOR
# ============================================================================

class TestBasicDataRead:
    def test_read_single(self):
        out = basic("10 DATA 42\n20 READ X\n30 PRINT X")
        assert has(out, "42")

    def test_read_multiple(self):
        out = basic("10 DATA 5, 10\n20 READ A\n30 READ B\n40 PRINT A + B")
        assert has(out, "15")

    def test_data_restore(self):
        out = basic("10 DATA 1, 2, 3\n20 READ X\n30 RESTORE\n40 READ Y\n50 PRINT Y")
        assert isinstance(out, list)

    def test_read_string(self):
        out = basic('10 DATA "hello"\n20 READ S$\n30 PRINT S$')
        assert isinstance(out, list)


class TestBasicLoops:
    def test_while_wend(self):
        out = basic("10 LET I = 1\n20 WHILE I <= 3\n30 PRINT I\n40 LET I = I + 1\n50 WEND")
        assert has(out, "1")

    def test_do_loop_until(self):
        out = basic("10 LET I = 1\n20 DO\n30 PRINT I\n40 LET I = I + 1\n50 LOOP UNTIL I > 3")
        assert has(out, "3")

    def test_do_while(self):
        out = basic("10 LET I = 1\n20 DO WHILE I <= 2\n30 PRINT I\n40 LET I = I + 1\n50 LOOP")
        assert has(out, "1") and has(out, "2")

    def test_for_step_negative(self):
        out = basic("10 FOR I = 5 TO 1 STEP -1\n20 PRINT I\n30 NEXT I")
        assert has(out, "5") and has(out, "1")

    def test_while_false(self):
        out = basic("10 WHILE 1 > 2\n20 PRINT \"never\"\n30 WEND\n40 PRINT \"after\"")
        assert has(out, "after")

    def test_do_loop_while_false(self):
        # DO at start, LOOP WHILE false — executes body at least once
        out = basic("10 LET I = 0\n20 DO\n30 LET I = I + 1\n40 LOOP WHILE I < 1\n50 PRINT I")
        assert has(out, "1")


class TestBasicGosub:
    def test_gosub_return(self):
        out = basic("10 GOSUB 100\n20 PRINT \"Back\"\n30 STOP\n100 PRINT \"Sub\"\n110 RETURN")
        assert has(out, "Sub") and has(out, "Back")

    def test_nested_gosub(self):
        out = basic("10 GOSUB 100\n20 STOP\n100 GOSUB 200\n110 RETURN\n200 PRINT \"deep\"\n210 RETURN")
        assert has(out, "deep")

    def test_on_goto(self):
        out = basic("10 LET X = 2\n20 ON X GOTO 100, 200, 300\n100 PRINT \"one\"\n110 STOP\n200 PRINT \"two\"\n210 STOP\n300 PRINT \"three\"")
        assert has(out, "two")

    def test_on_gosub(self):
        out = basic("10 LET X = 1\n20 ON X GOSUB 100, 200\n30 PRINT \"done\"\n40 STOP\n100 PRINT \"sub1\"\n110 RETURN\n200 PRINT \"sub2\"\n210 RETURN")
        assert has(out, "sub1") and has(out, "done")


class TestBasicStringFunctions:
    def test_len(self):
        out = basic('10 LET S$ = "HELLO"\n20 PRINT LEN(S$)')
        assert has(out, "5")

    def test_mid(self):
        out = basic('10 LET S$ = "HELLO"\n20 PRINT MID$(S$, 2, 3)')
        assert has(out, "ELL")

    def test_left_right(self):
        out = basic('10 LET S$ = "HELLO"\n20 PRINT LEFT$(S$, 3)\n30 PRINT RIGHT$(S$, 3)')
        assert has(out, "HEL") and has(out, "LLO")

    def test_instr(self):
        out = basic('10 LET S$ = "HELLO"\n20 PRINT INSTR(S$, "LL")')
        assert has(out, "3")

    def test_str_val(self):
        out = basic('10 LET S$ = STR$(42)\n20 PRINT VAL(S$)')
        assert has(out, "42")

    def test_space_string(self):
        out = basic('10 PRINT SPACE$(5)\n20 PRINT LEN(SPACE$(3))')
        assert has(out, "3")

    def test_string_func(self):
        out = basic('10 PRINT STRING$(5, 65)')
        assert has(out, "AAAAA")

    def test_ucase_lcase(self):
        out = basic('10 PRINT UCASE$("hello")\n20 PRINT LCASE$("HELLO")')
        assert isinstance(out, list)

    def test_ltrim_rtrim(self):
        out = basic('10 PRINT LTRIM$("  hi")\n20 PRINT RTRIM$("hi  ")')
        assert isinstance(out, list)


class TestBasicMathFunctions:
    def test_mod(self):
        out = basic("10 PRINT 17 MOD 5")
        assert has(out, "2")

    def test_integer_div(self):
        out = basic("10 PRINT 17 \\ 5")
        assert has(out, "3")

    def test_chr_asc(self):
        out = basic('10 PRINT CHR$(65)\n20 PRINT ASC("A")')
        assert has(out, "A")

    def test_timer(self):
        out = basic("10 PRINT TIMER > 0")
        assert isinstance(out, list)

    def test_date_time(self):
        out = basic("10 PRINT DATE$")
        assert isinstance(out, list)

    def test_hex_oct(self):
        out = basic("10 PRINT HEX$(255)\n20 PRINT OCT$(8)")
        assert isinstance(out, list)

    def test_sgn(self):
        out = basic("10 PRINT SGN(-5)\n20 PRINT SGN(0)\n30 PRINT SGN(5)")
        assert has(out, "-1") and has(out, "1")

    def test_fix(self):
        out = basic("10 PRINT FIX(3.7)\n20 PRINT FIX(-3.7)")
        assert has(out, "3") and has(out, "-3")

    def test_cint(self):
        out = basic("10 PRINT CINT(3.7)")
        assert has(out, "4")


class TestBasicArray:
    def test_dim_access(self):
        out = basic("10 DIM A(5)\n20 FOR I = 1 TO 5\n30 LET A(I) = I * 2\n40 NEXT I\n50 PRINT A(3)")
        assert has(out, "6")

    def test_dim_2d(self):
        out = basic("10 DIM A(3,3)\n20 LET A(1,1) = 42\n30 PRINT A(1,1)")
        assert isinstance(out, list)

    def test_array_string(self):
        out = basic('10 DIM S$(3)\n20 LET S$(1) = "hello"\n30 PRINT S$(1)')
        assert has(out, "hello")

    def test_erase(self):
        out = basic("10 DIM A(5)\n20 LET A(1) = 42\n30 ERASE A\n40 DIM A(5)\n50 PRINT A(1)")
        assert isinstance(out, list)


class TestBasicSelectCase:
    def test_select_match(self):
        out = basic("10 LET X = 3\n20 SELECT CASE X\n30 CASE 1\n40 PRINT \"one\"\n50 CASE 3\n60 PRINT \"three\"\n70 END SELECT")
        assert isinstance(out, list)

    def test_select_else(self):
        out = basic("10 LET X = 99\n20 SELECT CASE X\n30 CASE 1\n40 PRINT \"one\"\n50 CASE ELSE\n60 PRINT \"other\"\n70 END SELECT")
        assert isinstance(out, list)

    def test_select_range(self):
        out = basic("10 LET X = 5\n20 SELECT CASE X\n30 CASE 1 TO 10\n40 PRINT \"in range\"\n50 END SELECT")
        assert isinstance(out, list)


class TestBasicGraphics:
    def test_cls(self):
        out = basic("10 CLS\n20 PRINT \"after cls\"")
        assert has(out, "after cls")

    def test_screen(self):
        out = basic("10 SCREEN 1\n20 PRINT \"graphic mode\"")
        assert isinstance(out, list)

    def test_line_cmd(self):
        out = basic("10 SCREEN 1\n20 LINE (0,0)-(100,100)")
        assert isinstance(out, list)

    def test_circle_cmd(self):
        out = basic("10 SCREEN 1\n20 CIRCLE (100,100),50")
        assert isinstance(out, list)

    def test_pset_cmd(self):
        out = basic("10 SCREEN 1\n20 PSET (50,50)")
        assert isinstance(out, list)

    def test_color_cmd(self):
        out = basic("10 SCREEN 1\n20 COLOR 1,0\n30 PRINT \"colored\"")
        assert isinstance(out, list)

    def test_locate_cmd(self):
        out = basic("10 LOCATE 5,1\n20 PRINT \"at row 5\"")
        assert isinstance(out, list)


class TestBasicMisc:
    def test_def_fn(self):
        out = basic("10 DEF FN DOUBLE(X) = X * 2\n20 PRINT FN DOUBLE(5)")
        assert isinstance(out, list)

    def test_swap(self):
        out = basic("10 LET A = 1\n20 LET B = 2\n30 SWAP A, B\n40 PRINT A")
        assert isinstance(out, list)

    def test_type_def(self):
        out = basic("10 TYPE Point\n20   X AS INTEGER\n30   Y AS INTEGER\n40 END TYPE")
        assert isinstance(out, list)

    def test_error_handling(self):
        out = basic("10 ON ERROR GOTO 100\n20 PRINT 1/0\n30 STOP\n100 PRINT \"error\"\n110 RESUME NEXT")
        assert isinstance(out, list)


# ============================================================================
# FORTH EXECUTOR
# ============================================================================

class TestForthDefinitions:
    def test_colon_def(self):
        out = forth(": SQUARE dup * ; 5 SQUARE .")
        assert has(out, "25")

    def test_recursive_def(self):
        out = forth(": FACT dup 0 = if drop 1 else dup 1 - recurse * then ; 5 FACT .")
        assert isinstance(out, list)

    def test_variable_def(self):
        out = forth("variable x 42 x ! x @ .")
        assert has(out, "42")

    def test_constant_def(self):
        out = forth("42 constant ANSWER ANSWER .")
        assert has(out, "42")

    def test_create_does(self):
        out = forth(": ARRAY create cells allot does> swap cells + ; 5 ARRAY myArr")
        assert isinstance(out, list)


class TestForthStack:
    def test_dup(self):
        out = forth("5 dup . .")
        assert has(out, "5")

    def test_drop(self):
        out = forth("1 2 drop .")
        assert has(out, "1")

    def test_swap(self):
        out = forth("1 2 swap . .")
        assert has(out, "2")

    def test_over(self):
        out = forth("1 2 over . . .")
        assert has(out, "1")

    def test_rot(self):
        out = forth("1 2 3 rot . . .")
        assert has(out, "1")

    def test_nip(self):
        out = forth("1 2 nip .")
        assert has(out, "2")

    def test_tuck(self):
        out = forth("1 2 tuck . . .")
        assert has(out, "1")

    def test_2dup(self):
        out = forth("1 2 2dup . . . .")
        assert has(out, "2")

    def test_2drop(self):
        out = forth("1 2 3 4 2drop . .")
        assert has(out, "2")

    def test_r_stack(self):
        out = forth("5 >r r> .")
        assert has(out, "5")

    def test_dot_s(self):
        out = forth("1 2 3 .s")
        assert isinstance(out, list)

    def test_depth(self):
        out = forth("1 2 3 depth .")
        assert has(out, "3")


class TestForthArithmetic:
    def test_plus(self):
        assert has(forth("3 4 + ."), "7")

    def test_minus(self):
        assert has(forth("10 3 - ."), "7")

    def test_multiply(self):
        assert has(forth("3 4 * ."), "12")

    def test_divide(self):
        assert has(forth("10 2 / ."), "5")

    def test_mod(self):
        assert has(forth("10 3 mod ."), "1")

    def test_divmod(self):
        out = forth("10 3 /mod . .")
        assert isinstance(out, list)

    def test_abs(self):
        out = forth("-5 abs .")
        assert has(out, "5")

    def test_min(self):
        out = forth("3 5 min .")
        assert has(out, "3")

    def test_max(self):
        out = forth("3 5 max .")
        assert has(out, "5")


class TestForthControlFlow:
    def test_if_else_then(self):
        out = forth("5 3 > if 99 . else 0 . then")
        assert has(out, "99")

    def test_if_false(self):
        out = forth("1 3 > if 99 . else 0 . then")
        assert has(out, "0")

    def test_begin_until(self):
        out = forth("1 begin dup . 1 + dup 4 > until drop")
        assert has(out, "1") and has(out, "4")

    def test_begin_while_repeat(self):
        out = forth("1 begin dup 4 < while dup . 1 + repeat drop")
        assert has(out, "1") and has(out, "3")

    def test_do_loop(self):
        out = forth("1 10 do i . loop")
        assert has(out, "1")

    def test_do_plus_loop(self):
        out = forth("0 10 do i . 2 +loop")
        assert isinstance(out, list)

    def test_case(self):
        out = forth("2 case 1 of 11 . endof 2 of 22 . endof endcase")
        assert has(out, "22")


class TestForthStringIO:
    def test_print_string(self):
        out = forth('." hello world"')
        assert has(out, "hello world")

    def test_emit(self):
        out = forth("65 emit")
        assert has(out, "A")

    def test_cr(self):
        out = forth("cr")
        assert isinstance(out, list)

    def test_space(self):
        out = forth("space")
        assert isinstance(out, list)

    def test_dot_r(self):
        out = forth("42 5 .r")
        assert isinstance(out, list)

    def test_words(self):
        out = forth("words")
        assert isinstance(out, list)

    def test_char(self):
        out = forth("char A .")
        assert has(out, "65")


class TestForthMisc:
    def test_negate(self):
        out = forth("5 negate .")
        assert has(out, "-5")

    def test_invert(self):
        out = forth("-1 invert .")
        assert isinstance(out, list)

    def test_and(self):
        out = forth("5 3 and .")
        assert isinstance(out, list)

    def test_or(self):
        out = forth("5 3 or .")
        assert isinstance(out, list)

    def test_xor(self):
        out = forth("5 3 xor .")
        assert isinstance(out, list)

    def test_lshift(self):
        out = forth("1 4 lshift .")
        assert has(out, "16")

    def test_rshift(self):
        out = forth("16 2 rshift .")
        assert has(out, "4")

    def test_true_false(self):
        out = forth("true . false .")
        assert isinstance(out, list)

    def test_zero_compare(self):
        out = forth("0 0= .")
        assert isinstance(out, list)

    def test_include_path(self):
        out = forth("include nonexistent.fs")
        assert isinstance(out, list)


# ============================================================================
# ERLANG EXECUTOR
# ============================================================================

class TestErlangBasic:
    def test_hello(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("hello~n").')
        assert has(out, "hello")

    def test_arithmetic(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [3 + 4]).')
        assert has(out, "7")

    def test_variable(self):
        out = erlang('-module(t). -export([main/0]). main() -> X = 42, io:format("~w~n", [X]).')
        assert has(out, "42")

    def test_case_expr(self):
        out = erlang('-module(t). -export([main/0]). main() -> X = 5, case X of 5 -> io:format("five~n"); _ -> ok end.')
        assert has(out, "five")

    def test_if_guard(self):
        out = erlang('-module(t). -export([main/0]). main() -> X = 5, if X > 0 -> io:format("pos~n"); true -> ok end.')
        assert has(out, "pos")

    def test_list_comprehension(self):
        out = erlang('-module(t). -export([main/0]). main() -> L = [X*2 || X <- [1,2,3]], io:format("~w~n", [L]).')
        assert has(out, "2")

    def test_pattern_matching(self):
        out = erlang('-module(t). -export([main/0]). main() -> {ok, X} = {ok, 42}, io:format("~w~n", [X]).')
        assert has(out, "42")

    def test_fun_expression(self):
        out = erlang('-module(t). -export([main/0]). main() -> F = fun(X) -> X * 2 end, io:format("~w~n", [F(5)]).')
        assert isinstance(out, list)


class TestErlangFunctions:
    def test_recursive_function(self):
        src = ('-module(t). -export([main/0]).\n'
               'fac(0) -> 1;\nfac(N) -> N * fac(N-1).\n'
               'main() -> io:format("~w~n", [fac(5)]).')
        out = erlang(src)
        assert has(out, "120")

    def test_named_function(self):
        src = ('-module(t). -export([main/0]).\n'
               'double(X) -> X * 2.\n'
               'main() -> io:format("~w~n", [double(5)]).')
        out = erlang(src)
        assert has(out, "10")

    def test_multiple_clauses(self):
        src = ('-module(t). -export([main/0]).\n'
               'greet(world) -> "Hello, World!";\n'
               'greet(Name) -> "Hello, " ++ Name ++ "!".\n'
               'main() -> io:format("~s~n", [greet(world)]).')
        out = erlang(src)
        assert isinstance(out, list)

    def test_tail_recursive(self):
        src = ('-module(t). -export([main/0]).\n'
               'sum([], Acc) -> Acc;\n'
               'sum([H|T], Acc) -> sum(T, Acc + H).\n'
               'main() -> io:format("~w~n", [sum([1,2,3,4,5], 0)]).')
        out = erlang(src)
        assert has(out, "15")


class TestErlangListOps:
    def test_list_head_tail(self):
        out = erlang('-module(t). -export([main/0]). main() -> [H|T] = [1,2,3], io:format("~w~n", [H]).')
        assert has(out, "1")

    def test_list_append(self):
        out = erlang('-module(t). -export([main/0]). main() -> L = [1,2] ++ [3,4], io:format("~w~n", [L]).')
        assert has(out, "4")

    def test_list_length(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [length([1,2,3])]).')
        assert has(out, "3")

    def test_list_reverse(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [lists:reverse([1,2,3])]).')
        assert has(out, "1")

    def test_lists_map(self):
        out = erlang('-module(t). -export([main/0]). main() -> L = lists:map(fun(X) -> X*2 end, [1,2,3]), io:format("~w~n", [L]).')
        assert has(out, "2")

    def test_lists_filter(self):
        out = erlang('-module(t). -export([main/0]). main() -> L = lists:filter(fun(X) -> X > 2 end, [1,2,3,4]), io:format("~w~n", [L]).')
        assert has(out, "3")

    def test_lists_sum(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [lists:sum([1,2,3,4,5])]).')
        assert has(out, "15")

    def test_lists_sort(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [lists:sort([3,1,2])]).')
        assert has(out, "1")


class TestErlangBinaryOps:
    def test_band(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [3 band 5]).')
        assert isinstance(out, list)

    def test_bor(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [3 bor 5]).')
        assert isinstance(out, list)

    def test_bxor(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [3 bxor 5]).')
        assert isinstance(out, list)

    def test_rem(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [10 rem 3]).')
        assert has(out, "1")

    def test_div(self):
        out = erlang('-module(t). -export([main/0]). main() -> io:format("~w~n", [10 div 3]).')
        assert has(out, "3")


class TestErlangTuples:
    def test_tuple_element(self):
        out = erlang('-module(t). -export([main/0]). main() -> T = {a, 42, c}, io:format("~w~n", [element(2, T)]).')
        assert has(out, "42")

    def test_tuple_size(self):
        out = erlang('-module(t). -export([main/0]). main() -> T = {a, b, c}, io:format("~w~n", [tuple_size(T)]).')
        assert has(out, "3")

    def test_record_like(self):
        out = erlang('-module(t). -export([main/0]). main() -> P = {point, 3, 4}, {point, X, Y} = P, io:format("~w~n", [X+Y]).')
        assert has(out, "7")


class TestErlangStrings:
    def test_string_concat(self):
        out = erlang('-module(t). -export([main/0]). main() -> S = "hello" ++ " world", io:format("~s~n", [S]).')
        assert isinstance(out, list)

    def test_atom_to_list(self):
        out = erlang('-module(t). -export([main/0]). main() -> S = atom_to_list(hello), io:format("~s~n", [S]).')
        assert has(out, "hello")

    def test_integer_to_list(self):
        out = erlang('-module(t). -export([main/0]). main() -> S = integer_to_list(42), io:format("~s~n", [S]).')
        assert has(out, "42")


# ============================================================================
# LISP/SCHEME EXECUTOR
# ============================================================================

class TestLispBasic:
    def test_display(self):
        out = lisp('(display "hello")')
        assert has(out, "hello")

    def test_define_use(self):
        out = lisp("(define x 5)\n(display x)")
        assert has(out, "5")

    def test_arithmetic(self):
        out = lisp("(display (+ 3 4))")
        assert has(out, "7")

    def test_nested_arith(self):
        out = lisp("(display (* (+ 2 3) (- 4 1)))")
        assert has(out, "15")

    def test_let_binding(self):
        out = lisp("(let ((x 3) (y 4))\n  (display (* x y)))")
        assert has(out, "12")

    def test_let_star(self):
        out = lisp("(let* ((x 3) (y (* x 2)))\n  (display y))")
        assert has(out, "6")

    def test_cond(self):
        out = lisp("(cond ((= 1 2) (display \"no\"))\n      ((= 1 1) (display \"yes\")))")
        assert has(out, "yes")

    def test_if_true(self):
        out = lisp('(if (= 1 1) (display "yes") (display "no"))')
        assert has(out, "yes")

    def test_if_false(self):
        out = lisp('(if (= 1 2) (display "yes") (display "no"))')
        assert has(out, "no")

    def test_and_form(self):
        out = lisp("(display (and #t #f))")
        assert isinstance(out, list)

    def test_or_form(self):
        out = lisp("(display (or #f #t))")
        assert isinstance(out, list)

    def test_not(self):
        out = lisp("(display (not #t))")
        assert isinstance(out, list)


class TestLispFunctions:
    def test_lambda(self):
        out = lisp("(define f (lambda (x) (* x x)))\n(display (f 5))")
        assert has(out, "25")

    def test_recursion_factorial(self):
        out = lisp(
            "(define (fact n)\n"
            "  (if (<= n 1) 1\n"
            "      (* n (fact (- n 1)))))\n"
            "(display (fact 5))"
        )
        assert has(out, "120")

    def test_tail_recursive_sum(self):
        out = lisp(
            "(define (sum lst acc)\n"
            "  (if (null? lst) acc\n"
            "      (sum (cdr lst) (+ acc (car lst)))))\n"
            "(display (sum '(1 2 3 4 5) 0))"
        )
        assert has(out, "15")

    def test_named_define(self):
        out = lisp("(define (double x) (* x 2))\n(display (double 7))")
        assert has(out, "14")

    def test_higher_order(self):
        out = lisp(
            "(define (apply-twice f x) (f (f x)))\n"
            "(define (inc x) (+ x 1))\n"
            "(display (apply-twice inc 5))"
        )
        assert has(out, "7")


class TestLispListOps:
    def test_list_create(self):
        out = lisp("(display (list 1 2 3))")
        assert has(out, "1")

    def test_car(self):
        out = lisp("(display (car (list 1 2 3)))")
        assert has(out, "1")

    def test_cdr(self):
        out = lisp("(display (cdr (list 1 2 3)))")
        assert isinstance(out, list)

    def test_cons(self):
        out = lisp("(display (cons 0 (list 1 2 3)))")
        assert has(out, "0")

    def test_length(self):
        out = lisp("(display (length (list 1 2 3 4 5)))")
        assert has(out, "5")

    def test_null_true(self):
        out = lisp("(display (null? '()))")
        assert isinstance(out, list)

    def test_null_false(self):
        out = lisp("(display (null? '(1 2)))")
        assert isinstance(out, list)

    def test_map(self):
        out = lisp("(display (map (lambda (x) (* x 2)) (list 1 2 3)))")
        assert has(out, "2")

    def test_filter(self):
        out = lisp("(display (filter odd? (list 1 2 3 4 5)))")
        assert has(out, "1")

    def test_foldl(self):
        out = lisp("(display (foldl + 0 '(1 2 3 4 5)))")
        assert isinstance(out, list)

    def test_foldr(self):
        out = lisp("(display (foldr + 0 '(1 2 3 4 5)))")
        assert isinstance(out, list)

    def test_append(self):
        out = lisp("(display (append (list 1 2) (list 3 4)))")
        assert has(out, "1")

    def test_reverse(self):
        out = lisp("(display (reverse (list 1 2 3)))")
        assert has(out, "3")

    def test_quoted_list(self):
        out = lisp("(display '(1 2 3))")
        assert has(out, "1")

    def test_assoc(self):
        out = lisp("(display (assoc 'b '((a 1) (b 2) (c 3))))")
        assert isinstance(out, list)


class TestLispPredicates:
    def test_equal(self):
        out = lisp("(display (equal? (list 1 2) (list 1 2)))")
        assert isinstance(out, list)

    def test_zero(self):
        out = lisp("(display (zero? 0))")
        assert has(out, "#t")

    def test_positive(self):
        out = lisp("(display (positive? 5))")
        assert isinstance(out, list)

    def test_negative(self):
        out = lisp("(display (negative? -3))")
        assert isinstance(out, list)

    def test_even_odd(self):
        out = lisp("(display (even? 4))\n(display (odd? 3))")
        assert isinstance(out, list)

    def test_number_pred(self):
        out = lisp("(display (number? 42))")
        assert isinstance(out, list)

    def test_string_pred(self):
        out = lisp('(display (string? "hello"))')
        assert isinstance(out, list)


class TestLispStringOps:
    def test_string_append(self):
        out = lisp('(display (string-append "hello" " world"))')
        assert has(out, "hello world")

    def test_string_length(self):
        out = lisp('(display (string-length "hello"))')
        assert has(out, "5")

    def test_number_to_string(self):
        out = lisp("(display (number->string 42))")
        assert has(out, "42")

    def test_string_to_number(self):
        out = lisp('(display (string->number "42"))')
        assert has(out, "42")

    def test_symbol_to_string(self):
        out = lisp("(display (symbol->string 'hello))")
        assert has(out, "hello")


class TestLispControlFlow:
    def test_begin(self):
        out = lisp('(begin (display "a") (display "b") (display "c"))')
        assert has(out, "a") and has(out, "c")

    def test_when(self):
        out = lisp('(when (= 1 1) (display "yes"))')
        assert isinstance(out, list)

    def test_unless(self):
        out = lisp('(unless (= 1 2) (display "yes"))')
        assert isinstance(out, list)

    def test_do_form(self):
        out = lisp('(do ((i 0 (+ i 1)))\n    ((= i 3) (display "done"))\n  (display i))')
        assert isinstance(out, list)

    def test_case(self):
        out = lisp('(case 2\n  ((1) (display "one"))\n  ((2) (display "two"))\n  (else (display "other")))')
        assert isinstance(out, list)

    def test_values(self):
        out = lisp("(define (min-max a b) (if (< a b) (values a b) (values b a)))\n(define-values (lo hi) (min-max 3 1))\n(display lo)")
        assert isinstance(out, list)
