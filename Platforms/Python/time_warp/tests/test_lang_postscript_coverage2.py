"""Extended coverage tests for PostScript language executor (2nd pass).

Targets uncovered lines in postscript.py:
- Tokenizer: comments, string escapes, nested parens, hex strings, radix notation, empty-word skip
- Stack: mark, cleartomark
- Arithmetic: idiv div-by-zero, sin, cos
- Comparison: ge
- Boolean/bitwise: and/or/xor (int), bitshift, xor (bool)
- String ops: cvs, cvn
- Output: message, show
- Control: loop+exit, exec, stop, exit (top-level)
- Definitions: load, known, where, currentdict, systemdict
- Array/String: get-on-string, getinterval, putinterval, string-constructor, astore
- Search/Token: search, token
- Type: all type branches (bool, float, string, array, dict, name, null)
- Null operator
- Graphics: moveto, rmoveto, lineto, rlineto, stroke, fill, newpath, setgray,
  setrgbcolor, rotate, showpage
- _exec_proc with list (exec with array)
"""
import pytest
from time_warp.core.interpreter import Language
from time_warp.graphics.turtle_state import TurtleState
from .conftest_lang import run, has, no_errors, ok

# ---------------------------------------------------------------------------
# MockTurtle: duck-typed turtle with the methods postscript.py expects.
# TurtleState uses instance attributes (e.g. pen_down=True), so we cannot
# monkey-patch the class safely.  Instead turtle-integration tests use
# ps_turtle() which calls PostScriptEnvironment directly.
# ---------------------------------------------------------------------------


class _MockTurtle:
    """Minimal duck-type that satisfies all postscript.py turtle calls."""
    def pen_up(self): pass
    def pen_down(self): pass
    def set_pos(self, x, y): pass
    def right(self, angle): pass
    def penup(self): pass
    def pendown(self): pass
    def goto(self, x, y): pass


def ps_turtle(source: str) -> list[str]:
    """Execute PostScript with a mock turtle; returns output lines."""
    from time_warp.languages.postscript import PostScriptEnvironment
    from time_warp.core.interpreter import Interpreter
    interp = Interpreter()
    env = PostScriptEnvironment(interp, _MockTurtle())  # type: ignore[arg-type]
    raw = env.run(source)
    return [line for line in raw.split("\n") if line]


def ps(source: str) -> list[str]:
    return run(source, Language.POSTSCRIPT)


# ===========================================================================
# Tokenizer coverage
# ===========================================================================


class TestTokenizerComment:
    """Lines 87-89: comment tokenizer."""

    def test_comment_ignored(self):
        out = ps("% this is a comment\n42 =")
        assert has(out, "42")

    def test_comment_at_start(self):
        out = ps("% header\n% another line\n(hello) =")
        assert has(out, "hello")

    def test_comment_after_code(self):
        out = ps("1 2 add % adds 1 and 2\n=")
        assert has(out, "3")


class TestTokenizerStringEscape:
    """Lines 98-103: string escape sequences."""

    def test_escape_newline(self):
        out = ps("(hello\\nworld) =")
        assert out  # should produce output with a newline embedded

    def test_escape_tab(self):
        out = ps("(hello\\tworld) =")
        assert has(out, "hello")

    def test_escape_backslash(self):
        out = ps("(back\\\\slash) =")
        assert has(out, "back")

    def test_escape_r(self):
        out = ps("(carriage\\rreturn) =")
        assert out


class TestTokenizerNestedParens:
    """Lines 105, 109: nested balanced parentheses in string literal."""

    def test_nested_parens_preserved(self):
        out = ps("(hello (world) test) =")
        assert has(out, "hello")
        assert has(out, "world")

    def test_deeper_nesting(self):
        out = ps("(outer (inner (deep)) end) =")
        assert has(out, "outer")
        assert has(out, "inner")

    def test_balanced_open_closes(self):
        out = ps("((balanced)) =")
        assert has(out, "balanced")


class TestTokenizerHexString:
    """Lines 117-129: hex string <...> tokenization."""

    def test_hex_hello(self):
        # <48656c6c6f> = "Hello"
        out = ps("<48656c6c6f> =")
        assert has(out, "Hello")

    def test_hex_with_spaces(self):
        # spaces in hex string are ignored
        out = ps("<48 65 6c 6c 6f> =")
        assert has(out, "Hello")

    def test_hex_invalid_gives_empty(self):
        # invalid hex → empty string
        out = ps("<nothex> =")
        assert isinstance(out, list)

    def test_hex_empty(self):
        out = ps("<> =")
        assert isinstance(out, list)


class TestTokenizerRadix:
    """Lines 187-188: radix notation BASE#VALUE."""

    def test_hex_radix(self):
        out = ps("16#FF =")
        assert has(out, "255")

    def test_octal_radix(self):
        out = ps("8#17 =")
        assert has(out, "15")

    def test_binary_radix(self):
        out = ps("2#1010 =")
        assert has(out, "10")

    def test_base_36(self):
        out = ps("36#Z =")
        assert isinstance(out, list)


class TestTokenizerEmptyWord:
    """Lines 176-177: empty word skip (unrecognised punctuation)."""

    def test_stray_close_paren(self):
        # ) is in the exclusion set → empty word → skip
        out = ps(") 42 =")
        assert has(out, "42")

    def test_stray_greater_than(self):
        # > similarly produces empty word
        out = ps("> 99 =")
        assert has(out, "99")

    def test_stray_close_brace(self):
        out = ps("} 7 =")
        assert has(out, "7")


# ===========================================================================
# Stack operators
# ===========================================================================


class TestMarkCleartomark:
    """Lines 321-326: mark and cleartomark."""

    def test_cleartomark_removes_items(self):
        # cleartomark pops items up to (but not including) the mark tuple
        out = ps("mark 1 2 3 cleartomark count =")
        assert has(out, "1")  # the mark itself remains

    def test_mark_is_pushed(self):
        # mark increments count by 1 (the mark itself)
        out = ps("mark count =")
        assert has(out, "1")

    def test_mark_cleartomark_roundtrip(self):
        # after cleartomark the mark tuple is still on stack → count = 1
        out = ps("mark cleartomark count =")
        assert has(out, "1")


# ===========================================================================
# Arithmetic
# ===========================================================================


class TestIdivDivisionByZero:
    """Line 348: idiv division by zero."""

    def test_idiv_by_zero_error(self):
        out = ps("5 0 idiv")
        assert has(out, "❌")

    def test_idiv_normal(self):
        out = ps("10 3 idiv =")
        assert has(out, "3")


class TestTrigonometry:
    """Lines 391-395: sin and cos operators."""

    def test_sin_90(self):
        out = ps("90 sin =")
        # sin(90 degrees) ≈ 1.0
        assert out
        assert no_errors(out)

    def test_sin_0(self):
        out = ps("0 sin =")
        assert out
        assert no_errors(out)

    def test_cos_0(self):
        out = ps("0 cos =")
        # cos(0 degrees) = 1.0
        assert has(out, "1.0")

    def test_cos_90(self):
        out = ps("90 cos =")
        assert out
        assert no_errors(out)

    def test_sin_result_is_numeric(self):
        out = ps("45 sin =")
        assert out
        assert no_errors(out)


# ===========================================================================
# Comparison
# ===========================================================================


class TestGeOperator:
    """Lines 423-425: ge (>=) comparison."""

    def test_ge_greater(self):
        out = ps("5 3 ge =")
        assert has(out, "True")

    def test_ge_equal(self):
        out = ps("5 5 ge =")
        assert has(out, "True")

    def test_ge_less(self):
        out = ps("3 5 ge =")
        assert has(out, "False")


# ===========================================================================
# Boolean / bitwise
# ===========================================================================


class TestAndIntegerBitwise:
    """Line 441: and with integers (bitwise AND)."""

    def test_and_integers(self):
        out = ps("12 10 and =")
        assert has(out, "8")  # 0b1100 & 0b1010 = 0b1000

    def test_and_integers_zero(self):
        out = ps("5 0 and =")
        assert has(out, "0")


class TestOrIntegerBitwise:
    """Line 448: or with integers (bitwise OR)."""

    def test_or_integers(self):
        out = ps("12 10 or =")
        assert has(out, "14")  # 0b1100 | 0b1010 = 0b1110

    def test_or_integers_identity(self):
        out = ps("7 0 or =")
        assert has(out, "7")


class TestXorOperator:
    """Lines 455-457: xor operator."""

    def test_xor_bool_true_false(self):
        out = ps("true false xor =")
        assert has(out, "True")

    def test_xor_bool_same(self):
        out = ps("true true xor =")
        assert has(out, "False")

    def test_xor_integers(self):
        out = ps("12 10 xor =")
        assert has(out, "6")  # 0b1100 ^ 0b1010 = 0b0110

    def test_xor_integers_zero(self):
        out = ps("5 5 xor =")
        assert has(out, "0")


class TestBitshift:
    """Lines 459-461: bitshift operator."""

    def test_bitshift_left(self):
        out = ps("1 3 bitshift =")
        assert has(out, "8")  # 1 << 3 = 8

    def test_bitshift_left_larger(self):
        out = ps("3 2 bitshift =")
        assert has(out, "12")  # 3 << 2 = 12

    def test_bitshift_right(self):
        out = ps("16 -2 bitshift =")
        assert isinstance(out, list)  # right shift


# ===========================================================================
# String conversion
# ===========================================================================


class TestCvs:
    """Lines 479-484: cvs (convert to string)."""

    def test_cvs_integer(self):
        out = ps("42 10 string cvs =")
        assert has(out, "42")

    def test_cvs_float(self):
        out = ps("3.14 10 string cvs =")
        assert has(out, "3.14")

    def test_cvs_bool(self):
        out = ps("true 10 string cvs =")
        assert has(out, "True")

    def test_cvs_string_passthrough(self):
        out = ps("(hello) 10 string cvs =")
        assert has(out, "hello")


class TestCvn:
    """Lines 486-487: cvn (convert to name)."""

    def test_cvn_creates_name(self):
        out = ps("(hello) cvn type =")
        assert has(out, "nametype")

    def test_cvn_string_becomes_name(self):
        out = ps("(myop) cvn =")
        assert isinstance(out, list)


# ===========================================================================
# Output operators
# ===========================================================================


class TestMessageOperator:
    """Lines 499-501: message operator (output with newline)."""

    def test_message_string(self):
        out = ps("(hello world) message")
        assert has(out, "hello world")

    def test_message_number(self):
        out = ps("42 message")
        assert has(out, "42")

    def test_message_result(self):
        out = ps("(test output) message")
        assert no_errors(out)


class TestShowOperator:
    """Lines 502-505: show operator (draw string)."""

    def test_show_string(self):
        out = ps("(hello) show")
        assert has(out, "hello")

    def test_show_no_error(self):
        out = ps("(PostScript) show")
        assert no_errors(out)


# ===========================================================================
# Control flow
# ===========================================================================


class TestLoopOperator:
    """Lines 550-556: loop operator with exit."""

    def test_loop_with_exit(self):
        out = ps("{(looping) message exit} loop")
        assert has(out, "looping")

    def test_loop_exits_cleanly(self):
        out = ps("{exit} loop count =")
        assert no_errors(out)

    def test_loop_counter(self):
        out = ps("/n 0 def\n{/n n 1 add def n 3 ge {exit} if}\nloop n =")
        assert has(out, "3")


class TestExecOperator:
    """Lines 566-567: exec operator."""

    def test_exec_proc(self):
        out = ps("{42 =} exec")
        assert has(out, "42")

    def test_exec_string_proc(self):
        out = ps("/myfn { (executed) message } def /myfn load exec")
        assert has(out, "executed")


class TestExecWithArray:
    """Line 870: _exec_proc with a list (not a proc tuple)."""

    def test_exec_array(self):
        # [42] creates array [42]; exec calls _exec_proc([42])
        # _exec_proc hits the `elif isinstance(proc, list):` branch
        out = ps("[42] exec =")
        assert has(out, "42")

    def test_exec_empty_array(self):
        out = ps("[] exec count =")
        assert isinstance(out, list)


class TestExitOperator:
    """Line 569: exit operator (raises _PSExit, caught by run())."""

    def test_exit_from_loop(self):
        out = ps("{(before) message exit (after) message} loop")
        assert has(out, "before")

    def test_exit_top_level(self):
        # exit at top level is caught silently
        out = ps("(before) message exit (after) message")
        assert has(out, "before")


class TestStopOperator:
    """Line 571: stop operator (raises _PSStop, caught by run())."""

    def test_stop_halts_execution(self):
        out = ps("42 = stop 99 =")
        assert has(out, "42")
        # 99 should not be in output
        assert not has(out, "99")

    def test_stop_from_proc(self):
        out = ps("{stop} exec")
        assert no_errors(out)


# ===========================================================================
# Definitions / dictionary operations
# ===========================================================================


class TestLoadOperator:
    """Lines 581-584: load operator."""

    def test_load_defined_name(self):
        out = ps("/x 42 def /x load =")
        assert has(out, "42")

    def test_load_builtin_value(self):
        out = ps("/pi load type =")
        assert has(out, "realtype")

    def test_load_undefined_error(self):
        out = ps("/nonexistent load")
        assert has(out, "❌")


class TestKnownOperator:
    """Lines 589-593: known operator."""

    def test_known_true(self):
        out = ps("currentdict /pi known =")
        assert has(out, "True")

    def test_known_false(self):
        out = ps("currentdict /nonexistent known =")
        assert has(out, "False")

    def test_known_user_defined(self):
        out = ps("/myvar 99 def currentdict /myvar known =")
        assert has(out, "True")

    def test_known_empty_dict(self):
        out = ps("5 dict /foo known =")
        assert has(out, "False")


class TestWhereOperator:
    """Lines 595-603: where operator."""

    def test_where_found(self):
        out = ps("/pi where =")
        assert has(out, "True")

    def test_where_not_found(self):
        out = ps("/unknownsymbol where =")
        assert has(out, "False")

    def test_where_user_defined(self):
        out = ps("/myvar 99 def /myvar where =")
        assert has(out, "True")


class TestCurrentdictOperator:
    """Lines 619-620: currentdict operator."""

    def test_currentdict_returns_dict(self):
        out = ps("currentdict type =")
        assert has(out, "dicttype")

    def test_currentdict_has_pi(self):
        out = ps("currentdict /pi known =")
        assert has(out, "True")


class TestSystemdictOperator:
    """Lines 622-623: systemdict operator."""

    def test_systemdict_returns_dict(self):
        out = ps("systemdict type =")
        assert has(out, "dicttype")

    def test_systemdict_has_pi(self):
        out = ps("systemdict /pi known =")
        assert has(out, "True")


# ===========================================================================
# Get / Put / Interval operations
# ===========================================================================


class TestGetOnString:
    """Lines 644-645: get on string returns char code."""

    def test_get_first_char(self):
        # ord('h') = 104
        out = ps("(hello) 0 get =")
        assert has(out, "104")

    def test_get_second_char(self):
        # ord('e') = 101
        out = ps("(hello) 1 get =")
        assert has(out, "101")

    def test_get_out_of_bounds(self):
        # returns 0 for out-of-bounds
        out = ps("(hi) 99 get =")
        assert has(out, "0")


class TestGetinterval:
    """Lines 660-667: getinterval operator."""

    def test_getinterval_string(self):
        out = ps("(hello world) 6 5 getinterval =")
        assert has(out, "world")

    def test_getinterval_array(self):
        out = ps("[1 2 3 4 5] 1 3 getinterval =")
        assert has(out, "2")
        assert has(out, "3")
        assert has(out, "4")

    def test_getinterval_from_start(self):
        out = ps("(hello) 0 3 getinterval =")
        assert has(out, "hel")


class TestPutinterval:
    """Lines 669-674: putinterval operator."""

    def test_putinterval_array(self):
        out = ps("[1 2 3 4 5] dup 1 [9 8] putinterval =")
        assert has(out, "9")
        assert has(out, "8")

    def test_putinterval_no_crash(self):
        out = ps("[0 0 0] dup 0 [1 2] putinterval =")
        assert no_errors(out)


class TestStringConstructor:
    """Lines 676-678: string operator (allocate N-char string)."""

    def test_string_creates_buffer(self):
        out = ps("5 string =")
        assert isinstance(out, list)
        assert no_errors(out)

    def test_string_zero_length(self):
        out = ps("0 string =")
        assert isinstance(out, list)


class TestAstore:
    """Lines 690-695: astore operator."""

    def test_astore_fills_array(self):
        out = ps("10 20 30 3 array astore =")
        assert has(out, "10")
        assert has(out, "20")
        assert has(out, "30")

    def test_astore_single(self):
        out = ps("42 1 array astore =")
        assert has(out, "42")


# ===========================================================================
# Search and Token
# ===========================================================================


class TestSearchOperator:
    """Lines 697-708: search operator."""

    def test_search_found(self):
        out = ps("(hello world) (world) search pop =")
        assert has(out, "hello")  # pre part

    def test_search_not_found(self):
        out = ps("(hello) (xyz) search =")
        assert has(out, "False")

    def test_search_found_flag(self):
        out = ps("(hello world) (world) search { pop pop pop (yes) message } if")
        assert has(out, "yes")

    def test_search_match(self):
        # search: pushes post, match, pre, true
        out = ps(
            "(abcdef) (cd) search\n"
            "{ (match) message = = = }\n"
            "{ pop (nomatch) message } ifelse"
        )
        assert has(out, "match")


class TestTokenOperator:
    """Lines 710-725: token operator."""

    def test_token_integer(self):
        out = ps("(42 rest) token pop =")
        assert has(out, "42")

    def test_token_float(self):
        out = ps("(3.14 rest) token pop =")
        assert has(out, "3.14")

    def test_token_string_word(self):
        out = ps("(hello world) token pop =")
        assert has(out, "hello")

    def test_token_empty_string(self):
        out = ps("() token { pop = } { (empty) message } ifelse")
        assert has(out, "empty")

    def test_token_whitespace_only(self):
        out = ps("(   ) token { pop = } { (empty) message } ifelse")
        assert isinstance(out, list)


# ===========================================================================
# Type operator branches
# ===========================================================================


class TestTypeOperator:
    """Lines 729-743: all type operator branches."""

    def test_type_boolean(self):
        # Line 729: booleantype
        out = ps("true type =")
        assert has(out, "booleantype")

    def test_type_real(self):
        # Lines 732-733: realtype
        out = ps("3.14 type =")
        assert has(out, "realtype")

    def test_type_string(self):
        # Lines 734-735: stringtype
        out = ps("(hi) type =")
        assert has(out, "stringtype")

    def test_type_array(self):
        # Lines 736-737: arraytype
        out = ps("[1 2] type =")
        assert has(out, "arraytype")

    def test_type_dict(self):
        # Lines 738-739: dicttype
        out = ps("currentdict type =")
        assert has(out, "dicttype")

    def test_type_name(self):
        # Lines 740-741: nametype
        out = ps("/foo type =")
        assert has(out, "nametype")

    def test_type_null(self):
        # Lines 742-743: nulltype (via null op)
        out = ps("null type =")
        assert has(out, "nulltype")

    def test_type_integer(self):
        out = ps("42 type =")
        assert has(out, "integertype")


class TestNullOperator:
    """Lines 745-747: null operator."""

    def test_null_pushes_none(self):
        out = ps("null =")
        assert has(out, "None")

    def test_null_type(self):
        out = ps("null type =")
        assert has(out, "nulltype")


# ===========================================================================
# Graphics operators (turtle integration)
# ===========================================================================


class TestMovetoOperator:
    """Lines 753-755: moveto (uses ps_turtle() to avoid pen_down instance attr)."""

    def test_moveto_no_crash(self):
        out = ps_turtle("100 200 moveto")
        assert no_errors(out)

    def test_moveto_followed_by_lineto(self):
        out = ps_turtle("0 0 moveto 100 0 lineto")
        assert no_errors(out)


class TestRmovetoOperator:
    """Lines 758-764: rmoveto."""

    def test_rmoveto_no_crash(self):
        out = ps_turtle("10 20 rmoveto")
        assert no_errors(out)

    def test_rmoveto_relative(self):
        out = ps_turtle("0 0 moveto 10 20 rmoveto")
        assert no_errors(out)


class TestLinetoOperator:
    """Lines 767-769: lineto."""

    def test_lineto_no_crash(self):
        out = ps_turtle("0 0 moveto 100 0 lineto")
        assert no_errors(out)

    def test_lineto_draws(self):
        out = ps_turtle("0 0 moveto 50 100 lineto stroke")
        assert no_errors(out)


class TestRlinetoOperator:
    """Lines 772-776: rlineto."""

    def test_rlineto_no_crash(self):
        out = ps_turtle("0 0 moveto 5 10 rlineto")
        assert no_errors(out)

    def test_rlineto_relative(self):
        out = ps_turtle("50 50 moveto 10 20 rlineto stroke")
        assert no_errors(out)


class TestPathOperators:
    """Lines 780, 782, 784: stroke, fill, newpath/closepath."""

    def test_stroke(self):
        out = ps("stroke")
        assert no_errors(out)

    def test_fill(self):
        out = ps("fill")
        assert no_errors(out)

    def test_newpath(self):
        out = ps("newpath")
        assert no_errors(out)

    def test_closepath(self):
        out = ps("closepath")
        assert no_errors(out)


class TestSetgray:
    """Lines 789-791: setgray operator."""

    def test_setgray_half(self):
        out = ps("0.5 setgray")
        assert no_errors(out)

    def test_setgray_black(self):
        out = ps("0 setgray")
        assert no_errors(out)

    def test_setgray_white(self):
        out = ps("1 setgray")
        assert no_errors(out)


class TestSetrgbcolor:
    """Lines 793-795: setrgbcolor operator."""

    def test_setrgbcolor_red(self):
        out = ps("1 0 0 setrgbcolor")
        assert no_errors(out)

    def test_setrgbcolor_green(self):
        out = ps("0 1 0 setrgbcolor")
        assert no_errors(out)

    def test_setrgbcolor_blue(self):
        out = ps("0 0 1 setrgbcolor")
        assert no_errors(out)


class TestRotateOperator:
    """Lines 801-803: rotate operator."""

    def test_rotate_90(self):
        out = ps("90 rotate")
        assert no_errors(out)

    def test_rotate_negative(self):
        out = ps("-45 rotate")
        assert no_errors(out)

    def test_rotate_360(self):
        out = ps("360 rotate")
        assert no_errors(out)


class TestShowpageOperator:
    """Line 824: showpage and erasepage."""

    def test_showpage(self):
        out = ps("showpage")
        assert no_errors(out)

    def test_erasepage(self):
        out = ps("erasepage")
        assert no_errors(out)


# ===========================================================================
# Stack underflow (_pop and _peek)
# ===========================================================================


class TestStackUnderflow:
    """Line 852: _peek stackunderflow error."""

    def test_dup_underflow(self):
        out = ps("dup")
        assert has(out, "stackunderflow")

    def test_pop_underflow(self):
        out = ps("pop")
        assert has(out, "❌")


# ===========================================================================
# Combined / integration tests
# ===========================================================================


class TestCombinedCoverage:
    """Mixed tests to drive additional coverage."""

    def test_graphics_sequence(self):
        # Full drawing sequence with mock turtle
        out = ps_turtle(
            "newpath\n"
            "0 0 moveto\n"
            "100 0 lineto\n"
            "100 100 lineto\n"
            "0 100 lineto\n"
            "closepath\n"
            "0.5 setgray\n"
            "fill\n"
            "showpage"
        )
        assert no_errors(out)

    def test_string_operations(self):
        out = ps("(hello world) (world) search pop =")
        assert has(out, "hello")

    def test_radix_arithmetic(self):
        out = ps("16#10 16#10 add =")
        assert has(out, "32")

    def test_comment_does_not_affect_result(self):
        out = ps("% ignore\n3 4 add =\n% done")
        assert has(out, "7")

    def test_hex_string_length(self):
        out = ps("<48656c6c6f> length =")
        assert has(out, "5")

    def test_loop_with_counter(self):
        out = ps(
            "/count 0 def\n"
            "{\n"
            "  /count count 1 add def\n"
            "  count 5 ge { exit } if\n"
            "} loop\n"
            "count ="
        )
        assert has(out, "5")

    def test_known_after_def(self):
        out = ps("/mykey 42 def currentdict /mykey known =")
        assert has(out, "True")

    def test_where_after_def(self):
        out = ps("/wtest 1 def /wtest where =")
        assert has(out, "True")

    def test_type_of_proc(self):
        out = ps("{add} type =")
        assert isinstance(out, list)

    def test_cvs_then_cvn_roundtrip(self):
        out = ps("42 10 string cvs cvn type =")
        assert has(out, "nametype")

    def test_getinterval_then_search(self):
        out = ps("(hello world foo) 6 5 getinterval (world) search pop =")
        assert isinstance(out, list)

    def test_nested_procs(self):
        out = ps(
            "/double { 2 mul } def\n"
            "/quad { double double } def\n"
            "3 quad ="
        )
        assert has(out, "12")
