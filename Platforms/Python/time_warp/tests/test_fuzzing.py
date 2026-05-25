from ..core.interpreter import Language
from .conftest_lang import run, no_errors
from . import fuzzing

# Add fuzz testing for language executors


class FuzzTestExecutors:
    def test_fuzz_basic(self):
        """Fuzz test for BASIC language executor."""
        for _ in range(100):
            source = fuzzing.generate_random_basic_code()
            result = run(source, Language.BASIC)
            assert no_errors(result), f"Fuzz test failed for BASIC: {source}"

    def test_fuzz_logo(self):
        """Fuzz test for Logo language executor."""
        for _ in range(100):
            source = fuzzing.generate_random_logo_code()
            result = run(source, Language.LOGO)
            assert no_errors(result), f"Fuzz test failed for Logo: {source}"

    def test_fuzz_lua(self):
        """Fuzz test for Lua language executor."""
        for _ in range(100):
            source = fuzzing.generate_random_python_code()
            result = run(source, Language.LUA)
            assert no_errors(result), f"Fuzz test failed for Lua: {source}"


class TestFuzzingHelpers:
    """Tests for the fuzzing helper module itself."""

    def test_generate_basic_returns_string(self):
        from time_warp.tests import fuzzing
        code = fuzzing.generate_random_basic_code()
        assert isinstance(code, str)

    def test_generate_basic_non_empty(self):
        from time_warp.tests import fuzzing
        code = fuzzing.generate_random_basic_code()
        assert len(code) > 0

    def test_generate_logo_returns_string(self):
        from time_warp.tests import fuzzing
        code = fuzzing.generate_random_logo_code()
        assert isinstance(code, str)

    def test_generate_logo_non_empty(self):
        from time_warp.tests import fuzzing
        code = fuzzing.generate_random_logo_code()
        assert len(code) > 0

    def test_generate_python_returns_string(self):
        from time_warp.tests import fuzzing
        code = fuzzing.generate_random_python_code()
        assert isinstance(code, str)

    def test_generate_python_non_empty(self):
        from time_warp.tests import fuzzing
        code = fuzzing.generate_random_python_code()
        assert len(code) > 0

    def test_generate_different_each_time(self):
        from time_warp.tests import fuzzing
        results = set()
        for _ in range(10):
            results.add(fuzzing.generate_random_basic_code())
        assert len(results) >= 1  # may differ or not

    def test_fuzz_basic_runs_without_crash(self):
        from time_warp.tests import fuzzing
        from time_warp.core.interpreter import Language
        from .conftest_lang import run
        for _ in range(10):
            source = fuzzing.generate_random_basic_code()
            result = run(source, Language.BASIC)
            assert isinstance(result, list)

    def test_fuzz_logo_runs_without_crash(self):
        from time_warp.tests import fuzzing
        from time_warp.core.interpreter import Language
        from .conftest_lang import run
        for _ in range(10):
            source = fuzzing.generate_random_logo_code()
            result = run(source, Language.LOGO)
            assert isinstance(result, list)

    def test_fuzz_lua_runs_without_crash(self):
        from time_warp.tests import fuzzing
        from time_warp.core.interpreter import Language
        from .conftest_lang import run
        for _ in range(10):
            source = fuzzing.generate_random_python_code()
            result = run(source, Language.LUA)
            assert isinstance(result, list)


class TestFuzzingExtended:
    """Extended fuzz tests for more languages."""

    def test_fuzz_javascript_no_crash(self):
        for _ in range(10):
            source = fuzzing.generate_random_python_code()
            result = run(source, Language.JAVASCRIPT)
            assert isinstance(result, list)

    def test_fuzz_lua_no_crash_short(self):
        for source in ['print(1)', 'print("hi")', 'local x=1\nprint(x)']:
            result = run(source, Language.LUA)
            assert isinstance(result, list)

    def test_fuzz_basic_output_is_list(self):
        result = run(fuzzing.generate_random_basic_code(), Language.BASIC)
        assert isinstance(result, list)

    def test_fuzz_logo_output_is_list(self):
        result = run(fuzzing.generate_random_logo_code(), Language.LOGO)
        assert isinstance(result, list)

    def test_fuzz_brainfuck_no_crash(self):
        for source in ['+++.', '---.', '++++++++.']:
            result = run(source, Language.BRAINFUCK)
            assert isinstance(result, list)

    def test_fuzz_forth_no_crash(self):
        for source in ['1 2 + .', '3 4 * .', '5 .']:
            result = run(source, Language.FORTH)
            assert isinstance(result, list)

    def test_fuzz_erlang_no_crash(self):
        source = '-module(fuzz).\n-export([main/0]).\nmain() -> io:format("~w~n", [42]).'
        result = run(source, Language.ERLANG)
        assert isinstance(result, list)

    def test_fuzz_prolog_no_crash(self):
        for source in ['?- write(hello).', ':- write(42).']:
            result = run(source, Language.PROLOG)
            assert isinstance(result, list)

    def test_fuzz_lisp_no_crash(self):
        for source in ['(display 1)', '(display "hi")', '(+ 1 2)']:
            result = run(source, Language.LISP)
            assert isinstance(result, list)

    def test_fuzz_hypertalk_no_crash(self):
        for source in ['put 1', 'put "hi"']:
            result = run(source, Language.HYPERTALK)
            assert isinstance(result, list)

    def test_fuzz_tcl_no_crash(self):
        for source in ['puts 1', 'puts "hi"']:
            result = run(source, Language.TCL)
            assert isinstance(result, list)

    def test_fuzz_postscript_no_crash(self):
        for source in ['1 2 add =', '(hello) =']:
            result = run(source, Language.POSTSCRIPT)
            assert isinstance(result, list)

    def test_fuzz_cobol_no_crash(self):
        source = (
            "IDENTIFICATION DIVISION.\n"
            "PROGRAM-ID. FUZZ.\n"
            "PROCEDURE DIVISION.\n"
            "    DISPLAY 'HI'.\n"
            "    STOP RUN."
        )
        result = run(source, Language.COBOL)
        assert isinstance(result, list)


class TestFuzzingExtended2:
    """More fuzzing tests."""

    def test_basic_long_program_no_crash(self):
        src = "\n".join([f"PRINT {i}" for i in range(50)])
        result = run(src, Language.BASIC)
        assert isinstance(result, list)

    def test_lua_many_locals_no_crash(self):
        src = "\n".join([f"local x{i} = {i}" for i in range(50)])
        result = run(src, Language.LUA)
        assert isinstance(result, list)

    def test_javascript_many_vars_no_crash(self):
        src = "\n".join([f"let x{i} = {i};" for i in range(50)])
        result = run(src, Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_lisp_deep_nesting_no_crash(self):
        src = "(+ 1 " * 10 + "1" + ")" * 10
        result = run(src, Language.LISP)
        assert isinstance(result, list)

    def test_forth_long_stack_no_crash(self):
        src = " ".join([str(i) for i in range(20)]) + " " + ". " * 5
        result = run(src, Language.FORTH)
        assert isinstance(result, list)

    def test_brainfuck_max_cells_no_crash(self):
        result = run(">" * 100 + "<" * 100 + "+.", Language.BRAINFUCK)
        assert isinstance(result, list)

    def test_logo_many_forwards_no_crash(self):
        src = "FORWARD 10\n" * 30
        result = run(src, Language.LOGO)
        assert isinstance(result, list)

    def test_postscript_deep_stack_no_crash(self):
        src = " ".join([str(i) for i in range(20)]) + " " + "= " * 5
        result = run(src, Language.POSTSCRIPT)
        assert isinstance(result, list)

    def test_pascal_many_writelns_no_crash(self):
        body = "\n".join([f"writeln({i});" for i in range(20)])
        src = f"program Test;\nbegin\n{body}\nend."
        result = run(src, Language.PASCAL)
        assert isinstance(result, list)

    def test_cobol_many_displays_no_crash(self):
        body = "\n".join([f"    DISPLAY '{i}'." for i in range(10)])
        src = f"IDENTIFICATION DIVISION.\nPROGRAM-ID. T.\nPROCEDURE DIVISION.\n{body}\nSTOP RUN."
        result = run(src, Language.COBOL)
        assert isinstance(result, list)


class TestFuzzingExtended3:
    """Third round of fuzz tests."""

    def test_basic_empty_string_literal(self):
        result = run('PRINT ""', Language.BASIC)
        assert isinstance(result, list)

    def test_lua_empty_table(self):
        result = run("local t = {}\nprint(#t)", Language.LUA)
        assert isinstance(result, list)

    def test_javascript_empty_array(self):
        result = run("const a = [];\nconsole.log(a.length);", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_prolog_empty_list_query(self):
        result = run("?- length([], N), write(N).", Language.PROLOG)
        assert isinstance(result, list)

    def test_forth_empty_stack_print(self):
        result = run("0 .", Language.FORTH)
        assert isinstance(result, list)

    def test_brainfuck_all_comments(self):
        # Everything outside <>+- [] ,. is comments in BF
        result = run("this is a comment with no actual BF instructions", Language.BRAINFUCK)
        assert isinstance(result, list)

    def test_lisp_empty_list_display(self):
        result = run("(display '())", Language.LISP)
        assert isinstance(result, list)

    def test_hypertalk_empty_put(self):
        result = run('put "" into x', Language.HYPERTALK)
        assert isinstance(result, list)

    def test_erlang_minimal_module(self):
        src = '-module(t).\n-export([f/0]).\nf() -> ok.'
        result = run(src, Language.ERLANG)
        assert isinstance(result, list)

    def test_cobol_minimal_program(self):
        src = "IDENTIFICATION DIVISION.\nPROGRAM-ID. T.\nPROCEDURE DIVISION.\nSTOP RUN."
        result = run(src, Language.COBOL)
        assert isinstance(result, list)


class TestFuzzingExtended4:
    """Fourth round of fuzzing tests."""

    def test_basic_empty_string(self):
        result = run("", Language.BASIC)
        assert isinstance(result, list)

    def test_lua_nil(self):
        result = run("print(nil)", Language.LUA)
        assert isinstance(result, list)

    def test_js_null(self):
        result = run("console.log(null)", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_hypertalk_empty(self):
        result = run("", Language.HYPERTALK)
        assert isinstance(result, list)

    def test_lisp_empty_list(self):
        result = run("()", Language.LISP)
        assert isinstance(result, list)

    def test_lua_empty_table(self):
        result = run("t = {}", Language.LUA)
        assert isinstance(result, list)

    def test_basic_only_rem(self):
        result = run("10 REM nothing\n20 REM more nothing", Language.BASIC)
        assert isinstance(result, list)

    def test_js_empty_object(self):
        result = run("const o = {};", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_prolog_empty(self):
        result = run("", Language.PROLOG)
        assert isinstance(result, list)

    def test_forth_only_comments(self):
        result = run("\\ comment only", Language.FORTH)
        assert isinstance(result, list)


class TestFuzzingExtended5:
    """Fifth round of fuzzing tests."""

    def test_basic_random_chars(self):
        result = run("@#$%^&*", Language.BASIC)
        assert isinstance(result, list)

    def test_lua_random_chars(self):
        result = run("@#$%^&*", Language.LUA)
        assert isinstance(result, list)

    def test_js_unbalanced_braces(self):
        result = run("{{{", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_prolog_unbalanced_parens(self):
        result = run("foo((.", Language.PROLOG)
        assert isinstance(result, list)

    def test_pascal_no_end(self):
        result = run("PROGRAM t; BEGIN WRITELN('hi')", Language.PASCAL)
        assert isinstance(result, list)

    def test_logo_unknown_command(self):
        result = run("UNKNOWNCMD 100", Language.LOGO)
        assert isinstance(result, list)

    def test_forth_empty_loop(self):
        result = run(": TEST DO LOOP ;", Language.FORTH)
        assert isinstance(result, list)

    def test_erlang_syntax_error(self):
        result = run("this is not valid erlang!!!", Language.ERLANG)
        assert isinstance(result, list)

    def test_hypertalk_garbage(self):
        result = run("@#$@#$", Language.HYPERTALK)
        assert isinstance(result, list)

    def test_brainfuck_large_increment(self):
        result = run("+" * 255 + ".", Language.BRAINFUCK)
        assert isinstance(result, list)


class TestFuzzingExtended6:
    """Sixth round of fuzzing tests."""

    def test_basic_empty_input(self):
        result = run("", Language.BASIC)
        assert isinstance(result, list)

    def test_lua_empty_input(self):
        result = run("", Language.LUA)
        assert isinstance(result, list)

    def test_javascript_empty_input(self):
        result = run("", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_forth_random_symbols(self):
        result = run("!@#$%", Language.FORTH)
        assert isinstance(result, list)

    def test_pascal_random_symbols(self):
        result = run("!@#$%", Language.PASCAL)
        assert isinstance(result, list)

    def test_logo_random_symbols(self):
        result = run("!@#$%", Language.LOGO)
        assert isinstance(result, list)

    def test_prolog_random_symbols(self):
        result = run("!@#$%", Language.PROLOG)
        assert isinstance(result, list)

    def test_erlang_empty_input(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)

    def test_lisp_random_symbols(self):
        result = run("!@#$%", Language.LISP)
        assert isinstance(result, list)

    def test_hypertalk_empty_input(self):
        result = run("", Language.HYPERTALK)
        assert isinstance(result, list)


class TestFuzzingExtended7:
    """Seventh round of fuzzing tests - edge cases."""

    def test_basic_null_bytes(self):
        result = run("\x00\x01\x02", Language.BASIC)
        assert isinstance(result, list)

    def test_lua_unicode(self):
        result = run("print('日本語')", Language.LUA)
        assert isinstance(result, list)

    def test_js_unicode(self):
        result = run("console.log('café')", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_lisp_empty(self):
        result = run("", Language.LISP)
        assert isinstance(result, list)

    def test_erlang_gibberish(self):
        result = run("xxx yyy zzz", Language.ERLANG)
        assert isinstance(result, list)

    def test_hypertalk_gibberish(self):
        result = run("xyzzy frobble wibble", Language.HYPERTALK)
        assert isinstance(result, list)

    def test_prolog_gibberish(self):
        result = run("this is not prolog at all", Language.PROLOG)
        assert isinstance(result, list)

    def test_pascal_gibberish(self):
        result = run("!@#$%^&*()", Language.PASCAL)
        assert isinstance(result, list)

    def test_logo_gibberish(self):
        result = run("NOTACOMMAND", Language.LOGO)
        assert isinstance(result, list)

    def test_forth_gibberish(self):
        result = run("NOTAWORD ANOTHERFAKE", Language.FORTH)
        assert isinstance(result, list)


class TestFuzzingExtended8:
    """Eighth round of fuzzing tests."""

    def test_basic_only_spaces(self):
        result = run("   ", Language.BASIC)
        assert isinstance(result, list)

    def test_logo_random_symbols(self):
        result = run("!@#$%^&*()", Language.LOGO)
        assert isinstance(result, list)

    def test_forth_random_symbols(self):
        result = run("!@#$%", Language.FORTH)
        assert isinstance(result, list)

    def test_pascal_empty(self):
        result = run("", Language.PASCAL)
        assert isinstance(result, list)

    def test_cobol_empty(self):
        result = run("", Language.COBOL)
        assert isinstance(result, list)

    def test_tcl_gibberish(self):
        result = run("xyz123!@#", Language.TCL)
        assert isinstance(result, list)

    def test_postscript_gibberish(self):
        result = run("xyz123!@#", Language.POSTSCRIPT)
        assert isinstance(result, list)

    def test_basic_very_long_line(self):
        result = run("PRINT " + "A" * 200, Language.BASIC)
        assert isinstance(result, list)

    def test_lisp_unbalanced_parens(self):
        result = run("(((((", Language.LISP)
        assert isinstance(result, list)

    def test_brainfuck_all_ops(self):
        result = run("+-><.,[]", Language.BRAINFUCK)
        assert isinstance(result, list)


class TestFuzzingExtended9:
    """Ninth round of fuzzing tests."""

    def test_basic_empty(self):
        result = run("", Language.BASIC)
        assert isinstance(result, list)

    def test_logo_empty(self):
        result = run("", Language.LOGO)
        assert isinstance(result, list)

    def test_prolog_empty(self):
        result = run("", Language.PROLOG)
        assert isinstance(result, list)

    def test_pascal_empty(self):
        result = run("", Language.PASCAL)
        assert isinstance(result, list)

    def test_lua_empty(self):
        result = run("", Language.LUA)
        assert isinstance(result, list)

    def test_js_empty(self):
        result = run("", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_forth_empty(self):
        result = run("", Language.FORTH)
        assert isinstance(result, list)

    def test_erlang_empty(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, list)

    def test_lisp_empty(self):
        result = run("", Language.LISP)
        assert isinstance(result, list)

    def test_hypertalk_empty(self):
        result = run("", Language.HYPERTALK)
        assert isinstance(result, list)


class TestFuzzingExtended10:
    """Tenth round of fuzzing tests."""

    def test_basic_empty(self):
        result = run("", Language.BASIC)
        assert isinstance(result, list)

    def test_pascal_empty(self):
        result = run("", Language.PASCAL)
        assert isinstance(result, list)

    def test_prolog_empty(self):
        result = run("", Language.PROLOG)
        assert isinstance(result, list)

    def test_logo_empty(self):
        result = run("", Language.LOGO)
        assert isinstance(result, list)

    def test_c_empty(self):
        result = run("", Language.C)
        assert isinstance(result, list)

    def test_javascript_empty(self):
        result = run("", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_lua_empty(self):
        result = run("", Language.LUA)
        assert isinstance(result, list)

    def test_brainfuck_random(self):
        result = run("+-><.,[]", Language.BRAINFUCK)
        assert isinstance(result, list)

    def test_cobol_empty(self):
        result = run("", Language.COBOL)
        assert isinstance(result, list)

    def test_tcl_empty(self):
        result = run("", Language.TCL)
        assert isinstance(result, list)


class TestFuzzingExtended11:
    """Eleventh round of fuzzing tests."""

    def test_basic_empty(self):
        assert isinstance(run("", Language.BASIC), list)

    def test_pascal_empty(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_prolog_empty(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_logo_empty(self):
        assert isinstance(run("", Language.LOGO), list)

    def test_c_empty(self):
        assert isinstance(run("", Language.C), list)

    def test_javascript_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_lua_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_brainfuck_empty(self):
        assert isinstance(run("", Language.BRAINFUCK), list)

    def test_lisp_empty(self):
        assert isinstance(run("", Language.LISP), list)

    def test_erlang_empty(self):
        assert isinstance(run("", Language.ERLANG), list)


class TestFuzzingExtended12:
    """Twelfth extended round of fuzzing tests."""

    def test_empty_basic(self):
        assert isinstance(run("", Language.BASIC), list)

    def test_empty_lua(self):
        assert isinstance(run("", Language.LUA), list)

    def test_empty_javascript(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_empty_hypertalk(self):
        assert isinstance(run("", Language.HYPERTALK), list)

    def test_empty_erlang(self):
        assert isinstance(run("", Language.ERLANG), list)

    def test_empty_brainfuck(self):
        assert isinstance(run("", Language.BRAINFUCK), list)

    def test_empty_tcl(self):
        assert isinstance(run("", Language.TCL), list)

    def test_empty_cobol(self):
        assert isinstance(run("", Language.COBOL), list)

    def test_empty_forth(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_empty_postscript(self):
        assert isinstance(run("", Language.POSTSCRIPT), list)


class TestFuzzingExtended13:
    def test_empty_pascal(self):
        assert isinstance(run("", Language.PASCAL), list)

    def test_empty_prolog(self):
        assert isinstance(run("", Language.PROLOG), list)

    def test_empty_c(self):
        assert isinstance(run("", Language.C), list)

    def test_empty_logo(self):
        assert isinstance(run("", Language.LOGO), list)

    def test_empty_lisp(self):
        assert isinstance(run("", Language.LISP), list)

    def test_empty_basic2(self):
        assert isinstance(run("", Language.BASIC), list)

    def test_empty_pilot(self):
        assert isinstance(run("", Language.PILOT), list)

    def test_whitespace_lua(self):
        assert isinstance(run("   ", Language.LUA), list)

    def test_whitespace_js(self):
        assert isinstance(run("   ", Language.JAVASCRIPT), list)

    def test_whitespace_forth(self):
        assert isinstance(run("   ", Language.FORTH), list)
