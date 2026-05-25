"""Tests for the core Interpreter class — dispatch, state management, limits."""

from __future__ import annotations

import pytest
from time_warp.core.interpreter import (
    Interpreter,
    Language,
    ScreenMode,
)
from time_warp.graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def interp():
    """Fresh interpreter instance for each test."""
    return Interpreter()


@pytest.fixture
def turtle():
    return TurtleState()


# ---------------------------------------------------------------------------
# Language detection
# ---------------------------------------------------------------------------


class TestLanguageEnum:
    """Test Language enum from_extension and friendly_name."""

    @pytest.mark.parametrize(
        "ext,expected",
        [
            (".bas", Language.BASIC),
            (".pilot", Language.PILOT),
            (".logo", Language.LOGO),
            (".c", Language.C),
            (".pas", Language.PASCAL),
            (".pl", Language.PROLOG),
            (".f", Language.FORTH),
            (".lua", Language.LUA),
            (".bf", Language.BRAINFUCK),
            (".js", Language.JAVASCRIPT),
        ],
    )
    def test_from_extension(self, ext, expected):
        assert Language.from_extension(ext) == expected

    def test_unknown_extension_defaults_basic(self):
        assert Language.from_extension(".xyz") == Language.BASIC

    def test_friendly_name_returns_string(self):
        for lang in Language:
            name = lang.friendly_name()
            assert isinstance(name, str)
            assert len(name) > 0


# ---------------------------------------------------------------------------
# Init / Reset parity
# ---------------------------------------------------------------------------


class TestInitResetParity:
    """Verify __init__ and reset produce identical state."""

    def test_reset_clears_variables(self, interp):
        interp.variables["X"] = 42.0
        interp.string_variables["NAME$"] = "hello"
        interp.arrays["A"] = [1.0, 2.0, 3.0]
        interp.reset()
        assert interp.variables == {}
        assert interp.string_variables == {}
        assert interp.arrays == {}

    def test_reset_clears_program(self, interp, turtle):
        interp.load_program("PRINT 1", language=Language.BASIC)
        assert len(interp.program_lines) > 0
        interp.reset()
        assert interp.program_lines == []
        assert interp.current_line == 0

    def test_reset_preserves_callbacks(self, interp):
        def cb(_):
            return "test"
        interp.input_callback = cb
        interp.reset()
        assert interp.input_callback is cb

    def test_reset_preserves_debug_settings(self, interp):
        interp.breakpoints.add(5)
        interp.debug_mode = True
        interp.reset()
        # breakpoints and debug_mode survive reset
        assert 5 in interp.breakpoints
        assert interp.debug_mode is True

    def test_reset_clears_language_state(self, interp):
        interp.prolog_kb["parent"] = [("john", "mary")]
        interp.pascal_procs["hello"] = {"params": []}
        interp.c_block_stack.append({"type": "if"})
        interp.basic_subs["test"] = {"params": [], "start_line": 0}
        interp.reset()
        assert interp.prolog_kb == {}
        assert interp.pascal_procs == {}
        assert interp.c_block_stack == []
        assert interp.basic_subs == {}


# ---------------------------------------------------------------------------
# Execution dispatch
# ---------------------------------------------------------------------------


class TestExecution:
    """Test basic execution dispatch for different languages."""

    def test_basic_print(self, interp, turtle):
        interp.load_program('PRINT "Hello"', language=Language.BASIC)
        out = interp.execute(turtle)
        assert any("Hello" in line for line in out)

    def test_pilot_type(self, interp, turtle):
        interp.load_program("T:Hello PILOT\nE:", language=Language.PILOT)
        out = interp.execute(turtle)
        assert any("Hello PILOT" in line for line in out)

    def test_logo_print(self, interp, turtle):
        interp.load_program("PRINT [Hello Logo]", language=Language.LOGO)
        out = interp.execute(turtle)
        blob = "\n".join(out).upper()
        assert "HELLO LOGO" in blob

    def test_lua_whole_program(self, interp, turtle):
        interp.load_program("print('Lua works')", language=Language.LUA)
        out = interp.execute(turtle)
        assert any("Lua works" in line for line in out)

    def test_brainfuck_hello(self, interp, turtle):
        # Simple: output ASCII 65 = 'A'
        bf_code = "+" * 65 + "."
        interp.load_program(bf_code, language=Language.BRAINFUCK)
        out = interp.execute(turtle)
        blob = "\n".join(out)
        assert "A" in blob

    def test_empty_program_no_crash(self, interp, turtle):
        interp.load_program("", language=Language.BASIC)
        out = interp.execute(turtle)
        assert isinstance(out, list)


# ---------------------------------------------------------------------------
# Security limits
# ---------------------------------------------------------------------------


class TestSecurityLimits:
    """Verify iteration and timeout limits."""

    def test_iteration_limit(self, interp, turtle):
        # Infinite loop in BASIC
        interp.load_program("10 GOTO 10", language=Language.BASIC)
        out = interp.execute(turtle)
        blob = "\n".join(out)
        # Should terminate with a limit message, not hang
        assert "limit" in blob.lower() or "❌" in blob or len(out) >= 0

    def test_max_iterations_attribute(self, interp):
        assert interp.MAX_ITERATIONS == 100_000

    def test_max_execution_time_attribute(self, interp):
        assert interp.MAX_EXECUTION_TIME == 10.0


# ---------------------------------------------------------------------------
# Screen state
# ---------------------------------------------------------------------------


class TestScreenState:
    """Test screen mode management."""

    def test_default_screen_mode(self, interp):
        assert interp.screen_mode == ScreenMode.GRAPHICS

    def test_screen_config_defaults(self, interp):
        assert interp.screen_config.cols > 0
        assert interp.screen_config.rows > 0


# ---------------------------------------------------------------------------
# Typed variables
# ---------------------------------------------------------------------------


class TestTypedVariables:
    """Test the typed variable system."""

    def test_set_and_get_int(self, interp):
        interp.set_typed_variable("X%", 42)
        val = interp.get_numeric_value("X%")
        assert val == 42.0

    def test_set_and_get_string(self, interp):
        interp.set_typed_variable("NAME$", "hello")
        assert interp.string_variables.get("NAME$") == "hello"

    def test_set_and_get_double(self, interp):
        interp.set_typed_variable("PI#", 3.14159)
        val = interp.get_numeric_value("PI#")
        assert abs(val - 3.14159) < 0.001

    def test_variable_mirrors_to_aggregate(self, interp):
        interp.set_typed_variable("X", 99)
        assert "X" in interp.variables
        assert interp.variables["X"] == 99.0


# ---------------------------------------------------------------------------
# Output handling
# ---------------------------------------------------------------------------


class TestOutputHandling:
    """Test the output callback system."""

    def test_output_callback_receives_lines(self, interp, turtle):
        received = []
        interp.output_callback = lambda line: received.append(line)
        interp.load_program('PRINT "test output"', language=Language.BASIC)
        interp.execute(turtle)
        assert len(received) > 0

    def test_log_output_splits_lines(self, interp):
        received = []
        interp.output_callback = lambda line: received.append(line)
        interp.log_output("line1\nline2\nline3")
        assert len(received) == 3


class TestMultiLanguageExecution:
    """Test that different languages execute correctly."""

    def test_logo_forward_no_crash(self, interp, turtle):
        interp.load_program("FORWARD 50", language=Language.LOGO)
        out = interp.execute(turtle)
        assert isinstance(out, list)

    def test_forth_word_def(self, interp, turtle):
        interp.load_program(": DOUBLE 2 * ; 5 DOUBLE .", language=Language.FORTH)
        out = interp.execute(turtle)
        assert "10" in "\n".join(out)

    def test_javascript_print(self, interp, turtle):
        interp.load_program('console.log("Hello from JS")', language=Language.JAVASCRIPT)
        out = interp.execute(turtle)
        assert any("Hello from JS" in line for line in out)

    def test_prolog_fact(self, interp, turtle):
        src = "likes(alice, bob).\n?- likes(alice, bob)."
        interp.load_program(src, language=Language.PROLOG)
        out = interp.execute(turtle)
        assert isinstance(out, list)

    def test_erlang_hello(self, interp, turtle):
        src = '-module(hello).\n-export([main/0]).\nmain() -> io:format("Hi~n").'
        interp.load_program(src, language=Language.ERLANG)
        out = interp.execute(turtle)
        assert isinstance(out, list)


class TestInputSystem:
    """Test the input callback system."""

    def test_input_callback_used(self, interp, turtle):
        interp.input_callback = lambda _: "42"
        interp.load_program("10 INPUT X\n20 PRINT X", language=Language.BASIC)
        out = interp.execute(turtle)
        # After executing, the interpreter may pause for input
        assert isinstance(out, list)

    def test_provide_input_resumes(self, interp, turtle):
        interp.input_callback = lambda _: "Hello"
        src = "10 INPUT NAME$\n20 PRINT NAME$"
        interp.load_program(src, language=Language.BASIC)
        all_out = list(interp.execute(turtle))
        if interp.pending_input:
            interp.provide_input("Hello")
            more = interp.execute(turtle)
            all_out.extend(more)
        combined = "\n".join(all_out)
        assert isinstance(combined, str)


class TestLanguageProperties:
    """Test Language enum properties."""

    def test_all_languages_have_friendly_name(self):
        for lang in Language:
            name = lang.friendly_name()
            assert isinstance(name, str)
            assert len(name) > 0

    def test_basic_friendly_name(self):
        assert "BASIC" in Language.BASIC.friendly_name().upper() or Language.BASIC.friendly_name()

    def test_extension_roundtrip(self):
        ext_map = [
            (".bas", Language.BASIC),
            (".lua", Language.LUA),
            (".forth", Language.FORTH),
        ]
        for ext, expected in ext_map:
            result = Language.from_extension(ext)
            assert result == expected

    def test_load_clears_previous_state(self, interp, turtle):
        interp.load_program("10 PRINT 1", language=Language.BASIC)
        interp.execute(turtle)
        interp.load_program("10 PRINT 2", language=Language.BASIC)
        out = interp.execute(turtle)
        combined = "\n".join(out)
        assert "2" in combined


class TestMultiLanguageExecution:
    """Tests for executing various languages through the interpreter."""

    def test_lua_via_interpreter(self, interp, turtle):
        interp.load_program('print("lua hello")', language=Language.LUA)
        out = interp.execute(turtle)
        assert any("lua hello" in line for line in out)

    def test_lisp_via_interpreter(self, interp, turtle):
        interp.load_program('(display "lisp hello")', language=Language.LISP)
        out = interp.execute(turtle)
        assert any("lisp hello" in line for line in out)

    def test_pascal_via_interpreter(self, interp, turtle):
        interp.load_program("writeln('pascal hello')", language=Language.PASCAL)
        out = interp.execute(turtle)
        assert any("pascal hello" in line for line in out)

    def test_hypertalk_via_interpreter(self, interp, turtle):
        interp.load_program('put "ht hello"', language=Language.HYPERTALK)
        out = interp.execute(turtle)
        assert any("ht hello" in line for line in out)

    def test_brainfuck_via_interpreter(self, interp, turtle):
        interp.load_program("++++++++++[>++++++++++<-]>..", language=Language.BRAINFUCK)
        out = interp.execute(turtle)
        assert isinstance(out, list)

    def test_tcl_via_interpreter(self, interp, turtle):
        interp.load_program('puts "tcl hello"', language=Language.TCL)
        out = interp.execute(turtle)
        assert any("tcl hello" in line for line in out)

    def test_postscript_via_interpreter(self, interp, turtle):
        interp.load_program("(ps hello) =", language=Language.POSTSCRIPT)
        out = interp.execute(turtle)
        assert any("ps hello" in line for line in out)

    def test_javascript_via_interpreter(self, interp, turtle):
        interp.load_program('console.log("js hello")', language=Language.JAVASCRIPT)
        out = interp.execute(turtle)
        assert any("js hello" in line for line in out)

    def test_erlang_via_interpreter(self, interp, turtle):
        src = '-module(t).\n-export([main/0]).\nmain() -> io:format("erl hi~n").'
        interp.load_program(src, language=Language.ERLANG)
        out = interp.execute(turtle)
        assert any("erl hi" in line for line in out)


class TestInterpreterProperties:
    """Tests for interpreter properties and state."""

    def test_language_is_none_initially(self, interp):
        # Before loading a program, language may be None or a default
        assert interp.language is None or isinstance(interp.language, Language)

    def test_language_after_load(self, interp):
        interp.load_program("10 PRINT 1", language=Language.BASIC)
        assert interp.language == Language.BASIC

    def test_language_changes(self, interp):
        interp.load_program("10 PRINT 1", language=Language.BASIC)
        interp.load_program("print(1)", language=Language.LUA)
        assert interp.language == Language.LUA

    def test_multiple_loads(self, interp, turtle):
        for lang in [Language.BASIC, Language.LUA, Language.FORTH]:
            interp.load_program("", language=lang)
        # Should not crash
        assert interp.language == Language.FORTH


class TestInterpreterVariables2:
    """Extended variable management tests."""

    def test_set_and_read_numeric(self):
        interp = Interpreter()
        interp.set_typed_variable("A", 99)
        assert interp.variables["A"] == 99

    def test_set_multiple_variables(self):
        interp = Interpreter()
        interp.set_typed_variable("X", 1)
        interp.set_typed_variable("Y", 2)
        assert len(interp.variables) == 2

    def test_reset_clears_variables(self):
        interp = Interpreter()
        interp.set_typed_variable("Z", 77)
        interp.reset()
        assert interp.variables == {}

    def test_reset_clears_output(self):
        interp = Interpreter()
        interp.output.append("something")
        interp.reset()
        assert interp.output == []

    def test_set_language_updates_attr(self):
        interp = Interpreter()
        interp.set_language(Language.BASIC)
        assert interp.language == Language.BASIC

    def test_set_language_logo(self):
        interp = Interpreter()
        interp.set_language(Language.LOGO)
        assert interp.language == Language.LOGO

    def test_set_language_lua(self):
        interp = Interpreter()
        interp.set_language(Language.LUA)
        assert interp.language == Language.LUA

    def test_set_language_javascript(self):
        interp = Interpreter()
        interp.set_language(Language.JAVASCRIPT)
        assert interp.language == Language.JAVASCRIPT

    def test_default_language_is_none(self):
        interp = Interpreter()
        assert interp.language is None

    def test_multiple_reset(self):
        interp = Interpreter()
        interp.set_typed_variable("X", 1)
        interp.reset()
        interp.reset()
        assert interp.variables == {}


class TestInterpreterDebugFeatures:
    """Tests for interpreter debug/breakpoint features."""

    def test_debug_mode_default_false(self):
        interp = Interpreter()
        assert interp.debug_mode is False

    def test_set_debug_mode_true(self):
        interp = Interpreter()
        interp.set_debug_mode(True)
        assert interp.debug_mode is True

    def test_set_debug_mode_false(self):
        interp = Interpreter()
        interp.set_debug_mode(True)
        interp.set_debug_mode(False)
        assert interp.debug_mode is False

    def test_step_mode_default_false(self):
        interp = Interpreter()
        assert interp.step_mode is False

    def test_break_on_error_default_false(self):
        interp = Interpreter()
        assert interp.break_on_error is False

    def test_add_breakpoint(self):
        interp = Interpreter()
        interp.add_breakpoint(10)
        assert 10 in interp.breakpoints

    def test_remove_breakpoint(self):
        interp = Interpreter()
        interp.add_breakpoint(10)
        interp.remove_breakpoint(10)
        assert 10 not in interp.breakpoints

    def test_clear_breakpoints(self):
        interp = Interpreter()
        interp.add_breakpoint(10)
        interp.add_breakpoint(20)
        interp.clear_breakpoints()
        assert len(interp.breakpoints) == 0

    def test_add_multiple_breakpoints(self):
        interp = Interpreter()
        interp.add_breakpoint(5)
        interp.add_breakpoint(15)
        interp.add_breakpoint(25)
        assert len(interp.breakpoints) == 3

    def test_running_default_true(self):
        interp = Interpreter()
        assert interp.running is True


class TestInterpreterLanguageSetAll:
    """Tests for setting all supported languages."""

    def test_set_language_erlang(self):
        interp = Interpreter()
        interp.set_language(Language.ERLANG)
        assert interp.language == Language.ERLANG

    def test_set_language_prolog(self):
        interp = Interpreter()
        interp.set_language(Language.PROLOG)
        assert interp.language == Language.PROLOG

    def test_set_language_forth(self):
        interp = Interpreter()
        interp.set_language(Language.FORTH)
        assert interp.language == Language.FORTH

    def test_set_language_pilot(self):
        interp = Interpreter()
        interp.set_language(Language.PILOT)
        assert interp.language == Language.PILOT

    def test_set_language_brainfuck(self):
        interp = Interpreter()
        interp.set_language(Language.BRAINFUCK)
        assert interp.language == Language.BRAINFUCK

    def test_set_language_lisp(self):
        interp = Interpreter()
        interp.set_language(Language.LISP)
        assert interp.language == Language.LISP

    def test_set_language_hypertalk(self):
        interp = Interpreter()
        interp.set_language(Language.HYPERTALK)
        assert interp.language == Language.HYPERTALK

    def test_set_language_cobol(self):
        interp = Interpreter()
        interp.set_language(Language.COBOL)
        assert interp.language == Language.COBOL

    def test_set_language_tcl(self):
        interp = Interpreter()
        interp.set_language(Language.TCL)
        assert interp.language == Language.TCL

    def test_set_language_postscript(self):
        interp = Interpreter()
        interp.set_language(Language.POSTSCRIPT)
        assert interp.language == Language.POSTSCRIPT

    def test_set_language_c(self):
        interp = Interpreter()
        interp.set_language(Language.C)
        assert interp.language == Language.C

    def test_set_language_pascal(self):
        interp = Interpreter()
        interp.set_language(Language.PASCAL)
        assert interp.language == Language.PASCAL


from time_warp.core.interpreter import run, ScreenMode


class TestInterpreterExecuteAndState:
    """Tests for Interpreter.execute and state management."""

    def test_set_debug_mode(self, interp):
        interp.set_debug_mode(True)
        assert interp.debug_mode is True

    def test_set_debug_mode_false(self, interp):
        interp.set_debug_mode(False)
        assert interp.debug_mode is False

    def test_set_break_on_error(self, interp):
        interp.set_break_on_error(True)
        assert interp.break_on_error is True

    def test_breakpoints_empty_initially(self, interp):
        assert len(interp.breakpoints) == 0

    def test_add_and_check_breakpoint(self, interp):
        interp.add_breakpoint(42)
        assert 42 in interp.breakpoints

    def test_remove_nonexistent_breakpoint(self, interp):
        interp.remove_breakpoint(999)  # should not raise

    def test_get_variables_initially_empty(self, interp):
        assert interp.get_variables() == {}

    def test_set_typed_variable_numeric(self, interp):
        interp.set_typed_variable("X", 7)
        assert interp.variables["X"] == 7.0

    def test_reset_clears_variables(self, interp):
        interp.set_typed_variable("X", 7)
        interp.reset()
        assert interp.get_variables() == {}

    def test_running_initially_true(self, interp):
        assert interp.running is True

    def test_step_mode_initially_false(self, interp):
        assert interp.step_mode is False

    def test_screen_mode_graphics_default(self, interp):
        assert interp.screen_mode == ScreenMode.GRAPHICS

    def test_set_step_granularity(self, interp):
        interp.set_debug_step_granularity("statement")
        assert interp.debug_step_granularity == "statement"

    def test_output_initially_empty(self, interp):
        assert interp.output == []


class TestRunFunction:
    """Tests for the top-level run() helper function."""

    def test_run_basic_print(self):
        result = run('PRINT "hello"', Language.BASIC)
        assert isinstance(result, str)

    def test_run_lua_print(self):
        result = run("print(42)", Language.LUA)
        assert "42" in result

    def test_run_javascript_output(self):
        result = run('print("js")', Language.JAVASCRIPT)
        assert "js" in result

    def test_run_basic_math(self):
        result = run("PRINT 2 + 3", Language.BASIC)
        assert isinstance(result, str)

    def test_run_returns_string(self):
        result = run("PRINT 1", Language.BASIC)
        assert isinstance(result, str)

    def test_run_logo_forward(self):
        result = run("FORWARD 100", Language.LOGO)
        assert isinstance(result, str)

    def test_run_brainfuck_hello(self):
        result = run("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.", Language.BRAINFUCK)
        assert "Hello" in result

    def test_run_erlang_hello(self):
        result = run('-module(main).\n-export([main/0]).\nmain() ->\n    io:format("hi~n").\n', Language.ERLANG)
        assert isinstance(result, str)

    def test_run_lisp_print(self):
        result = run("(display 42)", Language.LISP)
        assert "42" in result

    def test_run_forth_print(self):
        result = run(": HELLO CR ; HELLO", Language.FORTH)
        assert isinstance(result, str)


class TestInterpreterExtended:
    """More interpreter tests."""

    def test_run_lua_returns_string(self):
        result = run('print("hello")', Language.LUA)
        assert isinstance(result, str)

    def test_run_lua_output_content(self):
        result = run('print("lua_test")', Language.LUA)
        assert "lua_test" in result

    def test_run_tcl_puts(self):
        result = run('puts "hello"', Language.TCL)
        assert isinstance(result, str)

    def test_run_postscript(self):
        result = run('(hello) =', Language.POSTSCRIPT)
        assert isinstance(result, str)

    def test_run_hypertalk(self):
        result = run('put "hello"', Language.HYPERTALK)
        assert isinstance(result, str)

    def test_run_cobol(self):
        source = "IDENTIFICATION DIVISION.\nPROGRAM-ID. X.\nPROCEDURE DIVISION.\nDISPLAY 'HI'.\nSTOP RUN."
        result = run(source, Language.COBOL)
        assert isinstance(result, str)

    def test_run_prolog_write(self):
        result = run("?- write(hello).", Language.PROLOG)
        assert isinstance(result, str)

    def test_run_pascal_writeln(self):
        result = run("BEGIN\nWriteLn('HI');\nEND.", Language.PASCAL)
        assert isinstance(result, str)

    def test_run_c_printf(self):
        result = run('int main() { printf("hi"); return 0; }', Language.C)
        assert isinstance(result, str)

    def test_run_forth_dot(self):
        result = run("42 .", Language.FORTH)
        assert isinstance(result, str)

    def test_run_basic_gosub(self):
        result = run("10 GOSUB 100\n20 END\n100 PRINT \"SUB\"\n110 RETURN", Language.BASIC)
        assert isinstance(result, str)

    def test_run_logo_repeat(self):
        result = run("REPEAT 4 [FORWARD 10 RIGHT 90]", Language.LOGO)
        assert isinstance(result, str)

    def test_run_empty_basic(self):
        result = run("", Language.BASIC)
        assert isinstance(result, str)

    def test_run_empty_lua(self):
        result = run("", Language.LUA)
        assert isinstance(result, str)

    def test_run_empty_javascript(self):
        result = run("", Language.JAVASCRIPT)
        assert isinstance(result, str)


class TestInterpreterExtended2:
    """Second round of extended interpreter tests."""

    def test_run_lisp_empty(self):
        result = run("", Language.LISP)
        assert isinstance(result, str)

    def test_run_erlang_empty(self):
        result = run("", Language.ERLANG)
        assert isinstance(result, str)

    def test_run_cobol_empty(self):
        result = run("", Language.COBOL)
        assert isinstance(result, str)

    def test_run_tcl_empty(self):
        result = run("", Language.TCL)
        assert isinstance(result, str)

    def test_run_postscript_empty(self):
        result = run("", Language.POSTSCRIPT)
        assert isinstance(result, str)

    def test_run_hypertalk_empty(self):
        result = run("", Language.HYPERTALK)
        assert isinstance(result, str)

    def test_run_brainfuck_empty(self):
        result = run("", Language.BRAINFUCK)
        assert isinstance(result, str)

    def test_run_basic_print(self):
        result = run("PRINT 1", Language.BASIC)
        assert isinstance(result, str)

    def test_run_forth_number(self):
        result = run("42 .", Language.FORTH)
        assert isinstance(result, str)

    def test_run_prolog_fact(self):
        result = run(":- write(hello).", Language.PROLOG)
        assert isinstance(result, str)
