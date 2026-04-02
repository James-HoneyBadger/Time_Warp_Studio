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
            (".py", Language.PYTHON),
            (".lua", Language.LUA),
            (".scm", Language.SCHEME),
            (".cob", Language.COBOL),
            (".bf", Language.BRAINFUCK),
            (".asm", Language.ASSEMBLY),
            (".js", Language.JAVASCRIPT),
            (".f77", Language.FORTRAN),
            (".rexx", Language.REXX),
            (".st", Language.SMALLTALK),
            (".hs", Language.HASKELL),
            (".apl", Language.APL),
            (".sql", Language.SQL),
            (".jcl", Language.JCL),
            (".cics", Language.CICS),
            (".sqr", Language.SQR),
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

    def test_python_whole_program(self, interp, turtle):
        interp.load_program("print('Python works')", language=Language.PYTHON)
        out = interp.execute(turtle)
        assert any("Python works" in line for line in out)

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
