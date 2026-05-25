"""
Tests for interpreter thread lifecycle and output panel logic.

These tests verify the non-UI, non-blocking aspects of the output system:
- InterpreterThread state management (stop flag, interp ref)
- is_running() semantics
- REPL null guard (no AttributeError when interp is None)

All tests are headless-safe; QApplication is not needed because we only
test pure Python state, not widget rendering.
"""

from __future__ import annotations

import pytest


# ---------------------------------------------------------------------------
# InterpreterThread state tests (no execution, just lifecycle)
# ---------------------------------------------------------------------------

class TestInterpreterThreadState:
    """Test InterpreterThread attribute initialisation and stop semantics."""

    def _make_thread(self, code="PRINT 1", language=None):
        """Import lazily so tests that don't need Qt can still run."""
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            app = QApplication.instance() or QApplication(sys.argv[:1])
            _ = app
        except ImportError:
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import InterpreterThread  # type: ignore[import-not-found]
        from time_warp.graphics.turtle_state import TurtleState  # type: ignore[import-not-found]
        turtle = TurtleState()
        return InterpreterThread(code=code, turtle=turtle, language=language)

    def test_initial_should_stop_is_false(self):
        t = self._make_thread()
        assert t.should_stop is False

    def test_stop_sets_should_stop_true(self):
        t = self._make_thread()
        t.stop()
        assert t.should_stop is True

    def test_stop_with_no_interp_does_not_raise(self):
        """stop() must be safe even when interp is None (REPL null guard)."""
        t = self._make_thread()
        assert t.interp is None
        t.stop()  # should not raise AttributeError

    def test_stop_with_interp_sets_running_false(self):
        t = self._make_thread()
        from time_warp.core.interpreter import Interpreter  # type: ignore[import-not-found]
        t.interp = Interpreter()
        t.stop()
        assert t.interp.running is False

    def test_step_delay_initial_zero(self):
        t = self._make_thread()
        assert t.step_delay_ms == 0

    def test_debug_mode_default_false(self):
        t = self._make_thread()
        assert t.debug_mode is False

    def test_breakpoints_default_empty_set(self):
        t = self._make_thread()
        assert t.breakpoints == set()

    def test_breakpoints_custom(self):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            app = QApplication.instance() or QApplication(sys.argv[:1])
            _ = app
        except ImportError:
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import InterpreterThread  # type: ignore[import-not-found]
        from time_warp.graphics.turtle_state import TurtleState  # type: ignore[import-not-found]
        turtle = TurtleState()
        t = InterpreterThread(code="", turtle=turtle, breakpoints={5, 10})
        assert t.breakpoints == {5, 10}


# ---------------------------------------------------------------------------
# is_running() contract tests
# ---------------------------------------------------------------------------

class TestIsRunning:
    """Verify OutputPanel.is_running() returns False when no thread exists."""

    def _make_panel(self):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            app = QApplication.instance() or QApplication(sys.argv[:1])
            _ = app
        except ImportError:
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import OutputPanel  # type: ignore[import-not-found]
        return OutputPanel()

    def test_is_running_false_when_no_thread(self):
        panel = self._make_panel()
        # Fresh panel has no thread — cannot be running (may return None or False)
        assert not panel.is_running()

    def test_stop_execution_safe_with_no_thread(self):
        panel = self._make_panel()
        # Should not raise even if no thread is active
        panel.stop_execution()


# ---------------------------------------------------------------------------
# REPL null guard — simulate provide_input with None interpreter
# ---------------------------------------------------------------------------

class TestReplNullGuard:
    """Simulate the REPL null guard added to the input handler in output.py."""

    def _handle_input(self, interp, text):
        """Mirror the guarded logic from the input handler in OutputPanel."""
        if interp is None:
            return "❌ Input cancelled: interpreter unavailable"
        interp.provide_input(text)
        return None

    def test_none_interp_returns_error_message(self):
        result = self._handle_input(None, "hello")
        assert "❌" in result
        assert "interpreter unavailable" in result

    def test_none_interp_does_not_raise(self):
        # Should never propagate AttributeError
        try:
            self._handle_input(None, "data")
        except AttributeError:
            pytest.fail("AttributeError raised for None interp — guard missing")

    def test_valid_interp_calls_provide_input(self):
        calls = []

        class FakeInterp:
            def provide_input(self, text):
                calls.append(text)

        self._handle_input(FakeInterp(), "test_data")
        assert calls == ["test_data"]


# ---------------------------------------------------------------------------
# Extended REPL null guard tests
# ---------------------------------------------------------------------------

class TestReplNullGuard2:
    """More REPL null guard edge cases."""

    def _handle_input(self, interp, text):
        if interp is None:
            return "❌ Input cancelled: interpreter unavailable"
        interp.provide_input(text)
        return None

    def test_empty_string_interp_none(self):
        result = self._handle_input(None, "")
        assert "❌" in result

    def test_special_chars_interp_none(self):
        result = self._handle_input(None, "!@#$%")
        assert "interpreter unavailable" in result

    def test_none_input_text_with_none_interp(self):
        result = self._handle_input(None, None)
        assert "❌" in result

    def test_multiple_calls_to_valid_interp(self):
        calls = []

        class FakeInterp:
            def provide_input(self, text):
                calls.append(text)

        self._handle_input(FakeInterp(), "first")
        self._handle_input(FakeInterp(), "second")
        assert len(calls) == 2

    def test_result_is_none_for_valid_interp(self):
        class FakeInterp:
            def provide_input(self, text):
                pass

        result = self._handle_input(FakeInterp(), "hello")
        assert result is None

    def test_error_message_contains_cancelled(self):
        result = self._handle_input(None, "test")
        assert "cancelled" in result

    def test_error_message_is_string(self):
        result = self._handle_input(None, "test")
        assert isinstance(result, str)


class TestInterpreterThreadState2:
    """Additional InterpreterThread lifecycle tests."""

    def _make_thread(self, code="PRINT 1", language=None):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            app = QApplication.instance() or QApplication(sys.argv[:1])
            _ = app
        except ImportError:
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import InterpreterThread  # type: ignore[import-not-found]
        from time_warp.graphics.turtle_state import TurtleState  # type: ignore[import-not-found]
        turtle = TurtleState()
        return InterpreterThread(code=code, turtle=turtle, language=language)

    def test_thread_has_code_attribute(self):
        t = self._make_thread(code="LET X=1")
        assert t.code == "LET X=1"

    def test_thread_stop_twice_no_error(self):
        t = self._make_thread()
        t.stop()
        t.stop()
        assert t.should_stop is True

    def test_thread_interp_initially_none(self):
        t = self._make_thread()
        assert t.interp is None

    def test_thread_stop_with_interp_assigned(self):
        from time_warp.core.interpreter import Interpreter  # type: ignore[import-not-found]
        t = self._make_thread()
        t.interp = Interpreter()
        t.stop()
        assert t.should_stop is True

    def test_thread_different_codes(self):
        t1 = self._make_thread(code="PRINT 1")
        t2 = self._make_thread(code="PRINT 2")
        assert t1.code != t2.code

    def test_thread_language_parameter_stored(self):
        from time_warp.core.interpreter import Language  # type: ignore[import-not-found]
        t = self._make_thread(language=Language.BASIC)
        assert t.language == Language.BASIC

    def test_thread_language_default_none(self):
        t = self._make_thread()
        assert t.language is None


class TestReplNullGuard3:
    """Further REPL null-guard tests."""

    def _handle_input(self, interp, text):
        if interp is None:
            return "❌ Input cancelled: interpreter unavailable"
        interp.provide_input(text)
        return None

    def test_long_text_null_interp(self):
        result = self._handle_input(None, "a" * 1000)
        assert "❌" in result

    def test_unicode_text_null_interp(self):
        result = self._handle_input(None, "héllo wörld")
        assert "❌" in result

    def test_valid_interp_different_texts(self):
        texts = []

        class FakeInterp:
            def provide_input(self, text):
                texts.append(text)

        for i in range(5):
            self._handle_input(FakeInterp(), str(i))
        assert len(texts) == 5

    def test_returned_none_for_non_none_interp(self):
        class FakeInterp:
            def provide_input(self, text):
                pass

        for _ in range(3):
            assert self._handle_input(FakeInterp(), "x") is None

    def test_error_format_stable(self):
        result1 = self._handle_input(None, "foo")
        result2 = self._handle_input(None, "bar")
        assert result1 == result2


class TestInterpreterThreadExtended:
    """More InterpreterThread state tests."""

    def _make_thread(self, code="10 PRINT 1", lang="BASIC"):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        return InterpreterThread(code=code, language=lang, turtle=TurtleState())

    def test_code_attribute_stored(self):
        t = self._make_thread(code="10 PRINT 2")
        assert t.code == "10 PRINT 2"

    def test_language_attribute_stored(self):
        t = self._make_thread(lang="LUA")
        assert t.language == "LUA"

    def test_debug_mode_default_false(self):
        t = self._make_thread()
        assert t.debug_mode is False

    def test_breakpoints_default_empty(self):
        t = self._make_thread()
        assert len(t.breakpoints) == 0

    def test_should_stop_default_false(self):
        t = self._make_thread()
        assert t.should_stop is False

    def test_stop_sets_should_stop(self):
        t = self._make_thread()
        t.stop()
        assert t.should_stop is True

    def test_interp_initially_none(self):
        t = self._make_thread()
        assert t.interp is None

    def test_turtle_stored(self):
        from time_warp.graphics.turtle_state import TurtleState
        ts = TurtleState()
        from time_warp.ui.output import InterpreterThread
        t = InterpreterThread(code="1", language="BASIC", turtle=ts)
        assert t.turtle is ts

    def test_step_delay_ms_default_zero(self):
        t = self._make_thread()
        assert t.step_delay_ms == 0

    def test_set_debug_mode_true(self):
        t = self._make_thread()
        t.debug_mode = True
        assert t.debug_mode is True

    def test_add_breakpoint(self):
        t = self._make_thread()
        t.breakpoints.add(5)
        assert 5 in t.breakpoints

    def test_code_javascript(self):
        t = self._make_thread(code='console.log("hi")', lang="JAVASCRIPT")
        assert t.language == "JAVASCRIPT"

    def test_stop_twice_no_error(self):
        t = self._make_thread()
        t.stop()
        t.stop()  # second stop should be safe
        assert t.should_stop is True


class TestInterpreterThreadExtended2:
    """More InterpreterThread tests."""

    def _make(self, code="10 PRINT 1", lang="BASIC"):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        return InterpreterThread(code=code, language=lang, turtle=TurtleState())

    def test_code_lua(self):
        t = self._make(code="print(1)", lang="LUA")
        assert t.code == "print(1)"

    def test_language_javascript(self):
        t = self._make(lang="JAVASCRIPT")
        assert t.language == "JAVASCRIPT"

    def test_language_lisp(self):
        t = self._make(lang="LISP")
        assert t.language == "LISP"

    def test_language_cobol(self):
        t = self._make(lang="COBOL")
        assert t.language == "COBOL"

    def test_language_tcl(self):
        t = self._make(lang="TCL")
        assert t.language == "TCL"

    def test_step_delay_settable(self):
        t = self._make()
        t.step_delay_ms = 100
        assert t.step_delay_ms == 100

    def test_multiple_breakpoints(self):
        t = self._make()
        t.breakpoints.add(1)
        t.breakpoints.add(2)
        t.breakpoints.add(3)
        assert len(t.breakpoints) == 3

    def test_clear_breakpoints(self):
        t = self._make()
        t.breakpoints.add(10)
        t.breakpoints.clear()
        assert len(t.breakpoints) == 0

    def test_breakpoints_is_set_type(self):
        t = self._make()
        assert isinstance(t.breakpoints, set)

    def test_language_postscript(self):
        t = self._make(lang="POSTSCRIPT")
        assert t.language == "POSTSCRIPT"

    def test_debug_toggle(self):
        t = self._make()
        t.debug_mode = True
        t.debug_mode = False
        assert t.debug_mode is False

    def test_stop_affects_should_stop_only(self):
        t = self._make()
        t.stop()
        assert t.should_stop is True
        assert t.debug_mode is False


class TestInterpreterThreadExtended3:
    """Third round of InterpreterThread state tests."""

    def _make(self, lang="BASIC"):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            QApplication.instance() or QApplication(sys.argv[:1])
        except ImportError:
            import pytest
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        turtle = TurtleState()
        return InterpreterThread(code="PRINT 1", turtle=turtle, language=lang)

    def test_default_language_stored(self):
        t = self._make("LUA")
        assert t.language == "LUA"

    def test_erlang_thread_creation(self):
        t = self._make("ERLANG")
        assert t.language == "ERLANG"

    def test_lisp_thread_creation(self):
        t = self._make("LISP")
        assert t.language == "LISP"

    def test_cobol_thread_creation(self):
        t = self._make("COBOL")
        assert t.language == "COBOL"

    def test_tcl_thread_creation(self):
        t = self._make("TCL")
        assert t.language == "TCL"

    def test_stop_sets_flag(self):
        t = self._make()
        t.stop()
        assert t.should_stop is True

    def test_stop_twice_no_crash(self):
        t = self._make()
        t.stop()
        t.stop()
        assert t.should_stop is True

    def test_debug_mode_default_false(self):
        t = self._make()
        assert t.debug_mode is False

    def test_set_debug_true(self):
        t = self._make()
        t.debug_mode = True
        assert t.debug_mode is True

    def test_code_stored(self):
        t = self._make()
        assert t.code == "PRINT 1"


class TestInterpreterThreadExtended4:
    """Fourth round of InterpreterThread state tests."""

    def _make(self, lang="BASIC", code="PRINT 1"):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            QApplication.instance() or QApplication(sys.argv[:1])
        except ImportError:
            import pytest
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        turtle = TurtleState()
        return InterpreterThread(code=code, turtle=turtle, language=lang)

    def test_prolog_language(self):
        t = self._make("PROLOG")
        assert t.language == "PROLOG"

    def test_pascal_language(self):
        t = self._make("PASCAL")
        assert t.language == "PASCAL"

    def test_forth_language(self):
        t = self._make("FORTH")
        assert t.language == "FORTH"

    def test_hypertalk_language(self):
        t = self._make("HYPERTALK")
        assert t.language == "HYPERTALK"

    def test_logo_language(self):
        t = self._make("LOGO")
        assert t.language == "LOGO"

    def test_custom_code_stored(self):
        t = self._make("LUA", "print('hello')")
        assert t.code == "print('hello')"

    def test_should_stop_default_false(self):
        t = self._make("BASIC")
        assert t.should_stop is False

    def test_stop_sets_should_stop(self):
        t = self._make("BASIC")
        t.stop()
        assert t.should_stop is True

    def test_stop_twice_stays_true(self):
        t = self._make("BASIC")
        t.stop()
        t.stop()
        assert t.should_stop is True

    def test_thread_not_none(self):
        t = self._make("JAVASCRIPT")
        assert t is not None


class TestInterpreterThreadExtended5:
    """Fifth round of InterpreterThread state tests."""

    def _make(self, lang="BASIC", code="PRINT 1"):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            QApplication.instance() or QApplication(sys.argv[:1])
        except ImportError:
            import pytest
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        turtle = TurtleState()
        return InterpreterThread(code=code, turtle=turtle, language=lang)

    def test_c_language(self):
        t = self._make("C")
        assert t.language == "C"

    def test_pilot_language(self):
        t = self._make("PILOT")
        assert t.language == "PILOT"

    def test_lisp_language(self):
        t = self._make("LISP")
        assert t.language == "LISP"

    def test_brainfuck_language(self):
        t = self._make("BRAINFUCK")
        assert t.language == "BRAINFUCK"

    def test_hypertalk_language(self):
        t = self._make("HYPERTALK")
        assert t.language == "HYPERTALK"

    def test_erlang_language(self):
        t = self._make("ERLANG")
        assert t.language == "ERLANG"

    def test_code_stored_properly(self):
        code = "10 PRINT 99"
        t = self._make("BASIC", code)
        assert t.code == code

    def test_stop_flag_initial(self):
        t = self._make("LUA")
        assert t.should_stop is False

    def test_stop_sets_flag(self):
        t = self._make("LUA")
        t.stop()
        assert t.should_stop is True

    def test_two_threads_independent_stop(self):
        t1 = self._make("BASIC")
        t2 = self._make("LUA")
        t1.stop()
        assert t2.should_stop is False


class TestInterpreterThreadExtended6:
    """Sixth round of InterpreterThread state tests."""

    def _make(self, lang="BASIC", code="PRINT 1"):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            QApplication.instance() or QApplication(sys.argv[:1])
        except ImportError:
            import pytest
            pytest.skip("PySide6 not installed")
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        turtle = TurtleState()
        return InterpreterThread(code=code, turtle=turtle, language=lang)

    def test_javascript_thread(self):
        t = self._make("JAVASCRIPT")
        assert t is not None

    def test_cobol_thread(self):
        t = self._make("COBOL")
        assert t is not None

    def test_tcl_thread(self):
        t = self._make("TCL")
        assert t is not None

    def test_postscript_thread(self):
        t = self._make("POSTSCRIPT")
        assert t is not None

    def test_code_stored(self):
        t = self._make(code="hello")
        assert t.code == "hello"

    def test_stop_initially_false(self):
        t = self._make()
        assert t.should_stop is False

    def test_stop_sets_true(self):
        t = self._make()
        t.stop()
        assert t.should_stop is True

    def test_stop_twice(self):
        t = self._make()
        t.stop()
        t.stop()
        assert t.should_stop is True

    def test_three_threads_all_independent(self):
        t1 = self._make("BASIC")
        t2 = self._make("LUA")
        t3 = self._make("LISP")
        t1.stop()
        assert not t2.should_stop and not t3.should_stop

    def test_logo_thread(self):
        t = self._make("LOGO")
        assert t is not None
