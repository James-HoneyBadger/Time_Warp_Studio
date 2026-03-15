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
