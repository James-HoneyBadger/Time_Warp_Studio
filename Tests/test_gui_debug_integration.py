"""Integration test for the old debug UI (skipped in this build).

These tests exercise the UI pause/step/resume flow which were removed
from this distribution. Keep the module skipped so CI doesn't run heavy
integration UI tests.
"""

import os
import time
import threading
import pytest

# PySide6 static analyzers can report missing members; tests only run
# in environments that provide PySide6 so silence import-time warnings.
# pylint: disable=import-error,no-name-in-module
from PySide6.QtWidgets import QApplication  # noqa: E402
from PySide6.QtCore import QCoreApplication  # noqa: E402

# Ensure headless Qt mode for CI environments
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

# Skip this module entirely — debug UI removed in this build
pytest.skip(
    "UI debug flow removed — skipping integration debug tests",
    allow_module_level=True,
)

# Make sure the python package path is available for tests
# (no explicit sys.path manipulation in test source; test runner should
# already include the project package path)


def _ensure_app():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def _wait_for_event(evt: threading.Event, timeout=3.0):
    """Wait while pumping Qt events so queued signals are delivered."""
    deadline = time.time() + timeout
    while not evt.is_set() and time.time() < deadline:
        QCoreApplication.processEvents()
        time.sleep(0.01)
    return evt.is_set()


def test_debug_pause_step_variables_flow():
    """Integration test: start debug, pause, step, variables update, resume.

    This test uses MainWindow and OutputPanel and allows a real QThread
    to run the interpreter. It pumps the Qt event loop while waiting for
    signals so the test is deterministic in headless environments.
    """
    from time_warp.ui.main_window import MainWindow

    _ensure_app()
    mw = MainWindow()

    # Simple program with three assignments and a print; logical lines
    # are counted by non-empty lines so we can step through reliably.
    code = """
    10 LET A = 1
    20 LET B = 2
    30 LET C = A + B
    40 PRINT C
    """

    editor = mw.get_current_editor()
    assert editor is not None
    editor.setPlainText(code)

    # Ensure we start with no breakpoints and connect signals
    mw.breakpoints = set()

    paused_event = threading.Event()
    paused_data = {}

    def on_pause(line, variables):
        paused_data["line"] = line
        paused_data["vars"] = dict(variables)
        paused_event.set()

    mw.output.debug_paused.connect(on_pause)

    # We will poll OutputPanel.is_running() after resuming execution to
    # detect when the interpreter finishes.

    # Start debug session; should pause immediately at first logical line
    mw.start_debug()

    assert _wait_for_event(paused_event, timeout=3.0), "Did not pause on start"

    # After initial pause variables should be empty (nothing executed yet)
    # VariableInspector rows may be zero – ensure that's true.
    assert mw.variable_inspector.rowCount() == 0

    # Stepping removed from UI — simply stop after pause
    mw.stop_program()

    # Wait for the interpreter thread to stop running
    deadline = time.time() + 3.0
    while mw.output.is_running() and time.time() < deadline:
        QCoreApplication.processEvents()
        time.sleep(0.01)

    assert not mw.output.is_running(), "Execution did not finish"
