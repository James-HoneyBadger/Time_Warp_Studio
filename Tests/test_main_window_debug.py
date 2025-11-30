"""MainWindow-related tests that require a QApplication runtime.

These tests are executed from the repository root — we add the in-tree
Platforms/Python package path at runtime so pytest can import the package.
"""

# PySide6 static analyzers can report that names are missing in some envs.
# Allow those import-time false positives for this test module.
# The tests intentionally import application objects at runtime so silence
# import-outside-toplevel as well.
# pylint: disable=no-name-in-module,import-error,import-outside-toplevel

import sys
from pathlib import Path
from typing import Any, Dict

# Debug UI removed from the application — skip these UI debug tests.
import pytest

# PySide6 static analyzers can report missing members; silence warnings for
# test environments where PySide6 is available at runtime.
# pylint: disable=import-error,no-name-in-module
from PySide6.QtWidgets import QApplication

# Ensure we import the in-repo package when running tests from the repo root.
# Add Platforms/Python to sys.path and allow non-top-level imports for tests.
p = Path(__file__).resolve().parents[1] / "Platforms" / "Python"
sys.path.insert(0, str(p))  # pylint: disable=wrong-import-position

# Skip module — debug UI removed in this build
pytest.skip("UI debug features removed in this build", allow_module_level=True)
# Allow imports that must happen at test runtime (UI classes).
# Keep runtime imports guarded where necessary.


def _ensure_app():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def test_main_window_has_breakpoints():
    """MainWindow should initialize `breakpoints` as a set."""
    # Module-level skip in effect for this test file
    from time_warp.ui.main_window import MainWindow

    _ensure_app()
    mw = MainWindow()

    assert hasattr(mw, "breakpoints")
    assert isinstance(mw.breakpoints, set)


def test_start_debug_calls_run_program_with_breakpoints(monkeypatch):
    """Ensure start_debug forwards breakpoints to OutputPanel.run_program."""
    from time_warp.ui.main_window import MainWindow

    _ensure_app()
    mw = MainWindow()

    # Put some demo code in the editor so run path is taken
    editor = mw.get_current_editor()
    editor.setPlainText('10 PRINT "HELLO"')

    # Set breakpoints and capture run_program calls
    mw.breakpoints = {1, 3}
    captured = {"called": False, "args": None, "kwargs": None}

    def fake_run_program(code, canvas, debug_mode=False, breakpoints=None):
        captured["called"] = True
        captured["args"] = (code, canvas)
        captured["kwargs"] = {
            "debug_mode": debug_mode,
            "breakpoints": breakpoints,
        }

    monkeypatch.setattr(mw.output, "run_program", fake_run_program)

    # Call start_debug and ensure call happened without exceptions
    mw.start_debug()

    assert captured["called"], "run_program was not called"
    # The captured kwargs are any-typed until runtime; copy into a dict
    # so static checkers understand subscripting is valid.
    captured_kwargs_any = captured["kwargs"]
    assert isinstance(captured_kwargs_any, dict)
    captured_kwargs: Dict[str, Any] = dict(captured_kwargs_any)
    assert captured_kwargs["debug_mode"] is True
    assert captured_kwargs["breakpoints"] == mw.breakpoints
