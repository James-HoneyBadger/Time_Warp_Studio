from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")
QApplication = QtWidgets.QApplication

from time_warp.ui.onboarding import OnboardingDialog, OnboardingManager


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def test_onboarding_manager_persists_and_resets(tmp_path: Path):
    manager = OnboardingManager(config_dir=tmp_path)

    assert manager.should_show_onboarding() is True

    manager.mark_step_completed("welcome")
    manager.mark_tutorial_completed(skip=True)

    restored = OnboardingManager(config_dir=tmp_path)
    assert "welcome" in restored.completed_steps
    assert restored.tutorial_completed is True
    assert restored.skip_onboarding is True
    assert restored.should_show_onboarding() is False

    restored.reset_onboarding()
    assert restored.completed_steps == set()
    assert restored.tutorial_completed is False
    assert restored.skip_onboarding is False
    assert restored.should_show_onboarding() is True

    restored.record_result("dismissed", skip_requested=True)
    assert restored.tutorial_completed is False
    assert restored.skip_onboarding is True
    assert restored.should_show_onboarding() is False


def test_onboarding_dialog_tracks_skip_and_completion_state(qapp):
    dialog = OnboardingDialog()
    assert dialog.completion_state == "in_progress"

    dialog._skip_tutorial()
    assert dialog.completion_state == "skipped"

    dialog = OnboardingDialog()
    dialog.current_step_index = len(dialog.steps) - 1
    dialog._finish_tutorial()
    assert dialog.completion_state == "completed"
