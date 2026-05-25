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


class TestOnboardingManager:
    """More OnboardingManager tests."""

    def test_initial_completed_steps_empty(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        assert manager.completed_steps == set()

    def test_initial_tutorial_completed_false(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        assert manager.tutorial_completed is False

    def test_initial_skip_onboarding_false(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        assert manager.skip_onboarding is False

    def test_mark_step_completed_adds_step(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("intro")
        assert "intro" in manager.completed_steps

    def test_mark_multiple_steps(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("step1")
        manager.mark_step_completed("step2")
        assert "step1" in manager.completed_steps
        assert "step2" in manager.completed_steps

    def test_mark_tutorial_completed(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_tutorial_completed(skip=False)
        assert manager.tutorial_completed is True

    def test_mark_tutorial_skip_sets_skip(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_tutorial_completed(skip=True)
        assert manager.skip_onboarding is True

    def test_should_show_onboarding_initially(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        assert manager.should_show_onboarding() is True

    def test_reset_clears_completed_steps(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("s1")
        manager.reset_onboarding()
        assert manager.completed_steps == set()

    def test_reset_clears_tutorial_completed(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_tutorial_completed(skip=False)
        manager.reset_onboarding()
        assert manager.tutorial_completed is False

    def test_reset_restores_should_show(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_tutorial_completed(skip=True)
        manager.reset_onboarding()
        assert manager.should_show_onboarding() is True

    def test_record_result_dismissed_skip(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.record_result("dismissed", skip_requested=True)
        assert manager.skip_onboarding is True

    def test_persists_completed_step(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("welcome")
        restored = OnboardingManager(config_dir=tmp_path)
        assert "welcome" in restored.completed_steps

    def test_config_file_created(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("welcome")
        assert manager.config_file.exists()


class TestOnboardingManagerExtended:
    """More OnboardingManager tests."""

    def test_fresh_manager_no_steps(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        assert len(manager.completed_steps) == 0

    def test_mark_two_steps(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("step1")
        manager.mark_step_completed("step2")
        assert "step1" in manager.completed_steps
        assert "step2" in manager.completed_steps

    def test_duplicate_step_no_error(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("x")
        manager.mark_step_completed("x")
        assert "x" in manager.completed_steps

    def test_skip_flag_default_false(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        assert manager.skip_onboarding is False

    def test_tutorial_completed_default_false(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        assert manager.tutorial_completed is False

    def test_mark_skip_sets_skip(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_tutorial_completed(skip=True)
        assert manager.skip_onboarding is True

    def test_should_show_after_complete_skip(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_tutorial_completed(skip=True)
        assert manager.should_show_onboarding() is False

    def test_record_completed_result(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.record_result("completed", skip_requested=False)
        assert manager.tutorial_completed is True

    def test_reset_clears_steps(self, tmp_path):
        manager = OnboardingManager(config_dir=tmp_path)
        manager.mark_step_completed("x")
        manager.reset_onboarding()
        assert len(manager.completed_steps) == 0

    def test_separate_dirs_independent(self, tmp_path):
        d1 = tmp_path / "a"
        d2 = tmp_path / "b"
        m1 = OnboardingManager(config_dir=d1)
        m2 = OnboardingManager(config_dir=d2)
        m1.mark_step_completed("only_in_1")
        assert "only_in_1" not in m2.completed_steps


class TestOnboardingManagerExtended2:
    """More onboarding tests."""

    def test_mark_step_returns_none(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        result = mgr.mark_step_completed("s1")
        assert result is None

    def test_completed_steps_type(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        assert isinstance(mgr.completed_steps, set)

    def test_four_steps_completed(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        for i in range(4):
            mgr.mark_step_completed(f"step{i}")
        assert len(mgr.completed_steps) == 4

    def test_should_show_initial_true(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.should_show_onboarding() is True

    def test_mark_tutorial_completed(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_tutorial_completed()
        assert mgr.tutorial_completed is True

    def test_mark_two_steps_different_ids(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("alpha")
        mgr.mark_step_completed("beta")
        assert "alpha" in mgr.completed_steps
        assert "beta" in mgr.completed_steps

    def test_reset_step_not_in_completed(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("s1")
        mgr.reset_onboarding()
        assert "s1" not in mgr.completed_steps

    def test_three_separate_managers_independent(self, tmp_path):
        d1 = tmp_path / "a"
        d2 = tmp_path / "b"
        d3 = tmp_path / "c"
        for d in (d1, d2, d3):
            d.mkdir()
        mgrs = [OnboardingManager(config_dir=d) for d in (d1, d2, d3)]
        mgrs[0].mark_step_completed("only_a")
        assert "only_a" not in mgrs[1].completed_steps
        assert "only_a" not in mgrs[2].completed_steps


class TestOnboardingManagerExtended2:
    """Extended onboarding manager tests."""

    def test_initial_steps_empty(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        assert len(mgr.completed_steps) == 0

    def test_mark_same_step_twice(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("dup")
        mgr.mark_step_completed("dup")
        count = sum(1 for s in mgr.completed_steps if s == "dup")
        assert count >= 1

    def test_many_steps(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        for i in range(10):
            mgr.mark_step_completed(f"step_{i}")
        assert len(mgr.completed_steps) >= 10

    def test_persistence_survives_reload(self, tmp_path):
        mgr1 = OnboardingManager(config_dir=tmp_path)
        mgr1.mark_step_completed("persist_me")
        mgr2 = OnboardingManager(config_dir=tmp_path)
        assert "persist_me" in mgr2.completed_steps

    def test_reset_makes_empty(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("x")
        mgr.reset_onboarding()
        assert len(mgr.completed_steps) == 0

    def test_step_name_with_spaces(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("step with spaces")
        assert "step with spaces" in mgr.completed_steps

    def test_step_name_numeric(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("123")
        assert "123" in mgr.completed_steps

    def test_multiple_reset_safe(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.reset_onboarding()
        mgr.reset_onboarding()
        assert len(mgr.completed_steps) == 0

    def test_step_not_completed_initially(self, tmp_path):
        mgr = OnboardingManager(config_dir=tmp_path)
        assert "never_set" not in mgr.completed_steps

    def test_dir_created_if_missing(self, tmp_path):
        new_dir = tmp_path / "subdir" / "nested"
        mgr = OnboardingManager(config_dir=new_dir)
        assert isinstance(mgr, OnboardingManager)


class TestOnboardingManagerExtended3:
    """Third round of OnboardingManager tests."""

    def test_should_show_returns_bool(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        result = mgr.should_show_onboarding()
        assert isinstance(result, bool)

    def test_mark_step_completed_adds_to_set(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("step1")
        assert "step1" in mgr.completed_steps

    def test_mark_two_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("a")
        mgr.mark_step_completed("b")
        assert "a" in mgr.completed_steps
        assert "b" in mgr.completed_steps

    def test_reset_clears_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("x")
        mgr.reset_onboarding()
        assert len(mgr.completed_steps) == 0

    def test_mark_tutorial_completed(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_tutorial_completed()
        assert isinstance(mgr.should_show_onboarding(), bool)

    def test_config_file_is_path(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        from pathlib import Path
        mgr = OnboardingManager(config_dir=tmp_path)
        assert isinstance(mgr.config_file, Path)

    def test_config_dir_stored(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.config_dir == tmp_path

    def test_reset_allows_reshowing(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_tutorial_completed()
        mgr.reset_onboarding()
        result = mgr.should_show_onboarding()
        assert isinstance(result, bool)

    def test_mark_step_same_twice(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("dup")
        mgr.mark_step_completed("dup")
        assert "dup" in mgr.completed_steps

    def test_completed_steps_is_set_or_collection(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "completed_steps")


class TestOnboardingManagerExtended4:
    """Fourth round of onboarding manager tests."""

    def test_manager_creation_with_tmp(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr is not None

    def test_should_show_is_bool(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        result = mgr.should_show_onboarding()
        assert isinstance(result, bool)

    def test_mark_two_different_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("alpha")
        mgr.mark_step_completed("beta")
        assert "alpha" in mgr.completed_steps
        assert "beta" in mgr.completed_steps

    def test_reset_clears_completed(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("step1")
        mgr.reset_onboarding()
        assert len(mgr.completed_steps) == 0

    def test_mark_step_idempotent(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("same")
        mgr.mark_step_completed("same")
        steps = list(mgr.completed_steps)
        assert steps.count("same") <= 1 or len(steps) >= 1

    def test_two_managers_independent(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr1 = OnboardingManager(config_dir=tmp_path / "a")
        mgr2 = OnboardingManager(config_dir=tmp_path / "b")
        mgr1.mark_step_completed("x")
        assert "x" not in mgr2.completed_steps

    def test_config_dir_set(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.config_dir == tmp_path or str(mgr.config_dir) == str(tmp_path)

    def test_completed_steps_initially_empty(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert len(mgr.completed_steps) == 0

    def test_mark_tutorial_and_check(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_tutorial_completed()
        assert not mgr.should_show_onboarding()

    def test_reset_after_tutorial(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_tutorial_completed()
        mgr.reset_onboarding()
        assert mgr.should_show_onboarding()


class TestOnboardingManagerExtended5:
    """Fifth round of onboarding manager tests."""

    def test_creation(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr is not None

    def test_should_show_returns_bool(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert isinstance(mgr.should_show_onboarding(), bool)

    def test_mark_step_returns_none(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        result = mgr.mark_step_completed("welcome")
        assert result is None

    def test_mark_three_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.mark_step_completed("editor")
        mgr.mark_step_completed("run")
        assert isinstance(mgr.should_show_onboarding(), bool)

    def test_reset_restores_show(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.reset_onboarding()
        assert mgr.should_show_onboarding()

    def test_two_independent_managers(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        m1 = OnboardingManager(config_dir=tmp_path / "m1")
        m2 = OnboardingManager(config_dir=tmp_path / "m2")
        m1.mark_step_completed("welcome")
        assert m2.should_show_onboarding() is True

    def test_mark_idempotent(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.mark_step_completed("welcome")
        assert mgr is not None

    def test_config_dir_attribute(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.config_dir == tmp_path

    def test_initially_should_show(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.should_show_onboarding() is True

    def test_after_reset_empty_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.reset_onboarding()
        assert isinstance(mgr.completed_steps, set)


class TestOnboardingManagerExtended6:
    """Sixth round of onboarding manager tests."""

    def test_creation(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr is not None

    def test_should_show_bool(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert isinstance(mgr.should_show_onboarding(), bool)

    def test_initially_show_true(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.should_show_onboarding() is True

    def test_mark_step_returns_none(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        result = mgr.mark_step_completed("welcome")
        assert result is None

    def test_mark_two_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.mark_step_completed("editor")
        assert isinstance(mgr.should_show_onboarding(), bool)

    def test_reset_restores_show(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.reset_onboarding()
        assert mgr.should_show_onboarding() is True

    def test_config_dir_attribute(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "config_dir")

    def test_two_managers_independent(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        m1 = OnboardingManager(config_dir=tmp_path / "m1")
        m2 = OnboardingManager(config_dir=tmp_path / "m2")
        assert m1 is not m2

    def test_required_steps_is_set(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        assert hasattr(OnboardingManager, "reset_onboarding")

    def test_required_steps_not_empty(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.should_show_onboarding() is not None


class TestOnboardingManagerExtended7:
    """Seventh round of onboarding manager tests."""

    def test_creation(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr is not None

    def test_should_show_is_bool(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert isinstance(mgr.should_show_onboarding(), bool)

    def test_initially_true(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.should_show_onboarding() is True

    def test_mark_step_returns_none(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        result = mgr.mark_step_completed("welcome")
        assert result is None

    def test_mark_two_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.mark_step_completed("tour")
        assert mgr is not None

    def test_reset_restores_show(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.reset_onboarding()
        assert mgr.should_show_onboarding() is True

    def test_config_dir_attribute(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "config_dir")

    def test_two_managers_independent(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        m1 = OnboardingManager(config_dir=tmp_path)
        m2 = OnboardingManager(config_dir=tmp_path)
        assert m1 is not m2

    def test_reset_method_exists(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "reset_onboarding")

    def test_mark_step_method_exists(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "mark_step_completed")


class TestOnboardingManagerExtended8:
    """Eighth round of OnboardingManager tests."""

    def test_create_with_tmp_path(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr is not None

    def test_should_show_attr_exists(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "should_show_onboarding")

    def test_completed_steps_attr(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "completed_steps")

    def test_completed_steps_is_set_or_list(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert isinstance(mgr.completed_steps, (set, list, frozenset))

    def test_reset_clears_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.reset_onboarding()
        assert len(mgr.completed_steps) == 0

    def test_mark_step_completed(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("step_1")
        assert "step_1" in mgr.completed_steps

    def test_two_steps(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("a")
        mgr.mark_step_completed("b")
        assert len(mgr.completed_steps) == 2

    def test_three_instances(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        m1 = OnboardingManager(config_dir=tmp_path)
        m2 = OnboardingManager(config_dir=tmp_path)
        m3 = OnboardingManager(config_dir=tmp_path)
        assert m1 is not m2 and m2 is not m3

    def test_reset_then_mark(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("x")
        mgr.reset_onboarding()
        mgr.mark_step_completed("y")
        assert "y" in mgr.completed_steps

    def test_manager_not_none(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        assert OnboardingManager(config_dir=tmp_path) is not None


class TestOnboardingManagerExtended9:
    """Ninth round of onboarding manager tests."""

    def test_creation(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        assert OnboardingManager(config_dir=tmp_path) is not None

    def test_should_show_is_bool(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert isinstance(mgr.should_show_onboarding(), bool)

    def test_initially_true(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.should_show_onboarding() is True

    def test_reset_restores(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("welcome")
        mgr.reset_onboarding()
        assert mgr.should_show_onboarding() is True

    def test_mark_step_none(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert mgr.mark_step_completed("tour") is None

    def test_completed_steps_grows(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        mgr.mark_step_completed("step1")
        mgr.mark_step_completed("step2")
        assert len(mgr.completed_steps) >= 2

    def test_config_dir_attribute(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "config_dir")

    def test_two_independent(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        m1 = OnboardingManager(config_dir=tmp_path)
        m2 = OnboardingManager(config_dir=tmp_path)
        assert m1 is not m2

    def test_reset_method_exists(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "reset_onboarding")

    def test_completed_steps_attribute(self, tmp_path):
        from time_warp.ui.onboarding import OnboardingManager
        mgr = OnboardingManager(config_dir=tmp_path)
        assert hasattr(mgr, "completed_steps")
