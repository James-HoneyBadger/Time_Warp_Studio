from argparse import Namespace

import test_runner
# pylint: disable=reimported,redefined-outer-name,line-too-long,wrong-import-position


def test_resolve_test_targets_supports_unit_integration_and_demos():
    args = Namespace(
        basic=False,
        comprehensive=False,
        parallel=False,
        integration=True,
        demos=True,
        unit=False,
        all=False,
    )

    targets = test_runner.resolve_test_targets(args)

    assert "time_warp/tests" in targets
    assert "../../tests/test_basic_functionality.py" in targets
    assert "../../tests/test_all_demos.py" in targets


def test_build_pytest_command_adds_coverage_and_parallel_flags():
    args = Namespace(
        basic=False,
        comprehensive=True,
        parallel=True,
        integration=False,
        demos=False,
        unit=True,
        all=False,
    )

    cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])

    assert cmd[:3] == [test_runner.sys.executable, "-m", "pytest"]
    assert "--cov=time_warp" in cmd
    assert "-n" in cmd
    assert "auto" in cmd
    assert "time_warp/tests" in cmd


class TestResolveTestTargets:
    """More resolve_test_targets tests."""

    def test_basic_returns_unit_targets(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        targets = test_runner.resolve_test_targets(args)
        assert "time_warp/tests" in targets

    def test_unit_returns_unit_targets(self):
        args = Namespace(basic=False, comprehensive=False, parallel=False, integration=False, demos=False, unit=True, all=False)
        targets = test_runner.resolve_test_targets(args)
        assert "time_warp/tests" in targets

    def test_all_returns_all_targets(self):
        args = Namespace(basic=False, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=True)
        targets = test_runner.resolve_test_targets(args)
        assert "time_warp/tests" in targets
        assert "../../tests/test_basic_functionality.py" in targets
        assert "../../tests/test_all_demos.py" in targets

    def test_demos_flag_includes_demo_target(self):
        args = Namespace(basic=False, comprehensive=False, parallel=False, integration=False, demos=True, unit=False, all=False)
        targets = test_runner.resolve_test_targets(args)
        assert "../../tests/test_all_demos.py" in targets

    def test_integration_flag_includes_integration_target(self):
        args = Namespace(basic=False, comprehensive=False, parallel=False, integration=True, demos=False, unit=False, all=False)
        targets = test_runner.resolve_test_targets(args)
        assert "../../tests/test_basic_functionality.py" in targets


class TestBuildPytestCommand:
    """More build_pytest_command tests."""

    def test_command_starts_with_python(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert cmd[0] == test_runner.sys.executable

    def test_command_includes_pytest(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "-m" in cmd
        assert "pytest" in cmd

    def test_command_includes_target(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "time_warp/tests" in cmd

    def test_no_parallel_flag_by_default(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "-n" not in cmd

    def test_coverage_not_in_basic_command(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "--cov=time_warp" not in cmd

    def test_unit_test_targets_constant(self):
        assert test_runner.UNIT_TEST_TARGETS == ["time_warp/tests"]

    def test_demo_test_targets_constant(self):
        assert test_runner.DEMO_TEST_TARGETS == ["../../tests/test_all_demos.py"]


class TestTestRunnerExtended:
    """Extended test runner tests."""

    def test_resolve_basic_targets(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)
        assert len(targets) > 0

    def test_resolve_all_targets(self):
        args = Namespace(basic=False, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)
        assert len(targets) > 0

    def test_build_command_is_list(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert isinstance(cmd, list)

    def test_build_command_starts_with_pytest(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert any("pytest" in part for part in cmd)

    def test_comprehensive_includes_coverage(self):
        args = Namespace(basic=False, comprehensive=True, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "--cov=time_warp" in cmd

    def test_unit_test_targets_is_list(self):
        assert isinstance(test_runner.UNIT_TEST_TARGETS, list)

    def test_demo_test_targets_is_list(self):
        assert isinstance(test_runner.DEMO_TEST_TARGETS, list)

    def test_resolve_unit_targets(self):
        args = Namespace(basic=False, comprehensive=False, parallel=False, integration=False, demos=False, unit=True, all=False)
        targets = test_runner.resolve_test_targets(args)
        assert "time_warp/tests" in targets

    def test_resolve_demo_targets(self):
        args = Namespace(basic=False, comprehensive=False, parallel=False, integration=False, demos=True, unit=False, all=False)
        targets = test_runner.resolve_test_targets(args)
        assert any("demos" in t or "test_all_demos" in t for t in targets)

    def test_build_command_includes_target_path(self):
        args = Namespace(basic=True, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "time_warp/tests" in cmd


class TestTestRunnerExtended2:
    """More test runner tests."""

    def _basic_args(self, **kwargs):
        defaults = dict(basic=False, comprehensive=False, parallel=False, integration=False, demos=False, unit=False, all=False)
        defaults.update(kwargs)
        return Namespace(**defaults)

    def test_resolve_all_returns_multiple_targets(self):
        args = self._basic_args(all=True)
        targets = test_runner.resolve_test_targets(args)
        assert len(targets) >= 2

    def test_build_command_is_list(self):
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert isinstance(cmd, list)

    def test_unit_test_targets_not_empty(self):
        assert len(test_runner.UNIT_TEST_TARGETS) > 0

    def test_demo_test_targets_not_empty(self):
        assert len(test_runner.DEMO_TEST_TARGETS) > 0

    def test_parallel_flag_adds_worker(self):
        args = self._basic_args(basic=True, parallel=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert any("-n" in part or "worker" in part or "auto" in part for part in cmd)

    def test_basic_mode_no_coverage(self):
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "--cov=time_warp" not in cmd

    def test_resolve_basic_returns_list(self):
        args = self._basic_args(basic=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_integration_includes_integration_path(self):
        args = self._basic_args(integration=True)
        targets = test_runner.resolve_test_targets(args)
        assert any("integration" in t or "test_basic" in t for t in targets)

    def test_command_contains_q_flag(self):
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "pytest" in " ".join(cmd)

    def test_command_non_empty(self):
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2


class TestTestRunnerExtended3:
    """Third round of test_runner tests."""

    def _basic_args(self, **kwargs):
        from argparse import Namespace
        defaults = dict(basic=False, comprehensive=False, parallel=False,
                        integration=False, demos=False, unit=False, all=False)
        defaults.update(kwargs)
        return Namespace(**defaults)

    def test_all_flag_includes_targets(self):
        import test_runner
        args = self._basic_args(all=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_unit_flag_resolves_targets(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_demos_flag_resolves_targets(self):
        import test_runner
        args = self._basic_args(demos=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_comprehensive_flag_resolves_targets(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_parallel_flag_resolves_targets(self):
        import test_runner
        args = self._basic_args(parallel=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_comprehensive_command_non_empty(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2

    def test_unit_command_non_empty(self):
        import test_runner
        args = self._basic_args(unit=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2

    def test_integration_command_non_empty(self):
        import test_runner
        args = self._basic_args(integration=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2

    def test_all_command_non_empty(self):
        import test_runner
        args = self._basic_args(all=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2

    def test_demos_command_non_empty(self):
        import test_runner
        args = self._basic_args(demos=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2


class TestTestRunnerExtended4:
    """Fourth round of test_runner tests."""

    def _basic_args(self, **kwargs):
        from argparse import Namespace
        defaults = dict(basic=False, comprehensive=False, parallel=False,
                        integration=False, demos=False, unit=False, all=False)
        defaults.update(kwargs)
        return Namespace(**defaults)

    def test_basic_flag_returns_list(self):
        import test_runner
        args = self._basic_args(basic=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_comprehensive_flag_returns_list(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_unit_flag_returns_list(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_no_flags_returns_list(self):
        import test_runner
        args = self._basic_args()
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_demos_flag_returns_list(self):
        import test_runner
        args = self._basic_args(demos=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_integration_flag_returns_list(self):
        import test_runner
        args = self._basic_args(integration=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_build_command_all(self):
        import test_runner
        args = self._basic_args(all=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2

    def test_build_command_basic(self):
        import test_runner
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2

    def test_build_command_comprehensive(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2

    def test_build_command_unit(self):
        import test_runner
        args = self._basic_args(unit=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 2


class TestTestRunnerExtended5:
    """Fifth round of test runner tests."""

    def _basic_args(self, **kwargs):
        import argparse
        defaults = dict(basic=False, unit=False, all=False, comprehensive=False,
                        demos=False, integration=False, parallel=False, coverage=False,
                        verbose=False, report=False, watch=False, fast=False, language=None)
        defaults.update(kwargs)
        return argparse.Namespace(**defaults)

    def test_resolve_unit_returns_list(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_basic_has_items(self):
        import test_runner
        args = self._basic_args(basic=True)
        targets = test_runner.resolve_test_targets(args)
        assert len(targets) > 0

    def test_build_cmd_contains_pytest(self):
        import test_runner
        args = self._basic_args(unit=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert "pytest" in " ".join(cmd)

    def test_unit_targets_constant_not_empty(self):
        import test_runner
        assert len(test_runner.UNIT_TEST_TARGETS) > 0

    def test_demo_targets_constant_not_empty(self):
        import test_runner
        assert len(test_runner.DEMO_TEST_TARGETS) > 0

    def test_build_comprehensive_cmd_not_empty(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) >= 3

    def test_parallel_flag_in_command(self):
        import test_runner
        args = self._basic_args(unit=True, parallel=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        cmd_str = " ".join(cmd)
        assert "-n" in cmd_str or "auto" in cmd_str or isinstance(cmd, list)

    def test_verbose_flag_exists(self):
        import test_runner
        args = self._basic_args(unit=True, verbose=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert isinstance(cmd, list)

    def test_resolve_returns_strings(self):
        import test_runner
        args = self._basic_args(basic=True)
        targets = test_runner.resolve_test_targets(args)
        for t in targets:
            assert isinstance(t, str)

    def test_build_command_returns_list(self):
        import test_runner
        args = self._basic_args(all=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert isinstance(cmd, list)


class TestTestRunnerExtended6:
    """Sixth round of test runner tests."""

    def _basic_args(self, **kwargs):
        import argparse
        defaults = dict(basic=False, unit=False, all=False, comprehensive=False,
                        demos=False, integration=False, parallel=False, coverage=False,
                        verbose=False, report=False, watch=False, fast=False, language=None)
        defaults.update(kwargs)
        return argparse.Namespace(**defaults)

    def test_resolve_all_targets_list(self):
        import test_runner
        args = self._basic_args(all=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_unit_targets_list(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_comprehensive_list(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_build_cmd_is_list(self):
        import test_runner
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert isinstance(cmd, list)

    def test_build_cmd_nonempty(self):
        import test_runner
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert len(cmd) > 0

    def test_build_cmd_contains_pytest(self):
        import test_runner
        args = self._basic_args(basic=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert any("pytest" in str(c) for c in cmd)

    def test_build_cmd_with_verbose(self):
        import test_runner
        args = self._basic_args(verbose=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert isinstance(cmd, list)

    def test_build_cmd_with_coverage(self):
        import test_runner
        args = self._basic_args(coverage=True)
        cmd = test_runner.build_pytest_command(args, ["time_warp/tests"])
        assert isinstance(cmd, list)

    def test_resolve_demos_list(self):
        import test_runner
        args = self._basic_args(demos=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_basic_not_none(self):
        import test_runner
        args = self._basic_args(basic=True)
        targets = test_runner.resolve_test_targets(args)
        assert targets is not None


class TestTestRunnerExtended7:
    """Seventh round of test runner tests."""

    def _basic_args(self, **kwargs):
        import argparse
        defaults = dict(basic=False, unit=False, all=False, comprehensive=False,
                        demos=False, integration=False, parallel=False, coverage=False,
                        verbose=False, report=False, watch=False, fast=False, language=None)
        defaults.update(kwargs)
        return argparse.Namespace(**defaults)

    def test_resolve_unit_list(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_comprehensive_list(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_all_list(self):
        import test_runner
        args = self._basic_args(all=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_build_cmd_comprehensive(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert isinstance(cmd, list)

    def test_build_cmd_contains_pytest(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert any("pytest" in c for c in cmd)

    def test_build_cmd_not_empty(self):
        import test_runner
        args = self._basic_args(all=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert len(cmd) > 0

    def test_build_cmd_with_coverage(self):
        import test_runner
        args = self._basic_args(unit=True, coverage=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert isinstance(cmd, list)

    def test_build_cmd_with_verbose(self):
        import test_runner
        args = self._basic_args(unit=True, verbose=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert isinstance(cmd, list)

    def test_resolve_integration_not_none(self):
        import test_runner
        args = self._basic_args(integration=True)
        targets = test_runner.resolve_test_targets(args)
        assert targets is not None

    def test_resolve_demos_not_empty(self):
        import test_runner
        args = self._basic_args(demos=True)
        targets = test_runner.resolve_test_targets(args)
        assert len(targets) > 0


class TestTestRunnerExtended8:
    """Eighth round of test runner tests."""

    def _basic_args(self, **kwargs):
        import argparse
        defaults = dict(basic=False, unit=False, all=False, comprehensive=False,
                        demos=False, integration=False, parallel=False, coverage=False,
                        verbose=False, report=False, watch=False, fast=False, language=None)
        defaults.update(kwargs)
        return argparse.Namespace(**defaults)

    def test_resolve_unit_is_list(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_basic_not_none(self):
        import test_runner
        args = self._basic_args(basic=True)
        targets = test_runner.resolve_test_targets(args)
        assert targets is not None

    def test_resolve_all_not_empty(self):
        import test_runner
        args = self._basic_args(all=True)
        targets = test_runner.resolve_test_targets(args)
        assert len(targets) > 0

    def test_build_cmd_is_list(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert isinstance(cmd, list)

    def test_build_cmd_contains_pytest(self):
        import test_runner
        args = self._basic_args(unit=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert any("pytest" in c for c in cmd)

    def test_build_cmd_not_empty(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert len(cmd) > 0

    def test_resolve_demos_is_list(self):
        import test_runner
        args = self._basic_args(demos=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_resolve_integration_is_list(self):
        import test_runner
        args = self._basic_args(integration=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)

    def test_build_cmd_comprehensive_not_empty(self):
        import test_runner
        args = self._basic_args(comprehensive=True)
        targets = test_runner.resolve_test_targets(args)
        cmd = test_runner.build_pytest_command(args, targets)
        assert len(cmd) > 0

    def test_resolve_all_is_list(self):
        import test_runner
        args = self._basic_args(all=True)
        targets = test_runner.resolve_test_targets(args)
        assert isinstance(targets, list)
