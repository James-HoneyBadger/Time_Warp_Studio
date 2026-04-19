from argparse import Namespace

import test_runner


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
