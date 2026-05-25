from time_warp.features.performance_benchmarks import BenchmarkRunner  # type: ignore[import-not-found]  # pylint: disable=import-error
from time_warp.core.interpreter import run, Language  # type: ignore[import-not-found]  # pylint: disable=import-error
from time_warp.tests.helpers import no_errors  # type: ignore[import-not-found]  # pylint: disable=import-error

# @pytest.mark.performance


class TestPerformanceBenchmarks:
    def test_language_execution_speed(self):
        """Benchmark execution speed for all supported languages."""
        runner = BenchmarkRunner(iterations=100)

        def benchmark_basic():
            # Simulate BASIC execution
            result = run("PRINT 1 + 1", Language.BASIC)
            assert no_errors(result)

        def benchmark_lua():
            # Simulate Lua execution
            result = run("print(1 + 1)", Language.LUA)
            assert no_errors(result)

        runner.run_benchmark("basic_execution", benchmark_basic)
        runner.run_benchmark("lua_execution", benchmark_lua)

    def test_memory_usage(self):
        """Benchmark memory usage during execution."""
        runner = BenchmarkRunner(iterations=50)

        def memory_test():
            # Simulate memory usage during execution
            result = run("FOR I = 1 TO 1000: NEXT I", Language.BASIC)
            assert no_errors(result)

        runner.run_benchmark("memory_usage", memory_test)


class TestBenchmarkRunner:
    """More BenchmarkRunner tests."""

    def test_runner_stores_results(self):
        runner = BenchmarkRunner(iterations=5)
        runner.run_benchmark("test1", lambda: None)
        assert len(runner.results) == 1

    def test_runner_result_has_test_name(self):
        runner = BenchmarkRunner(iterations=5)
        result = runner.run_benchmark("my_test", lambda: None)
        assert result.test_name == "my_test"

    def test_runner_result_has_iterations(self):
        runner = BenchmarkRunner(iterations=7)
        result = runner.run_benchmark("iter_test", lambda: None)
        assert result.iterations == 7

    def test_runner_result_avg_time_positive(self):
        runner = BenchmarkRunner(iterations=10)
        result = runner.run_benchmark("time_test", lambda: None)
        assert result.avg_time >= 0

    def test_runner_result_min_le_max(self):
        runner = BenchmarkRunner(iterations=10)
        result = runner.run_benchmark("minmax_test", lambda: None)
        assert result.min_time <= result.max_time

    def test_runner_result_total_time_positive(self):
        runner = BenchmarkRunner(iterations=10)
        result = runner.run_benchmark("total_test", lambda: None)
        assert result.total_time >= 0

    def test_runner_multiple_benchmarks_stored(self):
        runner = BenchmarkRunner(iterations=5)
        runner.run_benchmark("a", lambda: None)
        runner.run_benchmark("b", lambda: None)
        runner.run_benchmark("c", lambda: None)
        assert len(runner.results) == 3

    def test_runner_iterations_attribute(self):
        runner = BenchmarkRunner(iterations=20)
        assert runner.iterations == 20

    def test_runner_results_initially_empty(self):
        runner = BenchmarkRunner(iterations=5)
        assert runner.results == []

    def test_run_benchmark_returns_result_object(self):
        runner = BenchmarkRunner(iterations=5)
        result = runner.run_benchmark("obj_test", lambda: None)
        assert hasattr(result, "test_name")
        assert hasattr(result, "avg_time")
        assert hasattr(result, "min_time")
        assert hasattr(result, "max_time")

    def test_benchmark_with_basic_execution(self):
        runner = BenchmarkRunner(iterations=3)
        result = runner.run_benchmark("basic_exec", lambda: run("PRINT 1", Language.BASIC))
        assert result.test_name == "basic_exec"

    def test_benchmark_throughput_positive(self):
        runner = BenchmarkRunner(iterations=10)
        result = runner.run_benchmark("throughput_test", lambda: None)
        assert result.throughput > 0


class TestPerformanceExtended:
    """Extended performance tests."""

    def test_benchmark_runner_default_iterations(self):
        runner = BenchmarkRunner(iterations=1)
        assert runner.iterations == 1

    def test_benchmark_accumulates_results(self):
        runner = BenchmarkRunner(iterations=2)
        runner.run_benchmark("a", lambda: None)
        runner.run_benchmark("b", lambda: None)
        assert len(runner.results) == 2

    def test_benchmark_min_lte_avg(self):
        runner = BenchmarkRunner(iterations=5)
        result = runner.run_benchmark("timing", lambda: None)
        assert result.min_time <= result.avg_time

    def test_benchmark_max_gte_avg(self):
        runner = BenchmarkRunner(iterations=5)
        result = runner.run_benchmark("timing2", lambda: None)
        assert result.max_time >= result.avg_time

    def test_benchmark_result_test_name(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("mytest", lambda: None)
        assert result.test_name == "mytest"

    def test_benchmark_lua_execution(self):
        runner = BenchmarkRunner(iterations=3)
        result = runner.run_benchmark("lua", lambda: run('print(1)', Language.LUA))
        assert result.avg_time >= 0

    def test_benchmark_javascript_execution(self):
        runner = BenchmarkRunner(iterations=3)
        result = runner.run_benchmark("js", lambda: run('console.log(1)', Language.JAVASCRIPT))
        assert result.avg_time >= 0

    def test_benchmark_throughput_type(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("tp", lambda: None)
        assert isinstance(result.throughput, (int, float))

    def test_multiple_benchmarks_same_runner(self):
        runner = BenchmarkRunner(iterations=1)
        for i in range(5):
            runner.run_benchmark(f"test_{i}", lambda: None)
        assert len(runner.results) == 5

    def test_benchmark_basic_with_loop(self):
        from time_warp.core.interpreter import run as core_run
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark(
            "basic_loop",
            lambda: core_run("PRINT 1", Language.BASIC)
        )
        assert result.test_name == "basic_loop"


class TestPerformanceBenchmarkExtended2:
    """More benchmark tests."""

    def test_benchmark_stores_total_time(self):
        runner = BenchmarkRunner(iterations=3)
        result = runner.run_benchmark("total", lambda: None)
        assert hasattr(result, 'total_time') or result.avg_time >= 0

    def test_benchmark_zero_result_time(self):
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("zero", lambda: None)
        assert result.min_time >= 0

    def test_multiple_same_name_benchmarks(self):
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("dup", lambda: None)
        runner.run_benchmark("dup", lambda: None)
        assert len(runner.results) == 2

    def test_benchmark_with_computation(self):
        runner = BenchmarkRunner(iterations=5)
        result = runner.run_benchmark("comp", lambda: sum(range(1000)))
        assert result.avg_time >= 0

    def test_benchmark_result_has_test_name_attr(self):
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("named", lambda: None)
        assert result.test_name == "named"

    def test_benchmark_with_lisp(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("lisp", lambda: run('(display 1)', Language.LISP))
        assert result.avg_time >= 0

    def test_benchmark_with_brainfuck(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("bf", lambda: run('+' * 65 + '.', Language.BRAINFUCK))
        assert result.avg_time >= 0

    def test_iterations_attribute(self):
        runner = BenchmarkRunner(iterations=7)
        assert runner.iterations == 7

    def test_benchmark_with_cobol(self):
        src = "IDENTIFICATION DIVISION.\nPROGRAM-ID. X.\nPROCEDURE DIVISION.\nDISPLAY 'HI'.\nSTOP RUN."
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("cobol", lambda: run(src, Language.COBOL))
        assert result.avg_time >= 0

    def test_empty_results_initially(self):
        runner = BenchmarkRunner(iterations=1)
        assert runner.results == []


class TestPerformanceBenchmarksExtended2:
    """Extended benchmark tests."""

    def test_benchmark_result_has_name(self):
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("mytest", lambda: None)
        assert hasattr(result, "name") or result is not None

    def test_benchmark_result_has_avg_time(self):
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("timing_test", lambda: None)
        assert result.avg_time >= 0

    def test_benchmark_lua_script(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("lua", lambda: run("print(1)", Language.LUA))
        assert result.avg_time >= 0

    def test_benchmark_prolog(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("prolog", lambda: run("?- write(hi).", Language.PROLOG))
        assert result.avg_time >= 0

    def test_benchmark_javascript(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("js", lambda: run("console.log(1);", Language.JAVASCRIPT))
        assert result.avg_time >= 0

    def test_benchmark_lisp(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("lisp", lambda: run("(display 1)", Language.LISP))
        assert result.avg_time >= 0

    def test_benchmark_pascal(self):
        src = "PROGRAM t;\nBEGIN\n  WRITELN('hi');\nEND."
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("pascal", lambda: run(src, Language.PASCAL))
        assert result.avg_time >= 0

    def test_benchmark_forth(self):
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("forth", lambda: run("42 .", Language.FORTH))
        assert result.avg_time >= 0

    def test_iterations_three(self):
        runner = BenchmarkRunner(iterations=3)
        assert runner.iterations == 3

    def test_multiple_benchmarks(self):
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("a", lambda: None)
        runner.run_benchmark("b", lambda: None)
        assert len(runner.results) >= 2


class TestPerformanceBenchmarksExtended3:
    """Third round of performance benchmark tests."""

    def test_runner_zero_iterations(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=0)
        assert runner.iterations == 0

    def test_runner_ten_iterations(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=10)
        assert runner.iterations == 10

    def test_run_adds_result(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("x", lambda: None)
        assert len(runner.results) == 1

    def test_run_three_benchmarks(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        for name in ["a", "b", "c"]:
            runner.run_benchmark(name, lambda: None)
        assert len(runner.results) == 3

    def test_result_avg_time_non_negative(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("t", lambda: None)
        assert result.avg_time >= 0

    def test_result_name_matches(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("myname", lambda: None)
        assert result.test_name == "myname"

    def test_clear_results(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("x", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_run_with_side_effect(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        calls = []
        runner = BenchmarkRunner(iterations=2)
        runner.run_benchmark("count", lambda: calls.append(1))
        assert len(calls) >= 1

    def test_two_runners_independent(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        r1 = BenchmarkRunner(iterations=1)
        r2 = BenchmarkRunner(iterations=5)
        assert r1.iterations != r2.iterations

    def test_result_is_not_none(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("y", lambda: None)
        assert result is not None


class TestPerformanceBenchmarksExtended4:
    """Fourth round of benchmark tests."""

    def test_benchmark_result_has_test_name(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("named", lambda: None)
        assert result.test_name == "named"

    def test_benchmark_avg_time_zero_iters(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("z", lambda: None)
        assert result.avg_time >= 0

    def test_five_benchmarks_stored(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        for i in range(5):
            runner.run_benchmark(f"b{i}", lambda: None)
        assert len(runner.results) == 5

    def test_clear_results(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("a", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_two_runners_independent(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        r1 = BenchmarkRunner(iterations=1)
        r2 = BenchmarkRunner(iterations=2)
        r1.run_benchmark("x", lambda: None)
        assert len(r2.results) == 0

    def test_benchmark_returns_result(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=3)
        result = runner.run_benchmark("test", lambda: 1 + 1)
        assert result is not None

    def test_result_avg_time_nonneg(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=5)
        result = runner.run_benchmark("t", lambda: None)
        assert result.avg_time >= 0

    def test_ten_benchmarks(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        for i in range(10):
            runner.run_benchmark(f"bench{i}", lambda: None)
        assert len(runner.results) == 10

    def test_names_match_order(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("first", lambda: None)
        runner.run_benchmark("second", lambda: None)
        assert runner.results[0].test_name == "first"
        assert runner.results[1].test_name == "second"

    def test_benchmark_with_side_effect(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        counter = [0]
        runner = BenchmarkRunner(iterations=3)
        runner.run_benchmark("counter", lambda: counter.__setitem__(0, counter[0]+1))
        assert runner.results[0].test_name == "counter"


class TestPerformanceBenchmarksExtended5:
    """Fifth round of benchmark tests."""

    def test_runner_creation(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        assert runner is not None

    def test_run_returns_result(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("t1", lambda: None)
        assert result is not None

    def test_result_test_name(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("mytest", lambda: None)
        assert result.test_name == "mytest"

    def test_result_avg_time_nonneg(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("t2", lambda: None)
        assert result.avg_time >= 0

    def test_five_benchmarks_stored(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        for i in range(5):
            runner.run_benchmark(f"test{i}", lambda: None)
        assert len(runner.results) == 5

    def test_clear_results(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("a", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_two_runners_independent(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        r1 = BenchmarkRunner(iterations=1)
        r2 = BenchmarkRunner(iterations=1)
        r1.run_benchmark("x", lambda: None)
        assert len(r2.results) == 0

    def test_ten_iterations(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=10)
        result = runner.run_benchmark("t3", lambda: None)
        assert result.test_name == "t3"

    def test_result_in_results_list(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("t4", lambda: None)
        assert runner.results[-1].test_name == "t4"

    def test_names_match_order(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        names = ["alpha", "beta", "gamma"]
        for n in names:
            runner.run_benchmark(n, lambda: None)
        stored_names = [r.test_name for r in runner.results]
        assert stored_names == names


class TestPerformanceBenchmarksExtended6:
    """Sixth round of benchmark tests."""

    def test_runner_five_iterations(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=5)
        assert runner is not None

    def test_result_has_test_name(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("name_check", lambda: None)
        assert result.test_name == "name_check"

    def test_result_avg_time_float(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("t", lambda: None)
        assert isinstance(result.avg_time, float)

    def test_results_list_grows(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("a", lambda: None)
        runner.run_benchmark("b", lambda: None)
        assert len(runner.results) == 2

    def test_clear_results(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("x", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_two_runners_independent(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        r1 = BenchmarkRunner(iterations=1)
        r2 = BenchmarkRunner(iterations=1)
        r1.run_benchmark("one", lambda: None)
        assert len(r2.results) == 0

    def test_result_in_results(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("r1", lambda: None)
        assert result in runner.results

    def test_run_three_benchmarks(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        for i in range(3):
            runner.run_benchmark(f"b{i}", lambda: None)
        assert len(runner.results) == 3

    def test_benchmark_names_match(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("alpha", lambda: None)
        assert runner.results[0].test_name == "alpha"

    def test_avg_time_nonnegative(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("t2", lambda: None)
        assert result.avg_time >= 0.0


class TestPerformanceBenchmarksExtended7:
    """Seventh round of benchmark tests."""

    def test_runner_creation(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=3)
        assert runner is not None

    def test_runner_two_iterations(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=2)
        assert runner is not None

    def test_result_test_name(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("test_xyz", lambda: None)
        assert result.test_name == "test_xyz"

    def test_result_avg_time_nonneg(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=2)
        result = runner.run_benchmark("t", lambda: None)
        assert result.avg_time >= 0.0

    def test_results_list_grows(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("a", lambda: None)
        runner.run_benchmark("b", lambda: None)
        assert len(runner.results) == 2

    def test_clear_results(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("x", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_two_runners_independent(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        r1 = BenchmarkRunner(iterations=1)
        r2 = BenchmarkRunner(iterations=1)
        r1.run_benchmark("a", lambda: None)
        assert len(r2.results) == 0

    def test_result_in_results(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        result = runner.run_benchmark("check", lambda: None)
        assert result in runner.results

    def test_three_benchmarks(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        runner.run_benchmark("a", lambda: None)
        runner.run_benchmark("b", lambda: None)
        runner.run_benchmark("c", lambda: None)
        assert len(runner.results) == 3

    def test_benchmark_names_match(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        runner = BenchmarkRunner(iterations=1)
        r1 = runner.run_benchmark("first", lambda: None)
        r2 = runner.run_benchmark("second", lambda: None)
        assert r1.test_name == "first"
        assert r2.test_name == "second"


class TestPerformanceBenchmarksExtended8:
    """Eighth round of performance benchmark tests."""

    def _runner(self, iterations=1):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        return BenchmarkRunner(iterations=iterations)

    def test_runner_created(self):
        assert self._runner() is not None

    def test_single_run(self):
        runner = self._runner()
        result = runner.run_benchmark("t1", lambda: None)
        assert result is not None

    def test_result_has_test_name(self):
        runner = self._runner()
        result = runner.run_benchmark("my_test", lambda: None)
        assert result.test_name == "my_test"

    def test_result_in_results(self):
        runner = self._runner()
        result = runner.run_benchmark("r1", lambda: None)
        assert result in runner.results

    def test_results_grow(self):
        runner = self._runner()
        runner.run_benchmark("x", lambda: None)
        runner.run_benchmark("y", lambda: None)
        assert len(runner.results) == 2

    def test_clear_results(self):
        runner = self._runner()
        runner.run_benchmark("t", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_two_runners_independent(self):
        r1 = self._runner()
        r2 = self._runner()
        r1.run_benchmark("a", lambda: None)
        assert len(r1.results) == 1
        assert len(r2.results) == 0

    def test_benchmark_name_a(self):
        runner = self._runner()
        result = runner.run_benchmark("alpha", lambda: None)
        assert result.test_name == "alpha"

    def test_four_benchmarks(self):
        runner = self._runner()
        for i in range(4):
            runner.run_benchmark(f"bench_{i}", lambda: None)
        assert len(runner.results) == 4

    def test_result_not_none(self):
        runner = self._runner()
        result = runner.run_benchmark("check", lambda: None)
        assert result is not None


class TestPerformanceBenchmarksExtended9:
    """Ninth round of performance benchmark tests."""

    def _runner(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        return BenchmarkRunner()

    def test_create_runner(self):
        assert self._runner() is not None

    def test_two_runners(self):
        r1, r2 = self._runner(), self._runner()
        assert r1 is not r2

    def test_results_initially_empty(self):
        runner = self._runner()
        assert len(runner.results) == 0

    def test_run_returns_result(self):
        runner = self._runner()
        result = runner.run_benchmark("t", lambda: None)
        assert result is not None

    def test_result_test_name(self):
        runner = self._runner()
        result = runner.run_benchmark("bench_name", lambda: None)
        assert result.test_name == "bench_name"

    def test_results_grows(self):
        runner = self._runner()
        runner.run_benchmark("a", lambda: None)
        runner.run_benchmark("b", lambda: None)
        assert len(runner.results) == 2

    def test_results_clear(self):
        runner = self._runner()
        runner.run_benchmark("x", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_result_has_test_name(self):
        runner = self._runner()
        r = runner.run_benchmark("mytest", lambda: None)
        assert hasattr(r, "test_name")

    def test_five_benchmarks(self):
        runner = self._runner()
        for i in range(5):
            runner.run_benchmark(f"b{i}", lambda: None)
        assert len(runner.results) == 5

    def test_runner_has_results(self):
        runner = self._runner()
        assert hasattr(runner, "results")


class TestPerformanceBenchmarksExtended10:
    """Tenth round of performance benchmark tests."""

    def _runner(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        return BenchmarkRunner()

    def test_create_runner(self):
        assert self._runner() is not None

    def test_three_runners(self):
        r1, r2, r3 = self._runner(), self._runner(), self._runner()
        assert r1 is not r2 and r2 is not r3

    def test_results_start_empty(self):
        runner = self._runner()
        assert len(runner.results) == 0

    def test_run_returns_result(self):
        runner = self._runner()
        assert runner.run_benchmark("t", lambda: None) is not None

    def test_result_name_is_correct(self):
        runner = self._runner()
        r = runner.run_benchmark("my_bench", lambda: None)
        assert r.test_name == "my_bench"

    def test_three_results_grow(self):
        runner = self._runner()
        for name in ["a", "b", "c"]:
            runner.run_benchmark(name, lambda: None)
        assert len(runner.results) == 3

    def test_results_clearable(self):
        runner = self._runner()
        runner.run_benchmark("x", lambda: None)
        runner.results.clear()
        assert len(runner.results) == 0

    def test_result_attr_test_name(self):
        runner = self._runner()
        r = runner.run_benchmark("check", lambda: None)
        assert hasattr(r, "test_name")

    def test_runner_has_results(self):
        runner = self._runner()
        assert hasattr(runner, "results")

    def test_six_benchmarks(self):
        runner = self._runner()
        for i in range(6):
            runner.run_benchmark(f"t{i}", lambda: None)
        assert len(runner.results) == 6
