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

        def benchmark_python():
            # Simulate Python execution
            result = run("print(1 + 1)", Language.PYTHON)
            assert no_errors(result)

        runner.run_benchmark("basic_execution", benchmark_basic)
        runner.run_benchmark("python_execution", benchmark_python)

    def test_memory_usage(self):
        """Benchmark memory usage during execution."""
        runner = BenchmarkRunner(iterations=50)

        def memory_test():
            # Simulate memory usage during execution
            result = run("FOR I = 1 TO 1000: NEXT I", Language.BASIC)
            assert no_errors(result)

        runner.run_benchmark("memory_usage", memory_test)
