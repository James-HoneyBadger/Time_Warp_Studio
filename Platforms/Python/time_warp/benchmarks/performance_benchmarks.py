"""
Performance Benchmarking Suite for Phase VII-X

Provides:
- Marketplace performance benchmarks
- Debugger performance metrics
- AI intelligence performance tests
- System integration benchmarks
- Results reporting
"""

import json
import logging
import statistics
import time
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Callable, List, Optional


def utc_now() -> datetime:
    return datetime.now(timezone.utc)

# ===== DATA CLASSES =====


@dataclass
class BenchmarkResult:
    """Single benchmark execution result"""

    test_name: str
    iterations: int
    total_time: float
    min_time: float
    max_time: float
    avg_time: float
    std_dev: float
    throughput: float  # iterations per second
    timestamp: datetime = field(default_factory=utc_now)


@dataclass
class BenchmarkSuite:
    """Collection of benchmark results"""

    suite_name: str
    results: List[BenchmarkResult] = field(default_factory=list)
    start_time: datetime = field(default_factory=utc_now)
    end_time: Optional[datetime] = None
    total_duration: float = 0.0


# ===== BENCHMARK RUNNER =====


class BenchmarkRunner:
    """Execute and track benchmarks"""

    def __init__(self, iterations: int = 100):
        self.iterations = iterations
        self.logger = logging.getLogger(__name__)
        self.results: List[BenchmarkResult] = []

    def run_benchmark(
        self,
        test_name: str,
        test_func: Callable,
        setup: Optional[Callable] = None,
        teardown: Optional[Callable] = None,
    ) -> BenchmarkResult:
        """Run a single benchmark"""
        times: List[float] = []

        for i in range(self.iterations):
            # Setup
            if setup:
                setup()

            # Time the test
            start = time.perf_counter()
            test_func()
            elapsed = time.perf_counter() - start
            times.append(elapsed)

            # Teardown
            if teardown:
                teardown()

        # Calculate statistics
        total_time = sum(times)
        min_time = min(times)
        max_time = max(times)
        avg_time = statistics.mean(times)
        std_dev = statistics.stdev(times) if len(times) > 1 else 0.0
        throughput = self.iterations / total_time

        result = BenchmarkResult(
            test_name=test_name,
            iterations=self.iterations,
            total_time=total_time,
            min_time=min_time,
            max_time=max_time,
            avg_time=avg_time,
            std_dev=std_dev,
            throughput=throughput,
        )

        self.results.append(result)

        self.logger.info(
            f"{test_name}: avg={avg_time * 1000:.2f}ms, "
            f"min={min_time * 1000:.2f}ms, max={max_time * 1000:.2f}ms"
        )

        return result

    def run_suite(self, name: str) -> BenchmarkSuite:
        """Create a benchmark suite"""
        suite = BenchmarkSuite(suite_name=name)
        suite.results = self.results
        suite.end_time = utc_now()
        suite.total_duration = (suite.end_time - suite.start_time).total_seconds()
        return suite


# ===== MARKETPLACE BENCHMARKS =====


class MarketplaceBenchmarks:
    """Benchmark marketplace operations"""

    def __init__(self):
        self.runner = BenchmarkRunner(iterations=50)
        self.logger = logging.getLogger(__name__)

    def benchmark_plugin_search(self) -> BenchmarkResult:
        """Benchmark plugin search"""
        from marketplace.plugin_marketplace import MarketplaceService

        service = MarketplaceService()

        def test():
            service.search_plugins("test", limit=10)

        return self.runner.run_benchmark("marketplace_search", test)

    def benchmark_plugin_rating(self) -> BenchmarkResult:
        """Benchmark rating calculation"""
        from marketplace.plugin_marketplace import (
            MarketplaceService,
            PluginReview,
        )

        service = MarketplaceService()

        def test():
            review = PluginReview(
                plugin_id="test",
                user_id="user1",
                rating=5,
                title="Great!",
                content="This is excellent",
            )
            service.submit_review("test", review)

        return self.runner.run_benchmark("marketplace_rating", test)

    def run_all(self) -> BenchmarkSuite:
        """Run all marketplace benchmarks"""
        self.benchmark_plugin_search()
        self.benchmark_plugin_rating()

        return self.runner.run_suite("Marketplace")


# ===== DEBUGGER BENCHMARKS =====


class DebuggerBenchmarks:
    """Benchmark debugger operations"""

    def __init__(self):
        self.runner = BenchmarkRunner(iterations=100)
        self.logger = logging.getLogger(__name__)

    def benchmark_breakpoint_creation(self) -> BenchmarkResult:
        """Benchmark breakpoint creation"""
        from debugging.integrated_debugger import DebuggerEngine

        engine = DebuggerEngine()
        counter = [0]

        def test():
            counter[0] += 1
            engine.create_breakpoint(file="test.bas", line=counter[0], condition=None)

        return self.runner.run_benchmark("debugger_breakpoint", test)

    def benchmark_expression_evaluation(self) -> BenchmarkResult:
        """Benchmark expression evaluation"""
        from debugging.integrated_debugger import DebuggerEngine

        engine = DebuggerEngine()

        def test():
            engine.evaluate_expression("x + y", {"x": 10, "y": 20})

        return self.runner.run_benchmark("debugger_expression", test)

    def run_all(self) -> BenchmarkSuite:
        """Run all debugger benchmarks"""
        self.benchmark_breakpoint_creation()
        self.benchmark_expression_evaluation()

        return self.runner.run_suite("Debugger")


# ===== AI BENCHMARKS =====


class AIBenchmarks:
    """Benchmark AI intelligence operations"""

    def __init__(self):
        self.runner = BenchmarkRunner(iterations=50)
        self.logger = logging.getLogger(__name__)

    def benchmark_code_completion(self) -> BenchmarkResult:
        """Benchmark code completion"""
        from ai.intelligence_engine import CodeCompletionEngine

        engine = CodeCompletionEngine()

        def test():
            engine.suggest_completion("IF", 0, "basic")

        return self.runner.run_benchmark("ai_completion", test)

    def benchmark_bug_detection(self) -> BenchmarkResult:
        """Benchmark bug detection"""
        from ai.intelligence_engine import BugDetectionEngine

        engine = BugDetectionEngine()
        code = "IF x > 10 THEN\nPRINT x\nEND IF"

        def test():
            engine.analyze_code(code, "basic")

        return self.runner.run_benchmark("ai_bug_detection", test)

    def benchmark_learning_path(self) -> BenchmarkResult:
        """Benchmark learning path generation"""
        from ai.intelligence_engine import LearningLevel, LearningPathGenerator

        generator = LearningPathGenerator()
        counter = [0]

        def test():
            counter[0] += 1
            generator.create_learning_path(f"user_{counter[0]}", LearningLevel.BEGINNER)

        return self.runner.run_benchmark("ai_learning_path", test)

    def run_all(self) -> BenchmarkSuite:
        """Run all AI benchmarks"""
        self.benchmark_code_completion()
        self.benchmark_bug_detection()
        self.benchmark_learning_path()

        return self.runner.run_suite("AI Intelligence")


# ===== INTEGRATION BENCHMARKS =====


class IntegrationBenchmarks:
    """Benchmark full system integration"""

    def __init__(self):
        self.runner = BenchmarkRunner(iterations=25)
        self.logger = logging.getLogger(__name__)

    def benchmark_component_registration(self) -> BenchmarkResult:
        """Benchmark component registration"""
        from integration.integration_manager import (
            ComponentMetadata,
            ComponentType,
            IntegrationManager,
        )

        manager = IntegrationManager()
        counter = [0]

        def test():
            counter[0] += 1
            metadata = ComponentMetadata(
                name=f"Component {counter[0]}", type=ComponentType.MARKETPLACE
            )
            manager.register_component(f"comp_{counter[0]}", metadata)

        return self.runner.run_benchmark("integration_registration", test)

    def benchmark_metric_recording(self) -> BenchmarkResult:
        """Benchmark metric recording"""
        from integration.integration_manager import (
            IntegrationManager,
            PerformanceMetric,
        )

        manager = IntegrationManager()
        counter = [0]

        def test():
            counter[0] += 1
            metric = PerformanceMetric(
                component="test",
                metric_name="latency",
                value=float(counter[0]),
                unit="ms",
            )
            manager.record_metric(metric)

        return self.runner.run_benchmark("integration_metrics", test)

    def run_all(self) -> BenchmarkSuite:
        """Run all integration benchmarks"""
        self.benchmark_component_registration()
        self.benchmark_metric_recording()

        return self.runner.run_suite("Integration")


# ===== BENCHMARK REPORT =====


class BenchmarkReport:
    """Generate and display benchmark reports"""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    @staticmethod
    def format_result(result: BenchmarkResult) -> str:
        """Format a single result for display"""
        return (
            f"\n  {result.test_name}:\n"
            f"    Iterations: {result.iterations}\n"
            f"    Total:      {result.total_time * 1000:.2f}ms\n"
            f"    Avg:        {result.avg_time * 1000:.2f}ms\n"
            f"    Min:        {result.min_time * 1000:.2f}ms\n"
            f"    Max:        {result.max_time * 1000:.2f}ms\n"
            f"    Std Dev:    {result.std_dev * 1000:.2f}ms\n"
            f"    Throughput: {result.throughput:.0f} ops/sec"
        )

    @staticmethod
    def format_suite(suite: BenchmarkSuite) -> str:
        """Format complete suite"""
        output = f"\n{'=' * 60}\n"
        output += f"Benchmark Suite: {suite.suite_name}\n"
        output += f"Start Time: {suite.start_time.isoformat()}\n"
        output += f"Total Duration: {suite.total_duration:.2f}s\n"
        output += f"{'=' * 60}"

        for result in suite.results:
            output += BenchmarkReport.format_result(result)

        output += f"\n{'=' * 60}\n"
        return output

    @staticmethod
    def export_json(suites: List[BenchmarkSuite], filepath: str):
        """Export results to JSON"""
        data = {
            "suites": [
                {
                    "name": suite.suite_name,
                    "start_time": suite.start_time.isoformat(),
                    "total_duration": suite.total_duration,
                    "results": [
                        {
                            "test_name": result.test_name,
                            "iterations": result.iterations,
                            "total_time": result.total_time,
                            "avg_time": result.avg_time,
                            "min_time": result.min_time,
                            "max_time": result.max_time,
                            "std_dev": result.std_dev,
                            "throughput": result.throughput,
                        }
                        for result in suite.results
                    ],
                }
                for suite in suites
            ]
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)


# ===== MAIN RUNNER =====


def run_all_benchmarks() -> List[BenchmarkSuite]:
    """Run all benchmark suites"""
    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger(__name__)

    suites = []

    # Marketplace benchmarks
    logger.info("Running Marketplace benchmarks...")
    mp_bench = MarketplaceBenchmarks()
    try:
        suites.append(mp_bench.run_all())
    except Exception as e:
        logger.error("Marketplace benchmarks failed: %s", e)

    # Debugger benchmarks
    logger.info("Running Debugger benchmarks...")
    db_bench = DebuggerBenchmarks()
    try:
        suites.append(db_bench.run_all())
    except Exception as e:
        logger.error("Debugger benchmarks failed: %s", e)

    # AI benchmarks
    logger.info("Running AI Intelligence benchmarks...")
    ai_bench = AIBenchmarks()
    try:
        suites.append(ai_bench.run_all())
    except Exception as e:
        logger.error("AI benchmarks failed: %s", e)

    # Integration benchmarks
    logger.info("Running Integration benchmarks...")
    int_bench = IntegrationBenchmarks()
    try:
        suites.append(int_bench.run_all())
    except Exception as e:
        logger.error("Integration benchmarks failed: %s", e)

    return suites


if __name__ == "__main__":
    suites = run_all_benchmarks()

    # Print reports
    for suite in suites:
        print(BenchmarkReport.format_suite(suite))

    # Export to JSON
    BenchmarkReport.export_json(suites, "benchmark_results.json")

    print("\nBenchmark results saved to benchmark_results.json")
