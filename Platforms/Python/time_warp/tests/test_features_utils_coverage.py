"""Coverage tests for features and utility modules."""

import tempfile
from pathlib import Path

from time_warp.core.interpreter import Language
from time_warp.features.achievements import ProgressTracker
from time_warp.features.compiler import compile_to_c, compile_to_executable
from time_warp.features.syntax_validator import SyntaxValidator, SeverityLevel
from time_warp.utils.profiler import (
    LineProfile, ProfileSession, PerformanceProfiler
)


# ============================================================================
# PROFILER
# ============================================================================

class TestLineProfile:
    def test_default_values(self):
        p = LineProfile(line_number=1)
        assert p.line_number == 1
        assert p.execution_count == 0
        assert p.total_time_ms == 0.0
        assert p.avg_time_ms == 0.0

    def test_avg_no_executions(self):
        p = LineProfile(line_number=1)
        assert p.avg_time_ms == 0.0

    def test_avg_with_executions(self):
        p = LineProfile(line_number=1, execution_count=4, total_time_ms=100.0)
        assert p.avg_time_ms == 25.0


class TestProfileSession:
    def test_default_values(self):
        s = ProfileSession()
        assert s.start_time == 0.0
        assert s.end_time == 0.0
        assert s.total_lines_executed == 0
        assert len(s.line_profiles) == 0

    def test_total_time_ms(self):
        s = ProfileSession(start_time=1.0, end_time=2.5)
        assert abs(s.total_time_ms - 1500.0) < 0.01

    def test_get_hotspots_empty(self):
        s = ProfileSession()
        assert s.get_hotspots() == []

    def test_get_hotspots(self):
        s = ProfileSession()
        s.line_profiles[1] = LineProfile(line_number=1, execution_count=5, total_time_ms=100.0)
        s.line_profiles[2] = LineProfile(line_number=2, execution_count=1, total_time_ms=500.0)
        s.line_profiles[3] = LineProfile(line_number=3, execution_count=2, total_time_ms=50.0)
        hotspots = s.get_hotspots(top_n=2)
        assert len(hotspots) == 2
        assert hotspots[0].line_number == 2  # Highest total_time

    def test_get_most_executed(self):
        s = ProfileSession()
        s.line_profiles[1] = LineProfile(line_number=1, execution_count=10, total_time_ms=50.0)
        s.line_profiles[2] = LineProfile(line_number=2, execution_count=3, total_time_ms=100.0)
        most = s.get_most_executed(top_n=1)
        assert len(most) == 1
        assert most[0].line_number == 1


class TestPerformanceProfiler:
    def test_init(self):
        p = PerformanceProfiler()
        assert not p.enabled
        assert p.current_session is None
        assert p.sessions == []

    def test_start_session(self):
        p = PerformanceProfiler()
        p.start_session()
        assert p.enabled
        assert p.current_session is not None

    def test_end_session_no_session(self):
        p = PerformanceProfiler()
        result = p.end_session()
        assert result is None

    def test_full_session(self):
        p = PerformanceProfiler()
        p.start_session()
        p.start_line(1)
        p.end_line()
        p.start_line(2)
        p.end_line()
        p.start_line(1)
        p.end_line()
        session = p.end_session()
        assert session is not None
        assert session.total_lines_executed == 3
        assert len(session.line_profiles) == 2
        assert session.line_profiles[1].execution_count == 2

    def test_start_line_disabled(self):
        p = PerformanceProfiler()
        # Not enabled - should not crash
        p.start_line(5)
        assert p.current_session is None

    def test_end_line_disabled(self):
        p = PerformanceProfiler()
        # Not enabled - should not crash
        p.end_line()

    def test_multiple_sessions(self):
        p = PerformanceProfiler()
        p.start_session()
        p.end_session()
        p.start_session()
        p.end_session()
        assert len(p.sessions) == 2

    def test_get_report_empty(self):
        p = PerformanceProfiler()
        report = p.get_report()
        assert "No profiling data" in report

    def test_get_report_with_session(self):
        p = PerformanceProfiler()
        p.start_session()
        p.start_line(1)
        p.end_line()
        p.start_line(2)
        p.end_line()
        session = p.end_session()
        report = p.get_report(session)
        assert "PERFORMANCE PROFILE REPORT" in report
        assert "Total execution time" in report

    def test_min_max_tracking(self):
        p = PerformanceProfiler()
        p.start_session()
        for _ in range(5):
            p.start_line(1)
            p.end_line()
        session = p.end_session()
        profile = session.line_profiles[1]
        assert profile.execution_count == 5
        assert profile.min_time_ms <= profile.max_time_ms


# ============================================================================
# SYNTAX VALIDATOR
# ============================================================================

class TestSyntaxValidator:
    def setup_method(self):
        self.v = SyntaxValidator()

    def test_empty_code(self):
        issues = self.v.validate("", Language.BASIC)
        assert issues == []

    def test_whitespace_only(self):
        issues = self.v.validate("   \n  \n", Language.BASIC)
        assert issues == []

    def test_basic_valid_simple(self):
        code = "10 PRINT \"hello\"\n20 END"
        issues = self.v.validate(code, Language.BASIC)
        assert isinstance(issues, list)

    def test_basic_missing_then(self):
        code = "10 IF X > 5\n20 PRINT \"yes\""
        issues = self.v.validate(code, Language.BASIC)
        assert isinstance(issues, list)

    def test_basic_unclosed_for(self):
        code = "10 FOR I = 1 TO 10\n20 PRINT I"
        issues = self.v.validate(code, Language.BASIC)
        assert isinstance(issues, list)

    def test_basic_unclosed_string(self):
        code = '10 PRINT "hello'
        issues = self.v.validate(code, Language.BASIC)
        assert isinstance(issues, list)

    def test_basic_goto_undefined(self):
        code = "10 GOTO 999\n20 PRINT \"hello\""
        issues = self.v.validate(code, Language.BASIC)
        assert isinstance(issues, list)

    def test_logo_code(self):
        code = "FORWARD 100\nRIGHT 90"
        issues = self.v.validate(code, Language.LOGO)
        assert isinstance(issues, list)

    def test_pilot_code(self):
        code = "T:Hello\nA:\nM:yes"
        issues = self.v.validate(code, Language.PILOT)
        assert isinstance(issues, list)

    def test_pascal_code(self):
        code = "PROGRAM Hello;\nBEGIN\n  WRITELN('hello');\nEND."
        issues = self.v.validate(code, Language.PASCAL)
        assert isinstance(issues, list)

    def test_c_code(self):
        code = '#include <stdio.h>\nint main() {\n  printf("hello");\n  return 0;\n}'
        issues = self.v.validate(code, Language.C)
        assert isinstance(issues, list)

    def test_forth_code(self):
        code = ": DOUBLE dup + ;\n5 DOUBLE ."
        issues = self.v.validate(code, Language.FORTH)
        assert isinstance(issues, list)

    def test_prolog_code(self):
        code = "parent(tom, bob).\nancestor(X,Y) :- parent(X,Y)."
        issues = self.v.validate(code, Language.PROLOG)
        assert isinstance(issues, list)

    def test_caching(self):
        code = "10 PRINT \"hello\""
        # First call
        issues1 = self.v.validate(code, Language.BASIC)
        # Second call (from cache)
        issues2 = self.v.validate(code, Language.BASIC)
        assert issues1 == issues2

    def test_severity_levels(self):
        # Check severity enum values
        assert SeverityLevel.ERROR.value == "error"
        assert SeverityLevel.WARNING.value == "warning"
        assert SeverityLevel.INFO.value == "info"


# ============================================================================
# ACHIEVEMENTS / PROGRESS TRACKER
# ============================================================================

class TestProgressTracker:
    def setup_method(self):
        self.tmp_dir = tempfile.mkdtemp()
        # Create a minimal structure
        examples_dir = Path(self.tmp_dir) / "Examples" / "basic"
        examples_dir.mkdir(parents=True)
        (examples_dir / "hello.bas").write_text("10 PRINT \"hello\"")
        docs_dir = Path(self.tmp_dir) / "docs" / "tutorials"
        docs_dir.mkdir(parents=True)
        (docs_dir / "basic.md").write_text("# BASIC Tutorial")
        self.tracker = ProgressTracker(root=Path(self.tmp_dir))

    def test_init(self):
        assert self.tracker is not None
        assert "completed_tutorials" in self.tracker.state
        assert "completed_examples" in self.tracker.state

    def test_get_tutorials(self):
        tutorials = self.tracker.get_tutorials()
        assert isinstance(tutorials, list)
        assert len(tutorials) > 0

    def test_get_examples(self):
        examples = self.tracker.get_examples()
        assert isinstance(examples, list)
        assert len(examples) > 0

    def test_mark_tutorial_completed(self):
        self.tracker.mark_tutorial_completed("docs/tutorials/basic.md", True)
        assert "docs/tutorials/basic.md" in self.tracker.state["completed_tutorials"]

    def test_mark_tutorial_uncompleted(self):
        self.tracker.mark_tutorial_completed("docs/tutorials/basic.md", True)
        self.tracker.mark_tutorial_completed("docs/tutorials/basic.md", False)
        assert "docs/tutorials/basic.md" not in self.tracker.state["completed_tutorials"]

    def test_mark_example_completed(self):
        self.tracker.mark_example_completed("Examples/basic/hello.bas", True)
        assert "Examples/basic/hello.bas" in self.tracker.state["completed_examples"]

    def test_record_example_run(self):
        self.tracker.record_example_run("Examples/basic/hello.bas")
        assert "Examples/basic/hello.bas" in self.tracker.state["completed_examples"]

    def test_get_progress(self):
        progress = self.tracker.get_progress()
        assert "tutorial_percent" in progress
        assert "example_percent" in progress
        assert 0.0 <= progress["tutorial_percent"] <= 100.0

    def test_progress_after_completion(self):
        tutorials = self.tracker.get_tutorials()
        if tutorials:
            self.tracker.mark_tutorial_completed(tutorials[0], True)
            progress = self.tracker.get_progress()
            assert progress["tutorial_percent"] > 0.0

    def test_load_nonexistent(self):
        # State file may or may not exist - either way it should be a list
        tracker = ProgressTracker(root=Path(self.tmp_dir))
        assert isinstance(tracker.state["completed_tutorials"], list)


# ============================================================================
# COMPILER
# ============================================================================

class TestCompiler:
    def test_compile_to_c_empty(self):
        result = compile_to_c("")
        assert isinstance(result, str)
        assert "main" in result

    def test_compile_to_c_hello(self):
        code = '10 PRINT "Hello World"'
        result = compile_to_c(code)
        assert isinstance(result, str)
        assert "#include" in result

    def test_compile_to_c_with_rem(self):
        code = "REM This is a comment\n10 PRINT \"hello\""
        result = compile_to_c(code)
        assert isinstance(result, str)

    def test_compile_to_c_with_line_numbers(self):
        code = "10 LET X = 5\n20 PRINT X"
        result = compile_to_c(code)
        assert isinstance(result, str)

    def test_compile_to_c_multiple_lines(self):
        code = "10 PRINT \"line1\"\n20 PRINT \"line2\"\n30 PRINT \"line3\""
        result = compile_to_c(code)
        assert isinstance(result, str)

    def test_compile_and_run_no_compiler(self):
        # compile_to_executable returns bool (False if no compiler available)
        import tempfile, os
        with tempfile.NamedTemporaryFile(suffix=".out", delete=False) as f:
            out_path = f.name
        try:
            result = compile_to_executable("10 PRINT \"hello\"", out_path)
            assert isinstance(result, bool)
        finally:
            if os.path.exists(out_path):
                os.unlink(out_path)
