"""Coverage tests for beta_testing_framework.py and ide_hooks.py."""
from __future__ import annotations

import pytest


# ---------------------------------------------------------------------------
# beta_testing_framework.py
# ---------------------------------------------------------------------------

class TestBugReport:
    def test_create_valid(self):
        from time_warp.features.beta_testing_framework import BugReport, Severity
        bug = BugReport(title="Crash", description="App crashes on start",
                        severity=Severity.CRITICAL, component="core",
                        reporter="alice")
        assert bug.title == "Crash"
        assert bug.status == "open"
        assert bug.id is not None

    def test_missing_title_raises(self):
        from time_warp.features.beta_testing_framework import BugReport
        with pytest.raises(ValueError):
            BugReport(title="", description="some desc")

    def test_missing_description_raises(self):
        from time_warp.features.beta_testing_framework import BugReport
        with pytest.raises(ValueError):
            BugReport(title="Bug", description="")

    def test_defaults(self):
        from time_warp.features.beta_testing_framework import BugReport, Severity
        bug = BugReport(title="Test", description="Desc")
        assert bug.severity == Severity.MEDIUM
        assert bug.status == "open"
        assert bug.reproducible is False

    def test_all_severity_levels(self):
        from time_warp.features.beta_testing_framework import Severity
        severities = [s for s in Severity]
        assert len(severities) >= 3


class TestUserFeedback:
    def test_create_valid(self):
        from time_warp.features.beta_testing_framework import UserFeedback, FeedbackType
        fb = UserFeedback(type=FeedbackType.BUG_REPORT, title="Found a bug",
                          content="App crashes", rating=3, user_id="u1")
        assert fb.title == "Found a bug"
        assert fb.rating == 3

    def test_missing_title_raises(self):
        from time_warp.features.beta_testing_framework import UserFeedback
        with pytest.raises(ValueError):
            UserFeedback(title="")

    def test_invalid_rating_raises(self):
        from time_warp.features.beta_testing_framework import UserFeedback
        with pytest.raises(ValueError):
            UserFeedback(title="Test", rating=0)
        with pytest.raises(ValueError):
            UserFeedback(title="Test", rating=6)

    def test_valid_rating_range(self):
        from time_warp.features.beta_testing_framework import UserFeedback
        for rating in range(1, 6):
            fb = UserFeedback(title=f"Test {rating}", rating=rating)
            assert fb.rating == rating

    def test_feedback_types(self):
        from time_warp.features.beta_testing_framework import FeedbackType
        types = [t for t in FeedbackType]
        assert len(types) >= 3


class TestUserSession:
    def test_create_session(self):
        from time_warp.features.beta_testing_framework import UserSession
        s = UserSession(user_id="u1")
        assert s.user_id == "u1"
        assert s.end_time is None

    def test_end_session(self):
        from time_warp.features.beta_testing_framework import UserSession
        s = UserSession(user_id="u1")
        s.end_session()
        assert s.end_time is not None
        assert s.duration_seconds >= 0.0

    def test_unique_ids(self):
        from time_warp.features.beta_testing_framework import UserSession
        s1 = UserSession()
        s2 = UserSession()
        assert s1.id != s2.id


class TestABTest:
    def test_create_ab_test(self):
        from time_warp.features.beta_testing_framework import ABTest
        test = ABTest(
            name="Theme Test",
            description="Test dark vs light",
            feature="theme",
            traffic_allocation={"control": 0.5, "variant_a": 0.5}
        )
        assert test.name == "Theme Test"
        assert test.status == "running"

    def test_missing_name_raises(self):
        from time_warp.features.beta_testing_framework import ABTest
        with pytest.raises(ValueError):
            ABTest(name="", traffic_allocation={"a": 0.5, "b": 0.5})

    def test_invalid_traffic_allocation_raises(self):
        from time_warp.features.beta_testing_framework import ABTest
        with pytest.raises(ValueError):
            ABTest(name="Test", traffic_allocation={"a": 0.3, "b": 0.3})  # sum != 1.0

    def test_valid_allocation(self):
        from time_warp.features.beta_testing_framework import ABTest
        test = ABTest(
            name="Test",
            traffic_allocation={"control": 0.34, "variant": 0.33, "variant2": 0.33}
        )
        assert test.name == "Test"


class TestFeedbackCollector:
    def test_create(self):
        from time_warp.features.beta_testing_framework import FeedbackCollector
        fc = FeedbackCollector()
        assert fc.feedback_items == []
        assert fc.bug_reports == []

    def test_submit_feedback(self):
        from time_warp.features.beta_testing_framework import FeedbackCollector, UserFeedback, FeedbackType
        fc = FeedbackCollector()
        fb = UserFeedback(type=FeedbackType.USABILITY, title="Good app", rating=5)
        fid = fc.submit_feedback(fb)
        assert fid == fb.id
        assert len(fc.feedback_items) == 1

    def test_submit_bug_report(self):
        from time_warp.features.beta_testing_framework import FeedbackCollector, BugReport, Severity
        fc = FeedbackCollector()
        bug = BugReport(title="Memory leak", description="Memory grows unbounded",
                        severity=Severity.HIGH)
        bid = fc.submit_bug_report(bug)
        assert bid == bug.id
        assert len(fc.bug_reports) == 1

    def test_get_feedback_by_type(self):
        from time_warp.features.beta_testing_framework import (
            FeedbackCollector, UserFeedback, FeedbackType
        )
        fc = FeedbackCollector()
        fc.submit_feedback(UserFeedback(type=FeedbackType.BUG_REPORT, title="Bug", rating=2))
        fc.submit_feedback(UserFeedback(type=FeedbackType.FEATURE_REQUEST, title="Feature", rating=4))
        fc.submit_feedback(UserFeedback(type=FeedbackType.BUG_REPORT, title="Bug2", rating=1))
        bugs = fc.get_feedback_by_type(FeedbackType.BUG_REPORT)
        assert len(bugs) == 2

    def test_get_critical_bugs(self):
        from time_warp.features.beta_testing_framework import (
            FeedbackCollector, BugReport, Severity
        )
        fc = FeedbackCollector()
        fc.submit_bug_report(BugReport(title="Critical", description="Bad crash",
                                       severity=Severity.CRITICAL))
        fc.submit_bug_report(BugReport(title="High", description="High issue",
                                       severity=Severity.HIGH))
        fc.submit_bug_report(BugReport(title="Low", description="Low issue",
                                       severity=Severity.LOW))
        critical = fc.get_critical_bugs()
        assert len(critical) == 2  # CRITICAL + HIGH

    def test_get_feedback_summary(self):
        from time_warp.features.beta_testing_framework import (
            FeedbackCollector, UserFeedback, FeedbackType,
            BugReport, Severity
        )
        fc = FeedbackCollector()
        fc.submit_feedback(UserFeedback(type=FeedbackType.USABILITY, title="T1", rating=4))
        fc.submit_feedback(UserFeedback(type=FeedbackType.USABILITY, title="T2", rating=2))
        fc.submit_bug_report(BugReport(title="B", description="D", severity=Severity.CRITICAL))
        summary = fc.get_feedback_summary()
        assert summary["total_feedback"] == 2
        assert summary["total_bugs"] == 1
        assert summary["avg_rating"] == 3.0
        assert summary["critical_bugs"] == 1

    def test_get_feedback_summary_empty(self):
        from time_warp.features.beta_testing_framework import FeedbackCollector
        fc = FeedbackCollector()
        summary = fc.get_feedback_summary()
        assert summary["total_feedback"] == 0
        assert summary["avg_rating"] == 0.0


class TestBugReporter:
    def test_create(self):
        from time_warp.features.beta_testing_framework import BugReporter, FeedbackCollector
        collector = FeedbackCollector()
        reporter = BugReporter(collector)
        assert reporter is not None

    def test_create_bug_from_exception(self):
        from time_warp.features.beta_testing_framework import BugReporter, FeedbackCollector, Severity
        collector = FeedbackCollector()
        reporter = BugReporter(collector)
        bug = reporter.create_bug_from_exception(ValueError("test error"), "core")
        assert "core" in bug.title
        assert bug.severity == Severity.HIGH
        assert bug.reporter == "system"

    def test_report_performance_issue(self):
        from time_warp.features.beta_testing_framework import BugReporter, FeedbackCollector, Severity
        collector = FeedbackCollector()
        reporter = BugReporter(collector)
        bug = reporter.report_performance_issue("database", "response_time", 750.0, 500.0)
        assert "database" in bug.title
        assert bug.severity == Severity.MEDIUM
        assert bug.reproducible is True

    def test_update_bug_status(self):
        from time_warp.features.beta_testing_framework import (
            BugReporter, FeedbackCollector, BugReport, Severity
        )
        collector = FeedbackCollector()
        reporter = BugReporter(collector)
        bug = BugReport(title="Test", description="Desc", severity=Severity.LOW)
        collector.submit_bug_report(bug)
        reporter.update_bug_status(bug.id, "fixed")
        assert bug.status == "fixed"

    def test_update_nonexistent_bug(self):
        from time_warp.features.beta_testing_framework import BugReporter, FeedbackCollector
        collector = FeedbackCollector()
        reporter = BugReporter(collector)
        reporter.update_bug_status("nonexistent-id", "fixed")  # should not crash


# ---------------------------------------------------------------------------
# ide_hooks.py
# ---------------------------------------------------------------------------

class TestIDEComponentInitializer:
    def test_create_no_window(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        assert init.main_window is None
        assert init.components == {}

    def test_get_component_missing(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        assert init.get_component("nonexistent") is None

    def test_get_status_empty(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        status = init.get_status()
        assert status == {}

    def test_initialize_beta_testing(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        result = init.initialize_beta_testing()
        assert isinstance(result, bool)
        if result:
            comp = init.get_component("beta")
            assert comp is not None
            assert comp["status"] == "ready"

    def test_initialize_integration_manager(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        result = init.initialize_integration_manager()
        assert isinstance(result, bool)

    def test_initialize_debugger(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        result = init.initialize_debugger()
        assert isinstance(result, bool)

    def test_initialize_all(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        results = init.initialize_all()
        assert isinstance(results, dict)
        assert len(results) == 5
        for key in ("integration_manager", "marketplace", "debugger",
                    "ai_intelligence", "beta_testing"):
            assert key in results

    def test_get_status_after_init(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        init.initialize_beta_testing()
        status = init.get_status()
        assert isinstance(status, dict)


class TestIDEEventRouter:
    def test_create(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        assert router.handlers == {}

    def test_register_and_emit_event(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        results = []
        router.register_handler("test_event", lambda **data: results.append(data))
        router.emit_event("test_event", value=42)
        assert len(results) == 1
        assert results[0]["value"] == 42

    def test_emit_no_handlers(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.emit_event("unknown_event", x=1)  # should not crash

    def test_multiple_handlers(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        results = []
        router.register_handler("e", lambda **data: results.append("h1"))
        router.register_handler("e", lambda **data: results.append("h2"))
        router.emit_event("e")
        assert results == ["h1", "h2"]

    def test_on_plugin_search_no_marketplace(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_plugin_search("my-plugin")  # should not crash

    def test_on_breakpoint_created_no_debugger(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_breakpoint_created("test.bas", 10, None)  # should not crash

    def test_on_debug_start_no_debugger(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_debug_start("session-1")  # should not crash

    def test_on_debug_step_no_debugger(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_debug_step("session-1", "step_over")

    def test_on_code_change_no_ai(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("x = 1", "basic", (0, 0))  # should not crash

    def test_on_request_optimization_no_ai(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_request_optimization("FOR i = 1 TO 10", "basic")

    def test_handler_exception_silenced(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)

        def bad_handler(**data):
            raise RuntimeError("Handler error")

        router.register_handler("test", bad_handler)
        router.emit_event("test")  # should not propagate exception
