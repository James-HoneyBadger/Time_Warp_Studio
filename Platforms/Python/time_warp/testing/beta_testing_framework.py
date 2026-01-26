"""
Beta Testing Framework for Phase VII-X Features

Provides:
- Feedback collection system
- Bug reporting infrastructure
- User experience tracking
- A/B testing framework
- Analytics dashboard support
"""

import json
import logging
import uuid
from dataclasses import asdict, dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional

# ===== ENUMS =====


class FeedbackType(Enum):
    """Types of feedback"""

    FEATURE_REQUEST = "feature_request"
    BUG_REPORT = "bug_report"
    USABILITY = "usability"
    PERFORMANCE = "performance"
    DOCUMENTATION = "documentation"


class Severity(Enum):
    """Bug severity levels"""

    CRITICAL = 1
    HIGH = 2
    MEDIUM = 3
    LOW = 4


class TestVariant(Enum):
    """A/B testing variants"""

    CONTROL = "control"
    VARIANT_A = "variant_a"
    VARIANT_B = "variant_b"
    VARIANT_C = "variant_c"


# ===== DATA CLASSES =====


@dataclass
class BugReport:
    """Bug report structure"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    title: str = ""
    description: str = ""
    severity: Severity = Severity.MEDIUM
    component: str = ""
    reproducible: bool = False
    steps_to_reproduce: str = ""
    expected_behavior: str = ""
    actual_behavior: str = ""
    environment: Dict[str, str] = field(default_factory=dict)
    attachments: List[str] = field(default_factory=list)
    reporter: str = ""
    status: str = "open"  # open, in_progress, fixed, closed
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)

    def __post_init__(self):
        """Validate bug report"""
        if not self.title:
            raise ValueError("Bug report requires title")
        if not self.description:
            raise ValueError("Bug report requires description")


@dataclass
class UserFeedback:
    """User feedback structure"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    type: FeedbackType = FeedbackType.FEATURE_REQUEST
    title: str = ""
    content: str = ""
    feature_area: str = ""
    user_id: str = ""
    session_id: str = ""
    rating: int = 5  # 1-5 scale
    metadata: Dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.utcnow)
    helpful_count: int = 0

    def __post_init__(self):
        """Validate feedback"""
        if not self.title:
            raise ValueError("Feedback requires title")
        if not 1 <= self.rating <= 5:
            raise ValueError("Rating must be 1-5")


@dataclass
class ABTest:
    """A/B test configuration"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    name: str = ""
    description: str = ""
    feature: str = ""
    control_variant: str = ""
    test_variants: List[str] = field(default_factory=list)
    traffic_allocation: Dict[str, float] = field(default_factory=dict)
    start_date: datetime = field(default_factory=datetime.utcnow)
    end_date: Optional[datetime] = None
    results: Dict[str, Dict[str, float]] = field(default_factory=dict)
    status: str = "running"  # running, paused, completed

    def __post_init__(self):
        """Validate A/B test"""
        if not self.name:
            raise ValueError("A/B test requires name")
        total_allocation = sum(self.traffic_allocation.values())
        if not 0.99 <= total_allocation <= 1.01:
            raise ValueError("Traffic allocation must sum to 1.0")


@dataclass
class UserSession:
    """User session tracking"""

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    user_id: str = ""
    start_time: datetime = field(default_factory=datetime.utcnow)
    end_time: Optional[datetime] = None
    duration_seconds: float = 0.0
    actions: List[Dict[str, Any]] = field(default_factory=list)
    ab_test_assignments: Dict[str, str] = field(default_factory=dict)
    environment: Dict[str, str] = field(default_factory=dict)

    def end_session(self):
        """End session and calculate duration"""
        self.end_time = datetime.utcnow()
        self.duration_seconds = (self.end_time - self.start_time).total_seconds()


@dataclass
class BetaAnalytics:
    """Analytics aggregation"""

    period_start: datetime
    period_end: datetime
    total_users: int
    total_sessions: int
    avg_session_duration: float
    feature_adoption: Dict[str, float]  # feature -> adoption %
    bug_count: int
    feedback_count: int
    satisfaction_rating: float  # average rating


# ===== FEEDBACK COLLECTOR =====


class FeedbackCollector:
    """Collect and store user feedback"""

    def __init__(self):
        self.feedback_items: List[UserFeedback] = []
        self.bug_reports: List[BugReport] = []
        self.logger = logging.getLogger(__name__)

    def submit_feedback(self, feedback: UserFeedback) -> str:
        """Submit user feedback"""
        self.feedback_items.append(feedback)
        self.logger.info("Feedback submitted: %s", feedback.id)
        return feedback.id

    def submit_bug_report(self, bug: BugReport) -> str:
        """Submit bug report"""
        self.bug_reports.append(bug)
        self.logger.info("Bug reported: %s", bug.id)
        return bug.id

    def get_feedback_by_type(self, feedback_type: FeedbackType) -> List[UserFeedback]:
        """Get feedback by type"""
        return [f for f in self.feedback_items if f.type == feedback_type]

    def get_critical_bugs(self) -> List[BugReport]:
        """Get critical and high-severity bugs"""
        return [
            b
            for b in self.bug_reports
            if b.severity in [Severity.CRITICAL, Severity.HIGH]
        ]

    def get_feedback_summary(self) -> Dict[str, Any]:
        """Get feedback summary"""
        return {
            "total_feedback": len(self.feedback_items),
            "total_bugs": len(self.bug_reports),
            "avg_rating": sum(f.rating for f in self.feedback_items)
            / max(len(self.feedback_items), 1),
            "by_type": {
                ft.value: len(self.get_feedback_by_type(ft)) for ft in FeedbackType
            },
            "critical_bugs": len(self.get_critical_bugs()),
        }


# ===== BUG REPORTER =====


class BugReporter:
    """Bug reporting interface"""

    def __init__(self, collector: FeedbackCollector):
        self.collector = collector
        self.logger = logging.getLogger(__name__)

    def create_bug_from_exception(
        self, exc: Exception, component: str, reporter: str = "system"
    ) -> BugReport:
        """Create bug report from exception"""

        bug = BugReport(
            title=f"Exception in {component}",
            description=str(exc),
            severity=Severity.HIGH,
            component=component,
            reproducible=False,
            actual_behavior=str(exc),
            expected_behavior="No exception",
            reporter=reporter,
        )

        return bug

    def report_performance_issue(
        self,
        component: str,
        metric_name: str,
        current_value: float,
        threshold: float,
        reporter: str = "system",
    ) -> BugReport:
        """Report performance degradation"""
        bug = BugReport(
            title=f"Performance Issue: {component}.{metric_name}",
            description=f"{metric_name} exceeded threshold",
            severity=Severity.MEDIUM,
            component=component,
            reproducible=True,
            actual_behavior=f"{metric_name} = {current_value}",
            expected_behavior=f"{metric_name} <= {threshold}",
            reporter=reporter,
        )

        return bug

    def update_bug_status(self, bug_id: str, status: str):
        """Update bug status"""
        for bug in self.collector.bug_reports:
            if bug.id == bug_id:
                bug.status = status
                bug.updated_at = datetime.utcnow()
                self.logger.info("Bug %s updated to {status}", bug_id)
                return

        self.logger.warning("Bug %s not found", bug_id)


# ===== A/B TEST FRAMEWORK =====


class ABTestFramework:
    """A/B testing infrastructure"""

    def __init__(self):
        self.tests: Dict[str, ABTest] = {}
        self.user_assignments: Dict[str, Dict[str, str]] = (
            {}
        )  # user_id -> test_id -> variant
        self.logger = logging.getLogger(__name__)

    def create_test(self, ab_test: ABTest) -> str:
        """Create A/B test"""
        self.tests[ab_test.id] = ab_test
        self.logger.info("A/B test created: %s", ab_test.id)
        return ab_test.id

    def assign_user(self, user_id: str, test_id: str) -> str:
        """Assign user to test variant"""
        test = self.tests.get(test_id)
        if not test:
            raise ValueError(f"Test {test_id} not found")

        # Allocate variant based on traffic distribution
        import random

        rand = random.random()
        cumulative = 0.0

        for variant, allocation in test.traffic_allocation.items():
            cumulative += allocation
            if rand <= cumulative:
                if user_id not in self.user_assignments:
                    self.user_assignments[user_id] = {}
                self.user_assignments[user_id][test_id] = variant
                return variant

        return test.test_variants[0]

    def get_user_variant(self, user_id: str, test_id: str) -> Optional[str]:
        """Get variant for user"""
        return self.user_assignments.get(user_id, {}).get(test_id)

    def record_metric(self, test_id: str, variant: str, metric: str, value: float):
        """Record test metric"""
        test = self.tests.get(test_id)
        if not test:
            return

        if variant not in test.results:
            test.results[variant] = {}

        test.results[variant][metric] = value

    def finalize_test(self, test_id: str):
        """Finalize test and set status"""
        test = self.tests.get(test_id)
        if test:
            test.status = "completed"
            test.end_date = datetime.utcnow()
            self.logger.info("A/B test completed: %s", test_id)


# ===== SESSION TRACKER =====


class SessionTracker:
    """Track user sessions"""

    def __init__(self):
        self.sessions: Dict[str, UserSession] = {}
        self.logger = logging.getLogger(__name__)
        self.ab_framework = ABTestFramework()

    def start_session(self, user_id: str) -> UserSession:
        """Start new user session"""
        session = UserSession(user_id=user_id)
        self.sessions[session.id] = session
        self.logger.info("Session started: %s", session.id)
        return session

    def end_session(self, session_id: str):
        """End session"""
        session = self.sessions.get(session_id)
        if session:
            session.end_session()
            self.logger.info("Session ended: %s", session_id)

    def record_action(
        self,
        session_id: str,
        action_name: str,
        metadata: Optional[Dict] = None,
    ):
        """Record user action"""
        session = self.sessions.get(session_id)
        if session:
            action = {
                "name": action_name,
                "timestamp": datetime.utcnow().isoformat(),
                "metadata": metadata or {},
            }
            session.actions.append(action)
            self.logger.debug("Action recorded: %s", action_name)

    def get_session_analytics(self) -> BetaAnalytics:
        """Generate session analytics"""
        if not self.sessions:
            return None

        total_duration = sum(s.duration_seconds for s in self.sessions.values())
        avg_duration = total_duration / len(self.sessions)

        # Count feature usage
        feature_usage: Dict[str, int] = {}
        for session in self.sessions.values():
            for action in session.actions:
                feature = action["name"]
                feature_usage[feature] = feature_usage.get(feature, 0) + 1

        total_actions = sum(feature_usage.values())
        feature_adoption = (
            {f: (count / total_actions * 100) for f, count in feature_usage.items()}
            if total_actions > 0
            else {}
        )

        return BetaAnalytics(
            period_start=min(s.start_time for s in self.sessions.values()),
            period_end=max(
                s.end_time or datetime.utcnow() for s in self.sessions.values()
            ),
            total_users=len(set(s.user_id for s in self.sessions.values())),
            total_sessions=len(self.sessions),
            avg_session_duration=avg_duration,
            feature_adoption=feature_adoption,
            bug_count=0,  # Would be from BugReporter
            feedback_count=0,  # Would be from FeedbackCollector
            satisfaction_rating=0.0,  # Would calculate from feedback ratings
        )


# ===== BETA TESTING MANAGER =====


class BetaTestingManager:
    """Central beta testing manager"""

    def __init__(self):
        self.feedback_collector = FeedbackCollector()
        self.bug_reporter = BugReporter(self.feedback_collector)
        self.ab_framework = ABTestFramework()
        self.session_tracker = SessionTracker()
        self.logger = logging.getLogger(__name__)

    def get_status(self) -> Dict[str, Any]:
        """Get beta testing status"""
        return {
            "feedback": self.feedback_collector.get_feedback_summary(),
            "active_tests": len(
                [t for t in self.ab_framework.tests.values() if t.status == "running"]
            ),
            "active_sessions": len(self.session_tracker.sessions),
            "analytics": (
                self.session_tracker.get_session_analytics().__dict__
                if self.session_tracker.get_session_analytics()
                else None
            ),
        }

    def export_report(self, filepath: str):
        """Export beta testing report"""
        report = {
            "timestamp": datetime.utcnow().isoformat(),
            "feedback_summary": self.feedback_collector.get_feedback_summary(),
            "critical_bugs": [
                asdict(b) for b in self.bug_reporter.collector.get_critical_bugs()
            ],
            "ab_tests": {
                test_id: {
                    "name": test.name,
                    "status": test.status,
                    "results": test.results,
                }
                for test_id, test in self.ab_framework.tests.items()
            },
            "analytics": (
                self.session_tracker.get_session_analytics().__dict__
                if self.session_tracker.get_session_analytics()
                else None
            ),
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(report, f, indent=2, default=str)

        self.logger.info("Report exported to %s", filepath)


# ===== BETA TESTING UI =====


class BetaTestingUI:
    """UI for beta testing feedback (console-based)"""

    def __init__(self, manager: BetaTestingManager):
        self.manager = manager

    def display_feedback_form(self):
        """Display feedback collection form"""
        print("\n" + "=" * 60)
        print("TIME WARP STUDIO - BETA FEEDBACK FORM")
        print("=" * 60)

        feedback_type = input(
            "Type (1-feature, 2-bug, 3-usability, 4-performance, 5-docs): "
        )
        type_map = {
            "1": FeedbackType.FEATURE_REQUEST,
            "2": FeedbackType.BUG_REPORT,
            "3": FeedbackType.USABILITY,
            "4": FeedbackType.PERFORMANCE,
            "5": FeedbackType.DOCUMENTATION,
        }

        title = input("Title: ")
        content = input("Content: ")
        rating = int(input("Rating (1-5): "))

        feedback = UserFeedback(
            type=type_map.get(feedback_type, FeedbackType.FEATURE_REQUEST),
            title=title,
            content=content,
            rating=rating,
        )

        feedback_id = self.manager.feedback_collector.submit_feedback(feedback)
        print(f"\n✅ Feedback submitted (ID: {feedback_id})")

    def display_bug_form(self):
        """Display bug reporting form"""
        print("\n" + "=" * 60)
        print("TIME WARP STUDIO - BUG REPORT FORM")
        print("=" * 60)

        title = input("Bug Title: ")
        description = input("Description: ")
        component = input("Component: ")
        steps = input("Steps to reproduce: ")

        bug = BugReport(
            title=title,
            description=description,
            component=component,
            steps_to_reproduce=steps,
            reproducible=input("Reproducible? (y/n): ").lower() == "y",
        )

        bug_id = self.manager.feedback_collector.submit_bug_report(bug)
        print(f"\n✅ Bug report submitted (ID: {bug_id})")

    def display_status(self):
        """Display current beta testing status"""
        status = self.manager.get_status()

        print("\n" + "=" * 60)
        print("BETA TESTING STATUS")
        print("=" * 60)
        print(json.dumps(status, indent=2, default=str))


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)

    # Example usage
    manager = BetaTestingManager()
    ui = BetaTestingUI(manager)

    while True:
        print("\nBeta Testing Options:")
        print("1. Submit Feedback")
        print("2. Report Bug")
        print("3. View Status")
        print("4. Export Report")
        print("5. Exit")

        choice = input("\nSelect option: ")

        if choice == "1":
            ui.display_feedback_form()
        elif choice == "2":
            ui.display_bug_form()
        elif choice == "3":
            ui.display_status()
        elif choice == "4":
            manager.export_report("beta_report.json")
            print("Report exported to beta_report.json")
        elif choice == "5":
            break
