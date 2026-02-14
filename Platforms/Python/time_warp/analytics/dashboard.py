"""
Time Warp Studio - Analytics & Monitoring Dashboard

Provides:
- Real-time usage analytics
- Performance monitoring
- User behavior tracking
- System health metrics
- Data-driven insights
"""

from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from enum import Enum
from typing import Dict, List, Optional


def utc_now() -> datetime:
    return datetime.now(timezone.utc)

# ===== ENUMS =====


class MetricType(Enum):
    """Types of metrics"""

    EXECUTION = "execution"
    COMPILATION = "compilation"
    MEMORY = "memory"
    LATENCY = "latency"
    ERROR_RATE = "error_rate"
    CACHE_HIT = "cache_hit"
    USER_ACTIVITY = "user_activity"
    CODE_QUALITY = "code_quality"


class EventType(Enum):
    """Event types for tracking"""

    CODE_RUN = "code_run"
    CODE_SAVE = "code_save"
    CODE_COMPILE = "code_compile"
    USER_LOGIN = "user_login"
    USER_LOGOUT = "user_logout"
    ERROR = "error"
    PERFORMANCE_SPIKE = "performance_spike"
    DEPLOYMENT = "deployment"


# ===== DATA CLASSES =====


@dataclass
class Metric:
    """Single metric data point"""

    timestamp: datetime = field(default_factory=datetime.utcnow)
    metric_type: MetricType = MetricType.EXECUTION
    value: float = 0.0
    unit: str = ""
    tags: Dict[str, str] = field(default_factory=dict)  # language, user, etc.


@dataclass
class Event:
    """System event"""

    timestamp: datetime = field(default_factory=datetime.utcnow)
    event_type: EventType = EventType.CODE_RUN
    user_id: Optional[str] = None
    data: Dict = field(default_factory=dict)


@dataclass
class PerformanceStats:
    """Performance statistics"""

    p50_latency_ms: float = 0.0
    p95_latency_ms: float = 0.0
    p99_latency_ms: float = 0.0
    avg_latency_ms: float = 0.0
    max_latency_ms: float = 0.0
    min_latency_ms: float = 0.0
    throughput_per_sec: float = 0.0
    error_count: int = 0
    success_count: int = 0


@dataclass
class UserMetrics:
    """Per-user metrics"""

    user_id: str = ""
    total_executions: int = 0
    successful_executions: int = 0
    failed_executions: int = 0
    total_execution_time_ms: int = 0
    avg_execution_time_ms: float = 0.0
    favorite_language: str = ""
    languages_used: Dict[str, int] = field(default_factory=dict)  # language -> count
    last_active: datetime = field(default_factory=datetime.utcnow)


@dataclass
class SystemHealth:
    """System health status"""

    timestamp: datetime = field(default_factory=datetime.utcnow)
    cpu_usage_percent: float = 0.0
    memory_usage_mb: float = 0.0
    memory_available_mb: float = 0.0
    disk_usage_percent: float = 0.0
    active_connections: int = 0
    error_rate_percent: float = 0.0
    uptime_seconds: int = 0
    status: str = "healthy"  # healthy, degraded, critical


@dataclass
class Alert:
    """System alert"""

    id: str = ""
    severity: str = "warning"  # info, warning, error, critical
    title: str = ""
    message: str = ""
    metric: Optional[Metric] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    acknowledged: bool = False


# ===== ANALYTICS ENGINE =====


class MetricsCollector:
    """Collects and stores metrics"""

    def __init__(self, retention_hours: int = 24):
        self.retention_hours = retention_hours
        self.metrics: List[Metric] = []
        self.events: List[Event] = []

    def record_metric(
        self,
        metric_type: MetricType,
        value: float,
        unit: str = "",
        tags: Dict[str, str] = None,
    ) -> None:
        """Record a metric"""
        metric = Metric(
            metric_type=metric_type, value=value, unit=unit, tags=tags or {}
        )
        self.metrics.append(metric)
        self._cleanup_old_metrics()

    def record_event(
        self,
        event_type: EventType,
        user_id: Optional[str] = None,
        data: Dict | None = None,
    ) -> None:
        """Record an event"""
        event = Event(event_type=event_type, user_id=user_id, data=data or {})
        self.events.append(event)
        self._cleanup_old_events()

    def get_metrics(
        self, metric_type: Optional[MetricType] = None, hours: int = 1
    ) -> List[Metric]:
        """Get metrics for time period"""
        cutoff = datetime.utcnow() - timedelta(hours=hours)

        result = [m for m in self.metrics if m.timestamp >= cutoff]

        if metric_type:
            result = [m for m in result if m.metric_type == metric_type]

        return result

    def get_events(
        self,
        event_type: Optional[EventType] = None,
        user_id: Optional[str] = None,
        hours: int = 24,
    ) -> List[Event]:
        """Get events for time period"""
        cutoff = datetime.utcnow() - timedelta(hours=hours)

        result = [e for e in self.events if e.timestamp >= cutoff]

        if event_type:
            result = [e for e in result if e.event_type == event_type]

        if user_id:
            result = [e for e in result if e.user_id == user_id]

        return result

    def _cleanup_old_metrics(self) -> None:
        """Remove metrics older than retention period"""
        cutoff = datetime.utcnow() - timedelta(hours=self.retention_hours)
        self.metrics = [m for m in self.metrics if m.timestamp >= cutoff]

    def _cleanup_old_events(self) -> None:
        """Remove events older than retention period"""
        cutoff = datetime.utcnow() - timedelta(hours=self.retention_hours)
        self.events = [e for e in self.events if e.timestamp >= cutoff]


class AnalyticsEngine:
    """Analyzes metrics and events"""

    def __init__(self, collector: MetricsCollector):
        self.collector = collector

    def compute_performance_stats(
        self, metric_type: MetricType, hours: int = 1
    ) -> PerformanceStats:
        """Compute performance statistics"""
        metrics = self.collector.get_metrics(metric_type, hours)

        if not metrics:
            return PerformanceStats()

        values = sorted([m.value for m in metrics])
        success_count = sum(
            1
            for e in self.collector.get_events(hours=hours)
            if e.event_type == EventType.CODE_RUN and e.data.get("success", False)
        )
        error_count = sum(
            1
            for e in self.collector.get_events(hours=hours)
            if e.event_type == EventType.ERROR
        )

        return PerformanceStats(
            p50_latency_ms=values[len(values) // 2] if values else 0,
            p95_latency_ms=values[int(len(values) * 0.95)] if values else 0,
            p99_latency_ms=values[int(len(values) * 0.99)] if values else 0,
            avg_latency_ms=sum(values) / len(values) if values else 0,
            max_latency_ms=max(values) if values else 0,
            min_latency_ms=min(values) if values else 0,
            throughput_per_sec=len(values) / hours / 3600 if hours else 0,
            error_count=error_count,
            success_count=success_count,
        )

    def get_user_metrics(self, user_id: str, hours: int = 24) -> UserMetrics:
        """Get user-specific metrics"""
        events = self.collector.get_events(user_id=user_id, hours=hours)

        total_executions = sum(1 for e in events if e.event_type == EventType.CODE_RUN)
        successful = sum(
            1
            for e in events
            if e.event_type == EventType.CODE_RUN and e.data.get("success", False)
        )
        failed = total_executions - successful

        # Calculate average execution time
        execution_times = [
            e.data.get("execution_time_ms", 0)
            for e in events
            if e.event_type == EventType.CODE_RUN
        ]
        total_time = sum(execution_times)
        avg_time = total_time / len(execution_times) if execution_times else 0

        # Count languages used
        languages_used = defaultdict(int)
        for e in events:
            if e.event_type == EventType.CODE_RUN and "language" in e.data:
                languages_used[e.data["language"]] += 1

        favorite_language = (
            max(languages_used, key=languages_used.get) if languages_used else ""
        )

        return UserMetrics(
            user_id=user_id,
            total_executions=total_executions,
            successful_executions=successful,
            failed_executions=failed,
            total_execution_time_ms=total_time,
            avg_execution_time_ms=avg_time,
            favorite_language=favorite_language,
            languages_used=dict(languages_used),
            last_active=(
                max([e.timestamp for e in events]) if events else datetime.utcnow()
            ),
        )

    def get_system_health(self) -> SystemHealth:
        """Get current system health"""
        # Get recent error rate
        recent_events = self.collector.get_events(hours=1)
        error_count = sum(1 for e in recent_events if e.event_type == EventType.ERROR)
        total_count = len(recent_events)
        error_rate = (error_count / total_count * 100) if total_count > 0 else 0

        return SystemHealth(
            cpu_usage_percent=0.0,  # Would be filled by actual system monitoring
            memory_usage_mb=0.0,
            memory_available_mb=0.0,
            disk_usage_percent=0.0,
            active_connections=0,
            error_rate_percent=error_rate,
            status=(
                "healthy"
                if error_rate < 5
                else "degraded" if error_rate < 15 else "critical"
            ),
        )


class AlertingSystem:
    """Generates alerts based on metrics"""

    def __init__(self, analytics: AnalyticsEngine):
        self.analytics = analytics
        self.alerts: List[Alert] = []
        self.thresholds = {
            "p99_latency_ms": 1000,
            "error_rate_percent": 5,
            "memory_usage_mb": 8000,
            "cpu_usage_percent": 80,
        }

    def check_and_alert(self) -> None:
        """Check metrics and generate alerts"""
        # Check latency
        perf_stats = self.analytics.compute_performance_stats(
            MetricType.LATENCY, hours=1
        )
        if perf_stats.p99_latency_ms > self.thresholds["p99_latency_ms"]:
            self._create_alert(
                "error",
                "High Latency Detected",
                f'P99 latency is {
                    perf_stats.p99_latency_ms:.0f}ms (threshold: {
                    self.thresholds["p99_latency_ms"]}ms)',
            )

        # Check error rate
        health = self.analytics.get_system_health()
        if health.error_rate_percent > self.thresholds["error_rate_percent"]:
            self._create_alert(
                "warning",
                "High Error Rate",
                f'Error rate is {
                    health.error_rate_percent:.1f}% (threshold: {
                    self.thresholds["error_rate_percent"]}%)',
            )

    def _create_alert(self, severity: str, title: str, message: str) -> None:
        """Create and store alert"""
        alert = Alert(severity=severity, title=title, message=message)
        self.alerts.append(alert)

    def get_active_alerts(self, severity: Optional[str] = None) -> List[Alert]:
        """Get unacknowledged alerts"""
        result = [a for a in self.alerts if not a.acknowledged]
        if severity:
            result = [a for a in result if a.severity == severity]
        return result

    def acknowledge_alert(self, alert_id: str) -> bool:
        """Acknowledge an alert"""
        for alert in self.alerts:
            if alert.id == alert_id:
                alert.acknowledged = True
                return True
        return False


class DashboardService:
    """Provides dashboard data"""

    def __init__(self):
        self.collector = MetricsCollector()
        self.analytics = AnalyticsEngine(self.collector)
        self.alerting = AlertingSystem(self.analytics)

    def record_execution(
        self,
        user_id: str,
        language: str,
        execution_time_ms: float,
        success: bool,
        memory_used_mb: float = 0,
    ) -> None:
        """Record code execution"""
        self.collector.record_metric(
            MetricType.EXECUTION,
            execution_time_ms,
            "ms",
            {"user": user_id, "language": language},
        )

        self.collector.record_metric(
            MetricType.MEMORY, memory_used_mb, "mb", {"user": user_id}
        )

        self.collector.record_event(
            EventType.CODE_RUN,
            user_id=user_id,
            data={
                "language": language,
                "execution_time_ms": execution_time_ms,
                "success": success,
                "memory_used_mb": memory_used_mb,
            },
        )

    def record_error(self, user_id: Optional[str], error_message: str) -> None:
        """Record error"""
        self.collector.record_event(
            EventType.ERROR, user_id=user_id, data={"error": error_message}
        )

    def get_dashboard_data(self) -> Dict:
        """Get comprehensive dashboard data"""
        self.alerting.check_and_alert()

        return {
            "performance": self.analytics.compute_performance_stats(
                MetricType.EXECUTION
            ),
            "health": self.analytics.get_system_health(),
            "active_alerts": self.alerting.get_active_alerts(),
            "metrics_summary": {
                "total_executions": len(
                    self.collector.get_events(event_type=EventType.CODE_RUN)
                ),
                "total_errors": len(
                    self.collector.get_events(event_type=EventType.ERROR)
                ),
                "avg_latency_ms": self.analytics.compute_performance_stats(
                    MetricType.EXECUTION
                ).avg_latency_ms,
            },
        }


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    dashboard = DashboardService()

    # Record some executions
    dashboard.record_execution("user1", "basic", 150, True, 42)
    dashboard.record_execution("user1", "logo", 200, True, 35)
    dashboard.record_execution("user2", "basic", 180, False, 50)
    dashboard.record_error("user2", "Syntax error on line 5")

    # Get dashboard data
    data = dashboard.get_dashboard_data()
    print("Performance Stats:")
    print(f"  Avg Latency: {data['performance'].avg_latency_ms:.1f}ms")
    print(f"  P99 Latency: {data['performance'].p99_latency_ms:.1f}ms")
    print(f"  Error Rate: {data['health'].error_rate_percent:.1f}%")

    print("\nMetrics Summary:")
    print(f"  Total Executions: {data['metrics_summary']['total_executions']}")
    print(f"  Total Errors: {data['metrics_summary']['total_errors']}")

    # Get user metrics
    user_metrics = dashboard.analytics.get_user_metrics("user1")
    print("\nUser1 Metrics:")
    print(f"  Total: {user_metrics.total_executions}")
    print(f"  Successful: {user_metrics.successful_executions}")
    print(f"  Favorite: {user_metrics.favorite_language}")
