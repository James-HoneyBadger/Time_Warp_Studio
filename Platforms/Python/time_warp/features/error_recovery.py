"""
Time Warp Studio - Error Recovery & Monitoring Infrastructure

Provides:
- Structured error handling
- Automatic error recovery
- Performance monitoring
- Health checks
- Graceful degradation
"""

import logging
import time
import traceback
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from enum import Enum
from functools import wraps
from typing import Any, Callable, Dict, List, Optional


def utc_now() -> datetime:
    return datetime.now(timezone.utc)


# ===== ERROR TYPES =====


class ErrorSeverity(Enum):
    """Error severity levels"""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class ErrorCategory(Enum):
    """Error categories"""

    SYNTAX = "syntax"
    RUNTIME = "runtime"
    NETWORK = "network"
    DATABASE = "database"
    AUTHENTICATION = "authentication"
    AUTHORIZATION = "authorization"
    VALIDATION = "validation"
    CONFIGURATION = "configuration"
    EXTERNAL_SERVICE = "external_service"
    INTERNAL = "internal"


@dataclass
class ErrorRecord:
    """Recorded error information"""

    id: str
    timestamp: datetime
    category: ErrorCategory
    severity: ErrorSeverity
    message: str
    exception_type: str
    stack_trace: str
    context: Dict[str, Any]
    user_id: Optional[str] = None
    component: Optional[str] = None
    resolved: bool = False
    resolution_time: Optional[timedelta] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "id": self.id,
            "timestamp": self.timestamp.isoformat(),
            "category": self.category.value,
            "severity": self.severity.value,
            "message": self.message,
            "exception_type": self.exception_type,
            "context": self.context,
            "resolved": self.resolved,
        }


@dataclass
class HealthCheckResult:
    """Health check result"""

    component: str
    healthy: bool
    status_code: int
    response_time_ms: float
    message: str
    timestamp: datetime = field(default_factory=utc_now)
    dependencies: Dict[str, bool] = field(default_factory=dict)


# ===== ERROR RECOVERY =====


class RecoveryStrategy(ABC):
    """Abstract recovery strategy"""

    @abstractmethod
    def can_recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Check if error can be recovered"""
        pass

    @abstractmethod
    def recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Attempt recovery"""
        pass


class RetryStrategy(RecoveryStrategy):
    """Retry with exponential backoff"""

    def __init__(
        self,
        max_retries: int = 3,
        initial_delay: float = 1.0,
        max_delay: float = 30.0,
    ):
        self.max_retries = max_retries
        self.initial_delay = initial_delay
        self.max_delay = max_delay

    def can_recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Check if error is retryable"""
        retryable_errors = (
            ConnectionError,
            TimeoutError,
            IOError,
        )
        return isinstance(error, retryable_errors)

    def recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Retry with exponential backoff"""
        func = context.get("function")
        if not func:
            return False

        for attempt in range(self.max_retries):
            try:
                delay = min(self.initial_delay * (2**attempt), self.max_delay)
                time.sleep(delay)
                func()
                return True
            except (ValueError, TypeError):
                continue

        return False


class FallbackStrategy(RecoveryStrategy):
    """Use fallback implementation"""

    def __init__(self, fallback_func: Callable):
        self.fallback_func = fallback_func

    def can_recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Can always fallback"""
        return True

    def recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Execute fallback"""
        try:
            self.fallback_func()
            return True
        except (ValueError, TypeError):
            return False


class CircuitBreakerStrategy(RecoveryStrategy):
    """Circuit breaker pattern"""

    def __init__(self, failure_threshold: int = 5, timeout: float = 60.0):
        self.failure_threshold = failure_threshold
        self.timeout = timeout
        self.failure_count = 0
        self.last_failure_time = None
        self.open = False

    def can_recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Check if circuit can be recovered"""
        if self.open:
            # Check if timeout has passed
            if self.last_failure_time:
                elapsed = (utc_now() - self.last_failure_time).total_seconds()
                if elapsed > self.timeout:
                    self.open = False
                    self.failure_count = 0
                    return True
            return False

        return True

    def recover(self, error: Exception, context: Dict[str, Any]) -> bool:
        """Track failures and open circuit if needed"""
        self.failure_count += 1
        self.last_failure_time = utc_now()

        if self.failure_count >= self.failure_threshold:
            self.open = True
            return False

        return False


# ===== ERROR HANDLER =====


class ErrorHandler:
    """Centralized error handling"""

    def __init__(self):
        self.error_records: List[ErrorRecord] = []
        self.recovery_strategies: List[RecoveryStrategy] = []
        self.error_handlers: Dict[type, Callable] = {}
        self.logger = logging.getLogger(__name__)

    def register_recovery_strategy(self, strategy: RecoveryStrategy):
        """Register a recovery strategy"""
        self.recovery_strategies.append(strategy)

    def register_error_handler(self, error_type: type, handler: Callable):
        """Register custom error handler"""
        self.error_handlers[error_type] = handler

    def handle_error(
        self,
        error: Exception,
        category: ErrorCategory,
        severity: ErrorSeverity,
        context: Dict[str, Any] = None,
        component: str | None = None,
        user_id: str | None = None,
    ) -> bool:
        """Handle an error"""
        context = context or {}

        # Log error
        record = self._create_error_record(
            error, category, severity, context, component, user_id
        )
        self.error_records.append(record)

        # Log to system logger
        self.logger.error(
            f"{category.value}: {error}",
            extra={
                "error_id": record.id,
                "severity": severity.value,
                "component": component,
            },
        )

        # Try custom handler
        if type(error) in self.error_handlers:
            try:
                return self.error_handlers[type(error)](error, context)
            except (ValueError, TypeError):
                pass

        # Try recovery strategies
        for strategy in self.recovery_strategies:
            if strategy.can_recover(error, context):
                if strategy.recover(error, context):
                    record.resolved = True
                    record.resolution_time = utc_now() - record.timestamp
                    return True

        return False

    def _create_error_record(
        self,
        error: Exception,
        category: ErrorCategory,
        severity: ErrorSeverity,
        context: Dict[str, Any],
        component: str,
        user_id: str,
    ) -> ErrorRecord:
        """Create error record"""
        import uuid

        return ErrorRecord(
            id=str(uuid.uuid4()),
            timestamp=utc_now(),
            category=category,
            severity=severity,
            message=str(error),
            exception_type=type(error).__name__,
            stack_trace=traceback.format_exc(),
            context=context,
            user_id=user_id,
            component=component,
        )

    def get_recent_errors(self, limit: int = 100) -> List[ErrorRecord]:
        """Get recent errors"""
        return self.error_records[-limit:]

    def get_error_by_id(self, error_id: str) -> Optional[ErrorRecord]:
        """Get error by ID"""
        for record in self.error_records:
            if record.id == error_id:
                return record
        return None


# ===== MONITORING =====


@dataclass
class PerformanceMetrics:
    """Performance metrics"""

    response_time_ms: float
    memory_mb: float
    cpu_percent: float
    error_rate: float
    throughput: float  # requests per second
    timestamp: datetime = field(default_factory=utc_now)


class Monitor:
    """Performance and health monitor"""

    def __init__(self):
        self.metrics: List[PerformanceMetrics] = []
        self.health_checks: Dict[str, HealthCheckResult] = {}
        self.thresholds = {
            "response_time_ms": 500,
            "memory_mb": 1000,
            "error_rate": 0.01,  # 1%
        }

    def record_metric(self, metrics: PerformanceMetrics):
        """Record performance metrics"""
        self.metrics.append(metrics)

        # Check thresholds
        if metrics.response_time_ms > self.thresholds["response_time_ms"]:
            logging.warning(f"Slow response: {metrics.response_time_ms}ms")

        if metrics.memory_mb > self.thresholds["memory_mb"]:
            logging.warning(f"High memory: {metrics.memory_mb}MB")

        if metrics.error_rate > self.thresholds["error_rate"]:
            logging.warning(f"High error rate: {metrics.error_rate * 100}%")

    def record_health_check(self, result: HealthCheckResult):
        """Record health check result"""
        self.health_checks[result.component] = result

    def is_healthy(self) -> bool:
        """Check if system is healthy"""
        for result in self.health_checks.values():
            if not result.healthy:
                return False
        return True

    def get_latest_metrics(self) -> Optional[PerformanceMetrics]:
        """Get latest metrics"""
        return self.metrics[-1] if self.metrics else None

    def get_metrics_summary(self, minutes: int = 60) -> Dict[str, Any]:
        """Get metrics summary for time period"""
        cutoff = utc_now() - timedelta(minutes=minutes)
        recent = [m for m in self.metrics if m.timestamp >= cutoff]

        if not recent:
            return {}

        return {
            "count": len(recent),
            "avg_response_time": sum(m.response_time_ms for m in recent) / len(recent),
            "max_response_time": max(m.response_time_ms for m in recent),
            "min_response_time": min(m.response_time_ms for m in recent),
            "avg_memory": sum(m.memory_mb for m in recent) / len(recent),
            "avg_cpu": sum(m.cpu_percent for m in recent) / len(recent),
            "avg_error_rate": sum(m.error_rate for m in recent) / len(recent),
        }


# ===== DECORATORS =====


def handle_errors(
    category: ErrorCategory,
    severity: ErrorSeverity,
    component: str | None = None,
):
    """Decorator for automatic error handling"""

    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            try:
                return func(*args, **kwargs)
            except Exception as e:
                error_handler = ErrorHandler()
                context = {
                    "function": func.__name__,
                    "args": str(args)[:100],
                    "kwargs": str(kwargs)[:100],
                }
                error_handler.handle_error(
                    e,
                    category=category,
                    severity=severity,
                    context=context,
                    component=component,
                )
                raise

        return wrapper

    return decorator


def monitor_performance(func):
    """Decorator for performance monitoring"""

    @wraps(func)
    def wrapper(*args, **kwargs):
        start_time = time.time()
        get_memory_usage()

        try:
            result = func(*args, **kwargs)
            return result
        finally:
            duration = (time.time() - start_time) * 1000  # ms
            memory = get_memory_usage()

            metrics = PerformanceMetrics(
                response_time_ms=duration,
                memory_mb=memory,
                cpu_percent=0,  # Would need psutil
                error_rate=0,
                throughput=1 / (duration / 1000),
            )

            monitor = Monitor()
            monitor.record_metric(metrics)

    return wrapper


def retry(max_attempts: int = 3, delay: float = 1.0):
    """Decorator for retry logic"""

    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except (ValueError, TypeError):
                    if attempt == max_attempts - 1:
                        raise
                    time.sleep(delay)

        return wrapper

    return decorator


# ===== UTILITIES =====


def get_memory_usage() -> float:
    """Get current memory usage in MB"""
    try:
        import psutil

        process = psutil.Process()
        return process.memory_info().rss / (1024 * 1024)
    except ImportError:
        return 0.0


# ===== GLOBAL INSTANCES =====

error_handler = ErrorHandler()
monitor = Monitor()

# Register default recovery strategies
error_handler.register_recovery_strategy(RetryStrategy())
error_handler.register_recovery_strategy(CircuitBreakerStrategy())
