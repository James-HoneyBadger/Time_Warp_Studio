"""Execution timeout utilities to prevent infinite loops.

Provides timeout mechanisms for code execution to protect against
infinite loops and hanging programs.
"""

import functools
import signal
import sys
import threading
import time
from contextlib import contextmanager
from typing import Optional

from .logging_config import get_logger

logger = get_logger(__name__)


class ExecutionTimeoutError(Exception):
    """Raised when execution exceeds timeout limit."""

    pass


@contextmanager
def execution_timeout(seconds: float = 10.0):
    """Context manager for Unix-based execution timeout using signals.

    Uses SIGALRM to interrupt execution after specified time.
    Not available on Windows.

    Args:
        seconds: Timeout in seconds

    Raises:
        ExecutionTimeoutError: If execution exceeds timeout

    Example:
        >>> with execution_timeout(5.0):
        ...     result = interpreter.execute(user_code)
    """
    if sys.platform == "win32":
        # Windows doesn't support SIGALRM; use thread-based timeout instead
        yield
        return

    # For very small timeouts (< 1 second), use thread-based timeout
    if seconds < 1.0:
        with ThreadedExecutionTimeout(seconds):
            yield
        return

    def timeout_handler(signum, frame):
        raise ExecutionTimeoutError(
            f"Execution exceeded {seconds:.1f} second timeout"
        )

    # Save previous handler
    old_handler = signal.signal(signal.SIGALRM, timeout_handler)
    signal.alarm(int(seconds) + 1)  # Round up to ensure timeout

    try:
        yield
    finally:
        signal.alarm(0)  # Cancel alarm
        signal.signal(signal.SIGALRM, old_handler)  # Restore handler


class ThreadedExecutionTimeout:
    """Thread-based timeout mechanism (works on all platforms).

    Less reliable than signal-based timeout but platform-independent.
    """

    def __init__(self, seconds: float = 10.0):
        """Initialize timeout.

        Args:
            seconds: Timeout in seconds
        """
        self.timeout = seconds
        self.timed_out = False
        self.timer: Optional[threading.Timer] = None

    def __enter__(self):
        """Start timeout timer."""
        self.timed_out = False
        self.timer = threading.Timer(self.timeout, self._set_timeout_flag)
        self.timer.daemon = True
        self.timer.start()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Cancel timeout timer."""
        if self.timer:
            self.timer.cancel()

        if self.timed_out:
            raise ExecutionTimeoutError(
                f"Execution exceeded {self.timeout:.1f} second timeout"
            )

        return False

    def _set_timeout_flag(self):
        """Called when timeout expires."""
        self.timed_out = True
        logger.warning(
            "Execution timeout expired after %.1f seconds", self.timeout
        )

    def is_expired(self) -> bool:
        """Check if timeout has expired."""
        return self.timed_out


def with_timeout(max_seconds: float = 10.0):
    """Decorator to add timeout to a function.

    Args:
        max_seconds: Maximum execution time in seconds

    Returns:
        Decorated function that raises ExecutionTimeoutError if exceeded

    Example:
        >>> @with_timeout(5.0)
        ... def slow_function():
        ...     time.sleep(10)
        >>> slow_function()  # Raises ExecutionTimeoutError
    """

    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            try:
                with execution_timeout(max_seconds):
                    return func(*args, **kwargs)
            except ExecutionTimeoutError as e:
                logger.error("Function %s timed out: %s", func.__name__, e)
                raise

        return wrapper

    return decorator
