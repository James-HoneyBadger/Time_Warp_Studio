"""Centralized logging configuration for Time Warp IDE.

Provides structured logging with file rotation, multiple profiles, and
both console and file output for debugging and monitoring.
"""

import logging
import logging.handlers
import os
import sys
from pathlib import Path
from typing import Optional, Literal


# Profile types for different execution contexts
ProfileType = Literal["production", "development", "testing", "ci"]

# Global logger registry
_LOGGERS = {}


def _ensure_log_dir() -> Path:
    """Ensure log directory exists and return its path."""
    log_dir = Path.home() / ".time_warp" / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)
    return log_dir


def get_logger(name: str) -> logging.Logger:
    """Get or create a logger with the given name.
    
    Args:
        name: Logger name (typically __name__)
    
    Returns:
        Configured logger instance
    """
    if name not in _LOGGERS:
        logger = logging.getLogger(name)
        _LOGGERS[name] = logger
    return _LOGGERS[name]


def setup_logging(
    level: str = "INFO",
    profile: ProfileType = "production",
    log_file: Optional[str] = None,
    console_output: bool = True,
    max_bytes: int = 10485760,  # 10MB
    backup_count: int = 5,
) -> logging.Logger:
    """Configure logging for Time Warp IDE.
    
    Sets up both console and file logging with appropriate levels
    and formats based on the execution profile.
    
    Args:
        level: Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
        profile: Execution profile (production, development, testing, ci)
        log_file: Path to log file. If None, uses ~/.time_warp/logs/ide.log
        console_output: Whether to output to console
        max_bytes: Max file size before rotation (default 10MB)
        backup_count: Number of backup files to keep
    
    Returns:
        Root logger instance
    
    Example:
        >>> setup_logging("DEBUG", profile="development")
        >>> logger = get_logger(__name__)
        >>> logger.debug("Debug message")
    """
    # Get or create root logger
    root_logger = logging.getLogger()
    root_logger.setLevel(level)
    
    # Clear existing handlers
    root_logger.handlers = []
    
    # Determine log file path
    if log_file is None:
        log_file = str(_ensure_log_dir() / "ide.log")
    
    # Create formatters for different profiles
    if profile == "development":
        fmt = "%(asctime)s - %(name)s - %(levelname)s - [%(filename)s:%(lineno)d] - %(message)s"
        date_fmt = "%Y-%m-%d %H:%M:%S"
    elif profile == "testing":
        fmt = "%(levelname)s - %(name)s - %(message)s"
        date_fmt = None
    elif profile == "ci":
        fmt = "%(levelname)s::%(name)s::%(message)s"
        date_fmt = None
    else:  # production
        fmt = "%(asctime)s - %(levelname)s - %(name)s - %(message)s"
        date_fmt = "%Y-%m-%d %H:%M:%S"
    
    formatter = logging.Formatter(fmt, datefmt=date_fmt)
    
    # Add console handler if requested
    if console_output:
        console_handler = logging.StreamHandler(sys.stdout)
        console_handler.setLevel(level)
        console_handler.setFormatter(formatter)
        root_logger.addHandler(console_handler)
    
    # Add file handler (always, except in testing)
    if profile != "testing":
        file_handler = logging.handlers.RotatingFileHandler(
            log_file,
            maxBytes=max_bytes,
            backupCount=backup_count,
        )
        file_handler.setLevel(level)
        file_handler.setFormatter(formatter)
        root_logger.addHandler(file_handler)
    
    return root_logger


def configure_for_production(log_file: Optional[str] = None) -> logging.Logger:
    """Configure logging for production environment."""
    return setup_logging("INFO", profile="production", log_file=log_file)


def configure_for_development(log_file: Optional[str] = None) -> logging.Logger:
    """Configure logging for development environment."""
    return setup_logging("DEBUG", profile="development", log_file=log_file, console_output=True)


def configure_for_testing(log_file: Optional[str] = None) -> logging.Logger:
    """Configure logging for testing environment."""
    return setup_logging("DEBUG", profile="testing", log_file=log_file, console_output=False)


def configure_for_ci(log_file: Optional[str] = None) -> logging.Logger:
    """Configure logging for CI/CD environment."""
    return setup_logging("INFO", profile="ci", log_file=log_file, console_output=True)


def log_exception(logger: logging.Logger, message: str = "Exception occurred") -> None:
    """Log an exception with full traceback.
    
    Args:
        logger: Logger instance to use
        message: Message to log before exception
    
    Example:
        >>> logger = get_logger(__name__)
        >>> try:
        ...     do_something()
        ... except Exception:
        ...     log_exception(logger, "Failed to process")
    """
    logger.exception(message)
