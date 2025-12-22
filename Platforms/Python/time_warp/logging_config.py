"""Logging configuration for Time Warp IDE.

Provides structured logging with console and optional file output,
replacing ad-hoc print() statements throughout the codebase.
"""

import logging
import logging.handlers
import sys
from pathlib import Path
from typing import Optional


def get_logger(name: str) -> logging.Logger:
    """Get or create a logger for the specified module name.
    
    Args:
        name: Logger name (typically __name__)
    
    Returns:
        Configured logger instance
    """
    return logging.getLogger(name)


def setup_logging(
    log_level: int = logging.INFO,
    log_file: Optional[Path] = None,
    include_timestamps: bool = True,
) -> logging.Logger:
    """Configure Time Warp IDE logging system.
    
    Sets up console and optional file logging with consistent formatting.
    This function should be called once at application startup.
    
    Args:
        log_level: Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
        log_file: Optional path to log file. If provided, rotates at 10MB
        include_timestamps: Whether to include timestamps in log messages
    
    Returns:
        Root logger instance
    
    Example:
        >>> setup_logging(logging.DEBUG, Path.home() / ".time_warp.log")
        >>> logger = get_logger(__name__)
        >>> logger.info("Application started")
    """
    root_logger = logging.getLogger()  # Get the global root logger
    root_logger.setLevel(log_level)
    
    # Avoid duplicate handlers
    root_logger.handlers.clear()
    
    # Determine format based on timestamp preference
    if include_timestamps:
        log_format = (
            "%(asctime)s - %(name)s - %(levelname)-8s - %(message)s"
        )
        date_format = "%Y-%m-%d %H:%M:%S"
    else:
        log_format = "%(name)s - %(levelname)-8s - %(message)s"
        date_format = None
    
    formatter = logging.Formatter(log_format, datefmt=date_format)
    
    # Console handler (always enabled)
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setLevel(log_level)
    console_handler.setFormatter(formatter)
    root_logger.addHandler(console_handler)
    
    # File handler (if specified)
    if log_file:
        log_file = Path(log_file)
        log_file.parent.mkdir(parents=True, exist_ok=True)
        
        file_handler = logging.handlers.RotatingFileHandler(
            log_file,
            maxBytes=10 * 1024 * 1024,  # 10 MB
            backupCount=5,
        )
        file_handler.setLevel(log_level)
        file_handler.setFormatter(formatter)
        root_logger.addHandler(file_handler)
    
    return root_logger


def configure_for_testing(verbose: bool = False) -> logging.Logger:
    """Configure logging for test environment.
    
    Uses higher verbosity by default to aid in debugging test failures.
    
    Args:
        verbose: If True, use DEBUG level; otherwise use WARNING
    
    Returns:
        Root logger instance
    """
    level = logging.DEBUG if verbose else logging.WARNING
    return setup_logging(log_level=level, log_file=None, include_timestamps=False)


def configure_for_ci(log_file: Optional[Path] = None) -> logging.Logger:
    """Configure logging for CI environment (GitHub Actions, etc.)
    
    Includes timestamps and file output for audit trails.
    
    Args:
        log_file: Optional custom log file path. If None, uses default.
    
    Returns:
        Root logger instance
    """
    if log_file is None:
        log_dir = Path.home() / ".time_warp" / "logs"
        log_file = log_dir / "ci_run.log"
    return setup_logging(log_level=logging.DEBUG, log_file=log_file)
