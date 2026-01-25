"""Utilities module for Time Warp Studio."""

from .error_hints import check_syntax_mistakes, suggest_command
from .error_messages import ErrorMessage
from .execution_timeout import (
    ExecutionTimeoutError,
    execution_timeout,
    with_timeout,
)
from .expression_evaluator import ExpressionEvaluator, Token
from .validators import (
    ValidationError,
    validate_arg_count,
    validate_file_path,
    validate_numeric,
    validate_range,
    validate_variable_name,
)

__all__ = [
    # Original exports
    "ExpressionEvaluator",
    "Token",
    "suggest_command",
    "check_syntax_mistakes",
    # Phase 1 exports
    "validate_arg_count",
    "validate_numeric",
    "validate_variable_name",
    "validate_file_path",
    "validate_range",
    "ValidationError",
    "ErrorMessage",
    "execution_timeout",
    "with_timeout",
    "ExecutionTimeoutError",
]
