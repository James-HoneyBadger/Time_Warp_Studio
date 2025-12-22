"""Utilities module for Time Warp IDE."""

from .error_hints import check_syntax_mistakes, suggest_command
from .expression_evaluator import ExpressionEvaluator, Token
from .validators import (
    validate_arg_count,
    validate_numeric,
    validate_variable_name,
    validate_file_path,
    validate_range,
    ValidationError,
)
from .error_messages import ErrorMessage
from .execution_timeout import execution_timeout, with_timeout, ExecutionTimeoutError

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
