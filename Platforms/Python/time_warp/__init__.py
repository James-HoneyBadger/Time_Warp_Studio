"""
Time Warp Studio - Python Implementation
Educational programming environment supporting 9 languages
Aligned with Time Warp Studio v14.0.0 release
"""

__version__ = "14.0.0"
__author__ = "James-HoneyBadger"

from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from .core.interpreter import ExecutionResult, Interpreter
else:
    # Runtime fallback: keep names bound to Any to satisfy callers
    ExecutionResult = Any  # type: ignore
    Interpreter = Any  # type: ignore

if TYPE_CHECKING:
    from .graphics.turtle_state import TurtleState  # type: ignore
else:
    TurtleState = Any  # type: ignore

if TYPE_CHECKING:
    from .utils.expression_evaluator import ExpressionEvaluator
else:
    ExpressionEvaluator = Any  # type: ignore

__all__ = [
    "Interpreter",
    "ExecutionResult",
    "TurtleState",
    "ExpressionEvaluator",
    "run",
]
