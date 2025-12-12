"""
Time Warp IDE - Python Implementation
Educational programming environment (unified BASIC, PILOT, Logo)
Aligned with Time Warp IDE v5.0.1 release
"""

__version__ = "5.0.0"
__author__ = "James-HoneyBadger"

from typing import Any, TYPE_CHECKING

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
]
