"""
Time Warp Studio - language bridge engine

The temporary Python engine that the Qt application spawns for the languages that
are not ported to C++ yet, and the reference the native engines are checked against.
Aligned with Time Warp Studio v15.0.0 release
"""

__version__ = "15.0.0"
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
]
