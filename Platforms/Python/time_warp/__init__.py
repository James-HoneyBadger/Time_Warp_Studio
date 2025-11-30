"""
Time Warp IDE - Python Implementation
Educational programming environment (unified BASIC, PILOT, Logo)
Aligned with Time Warp IDE v4.0.0 release
"""

__version__ = "4.0.0"
__author__ = "James-HoneyBadger"

from .core.interpreter import ExecutionResult, Interpreter
from .graphics.turtle_state import TurtleState
from .utils.expression_evaluator import ExpressionEvaluator

__all__ = [
    "Interpreter",
    "ExecutionResult",
    "TurtleState",
    "ExpressionEvaluator",
]
