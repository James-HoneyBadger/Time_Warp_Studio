"""Languages package for Time Warp IDE - Separate language executors."""

from .basic import execute_basic
from .pilot import execute_pilot
from .logo import execute_logo

__all__ = [
    "execute_pilot",
    "execute_basic",
    "execute_logo",
]
