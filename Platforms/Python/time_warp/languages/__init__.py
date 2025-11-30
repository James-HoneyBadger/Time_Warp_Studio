"""Languages package for Time Warp IDE - Separate language executors."""

from .basic import execute_basic
from .logo import execute_logo
from .pilot import execute_pilot

__all__ = [
    "execute_pilot",
    "execute_basic",
    "execute_logo",
]
