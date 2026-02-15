"""Languages package for Time Warp Studio - Separate language executors."""

from .basic import execute_basic
from .c_lang_fixed import execute_c
from .forth import execute_forth
from .logo import execute_logo
from .pascal import execute_pascal
from .pilot import execute_pilot
from .prolog import execute_prolog

__all__ = [
    "execute_basic",
    "execute_c",
    "execute_forth",
    "execute_logo",
    "execute_pascal",
    "execute_pilot",
    "execute_prolog",
]
