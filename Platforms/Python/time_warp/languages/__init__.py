"""Languages package for Time Warp Studio — 12 language executors."""

from .basic import execute_basic
from .brainfuck import execute_brainfuck
from .c_lang_fixed import execute_c
from .erlang import execute_erlang
from .forth import execute_forth
from .hypertalk import execute_hypertalk
from .javascript import execute_javascript
from .logo import execute_logo
from .lua import execute_lua
from .pascal import execute_pascal
from .pilot import execute_pilot
from .prolog import execute_prolog

__all__ = [
    "execute_basic",
    "execute_brainfuck",
    "execute_c",
    "execute_erlang",
    "execute_forth",
    "execute_hypertalk",
    "execute_javascript",
    "execute_logo",
    "execute_lua",
    "execute_pascal",
    "execute_pilot",
    "execute_prolog",
]
