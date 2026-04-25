"""Languages package for Time Warp Studio — all 20 language executors."""

from .basic import execute_basic
from .brainfuck import execute_brainfuck
from .c_lang_fixed import execute_c
from .erlang import execute_erlang
from .forth import execute_forth
from .haskell import execute_haskell
from .hypertalk import execute_hypertalk
from .javascript import execute_javascript
from .logo import execute_logo
from .lua import execute_lua
from .pascal import execute_pascal
from .perl import execute_perl
from .pilot import execute_pilot
from .prolog import execute_prolog
from .python import execute_python
from .rexx import execute_rexx
from .ruby import execute_ruby
from .rust import execute_rust
from .scheme import execute_scheme
from .smalltalk import execute_smalltalk

__all__ = [
    "execute_basic",
    "execute_brainfuck",
    "execute_c",
    "execute_erlang",
    "execute_forth",
    "execute_haskell",
    "execute_hypertalk",
    "execute_javascript",
    "execute_logo",
    "execute_lua",
    "execute_pascal",
    "execute_perl",
    "execute_pilot",
    "execute_prolog",
    "execute_python",
    "execute_rexx",
    "execute_ruby",
    "execute_rust",
    "execute_scheme",
    "execute_smalltalk",
]
