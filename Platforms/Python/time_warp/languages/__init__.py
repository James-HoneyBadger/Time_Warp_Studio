"""Languages package for Time Warp Studio — 24 language executors."""

from .apl import execute_apl
from .asm6502 import execute_asm6502
from .basic import execute_basic
from .brainfuck import execute_brainfuck
from .c_lang_fixed import execute_c
from .cobol import execute_cobol
from .erlang import execute_erlang
from .forth import execute_forth
from .haskell import execute_haskell
from .hypertalk import execute_hypertalk
from .javascript import execute_javascript
from .lisp import execute_lisp
from .logo import execute_logo
from .lua import execute_lua
from .pascal import execute_pascal
from .perl import execute_perl
from .pilot import execute_pilot
from .postscript import execute_postscript
from .prolog import execute_prolog
from .python_lang import execute_python_lang
from .rexx import execute_rexx
from .ruby import execute_ruby
from .smalltalk import execute_smalltalk
from .tcl import execute_tcl

__all__ = [
    "execute_apl",
    "execute_asm6502",
    "execute_basic",
    "execute_brainfuck",
    "execute_c",
    "execute_cobol",
    "execute_erlang",
    "execute_forth",
    "execute_haskell",
    "execute_hypertalk",
    "execute_javascript",
    "execute_lisp",
    "execute_logo",
    "execute_lua",
    "execute_pascal",
    "execute_perl",
    "execute_pilot",
    "execute_postscript",
    "execute_prolog",
    "execute_python_lang",
    "execute_rexx",
    "execute_ruby",
    "execute_smalltalk",
    "execute_tcl",
]
