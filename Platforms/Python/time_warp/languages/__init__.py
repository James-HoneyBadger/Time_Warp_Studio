"""Languages package for Time Warp Studio — all 24 language executors."""

from .apl import execute_apl
from .assembly import execute_assembly
from .basic import execute_basic
from .brainfuck import execute_brainfuck
from .c_lang_fixed import execute_c
from .cics import execute_cics
from .cobol import execute_cobol
from .forth import execute_forth
from .fortran import execute_fortran
from .haskell import execute_haskell
from .hypertalk import execute_hypertalk
from .javascript import execute_javascript
from .jcl import execute_jcl
from .logo import execute_logo
from .lua import execute_lua
from .pascal import execute_pascal
from .pilot import execute_pilot
from .prolog import execute_prolog
from .python import execute_python
from .rexx import execute_rexx
from .scheme import execute_scheme
from .smalltalk import execute_smalltalk
from .sql import execute_sql
from .sqr import execute_sqr

__all__ = [
    "execute_apl",
    "execute_assembly",
    "execute_basic",
    "execute_brainfuck",
    "execute_c",
    "execute_cics",
    "execute_cobol",
    "execute_forth",
    "execute_fortran",
    "execute_haskell",
    "execute_hypertalk",
    "execute_javascript",
    "execute_jcl",
    "execute_logo",
    "execute_lua",
    "execute_pascal",
    "execute_pilot",
    "execute_prolog",
    "execute_python",
    "execute_rexx",
    "execute_scheme",
    "execute_smalltalk",
    "execute_sql",
    "execute_sqr",
]
