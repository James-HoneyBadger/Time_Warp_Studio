"""Language-level feature modules used by the BASIC interpreter.

These are not part of the (removed) PySide6 application: each is reached by name
from a running program -- ``SOUND``/``PLAY`` (music), ``SPEAK`` (speech),
``SHAPE`` (shapes), ``PARTICLE`` (particles), the fractal commands, ``GAMEPAD``,
and ``INKEY$``/``TIMER``/collision (game support).

The interpreter imports them lazily, so importing one here would force that cost
on every program.  Nothing is re-exported: import the submodule you need
(``from ..features.game_support import get_game_state``).
"""
