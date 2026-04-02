"""Base protocol and types for language executors.

Every language executor module must provide a public ``execute_<lang>``
function that conforms to :class:`LanguageExecutor`.  The two execution
modes differ only in *what* is passed as ``source``:

* **Whole-program executors** receive the entire program text and return
  a single string containing all output lines.
* **Line-by-line executors** are called once per logical line/command
  during the interpreter's main loop.

Both signatures are identical — ``(interpreter, source, turtle) -> str``.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Protocol, runtime_checkable

if TYPE_CHECKING:
    from time_warp.core.interpreter import Interpreter
    from time_warp.graphics.turtle_state import TurtleState


@runtime_checkable
class LanguageExecutor(Protocol):
    """Callable protocol that every ``execute_<lang>`` function satisfies.

    Parameters
    ----------
    interpreter:
        The active :class:`Interpreter` instance (provides variables,
        I/O callbacks, program state, etc.).
    source:
        Either the complete program source (whole-program executors) or
        a single command/line (line-by-line executors).
    turtle:
        The shared :class:`TurtleState` for graphics output.

    Returns
    -------
    str
        Accumulated output text.  Lines are separated by ``\\n``.
        Error lines are prefixed with ``❌``, info with ``ℹ️``, etc.
    """

    def __call__(  # noqa: E704
        self,
        interpreter: Interpreter,
        source: str,
        turtle: TurtleState,
    ) -> str: ...
