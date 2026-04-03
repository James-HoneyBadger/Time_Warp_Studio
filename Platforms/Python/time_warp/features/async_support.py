"""
Async Interpreter Runner for Time Warp Studio

Provides non-blocking code execution via AsyncInterpreterRunner,
wrapping the threading model already used by the UI layer.
"""

from __future__ import annotations

import threading
from typing import Any, Callable, Dict, List, Optional


class AsyncInterpreterRunner:
    """
    Runs interpreter code asynchronously in a background thread.

    Usage::

        runner = get_async_runner()
        runner.run_code("10 PRINT 'Hello'", language="BASIC",
                        callback=lambda output: print(output))

    The callback receives the collected output lines as a list of strings.
    It is called from the background thread; use Qt signals or
    ``threading.Event`` to marshal results back to the UI thread when needed.
    """

    def __init__(self) -> None:
        self._thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def run_code(
        self,
        code_string: str,
        language: Optional[str] = None,
        callback: Optional[Callable[[List[str]], None]] = None,
        error_callback: Optional[Callable[[str], None]] = None,
    ) -> None:
        """Start code execution in a background thread.

        Args:
            code_string: Source code to execute.
            language: Language identifier (e.g. ``"BASIC"``, ``"LOGO"``).
                      ``None`` lets the interpreter auto-detect.
            callback: Called with ``list[str]`` of output lines on success.
            error_callback: Called with an error message string on failure.
                            If *None*, errors are silently swallowed.
        """
        if self._thread is not None and self._thread.is_alive():
            self.stop()

        self._stop_event.clear()
        self._thread = threading.Thread(
            target=self._run_worker,
            args=(code_string, language, callback, error_callback),
            daemon=True,
            name="AsyncInterpreterRunner",
        )
        self._thread.start()

    def stop(self) -> None:
        """Request the background thread to stop and wait for it."""
        self._stop_event.set()
        if self._thread is not None:
            self._thread.join(timeout=5.0)
            self._thread = None

    @property
    def is_running(self) -> bool:
        """True if a background execution is currently in progress."""
        return self._thread is not None and self._thread.is_alive()

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _run_worker(
        self,
        code_string: str,
        language: Optional[str],
        callback: Optional[Callable[[List[str]], None]],
        error_callback: Optional[Callable[[str], None]],
    ) -> None:
        """Worker executed in a background thread."""
        try:
            from ..core.interpreter import Interpreter, Language
            from ..graphics.turtle_state import TurtleState

            interp = Interpreter()
            # Accept a Language enum or a raw string identifier
            lang: Optional[Language] = None
            if language is not None:
                try:
                    lang = Language[language.upper()]
                except KeyError:
                    lang = None
            interp.load_program(code_string, lang)

            turtle = TurtleState()
            output_lines: List[str] = interp.execute(turtle)

            if callback is not None:
                callback(output_lines)
        except Exception as exc:  # noqa: BLE001
            msg = f"❌ AsyncInterpreterRunner error: {exc}"
            if error_callback is not None:
                error_callback(msg)

    def get_stats(self) -> Dict[str, Any]:
        """Return basic status information."""
        return {
            "is_running": self.is_running,
            "thread_name": self._thread.name if self._thread else None,
        }


# Module-level singleton ---------------------------------------------------

_runner_instance: Optional[AsyncInterpreterRunner] = None


def get_async_runner() -> AsyncInterpreterRunner:
    """Return the module-level singleton AsyncInterpreterRunner.

    Creates a new instance on first call.
    """
    global _runner_instance  # pylint: disable=global-statement
    if _runner_instance is None:
        _runner_instance = AsyncInterpreterRunner()
    return _runner_instance
