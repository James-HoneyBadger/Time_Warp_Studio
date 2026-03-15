"""Output panel with interpreter execution."""

# PySide6 names sometimes trigger false positives in static analysis.
# Disable 'no-name-in-module' here; PySide6 provides these symbols at runtime.
# pylint: disable=no-name-in-module

import copy
import os
import re
import threading
import time

from PySide6.QtCore import QSettings, Qt, QThread, QTimer, Signal
from PySide6.QtGui import QColor, QFont, QKeySequence, QTextCharFormat, QTextCursor
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QLineEdit,
    QPushButton,
    QTextEdit,
    QVBoxLayout,
    QWidget,
)

from ..core.debugger import ExecutionTimeline
from ..core.interpreter import Interpreter, Language
from ..graphics.turtle_state import TurtleState

# Set to True or use TWS_DEBUG=1 env var to enable verbose debug output
_DEBUG = os.environ.get("TWS_DEBUG", "").strip() in ("1", "true", "yes")


class REPLThread(QThread):
    """Background thread for REPL / immediate-mode single-command execution.

    Keeps the main UI thread free while a REPL command is running.
    """

    execution_complete = Signal(list, object)  # (output_lines, variables_dict)
    error_occurred = Signal(str)

    def __init__(self, interpreter, turtle, command, language, parent=None):
        super().__init__(parent)
        self._interp = interpreter
        self._turtle = turtle
        self._command = command
        self._language = language

    def run(self):
        """Execute the REPL command in a background thread."""
        output_lines = []

        def capture(text):
            output_lines.append(text)

        self._interp.output_callback = capture
        try:
            self._interp.load_program(self._command, self._language)
            self._interp.execute(self._turtle)
            try:
                variables = self._interp.get_variables()
            except Exception:  # pylint: disable=broad-except
                variables = {}
            self.execution_complete.emit(list(output_lines), variables)
        except (SyntaxError, NameError, ValueError, TypeError, ZeroDivisionError) as e:
            self.error_occurred.emit(f"❌ Error: {e}")
        except Exception as e:  # pylint: disable=broad-except
            self.error_occurred.emit(f"❌ Unexpected error: {e}")


class InterpreterThread(QThread):
    """Thread for running interpreter."""

    output_ready = Signal(str, str)  # (text, type)
    execution_complete = Signal()
    error_occurred = Signal(str)
    state_changed = Signal()
    input_requested = Signal(str, bool)  # (prompt, is_numeric)
    debug_paused = Signal(int, dict)  # (line, variables)
    debug_frame_recorded = Signal(object)
    debug_timeline_ready = Signal(object)
    variables_updated = Signal(dict)  # variables dict after execution
    execution_stats = Signal(dict)  # execution metrics

    # The thread __init__ intentionally accepts several parameters for
    # configuration; silence the "too many args" checks for readability.
    # pylint: disable=too-many-arguments,too-many-positional-arguments
    def __init__(
        self,
        code,
        turtle,
        language=None,
        interpreter=None,
        debug_mode=False,
        breakpoints=None,
        debug_step_granularity="line",
    ):
        super().__init__()
        self.code = code
        self.turtle = turtle
        self.language = language
        self.should_stop = False
        self.interp = interpreter
        self.debug_mode = debug_mode
        self.breakpoints = breakpoints or set()
        self.debug_step_granularity = debug_step_granularity
        self._had_error = False
        self.step_delay_ms: int = 0  # 0 = no throttle

    def run(self):
        """Run interpreter in background."""
        start_time = time.perf_counter()
        _complete_emitted = False
        try:
            if self.interp is None:
                self.interp = Interpreter()
                self.interp.load_program(self.code, self.language)

            # Configure debugging
            self.interp.set_debug_mode(self.debug_mode)
            if self.debug_mode:
                timeline = ExecutionTimeline()
                timeline.start_recording()
                self.interp.set_debug_timeline(timeline)
                granularity = self.debug_step_granularity
                if self.language != Language.BASIC:
                    granularity = "line"
                self.interp.set_debug_step_granularity(granularity)
            if self.debug_mode:
                self.interp.step_mode = True  # Start paused at first line
            for bp in self.breakpoints:
                self.interp.add_breakpoint(bp)

            def on_debug(line, variables):
                self.debug_paused.emit(line, variables)

            self.interp.set_debug_callback(on_debug)

            def on_frame(frame):
                self.debug_frame_recorded.emit(frame)

            self.interp.set_debug_frame_callback(on_frame)

            # Setup streaming output (always reconnect signals)
            def on_output(text):
                if not self.should_stop:
                    self.output_ready.emit(text, "normal")
                    if self.step_delay_ms > 0:
                        time.sleep(self.step_delay_ms / 1000.0)

            self.interp.output_callback = on_output

            # Setup state change callback - emit the signal when turtle changes
            # The wrapper lambda captures the turtle and passes it to the
            # signal
            def on_turtle_change():
                try:
                    import sys

                    message = (
                        "[THREAD] Turtle changed! " f"{len(self.turtle.lines)} lines"
                    )
                    if _DEBUG:  # pragma: no cover
                        print(message, file=sys.stderr)
                except (BrokenPipeError, OSError):
                    pass
                self.state_changed.emit()

            self.turtle.on_change = on_turtle_change

            # Execute with timeout protection
            _TIMEOUT_SECONDS = 30.0

            def _on_timeout():
                """Kill runaway program after timeout."""
                if self.interp is not None:
                    self.interp.running = False
                self.output_ready.emit(
                    f"\n\u23f1\ufe0f Execution stopped: exceeded {_TIMEOUT_SECONDS:.0f} second timeout",
                    "error",
                )

            _timeout_timer = threading.Timer(_TIMEOUT_SECONDS, _on_timeout)
            _timeout_timer.daemon = True
            _timeout_timer.start()
            try:
                self.interp.execute(self.turtle)
            finally:
                _timeout_timer.cancel()

            if self.debug_mode and self.interp.debug_timeline:
                self.interp.debug_timeline.stop_recording()
                self.debug_timeline_ready.emit(self.interp.debug_timeline)

            if self.interp.pending_input:
                req = self.interp.pending_input
                self.input_requested.emit(req.prompt, req.is_numeric)
            elif not self.should_stop:
                self.output_ready.emit("\n✅ Execution complete", "success")
                # Emit variables after execution
                variables = self.interp.get_variables()
                self.variables_updated.emit(variables)
                duration_ms = (time.perf_counter() - start_time) * 1000
                line_count = len(
                    [line for line in self.code.splitlines() if line.strip()]
                )
                self.execution_stats.emit(
                    {
                        "duration_ms": duration_ms,
                        "lines": line_count,
                        "language": self.language,
                        "successful": not self._had_error,
                    }
                )
                self.execution_complete.emit()
                _complete_emitted = True

        except Exception as e:  # pylint: disable=broad-except
            self._had_error = True
            self.error_occurred.emit(str(e))
            # Still emit variables on error if interpreter exists
            if self.interp:
                try:
                    variables = self.interp.get_variables()
                    self.variables_updated.emit(variables)
                except (ValueError, TypeError):  # pylint: disable=broad-except
                    pass
            duration_ms = (time.perf_counter() - start_time) * 1000
            line_count = len([line for line in self.code.splitlines() if line.strip()])
            self.execution_stats.emit(
                {
                    "duration_ms": duration_ms,
                    "lines": line_count,
                    "language": self.language,
                    "successful": False,
                }
            )
            self.execution_complete.emit()
            _complete_emitted = True
            if self.debug_mode and self.interp and self.interp.debug_timeline:
                self.interp.debug_timeline.stop_recording()
                self.debug_timeline_ready.emit(self.interp.debug_timeline)

        finally:
            # Always re-enable the Run button, regardless of how execution ended
            if not _complete_emitted:
                self.execution_complete.emit()

    def stop(self):
        """Request thread to stop."""
        self.should_stop = True
        if self.interp:
            self.interp.running = False


class OutputPanel(QTextEdit):
    """Output panel for program execution."""

    # Forwarded from the execution thread so callers can connect before the
    # interpreter thread is started (avoids a race where the thread emits a
    # pause before the UI has hooked the thread signal).
    debug_paused = Signal(int, dict)  # (line, variables)
    debug_frame_recorded = Signal(object)
    debug_timeline_ready = Signal(object)
    variables_updated = Signal(dict)  # variables after execution
    execution_stats = Signal(dict)
    error_occurred = Signal(str)
    execution_complete = Signal()
    output_streamed = Signal(str, str)  # (text, type)
    turtle_state_changed = Signal(object)
    turtle_state_reset = Signal()
    line_clicked = Signal(int)  # emitted when a line-number link is clicked

    # Regex to find line-number references in error messages
    _LINE_REF_RE = re.compile(r"((?:at\s+)?line\s+\d+)", re.IGNORECASE)

    def __init__(self, parent=None):
        super().__init__(parent)

        # Make read-only
        self.setReadOnly(True)

        # Font
        font = QFont("Courier New", 11)
        self.setFont(font)

        # Semantic colors (defaults — overridden by apply_theme_colors)
        self._theme_colors = {
            "error": QColor(255, 100, 100),
            "warning": QColor(255, 200, 100),
            "success": QColor(100, 255, 100),
            "info": QColor(100, 200, 255),
        }

        # Execution thread
        self.exec_thread = None

        # Current language
        self.current_language = None
        self.current_canvas = None

        # Store last error for AI assistance
        self.last_error = None
        # Reference to right_tabs for switching to Graphics tab
        self.tabs_widget = None
        self._debug_turtle_frames: dict[int, object] = {}

        # Cap output text to avoid unbounded memory growth.
        # Qt automatically drops the oldest blocks when the limit is exceeded.
        self.document().setMaximumBlockCount(5000)

        # Per-output-line execution delay (for speed throttle slider, 0 = no delay)
        self._step_delay_ms: int = 0

        # Live variable watch timer — polls interpreter state while a program is running
        self._live_watch_timer = QTimer(self)
        self._live_watch_timer.setInterval(500)  # poll every 500 ms
        self._live_watch_timer.timeout.connect(self._poll_live_variables)

        # Repaint throttle: coalesce rapid turtle-move signals into at most
        # ~30 repaints per second so a tight Logo loop can't flood the queue.
        self._paint_pending = False
        self._paint_timer = QTimer(self)
        self._paint_timer.setSingleShot(True)
        self._paint_timer.setInterval(33)  # ~30 fps
        self._paint_timer.timeout.connect(self._flush_paint)
        self._paint_turtle: object = None

        # Search bar (Ctrl+F) — hidden by default
        self._search_bar = self._create_search_bar()
        self._search_bar.setVisible(False)
        # We can't add child widgets to a QTextEdit directly; expose it for the
        # wrapping widget to embed.  See OutputPanelContainer below.

    def apply_theme_colors(self, theme):
        """Update semantic output colors from a Theme dataclass."""
        self._theme_colors = {
            "error": QColor(getattr(theme, "error_color", "#ff6464")),
            "warning": QColor(getattr(theme, "warning_color", "#ffc864")),
            "success": QColor(getattr(theme, "success_color", "#64ff64")),
            "info": QColor(getattr(theme, "info_color", "#64c8ff")),
        }

    def _create_search_bar(self) -> QWidget:
        """Build the collapsible search toolbar."""
        bar = QWidget()
        bar.setFixedHeight(32)
        layout = QHBoxLayout(bar)
        layout.setContentsMargins(4, 2, 4, 2)
        layout.setSpacing(4)
        layout.addWidget(QLabel("🔍"))
        self._search_field = QLineEdit()
        self._search_field.setPlaceholderText("Search output…")
        self._search_field.returnPressed.connect(self._search_next)
        self._search_field.textChanged.connect(self._search_highlight)
        layout.addWidget(self._search_field)
        btn_next = QPushButton("▼")
        btn_next.setFixedWidth(28)
        btn_next.setToolTip("Find next")
        btn_next.clicked.connect(self._search_next)
        layout.addWidget(btn_next)
        btn_prev = QPushButton("▲")
        btn_prev.setFixedWidth(28)
        btn_prev.setToolTip("Find previous")
        btn_prev.clicked.connect(self._search_prev)
        layout.addWidget(btn_prev)
        self._search_count_label = QLabel("")
        layout.addWidget(self._search_count_label)
        btn_close = QPushButton("✕")
        btn_close.setFixedWidth(24)
        btn_close.setToolTip("Close search bar (Esc)")
        btn_close.clicked.connect(self._hide_search_bar)
        layout.addWidget(btn_close)
        return bar

    def _show_search_bar(self):
        self._search_bar.setVisible(True)
        self._search_field.setFocus()
        self._search_field.selectAll()

    def _hide_search_bar(self):
        self._search_bar.setVisible(False)
        self.setFocus()
        # Clear extra highlights
        self._clear_search_highlights()
        self._search_count_label.setText("")

    def _clear_search_highlights(self):
        fmt = QTextCharFormat()
        cursor = self.textCursor()
        cursor.select(QTextCursor.SelectionType.Document)
        cursor.setCharFormat(fmt)
        cursor.clearSelection()
        self.setTextCursor(cursor)

    def _search_highlight(self, text: str):
        """Highlight all occurrences in yellow."""
        self._clear_search_highlights()
        if not text:
            self._search_count_label.setText("")
            return
        highlight_fmt = QTextCharFormat()
        highlight_fmt.setBackground(QColor("#FFD700"))
        highlight_fmt.setForeground(QColor("black"))
        count = 0
        cursor = self.document().find(text)
        while not cursor.isNull():
            cursor.mergeCharFormat(highlight_fmt)
            count += 1
            cursor = self.document().find(text, cursor)
        self._search_count_label.setText(f"{count} match{'es' if count != 1 else ''}")

    def _search_next(self):
        text = self._search_field.text()
        if text:
            found = self.find(text)
            if not found:
                # wrap around
                cursor = self.textCursor()
                cursor.movePosition(QTextCursor.MoveOperation.Start)
                self.setTextCursor(cursor)
                self.find(text)

    def _search_prev(self):
        from PySide6.QtGui import QTextDocument  # noqa: PLC0415

        text = self._search_field.text()
        if text:
            found = self.find(text, QTextDocument.FindFlag.FindBackward)
            if not found:
                cursor = self.textCursor()
                cursor.movePosition(QTextCursor.MoveOperation.End)
                self.setTextCursor(cursor)
                self.find(text, QTextDocument.FindFlag.FindBackward)

    def keyPressEvent(self, event):
        """Override keyPressEvent to intercept Ctrl+F and Escape."""
        if event.matches(QKeySequence.StandardKey.Find):
            self._show_search_bar()
            event.accept()
            return
        if event.key() == Qt.Key.Key_Escape and self._search_bar.isVisible():
            self._hide_search_bar()
            event.accept()
            return
        super().keyPressEvent(event)

    def _flush_paint(self):
        """Perform the deferred canvas repaint (runs on the main thread)."""
        self._paint_pending = False
        turtle = self._paint_turtle
        if turtle is None or self.current_canvas is None:
            return
        self.current_canvas.set_turtle_state(turtle)
        # Emit without deepcopy — listeners must not mutate the object.
        self.turtle_state_changed.emit(turtle)

    def set_language(self, language):
        """Set the current language for execution."""
        self.current_language = language

    def set_tabs_widget(self, tabs):
        """Set reference to tabs widget for auto-switching to Graphics tab."""
        self.tabs_widget = tabs

    def run_program(
        self,
        code,
        canvas,
        debug_mode=False,
        breakpoints=None,
        debug_step_granularity="line",
    ):
        """Run program in background thread."""
        import sys

        if _DEBUG:  # pragma: no cover
            try:
                print(
                    f"[OUTPUT] run_program called: canvas={canvas is not None}, "
                    f"code_length={len(code)}",
                    file=sys.stderr,
                )
            except (BrokenPipeError, OSError):
                pass

        if self.exec_thread and self.exec_thread.isRunning():
            self.append_colored("⚠️ Program already running", "warning")
            return

        self.current_canvas = canvas
        if _DEBUG:  # pragma: no cover
            try:
                print(
                    f"[OUTPUT] Set current_canvas = {canvas is not None}",
                    file=sys.stderr,
                )
            except (BrokenPipeError, OSError):
                pass

        # Clear and show header
        self.clear()
        self.append_colored("🚀 Running program...\n", "info")
        self.turtle_state_reset.emit()

        # Create new turtle
        turtle = TurtleState()
        try:
            if _DEBUG:  # pragma: no cover
                print("[OUTPUT] Created new turtle", file=sys.stderr)
        except (BrokenPipeError, OSError):
            pass

        # Create and start thread
        self._start_thread(
            code,
            turtle,
            self.current_language,
            debug_mode=debug_mode,
            breakpoints=breakpoints,
            debug_step_granularity=debug_step_granularity,
        )

    # This method coordinates starting the interpreter thread and intentionally
    # accepts several parameters (language, interpreter, debug options). Keep
    # the signature concise for clarity and disable the argument-count checks.
    # pylint: disable=R0913,R0917
    def _start_thread(
        self,
        code,
        turtle,
        language,
        interpreter=None,
        debug_mode=False,
        breakpoints=None,
        debug_step_granularity="line",
    ):
        """Start execution thread."""
        import sys

        if _DEBUG:  # pragma: no cover
            try:
                print(
                    f"[OUTPUT] _start_thread: language={language}, "
                    f"turtle={turtle is not None}",
                    file=sys.stderr,
                )
            except (BrokenPipeError, OSError):
                pass

        # Clean up the previous thread before creating a new one so Qt
        # objects and signal connections don't accumulate between runs.
        if self.exec_thread is not None:
            old = self.exec_thread
            self.exec_thread = None
            if old.isRunning():
                old.stop()
                old.quit()
                # Do not call wait() here — it blocks the main/UI thread.
                # Schedule deletion after the thread naturally finishes.
            old.deleteLater()

        self.exec_thread = InterpreterThread(
            code,
            turtle,
            language,
            interpreter,
            debug_mode,
            breakpoints,
            debug_step_granularity,
        )
        self.exec_thread.step_delay_ms = self._step_delay_ms
        self.exec_thread.output_ready.connect(self.on_output)
        self.exec_thread.error_occurred.connect(self.on_error)
        self.exec_thread.execution_complete.connect(
            lambda: self._on_execution_complete(self.current_canvas, turtle)
        )
        self.exec_thread.execution_stats.connect(self.execution_stats.emit)
        # Wrap state_changed connection so we don't build a long inline
        # lambda expression and exceed line-length limits.

        def _on_state_changed():
            if _DEBUG:  # pragma: no cover
                print(
                    "[OUTPUT] _on_state_changed signal received!",
                    file=sys.stderr,
                )
            self.on_state_change(turtle)

        self.exec_thread.state_changed.connect(_on_state_changed)
        if _DEBUG:  # pragma: no cover
            print("[OUTPUT] Connected state_changed signal", file=sys.stderr)

        self.exec_thread.input_requested.connect(self.on_input_requested)

        # Forward the execution thread debug_paused into the OutputPanel
        # signal so callers can connect to this signal even before the
        # thread is started. This prevents a race where the thread pauses
        # and emits debug_paused before the UI connects.
        # Connect the thread-level debug_paused to our panel-level signal.
        # This forwarder keeps callers able to attach handlers before the
        # thread is started (avoids races).
        try:
            self.exec_thread.debug_paused.connect(self._forward_debug_paused)
        except AttributeError:
            # If the thread doesn't expose debug_paused for any reason,
            # continue without breaking execution.
            pass

        try:
            self.exec_thread.debug_frame_recorded.connect(self._on_debug_frame_recorded)
            self.exec_thread.debug_timeline_ready.connect(
                self.debug_timeline_ready.emit
            )
        except AttributeError:
            pass

        # Forward variables_updated signal
        try:
            self.exec_thread.variables_updated.connect(self.variables_updated)
        except AttributeError:
            pass

        # Start the thread after connecting signals.
        self.exec_thread.start()
        # Start live variable polling while the program is running
        self._live_watch_timer.start()

    def _forward_debug_paused(self, line, variables):
        """Forward a pause event from the interpreter thread.

        Kept as a separate method to avoid long inline lambdas and to make
        the signal chain clearer during testing.
        """
        self.debug_paused.emit(line, variables)

    def _on_debug_frame_recorded(self, frame):
        """Capture turtle state per debug frame and emit signal."""
        if self.exec_thread and self.exec_thread.turtle:
            try:
                snapshot = copy.deepcopy(self.exec_thread.turtle)
            except (TypeError, ValueError):
                snapshot = self.exec_thread.turtle
            frame_id = getattr(frame, "timestamp", None)
            if frame_id is not None:
                self._debug_turtle_frames[frame_id] = snapshot
        self.debug_frame_recorded.emit(frame)

    def clear_debug_timeline(self):
        """Clear stored debug timeline snapshots."""
        self._debug_turtle_frames = {}

    def get_debug_turtle_snapshot(self, frame):
        """Get turtle snapshot for a debug frame."""
        frame_id = getattr(frame, "timestamp", None)
        if frame_id is None:
            return None
        return self._debug_turtle_frames.get(frame_id)

    def set_step_delay(self, ms: int) -> None:
        """Set per-output-line delay (0 = no throttle, max 500 ms)."""
        self._step_delay_ms = max(0, min(ms, 500))
        if self.exec_thread is not None:
            self.exec_thread.step_delay_ms = self._step_delay_ms

    def _poll_live_variables(self) -> None:
        """Emit variables_updated with the current interpreter state while running."""
        if self.exec_thread and self.exec_thread.interp:
            try:
                variables = self.exec_thread.interp.get_variables()
                self.variables_updated.emit(variables)
            except Exception:  # pylint: disable=broad-except
                pass

    def resume_execution(self):
        """Resume execution."""
        if self.exec_thread and self.exec_thread.interp:
            self.exec_thread.interp.resume_execution()

    def step_execution(self):
        """Step execution."""
        if self.exec_thread and self.exec_thread.interp:
            self.exec_thread.interp.step_execution()

    def on_state_change(self, turtle):
        """Handle turtle state change."""
        # DEBUG: Log state changes
        import sys

        message = (
            "[OUTPUT] on_state_change: "
            f"canvas={self.current_canvas is not None}, "
            f"lines={len(turtle.lines)}"
        )
        if _DEBUG:  # pragma: no cover
            print(message, file=sys.stderr)

        if self.current_canvas:
            if _DEBUG:  # pragma: no cover
                print(
                    (
                        "[OUTPUT] Calling set_turtle_state with "
                        f"{len(turtle.lines)} lines"
                    ),
                    file=sys.stderr,
                )
            # Throttle repaints: store the latest turtle reference and let
            # a timer coalesce rapid moves into at most ~30 repaints/sec.
            self._paint_turtle = turtle
            if not self._paint_pending:
                self._paint_pending = True
                self._paint_timer.start()
        else:
            if _DEBUG:  # pragma: no cover
                print("[OUTPUT] WARNING: current_canvas is None!", file=sys.stderr)

    def on_input_requested(self, prompt, _is_numeric):
        """Handle input request."""
        # Get interpreter from thread before it's destroyed
        interp = self.exec_thread.interp
        turtle = self.exec_thread.turtle
        code = self.exec_thread.code
        lang = self.exec_thread.language

        # Show dialog
        text, ok = QInputDialog.getText(
            self, "Input Required", prompt, QLineEdit.Normal, ""
        )

        if ok:
            # Provide input
            if interp is None:
                self.append_colored("\n❌ Input cancelled: interpreter unavailable", "error")
                return
            interp.provide_input(text)
            # Resume execution
            self._start_thread(code, turtle, lang, interp)
        else:
            self.append_colored("\n❌ Input cancelled", "error")

    def on_output(self, text, output_type):
        """Handle output from interpreter."""
        self.append_colored(text, output_type)
        self.output_streamed.emit(text, output_type)

    def on_error(self, error):
        """Handle error from interpreter with suggestions."""
        self.last_error = error  # Store for AI assistance
        self.append_colored(f"\n❌ Error: {error}", "error")

        self.error_occurred.emit(error)
        self.output_streamed.emit(f"\n❌ Error: {error}", "error")

        # Provide suggestions based on error type
        suggestions = self._get_error_suggestions(error)
        if suggestions:
            self.append_colored(f"\n💡 Suggestions: {suggestions}", "info")

    def _get_error_suggestions(self, error):
        """Get suggestions for common errors.

        To keep the logic compact and avoid many early returns, match
        substrings against a small mapping of heuristic suggestions.
        """
        error_lower = error.lower()

        suggestions_map = [
            (
                "undefined variable",
                "Check variable spelling or declare first.",
            ),
            (
                "syntax error",
                "Check for missing keywords, incorrect operators, or "
                "unmatched parentheses.",
            ),
            (
                "division by zero",
                "Ensure denominators aren't zero. Add checks.",
            ),
            (
                "type mismatch",
                "Check data types - strings and numbers cannot be mixed.",
            ),
            (
                "cannot convert",
                "Check data types - strings and numbers cannot be mixed.",
            ),
            (
                "index out of bounds",
                "Array indices start from 0. Check array size.",
            ),
            (
                "missing operand",
                "Operators need values on both sides (e.g. A+).",
            ),
            (
                "unexpected character",
                "Remove invalid characters. Check quotes and brackets.",
            ),
            (
                "mismatched parentheses",
                "Count opening and closing parentheses - they must match.",
            ),
            (
                "unknown command",
                ("Check spelling of commands. " "Use language-specific syntax."),
            ),
            (
                "unknown keyword",
                ("Check spelling of commands. " "Use language-specific syntax."),
            ),
            (
                "invalid expression",
                "Expressions must be valid math or logic. "
                + "Check operators and variables.",
            ),
        ]

        for needle, suggestion in suggestions_map:
            if needle in error_lower:
                return suggestion

        return None

    def on_complete(self, canvas, turtle):
        """Handle execution complete."""
        # Update canvas with turtle lines (guard against None canvas)
        if canvas is not None:
            canvas.set_turtle_state(turtle)

    def _on_execution_complete(self, canvas, turtle):
        """Handle completion and notify listeners."""
        self._live_watch_timer.stop()
        try:
            self.on_complete(canvas, turtle)
        except Exception:  # pylint: disable=broad-except
            # Never let canvas errors prevent the completion signal from firing
            pass
        finally:
            self.execution_complete.emit()

    def stop_execution(self):
        """Stop running program."""
        if self.exec_thread and self.exec_thread.isRunning():
            self.append_colored("\n⏹️ Stopping...", "warning")
            self.exec_thread.stop()
            # Do NOT call wait() here — it blocks the main/UI thread.
            # The thread will terminate shortly and emit execution_complete.

    def is_running(self):
        """Check if execution is running."""
        return self.exec_thread and self.exec_thread.isRunning()

    def mousePressEvent(self, event):
        """Detect clicks on line-number hyperlinks in error output."""
        if event.button() == Qt.LeftButton:
            anchor = self.anchorAt(event.position().toPoint())
            if anchor.startswith("line:"):
                try:
                    line = int(anchor.split(":", 1)[1])
                    self.line_clicked.emit(line)
                except (ValueError, IndexError):
                    pass
        super().mousePressEvent(event)

    def append_colored(self, text, color_type="normal"):
        """Append colored text, with clickable line references in errors."""
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.End)

        # Base format
        base_fmt = QTextCharFormat()
        if color_type in self._theme_colors:
            base_fmt.setForeground(self._theme_colors[color_type])

        # For error output, make line-number references into clickable links
        if color_type == "error":
            parts = self._LINE_REF_RE.split(text)
            for part in parts:
                if self._LINE_REF_RE.fullmatch(part):
                    # Extract the numeric part from e.g. "line 42" / "at line 5"
                    num_m = re.search(r"\d+", part)
                    if num_m:
                        link_fmt = QTextCharFormat(base_fmt)
                        link_fmt.setAnchor(True)
                        link_fmt.setAnchorHref(f"line:{num_m.group()}")
                        link_fmt.setForeground(QColor(100, 210, 255))
                        link_fmt.setFontUnderline(True)
                        cursor.setCharFormat(link_fmt)
                        cursor.insertText(part)
                        continue
                cursor.setCharFormat(base_fmt)
                cursor.insertText(part)
            # Reset format and add newline
            cursor.setCharFormat(QTextCharFormat())
            cursor.insertText("\n")
        else:
            cursor.setCharFormat(base_fmt)
            cursor.insertText(text + "\n")

        # Auto-scroll
        self.setTextCursor(cursor)
        self.ensureCursorVisible()

    def get_last_error(self):
        """Get the last error message for AI assistance."""
        return self.last_error


class ImmediateModePanel(QWidget):
    """Simple command input for immediate mode execution.

    Just a command line - output goes to the main Output panel.
    """

    # Signal emitted when variables change
    variables_updated = Signal(dict)
    # Signal to send output to the main output panel
    output_ready = Signal(str, str)  # (text, type)

    def __init__(self, parent=None):
        super().__init__(parent)

        # Persistent interpreter for immediate mode
        self.interpreter = Interpreter()
        self.turtle = TurtleState()
        self.canvas = None
        self.output_panel = None  # Reference to main output panel
        self.current_language = Language.BASIC
        self._repl_thread = None  # Background thread for REPL execution

        # Command history — loaded from persistent settings
        from ..core.config import QSETTINGS_APP, QSETTINGS_ORG

        _s = QSettings(QSETTINGS_ORG, QSETTINGS_APP)
        saved_history = _s.value("repl_history", [])
        self.command_history = list(saved_history) if saved_history else []
        self.history_index = -1

        self._setup_ui()

    def _setup_ui(self):
        """Setup the immediate mode UI - just a command input line."""
        layout = QHBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(4)

        # Prompt label
        self.prompt_label = QLabel("READY>")
        self.prompt_label.setFont(QFont("Courier New", 11))
        self.prompt_label.setStyleSheet("color: #50fa7b; font-weight: bold;")
        layout.addWidget(self.prompt_label)

        # Command input field
        self.command_input = QLineEdit()
        self.command_input.setFont(QFont("Courier New", 11))
        self.command_input.setPlaceholderText(
            "Enter command (e.g., PRINT 10+5, LET X=100, FORWARD 50)"
        )
        self.command_input.returnPressed.connect(self._execute_command)
        layout.addWidget(self.command_input)

        # Execute button
        self.exec_button = QPushButton("Run")
        self.exec_button.clicked.connect(self._execute_command)
        self.exec_button.setFixedWidth(50)
        layout.addWidget(self.exec_button)

        # Install event filter for command history navigation
        self.command_input.installEventFilter(self)

    def eventFilter(self, obj, event):  # pylint: disable=invalid-name
        """Handle key events for command history."""
        if obj == self.command_input and event.type() == event.Type.KeyPress:
            if event.key() == Qt.Key_Up:
                self._history_up()
                return True
            elif event.key() == Qt.Key_Down:
                self._history_down()
                return True
        return super().eventFilter(obj, event)

    def _history_up(self):
        """Navigate up in command history."""
        if self.command_history and (
            self.history_index < len(self.command_history) - 1
        ):
            self.history_index += 1
            cmd = self.command_history[-(self.history_index + 1)]
            self.command_input.setText(cmd)
            self.command_input.selectAll()

    def _history_down(self):
        """Navigate down in command history."""
        if self.history_index > 0:
            self.history_index -= 1
            cmd = self.command_history[-(self.history_index + 1)]
            self.command_input.setText(cmd)
            self.command_input.selectAll()
        elif self.history_index == 0:
            self.history_index = -1
            self.command_input.clear()

    def set_canvas(self, canvas):
        """Set the canvas for turtle graphics."""
        self.canvas = canvas
        self.turtle.on_change = self._on_turtle_change

    def set_output_panel(self, output_panel):
        """Set the output panel to send results to."""
        self.output_panel = output_panel

    def set_language(self, language):
        """Set the current language."""
        self.current_language = language
        # Update prompt based on language
        prompts = {
            Language.BASIC: "READY>",
            Language.PILOT: "PILOT>",
            Language.LOGO: "LOGO>",
            Language.PYTHON: "PY>",
            Language.LUA: "LUA>",
            Language.SCHEME: "SCM>",
            Language.COBOL: "COBOL>",
            Language.BRAINFUCK: "BF>",
            Language.ASSEMBLY: "ASM>",
            Language.JAVASCRIPT: "JS>",
            Language.FORTRAN: "F77>",
            Language.REXX: "REXX>",
            Language.SMALLTALK: "ST>",
            Language.HYPERTALK: "HTALK>",
            Language.HASKELL: "HS>",
            Language.APL: "APL>",
            Language.SQL: "SQL>",
            Language.JCL: "JCL>",
            Language.CICS: "CICS>",
        }
        self.prompt_label.setText(prompts.get(language, "CMD>"))

    def _on_turtle_change(self):
        """Handle turtle state changes."""
        if self.canvas:
            self.canvas.set_turtle_state(self.turtle)

    def _send_output(self, text, output_type="normal"):
        """Send output to the main output panel."""
        if self.output_panel:
            self.output_panel.append_colored(text, output_type)
        self.output_ready.emit(text, output_type)

    def _execute_command(self):
        """Execute the entered command in a background thread (non-blocking)."""
        command = self.command_input.text().strip()
        if not command:
            return

        # Guard against running a second command while one is in flight
        if self._repl_thread and self._repl_thread.isRunning():
            return

        # Add to history and persist (cap at 200 entries)
        if not self.command_history or self.command_history[-1] != command:
            self.command_history.append(command)
        self.history_index = -1
        from ..core.config import QSETTINGS_APP, QSETTINGS_ORG

        _s = QSettings(QSETTINGS_ORG, QSETTINGS_APP)
        _s.setValue("repl_history", self.command_history[-200:])

        # Show command in output
        self._send_output(f"⚡ {command}", "info")

        # Disable input while the command is running so the UI stays consistent
        self.command_input.setEnabled(False)
        self.exec_button.setEnabled(False)
        self.command_input.clear()

        # Execute in a background thread to avoid blocking the main UI thread
        self._repl_thread = REPLThread(
            self.interpreter, self.turtle, command, self.current_language, self
        )
        self._repl_thread.execution_complete.connect(self._on_repl_complete)
        self._repl_thread.error_occurred.connect(self._on_repl_error)
        self._repl_thread.start()

    def _on_repl_complete(self, output_lines, variables):
        """Handle successful REPL execution completion (called on main thread)."""
        for line in output_lines:
            self._send_output(line, "normal")
        if self.canvas:
            self.canvas.set_turtle_state(self.turtle)
        self.variables_updated.emit(variables)
        self._repl_done()

    def _on_repl_error(self, message):
        """Handle REPL execution error (called on main thread)."""
        self._send_output(message, "error")
        self._repl_done()

    def _repl_done(self):
        """Re-enable the REPL input after execution finishes."""
        self.command_input.setEnabled(True)
        self.exec_button.setEnabled(True)
        self.command_input.setFocus()

    def clear_state(self):
        """Clear the interpreter state (variables, etc.)."""
        self.interpreter = Interpreter()
        self.turtle = TurtleState()
        if self.canvas:
            self.turtle.on_change = self._on_turtle_change
        self._send_output("🔄 State cleared", "info")
        self.variables_updated.emit({})


class OutputPanelContainer(QWidget):
    """Wrapper that embeds OutputPanel with a collapsible search bar at the top."""

    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        self.output_panel = OutputPanel(self)
        # Insert search bar at top (hidden initially)
        layout.addWidget(self.output_panel._search_bar)
        layout.addWidget(self.output_panel)

    def __getattr__(self, name: str):
        """Delegate all attribute lookups to the embedded OutputPanel."""
        return getattr(self.output_panel, name)
