"""Output panel with interpreter execution."""

# PySide6 names sometimes trigger false positives in static analysis.
# Disable 'no-name-in-module' here; PySide6 provides these symbols at runtime.
# pylint: disable=no-name-in-module

from PySide6.QtCore import Qt, QThread, Signal
from PySide6.QtGui import QColor, QFont, QTextCharFormat, QTextCursor
from PySide6.QtWidgets import (
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QLineEdit,
    QPushButton,
    QTextEdit,
    QWidget,
)

from ..core.interpreter import Interpreter, Language
from ..graphics.turtle_state import TurtleState
from ..logging_config import get_logger


class InterpreterThread(QThread):
    """Thread for running interpreter."""

    output_ready = Signal(str, str)  # (text, type)
    execution_complete = Signal()
    error_occurred = Signal(str)
    state_changed = Signal()
    input_requested = Signal(str, bool)  # (prompt, is_numeric)
    debug_paused = Signal(int, dict)  # (line, variables)
    variables_updated = Signal(dict)  # variables dict after execution

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
    ):
        super().__init__()
        self.code = code
        self.turtle = turtle
        self.language = language
        self.should_stop = False
        self.interp = interpreter
        self.debug_mode = debug_mode
        self.breakpoints = breakpoints or set()

    def run(self):
        """Run interpreter in background."""
        try:
            if self.interp is None:
                self.interp = Interpreter()
                self.interp.load_program(self.code, self.language)

            # Configure debugging
            self.interp.set_debug_mode(self.debug_mode)
            if self.debug_mode:
                self.interp.step_mode = True  # Start paused at first line
            for bp in self.breakpoints:
                self.interp.add_breakpoint(bp)

            def on_debug(line, variables):
                self.debug_paused.emit(line, variables)

            self.interp.set_debug_callback(on_debug)

            # Setup streaming output (always reconnect signals)
            def on_output(text):
                if not self.should_stop:
                    self.output_ready.emit(text, "normal")

            self.interp.output_callback = on_output

            # Setup state change callback - emit the signal when turtle changes
            # The wrapper lambda captures the turtle and passes it to the
            # signal
            def on_turtle_change():
                try:
                    import sys

                    print(
                        f"[THREAD] Turtle changed! {len(self.turtle.lines)} lines",
                        file=sys.stderr,
                    )
                except (BrokenPipeError, OSError):
                    pass
                self.state_changed.emit()

            self.turtle.on_change = on_turtle_change

            # Execute with timeout protection
            self.interp.execute(self.turtle)

            if self.interp.pending_input:
                req = self.interp.pending_input
                self.input_requested.emit(req.prompt, req.is_numeric)
            elif not self.should_stop:
                self.output_ready.emit("\n✅ Execution complete", "success")
                # Emit variables after execution
                variables = self.interp.get_variables()
                self.variables_updated.emit(variables)
                self.execution_complete.emit()

        except (ValueError, RuntimeError, OSError) as e:
            self.error_occurred.emit(str(e))
            # Still emit variables on error if interpreter exists
            if self.interp:
                try:
                    variables = self.interp.get_variables()
                    self.variables_updated.emit(variables)
                except (ValueError, TypeError):  # pylint: disable=broad-except
                    pass
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
    variables_updated = Signal(dict)  # variables after execution

    def __init__(self, parent=None):
        super().__init__(parent)

        # Make read-only
        self.setReadOnly(True)

        # Font
        font = QFont("Courier New", 11)
        self.setFont(font)

        # Execution thread
        self.exec_thread = None

        # Current language
        self.current_language = None
        self.current_canvas = None

        # Store last error for AI assistance
        self.last_error = None
        self.tabs_widget = None  # Reference to right_tabs for switching to Graphics tab

    def set_language(self, language):
        """Set the current language for execution."""
        self.current_language = language

    def set_tabs_widget(self, tabs):
        """Set reference to tabs widget for auto-switching to Graphics tab."""
        self.tabs_widget = tabs

    def run_program(self, code, canvas, debug_mode=False, breakpoints=None):
        """Run program in background thread."""
        # DEBUG: Log program start
        import sys

        try:
            print(
                f"[OUTPUT] run_program called: canvas={canvas is not None}, "
                f"code_length={len(code)}",
                file=sys.stderr,
            )
        except (BrokenPipeError, OSError):
            pass  # Ignore broken pipe errors during debug output

        if self.exec_thread and self.exec_thread.isRunning():
            self.append_colored("⚠️ Program already running", "warning")
            return

        self.current_canvas = canvas
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

        # Create new turtle
        turtle = TurtleState()
        try:
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
    ):
        """Start execution thread."""
        import sys

        try:
            print(
                f"[OUTPUT] _start_thread: language={language}, "
                f"turtle={turtle is not None}",
                file=sys.stderr,
            )
        except (BrokenPipeError, OSError):
            pass

        self.exec_thread = InterpreterThread(
            code, turtle, language, interpreter, debug_mode, breakpoints
        )
        self.exec_thread.output_ready.connect(self.on_output)
        self.exec_thread.error_occurred.connect(self.on_error)
        self.exec_thread.execution_complete.connect(
            lambda: self.on_complete(self.current_canvas, turtle)
        )
        # Wrap state_changed connection so we don't build a long inline
        # lambda expression and exceed line-length limits.

        def _on_state_changed():
            print("[OUTPUT] _on_state_changed signal received!", file=sys.stderr)
            self.on_state_change(turtle)

        self.exec_thread.state_changed.connect(_on_state_changed)
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

        # Forward variables_updated signal
        try:
            self.exec_thread.variables_updated.connect(self.variables_updated)
        except AttributeError:
            pass

        # Start the thread after connecting signals.
        self.exec_thread.start()

    def _forward_debug_paused(self, line, variables):
        """Forward a pause event from the interpreter thread.

        Kept as a separate method to avoid long inline lambdas and to make
        the signal chain clearer during testing.
        """
        self.debug_paused.emit(line, variables)

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

        print(
            f"[OUTPUT] on_state_change: canvas={self.current_canvas is not None}, "
            f"lines={len(turtle.lines)}",
            file=sys.stderr,
        )

        if self.current_canvas:
            print(
                f"[OUTPUT] Calling set_turtle_state with {len(turtle.lines)} lines",
                file=sys.stderr,
            )
            self.current_canvas.set_turtle_state(turtle)

            # Auto-switch to Graphics tab when lines are being drawn
            if turtle.lines and self.tabs_widget:
                try:
                    # Find the Graphics tab and switch to it
                    for i in range(self.tabs_widget.count()):
                        if "Graphics" in self.tabs_widget.tabText(i):
                            print(
                                f"[OUTPUT] Switching to Graphics tab (index {i})",
                                file=sys.stderr,
                            )
                            self.tabs_widget.setCurrentIndex(i)
                            break
                except (AttributeError, RuntimeError) as e:
                    # tabs_widget not available or invalid, skip tab switching
                    print(f"[OUTPUT] Tab switching error: {e}", file=sys.stderr)
        else:
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
            interp.provide_input(text)
            # Resume execution
            self._start_thread(code, turtle, lang, interp)
        else:
            self.append_colored("\n❌ Input cancelled", "error")

    def on_output(self, text, output_type):
        """Handle output from interpreter."""
        self.append_colored(text, output_type)

    def on_error(self, error):
        """Handle error from interpreter with suggestions."""
        self.last_error = error  # Store for AI assistance
        self.append_colored(f"\n❌ Error: {error}", "error")

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
                "Check spelling of commands. " + "Use language-specific syntax.",
            ),
            (
                "unknown keyword",
                "Check spelling of commands. " + "Use language-specific syntax.",
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
        # Update canvas with turtle lines
        canvas.set_turtle_state(turtle)

    def stop_execution(self):
        """Stop running program."""
        if self.exec_thread and self.exec_thread.isRunning():
            self.append_colored("\n⏹️ Stopping...", "warning")
            self.exec_thread.stop()
            self.exec_thread.wait(2000)  # Wait up to 2 seconds

    def is_running(self):
        """Check if execution is running."""
        return self.exec_thread and self.exec_thread.isRunning()

    def append_colored(self, text, color_type="normal"):
        """Append colored text."""
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.End)

        # Create format
        fmt = QTextCharFormat()

        if color_type == "error":
            fmt.setForeground(QColor(255, 100, 100))  # Red
        elif color_type == "warning":
            fmt.setForeground(QColor(255, 200, 100))  # Orange
        elif color_type == "success":
            fmt.setForeground(QColor(100, 255, 100))  # Green
        elif color_type == "info":
            fmt.setForeground(QColor(100, 200, 255))  # Blue

        cursor.setCharFormat(fmt)
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

        # Command history
        self.command_history = []
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
        if self.command_history and self.history_index < len(self.command_history) - 1:
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
        """Execute the entered command."""
        command = self.command_input.text().strip()
        if not command:
            return

        # Add to history
        if not self.command_history or self.command_history[-1] != command:
            self.command_history.append(command)
        self.history_index = -1

        # Show command in output
        self._send_output(f"⚡ {command}", "info")

        # Capture output
        output_lines = []

        def capture_output(text):
            output_lines.append(text)

        self.interpreter.output_callback = capture_output

        try:
            # Load and execute single command
            self.interpreter.load_program(command, self.current_language)
            self.interpreter.execute(self.turtle)

            # Show any output
            for line in output_lines:
                self._send_output(line, "normal")

            # Update canvas if turtle was used
            if self.canvas:
                self.canvas.set_turtle_state(self.turtle)

            # Emit variables update
            variables = self.interpreter.get_variables()
            self.variables_updated.emit(variables)

        except (
            SyntaxError,
            NameError,
            ValueError,
            TypeError,
            ZeroDivisionError,
        ) as e:
            self._send_output(f"❌ Error: {e}", "error")
        except KeyboardInterrupt:
            self._send_output("⚠️ Execution interrupted", "warning")
        except Exception as e:  # pylint: disable=broad-except
            logger = get_logger(__name__)
            logger.exception("Unexpected error in command execution")
            self._send_output(f"❌ Unexpected error: {e}", "error")

        # Clear input and focus
        self.command_input.clear()
        self.command_input.setFocus()

    def clear_state(self):
        """Clear the interpreter state (variables, etc.)."""
        self.interpreter = Interpreter()
        self.turtle = TurtleState()
        if self.canvas:
            self.turtle.on_change = self._on_turtle_change
        self._send_output("🔄 State cleared", "info")
        self.variables_updated.emit({})
