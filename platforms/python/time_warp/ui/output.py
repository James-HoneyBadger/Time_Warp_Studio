"""Output panel with interpreter execution."""

from PySide6.QtWidgets import QTextEdit, QInputDialog, QLineEdit
from PySide6.QtCore import QThread, Signal
from PySide6.QtGui import QTextCursor, QColor, QTextCharFormat

from ..core.interpreter import Interpreter
from ..graphics.turtle_state import TurtleState


class InterpreterThread(QThread):
    """Thread for running interpreter."""

    output_ready = Signal(str, str)  # (text, type)
    execution_complete = Signal()
    error_occurred = Signal(str)
    state_changed = Signal()
    input_requested = Signal(str, bool)  # (prompt, is_numeric)
    debug_paused = Signal(int, dict)  # (line, variables)

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

            # Setup state change callback
            self.turtle.on_change = self.state_changed.emit

            # Execute with timeout protection
            self.interp.execute(self.turtle)

            if self.interp.pending_input:
                req = self.interp.pending_input
                self.input_requested.emit(req.prompt, req.is_numeric)
            elif not self.should_stop:
                self.output_ready.emit("\n✅ Execution complete", "success")
                self.execution_complete.emit()

        except Exception as e:
            self.error_occurred.emit(str(e))
            self.execution_complete.emit()

    def stop(self):
        """Request thread to stop."""
        self.should_stop = True
        if self.interp:
            self.interp.running = False


class OutputPanel(QTextEdit):
    """Output panel for program execution."""

    def __init__(self, parent=None):
        super().__init__(parent)

        # Make read-only
        self.setReadOnly(True)

        # Font
        from PySide6.QtGui import QFont

        font = QFont("Courier New", 11)
        self.setFont(font)

        # Execution thread
        self.exec_thread = None

        # Current language
        self.current_language = None
        self.current_canvas = None

    def set_language(self, language):
        """Set the current language for execution."""
        self.current_language = language

    def run_program(self, code, canvas, debug_mode=False, breakpoints=None):
        """Run program in background thread."""
        if self.exec_thread and self.exec_thread.isRunning():
            self.append_colored("⚠️ Program already running", "warning")
            return

        self.current_canvas = canvas

        # Clear and show header
        self.clear()
        self.append_colored("🚀 Running program...\n", "info")

        # Create new turtle
        turtle = TurtleState()

        # Create and start thread
        self._start_thread(
            code,
            turtle,
            self.current_language,
            debug_mode=debug_mode,
            breakpoints=breakpoints,
        )

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
        self.exec_thread = InterpreterThread(
            code, turtle, language, interpreter, debug_mode, breakpoints
        )
        self.exec_thread.output_ready.connect(self.on_output)
        self.exec_thread.error_occurred.connect(self.on_error)
        self.exec_thread.execution_complete.connect(
            lambda: self.on_complete(self.current_canvas, turtle)
        )
        self.exec_thread.state_changed.connect(lambda: self.on_state_change(turtle))
        self.exec_thread.input_requested.connect(self.on_input_requested)
        self.exec_thread.start()

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
        if self.current_canvas:
            self.current_canvas.set_turtle_state(turtle)

    def on_input_requested(self, prompt, is_numeric):
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
        """Handle error from interpreter."""
        self.append_colored(f"\n❌ Error: {error}", "error")

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
