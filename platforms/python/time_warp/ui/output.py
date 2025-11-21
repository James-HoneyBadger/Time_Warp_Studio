"""Output panel with interpreter execution."""

from PySide6.QtWidgets import QTextEdit
from PySide6.QtCore import QThread, Signal
from PySide6.QtGui import QTextCursor, QColor, QTextCharFormat

from ..core.interpreter import Interpreter
from ..graphics.turtle_state import TurtleState


class InterpreterThread(QThread):
    """Thread for running interpreter."""
    
    output_ready = Signal(str, str)  # (text, type)
    execution_complete = Signal()
    error_occurred = Signal(str)
    
    def __init__(self, code, turtle):
        super().__init__()
        self.code = code
        self.turtle = turtle
        self.should_stop = False
        
    def run(self):
        """Run interpreter in background."""
        try:
            interp = Interpreter()
            interp.load_program(self.code)
            
            # Execute with timeout protection
            output = interp.execute(self.turtle)
            
            # Send output
            for line in output:
                if self.should_stop:
                    break
                self.output_ready.emit(line, 'normal')
                
            if not self.should_stop:
                self.output_ready.emit(
                    '\n✅ Execution complete',
                    'success'
                )
                
        except Exception as e:
            self.error_occurred.emit(str(e))
        finally:
            self.execution_complete.emit()
            
    def stop(self):
        """Request thread to stop."""
        self.should_stop = True


class OutputPanel(QTextEdit):
    """Output panel for program execution."""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        
        # Make read-only
        self.setReadOnly(True)
        
        # Font
        from PySide6.QtGui import QFont
        font = QFont('Courier New', 11)
        self.setFont(font)
        
        # Execution thread
        self.exec_thread = None
        
    def run_program(self, code, canvas):
        """Run program in background thread."""
        if self.exec_thread and self.exec_thread.isRunning():
            self.append_colored(
                '⚠️ Program already running',
                'warning'
            )
            return
        
        # Clear and show header
        self.clear()
        self.append_colored('🚀 Running program...\n', 'info')
        
        # Create new turtle
        turtle = TurtleState()
        
        # Create and start thread
        self.exec_thread = InterpreterThread(code, turtle)
        self.exec_thread.output_ready.connect(self.on_output)
        self.exec_thread.error_occurred.connect(self.on_error)
        self.exec_thread.execution_complete.connect(
            lambda: self.on_complete(canvas, turtle)
        )
        self.exec_thread.start()
        
    def on_output(self, text, output_type):
        """Handle output from interpreter."""
        self.append_colored(text, output_type)
        
    def on_error(self, error):
        """Handle error from interpreter."""
        self.append_colored(f'\n❌ Error: {error}', 'error')
        
    def on_complete(self, canvas, turtle):
        """Handle execution complete."""
        # Update canvas with turtle lines
        canvas.set_turtle_state(turtle)
        
    def stop_execution(self):
        """Stop running program."""
        if self.exec_thread and self.exec_thread.isRunning():
            self.append_colored('\n⏹️ Stopping...', 'warning')
            self.exec_thread.stop()
            self.exec_thread.wait(2000)  # Wait up to 2 seconds
            
    def is_running(self):
        """Check if execution is running."""
        return self.exec_thread and self.exec_thread.isRunning()
        
    def append_colored(self, text, color_type='normal'):
        """Append colored text."""
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.End)
        
        # Create format
        fmt = QTextCharFormat()
        
        if color_type == 'error':
            fmt.setForeground(QColor(255, 100, 100))  # Red
        elif color_type == 'warning':
            fmt.setForeground(QColor(255, 200, 100))  # Orange
        elif color_type == 'success':
            fmt.setForeground(QColor(100, 255, 100))  # Green
        elif color_type == 'info':
            fmt.setForeground(QColor(100, 200, 255))  # Blue
        
        cursor.setCharFormat(fmt)
        cursor.insertText(text + '\n')
        
        # Auto-scroll
        self.setTextCursor(cursor)
        self.ensureCursorVisible()
