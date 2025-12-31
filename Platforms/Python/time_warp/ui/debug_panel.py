# pylint: disable=too-few-public-methods
"""Debug panel with controls, watch expressions, and call stack."""

# pylint: disable=no-name-in-module

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QFrame,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QSplitter,
    QTableWidget,
    QTableWidgetItem,
    QToolButton,
    QVBoxLayout,
    QWidget,
)


class DebugToolbar(QFrame):
    """Toolbar with debug control buttons."""

    # Signals for debug actions
    start_debug = Signal()
    stop_debug = Signal()
    step_into = Signal()
    step_over = Signal()
    step_out = Signal()
    continue_execution = Signal()
    pause_execution = Signal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self._setup_ui()
        self._is_debugging = False
        self._is_paused = False

    def _setup_ui(self):
        """Setup the toolbar UI."""
        self.setFrameShape(QFrame.StyledPanel)
        self.setStyleSheet(
            """
            QFrame {
                background-color: palette(window);
                border: 1px solid palette(dark);
                border-radius: 4px;
                padding: 4px;
            }
            QToolButton {
                background-color: transparent;
                border: 1px solid transparent;
                border-radius: 4px;
                padding: 6px 10px;
                font-size: 14px;
                min-width: 32px;
            }
            QToolButton:hover {
                background-color: palette(highlight);
                color: palette(highlighted-text);
            }
            QToolButton:pressed {
                background-color: palette(dark);
            }
            QToolButton:disabled {
                color: palette(disabled-text);
            }
        """
        )

        layout = QHBoxLayout(self)
        layout.setContentsMargins(4, 2, 4, 2)
        layout.setSpacing(4)

        # Debug label
        debug_label = QLabel("🐛 Debug:")
        debug_label.setStyleSheet("font-weight: bold; padding: 0 8px;")
        layout.addWidget(debug_label)

        # Start/Stop Debug
        self.start_btn = QToolButton()
        self.start_btn.setText("▶️ Start")
        self.start_btn.setToolTip("Start Debugging (F5)")
        self.start_btn.clicked.connect(self.start_debug.emit)
        layout.addWidget(self.start_btn)

        self.stop_btn = QToolButton()
        self.stop_btn.setText("⏹️ Stop")
        self.stop_btn.setToolTip("Stop Debugging (Shift+F5)")
        self.stop_btn.setEnabled(False)
        self.stop_btn.clicked.connect(self.stop_debug.emit)
        layout.addWidget(self.stop_btn)

        layout.addSpacing(10)

        # Pause/Continue
        self.pause_btn = QToolButton()
        self.pause_btn.setText("⏸️ Pause")
        self.pause_btn.setToolTip("Pause Execution (F6)")
        self.pause_btn.setEnabled(False)
        self.pause_btn.clicked.connect(self.pause_execution.emit)
        layout.addWidget(self.pause_btn)

        self.continue_btn = QToolButton()
        self.continue_btn.setText("▶▶ Continue")
        self.continue_btn.setToolTip("Continue Execution (F5)")
        self.continue_btn.setEnabled(False)
        self.continue_btn.clicked.connect(self.continue_execution.emit)
        layout.addWidget(self.continue_btn)

        layout.addSpacing(10)

        # Step controls
        self.step_into_btn = QToolButton()
        self.step_into_btn.setText("↓ Step Into")
        self.step_into_btn.setToolTip("Step Into (F11)")
        self.step_into_btn.setEnabled(False)
        self.step_into_btn.clicked.connect(self.step_into.emit)
        layout.addWidget(self.step_into_btn)

        self.step_over_btn = QToolButton()
        self.step_over_btn.setText("→ Step Over")
        self.step_over_btn.setToolTip("Step Over (F10)")
        self.step_over_btn.setEnabled(False)
        self.step_over_btn.clicked.connect(self.step_over.emit)
        layout.addWidget(self.step_over_btn)

        self.step_out_btn = QToolButton()
        self.step_out_btn.setText("↑ Step Out")
        self.step_out_btn.setToolTip("Step Out (Shift+F11)")
        self.step_out_btn.setEnabled(False)
        self.step_out_btn.clicked.connect(self.step_out.emit)
        layout.addWidget(self.step_out_btn)

        layout.addStretch()

        # Status label
        self.status_label = QLabel("Ready")
        self.status_label.setStyleSheet(
            """
            QLabel {
                background-color: palette(base);
                border: 1px solid palette(dark);
                border-radius: 3px;
                padding: 2px 8px;
                font-weight: bold;
            }
        """
        )
        layout.addWidget(self.status_label)

    def set_debugging(self, is_debugging: bool):
        """Update toolbar state for debugging mode."""
        self._is_debugging = is_debugging
        self.start_btn.setEnabled(not is_debugging)
        self.stop_btn.setEnabled(is_debugging)
        self.pause_btn.setEnabled(is_debugging and not self._is_paused)

        if is_debugging:
            self.status_label.setText("🟢 Running")
            self.status_label.setStyleSheet(
                """
                QLabel {
                    background-color: #2d5016;
                    color: #90EE90;
                    border: 1px solid #4a8c1c;
                    border-radius: 3px;
                    padding: 2px 8px;
                    font-weight: bold;
                }
            """
            )
        else:
            self.status_label.setText("⚪ Ready")
            self.status_label.setStyleSheet(
                """
                QLabel {
                    background-color: palette(base);
                    border: 1px solid palette(dark);
                    border-radius: 3px;
                    padding: 2px 8px;
                    font-weight: bold;
                }
            """
            )

    def set_paused(self, is_paused: bool, line: int = 0):
        """Update toolbar state for paused execution."""
        self._is_paused = is_paused
        self.pause_btn.setEnabled(self._is_debugging and not is_paused)
        self.continue_btn.setEnabled(is_paused)
        self.step_into_btn.setEnabled(is_paused)
        self.step_over_btn.setEnabled(is_paused)
        self.step_out_btn.setEnabled(is_paused)

        if is_paused:
            self.status_label.setText(f"🟡 Paused (Line {line})")
            self.status_label.setStyleSheet(
                """
                QLabel {
                    background-color: #5c4a00;
                    color: #FFD700;
                    border: 1px solid #8c7000;
                    border-radius: 3px;
                    padding: 2px 8px;
                    font-weight: bold;
                }
            """
            )
        elif self._is_debugging:
            self.status_label.setText("🟢 Running")
            self.status_label.setStyleSheet(
                """
                QLabel {
                    background-color: #2d5016;
                    color: #90EE90;
                    border: 1px solid #4a8c1c;
                    border-radius: 3px;
                    padding: 2px 8px;
                    font-weight: bold;
                }
            """
            )


class WatchPanel(QWidget):
    """Panel for watching variable expressions."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._watches = []  # List of watch expressions
        self._variables = {}  # Current variables from interpreter
        self._setup_ui()

    def _setup_ui(self):
        """Setup the watch panel UI."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(4)

        # Header with add watch input
        header_layout = QHBoxLayout()

        self.watch_input = QLineEdit()
        self.watch_input.setPlaceholderText("Add watch expression...")
        self.watch_input.setFont(QFont("Courier New", 10))
        self.watch_input.returnPressed.connect(self._add_watch)
        header_layout.addWidget(self.watch_input)

        add_btn = QPushButton("+")
        add_btn.setFixedWidth(30)
        add_btn.setToolTip("Add watch expression")
        add_btn.clicked.connect(self._add_watch)
        header_layout.addWidget(add_btn)

        layout.addLayout(header_layout)

        # Watch table
        self.watch_table = QTableWidget()
        self.watch_table.setColumnCount(3)
        self.watch_table.setHorizontalHeaderLabels(["Expression", "Value", ""])
        self.watch_table.horizontalHeader().setSectionResizeMode(
            0, QHeaderView.Stretch
        )
        self.watch_table.horizontalHeader().setSectionResizeMode(
            1, QHeaderView.Stretch
        )
        self.watch_table.horizontalHeader().setSectionResizeMode(
            2, QHeaderView.Fixed
        )
        self.watch_table.setColumnWidth(2, 30)
        self.watch_table.verticalHeader().setVisible(False)
        self.watch_table.setAlternatingRowColors(True)
        self.watch_table.setFont(QFont("Courier New", 10))
        layout.addWidget(self.watch_table)

    def _add_watch(self):
        """Add a watch expression."""
        expr = self.watch_input.text().strip()
        if expr and expr not in self._watches:
            self._watches.append(expr)
            self._update_display()
        self.watch_input.clear()

    def _remove_watch(self, expr: str):
        """Remove a watch expression."""
        if expr in self._watches:
            self._watches.remove(expr)
            self._update_display()

    def update_variables(self, variables: dict):
        """Update with current variable values."""
        self._variables = variables
        self._update_display()

    def _evaluate_expression(self, expr: str) -> str:
        """Evaluate a watch expression against current variables."""
        # Simple variable lookup - could be extended for expressions
        expr_upper = expr.upper()
        if expr_upper in self._variables:
            return str(self._variables[expr_upper])
        if expr in self._variables:
            return str(self._variables[expr])
        return "<not found>"

    def _update_display(self):
        """Update the watch table display."""
        self.watch_table.setRowCount(len(self._watches))

        for row, expr in enumerate(self._watches):
            # Expression
            expr_item = QTableWidgetItem(expr)
            expr_item.setFlags(expr_item.flags() & ~Qt.ItemIsEditable)
            self.watch_table.setItem(row, 0, expr_item)

            # Value
            value = self._evaluate_expression(expr)
            value_item = QTableWidgetItem(value)
            value_item.setFlags(value_item.flags() & ~Qt.ItemIsEditable)
            self.watch_table.setItem(row, 1, value_item)

            # Remove button
            remove_btn = QPushButton("×")
            remove_btn.setFixedSize(24, 24)
            remove_btn.setStyleSheet("color: #ff6b6b; font-weight: bold;")
            remove_btn.clicked.connect(lambda _, e=expr: self._remove_watch(e))
            self.watch_table.setCellWidget(row, 2, remove_btn)

    def clear_watches(self):
        """Clear all watch expressions."""
        self._watches.clear()
        self._update_display()


class CallStackPanel(QWidget):
    """Panel for displaying the call stack."""

    # Signal when a stack frame is selected
    frame_selected = Signal(int)  # line number

    def __init__(self, parent=None):
        super().__init__(parent)
        self._setup_ui()

    def _setup_ui(self):
        """Setup the call stack panel UI."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        # Stack list
        self.stack_list = QListWidget()
        self.stack_list.setFont(QFont("Courier New", 10))
        self.stack_list.setAlternatingRowColors(True)
        self.stack_list.itemDoubleClicked.connect(self._on_frame_selected)
        layout.addWidget(self.stack_list)

    def update_call_stack(self, call_stack: list):
        """Update the call stack display.

        Args:
            call_stack: List of (line_number, subroutine_name) tuples
        """
        self.stack_list.clear()

        if not call_stack:
            item = QListWidgetItem("(empty)")
            item.setFlags(item.flags() & ~Qt.ItemIsSelectable)
            self.stack_list.addItem(item)
            return

        for i, frame in enumerate(reversed(call_stack)):
            if isinstance(frame, tuple) and len(frame) >= 2:
                line, name = frame[0], frame[1]
                text = f"#{i}: {name} (Line {line})"
            else:
                text = f"#{i}: Line {frame}"
            item = QListWidgetItem(text)
            item.setData(Qt.UserRole, frame)
            self.stack_list.addItem(item)

    def _on_frame_selected(self, item: QListWidgetItem):
        """Handle stack frame selection."""
        frame = item.data(Qt.UserRole)
        if frame:
            line = frame[0] if isinstance(frame, tuple) else frame
            self.frame_selected.emit(line)

    def clear(self):
        """Clear the call stack display."""
        self.stack_list.clear()


class BreakpointPanel(QWidget):
    """Panel for managing breakpoints."""

    # Signals
    breakpoint_toggled = Signal(int)  # line number
    breakpoint_goto = Signal(int)  # line number
    clear_all_requested = Signal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self._breakpoints = set()
        self._setup_ui()

    def _setup_ui(self):
        """Setup the breakpoint panel UI."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(4)

        # Header with clear all button
        header_layout = QHBoxLayout()
        header_layout.addWidget(QLabel("Breakpoints:"))
        header_layout.addStretch()

        clear_btn = QPushButton("Clear All")
        clear_btn.setFixedWidth(70)
        clear_btn.clicked.connect(self.clear_all_requested.emit)
        header_layout.addWidget(clear_btn)

        layout.addLayout(header_layout)

        # Breakpoint list
        self.bp_list = QListWidget()
        self.bp_list.setFont(QFont("Courier New", 10))
        self.bp_list.setAlternatingRowColors(True)
        self.bp_list.itemDoubleClicked.connect(self._on_breakpoint_goto)
        layout.addWidget(self.bp_list)

    def update_breakpoints(self, breakpoints: set):
        """Update the breakpoint list."""
        self._breakpoints = breakpoints
        self._update_display()

    def _update_display(self):
        """Update the breakpoint list display."""
        self.bp_list.clear()

        if not self._breakpoints:
            item = QListWidgetItem("(no breakpoints)")
            item.setFlags(item.flags() & ~Qt.ItemIsSelectable)
            self.bp_list.addItem(item)
            return

        for line in sorted(self._breakpoints):
            item = QListWidgetItem(f"🔴 Line {line}")
            item.setData(Qt.UserRole, line)
            self.bp_list.addItem(item)

    def _on_breakpoint_goto(self, item: QListWidgetItem):
        """Handle double-click to go to breakpoint line."""
        line = item.data(Qt.UserRole)
        if line:
            self.breakpoint_goto.emit(line)


class DebugPanel(QWidget):
    """Main debug panel containing all debug components."""

    # Signals forwarded from components
    start_debug = Signal()
    stop_debug = Signal()
    step_into = Signal()
    step_over = Signal()
    step_out = Signal()
    continue_execution = Signal()
    pause_execution = Signal()
    breakpoint_toggled = Signal(int)
    goto_line = Signal(int)

    def __init__(self, parent=None):
        super().__init__(parent)
        self._setup_ui()
        self._connect_signals()

    def _setup_ui(self):
        """Setup the debug panel UI."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(4)

        # Debug toolbar at top
        self.toolbar = DebugToolbar(self)
        layout.addWidget(self.toolbar)

        # Splitter for sub-panels
        splitter = QSplitter(Qt.Vertical)

        # Watch expressions panel
        watch_container = QWidget()
        watch_layout = QVBoxLayout(watch_container)
        watch_layout.setContentsMargins(4, 4, 4, 4)
        watch_label = QLabel("👁️ Watch Expressions")
        watch_label.setStyleSheet("font-weight: bold;")
        watch_layout.addWidget(watch_label)
        self.watch_panel = WatchPanel(self)
        watch_layout.addWidget(self.watch_panel)
        splitter.addWidget(watch_container)

        # Call stack panel
        stack_container = QWidget()
        stack_layout = QVBoxLayout(stack_container)
        stack_layout.setContentsMargins(4, 4, 4, 4)
        stack_label = QLabel("📚 Call Stack")
        stack_label.setStyleSheet("font-weight: bold;")
        stack_layout.addWidget(stack_label)
        self.call_stack_panel = CallStackPanel(self)
        stack_layout.addWidget(self.call_stack_panel)
        splitter.addWidget(stack_container)

        # Breakpoints panel
        bp_container = QWidget()
        bp_layout = QVBoxLayout(bp_container)
        bp_layout.setContentsMargins(4, 4, 4, 4)
        bp_label = QLabel("🔴 Breakpoints")
        bp_label.setStyleSheet("font-weight: bold;")
        bp_layout.addWidget(bp_label)
        self.breakpoint_panel = BreakpointPanel(self)
        bp_layout.addWidget(self.breakpoint_panel)
        splitter.addWidget(bp_container)

        layout.addWidget(splitter)

    def _connect_signals(self):
        """Connect internal signals."""
        # Forward toolbar signals
        self.toolbar.start_debug.connect(self.start_debug.emit)
        self.toolbar.stop_debug.connect(self.stop_debug.emit)
        self.toolbar.step_into.connect(self.step_into.emit)
        self.toolbar.step_over.connect(self.step_over.emit)
        self.toolbar.step_out.connect(self.step_out.emit)
        self.toolbar.continue_execution.connect(self.continue_execution.emit)
        self.toolbar.pause_execution.connect(self.pause_execution.emit)

        # Forward other panel signals
        self.call_stack_panel.frame_selected.connect(self.goto_line.emit)
        self.breakpoint_panel.breakpoint_goto.connect(self.goto_line.emit)
        self.breakpoint_panel.breakpoint_toggled.connect(
            self.breakpoint_toggled.emit
        )

    def set_debugging(self, is_debugging: bool):
        """Update UI for debugging state."""
        self.toolbar.set_debugging(is_debugging)
        if not is_debugging:
            self.call_stack_panel.clear()

    def set_paused(self, is_paused: bool, line: int = 0):
        """Update UI for paused state."""
        self.toolbar.set_paused(is_paused, line)

    def update_variables(self, variables: dict):
        """Update watch panel with variables."""
        self.watch_panel.update_variables(variables)

    def update_call_stack(self, call_stack: list):
        """Update call stack display."""
        self.call_stack_panel.update_call_stack(call_stack)

    def update_breakpoints(self, breakpoints: set):
        """Update breakpoints display."""
        self.breakpoint_panel.update_breakpoints(breakpoints)
