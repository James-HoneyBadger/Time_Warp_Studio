"""
Error Explorer UI - Rich error navigator with jump-to-line,
suggestions, and state inspection.
"""

# pylint: disable=no-name-in-module

from __future__ import annotations

from typing import TYPE_CHECKING

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QColor
from PySide6.QtWidgets import (
    QDockWidget,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QTextEdit,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter


class ErrorExplorerWidget(QDockWidget):
    """Dock widget displaying errors with navigation and suggestions."""

    error_selected = Signal(int)  # Emits line number when error clicked

    def __init__(self, parent=None):
        super().__init__("Error Explorer", parent)
        self.setAllowedAreas(Qt.LeftDockWidgetArea | Qt.RightDockWidgetArea)

        # Main widget
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Header with clear button
        header = QHBoxLayout()
        self.error_count_label = QLabel("No errors")
        header.addWidget(self.error_count_label)
        header.addStretch()
        self.clear_btn = QPushButton("Clear")
        self.clear_btn.clicked.connect(self.clear_errors)
        header.addWidget(self.clear_btn)
        layout.addLayout(header)

        # Error tree
        self.error_tree = QTreeWidget()
        self.error_tree.setHeaderLabels(["Line", "Type", "Message"])
        self.error_tree.setColumnWidth(0, 60)
        self.error_tree.setColumnWidth(1, 80)
        self.error_tree.itemDoubleClicked.connect(self._on_error_double_click)
        layout.addWidget(self.error_tree)

        # Detail panel
        detail_label = QLabel("Error Details:")
        detail_label.setStyleSheet("font-weight: bold; margin-top: 10px;")
        layout.addWidget(detail_label)

        self.detail_text = QTextEdit()
        self.detail_text.setReadOnly(True)
        self.detail_text.setMaximumHeight(150)
        layout.addWidget(self.detail_text)

        # Action buttons
        actions = QHBoxLayout()
        self.jump_btn = QPushButton("Jump to Line")
        self.jump_btn.clicked.connect(self._jump_to_selected_error)
        self.jump_btn.setEnabled(False)
        actions.addWidget(self.jump_btn)

        self.copy_btn = QPushButton("Copy Error")
        self.copy_btn.clicked.connect(self._copy_selected_error)
        self.copy_btn.setEnabled(False)
        actions.addWidget(self.copy_btn)
        layout.addLayout(actions)

        self.setWidget(widget)

        # Connect tree selection
        self.error_tree.itemSelectionChanged.connect(
            self._on_selection_changed
        )

    def add_error(
        self,
        line_number: int,
        error_type: str,
        message: str,
        suggestion: str | None = None,
        state_info: str | None = None,
    ):  # pylint: disable=too-many-positional-arguments
        """Add an error to the explorer."""
        item = QTreeWidgetItem(
            [
                str(line_number),
                error_type,
                message[:50] + "..." if len(message) > 50 else message,
            ]
        )

        # Color code by type
        if error_type == "Syntax":
            item.setForeground(1, QColor(255, 100, 100))
        elif error_type == "Runtime":
            item.setForeground(1, QColor(255, 165, 0))
        elif error_type == "Warning":
            item.setForeground(1, QColor(255, 255, 100))

        # Store full details
        item.setData(
            0,
            Qt.UserRole,
            {
                "line": line_number,
                "type": error_type,
                "message": message,
                "suggestion": suggestion,
                "state": state_info,
            },
        )

        self.error_tree.addTopLevelItem(item)
        self._update_count()

    def clear_errors(self):
        """Clear all errors."""
        self.error_tree.clear()
        self.detail_text.clear()
        self._update_count()

    def parse_interpreter_output(
        self, output_lines: list[str], interpreter: Interpreter
    ):
        """Parse interpreter output and extract errors."""
        self.clear_errors()

        for i, line in enumerate(output_lines):
            if line.startswith("❌"):
                # Extract line number if present
                line_num = i + 1
                if "line" in line.lower():
                    try:
                        parts = line.split("line")
                        if len(parts) > 1:
                            num_str = parts[1].split(":")[0].strip()
                            line_num = int(num_str)
                    except (ValueError, IndexError):
                        pass

                # Determine error type
                error_type = "Runtime"
                if "syntax" in line.lower():
                    error_type = "Syntax"

                # Extract message
                message = line.replace("❌", "").strip()

                # Look for suggestions (💡)
                suggestion = None
                if i + 1 < len(output_lines) and "💡" in output_lines[i + 1]:
                    suggestion = output_lines[i + 1].replace("💡", "").strip()

                # Get state info
                state_info = self._format_state_info(interpreter)

                self.add_error(
                    line_num, error_type, message, suggestion, state_info
                )

    def _format_state_info(self, interpreter: Interpreter) -> str:
        """Format interpreter state for error context."""
        info_parts = []

        # Current line
        if interpreter.current_line < len(interpreter.program_lines):
            info_parts.append(f"Current line: {interpreter.current_line + 1}")

        # Variables (top 5)
        if interpreter.variables:
            var_list = list(interpreter.variables.items())[:5]
            var_str = ", ".join(f"{k}={v}" for k, v in var_list)
            info_parts.append(f"Variables: {var_str}")

        # Call stack depth
        if (
            hasattr(interpreter, "subroutine_stack")
            and interpreter.subroutine_stack
        ):
            info_parts.append(
                f"Call depth: {len(interpreter.subroutine_stack)}"
            )

        return (
            "\n".join(info_parts) if info_parts else "No state info available"
        )

    def _update_count(self):
        """Update error count label."""
        count = self.error_tree.topLevelItemCount()
        if count == 0:
            self.error_count_label.setText("No errors")
        elif count == 1:
            self.error_count_label.setText("1 error")
        else:
            self.error_count_label.setText(f"{count} errors")

    def _on_selection_changed(self):
        """Handle error selection."""
        selected = self.error_tree.selectedItems()
        has_selection = len(selected) > 0

        self.jump_btn.setEnabled(has_selection)
        self.copy_btn.setEnabled(has_selection)

        if has_selection:
            data = selected[0].data(0, Qt.UserRole)
            self._display_error_details(data)

    def _display_error_details(self, error_data: dict):
        """Display full error details."""
        details = []
        details.append(f"<b>Line {error_data['line']}</b>")
        details.append(f"<b>Type:</b> {error_data['type']}")
        details.append(f"<b>Message:</b> {error_data['message']}")

        if error_data.get("suggestion"):
            details.append(
                f"<p><b>💡 Suggestion:</b> {error_data['suggestion']}</p>"
            )

        if error_data.get("state"):
            details.append(
                f"<p><b>State:</b><br><pre>{error_data['state']}</pre></p>"
            )

        self.detail_text.setHtml("<br>".join(details))

    def _on_error_double_click(self, item: QTreeWidgetItem, _column: int):
        """Handle double-click on error."""
        data = item.data(0, Qt.UserRole)
        self.error_selected.emit(data["line"])

    def _jump_to_selected_error(self):
        """Jump to the selected error line."""
        selected = self.error_tree.selectedItems()
        if selected:
            data = selected[0].data(0, Qt.UserRole)
            self.error_selected.emit(data["line"])

    def _copy_selected_error(self):
        """Copy selected error to clipboard."""
        # pylint: disable=import-outside-toplevel
        from PySide6.QtWidgets import QApplication

        selected = self.error_tree.selectedItems()
        if selected:
            data = selected[0].data(0, Qt.UserRole)
            text = f"Line {data['line']}: {data['message']}"
            if data.get("suggestion"):
                text += f"\nSuggestion: {data['suggestion']}"
            QApplication.clipboard().setText(text)
