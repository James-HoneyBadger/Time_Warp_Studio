"""Variable inspector widget for debugging."""

# PySide6 names are created dynamically so static analysis tools may report
# `no-name-in-module` false positives. Silence that for this module.
# pylint: disable=no-name-in-module

from typing import Any, Dict

from PySide6.QtWidgets import QHeaderView  # pylint: disable=no-name-in-module
from PySide6.QtWidgets import (
    QTableWidget,
    QTableWidgetItem,
)


class VariableInspector(QTableWidget):
    """Widget to display variable values during debugging."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setup_ui()

    def setup_ui(self):
        """Setup the table layout."""
        self.setColumnCount(2)
        self.setHorizontalHeaderLabels(["Variable", "Value"])
        hdr = self.horizontalHeader()
        hdr.setSectionResizeMode(0, QHeaderView.ResizeToContents)
        hdr.setSectionResizeMode(1, QHeaderView.Stretch)
        self.verticalHeader().setVisible(False)
        self.setAlternatingRowColors(True)
        self.setEditTriggers(QTableWidget.NoEditTriggers)

    def update_variables(self, variables: Dict[str, Any]):
        """Update the display with new variable values."""
        self.setRowCount(len(variables))

        # Sort variables by name
        sorted_vars = sorted(variables.items())

        for row, (name, value) in enumerate(sorted_vars):
            name_item = QTableWidgetItem(str(name))

            # Format value based on type
            if isinstance(value, (list, dict, set)):
                value_str = str(value)
            elif isinstance(value, float):
                value_str = f"{value:.4f}"
            else:
                value_str = str(value)

            value_item = QTableWidgetItem(value_str)

            self.setItem(row, 0, name_item)
            self.setItem(row, 1, value_item)
