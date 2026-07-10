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

            value_str = self._format_value(name, value)

            value_item = QTableWidgetItem(value_str)

            self.setItem(row, 0, name_item)
            self.setItem(row, 1, value_item)

    def _format_value(self, name: str, value: Any) -> str:
        """Format a variable value for display in the table."""
        if name == "__hardware__" and isinstance(value, dict):
            environment = value.get("environment", {})
            devices = value.get("devices", {})
            env_bits = []
            for key in ("temperature", "humidity", "light_level"):
                if key in environment:
                    env_bits.append(f"{key}={environment[key]}")
            device_lines = []
            for device_id, info in sorted(devices.items()):
                device_lines.append(
                    f"{device_id}: type={info.get('type', '?')}, "
                    f"active={info.get('active', False)}, "
                    f"value={info.get('value', 0)}"
                )
            summary = [
                "Hardware snapshot",
                f"Environment: {', '.join(env_bits) if env_bits else 'none'}",
                f"Devices: {len(devices)}",
            ]
            if device_lines:
                summary.append("Device details:")
                summary.extend(f"  {line}" for line in device_lines)
            return "\n".join(summary)

        # Format value based on type
        if isinstance(value, (list, dict, set)):
            return str(value)
        if isinstance(value, float):
            return f"{value:.4f}"
        return str(value)
