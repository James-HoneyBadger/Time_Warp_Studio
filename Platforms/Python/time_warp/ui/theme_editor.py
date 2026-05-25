"""Theme editor dialog — lets users customise and save new themes.

The :class:`ThemeEditorDialog` allows users to pick colours for every
configurable theme field and save the result as a new named theme in the
:class:`~time_warp.ui.themes.ThemeManager`.  Use
:func:`open_theme_editor` as a convenience helper from the main window.

Legacy :class:`ThemeEditor` (basic QWidget) kept as a compatibility shim.
"""

from __future__ import annotations

import json
from dataclasses import asdict
from pathlib import Path
from typing import TYPE_CHECKING

from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QFont
from PySide6.QtWidgets import (
    QColorDialog,
    QDialog,
    QDialogButtonBox,
    QFileDialog,
    QFormLayout,
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QMessageBox,
    QPushButton,
    QScrollArea,
    QVBoxLayout,
    QWidget,
)

if TYPE_CHECKING:
    from .themes import Theme, ThemeManager


# ---------------------------------------------------------------------------
# Color-field metadata
# ---------------------------------------------------------------------------

_COLOR_FIELDS = [
    "background",
    "foreground",
    "editor_bg",
    "editor_fg",
    "line_number_bg",
    "line_number_fg",
    "keyword",
    "comment",
    "string",
    "number",
    "selection_bg",
    "selection_fg",
    "canvas_bg",
    "operator",
    "function",
    "variable",
    "error_color",
    "warning_color",
    "success_color",
    "info_color",
]

_OPTIONAL_COLOR_FIELDS = ["menu_bg", "menu_fg", "border_color"]


# ---------------------------------------------------------------------------
# Color swatch button
# ---------------------------------------------------------------------------


class _ColorButton(QPushButton):
    """Button showing a colour swatch; click to open a colour picker."""

    def __init__(self, color: str | None, parent: QWidget | None = None):
        super().__init__(parent)
        self._color: str | None = color
        self.setFixedSize(90, 24)
        self._refresh()
        self.clicked.connect(self._pick)

    def _refresh(self):
        if self._color:
            text_col = "#ffffff" if QColor(self._color).lightness() < 128 else "#000000"
            self.setStyleSheet(
                f"background-color: {self._color}; color: {text_col}; "
                "border: 1px solid #555; border-radius: 3px;"
            )
            self.setText(self._color)
        else:
            self.setStyleSheet("border: 1px solid #555; border-radius: 3px;")
            self.setText("(inherit)")

    def _pick(self):
        initial = QColor(self._color) if self._color else QColor("#ffffff")
        chosen = QColorDialog.getColor(initial, self, "Choose colour")
        if chosen.isValid():
            self._color = chosen.name()
            self._refresh()

    def color(self) -> str | None:
        return self._color

    def clear_color(self):
        self._color = None
        self._refresh()


# ---------------------------------------------------------------------------
# Main dialog
# ---------------------------------------------------------------------------


class ThemeEditorDialog(QDialog):
    """Dialog to create or edit a custom theme.

    Parameters
    ----------
    theme_manager:
        The application :class:`~time_warp.ui.themes.ThemeManager` instance.
    base_theme:
        When provided, pre-fills all colour fields with the base theme's
        values so the user can create a variant.
    parent:
        Parent widget.
    """

    def __init__(
        self,
        theme_manager: "ThemeManager",
        base_theme: "Theme | None" = None,
        parent: QWidget | None = None,
    ):
        super().__init__(parent)
        self._theme_manager = theme_manager
        self._base_theme = base_theme
        self._saved_name: str | None = None

        self.setWindowTitle("Theme Editor")
        self.resize(440, 620)
        self.setModal(True)

        self._buttons: dict[str, _ColorButton] = {}
        self._build_ui()

    # ------------------------------------------------------------------
    # UI
    # ------------------------------------------------------------------

    def _build_ui(self):
        outer = QVBoxLayout(self)

        # Heading
        base_name = self._base_theme.name if self._base_theme else "new"
        outer.addWidget(QLabel(f"<b>Editing based on: {base_name}</b>"))

        # Scrollable color form
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        content = QWidget()
        form = QFormLayout(content)
        form.setLabelAlignment(Qt.AlignmentFlag.AlignRight)
        form.setFieldGrowthPolicy(QFormLayout.FieldGrowthPolicy.FieldsStayAtSizeHint)

        base = asdict(self._base_theme) if self._base_theme else {}

        for field in _COLOR_FIELDS:
            val: str = base.get(field) or "#ffffff"
            btn = _ColorButton(val)
            self._buttons[field] = btn
            form.addRow(field.replace("_", " ").title() + ":", btn)

        form.addRow(QLabel("─── Optional (inherit = use fallback) ───"))

        for field in _OPTIONAL_COLOR_FIELDS:
            val_opt: str | None = base.get(field)  # may be None
            btn = _ColorButton(val_opt)

            row_w = QWidget()
            row_l = QHBoxLayout(row_w)
            row_l.setContentsMargins(0, 0, 0, 0)
            row_l.setSpacing(4)
            row_l.addWidget(btn)
            clear = QPushButton("✕")
            clear.setFixedSize(24, 24)
            clear.setToolTip("Reset to inherit")
            clear.clicked.connect(lambda _, b=btn: b.clear_color())
            row_l.addWidget(clear)
            row_l.addStretch()

            self._buttons[field] = btn
            form.addRow(field.replace("_", " ").title() + ":", row_w)

        scroll.setWidget(content)
        outer.addWidget(scroll)

        bbox = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Save
            | QDialogButtonBox.StandardButton.Cancel
        )
        bbox.accepted.connect(self._save)
        bbox.rejected.connect(self.reject)

        # Export / Import buttons
        export_btn = QPushButton("Export JSON…")
        export_btn.setToolTip("Save this theme as a JSON file")
        export_btn.clicked.connect(self._export_json)
        import_btn = QPushButton("Import JSON…")
        import_btn.setToolTip("Load colours from a previously exported JSON theme file")
        import_btn.clicked.connect(self._import_json)

        btn_row = QHBoxLayout()
        btn_row.addWidget(export_btn)
        btn_row.addWidget(import_btn)
        btn_row.addStretch()

        outer.addLayout(btn_row)
        outer.addWidget(bbox)

    # ------------------------------------------------------------------
    # Export / Import
    # ------------------------------------------------------------------

    def _export_json(self) -> None:
        """Export the current colour selections to a JSON file."""
        path, _ = QFileDialog.getSaveFileName(
            self,
            "Export Theme",
            "my_theme.json",
            "JSON theme files (*.json);;All files (*)",
        )
        if not path:
            return
        data: dict = {}
        for field in _COLOR_FIELDS:
            data[field] = self._buttons[field].color() or "#ffffff"
        for field in _OPTIONAL_COLOR_FIELDS:
            val = self._buttons[field].color()
            if val:
                data[field] = val
        try:
            Path(path).write_text(json.dumps(data, indent=2), encoding="utf-8")
            QMessageBox.information(self, "Exported", f"Theme exported to:\n{path}")
        except OSError as exc:
            QMessageBox.critical(self, "Export Failed", str(exc))

    def _import_json(self) -> None:
        """Import colours from a previously exported JSON theme file."""
        path, _ = QFileDialog.getOpenFileName(
            self, "Import Theme", "", "JSON theme files (*.json);;All files (*)"
        )
        if not path:
            return
        try:
            data = json.loads(Path(path).read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as exc:
            QMessageBox.critical(
                self, "Import Failed", f"Could not read theme file:\n{exc}"
            )
            return
        # Apply colour values from file to buttons
        changed = 0
        for field in _COLOR_FIELDS + _OPTIONAL_COLOR_FIELDS:
            if field in data and data[field]:
                btn = self._buttons.get(field)
                if btn:
                    btn._color = data[field]  # pylint: disable=protected-access
                    btn._refresh()  # pylint: disable=protected-access
                    changed += 1
        QMessageBox.information(
            self, "Imported", f"Loaded {changed} colour(s) from:\n{Path(path).name}"
        )

    # ------------------------------------------------------------------
    # Save
    # ------------------------------------------------------------------

    def _save(self):
        suggested = (
            (self._base_theme.name + " (custom)") if self._base_theme else "My Theme"
        )
        name, ok = QInputDialog.getText(
            self, "Theme Name", "Save theme as:", text=suggested
        )
        if not ok or not name.strip():
            return
        name = name.strip()

        if name in self._theme_manager.themes:
            reply = QMessageBox.question(
                self,
                "Overwrite?",
                f"A theme named \u2018{name}\u2019 already exists. Overwrite?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            )
            if reply != QMessageBox.StandardButton.Yes:
                return

        from .themes import Theme  # avoid circular import at module level

        kwargs: dict = {"name": name}
        for field in _COLOR_FIELDS:
            kwargs[field] = self._buttons[field].color() or "#ffffff"
        for field in _OPTIONAL_COLOR_FIELDS:
            kwargs[field] = self._buttons[field].color()

        try:
            new_theme = Theme(**kwargs)
        except TypeError as exc:
            QMessageBox.critical(self, "Error", f"Could not create theme: {exc}")
            return

        self._theme_manager.themes[name] = new_theme
        self._saved_name = name
        self.accept()

    # ------------------------------------------------------------------

    def saved_name(self) -> str | None:
        """Return the name of the saved theme (valid after accept())."""
        return self._saved_name


# ---------------------------------------------------------------------------
# Convenience helper
# ---------------------------------------------------------------------------


def open_theme_editor(
    theme_manager: "ThemeManager",
    base_theme_name: str | None = None,
    parent: QWidget | None = None,
) -> str | None:
    """Open the :class:`ThemeEditorDialog` and return the saved theme name.

    Returns *None* if the user cancelled.
    """
    base = (
        theme_manager.themes.get(base_theme_name)
        if base_theme_name
        else theme_manager.themes.get(theme_manager.current_theme_name)
    )
    dlg = ThemeEditorDialog(theme_manager, base_theme=base, parent=parent)
    if dlg.exec() == QDialog.DialogCode.Accepted:
        return dlg.saved_name()
    return None


# ---------------------------------------------------------------------------
# Legacy shim (kept for backward compatibility)
# ---------------------------------------------------------------------------


class ThemeEditor(QWidget):
    """Legacy theme editor widget (stub).  Use :class:`ThemeEditorDialog` instead."""

    def __init__(self):
        super().__init__()
        self.setWindowTitle("Theme Editor")
        self.setGeometry(100, 100, 400, 300)
        self.current_theme = {
            "background_color": "#ffffff",
            "text_color": "#000000",
            "font": QFont("Arial", 10),
        }
        layout = QVBoxLayout(self)
        layout.addWidget(
            QLabel("Use View → Edit Current Theme… in the IDE for the full editor.")
        )

    def save_theme(self):
        pass

    def reset_to_default(self):
        pass


# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import sys

    from PySide6.QtWidgets import QApplication

    _app = QApplication(sys.argv)
    _editor = ThemeEditor()
    _editor.show()
    sys.exit(_app.exec())
