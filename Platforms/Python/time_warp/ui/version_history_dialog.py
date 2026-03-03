"""Version History Dialog — lets users browse and restore autosaved versions."""

from __future__ import annotations

from typing import TYPE_CHECKING, Optional

from PySide6.QtCore import Qt
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QHBoxLayout,
    QLabel,
    QListWidget,
    QListWidgetItem,
    QMessageBox,
    QPlainTextEdit,
    QPushButton,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

if TYPE_CHECKING:
    from pathlib import Path

    from ..features.autosave_manager import AutosaveManager


class VersionHistoryDialog(QDialog):
    """Browse and restore autosaved versions of the current file."""

    def __init__(
        self,
        autosave_manager: "AutosaveManager",
        file_path: "Path",
        current_content: str,
        parent: Optional[QWidget] = None,
    ):
        super().__init__(parent)
        self.autosave_manager = autosave_manager
        self.file_path = file_path
        self.current_content = current_content
        self._selected_content: Optional[str] = None

        self.setWindowTitle(f"Version History — {file_path.name}")
        self.setMinimumSize(700, 480)
        self._build_ui()
        self._load_versions()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        layout = QVBoxLayout(self)

        header = QLabel(
            f"<b>Autosaved versions of <code>{self.file_path.name}</code></b><br>"
            "<small>Select a version to preview it, then click "
            "<em>Restore</em> to load it into the editor.</small>"
        )
        header.setTextFormat(Qt.RichText)
        layout.addWidget(header)

        splitter = QSplitter(Qt.Horizontal)

        # Left: version list
        left = QWidget()
        left_layout = QVBoxLayout(left)
        left_layout.setContentsMargins(0, 0, 0, 0)

        left_layout.addWidget(QLabel("Saved versions:"))
        self.list_widget = QListWidget()
        self.list_widget.setAlternatingRowColors(True)
        self.list_widget.currentRowChanged.connect(self._on_select)
        left_layout.addWidget(self.list_widget)

        clear_btn = QPushButton("Clear All History")
        clear_btn.clicked.connect(self._clear_history)
        left_layout.addWidget(clear_btn)

        splitter.addWidget(left)

        # Right: preview
        right = QWidget()
        right_layout = QVBoxLayout(right)
        right_layout.setContentsMargins(0, 0, 0, 0)

        right_layout.addWidget(QLabel("Preview:"))
        self.preview = QPlainTextEdit()
        self.preview.setReadOnly(True)
        self.preview.setFont(QFont("Courier New", 10))
        right_layout.addWidget(self.preview)

        splitter.addWidget(right)
        splitter.setSizes([220, 460])
        layout.addWidget(splitter)

        # Buttons
        buttons = QDialogButtonBox()
        self.restore_btn = buttons.addButton(
            "Restore Selected", QDialogButtonBox.AcceptRole
        )
        self.restore_btn.setEnabled(False)
        buttons.addButton(QDialogButtonBox.Cancel)
        buttons.accepted.connect(self._restore)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

    # ------------------------------------------------------------------
    # Data loading
    # ------------------------------------------------------------------

    def _load_versions(self):
        """Populate the version list from AutosaveManager."""
        history = self.autosave_manager.get_file_history(self.file_path)
        if not history:
            self.list_widget.addItem("No saved versions found.")
            self.list_widget.item(0).setFlags(Qt.NoItemFlags)
            return

        versions = history.list_versions()
        if not versions:
            self.list_widget.addItem("No saved versions found.")
            self.list_widget.item(0).setFlags(Qt.NoItemFlags)
            return

        for ver in reversed(versions):  # Newest first
            ts = ver.timestamp.strftime("%Y-%m-%d  %H:%M:%S")
            label = f"v{ver.version_num}  —  {ts}"
            item = QListWidgetItem(label)
            item.setData(Qt.UserRole, ver.version_num)
            self.list_widget.addItem(item)

        self.list_widget.setCurrentRow(0)

    # ------------------------------------------------------------------
    # Slots
    # ------------------------------------------------------------------

    def _on_select(self, row: int):
        """Show preview for the selected version."""
        item = self.list_widget.item(row)
        if not item:
            return
        version_num = item.data(Qt.UserRole)
        if version_num is None:
            return

        history = self.autosave_manager.get_file_history(self.file_path)
        if not history:
            return

        content = history.get_version(version_num)
        if content is None:
            self.preview.setPlainText("(Content not available)")
            self.restore_btn.setEnabled(False)
            return

        self.preview.setPlainText(content)
        self._selected_content = content
        self.restore_btn.setEnabled(True)

    def _restore(self):
        """Accept dialog — caller reads restored_content."""
        if self._selected_content is None:
            return
        reply = QMessageBox.question(
            self,
            "Restore Version",
            "Replace the current editor contents with this version?",
            QMessageBox.Yes | QMessageBox.No,
            QMessageBox.No,
        )
        if reply == QMessageBox.Yes:
            self.accept()

    def _clear_history(self):
        reply = QMessageBox.question(
            self,
            "Clear History",
            "Delete all saved versions for this file?",
            QMessageBox.Yes | QMessageBox.No,
            QMessageBox.No,
        )
        if reply != QMessageBox.Yes:
            return
        history = self.autosave_manager.get_file_history(self.file_path)
        if history:
            history.clear_history()
        self.list_widget.clear()
        self.preview.clear()
        self.restore_btn.setEnabled(False)
        self._selected_content = None

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    @property
    def restored_content(self) -> Optional[str]:
        """The content to restore into the editor, or None if cancelled."""
        return self._selected_content
