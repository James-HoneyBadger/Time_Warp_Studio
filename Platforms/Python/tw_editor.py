#!/usr/bin/env python3
"""
TW Editor - Standalone Code Editor for Time Warp Languages.
Features syntax highlighting, formatting, and basic editing capabilities.
"""

import logging
import sys
from pathlib import Path

from PySide6.QtCore import QSize
from PySide6.QtGui import QAction, QKeySequence
from PySide6.QtWidgets import (
    QApplication,
    QComboBox,
    QFileDialog,
    QLabel,
    QMainWindow,
    QMessageBox,
    QStatusBar,
    QToolBar,
    QVBoxLayout,
    QWidget,
)

from time_warp.core.interpreter import Language
from time_warp.logging_config import get_logger, setup_logging

# Import Time Warp components
from time_warp.ui.editor import CodeEditor
from time_warp.utils.code_formatter import get_formatter


class TWEditorWindow(QMainWindow):
    """Main window for TW Editor."""

    def __init__(self):
        super().__init__()

        self.setWindowTitle("TW Editor v6.0.0 - Time Warp Studio")
        self.resize(1024, 768)

        # Current file path
        self.current_file = None

        # Setup logging
        self.logger = get_logger(__name__)

        # Central Widget
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        layout = QVBoxLayout(central_widget)
        layout.setContentsMargins(0, 0, 0, 0)

        # Toolbar for quick access
        self.toolbar = QToolBar("Main Toolbar")
        self.toolbar.setIconSize(QSize(16, 16))
        self.addToolBar(self.toolbar)

        # Language Selector in Toolbar
        lang_label = QLabel("  Language: ")
        self.toolbar.addWidget(lang_label)

        self.lang_combo = QComboBox()
        for lang in Language:
            self.lang_combo.addItem(lang.name, lang)
        self.lang_combo.currentIndexChanged.connect(self.on_language_changed)
        self.toolbar.addWidget(self.lang_combo)

        self.toolbar.addSeparator()

        # Code Editor
        self.editor = CodeEditor()
        layout.addWidget(self.editor)

        # Status Bar
        self.status_bar = QStatusBar()
        self.setStatusBar(self.status_bar)
        self.status_bar.showMessage("Ready")

        # Initialize Menus and Actions
        self.create_actions()
        self.create_menus()

        # Set default language
        self.set_language(Language.BASIC)

    def create_actions(self):
        """Create actions for menus and toolbar."""
        # File Actions
        self.new_act = QAction(
            "&New",
            self,
            shortcut=QKeySequence.New,
            statusTip="Create a new file",
            triggered=self.new_file,
        )
        self.open_act = QAction(
            "&Open...",
            self,
            shortcut=QKeySequence.Open,
            statusTip="Open an existing file",
            triggered=self.open_file,
        )
        self.save_act = QAction(
            "&Save",
            self,
            shortcut=QKeySequence.Save,
            statusTip="Save the document to disk",
            triggered=self.save_file,
        )
        self.save_as_act = QAction(
            "Save &As...",
            self,
            shortcut=QKeySequence.SaveAs,
            statusTip="Save under a new name",
            triggered=self.save_file_as,
        )
        self.exit_act = QAction(
            "E&xit",
            self,
            shortcut="Ctrl+Q",
            statusTip="Exit the application",
            triggered=self.close,
        )

        # Edit Actions
        self.undo_act = QAction(
            "&Undo",
            self,
            shortcut=QKeySequence.Undo,
            statusTip="Undo the last operation",
            triggered=self.editor.undo,
        )
        self.redo_act = QAction(
            "&Redo",
            self,
            shortcut=QKeySequence.Redo,
            statusTip="Redo the last operation",
            triggered=self.editor.redo,
        )
        self.cut_act = QAction(
            "Cu&t",
            self,
            shortcut=QKeySequence.Cut,
            statusTip="Cut current selection to clipboard",
            triggered=self.editor.cut,
        )
        self.copy_act = QAction(
            "&Copy",
            self,
            shortcut=QKeySequence.Copy,
            statusTip="Copy current selection to clipboard",
            triggered=self.editor.copy,
        )
        self.paste_act = QAction(
            "&Paste",
            self,
            shortcut=QKeySequence.Paste,
            statusTip="Paste clipboard contents into selection",
            triggered=self.editor.paste,
        )

        # Tools Actions
        self.format_act = QAction(
            "&Format Code",
            self,
            shortcut="Ctrl+Shift+F",
            statusTip="Format and normalize code",
            triggered=self.format_code,
        )

        # Add to Toolbar
        self.toolbar.addAction(self.new_act)
        self.toolbar.addAction(self.open_act)
        self.toolbar.addAction(self.save_act)
        self.toolbar.addSeparator()
        self.toolbar.addAction(self.undo_act)
        self.toolbar.addAction(self.redo_act)
        self.toolbar.addSeparator()
        self.toolbar.addAction(self.format_act)

    def create_menus(self):
        """Create application menus."""
        menubar = self.menuBar()

        # File Menu
        file_menu = menubar.addMenu("&File")
        file_menu.addAction(self.new_act)
        file_menu.addAction(self.open_act)
        file_menu.addAction(self.save_act)
        file_menu.addAction(self.save_as_act)
        file_menu.addSeparator()
        file_menu.addAction(self.exit_act)

        # Edit Menu
        edit_menu = menubar.addMenu("&Edit")
        edit_menu.addAction(self.undo_act)
        edit_menu.addAction(self.redo_act)
        edit_menu.addSeparator()
        edit_menu.addAction(self.cut_act)
        edit_menu.addAction(self.copy_act)
        edit_menu.addAction(self.paste_act)

        # Tools Menu
        tools_menu = menubar.addMenu("&Tools")
        tools_menu.addAction(self.format_act)

    def new_file(self):
        """Create a new file."""
        if self.maybe_save():
            self.editor.clear()
            self.current_file = None
            self.setWindowTitle("Untitled - TW Editor v6.0.0")
            self.status_bar.showMessage("New file created")

    def open_file(self):
        """Open a file."""
        if self.maybe_save():
            file_path, _ = QFileDialog.getOpenFileName(
                self,
                "Open File",
                "",
                "Time Warp Files (*.bas *.pilot *.logo *.pas *.pl *.f *.c)"
                ";;All Files (*)",
            )
            if file_path:
                self.load_file(file_path)
            if file_path:
                self.load_file(file_path)

    def load_file(self, file_path):
        """Load content from file."""
        try:
            path = Path(file_path)
            text = path.read_text(encoding="utf-8")
            self.editor.setPlainText(text)
            self.current_file = file_path
            self.setWindowTitle(f"{path.name} - TW Editor v6.0.0")
            self.status_bar.showMessage(f"Loaded {file_path}")

            # Auto-detect language
            ext = path.suffix
            try:
                lang = Language.from_extension(ext)
                self.set_language(lang)
            except ValueError:
                pass  # Keep current language if unknown

        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not open file: {e}")

    def save_file(self):
        """Save current file."""
        if self.current_file:
            return self.save_to_path(self.current_file)
        else:
            return self.save_file_as()

    def save_file_as(self):
        """Save current file as..."""
        file_path, _ = QFileDialog.getSaveFileName(
            self,
            "Save File",
            "",
            "Time Warp Files (*.bas *.pilot *.logo *.pas *.pl *.f *.c)"
            ";;All Files (*)",
        )
        if file_path:
            return self.save_to_path(file_path)
        return False

    def save_to_path(self, file_path):
        """Save content to specific path."""
        try:
            path = Path(file_path)
            path.write_text(self.editor.toPlainText(), encoding="utf-8")
            self.current_file = file_path
            self.setWindowTitle(f"{path.name} - TW Editor v6.0.0")
            self.editor.document().setModified(False)
            self.status_bar.showMessage(f"Saved {file_path}")

            # Update language based on new extension
            try:
                lang = Language.from_extension(path.suffix)
                self.set_language(lang)
            except ValueError:
                pass

            return True
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Could not save file: {e}")
            return False

    def maybe_save(self):
        """Prompt to save if modified."""
        if not self.editor.document().isModified():
            return True

        ret = QMessageBox.warning(
            self,
            "TW Editor",
            "The document has been modified.\n"
            "Do you want to save your changes?",
            QMessageBox.Save | QMessageBox.Discard | QMessageBox.Cancel,
        )

        if ret == QMessageBox.Save:
            return self.save_file()
        elif ret == QMessageBox.Cancel:
            return False
        return True

    def on_language_changed(self, index):
        """Handle language selection change."""
        lang = self.lang_combo.itemData(index)
        if lang:
            self.editor.set_language(lang)
            self.status_bar.showMessage(f"Language set to {lang.name}")

    def set_language(self, lang):
        """Set the current language programmatically."""
        index = self.lang_combo.findData(lang)
        if index >= 0:
            self.lang_combo.setCurrentIndex(index)
        self.editor.set_language(lang)

    def format_code(self):
        """Format the current code."""
        lang = self.lang_combo.currentData()
        if not lang:
            return

        code = self.editor.toPlainText()
        formatter = get_formatter()

        # Map Language enum to formatter string
        lang_str = lang.name

        formatted_code, msg = formatter.format_and_normalize(code, lang_str)

        if "❌" not in msg:
            self.editor.setPlainText(formatted_code)

        self.status_bar.showMessage(msg)
        if "❌" in msg:
            QMessageBox.warning(self, "Format Error", msg)

    def closeEvent(self, event):
        """Handle window close event."""
        if self.maybe_save():
            event.accept()
        else:
            event.ignore()


def main():
    """Entry point for TW Editor."""
    # Setup logging
    log_file = Path.home() / ".time_warp" / "logs" / "tw_editor.log"
    setup_logging(log_level=logging.INFO, log_file=log_file)

    app = QApplication(sys.argv)
    app.setApplicationName("TW Editor")
    app.setOrganizationName("TimeWarp")

    window = TWEditorWindow()
    window.show()

    # Load file from command line if provided
    if len(sys.argv) > 1:
        file_path = sys.argv[1]
        if Path(file_path).exists():
            window.load_file(file_path)

    sys.exit(app.exec())


if __name__ == "__main__":
    main()
