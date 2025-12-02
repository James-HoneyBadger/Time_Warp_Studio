#!/usr/bin/env python3
"""
Time Warp IDE with all new features integrated.
This demonstrates how to wire the new UI and creative components.
"""
from __future__ import annotations
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

try:
    from PySide6.QtWidgets import QApplication, QMainWindow, QTextEdit, QSplitter
    from PySide6.QtCore import Qt, QShortcut
    from PySide6.QtGui import QKeySequence
except ImportError:
    print("❌ PySide6 not installed. Install with: pip install PySide6")
    sys.exit(1)

# Import new feature modules
from Platforms.Python.time_warp.ui.error_explorer import ErrorExplorerWidget
from Platforms.Python.time_warp.ui.focus_mode import FocusModeManager
from Platforms.Python.time_warp.ui.onboarding import OnboardingManager, OnboardingDialog
from Platforms.Python.time_warp.ui.accessibility import (
    get_accessibility_theme,
    apply_accessibility_stylesheet,
)
from Platforms.Python.time_warp.graphics.turtle_gallery import TurtleGallery


class TimeWarpIDEWithFeatures(QMainWindow):
    """Time Warp IDE with all new features integrated."""

    def __init__(self):
        super().__init__()
        self.setWindowTitle("Time Warp IDE - Feature Demo")
        self.setGeometry(100, 100, 1200, 800)

        # Core components (simplified for demo)
        self.editor = QTextEdit()
        self.output = QTextEdit()
        self.output.setReadOnly(True)

        splitter = QSplitter(Qt.Horizontal)
        splitter.addWidget(self.editor)
        splitter.addWidget(self.output)
        self.setCentralWidget(splitter)

        # Feature 1: Error Explorer (Dock Widget)
        self.setup_error_explorer()

        # Feature 2: Focus Mode Manager
        self.setup_focus_mode()

        # Feature 3: Onboarding (First Run)
        self.setup_onboarding()

        # Feature 4: Accessibility Theme
        self.setup_accessibility()

        # Feature 5: Turtle Gallery
        self.turtle_gallery = TurtleGallery()

        # Setup keyboard shortcuts
        self.setup_shortcuts()

        # Status message
        self.statusBar().showMessage("✅ All features initialized")

    def setup_error_explorer(self):
        """Setup error explorer dock widget."""
        self.error_explorer = ErrorExplorerWidget(self)
        self.addDockWidget(Qt.RightDockWidgetArea, self.error_explorer)

        # Connect to navigation
        self.error_explorer.error_selected.connect(self.jump_to_line)

        # Demo: Add sample errors
        self.error_explorer.add_error(
            10, "Syntax", "Unexpected token ']'", "Check for missing '['"
        )
        self.error_explorer.add_error(
            25, "Runtime", "Division by zero", "Add check: IF X <> 0 THEN ..."
        )

    def setup_focus_mode(self):
        """Setup focus mode manager."""
        self.focus_manager = FocusModeManager(self)

        # Connect to status updates
        self.focus_manager.focus_mode_changed.connect(
            lambda enabled: self.statusBar().showMessage(
                f"Focus Mode: {'ON' if enabled else 'OFF'}"
            )
        )

    def setup_onboarding(self):
        """Setup onboarding system."""
        self.onboarding = OnboardingManager()

        # Check if we should show onboarding
        if self.onboarding.should_show_onboarding():
            dialog = OnboardingDialog(self)

            # Connect signals
            dialog.step_completed.connect(self.onboarding.mark_step_completed)
            dialog.tutorial_finished.connect(
                lambda: self.onboarding.mark_tutorial_completed(
                    dialog.should_skip_onboarding()
                )
            )

            # Show dialog (non-blocking)
            dialog.show()
        else:
            print("ℹ️ Onboarding already completed")

    def setup_accessibility(self):
        """Setup accessibility theme."""
        # Use high contrast dark theme for demo
        theme = get_accessibility_theme("high_contrast_dark")
        if theme:
            stylesheet = apply_accessibility_stylesheet(theme)
            self.setStyleSheet(stylesheet)
            self.statusBar().showMessage(f"Theme: {theme.name}")

    def setup_shortcuts(self):
        """Setup keyboard shortcuts."""
        # F11: Toggle focus mode
        QShortcut(
            QKeySequence("F11"),
            self,
            activated=self.focus_manager.toggle_focus_mode,
        )

        # F8: Next error
        QShortcut(
            QKeySequence("F8"),
            self,
            activated=self.next_error,
        )

        # Shift+F8: Previous error
        QShortcut(
            QKeySequence("Shift+F8"),
            self,
            activated=self.previous_error,
        )

        # Ctrl+Shift+T: Toggle tooltips
        QShortcut(
            QKeySequence("Ctrl+Shift+T"),
            self,
            activated=self.focus_manager.toggle_tooltips,
        )

    def jump_to_line(self, line_number: int):
        """Jump editor to specific line."""
        self.statusBar().showMessage(f"Jumping to line {line_number}")
        # In real implementation, move cursor to line_number
        cursor = self.editor.textCursor()
        cursor.movePosition(cursor.Start)
        for _ in range(line_number - 1):
            cursor.movePosition(cursor.Down)
        self.editor.setTextCursor(cursor)

    def next_error(self):
        """Navigate to next error in error explorer."""
        self.statusBar().showMessage("Next Error (F8)")
        # Would select next item in error_explorer.error_tree

    def previous_error(self):
        """Navigate to previous error in error explorer."""
        self.statusBar().showMessage("Previous Error (Shift+F8)")
        # Would select previous item in error_explorer.error_tree


def main():
    """Launch Time Warp IDE with all features."""
    app = QApplication(sys.argv)

    window = TimeWarpIDEWithFeatures()
    window.show()

    # Display keyboard shortcuts
    print("\n=== Time Warp IDE Feature Demo ===")
    print("Keyboard Shortcuts:")
    print("  F11             - Toggle Focus Mode")
    print("  F8              - Next Error")
    print("  Shift+F8        - Previous Error")
    print("  Ctrl+Shift+T    - Toggle Tooltips")
    print("\nFeatures Enabled:")
    print("  ✅ Error Explorer (right dock)")
    print("  ✅ Focus Mode (F11)")
    print("  ✅ Guided Onboarding (first run)")
    print("  ✅ Accessibility Theme (high contrast)")
    print("  ✅ Turtle Gallery (ready for recording)")
    print("  ✅ Procedural Art Toolkit (available)")
    print("  ✅ Pixel Canvas (available)")
    print("\n")

    sys.exit(app.exec())


if __name__ == "__main__":
    main()
