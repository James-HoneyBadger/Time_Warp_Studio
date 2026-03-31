"""
Accessibility Features Module

This module provides accessibility enhancements for the Time Warp Studio UI.
Features include high-contrast mode and font size adjustments.
"""

from PySide6.QtGui import QFont
from PySide6.QtWidgets import QShortcut, QKeySequence, QMainWindow

class AccessibilityFeatures:
    """Class to manage accessibility features."""

    def __init__(self, main_window: QMainWindow):
        self.main_window = main_window
        self._apply_accessibility_features()

    def _apply_accessibility_features(self):
        """Apply accessibility features to the main window."""
        # High-contrast mode toggle
        self.high_contrast_shortcut = QShortcut(QKeySequence("Ctrl+H"), self.main_window)
        self.high_contrast_shortcut.activated.connect(self._toggle_high_contrast)

        # Increase font size
        self.increase_font_shortcut = QShortcut(QKeySequence("Ctrl++"), self.main_window)
        self.increase_font_shortcut.activated.connect(self._increase_font_size)

        # Decrease font size
        self.decrease_font_shortcut = QShortcut(QKeySequence("Ctrl+-"), self.main_window)
        self.decrease_font_shortcut.activated.connect(self._decrease_font_size)

    def _toggle_high_contrast(self):
        """Toggle high-contrast mode."""
        # Example: Apply a high-contrast stylesheet
        self.main_window.setStyleSheet("background-color: black; color: white;")
        print("🎨 High-contrast mode toggled.")

    def _increase_font_size(self):
        """Increase the font size of the application."""
        font = self.main_window.font()
        font.setPointSize(font.pointSize() + 1)
        self.main_window.setFont(font)
        print("🔍 Font size increased.")

    def _decrease_font_size(self):
        """Decrease the font size of the application."""
        font = self.main_window.font()
        font.setPointSize(max(font.pointSize() - 1, 8))  # Minimum font size of 8
        self.main_window.setFont(font)
        print("🔍 Font size decreased.")

# Example usage
if __name__ == "__main__":
    from PySide6.QtWidgets import QApplication, QMainWindow
    import sys

    app = QApplication(sys.argv)
    main_window = QMainWindow()
    main_window.setWindowTitle("Accessibility Features Test")
    accessibility = AccessibilityFeatures(main_window)
    main_window.show()
    sys.exit(app.exec())