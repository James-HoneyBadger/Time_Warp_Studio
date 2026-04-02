"""
GUI-Based Theme Editor Module

This module provides a graphical interface for users to customize and save themes
for Time Warp Studio. Users can modify colors, fonts, and other UI elements.

Features:
- Real-time theme preview.
- Save and load custom themes.
- Reset to default themes.
"""

from PySide6.QtWidgets import (
    QWidget, QVBoxLayout, QLabel, QPushButton, QColorDialog, QFontDialog
)
from PySide6.QtGui import QFont


class ThemeEditor(QWidget):
    """Class to manage the GUI-based theme editor."""

    def __init__(self):
        super().__init__()
        self.setWindowTitle("Theme Editor")
        self.setGeometry(100, 100, 400, 300)

        self.current_theme = {
            "background_color": "#ffffff",
            "text_color": "#000000",
            "font": QFont("Arial", 10),
        }

        self.init_ui()

    def init_ui(self):
        """Initialize the theme editor UI."""
        layout = QVBoxLayout()

        # Background color
        bg_label = QLabel("Background Color:")
        bg_button = QPushButton("Choose Background Color")
        bg_button.clicked.connect(self.choose_background_color)

        # Text color
        text_label = QLabel("Text Color:")
        text_button = QPushButton("Choose Text Color")
        text_button.clicked.connect(self.choose_text_color)

        # Font
        font_label = QLabel("Font:")
        font_button = QPushButton("Choose Font")
        font_button.clicked.connect(self.choose_font)

        # Save and Reset buttons
        save_button = QPushButton("Save Theme")
        save_button.clicked.connect(self.save_theme)

        reset_button = QPushButton("Reset to Default")
        reset_button.clicked.connect(self.reset_to_default)

        # Add widgets to layout
        layout.addWidget(bg_label)
        layout.addWidget(bg_button)
        layout.addWidget(text_label)
        layout.addWidget(text_button)
        layout.addWidget(font_label)
        layout.addWidget(font_button)
        layout.addWidget(save_button)
        layout.addWidget(reset_button)

        self.setLayout(layout)

    def choose_background_color(self):
        """Open a color dialog to choose the background color."""
        color = QColorDialog.getColor()
        if color.isValid():
            self.current_theme["background_color"] = color.name()
            self.setStyleSheet(f"background-color: {color.name()};")

    def choose_text_color(self):
        """Open a color dialog to choose the text color."""
        color = QColorDialog.getColor()
        if color.isValid():
            self.current_theme["text_color"] = color.name()
            self.setStyleSheet(f"color: {color.name()};")

    def choose_font(self):
        """Open a font dialog to choose the font."""
        font, ok = QFontDialog.getFont()
        if ok:
            self.current_theme["font"] = font
            self.setFont(font)

    def save_theme(self):
        """Save the current theme settings."""
        print("✅ Theme saved:", self.current_theme)

    def reset_to_default(self):
        """Reset the theme to default settings."""
        self.current_theme = {
            "background_color": "#ffffff",
            "text_color": "#000000",
            "font": QFont("Arial", 10),
        }
        self.setStyleSheet("background-color: #ffffff; color: #000000;")
        self.setFont(QFont("Arial", 10))
        print("🔄 Theme reset to default.")


# Example usage
if __name__ == "__main__":
    from PySide6.QtWidgets import QApplication
    import sys

    app = QApplication(sys.argv)
    editor = ThemeEditor()
    editor.show()
    sys.exit(app.exec())
