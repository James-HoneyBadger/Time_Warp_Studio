"""
Accessibility themes with high-contrast modes and dyslexia-friendly fonts.
"""

from __future__ import annotations
from dataclasses import dataclass


@dataclass
class AccessibilityTheme:  # pylint: disable=too-many-instance-attributes
    """Accessibility-focused theme configuration."""

    name: str
    background: str
    foreground: str
    selection_bg: str
    selection_fg: str
    keyword_color: str
    comment_color: str
    string_color: str
    number_color: str
    error_color: str
    warning_color: str
    success_color: str
    font_family: str
    font_size: int
    line_spacing: float
    contrast_level: str  # "standard", "high", "maximum"


# High Contrast themes
HIGH_CONTRAST_DARK = AccessibilityTheme(
    name="High Contrast Dark",
    background="#000000",
    foreground="#FFFFFF",
    selection_bg="#FFFF00",
    selection_fg="#000000",
    keyword_color="#00FFFF",
    comment_color="#00FF00",
    string_color="#FF00FF",
    number_color="#FFFF00",
    error_color="#FF0000",
    warning_color="#FFAA00",
    success_color="#00FF00",
    font_family="Consolas",
    font_size=12,
    line_spacing=1.5,
    contrast_level="maximum",
)

HIGH_CONTRAST_LIGHT = AccessibilityTheme(
    name="High Contrast Light",
    background="#FFFFFF",
    foreground="#000000",
    selection_bg="#0000FF",
    selection_fg="#FFFFFF",
    keyword_color="#0000FF",
    comment_color="#008000",
    string_color="#A31515",
    number_color="#09885A",
    error_color="#FF0000",
    warning_color="#FF6600",
    success_color="#008000",
    font_family="Consolas",
    font_size=12,
    line_spacing=1.5,
    contrast_level="maximum",
)

# Dyslexia-friendly themes
DYSLEXIA_FRIENDLY = AccessibilityTheme(
    name="Dyslexia Friendly",
    background="#FDF6E3",  # Cream background
    foreground="#000000",
    selection_bg="#B58900",
    selection_fg="#FDF6E3",
    keyword_color="#268BD2",
    comment_color="#93A1A1",
    string_color="#2AA198",
    number_color="#D33682",
    error_color="#DC322F",
    warning_color="#CB4B16",
    success_color="#859900",
    font_family="OpenDyslexic",  # Fallback to Arial
    font_size=14,
    line_spacing=1.8,
    contrast_level="high",
)

ACCESSIBLE_DARK_BLUE = AccessibilityTheme(
    name="Accessible Dark Blue",
    background="#002240",
    foreground="#E0E0E0",
    selection_bg="#005A8C",
    selection_fg="#FFFFFF",
    keyword_color="#00D9FF",
    comment_color="#7CB8BB",
    string_color="#FFB454",
    number_color="#B4FA72",
    error_color="#FF6B6B",
    warning_color="#FFD93D",
    success_color="#6BCF7F",
    font_family="Verdana",
    font_size=13,
    line_spacing=1.6,
    contrast_level="high",
)


ACCESSIBILITY_THEMES = {
    "high_contrast_dark": HIGH_CONTRAST_DARK,
    "high_contrast_light": HIGH_CONTRAST_LIGHT,
    "dyslexia_friendly": DYSLEXIA_FRIENDLY,
    "accessible_dark_blue": ACCESSIBLE_DARK_BLUE,
}


def get_accessibility_theme(name: str) -> AccessibilityTheme | None:
    """Get accessibility theme by name."""
    return ACCESSIBILITY_THEMES.get(name)


def apply_accessibility_stylesheet(theme: AccessibilityTheme) -> str:
    """Generate Qt stylesheet for accessibility theme."""
    return f"""
    QWidget {{
        background-color: {theme.background};
        color: {theme.foreground};
        font-family: {theme.font_family};
        font-size: {theme.font_size}pt;
    }}

    QTextEdit, QPlainTextEdit {{
        background-color: {theme.background};
        color: {theme.foreground};
        selection-background-color: {theme.selection_bg};
        selection-color: {theme.selection_fg};
        line-height: {int(theme.line_spacing * 100)}%;
    }}

    QPushButton {{
        background-color: {theme.keyword_color};
        color: {theme.background};
        border: 2px solid {theme.foreground};
        padding: 8px 16px;
        font-size: {theme.font_size}pt;
        min-height: 36px;
    }}

    QPushButton:hover {{
        background-color: {theme.selection_bg};
        border-color: {theme.selection_fg};
    }}

    QPushButton:pressed {{
        background-color: {theme.foreground};
        color: {theme.background};
    }}

    QMenuBar {{
        background-color: {theme.background};
        color: {theme.foreground};
        border-bottom: 2px solid {theme.foreground};
    }}

    QMenuBar::item:selected {{
        background-color: {theme.selection_bg};
        color: {theme.selection_fg};
    }}

    QMenu {{
        background-color: {theme.background};
        color: {theme.foreground};
        border: 2px solid {theme.foreground};
    }}

    QMenu::item:selected {{
        background-color: {theme.selection_bg};
        color: {theme.selection_fg};
    }}

    QLabel {{
        color: {theme.foreground};
    }}

    QLineEdit {{
        background-color: {theme.background};
        color: {theme.foreground};
        border: 2px solid {theme.foreground};
        padding: 4px;
        selection-background-color: {theme.selection_bg};
        selection-color: {theme.selection_fg};
    }}

    QTreeWidget {{
        background-color: {theme.background};
        color: {theme.foreground};
        alternate-background-color: {theme.comment_color};
        border: 2px solid {theme.foreground};
    }}

    QTreeWidget::item:selected {{
        background-color: {theme.selection_bg};
        color: {theme.selection_fg};
    }}
    """


# Keyboard navigation helpers
KEYBOARD_SHORTCUTS = {
    "jump_to_line": "Ctrl+G",
    "next_error": "F8",
    "previous_error": "Shift+F8",
    "toggle_focus_mode": "F11",
    "increase_font": "Ctrl++",
    "decrease_font": "Ctrl+-",
    "toggle_high_contrast": "Alt+H",
    "run_program": "F5",
    "stop_program": "Shift+F5",
}


def get_keyboard_shortcuts_help() -> str:
    """Generate keyboard shortcuts help text."""
    lines = ["=== Keyboard Shortcuts ===\n"]
    for action, shortcut in KEYBOARD_SHORTCUTS.items():
        readable_name = action.replace("_", " ").title()
        lines.append(f"{readable_name:25s} {shortcut}")
    return "\n".join(lines)
