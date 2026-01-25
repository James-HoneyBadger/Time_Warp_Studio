"""Theme management for Time Warp Studio."""

# PySide6 static analysis may not expose all names in some environments.
# Pylint can raise false positives like 'no-name-in-module' for these symbols.
# Disable the check for this file where Qt types are used.
# pylint: disable=no-name-in-module

from dataclasses import dataclass
from typing import Dict, List

from PySide6.QtGui import QColor, QFont, QFontDatabase, QPalette
from PySide6.QtWidgets import QApplication


@dataclass
# Theme is a plain data container with many color fields by design.
# Allow the larger instance attribute count here.
# pylint: disable=too-many-instance-attributes
class Theme:
    """Theme color scheme."""

    name: str
    background: str
    foreground: str
    editor_bg: str
    editor_fg: str
    line_number_bg: str
    line_number_fg: str
    keyword: str
    comment: str
    string: str
    number: str
    selection_bg: str
    selection_fg: str
    canvas_bg: str
    operator: str = "#d4d4d4"  # Default operator color
    function: str = "#dcdcaa"  # Default function color
    variable: str = "#9cdcfe"  # Default variable color
    # Menu and UI colors for consistency
    menu_bg: str = None  # Falls back to background if None
    menu_fg: str = None  # Falls back to foreground if None
    border_color: str = None  # Falls back to line_number_bg if None


class ThemeManager:
    """Manages IDE themes."""

    def __init__(self):
        # Default font settings
        self.current_font_family = "Monospace"
        self.current_font_size = 12
        self.available_fonts = self._get_monospace_fonts()

        self.themes: Dict[str, Theme] = {
            # === DARK THEMES ===
            "Dracula": Theme(
                name="Dracula",
                background="#282a36",
                foreground="#f8f8f2",
                editor_bg="#282a36",
                editor_fg="#f8f8f2",
                line_number_bg="#21222c",
                line_number_fg="#6272a4",
                keyword="#ff79c6",
                comment="#6272a4",
                string="#f1fa8c",
                number="#bd93f9",
                selection_bg="#44475a",
                selection_fg="#f8f8f2",
                canvas_bg="#282a36",
                operator="#ff79c6",
                function="#50fa7b",
                variable="#8be9fd",
                menu_bg="#21222c",
                menu_fg="#f8f8f2",
                border_color="#44475a",
            ),
            "Monokai": Theme(
                name="Monokai",
                background="#272822",
                foreground="#f8f8f2",
                editor_bg="#272822",
                editor_fg="#f8f8f2",
                line_number_bg="#1e1f1c",
                line_number_fg="#90908a",
                keyword="#f92672",
                comment="#75715e",
                string="#e6db74",
                number="#ae81ff",
                operator="#f92672",
                function="#a6e22e",
                variable="#66d9ef",
                selection_bg="#49483e",
                selection_fg="#f8f8f2",
                canvas_bg="#272822",
                menu_bg="#1e1f1c",
                menu_fg="#f8f8f2",
                border_color="#3e3d32",
            ),
            "VS Code Dark": Theme(
                name="VS Code Dark",
                background="#1e1e1e",
                foreground="#d4d4d4",
                editor_bg="#1e1e1e",
                editor_fg="#d4d4d4",
                line_number_bg="#1e1e1e",
                line_number_fg="#858585",
                keyword="#569cd6",
                comment="#6a9955",
                string="#ce9178",
                number="#b5cea8",
                operator="#d4d4d4",
                function="#dcdcaa",
                variable="#9cdcfe",
                selection_bg="#264f78",
                selection_fg="#ffffff",
                canvas_bg="#1e1e1e",
                menu_bg="#252526",
                menu_fg="#cccccc",
                border_color="#3c3c3c",
            ),
            "GitHub Dark": Theme(
                name="GitHub Dark",
                background="#0d1117",
                foreground="#e6edf3",
                editor_bg="#0d1117",
                editor_fg="#e6edf3",
                line_number_bg="#0d1117",
                line_number_fg="#7d8590",
                keyword="#ff7b72",
                comment="#8b949e",
                string="#a5d6ff",
                number="#79c0ff",
                operator="#ff7b72",
                function="#d2a8ff",
                variable="#79c0ff",
                selection_bg="#264f78",
                selection_fg="#ffffff",
                canvas_bg="#0d1117",
                menu_bg="#161b22",
                menu_fg="#e6edf3",
                border_color="#30363d",
            ),
            "Nord": Theme(
                name="Nord",
                background="#2e3440",
                foreground="#eceff4",
                editor_bg="#2e3440",
                editor_fg="#eceff4",
                line_number_bg="#2e3440",
                line_number_fg="#4c566a",
                keyword="#81a1c1",
                comment="#616e88",
                string="#a3be8c",
                number="#b48ead",
                operator="#81a1c1",
                function="#88c0d0",
                variable="#d8dee9",
                selection_bg="#434c5e",
                selection_fg="#eceff4",
                canvas_bg="#2e3440",
                menu_bg="#3b4252",
                menu_fg="#eceff4",
                border_color="#4c566a",
            ),
            "One Dark Pro": Theme(
                name="One Dark Pro",
                background="#282c34",
                foreground="#abb2bf",
                editor_bg="#282c34",
                editor_fg="#abb2bf",
                line_number_bg="#282c34",
                line_number_fg="#4b5263",
                keyword="#c678dd",
                comment="#5c6370",
                string="#98c379",
                number="#d19a66",
                operator="#56b6c2",
                function="#61afef",
                variable="#e5c07b",
                selection_bg="#3e4451",
                selection_fg="#abb2bf",
                canvas_bg="#282c34",
                menu_bg="#21252b",
                menu_fg="#abb2bf",
                border_color="#3e4451",
            ),
            "Solarized Dark": Theme(
                name="Solarized Dark",
                background="#002b36",
                foreground="#93a1a1",
                editor_bg="#002b36",
                editor_fg="#93a1a1",
                line_number_bg="#002b36",
                line_number_fg="#586e75",
                keyword="#268bd2",
                comment="#586e75",
                string="#2aa198",
                number="#d33682",
                operator="#859900",
                function="#b58900",
                variable="#cb4b16",
                selection_bg="#073642",
                selection_fg="#93a1a1",
                canvas_bg="#002b36",
                menu_bg="#073642",
                menu_fg="#93a1a1",
                border_color="#073642",
            ),
            "Ocean": Theme(
                name="Ocean",
                background="#1b2b34",
                foreground="#d8dee9",
                editor_bg="#1b2b34",
                editor_fg="#d8dee9",
                line_number_bg="#1b2b34",
                line_number_fg="#65737e",
                keyword="#6699cc",
                comment="#65737e",
                string="#99c794",
                number="#f99157",
                operator="#5fb3b3",
                function="#c594c5",
                variable="#ec5f67",
                selection_bg="#4f5b66",
                selection_fg="#d8dee9",
                canvas_bg="#1b2b34",
                menu_bg="#343d46",
                menu_fg="#d8dee9",
                border_color="#4f5b66",
            ),
            # === LIGHT THEMES ===
            "VS Code Light": Theme(
                name="VS Code Light",
                background="#ffffff",
                foreground="#000000",
                editor_bg="#ffffff",
                editor_fg="#000000",
                line_number_bg="#f3f3f3",
                line_number_fg="#237893",
                keyword="#0000ff",
                comment="#008000",
                string="#a31515",
                number="#098658",
                operator="#000000",
                function="#795e26",
                variable="#001080",
                selection_bg="#add6ff",
                selection_fg="#000000",
                canvas_bg="#ffffff",
                menu_bg="#f3f3f3",
                menu_fg="#000000",
                border_color="#c8c8c8",
            ),
            "GitHub Light": Theme(
                name="GitHub Light",
                background="#ffffff",
                foreground="#24292f",
                editor_bg="#ffffff",
                editor_fg="#24292f",
                line_number_bg="#f6f8fa",
                line_number_fg="#57606a",
                keyword="#cf222e",
                comment="#6e7781",
                string="#0a3069",
                number="#0550ae",
                operator="#cf222e",
                function="#8250df",
                variable="#953800",
                selection_bg="#ddf4ff",
                selection_fg="#24292f",
                canvas_bg="#ffffff",
                menu_bg="#f6f8fa",
                menu_fg="#24292f",
                border_color="#d0d7de",
            ),
            "Solarized Light": Theme(
                name="Solarized Light",
                background="#fdf6e3",
                foreground="#657b83",
                editor_bg="#fdf6e3",
                editor_fg="#657b83",
                line_number_bg="#eee8d5",
                line_number_fg="#93a1a1",
                keyword="#268bd2",
                comment="#93a1a1",
                string="#2aa198",
                number="#d33682",
                operator="#859900",
                function="#b58900",
                variable="#cb4b16",
                selection_bg="#eee8d5",
                selection_fg="#657b83",
                canvas_bg="#fdf6e3",
                menu_bg="#eee8d5",
                menu_fg="#657b83",
                border_color="#93a1a1",
            ),
            "Spring": Theme(
                name="Spring",
                background="#f8f9fa",
                foreground="#212529",
                editor_bg="#ffffff",
                editor_fg="#212529",
                line_number_bg="#f1f3f5",
                line_number_fg="#868e96",
                keyword="#1971c2",
                comment="#5c6b77",
                string="#c92a2a",
                number="#5c940d",
                operator="#1971c2",
                function="#7048e8",
                variable="#d9480f",
                selection_bg="#d0ebff",
                selection_fg="#212529",
                canvas_bg="#ffffff",
                menu_bg="#f1f3f5",
                menu_fg="#212529",
                border_color="#dee2e6",
            ),
            # === SPECIALTY THEMES ===
            "High Contrast Dark": Theme(
                name="High Contrast Dark",
                background="#000000",
                foreground="#ffffff",
                editor_bg="#000000",
                editor_fg="#ffffff",
                line_number_bg="#000000",
                line_number_fg="#6fc3df",
                keyword="#569cd6",
                comment="#7ca668",
                string="#d69d85",
                number="#b5cea8",
                operator="#d4d4d4",
                function="#dcdcaa",
                variable="#9cdcfe",
                selection_bg="#264f78",
                selection_fg="#ffffff",
                canvas_bg="#000000",
                menu_bg="#000000",
                menu_fg="#ffffff",
                border_color="#6fc3df",
            ),
            "High Contrast Light": Theme(
                name="High Contrast Light",
                background="#ffffff",
                foreground="#000000",
                editor_bg="#ffffff",
                editor_fg="#000000",
                line_number_bg="#ffffff",
                line_number_fg="#005a9e",
                keyword="#0000ff",
                comment="#008000",
                string="#a31515",
                number="#098658",
                operator="#000000",
                function="#795e26",
                variable="#001080",
                selection_bg="#0078d7",
                selection_fg="#ffffff",
                canvas_bg="#ffffff",
                menu_bg="#ffffff",
                menu_fg="#000000",
                border_color="#0078d7",
            ),
            # === RETRO CRT THEMES ===
            "Amber Monochrome": Theme(
                name="Amber Monochrome",
                background="#1a1400",
                foreground="#ffb000",
                editor_bg="#1a1400",
                editor_fg="#ffb000",
                line_number_bg="#0d0a00",
                line_number_fg="#996600",
                keyword="#ffc000",
                comment="#8b6914",
                string="#ffcc00",
                number="#ffa500",
                operator="#ffb000",
                function="#ffd700",
                variable="#ffb000",
                selection_bg="#4d3800",
                selection_fg="#ffb000",
                canvas_bg="#1a1400",
                menu_bg="#0d0a00",
                menu_fg="#ffb000",
                border_color="#4d3800",
            ),
            "Green Monochrome": Theme(
                name="Green Monochrome",
                background="#001400",
                foreground="#00ff00",
                editor_bg="#001400",
                editor_fg="#00ff00",
                line_number_bg="#000a00",
                line_number_fg="#009900",
                keyword="#00ff00",
                comment="#006600",
                string="#33ff33",
                number="#00cc00",
                operator="#00ff00",
                function="#66ff66",
                variable="#00ff00",
                selection_bg="#003300",
                selection_fg="#00ff00",
                canvas_bg="#001400",
                menu_bg="#000a00",
                menu_fg="#00ff00",
                border_color="#003300",
            ),
            "IBM PC CGA": Theme(
                name="IBM PC CGA",
                background="#000000",
                foreground="#aaaaaa",
                editor_bg="#000000",
                editor_fg="#aaaaaa",
                line_number_bg="#000000",
                line_number_fg="#555555",
                keyword="#5555ff",
                comment="#555555",
                string="#55ffff",
                number="#ff5555",
                operator="#ffffff",
                function="#55ff55",
                variable="#ffff55",
                selection_bg="#0000aa",
                selection_fg="#ffffff",
                canvas_bg="#000000",
                menu_bg="#0000aa",
                menu_fg="#ffffff",
                border_color="#555555",
            ),
            "Commodore 64": Theme(
                name="Commodore 64",
                background="#4040e0",
                foreground="#a0a0ff",
                editor_bg="#4040e0",
                editor_fg="#a0a0ff",
                line_number_bg="#3030b0",
                line_number_fg="#8080cc",
                keyword="#ffffff",
                comment="#6060aa",
                string="#ffff00",
                number="#00ff00",
                operator="#ffffff",
                function="#00ffff",
                variable="#a0a0ff",
                selection_bg="#6060ff",
                selection_fg="#ffffff",
                canvas_bg="#4040e0",
                menu_bg="#3030b0",
                menu_fg="#a0a0ff",
                border_color="#6060ff",
            ),
            "Apple II": Theme(
                name="Apple II",
                background="#000000",
                foreground="#00ff00",
                editor_bg="#000000",
                editor_fg="#00ff00",
                line_number_bg="#000000",
                line_number_fg="#008800",
                keyword="#00ff00",
                comment="#006600",
                string="#ffffff",
                number="#00ff00",
                operator="#00ff00",
                function="#00ff00",
                variable="#00ff00",
                selection_bg="#00aa00",
                selection_fg="#000000",
                canvas_bg="#000000",
                menu_bg="#000000",
                menu_fg="#00ff00",
                border_color="#00aa00",
            ),
            "DOS Blue": Theme(
                name="DOS Blue",
                background="#0000aa",
                foreground="#ffff55",
                editor_bg="#0000aa",
                editor_fg="#ffff55",
                line_number_bg="#000080",
                line_number_fg="#aaaaaa",
                keyword="#ffffff",
                comment="#00aaaa",
                string="#55ff55",
                number="#ff5555",
                operator="#ffff55",
                function="#55ffff",
                variable="#ffff55",
                selection_bg="#00aa00",
                selection_fg="#ffffff",
                canvas_bg="#0000aa",
                menu_bg="#000080",
                menu_fg="#ffff55",
                border_color="#555555",
            ),
            "ZX Spectrum": Theme(
                name="ZX Spectrum",
                background="#d0d0d0",
                foreground="#000000",
                editor_bg="#d0d0d0",
                editor_fg="#000000",
                line_number_bg="#c0c0c0",
                line_number_fg="#404040",
                keyword="#0000cd",
                comment="#606060",
                string="#cd0000",
                number="#00cd00",
                operator="#000000",
                function="#cdcd00",
                variable="#000000",
                selection_bg="#0000cd",
                selection_fg="#ffffff",
                canvas_bg="#d0d0d0",
                menu_bg="#c0c0c0",
                menu_fg="#000000",
                border_color="#808080",
            ),
        }

        self.current_theme_name = "Dracula"

    def _get_monospace_fonts(self) -> List[str]:
        """Get list of available monospace fonts."""
        font_db = QFontDatabase()
        monospace_fonts = []

        # Preferred programming fonts (check if available)
        preferred = [
            "JetBrains Mono",
            "Fira Code",
            "Source Code Pro",
            "Cascadia Code",
            "Consolas",
            "Monaco",
            "Menlo",
            "Ubuntu Mono",
            "DejaVu Sans Mono",
            "Liberation Mono",
            "Courier New",
            "Monospace",
        ]

        all_families = font_db.families()

        # Add preferred fonts that are available
        for font in preferred:
            if font in all_families:
                monospace_fonts.append(font)

        # Add other monospace fonts
        for family in all_families:
            if font_db.isFixedPitch(family) and family not in monospace_fonts:
                monospace_fonts.append(family)

        # Ensure we have at least one font
        if not monospace_fonts:
            monospace_fonts = ["Monospace"]

        return monospace_fonts

    def get_available_fonts(self) -> List[str]:
        """Get list of available monospace fonts."""
        return self.available_fonts

    def get_font_sizes(self) -> List[int]:
        """Get list of available font sizes."""
        return [8, 9, 10, 11, 12, 13, 14, 16, 18, 20, 22, 24, 28, 32]

    def set_font(self, family: str, size: int):
        """Set the current font family and size."""
        self.current_font_family = family
        self.current_font_size = size

    def get_font(self) -> QFont:
        """Get the current font."""
        font = QFont(self.current_font_family, self.current_font_size)
        font.setFixedPitch(True)
        return font

    def get_theme_names(self):
        """Get list of available theme names."""
        return list(self.themes.keys())

    def get_theme(self, name: str) -> Theme:
        """Get theme by name."""
        return self.themes.get(name, self.themes["Dracula"])

    def apply_theme(
        self,
        name: str,
        editor=None,
        output=None,
        canvas=None,
        highlighter=None,
    ):  # pylint: disable=too-many-arguments,too-many-positional-arguments
        """Apply theme to application and widgets."""
        if name not in self.themes:
            name = "Dracula"

        theme = self.themes[name]
        self.current_theme_name = name

        # Get colors with fallbacks
        menu_bg = theme.menu_bg or theme.background
        menu_fg = theme.menu_fg or theme.foreground
        border_color = theme.border_color or theme.line_number_bg

        # Apply to application palette
        app = QApplication.instance()
        palette = QPalette()

        palette.setColor(QPalette.Window, QColor(theme.background))
        palette.setColor(QPalette.WindowText, QColor(theme.foreground))
        palette.setColor(QPalette.Base, QColor(theme.editor_bg))
        palette.setColor(QPalette.AlternateBase, QColor(theme.line_number_bg))
        palette.setColor(QPalette.Text, QColor(theme.editor_fg))
        palette.setColor(QPalette.Button, QColor(menu_bg))
        palette.setColor(QPalette.ButtonText, QColor(menu_fg))
        palette.setColor(QPalette.Highlight, QColor(theme.selection_bg))
        palette.setColor(QPalette.HighlightedText, QColor(theme.selection_fg))
        palette.setColor(QPalette.ToolTipBase, QColor(menu_bg))
        palette.setColor(QPalette.ToolTipText, QColor(menu_fg))
        palette.setColor(QPalette.Link, QColor(theme.keyword))
        palette.setColor(QPalette.LinkVisited, QColor(theme.function))

        app.setPalette(palette)

        # Apply comprehensive stylesheet for consistent UI
        app.setStyleSheet(f"""
            QMainWindow {{
                background-color: {theme.background};
            }}
            QMenuBar {{
                background-color: {menu_bg};
                color: {menu_fg};
                border-bottom: 1px solid {border_color};
                padding: 2px;
            }}
            QMenuBar::item {{
                background-color: transparent;
                padding: 4px 8px;
                border-radius: 4px;
            }}
            QMenuBar::item:selected {{
                background-color: {theme.selection_bg};
                color: {theme.selection_fg};
            }}
            QMenu {{
                background-color: {menu_bg};
                color: {menu_fg};
                border: 1px solid {border_color};
                border-radius: 4px;
                padding: 4px;
            }}
            QMenu::item {{
                padding: 6px 24px 6px 12px;
                border-radius: 3px;
            }}
            QMenu::item:selected {{
                background-color: {theme.selection_bg};
                color: {theme.selection_fg};
            }}
            QMenu::separator {{
                height: 1px;
                background-color: {border_color};
                margin: 4px 8px;
            }}
            QToolBar {{
                background-color: {menu_bg};
                border-bottom: 1px solid {border_color};
                padding: 2px;
                spacing: 4px;
            }}
            QToolButton {{
                background-color: transparent;
                color: {menu_fg};
                border: 1px solid transparent;
                border-radius: 4px;
                padding: 4px 8px;
            }}
            QToolButton:hover {{
                background-color: {theme.selection_bg};
                color: {theme.selection_fg};
            }}
            QToolButton:pressed {{
                background-color: {border_color};
            }}
            QStatusBar {{
                background-color: {menu_bg};
                color: {menu_fg};
                border-top: 1px solid {border_color};
            }}
            QTabWidget::pane {{
                border: 1px solid {border_color};
                background-color: {theme.editor_bg};
            }}
            QTabBar::tab {{
                background-color: {menu_bg};
                color: {menu_fg};
                border: 1px solid {border_color};
                border-bottom: none;
                padding: 6px 12px;
                margin-right: 2px;
                border-top-left-radius: 4px;
                border-top-right-radius: 4px;
            }}
            QTabBar::tab:selected {{
                background-color: {theme.editor_bg};
                color: {theme.editor_fg};
            }}
            QTabBar::tab:hover:!selected {{
                background-color: {theme.selection_bg};
            }}
            QScrollBar:vertical {{
                background-color: {theme.background};
                width: 12px;
                border: none;
            }}
            QScrollBar::handle:vertical {{
                background-color: {border_color};
                border-radius: 6px;
                min-height: 20px;
                margin: 2px;
            }}
            QScrollBar::handle:vertical:hover {{
                background-color: {theme.line_number_fg};
            }}
            QScrollBar:horizontal {{
                background-color: {theme.background};
                height: 12px;
                border: none;
            }}
            QScrollBar::handle:horizontal {{
                background-color: {border_color};
                border-radius: 6px;
                min-width: 20px;
                margin: 2px;
            }}
            QScrollBar::add-line, QScrollBar::sub-line {{
                width: 0px;
                height: 0px;
            }}
            QSplitter::handle {{
                background-color: {border_color};
            }}
            QComboBox {{
                background-color: {menu_bg};
                color: {menu_fg};
                border: 1px solid {border_color};
                border-radius: 4px;
                padding: 4px 8px;
            }}
            QComboBox:hover {{
                border-color: {theme.keyword};
            }}
            QComboBox::drop-down {{
                border: none;
                padding-right: 8px;
            }}
            QComboBox QAbstractItemView {{
                background-color: {menu_bg};
                color: {menu_fg};
                border: 1px solid {border_color};
                selection-background-color: {theme.selection_bg};
                selection-color: {theme.selection_fg};
            }}
            QLineEdit {{
                background-color: {theme.editor_bg};
                color: {theme.editor_fg};
                border: 1px solid {border_color};
                border-radius: 4px;
                padding: 4px 8px;
            }}
            QLineEdit:focus {{
                border-color: {theme.keyword};
            }}
            QPushButton {{
                background-color: {menu_bg};
                color: {menu_fg};
                border: 1px solid {border_color};
                border-radius: 4px;
                padding: 6px 16px;
            }}
            QPushButton:hover {{
                background-color: {theme.selection_bg};
                color: {theme.selection_fg};
            }}
            QPushButton:pressed {{
                background-color: {border_color};
            }}
            QListWidget {{
                background-color: {theme.editor_bg};
                color: {theme.editor_fg};
                border: 1px solid {border_color};
                border-radius: 4px;
            }}
            QListWidget::item:selected {{
                background-color: {theme.selection_bg};
                color: {theme.selection_fg};
            }}
            QTreeWidget {{
                background-color: {theme.editor_bg};
                color: {theme.editor_fg};
                border: 1px solid {border_color};
            }}
            QTreeWidget::item:selected {{
                background-color: {theme.selection_bg};
                color: {theme.selection_fg};
            }}
            QMessageBox {{
                background-color: {theme.background};
                color: {theme.foreground};
            }}
            QDialog {{
                background-color: {theme.background};
                color: {theme.foreground};
            }}
            QLabel {{
                color: {theme.foreground};
            }}
            QGroupBox {{
                color: {theme.foreground};
                border: 1px solid {border_color};
                border-radius: 4px;
                margin-top: 8px;
                padding-top: 8px;
            }}
            QGroupBox::title {{
                subcontrol-origin: margin;
                subcontrol-position: top left;
                padding: 0 4px;
            }}
        """)

        # Get current font
        font = self.get_font()

        # Apply to editor
        if editor:
            editor.setFont(font)
            editor.setStyleSheet(f"""
                QPlainTextEdit {{
                    background-color: {theme.editor_bg};
                    color: {theme.editor_fg};
                    selection-background-color: {theme.selection_bg};
                    selection-color: {theme.selection_fg};
                    border: none;
                }}
            """)

        # Apply to output
        if output:
            output.setFont(font)
            output.setStyleSheet(f"""
                QTextEdit {{
                    background-color: {theme.editor_bg};
                    color: {theme.editor_fg};
                    selection-background-color: {theme.selection_bg};
                    selection-color: {theme.selection_fg};
                    border: none;
                }}
            """)

        # Apply to canvas
        if canvas:
            canvas.bg_color = QColor(theme.canvas_bg)
            canvas.update()

        # Apply to syntax highlighter
        if highlighter:
            highlighter.keyword_format.setForeground(QColor(theme.keyword))
            highlighter.comment_format.setForeground(QColor(theme.comment))
            highlighter.string_format.setForeground(QColor(theme.string))
            highlighter.number_format.setForeground(QColor(theme.number))
            highlighter.operator_format.setForeground(QColor(theme.operator))
            highlighter.function_format.setForeground(QColor(theme.function))
            highlighter.variable_format.setForeground(QColor(theme.variable))
            highlighter.rehighlight()
