"""Theme management for Time Warp IDE."""

# PySide6 static analysis may not expose all names in some environments.
# Pylint can raise false positives like 'no-name-in-module' for these symbols.
# Disable the check for this file where Qt types are used.
# pylint: disable=no-name-in-module

from dataclasses import dataclass
from typing import Dict

from PySide6.QtGui import QColor, QPalette
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


class ThemeManager:
    """Manages IDE themes."""

    def __init__(self):
        self.themes: Dict[str, Theme] = {
            "Dracula": Theme(
                name="Dracula",
                background="#282a36",
                foreground="#f8f8f2",
                editor_bg="#282a36",
                editor_fg="#f8f8f2",
                line_number_bg="#1e1f29",
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
            ),
            "Monokai": Theme(
                name="Monokai",
                background="#272822",
                foreground="#f8f8f2",
                editor_bg="#272822",
                editor_fg="#f8f8f2",
                line_number_bg="#1e1f1c",
                line_number_fg="#75715e",
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
                operator="#268bd2",
                function="#b58900",
                variable="#268bd2",
                selection_bg="#eee8d5",
                selection_fg="#657b83",
                canvas_bg="#fdf6e3",
            ),
            "Solarized Dark": Theme(
                name="Solarized Dark",
                background="#002b36",
                foreground="#839496",
                editor_bg="#002b36",
                editor_fg="#839496",
                line_number_bg="#073642",
                line_number_fg="#586e75",
                keyword="#268bd2",
                comment="#586e75",
                string="#2aa198",
                number="#d33682",
                operator="#268bd2",
                function="#b58900",
                variable="#268bd2",
                selection_bg="#073642",
                selection_fg="#839496",
                canvas_bg="#002b36",
            ),
            "Ocean": Theme(
                name="Ocean",
                background="#1b2b34",
                foreground="#c0c5ce",
                editor_bg="#1b2b34",
                editor_fg="#c0c5ce",
                line_number_bg="#343d46",
                line_number_fg="#65737e",
                keyword="#6699cc",
                comment="#65737e",
                string="#99c794",
                number="#f99157",
                operator="#6699cc",
                function="#c594c5",
                variable="#6699cc",
                selection_bg="#4f5b66",
                selection_fg="#c0c5ce",
                canvas_bg="#1b2b34",
            ),
            "Spring": Theme(
                name="Spring",
                background="#f5f5f5",
                foreground="#333333",
                editor_bg="#ffffff",
                editor_fg="#333333",
                line_number_bg="#e8e8e8",
                line_number_fg="#888888",
                keyword="#0066cc",
                comment="#008000",
                string="#dd1144",
                number="#009999",
                operator="#0066cc",
                function="#990099",
                variable="#0066cc",
                selection_bg="#d3e8f8",
                selection_fg="#333333",
                canvas_bg="#ffffff",
            ),
            "Sunset": Theme(
                name="Sunset",
                background="#2d1b00",
                foreground="#ffd699",
                editor_bg="#2d1b00",
                editor_fg="#ffd699",
                line_number_bg="#1a0f00",
                line_number_fg="#b37700",
                keyword="#ff9933",
                comment="#b37700",
                string="#ffcc66",
                number="#ff6600",
                operator="#ff9933",
                function="#ffcc66",
                variable="#ff9933",
                selection_bg="#4d3300",
                selection_fg="#ffd699",
                canvas_bg="#2d1b00",
            ),
            "Candy": Theme(
                name="Candy",
                background="#1a0033",
                foreground="#ffccff",
                editor_bg="#1a0033",
                editor_fg="#ffccff",
                line_number_bg="#0d001a",
                line_number_fg="#b366ff",
                keyword="#ff66ff",
                comment="#9933cc",
                string="#ffb3ff",
                number="#cc66ff",
                operator="#ff66ff",
                function="#ffb3ff",
                variable="#ff66ff",
                selection_bg="#330066",
                selection_fg="#ffccff",
                canvas_bg="#1a0033",
            ),
            "GitHub Dark": Theme(
                name="GitHub Dark",
                background="#0d1117",
                foreground="#c9d1d9",
                editor_bg="#0d1117",
                editor_fg="#c9d1d9",
                line_number_bg="#161b22",
                line_number_fg="#7d8590",
                keyword="#ff7b72",
                comment="#7d8590",
                string="#a5c9ea",
                number="#79c0ff",
                operator="#ff7b72",
                function="#d2a8ff",
                variable="#79c0ff",
                selection_bg="#264f78",
                selection_fg="#c9d1d9",
                canvas_bg="#0d1117",
            ),
            "GitHub Light": Theme(
                name="GitHub Light",
                background="#ffffff",
                foreground="#24292f",
                editor_bg="#ffffff",
                editor_fg="#24292f",
                line_number_bg="#f6f8fa",
                line_number_fg="#656d76",
                keyword="#cf222e",
                comment="#656d76",
                string="#0a3069",
                number="#0550ae",
                operator="#cf222e",
                function="#8250df",
                variable="#0550ae",
                selection_bg="#0969da",
                selection_fg="#ffffff",
                canvas_bg="#ffffff",
            ),
            "VS Code Dark": Theme(
                name="VS Code Dark",
                background="#1e1e1e",
                foreground="#d4d4d4",
                editor_bg="#1e1e1e",
                editor_fg="#d4d4d4",
                line_number_bg="#252526",
                line_number_fg="#858585",
                keyword="#569cd6",
                comment="#6a9955",
                string="#ce9178",
                number="#b5cea8",
                operator="#d4d4d4",
                function="#dcdcaa",
                variable="#9cdcfe",
                selection_bg="#264f78",
                selection_fg="#d4d4d4",
                canvas_bg="#1e1e1e",
            ),
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
            ),
            "Nord": Theme(
                name="Nord",
                background="#2e3440",
                foreground="#d8dee9",
                editor_bg="#2e3440",
                editor_fg="#d8dee9",
                line_number_bg="#3b4252",
                line_number_fg="#616e88",
                keyword="#81a1c1",
                comment="#616e88",
                string="#a3be8c",
                number="#ebcb8b",
                operator="#81a1c1",
                function="#88c0d0",
                variable="#81a1c1",
                selection_bg="#4c566a",
                selection_fg="#d8dee9",
                canvas_bg="#2e3440",
            ),
            "One Dark Pro": Theme(
                name="One Dark Pro",
                background="#282c34",
                foreground="#abb2bf",
                editor_bg="#282c34",
                editor_fg="#abb2bf",
                line_number_bg="#21252b",
                line_number_fg="#636d83",
                keyword="#e06c75",
                comment="#5c6370",
                string="#98c379",
                number="#d19a66",
                operator="#56b6c2",
                function="#e5c07b",
                variable="#61afef",
                selection_bg="#3e4451",
                selection_fg="#abb2bf",
                canvas_bg="#282c34",
            ),
        }

        self.current_theme_name = "Dracula"

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

        # Apply to application palette
        app = QApplication.instance()
        palette = QPalette()

        palette.setColor(QPalette.Window, QColor(theme.background))
        palette.setColor(QPalette.WindowText, QColor(theme.foreground))
        palette.setColor(QPalette.Base, QColor(theme.editor_bg))
        palette.setColor(QPalette.AlternateBase, QColor(theme.line_number_bg))
        palette.setColor(QPalette.Text, QColor(theme.editor_fg))
        palette.setColor(QPalette.Button, QColor(theme.background))
        palette.setColor(QPalette.ButtonText, QColor(theme.foreground))
        palette.setColor(QPalette.Highlight, QColor(theme.selection_bg))
        palette.setColor(QPalette.HighlightedText, QColor(theme.selection_fg))

        app.setPalette(palette)

        # Apply to editor
        if editor:
            editor.setStyleSheet(
                f"""
                QPlainTextEdit {{
                    background-color: {theme.editor_bg};
                    color: {theme.editor_fg};
                    selection-background-color: {theme.selection_bg};
                    selection-color: {theme.selection_fg};
                }}
            """
            )

        # Apply to output
        if output:
            output.setStyleSheet(
                f"""
                QTextEdit {{
                    background-color: {theme.editor_bg};
                    color: {theme.editor_fg};
                    selection-background-color: {theme.selection_bg};
                    selection-color: {theme.selection_fg};
                }}
            """
            )

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
