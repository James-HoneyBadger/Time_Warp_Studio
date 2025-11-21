"""Theme management for Time Warp IDE."""

from dataclasses import dataclass
from typing import Dict
from PySide6.QtWidgets import QApplication
from PySide6.QtGui import QPalette, QColor


@dataclass
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


class ThemeManager:
    """Manages IDE themes."""
    
    def __init__(self):
        self.themes: Dict[str, Theme] = {
            'Dracula': Theme(
                name='Dracula',
                background='#282a36',
                foreground='#f8f8f2',
                editor_bg='#282a36',
                editor_fg='#f8f8f2',
                line_number_bg='#1e1f29',
                line_number_fg='#6272a4',
                keyword='#ff79c6',
                comment='#6272a4',
                string='#f1fa8c',
                number='#bd93f9',
                selection_bg='#44475a',
                selection_fg='#f8f8f2',
                canvas_bg='#282a36'
            ),
            'Monokai': Theme(
                name='Monokai',
                background='#272822',
                foreground='#f8f8f2',
                editor_bg='#272822',
                editor_fg='#f8f8f2',
                line_number_bg='#1e1f1c',
                line_number_fg='#75715e',
                keyword='#f92672',
                comment='#75715e',
                string='#e6db74',
                number='#ae81ff',
                selection_bg='#49483e',
                selection_fg='#f8f8f2',
                canvas_bg='#272822'
            ),
            'Solarized Light': Theme(
                name='Solarized Light',
                background='#fdf6e3',
                foreground='#657b83',
                editor_bg='#fdf6e3',
                editor_fg='#657b83',
                line_number_bg='#eee8d5',
                line_number_fg='#93a1a1',
                keyword='#268bd2',
                comment='#93a1a1',
                string='#2aa198',
                number='#d33682',
                selection_bg='#eee8d5',
                selection_fg='#657b83',
                canvas_bg='#fdf6e3'
            ),
            'Solarized Dark': Theme(
                name='Solarized Dark',
                background='#002b36',
                foreground='#839496',
                editor_bg='#002b36',
                editor_fg='#839496',
                line_number_bg='#073642',
                line_number_fg='#586e75',
                keyword='#268bd2',
                comment='#586e75',
                string='#2aa198',
                number='#d33682',
                selection_bg='#073642',
                selection_fg='#839496',
                canvas_bg='#002b36'
            ),
            'Ocean': Theme(
                name='Ocean',
                background='#1b2b34',
                foreground='#c0c5ce',
                editor_bg='#1b2b34',
                editor_fg='#c0c5ce',
                line_number_bg='#343d46',
                line_number_fg='#65737e',
                keyword='#6699cc',
                comment='#65737e',
                string='#99c794',
                number='#f99157',
                selection_bg='#4f5b66',
                selection_fg='#c0c5ce',
                canvas_bg='#1b2b34'
            ),
            'Spring': Theme(
                name='Spring',
                background='#f5f5f5',
                foreground='#333333',
                editor_bg='#ffffff',
                editor_fg='#333333',
                line_number_bg='#e8e8e8',
                line_number_fg='#888888',
                keyword='#0066cc',
                comment='#008000',
                string='#dd1144',
                number='#009999',
                selection_bg='#d3e8f8',
                selection_fg='#333333',
                canvas_bg='#ffffff'
            ),
            'Sunset': Theme(
                name='Sunset',
                background='#2d1b00',
                foreground='#ffd699',
                editor_bg='#2d1b00',
                editor_fg='#ffd699',
                line_number_bg='#1a0f00',
                line_number_fg='#b37700',
                keyword='#ff9933',
                comment='#b37700',
                string='#ffcc66',
                number='#ff6600',
                selection_bg='#4d3300',
                selection_fg='#ffd699',
                canvas_bg='#2d1b00'
            ),
            'Candy': Theme(
                name='Candy',
                background='#1a0033',
                foreground='#ffccff',
                editor_bg='#1a0033',
                editor_fg='#ffccff',
                line_number_bg='#0d001a',
                line_number_fg='#b366ff',
                keyword='#ff66ff',
                comment='#9933cc',
                string='#ffb3ff',
                number='#cc66ff',
                selection_bg='#330066',
                selection_fg='#ffccff',
                canvas_bg='#1a0033'
            ),
        }
        
        self.current_theme_name = 'Dracula'
        
    def get_theme_names(self):
        """Get list of available theme names."""
        return list(self.themes.keys())
        
    def get_theme(self, name: str) -> Theme:
        """Get theme by name."""
        return self.themes.get(name, self.themes['Dracula'])
        
    def apply_theme(self, name: str, editor=None, output=None, canvas=None, highlighter=None):
        """Apply theme to application and widgets."""
        if name not in self.themes:
            name = 'Dracula'
            
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
            editor.setStyleSheet(f"""
                QPlainTextEdit {{
                    background-color: {theme.editor_bg};
                    color: {theme.editor_fg};
                    selection-background-color: {theme.selection_bg};
                    selection-color: {theme.selection_fg};
                }}
            """)
            
        # Apply to output
        if output:
            output.setStyleSheet(f"""
                QTextEdit {{
                    background-color: {theme.editor_bg};
                    color: {theme.editor_fg};
                    selection-background-color: {theme.selection_bg};
                    selection-color: {theme.selection_fg};
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
            highlighter.rehighlight()
