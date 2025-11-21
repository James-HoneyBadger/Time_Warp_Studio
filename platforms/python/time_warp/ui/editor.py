"""Code editor widget with line numbers and basic syntax highlighting."""

from PySide6.QtWidgets import (
    QPlainTextEdit, QWidget, QDialog,
    QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton, QLabel
)
from PySide6.QtCore import Qt, QRect, QSize
from PySide6.QtGui import (
    QColor, QPainter, QFont,
    QSyntaxHighlighter, QTextCharFormat, QPalette, QTextDocument
)
import re


class LineNumberArea(QWidget):
    """Line number area widget."""
    
    def __init__(self, editor):
        super().__init__(editor)
        self.editor = editor
        
    def sizeHint(self):
        return QSize(self.editor.line_number_area_width(), 0)
        
    def paintEvent(self, event):
        self.editor.line_number_area_paint_event(event)


class SimpleSyntaxHighlighter(QSyntaxHighlighter):
    """Simple syntax highlighter for TempleCode (BASIC/PILOT/Logo styles)."""
    
    def __init__(self, document):
        super().__init__(document)
        
        # Define formats
        self.keyword_format = QTextCharFormat()
        self.keyword_format.setForeground(QColor(86, 156, 214))  # Blue
        self.keyword_format.setFontWeight(QFont.Bold)
        
        self.comment_format = QTextCharFormat()
        self.comment_format.setForeground(QColor(106, 153, 85))  # Green
        
        self.string_format = QTextCharFormat()
        self.string_format.setForeground(QColor(206, 145, 120))  # Orange
        
        self.number_format = QTextCharFormat()
        self.number_format.setForeground(QColor(181, 206, 168))  # Light green
        
    # TempleCode style keywords (PILOT-style, BASIC-style, Logo-style)
        pilot_keywords = [
            'T:', 'A:', 'M:', 'Y:', 'N:', 'C:', 'U:', 'J:', 'L:', 'E:', 'R:'
        ]
        basic_keywords = [
            'PRINT', 'LET', 'INPUT', 'GOTO', 'IF', 'THEN', 'ELSE',
            'FOR', 'TO', 'STEP', 'NEXT', 'GOSUB', 'RETURN', 'REM',
            'END', 'DIM', 'DATA', 'READ', 'SCREEN', 'CLS', 'LOCATE'
        ]
        logo_keywords = [
            'FORWARD', 'FD', 'BACK', 'BK', 'LEFT', 'LT', 'RIGHT', 'RT',
            'PENUP', 'PU', 'PENDOWN', 'PD', 'HOME', 'CLEARSCREEN', 'CS',
            'REPEAT', 'TO', 'END', 'SETXY', 'SETHEADING', 'SETH',
            'SETCOLOR', 'SETPENCOLOR', 'SETBGCOLOR', 'SETPENWIDTH',
            'HIDETURTLE', 'HT', 'SHOWTURTLE', 'ST', 'PRINT'
        ]
        
        self.keywords = pilot_keywords + basic_keywords + logo_keywords
        
    def highlightBlock(self, text):
        """Highlight a single block of text."""
        # Keywords
        for keyword in self.keywords:
            pattern = r'\b' + re.escape(keyword) + r'\b'
            for match in re.finditer(pattern, text, re.IGNORECASE):
                self.setFormat(
                    match.start(),
                    match.end() - match.start(),
                    self.keyword_format
                )
        
        # Comments (REM in BASIC, R: in PILOT)
        comment_pattern = r'(^|\s)REM\b.*$|^R:.*$'
        for match in re.finditer(comment_pattern, text, re.IGNORECASE):
            self.setFormat(
                match.start(),
                match.end() - match.start(),
                self.comment_format
            )
        
        # Strings (double quotes)
        string_pattern = r'"[^"]*"'
        for match in re.finditer(string_pattern, text):
            self.setFormat(
                match.start(),
                match.end() - match.start(),
                self.string_format
            )
        
        # Numbers
        number_pattern = r'\b\d+\.?\d*\b'
        for match in re.finditer(number_pattern, text):
            self.setFormat(
                match.start(),
                match.end() - match.start(),
                self.number_format
            )


class FindDialog(QDialog):
    """Simple find dialog."""
    
    def __init__(self, parent):
        super().__init__(parent)
        self.editor = parent
        self.setup_ui()
        
    def setup_ui(self):
        """Setup dialog UI."""
        self.setWindowTitle('Find')
        layout = QVBoxLayout()
        
        # Search field
        search_layout = QHBoxLayout()
        search_layout.addWidget(QLabel('Find:'))
        self.search_field = QLineEdit()
        self.search_field.returnPressed.connect(self.find_next)
        search_layout.addWidget(self.search_field)
        layout.addLayout(search_layout)
        
        # Buttons
        button_layout = QHBoxLayout()
        find_next_btn = QPushButton('Find Next')
        find_next_btn.clicked.connect(self.find_next)
        button_layout.addWidget(find_next_btn)
        
        find_prev_btn = QPushButton('Find Previous')
        find_prev_btn.clicked.connect(self.find_previous)
        button_layout.addWidget(find_prev_btn)
        
        close_btn = QPushButton('Close')
        close_btn.clicked.connect(self.close)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
        self.setLayout(layout)
        
    def find_next(self):
        """Find next occurrence."""
        text = self.search_field.text()
        if text:
            self.editor.find(text)
            
    def find_previous(self):
        """Find previous occurrence."""
        text = self.search_field.text()
        if text:
            self.editor.find(
                text,
                QTextDocument.FindBackward
            )


class CodeEditor(QPlainTextEdit):
    """Code editor with line numbers."""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        
        # Line number area
        self.line_number_area = LineNumberArea(self)
        
        # Syntax highlighter
        self.highlighter = SimpleSyntaxHighlighter(self.document())
        
        # Font
        font = QFont('Courier New', 12)
        self.setFont(font)
        
        # Tab settings
        self.setTabStopDistance(40)  # 4 spaces worth
        
        # Connect signals
        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)
        
        self.update_line_number_area_width(0)
        
    def line_number_area_width(self):
        """Calculate width of line number area."""
        digits = len(str(max(1, self.blockCount())))
        space = 3 + self.fontMetrics().horizontalAdvance('9') * digits
        return space
        
    def update_line_number_area_width(self, _):
        """Update line number area width."""
        self.setViewportMargins(self.line_number_area_width(), 0, 0, 0)
        
    def update_line_number_area(self, rect, dy):
        """Update line number area."""
        if dy:
            self.line_number_area.scroll(0, dy)
        else:
            self.line_number_area.update(
                0, rect.y(),
                self.line_number_area.width(),
                rect.height()
            )
            
        if rect.contains(self.viewport().rect()):
            self.update_line_number_area_width(0)
            
    def resizeEvent(self, event):
        """Handle resize event."""
        super().resizeEvent(event)
        
        cr = self.contentsRect()
        self.line_number_area.setGeometry(
            QRect(cr.left(), cr.top(),
                  self.line_number_area_width(), cr.height())
        )
        
    def line_number_area_paint_event(self, event):
        """Paint line numbers."""
        painter = QPainter(self.line_number_area)
        
        # Background
        palette = self.palette()
        bg_color = palette.color(QPalette.Window).darker(110)
        painter.fillRect(event.rect(), bg_color)
        
        # Line numbers
        block = self.firstVisibleBlock()
        block_number = block.blockNumber()
        top = self.blockBoundingGeometry(block).translated(
            self.contentOffset()
        ).top()
        bottom = top + self.blockBoundingRect(block).height()
        
        fg_color = palette.color(QPalette.Text).darker(150)
        
        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                painter.setPen(fg_color)
                painter.drawText(
                    0, int(top),
                    self.line_number_area.width() - 5,
                    self.fontMetrics().height(),
                    Qt.AlignRight,
                    number
                )
                
            block = block.next()
            top = bottom
            bottom = top + self.blockBoundingRect(block).height()
            block_number += 1
            
    def zoom_in(self):
        """Increase font size."""
        font = self.font()
        size = font.pointSize()
        if size < 32:
            font.setPointSize(size + 1)
            self.setFont(font)
            
    def zoom_out(self):
        """Decrease font size."""
        font = self.font()
        size = font.pointSize()
        if size > 6:
            font.setPointSize(size - 1)
            self.setFont(font)
            
    def show_find_dialog(self):
        """Show find dialog."""
        dialog = FindDialog(self)
        dialog.show()
