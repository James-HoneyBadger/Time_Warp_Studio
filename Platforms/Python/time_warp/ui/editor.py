"""Code editor widget with line numbers and basic syntax highlighting."""

# Static analyzers sometimes miss PySide6 platform-specific symbols. Silence
# pylint 'no-name-in-module' here since the runtime provides these names.
# pylint: disable=no-name-in-module

import re

from PySide6.QtCore import QRect, QSize, QStringListModel, Qt
from PySide6.QtGui import (
    QColor,
    QFont,
    QPainter,
    QPalette,
    QSyntaxHighlighter,
    QTextCharFormat,
    QTextCursor,
    QTextDocument,
)
from PySide6.QtWidgets import (
    QCompleter,
    QDialog,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPlainTextEdit,
    QPushButton,
    QTextEdit,
    QVBoxLayout,
    QWidget,
)

from ..core.interpreter import Language


class LineNumberArea(QWidget):
    """Line number area widget.

    This small helper uses Qt-style method names (sizeHint, paintEvent) which
    are required by the framework — allow those names locally.
    """

    # Qt uses camelCase callback names for paint/size methods; silence
    # invalid-name here for that reason.
    # pylint: disable=invalid-name

    def __init__(self, editor):
        super().__init__(editor)
        self.editor = editor

    def sizeHint(self):
        """Return a suggested size for the line number area."""
        return QSize(self.editor.line_number_area_width(), 0)

    def paintEvent(self, event):
        """Forward paint event to the parent editor for rendering."""
        self.editor.line_number_area_paint_event(event)


class SimpleSyntaxHighlighter(QSyntaxHighlighter):
    """Enhanced syntax highlighter for Time Warp languages.

    This class uses highlightBlock (Qt API) and contains many language
    rules so some complexity/length checks are disabled locally.
    """

    # Qt API uses camelCase names and the rule tables are large. Keep
    # these checks off for readability and straightforward rule lists.
    # This class intentionally keeps several QTextCharFormat attributes and
    # pattern tables — allow the larger instance count here.
    # pylint: disable=invalid-name,too-many-statements
    # pylint: disable=too-many-instance-attributes

    def __init__(self, document, language=None):
        super().__init__(document)

        if language is None:
            language = Language.BASIC

        # Define formats
        self.keyword_format = QTextCharFormat()
        self.keyword_format.setFontWeight(QFont.Bold)

        self.comment_format = QTextCharFormat()

        self.string_format = QTextCharFormat()

        self.number_format = QTextCharFormat()

        self.operator_format = QTextCharFormat()

        self.function_format = QTextCharFormat()
        self.function_format.setFontItalic(True)

        self.variable_format = QTextCharFormat()

        # Language-specific keywords and patterns
        self._setup_language_patterns(language)

    def _setup_language_patterns(self, language):
        """Setup syntax patterns for the specified language."""
        if language == Language.PILOT:
            self.keywords = [
                "T:",
                "A:",
                "M:",
                "Y:",
                "N:",
                "C:",
                "U:",
                "J:",
                "L:",
                "E:",
                "R:",
                "ACCEPT",
                "COMPUTE",
                "END",
                "JUMP",
                "MATCH",
                "TYPE",
                "USE",
            ]
            self.comment_pattern = r"(^|\s)R:.*$"
            self.string_pattern = r'"[^"]*"'
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[+\-*/=<>!&|]"
            self.function_pattern = None
            self.variable_pattern = r"\*[A-Z_][A-Z0-9_]*\*"

        elif language == Language.BASIC:
            self.keywords = [
                "PRINT",
                "LET",
                "INPUT",
                "GOTO",
                "IF",
                "THEN",
                "ELSE",
                "FOR",
                "TO",
                "STEP",
                "NEXT",
                "GOSUB",
                "RETURN",
                "REM",
                "END",
                "DIM",
                "DATA",
                "READ",
                "SCREEN",
                "CLS",
                "LOCATE",
                "DEF",
                "FN",
                "WHILE",
                "WEND",
                "DO",
                "LOOP",
                "UNTIL",
                "SELECT",
                "CASE",
                "AND",
                "OR",
                "NOT",
                "MOD",
                "INT",
                "ABS",
                "SIN",
                "COS",
                "TAN",
                "SQR",
                "RND",
                "LEN",
                "LEFT$",
                "RIGHT$",
                "MID$",
                "CHR$",
                "ASC",
                "VAL",
                "STR$",
            ]
            self.comment_pattern = r"(^|\s)REM\b.*$"
            self.string_pattern = r'"[^"]*"'
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[+\-*/=<>!&|]"
            self.function_pattern = r"\bFN[A-Z_][A-Z0-9_]*\b"
            self.variable_pattern = r"\b[A-Z_][A-Z0-9_]*[$%!#]?\b"

        elif language == Language.LOGO:
            self.keywords = [
                "FORWARD",
                "FD",
                "BACK",
                "BK",
                "LEFT",
                "LT",
                "RIGHT",
                "RT",
                "PENUP",
                "PU",
                "PENDOWN",
                "PD",
                "HOME",
                "CLEARSCREEN",
                "CS",
                "REPEAT",
                "TO",
                "END",
                "SETXY",
                "SETHEADING",
                "SETH",
                "SETCOLOR",
                "SETPENCOLOR",
                "SETBGCOLOR",
                "SETPENWIDTH",
                "HIDETURTLE",
                "HT",
                "SHOWTURTLE",
                "ST",
                "PRINT",
                "MAKE",
                "THING",
                "IF",
                "IFELSE",
                "STOP",
                "OUTPUT",
                "LOCAL",
                "GLOBAL",
            ]
            self.comment_pattern = r";.*$"
            self.string_pattern = r'"[^"]*"'
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[+\-*/=<>]"
            self.function_pattern = r"\b[A-Z_][A-Z0-9_]*\b(?=\s*\()"
            self.variable_pattern = r":[A-Z_][A-Z0-9_]*"

        elif language == Language.C:
            self.keywords = [
                "auto",
                "break",
                "case",
                "char",
                "const",
                "continue",
                "default",
                "do",
                "double",
                "else",
                "enum",
                "extern",
                "float",
                "for",
                "goto",
                "if",
                "int",
                "long",
                "register",
                "return",
                "short",
                "signed",
                "sizeof",
                "static",
                "struct",
                "switch",
                "typedef",
                "union",
                "unsigned",
                "void",
                "volatile",
                "while",
                "printf",
                "scanf",
                "malloc",
                "free",
                "include",
                "define",
            ]
            self.comment_pattern = r"(//.*$|/\*.*?\*/)"
            self.string_pattern = r'"(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\''
            self.number_pattern = r"\b\d+\.?\d*[fFlLuU]*\b|0[xX][0-9a-fA-F]+\b"
            self.operator_pattern = r"[+\-*/=<>!&|%^~?:]"
            self.function_pattern = r"\b[A-Za-z_][A-Za-z0-9_]*\b(?=\s*\()"
            self.variable_pattern = r"\b[A-Za-z_][A-Za-z0-9_]*\b"

        elif language == Language.PASCAL:
            self.keywords = [
                "and",
                "array",
                "begin",
                "case",
                "const",
                "div",
                "do",
                "downto",
                "else",
                "end",
                "file",
                "for",
                "function",
                "goto",
                "if",
                "in",
                "label",
                "mod",
                "nil",
                "not",
                "of",
                "or",
                "packed",
                "procedure",
                "program",
                "record",
                "repeat",
                "set",
                "then",
                "to",
                "type",
                "until",
                "var",
                "while",
                "with",
                "writeln",
                "readln",
                "write",
                "read",
                "integer",
                "real",
                "char",
                "boolean",
                "string",
            ]
            self.comment_pattern = r"\{.*?\}|\(\*.*?\*\)"
            self.string_pattern = r"'[^']*'"
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[+\-*/=<>!&|]"
            self.function_pattern = r"\b[A-Za-z_][A-Za-z0-9_]*\b(?=\s*\()"
            self.variable_pattern = r"\b[A-Za-z_][A-Za-z0-9_]*\b"

        elif language == Language.PROLOG:
            self.keywords = [
                "is",
                "not",
                "fail",
                "true",
                "false",
                "cut",
                "!",
                "assert",
                "retract",
                "consult",
                "listing",
                "halt",
                "write",
                "nl",
                "read",
                "atom",
                "integer",
                "var",
                "nonvar",
                "number",
                "atomic",
                "compound",
            ]
            self.comment_pattern = r"%.*$|/\*.*?\*/"
            self.string_pattern = r'"[^"]*"'
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[+\-*/=<>!&|]"
            self.function_pattern = r"\b[a-z][a-zA-Z0-9_]*\b(?=\s*\()"
            self.variable_pattern = r"\b[A-Z_][A-Za-z0-9_]*\b"

        else:
            # Default to BASIC
            self._setup_language_patterns(Language.BASIC)

    def set_language(self, language):
        """Set the syntax highlighting language."""
        self._setup_language_patterns(language)
        self.rehighlight()

    def highlightBlock(self, text):
        """Highlight a single block of text for syntax patterns.

        The method implements language-specific highlighting and may be large
        because it enumerates many token types and patterns.
        """
        # pylint: disable=too-many-branches
        # (docstring moved earlier; keep implementation-only block)
        # Keywords
        if hasattr(self, "keywords"):
            for keyword in self.keywords:
                pattern = r"\b" + re.escape(keyword) + r"\b"
                for match in re.finditer(pattern, text, re.IGNORECASE):
                    start = match.start()
                    length = match.end() - match.start()
                    self.setFormat(start, length, self.keyword_format)

        # Comments
        if hasattr(self, "comment_pattern") and self.comment_pattern:
            for match in re.finditer(
                self.comment_pattern, text, re.MULTILINE | re.DOTALL
            ):
                start = match.start()
                length = match.end() - start
                self.setFormat(start, length, self.comment_format)

        # Strings
        if hasattr(self, "string_pattern") and self.string_pattern:
            for match in re.finditer(self.string_pattern, text):
                start = match.start()
                length = match.end() - start
                self.setFormat(start, length, self.string_format)

        # Numbers
        if hasattr(self, "number_pattern") and self.number_pattern:
            for match in re.finditer(self.number_pattern, text):
                start = match.start()
                length = match.end() - start
                self.setFormat(start, length, self.number_format)

        # Operators
        if hasattr(self, "operator_pattern") and self.operator_pattern:
            for match in re.finditer(self.operator_pattern, text):
                start = match.start()
                length = match.end() - start
                self.setFormat(start, length, self.operator_format)

        # Functions
        if hasattr(self, "function_pattern") and self.function_pattern:
            for match in re.finditer(self.function_pattern, text):
                start = match.start()
                length = match.end() - start
                self.setFormat(start, length, self.function_format)

        # Variables
        if hasattr(self, "variable_pattern") and self.variable_pattern:
            for match in re.finditer(self.variable_pattern, text):
                # Skip if it's already highlighted as a keyword or function
                start = match.start()
                length = match.end() - match.start()
                if not self.format(start).fontWeight() == QFont.Bold:
                    self.setFormat(start, length, self.variable_format)


class _WhitespaceHighlighter(QSyntaxHighlighter):
    """Simple highlighter to visually mark runs of spaces/tabs.

    Implementation uses a subtle underline so whitespace is visible but the
    document text is not modified.
    """

    # no custom init needed
    # The highlighter is intentionally small (single public method). It
    # implements the QSyntaxHighlighter API which uses camelCase names.
    # pylint: disable=too-few-public-methods,invalid-name

    def highlightBlock(self, text):  # pragma: no cover - UI
        """Highlight whitespace runs (spaces/tabs) visually.

        We purposely keep this lightweight and do not modify document text.
        """
        for match in re.finditer(r"[ \t]+", text):
            start = match.start()
            length = match.end() - start
            fmt = QTextCharFormat()
            fmt.setUnderlineStyle(QTextCharFormat.SingleUnderline)
            fmt.setUnderlineColor(QColor(160, 160, 160))
            self.setFormat(start, length, fmt)


class FindDialog(QDialog):
    """Simple find dialog."""

    def __init__(self, parent):
        super().__init__(parent)
        self.editor = parent
        self.setup_ui()

    def setup_ui(self):
        """Setup dialog UI."""
        self.setWindowTitle("Find")
        layout = QVBoxLayout()

        # Search field
        search_layout = QHBoxLayout()
        search_layout.addWidget(QLabel("Find:"))
        self.search_field = QLineEdit()
        self.search_field.returnPressed.connect(self.find_next)
        search_layout.addWidget(self.search_field)
        layout.addLayout(search_layout)

        # Buttons
        button_layout = QHBoxLayout()
        find_next_btn = QPushButton("Find Next")
        find_next_btn.clicked.connect(self.find_next)
        button_layout.addWidget(find_next_btn)

        find_prev_btn = QPushButton("Find Previous")
        find_prev_btn.clicked.connect(self.find_previous)
        button_layout.addWidget(find_prev_btn)

        close_btn = QPushButton("Close")
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
            self.editor.find(text, QTextDocument.FindBackward)


class CodeEditor(QPlainTextEdit):
    """Code editor with line numbers.

    This widget exposes Qt/QtWidgets API methods using camelCase names
    (e.g. keyPressEvent, resizeEvent) and maintains multiple runtime
    attributes for UI state. Suppress those specific lint checks here.
    """

    # Qt naming and UI statefulness — disable relevant style checks.
    # pylint: disable=too-many-instance-attributes,invalid-name

    def __init__(self, parent=None):
        super().__init__(parent)

        # Line number area
        self.line_number_area = LineNumberArea(self)

        # Syntax highlighter
        self.highlighter = SimpleSyntaxHighlighter(self.document())

        # Auto-completer
        self.completer = QCompleter()
        self.completer.setWidget(self)
        self.completer.setCompletionMode(QCompleter.PopupCompletion)
        self.completer.setCaseSensitivity(Qt.CaseInsensitive)
        self.completer.activated.connect(self.insert_completion)

        # Initialize completer with default language
        self._update_completer(Language.BASIC)

        # Font
        font = QFont("Courier New", 12)
        self.setFont(font)

        # Tab settings
        self.setTabStopDistance(40)  # 4 spaces worth

        # Connect signals
        self.blockCountChanged.connect(self.update_line_number_area_width)
        self.updateRequest.connect(self.update_line_number_area)

        self.update_line_number_area_width(0)

        # Highlighting features (defaults off)
        self._highlight_current_line = False
        self._whitespace_highlighter = None

    def set_language(self, language):
        """Set the syntax highlighting language."""
        # Re-create highlighter with new language
        self.highlighter = SimpleSyntaxHighlighter(self.document(), language)
        # Force re-highlight
        self.highlighter.rehighlight()

        # Update completer with language keywords
        self._update_completer(language)

    def _update_completer(self, language):
        """Update completer with keywords for the language."""
        # Get keywords from highlighter
        temp_highlighter = SimpleSyntaxHighlighter(None, language)
        keywords = temp_highlighter.keywords

        # Set completer model
        model = QStringListModel(keywords)
        self.completer.setModel(model)

    def line_number_area_width(self):
        """Calculate width of line number area."""
        digits = len(str(max(1, self.blockCount())))
        space = 3 + self.fontMetrics().horizontalAdvance("9") * digits
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
                0, rect.y(), self.line_number_area.width(), rect.height()
            )

        if rect.contains(self.viewport().rect()):
            self.update_line_number_area_width(0)

    def resizeEvent(self, event):
        """Handle resize event."""
        super().resizeEvent(event)

        cr = self.contentsRect()
        width = self.line_number_area_width()
        rect = QRect(cr.left(), cr.top(), width, cr.height())
        self.line_number_area.setGeometry(rect)

    def line_number_area_paint_event(self, event):
        """Paint line numbers."""
        painter = QPainter(self.line_number_area)

        # Background
        palette = self.palette()
        bg_color = palette.color(QPalette.Window).darker(110)
        painter.fillRect(event.rect(), bg_color)

        # Line numbers
        block = self.document().firstBlock()
        geom = self.blockBoundingGeometry(block)
        top = geom.translated(self.contentOffset()).top()
        block_number = block.blockNumber()
        geom = self.blockBoundingGeometry(block)
        top = geom.translated(self.contentOffset()).top()
        bottom = top + self.blockBoundingRect(block).height()

        fg_color = palette.color(QPalette.Text).darker(150)

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                number = str(block_number + 1)
                painter.setPen(fg_color)
                painter.drawText(
                    0,
                    int(top),
                    self.line_number_area.width() - 5,
                    self.fontMetrics().height(),
                    Qt.AlignRight,
                    number,
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

    def enable_highlight_current_line(self, enable: bool):
        """Toggle highlighting for the current cursor line.

        When enabled this will keep an extra selection with a subtle
        background color on the line that contains the cursor.
        """
        self._highlight_current_line = bool(enable)

        # connect/disconnect signal for live updates
        try:
            self.cursorPositionChanged.disconnect(self._update_current_line_highlight)
        except TypeError:
            # ignore if not connected
            pass

        if self._highlight_current_line:
            self.cursorPositionChanged.connect(self._update_current_line_highlight)
            # ensure highlight is applied immediately
            self._update_current_line_highlight()
        else:
            # clear any existing extra selection used for line highlight
            self.setExtraSelections([])

    def is_highlight_current_line_enabled(self) -> bool:
        """Return True if current-line highlighting is enabled."""
        return self._highlight_current_line

    def _update_current_line_highlight(self):
        """Apply an extra selection that highlights the current line."""
        if not self._highlight_current_line:
            return

        selection = QTextEdit.ExtraSelection()
        palette = self.palette()
        bg = palette.color(QPalette.Highlight).lighter(150)
        fmt = selection.format
        fmt.setBackground(bg)

        cursor = self.textCursor()
        cursor.clearSelection()
        selection.cursor = cursor

        # Keep existing non-highlight selections (e.g. other plugins).
        extras = [selection]
        existing = [s for s in self.extraSelections() if s is not selection]
        extras.extend(existing)
        self.setExtraSelections(extras)

    def zoom_out(self):
        """Decrease font size."""
        font = self.font()
        size = font.pointSize()
        if size > 6:
            font.setPointSize(size - 1)
            self.setFont(font)

    def insert_completion(self, completion):
        """Insert the selected completion."""
        cursor = self.textCursor()
        extra = len(completion) - len(self.completer.completionPrefix())
        cursor.movePosition(QTextCursor.Left)
        cursor.movePosition(QTextCursor.EndOfWord)
        cursor.insertText(completion[-extra:])
        self.setTextCursor(cursor)

    def enable_show_whitespace(self, enable: bool):
        """Toggle a visual whitespace highlighter.

        When enabled a lightweight QSyntaxHighlighter will mark runs of spaces
        and tabs with a subtle underline so they become visible for learners.
        """
        if enable:
            if self._whitespace_highlighter is None:
                self._whitespace_highlighter = _WhitespaceHighlighter(self.document())
        else:
            if self._whitespace_highlighter is not None:
                # Delete the highlighter instance to stop highlighting
                self._whitespace_highlighter.setDocument(None)
                self._whitespace_highlighter = None

    def is_show_whitespace_enabled(self) -> bool:
        """Return True if the whitespace highlighter is active."""
        return self._whitespace_highlighter is not None

    def keyPressEvent(self, event):
        """Handle key press events for auto-completion."""
        if self.completer.popup().isVisible():
            if event.key() in (
                Qt.Key_Enter,
                Qt.Key_Return,
                Qt.Key_Escape,
                Qt.Key_Tab,
                Qt.Key_Backtab,
            ):
                event.ignore()
                return

        # Call parent implementation first
        super().keyPressEvent(event)

        # Check for completion trigger
        cursor = self.textCursor()
        if cursor.hasSelection():
            return

        # Get the text under cursor
        cursor.movePosition(QTextCursor.StartOfWord, QTextCursor.KeepAnchor)
        prefix = cursor.selectedText()

        if len(prefix) >= 2:  # Start completing after 2 characters
            if self.completer.completionPrefix() != prefix:
                self.completer.setCompletionPrefix(prefix)
                popup = self.completer.popup()
                idx = self.completer.completionModel().index(0, 0)
                popup.setCurrentIndex(idx)

                cr = self.cursorRect()
                cr.setWidth(
                    popup.sizeHintForColumn(0)
                    + popup.verticalScrollBar().sizeHint().width()
                )
                self.completer.complete(cr)
