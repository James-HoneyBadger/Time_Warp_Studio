# pylint: disable=too-many-lines
"""Code editor widget with line numbers and basic syntax highlighting."""

# Static analyzers sometimes miss PySide6 platform-specific symbols. Silence
# pylint 'no-name-in-module' here since the runtime provides these names.
# pylint: disable=no-name-in-module

import re

from PySide6.QtCore import QPoint, QRect, QSize, QStringListModel, Qt, Signal
from PySide6.QtGui import (
    QBrush,
    QColor,
    QFont,
    QMouseEvent,
    QPainter,
    QPalette,
    QPen,
    QPolygon,
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
    """Line number area widget with breakpoint support.

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

    def mousePressEvent(self, event: QMouseEvent):
        """Handle mouse click to toggle breakpoints."""
        if event.button() == Qt.MouseButton.LeftButton:
            # Calculate which line was clicked
            block = self.editor.firstVisibleBlock()
            top = (
                self.editor.blockBoundingGeometry(block)
                .translated(self.editor.contentOffset())
                .top()
            )

            while block.isValid():
                block_top = top
                block_bottom = block_top + self.editor.blockBoundingRect(block).height()

                if block_top <= event.position().y() < block_bottom:
                    line_number = block.blockNumber() + 1
                    self.editor.toggle_breakpoint(line_number)
                    break

                block = block.next()
                top = block_bottom

        super().mousePressEvent(event)


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

        elif language == Language.FORTH:
            self.keywords = [
                "DUP",
                "DROP",
                "SWAP",
                "OVER",
                "ROT",
                "IF",
                "ELSE",
                "THEN",
                "DO",
                "LOOP",
                "BEGIN",
                "UNTIL",
                "WHILE",
                "REPEAT",
                "VARIABLE",
                "CONSTANT",
                "ALLOT",
                "CREATE",
                "!",
                "@",
                ",",
                ":",
                ";",
                ".",
                ".S",
                "CR",
                "FD",
                "BK",
                "RT",
                "LT",
                "PU",
                "PD",
                "HOME",
                "CLEAN",
                "PEN",
            ]
            self.comment_pattern = r"\( .*? \)|\\ .*$"
            self.string_pattern = r'\." .*?"'
            self.number_pattern = r"\b-?\d+\b"
            self.operator_pattern = r"[+\-*/=<>!@]"
            self.function_pattern = r":\s+(\S+)"
            self.variable_pattern = r"\b[A-Z0-9_]+\b"

        elif language == Language.PYTHON:
            self.keywords = [
                # Statement keywords
                "False", "None", "True",
                "and", "as", "assert", "async", "await",
                "break", "class", "continue", "def", "del",
                "elif", "else", "except", "finally", "for",
                "from", "global", "if", "import", "in",
                "is", "lambda", "nonlocal", "not", "or",
                "pass", "raise", "return", "try", "while",
                "with", "yield",
                # Common builtins
                "print", "input", "len", "range", "type",
                "int", "float", "str", "bool", "list",
                "dict", "set", "tuple", "enumerate", "zip",
                "map", "filter", "sorted", "reversed",
                "abs", "max", "min", "sum", "round",
                "open", "repr", "format", "hasattr", "getattr",
                "setattr", "isinstance", "issubclass",
                "super", "object", "staticmethod", "classmethod",
                "property",
                # Common exceptions
                "Exception", "ValueError", "TypeError",
                "KeyError", "IndexError", "AttributeError",
                "NameError", "ZeroDivisionError", "RuntimeError",
                "StopIteration", "NotImplementedError",
                "ImportError", "FileNotFoundError",
            ]
            self.comment_pattern = r"#.*$"
            self.string_pattern = r'"""[\s\S]*?"""|\'\'\'[\s\S]*?\'\'\'|"[^"\\]*(?:\\.[^"\\]*)*"|\'[^\'\\]*(?:\\.[^\'\\]*)*\''
            self.number_pattern = r"\b0[xX][0-9a-fA-F]+\b|\b\d+\.?\d*(?:[eE][+-]?\d+)?[jJ]?\b"
            self.operator_pattern = r"[+\-*/=<>!&|%^~@]"
            self.function_pattern = r"\bdef\s+([A-Za-z_][A-Za-z0-9_]*)"
            self.variable_pattern = r"\b[A-Za-z_][A-Za-z0-9_]*\b"

        elif language == Language.LUA:
            self.keywords = [
                "and", "break", "do", "else", "elseif", "end", "false", "for",
                "function", "goto", "if", "in", "local", "nil", "not", "or",
                "repeat", "return", "then", "true", "until", "while",
                "print", "pairs", "ipairs", "next", "type", "tostring", "tonumber",
                "string", "table", "math", "io", "os", "unpack", "select",
                "pcall", "xpcall", "error", "assert", "require", "rawget", "rawset",
            ]
            self.comment_pattern = r"--.*$|--\[\[[\s\S]*?\]\]"
            self.string_pattern = r'"[^"\\]*(?:\\.[^"\\]*)*"|\'[^\'\\]*(?:\\.[^\'\\]*)*\'|\[\[[\s\S]*?\]\]'
            self.number_pattern = r"\b0[xX][0-9a-fA-F]+\b|\b\d+\.?\d*(?:[eE][+-]?\d+)?\b"
            self.operator_pattern = r"[+\-*/=<>~%#^&|]"
            self.function_pattern = r"\bfunction\s+([A-Za-z_][A-Za-z0-9_.]*)"
            self.variable_pattern = r"\b[A-Za-z_][A-Za-z0-9_]*\b"

        elif language == Language.SCHEME:
            self.keywords = [
                "define", "lambda", "let", "let*", "letrec", "if", "cond",
                "case", "and", "or", "when", "unless", "begin", "do", "set!",
                "quote", "quasiquote", "unquote", "unquote-splicing", "define-syntax",
                "let-syntax", "letrec-syntax", "syntax-rules",
                "car", "cdr", "cons", "list", "pair?", "null?", "number?", "string?",
                "symbol?", "boolean?", "procedure?", "eq?", "eqv?", "equal?",
                "map", "filter", "fold-left", "fold-right", "for-each",
                "display", "newline", "write", "read", "apply",
                "+", "-", "*", "/", "<", ">", "<=", ">=", "=",
                "abs", "floor", "ceiling", "round", "truncate", "expt", "sqrt",
                "string-append", "string-length", "substring", "number->string",
                "string->number", "string->list", "list->string",
            ]
            self.comment_pattern = r";.*$"
            self.string_pattern = r'"[^"\\]*(?:\\.[^"\\]*)*"'
            self.number_pattern = r"\b#?-?\d+\.?\d*\b"
            self.operator_pattern = r"[()\[\]']"
            self.function_pattern = r"\(define\s+\(([A-Za-z!?<>/+\-*_.][A-Za-z0-9!?<>/+\-*_.]*)"
            self.variable_pattern = r"\b[A-Za-z!?<>/+\-*_.][A-Za-z0-9!?<>/+\-*_.]*\b"

        elif language == Language.COBOL:
            self.keywords = [
                "IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE", "DIVISION",
                "PROGRAM-ID", "WORKING-STORAGE", "SECTION", "PARAGRAPH",
                "MOVE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "COMPUTE",
                "DISPLAY", "ACCEPT", "PERFORM", "VARYING", "UNTIL",
                "IF", "THEN", "ELSE", "END-IF", "EVALUATE", "WHEN", "END-EVALUATE",
                "STOP", "RUN", "GO", "TO", "THRU", "TIMES",
                "OPEN", "CLOSE", "READ", "WRITE", "REWRITE", "DELETE",
                "PIC", "PICTURE", "VALUE", "IS", "ZERO", "ZEROS", "ZEROES",
                "SPACE", "SPACES", "QUOTE", "QUOTES", "ALL", "BY",
                "GIVING", "FROM", "INTO", "OF", "ON", "SIZE", "ERROR",
                "NOT", "AND", "OR", "TRUE", "FALSE",
                "01", "05", "10", "15", "77", "88",
            ]
            self.comment_pattern = r"^\s*\*.*$"
            self.string_pattern = r'"[^"]*"|\'[^\']*\''
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[+\-*/=<>]"
            self.function_pattern = r"^\s+(\w+)\s*\."
            self.variable_pattern = r"\b[A-Z][A-Z0-9\-]*\b"

        elif language == Language.BRAINFUCK:
            self.keywords = []
            self.comment_pattern = r"[^+\-<>\.,\[\]\n]+"
            self.string_pattern = None
            self.number_pattern = None
            self.operator_pattern = r"[+\-<>\.,\[\]]"
            self.function_pattern = None
            self.variable_pattern = None

        elif language == Language.ASSEMBLY:
            self.keywords = [
                "MOV", "ADD", "SUB", "MUL", "DIV", "MOD", "AND", "OR", "XOR",
                "NOT", "INC", "DEC", "CMP", "PUSH", "POP", "CALL", "RET",
                "LOAD", "STORE", "JMP", "JE", "JNE", "JG", "JL", "JGE", "JLE",
                "PRINT", "INPUT", "HALT", "NOP",
                "FORWARD", "FWD", "BACKWARD", "BCK", "LEFT", "LFT", "RIGHT", "RGT",
                "PEN", "UP", "DOWN",
            ]
            self.comment_pattern = r";.*$"
            self.string_pattern = r'"[^"]*"'
            self.number_pattern = r"\b0[xX][0-9a-fA-F]+\b|\b0[bB][01]+\b|\b\d+\b"
            self.operator_pattern = r"[,:\[\]]"
            self.function_pattern = r"^([A-Za-z_][A-Za-z0-9_]*):"
            self.variable_pattern = r"\bR\d+\b|\b[A-Za-z_][A-Za-z0-9_]*\b"

        elif language == Language.JAVASCRIPT:
            self.keywords = [
                "var", "let", "const", "function", "return", "if", "else",
                "for", "while", "do", "break", "continue", "switch", "case",
                "default", "new", "delete", "typeof", "instanceof", "this",
                "null", "undefined", "true", "false", "try", "catch", "finally",
                "throw", "class", "extends", "import", "export", "async", "await",
                "of", "in", "void", "with", "yield", "static", "super",
                "console", "Math", "Array", "Object", "String", "Number", "Boolean",
                "JSON", "Date", "RegExp", "Error", "Promise", "Map", "Set",
                "parseInt", "parseFloat", "isNaN", "isFinite", "alert", "prompt",
                "document", "window", "setTimeout", "setInterval", "clearTimeout",
            ]
            self.comment_pattern = r"//.*$|/\*[\s\S]*?\*/"
            self.string_pattern = r'`[^`]*`|"[^"\\]*(?:\\.[^"\\]*)*"|\'[^\'\\]*(?:\\.[^\'\\]*)*\''
            self.number_pattern = r"\b0[xX][0-9a-fA-F]+\b|\b\d+\.?\d*(?:[eE][+-]?\d+)?\b"
            self.operator_pattern = r"[+\-*/=<>!&|%^~?:.]"
            self.function_pattern = r"\bfunction\s*([A-Za-z_$][A-Za-z0-9_$]*)|([A-Za-z_$][A-Za-z0-9_$]*)\s*=>\s*"
            self.variable_pattern = r"\b[A-Za-z_$][A-Za-z0-9_$]*\b"

        elif language == Language.FORTRAN:
            self.keywords = [
                "PROGRAM", "END", "STOP", "PAUSE", "RETURN", "CONTINUE",
                "INTEGER", "REAL", "DOUBLE", "PRECISION", "COMPLEX", "LOGICAL",
                "CHARACTER", "DIMENSION", "COMMON", "EQUIVALENCE", "EXTERNAL",
                "IF", "THEN", "ELSE", "ELSEIF", "ENDIF", "DO", "GOTO", "CALL",
                "SUBROUTINE", "FUNCTION", "WRITE", "READ", "PRINT", "FORMAT",
                "OPEN", "CLOSE", "INQUIRE", "REWIND", "BACKSPACE", "ENDFILE",
                ".TRUE.", ".FALSE.", ".AND.", ".OR.", ".NOT.", ".EQ.", ".NE.",
                ".LT.", ".GT.", ".LE.", ".GE.",
                "SQRT", "ABS", "INT", "REAL", "SIN", "COS", "TAN", "EXP", "LOG",
            ]
            self.comment_pattern = r"^[Cc*].*$"
            self.string_pattern = r"'[^']*'"
            self.number_pattern = r"\b\d+\.?\d*(?:[EeDd][+-]?\d+)?\b"
            self.operator_pattern = r"[+\-*/=()]"
            self.function_pattern = r"\bSUBROUTINE\s+(\w+)|\bFUNCTION\s+(\w+)"
            self.variable_pattern = r"\b[A-Za-z][A-Za-z0-9]{0,5}\b"

        elif language == Language.REXX:
            self.keywords = [
                "SAY", "EXIT", "RETURN", "CALL", "SIGNAL", "PROCEDURE",
                "PARSE", "PULL", "ARG", "VAR", "VALUE", "UPPER", "NUMERIC",
                "IF", "THEN", "ELSE", "END", "DO", "LOOP", "WHILE", "UNTIL",
                "TO", "BY", "FOR", "FOREVER", "LEAVE", "ITERATE",
                "SELECT", "WHEN", "OTHERWISE", "ADDRESS", "INTERPRET",
                "DROP", "EXPOSE", "NOP",
                "LENGTH", "SUBSTR", "LEFT", "RIGHT", "STRIP", "UPPER", "LOWER",
                "REVERSE", "POS", "WORD", "WORDS", "COPIES", "SPACE",
                "ABS", "MAX", "MIN", "SIGN", "TRUNC",
            ]
            self.comment_pattern = r"/\*[\s\S]*?\*/"
            self.string_pattern = r'"[^"]*"|\'[^\']*\''
            self.number_pattern = r"\b\d+\.?\d*(?:[eE][+-]?\d+)?\b"
            self.operator_pattern = r"[+\-*/=<>|&//\\]"
            self.function_pattern = r"^([A-Za-z_][A-Za-z0-9_]*)\s*:"
            self.variable_pattern = r"\b[A-Za-z_][A-Za-z0-9_.]*\b"

        elif language == Language.SMALLTALK:
            self.keywords = [
                "self", "super", "true", "false", "nil", "thisContext",
                "ifTrue:", "ifFalse:", "ifTrue:ifFalse:", "whileTrue:", "whileFalse:",
                "timesRepeat:", "to:do:", "to:by:do:", "do:", "collect:",
                "select:", "reject:", "detect:", "inject:into:",
                "Transcript", "OrderedCollection", "Dictionary", "Array",
                "printString", "printNl", "yourself", "new", "class",
                "isNil", "notNil", "isKindOf:", "respondsTo:", "perform:",
                "at:", "at:put:", "size", "isEmpty", "add:", "remove:",
                "value", "value:", "numArgs",
            ]
            self.comment_pattern = r'"[^"]*"'
            self.string_pattern = r"'[^']*'"
            self.number_pattern = r"\b\d+\.?\d*(?:[eE][+-]?\d+)?\b|\b[0-9a-fA-F]+r[0-9a-fA-F]+\b"
            self.operator_pattern = r"[+\-*/=<>~@,;|&^!?:.]"
            self.function_pattern = r"\b([A-Za-z][A-Za-z0-9]*:)\s"
            self.variable_pattern = r"\b[A-Za-z][A-Za-z0-9]*\b"

        elif language == Language.HYPERTALK:
            self.keywords = [
                "put", "into", "after", "before", "get", "set", "to",
                "if", "then", "else", "end", "repeat", "with", "while", "until",
                "forever", "times", "from",
                "on", "return", "exit", "pass", "send",
                "answer", "ask", "say",
                "global", "local",
                "char", "character", "word", "line", "item",
                "the", "of", "in", "number", "it",
                "true", "false", "empty", "space", "tab", "return", "cr",
                "and", "or", "not", "is", "contains",
            ]
            self.comment_pattern = r"--.*$"
            self.string_pattern = r'"[^"]*"'
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[+\-*/=<>&]"
            self.function_pattern = r"^on\s+(\w+)"
            self.variable_pattern = r"\b[A-Za-z][A-Za-z0-9]*\b"

        elif language == Language.HASKELL:
            self.keywords = [
                "where", "let", "in", "do", "if", "then", "else", "case", "of",
                "data", "type", "newtype", "class", "instance", "module",
                "import", "qualified", "as", "hiding", "deriving",
                "forall", "infixl", "infixr", "infix",
                "main", "IO", "Int", "Integer", "Float", "Double", "Bool", "Char",
                "String", "Maybe", "Either", "List", "Map", "Set",
                "True", "False", "Nothing", "Just", "Left", "Right",
                "return", "putStr", "putStrLn", "print", "getLine", "getContents",
                "show", "read", "length", "null", "head", "tail", "map", "filter",
                "foldr", "foldl", "zip", "zipWith", "concat", "concatMap",
                "take", "drop", "reverse", "sum", "product", "maximum", "minimum",
                "elem", "notElem", "all", "any", "and", "or",
            ]
            self.comment_pattern = r"--.*$|\{-[\s\S]*?-\}"
            self.string_pattern = r'"[^"\\]*(?:\\.[^"\\]*)*"'
            self.number_pattern = r"\b0[xX][0-9a-fA-F]+\b|\b\d+\.?\d*(?:[eE][+-]?\d+)?\b"
            self.operator_pattern = r"[+\-*/=<>!&|%^~@:.]"
            self.function_pattern = r"^([a-z][A-Za-z0-9_']*)\s+"
            self.variable_pattern = r"\b[a-zA-Z][A-Za-z0-9_']*\b"

        elif language == Language.APL:
            self.keywords = []   # APL uses symbols, not word keywords
            self.comment_pattern = r"⍝.*$"
            self.string_pattern = r"'[^']*'"
            self.number_pattern = r"¯?\d+\.?\d*(?:[eE][+-]?\d+)?\b"
            self.operator_pattern = r"[⍳⍴⌽⍉⌈⌊⍋⍒∊≡≢←⎕↑↓,/\\⌿+\-×÷*!=<>≤≥≠&|~?]"
            self.function_pattern = r"\b(\w+)\s*←\s*\{"
            self.variable_pattern = r"\b[A-Za-z_][A-Za-z0-9_]*\b"

        elif language == Language.SQL:
            self.keywords = [
                "SELECT", "FROM", "WHERE", "JOIN", "INNER", "LEFT", "RIGHT", "FULL",
                "OUTER", "ON", "AS", "INSERT", "INTO", "VALUES", "UPDATE", "SET",
                "DELETE", "CREATE", "DROP", "ALTER", "TABLE", "VIEW", "DATABASE",
                "INDEX", "PROCEDURE", "FUNCTION", "TRIGGER", "EXEC", "EXECUTE",
                "DECLARE", "BEGIN", "END", "COMMIT", "ROLLBACK", "TRANSACTION",
                "IF", "ELSE", "WHILE", "RETURN", "PRINT", "GO", "USE",
                "EXISTS", "NOT", "AND", "OR", "IN", "LIKE", "BETWEEN", "IS",
                "NULL", "TOP", "ORDER", "BY", "GROUP", "HAVING", "DISTINCT",
                "UNION", "ALL", "CASE", "WHEN", "THEN", "WITH",
                "PRIMARY", "FOREIGN", "KEY", "REFERENCES", "CONSTRAINT",
                "IDENTITY", "DEFAULT", "UNIQUE", "CHECK", "CASCADE",
            ]
            self.comment_pattern = r"--.*$|/\*.*?\*/"
            self.string_pattern = r"'(?:[^']|'')*'"
            self.number_pattern = r"\b\d+\.?\d*\b"
            self.operator_pattern = r"[@=<>!+\-*/]+"
            self.function_pattern = r"\b(COUNT|SUM|AVG|MIN|MAX|LEN|ISNULL|GETDATE|CONVERT|CAST|COALESCE|NULLIF|CHARINDEX|SUBSTRING|UPPER|LOWER|LTRIM|RTRIM|REPLACE)\s*\("
            self.variable_pattern = r"@{1,2}[A-Za-z_][A-Za-z0-9_]*"

        elif language == Language.JCL:
            self.keywords = [
                "JOB", "EXEC", "DD", "PROC", "PEND", "SET",
                "PGM", "PROC", "COND", "REGION", "TIME", "CLASS", "MSGCLASS",
                "MSGLEVEL", "NOTIFY", "UNIT", "SPACE", "DCB", "DISP",
                "DSN", "DSNAME", "SYSOUT", "DUMMY", "BLKSIZE", "RECFM",
                "LRECL", "TRACKS", "CYLINDERS", "TRK", "CYL",
                "OLD", "NEW", "SHR", "MOD", "CATLG", "DELETE", "KEEP", "PASS",
            ]
            self.comment_pattern = r"//\*.*$"
            self.string_pattern = r"'[^']*'"
            self.number_pattern = r"\b\d+\b"
            self.operator_pattern = r"[=,()]"
            self.function_pattern = r"^//\S+"
            self.variable_pattern = r"\b[A-Z#@$][A-Z0-9#@$]{0,7}\b"

        elif language == Language.CICS:
            self.keywords = [
                "EXEC", "CICS", "END-EXEC",
                "SEND", "RECEIVE", "READ", "READNEXT", "WRITE", "REWRITE",
                "DELETE", "BROWSE", "STARTBR", "READNEXT", "ENDBR",
                "LINK", "XCTL", "RETURN", "ABEND", "HANDLE",
                "CONDITION", "IGNORE", "ASSIGN", "GETMAIN", "FREEMAIN",
                "ENQ", "DEQ", "SYNCPOINT", "DELAY", "ASKTIME",
                "FORMATTIME", "RETRIEVE", "SET", "INQUIRE", "PUT",
                "TEXT", "MAP", "MAPSET", "TERMINAL", "INTO", "FROM",
                "LENGTH", "TRANSID", "COMMAREA", "FLENGTH", "DATASET",
                "RIDFLD", "KEYLENGTH", "GENERIC", "NOTFND", "DUPKEY",
                "ENDFILE", "ERROR", "PGMIDERR", "INVREQ",
                # COBOL verbs
                "MOVE", "DISPLAY", "PERFORM", "IF", "ELSE", "END-IF",
                "STOP", "RUN", "GOBACK", "COMPUTE", "ADD", "SUBTRACT",
                "MULTIPLY", "DIVIDE", "INITIALIZE", "EVALUATE",
            ]
            self.comment_pattern = r"\*.*$"
            self.string_pattern = r"'[^']*'"
            self.number_pattern = r"\b\d+\b"
            self.operator_pattern = r"[=<>()+\-*/]"
            self.function_pattern = r"\bEXEC\s+CICS\b"
            self.variable_pattern = r"\b[A-Z][A-Z0-9-]*\b"

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
    """Find & Replace dialog."""

    def __init__(self, parent):
        super().__init__(parent)
        self.editor = parent
        self.setup_ui()

    def setup_ui(self):
        """Setup dialog UI."""
        self.setWindowTitle("Find & Replace")
        layout = QVBoxLayout()

        # Find field
        search_layout = QHBoxLayout()
        search_layout.addWidget(QLabel("Find:   "))
        self.search_field = QLineEdit()
        self.search_field.returnPressed.connect(self.find_next)
        search_layout.addWidget(self.search_field)
        layout.addLayout(search_layout)

        # Replace field
        replace_layout = QHBoxLayout()
        replace_layout.addWidget(QLabel("Replace:"))
        self.replace_field = QLineEdit()
        replace_layout.addWidget(self.replace_field)
        layout.addLayout(replace_layout)

        # Find buttons
        find_btn_layout = QHBoxLayout()
        find_next_btn = QPushButton("Find Next")
        find_next_btn.clicked.connect(self.find_next)
        find_btn_layout.addWidget(find_next_btn)

        find_prev_btn = QPushButton("Find Previous")
        find_prev_btn.clicked.connect(self.find_previous)
        find_btn_layout.addWidget(find_prev_btn)
        layout.addLayout(find_btn_layout)

        # Replace buttons
        replace_btn_layout = QHBoxLayout()
        replace_btn = QPushButton("Replace")
        replace_btn.clicked.connect(self.replace_one)
        replace_btn_layout.addWidget(replace_btn)

        replace_all_btn = QPushButton("Replace All")
        replace_all_btn.clicked.connect(self.replace_all)
        replace_btn_layout.addWidget(replace_all_btn)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        replace_btn_layout.addWidget(close_btn)
        layout.addLayout(replace_btn_layout)

        self.setLayout(layout)
        self.setMinimumWidth(380)

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

    def replace_one(self):
        """Replace current selection if it matches, then find next."""
        find_text = self.search_field.text()
        replace_text = self.replace_field.text()
        if not find_text:
            return
        cursor = self.editor.textCursor()
        if cursor.hasSelection() and cursor.selectedText() == find_text:
            cursor.insertText(replace_text)
            self.editor.setTextCursor(cursor)
        self.editor.find(find_text)

    def replace_all(self):
        """Replace all occurrences; returns count replaced."""
        find_text = self.search_field.text()
        replace_text = self.replace_field.text()
        if not find_text:
            return 0
        doc = self.editor.document()
        cursor = QTextCursor(doc)
        cursor.beginEditBlock()
        count = 0
        while True:
            cursor = doc.find(find_text, cursor)
            if cursor.isNull():
                break
            cursor.insertText(replace_text)
            count += 1
        cursor.endEditBlock()
        return count


class CodeEditor(QPlainTextEdit):
    """Code editor with line numbers and breakpoint support.

    This widget exposes Qt/QtWidgets API methods using camelCase names
    (e.g. keyPressEvent, resizeEvent) and maintains multiple runtime
    attributes for UI state. Suppress those specific lint checks here.
    """

    # Qt naming and UI statefulness — disable relevant style checks.
    # pylint: disable=too-many-instance-attributes,invalid-name

    # Signal emitted when breakpoint is toggled (line_number)
    breakpoint_toggled = Signal(int)
    # Signal emitted when cursor moves: (line, col) 1-based
    cursor_line_col_changed = Signal(int, int)

    def __init__(self, parent=None):
        super().__init__(parent)

        # Breakpoints set (line numbers with breakpoints)
        self._breakpoints = set()

        # Error lines set (line numbers with errors)
        self._error_lines = set()

        # Current execution line (for debugging highlight)
        self._current_line = 0

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
        self.cursorPositionChanged.connect(self._on_cursor_position_changed)
        self.cursorPositionChanged.connect(self._update_bracket_match)

        self.update_line_number_area_width(0)

        # Highlighting features (defaults off)
        self._highlight_current_line = False
        self._whitespace_highlighter = None

    def show_find_dialog(self):
        """Show the find/replace dialog."""
        dialog = FindDialog(self)
        dialog.show()

    def _on_cursor_position_changed(self):
        """Emit cursor line/col for status bar updates."""
        cursor = self.textCursor()
        self.cursor_line_col_changed.emit(
            cursor.blockNumber() + 1,
            cursor.columnNumber() + 1,
        )

    def _update_bracket_match(self):
        """Highlight matching bracket pair around cursor."""
        fmt = QTextCharFormat()
        fmt.setBackground(QColor(80, 120, 80, 120))
        fmt.setForeground(QColor(100, 255, 100))

        # Remove previous bracket selections (keep others)
        selections = [
            s for s in self.extraSelections()
            if getattr(s, "_bracket_match", False) is False
        ]

        cursor = self.textCursor()
        doc = self.document()
        text = doc.toPlainText()
        pos = cursor.position()

        pairs_fwd = {"(": ")", "[": "]", "{": "}"}
        pairs_bwd = {v: k for k, v in pairs_fwd.items()}

        char_at = text[pos] if pos < len(text) else ""
        char_before = text[pos - 1] if pos > 0 else ""

        our_pos = match_pos = None
        if char_at in pairs_fwd:
            our_pos = pos
            match_pos = self._find_matching_bracket(text, pos, pairs_fwd[char_at], True)
        elif char_before in pairs_fwd:
            our_pos = pos - 1
            match_pos = self._find_matching_bracket(text, pos - 1, pairs_fwd[char_before], True)
        elif char_at in pairs_bwd:
            our_pos = pos
            match_pos = self._find_matching_bracket(text, pos, pairs_bwd[char_at], False)
        elif char_before in pairs_bwd:
            our_pos = pos - 1
            match_pos = self._find_matching_bracket(text, pos - 1, pairs_bwd[char_before], False)

        if our_pos is not None and match_pos is not None:
            for p in (our_pos, match_pos):
                sel = QTextEdit.ExtraSelection()
                c = QTextCursor(doc)
                c.setPosition(p)
                c.movePosition(QTextCursor.MoveOperation.NextCharacter, QTextCursor.MoveMode.KeepAnchor)
                sel.cursor = c
                sel.format = fmt
                sel._bracket_match = True  # type: ignore[attr-defined]
                selections.append(sel)

        self.setExtraSelections(selections)

    def _find_matching_bracket(self, text: str, pos: int, close_char: str, forward: bool):
        """Find the position of the bracket that matches the one at pos.

        Args:
            text: Full document text.
            pos: Position of the opening/closing bracket.
            close_char: The character we are searching for as the match.
            forward: True to search forward, False to search backward.

        Returns:
            Index into text of the matching bracket, or None.
        """
        open_char = text[pos]
        depth = 0
        if forward:
            for i in range(pos, len(text)):
                ch = text[i]
                if ch == open_char:
                    depth += 1
                elif ch == close_char:
                    depth -= 1
                    if depth == 0:
                        return i
        else:
            for i in range(pos, -1, -1):
                ch = text[i]
                if ch == open_char:
                    depth += 1
                elif ch == close_char:
                    depth -= 1
                    if depth == 0:
                        return i
        return None

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
        """Calculate width of line number area (includes breakpoint gutter)."""
        digits = len(str(max(1, self.blockCount())))
        # Add 20px for breakpoint markers
        space = 20 + 3 + self.fontMetrics().horizontalAdvance("9") * digits
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
        """Paint line numbers with breakpoint markers and current line."""
        painter = QPainter(self.line_number_area)

        # Background
        palette = self.palette()
        bg_color = palette.color(QPalette.Window).darker(110)
        painter.fillRect(event.rect(), bg_color)

        # Line numbers and breakpoints
        block = self.document().firstBlock()
        geom = self.blockBoundingGeometry(block)
        top = geom.translated(self.contentOffset()).top()
        block_number = block.blockNumber()
        geom = self.blockBoundingGeometry(block)
        top = geom.translated(self.contentOffset()).top()
        bottom = top + self.blockBoundingRect(block).height()

        fg_color = palette.color(QPalette.Text).darker(150)
        bp_color = QColor(255, 80, 80)  # Red for breakpoints
        error_color = QColor(255, 100, 100, 80)  # Red tint for error lines
        current_line_color = QColor(255, 255, 0)  # Yellow for current line

        while block.isValid() and top <= event.rect().bottom():
            if block.isVisible() and bottom >= event.rect().top():
                line_num = block_number + 1
                font_height = self.fontMetrics().height()

                # Draw error line background
                if line_num in self._error_lines:
                    painter.fillRect(
                        0,
                        int(top),
                        self.line_number_area.width(),
                        font_height,
                        error_color,
                    )

                # Draw breakpoint marker if set
                if line_num in self._breakpoints:
                    painter.setBrush(QBrush(bp_color))
                    painter.setPen(QPen(bp_color.darker(120)))
                    # Draw circle for breakpoint
                    circle_size = min(14, font_height - 2)
                    circle_x = 3
                    circle_y = int(top) + (font_height - circle_size) // 2
                    painter.drawEllipse(circle_x, circle_y, circle_size, circle_size)

                # Draw current execution line indicator
                if line_num == self._current_line:
                    painter.setBrush(QBrush(current_line_color))
                    painter.setPen(QPen(current_line_color.darker(120)))
                    # Draw arrow for current line
                    arrow_x = 4
                    arrow_y = int(top) + font_height // 2
                    arrow_size = 6
                    points = [
                        (arrow_x, arrow_y - arrow_size // 2),
                        (arrow_x + arrow_size, arrow_y),
                        (arrow_x, arrow_y + arrow_size // 2),
                    ]
                    polygon = QPolygon([QPoint(x, y) for x, y in points])
                    painter.drawPolygon(polygon)

                # Draw line number (offset for breakpoint gutter)
                number = str(line_num)
                # Use red text for error lines
                if line_num in self._error_lines:
                    painter.setPen(QColor(255, 80, 80))
                else:
                    painter.setPen(fg_color)
                painter.drawText(
                    20,  # Offset for breakpoint gutter
                    int(top),
                    self.line_number_area.width() - 25,
                    font_height,
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

    # ---- Breakpoint Management ----

    def toggle_breakpoint(self, line: int):
        """Toggle a breakpoint at the specified line."""
        if line in self._breakpoints:
            self._breakpoints.remove(line)
        else:
            self._breakpoints.add(line)
        self.line_number_area.update()
        self.breakpoint_toggled.emit(line)

    def add_breakpoint(self, line: int):
        """Add a breakpoint at the specified line."""
        if line not in self._breakpoints:
            self._breakpoints.add(line)
            self.line_number_area.update()
            self.breakpoint_toggled.emit(line)

    def remove_breakpoint(self, line: int):
        """Remove a breakpoint from the specified line."""
        if line in self._breakpoints:
            self._breakpoints.remove(line)
            self.line_number_area.update()
            self.breakpoint_toggled.emit(line)

    def clear_breakpoints(self):
        """Clear all breakpoints."""
        self._breakpoints.clear()
        self.line_number_area.update()

    def get_breakpoints(self) -> set:
        """Get the set of breakpoint line numbers."""
        return self._breakpoints.copy()

    def set_breakpoints(self, breakpoints: set):
        """Set breakpoints from a set of line numbers."""
        self._breakpoints = set(breakpoints)
        self.line_number_area.update()

    def has_breakpoint(self, line: int) -> bool:
        """Check if a line has a breakpoint."""
        return line in self._breakpoints

    # ---- Execution Line Indicator ----

    def set_current_line(self, line: int):
        """Set the current execution line (shown with yellow arrow)."""
        self._current_line = line
        self.line_number_area.update()

        # Scroll to make the current line visible
        if line > 0:
            self.goto_line(line)

    def clear_current_line(self):
        """Clear the current execution line indicator."""
        self._current_line = 0
        self.line_number_area.update()

    # ---- Error Line Management ----

    def set_error_line(self, line: int):
        """Mark a line as having an error."""
        if line not in self._error_lines:
            self._error_lines.add(line)
            self.line_number_area.update()

    def clear_error_line(self, line: int):
        """Remove error mark from a line."""
        if line in self._error_lines:
            self._error_lines.remove(line)
            self.line_number_area.update()

    def clear_error_lines(self):
        """Clear all error line marks."""
        self._error_lines.clear()
        self.line_number_area.update()

    def set_error_lines(self, lines: set):
        """Set error lines from a set of line numbers."""
        self._error_lines = set(lines)
        self.line_number_area.update()

    def get_error_lines(self) -> set:
        """Get the set of error line numbers."""
        return self._error_lines.copy()

    def has_error(self, line: int) -> bool:
        """Check if a line has an error."""
        return line in self._error_lines

    def mark_errors_from_output(self, output: str):
        """Parse output for error messages and mark lines.

        Looks for patterns like 'Error at line N:' in the output.
        """
        self._error_lines.clear()
        # Match patterns like "line 5:" or "at line 10:"
        pattern = r"(?:at\s+)?line\s+(\d+)"
        for match in re.finditer(pattern, output, re.IGNORECASE):
            try:
                line_num = int(match.group(1))
                self._error_lines.add(line_num)
            except ValueError:
                pass
        self.line_number_area.update()

    def goto_line(self, line: int):
        """Scroll to and highlight the specified line."""
        block = self.document().findBlockByLineNumber(line - 1)
        if block.isValid():
            cursor = QTextCursor(block)
            self.setTextCursor(cursor)
            self.centerCursor()

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
        """Handle key press events for auto-completion and auto-indent."""
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

        # Handle Enter/Return for auto-indent
        if event.key() in (Qt.Key_Enter, Qt.Key_Return):
            self._handle_auto_indent()
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

    def _handle_auto_indent(self):
        """Handle auto-indentation on Enter/Return key."""
        cursor = self.textCursor()

        # Get current line text
        cursor.select(QTextCursor.LineUnderCursor)
        current_line = cursor.selectedText()
        cursor.clearSelection()

        # Get leading whitespace from current line
        leading_spaces = ""
        for char in current_line:
            if char in (" ", "\t"):
                leading_spaces += char
            else:
                break

        # Check if we should increase indent (line ends with block opener)
        line_upper = current_line.strip().upper()

        # BASIC indent triggers
        increase_indent = False
        if line_upper.startswith("FOR ") and " NEXT" not in line_upper:
            increase_indent = True
        elif line_upper.startswith("WHILE ") and " WEND" not in line_upper:
            increase_indent = True
        elif line_upper.startswith("DO"):
            increase_indent = True
        elif " THEN" in line_upper and not any(
            keyword in line_upper
            for keyword in ("GOTO", "GOSUB", "END", "PRINT", "LET")
        ):
            # Multi-line IF (THEN without action on same line)
            if line_upper.endswith("THEN"):
                increase_indent = True
        elif line_upper.startswith("SUB "):
            increase_indent = True
        elif line_upper.startswith("FUNCTION "):
            increase_indent = True
        elif line_upper.startswith("SELECT "):
            increase_indent = True
        elif line_upper.startswith("CASE "):
            increase_indent = True

        # PILOT indent triggers
        if line_upper.startswith("*") and ":" not in line_upper:
            # Label - don't indent
            pass

        # Python/general indent: any line ending with ':' increases indent
        if line_upper.endswith(":"):
            increase_indent = True

        # Logo indent triggers
        if line_upper.startswith("TO "):
            increase_indent = True
        elif "[" in line_upper:
            # Count brackets to see if we should indent
            open_count = line_upper.count("[")
            close_count = line_upper.count("]")
            if open_count > close_count:
                increase_indent = True

        # Check if we should decrease indent
        decrease_indent = False
        if line_upper in (
            "NEXT",
            "WEND",
            "END",
            "END SUB",
            "END FUNCTION",
            "END SELECT",
            "LOOP",
            "ELSE",
            "ELSEIF",
        ):
            decrease_indent = True
        elif line_upper.startswith("NEXT "):
            decrease_indent = True
        elif line_upper.startswith("LOOP "):
            decrease_indent = True

        # Insert newline with proper indentation
        cursor = self.textCursor()
        cursor.movePosition(QTextCursor.EndOfLine)

        if increase_indent:
            cursor.insertText("\n" + leading_spaces + "  ")
        elif decrease_indent and len(leading_spaces) >= 2:
            cursor.insertText("\n" + leading_spaces[:-2])
        else:
            cursor.insertText("\n" + leading_spaces)

        self.setTextCursor(cursor)
