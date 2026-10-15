"""
Code formatter for Time Warp Studio.
Auto-formats BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Brainfuck, and Python code.
"""

import re
from typing import Tuple


class BasicFormatter:
    """Formatter for BASIC code."""

    # Keywords that increase indentation on the next line
    INDENT_KEYWORDS = [
        "FOR",
        "WHILE",
        "DO",
        "SUB",
        "FUNCTION",
        "IF",
        "SELECT",
        "CASE",
    ]

    # Keywords that decrease indentation
    DEDENT_KEYWORDS = [
        "NEXT",
        "WEND",
        "LOOP",
        "END SUB",
        "END FUNCTION",
        "END IF",
        "END SELECT",
        "ELSE",
        "ELSEIF",
    ]

    # Keywords that both dedent and indent (same line)
    DEDENT_INDENT_KEYWORDS = ["ELSE", "ELSEIF", "CASE"]

    def format_code(self, code: str, indent_size: int = 4) -> str:
        """Format BASIC code with proper indentation.

        Args:
            code: The BASIC source code
            indent_size: Number of spaces per indent level

        Returns:
            Formatted code string
        """
        lines = code.split("\n")
        result = []
        indent_level = 0
        indent_str = " " * indent_size

        for line in lines:
            stripped = line.strip()
            if not stripped:
                result.append("")
                continue

            # Extract line number if present
            line_num = ""
            content = stripped
            match = re.match(r"^(\d+)\s+(.*)$", stripped)
            if match:
                line_num = match.group(1) + " "
                content = match.group(2)

            upper_content = content.upper()

            # Check for dedent keywords first
            should_dedent = False
            for kw in self.DEDENT_KEYWORDS:
                if upper_content.startswith(kw):
                    should_dedent = True
                    break

            # Apply dedent before this line
            if should_dedent and indent_level > 0:
                indent_level -= 1

            # Format the line
            formatted = line_num + indent_str * indent_level + content
            result.append(formatted)

            # Check if line ends in THEN without a statement after
            # (multi-line IF)
            if upper_content.endswith(" THEN") or upper_content == "THEN":
                indent_level += 1
                continue

            # Check for indent keywords for next line
            should_indent = False
            for kw in self.INDENT_KEYWORDS:
                if upper_content.startswith(kw + " ") or upper_content == kw:
                    # Don't indent if it's a single-line IF with THEN action
                    if kw == "IF" and " THEN " in upper_content:
                        after_then = upper_content.split(" THEN ", 1)[1]
                        if after_then and not after_then.isdigit():
                            continue  # Single-line IF, no indent
                    should_indent = True
                    break

            if should_indent:
                indent_level += 1

            # Handle CASE which dedents then indents
            if upper_content.startswith("CASE ") and upper_content != "CASE ELSE":
                # Already dedented above, now indent for case body
                indent_level += 1

        return "\n".join(result)

    def normalize_keywords(self, code: str) -> str:
        """Normalize BASIC keywords to uppercase."""
        keywords = [
            "PRINT",
            "INPUT",
            "LET",
            "IF",
            "THEN",
            "ELSE",
            "ELSEIF",
            "END IF",
            "FOR",
            "TO",
            "STEP",
            "NEXT",
            "WHILE",
            "WEND",
            "DO",
            "LOOP",
            "UNTIL",
            "GOTO",
            "GOSUB",
            "RETURN",
            "REM",
            "DIM",
            "SUB",
            "END SUB",
            "FUNCTION",
            "END FUNCTION",
            "CALL",
            "SELECT",
            "CASE",
            "END SELECT",
            "AND",
            "OR",
            "NOT",
            "MOD",
            "CLS",
            "END",
            "RUN",
            "DATA",
            "READ",
            "RESTORE",
            "SCREEN",
            "COLOR",
            "LOCATE",
            "LINE",
            "CIRCLE",
            "BEEP",
            "SOUND",
            "PLAY",
            "TIMER",
            "ON",
            "INKEY$",
            "TIME$",
            "DATE$",
        ]

        result = code
        for kw in keywords:
            # Match keyword as whole word, case insensitive
            pattern = r"\b" + re.escape(kw) + r"\b"
            result = re.sub(pattern, kw, result, flags=re.IGNORECASE)

        return result


class LogoFormatter:
    """Formatter for Logo code."""

    # Commands that start blocks
    BLOCK_START = ["TO", "IF", "IFELSE", "REPEAT", "FOREVER", "WHILE"]

    # Commands that end blocks
    BLOCK_END = ["END"]

    def format_code(self, code: str, indent_size: int = 2) -> str:
        """Format Logo code with proper indentation.

        Args:
            code: The Logo source code
            indent_size: Number of spaces per indent level

        Returns:
            Formatted code string
        """
        lines = code.split("\n")
        result = []
        indent_level = 0
        indent_str = " " * indent_size

        for line in lines:
            stripped = line.strip()
            if not stripped:
                result.append("")
                continue

            upper = stripped.upper()

            # Check for END first
            if upper == "END" or upper.startswith("END "):
                if indent_level > 0:
                    indent_level -= 1
                result.append(indent_str * indent_level + stripped)
                continue

            # Format the line
            result.append(indent_str * indent_level + stripped)

            # Check for block start
            for kw in self.BLOCK_START:
                if upper.startswith(kw + " ") or upper == kw:
                    indent_level += 1
                    break

            # Handle bracket blocks [ ]
            open_brackets = stripped.count("[")
            close_brackets = stripped.count("]")
            indent_level += open_brackets - close_brackets
            indent_level = max(0, indent_level)

        return "\n".join(result)

    def normalize_keywords(self, code: str) -> str:
        """Normalize Logo keywords to lowercase (Logo convention)."""
        keywords = [
            "to",
            "end",
            "if",
            "ifelse",
            "repeat",
            "forever",
            "while",
            "forward",
            "fd",
            "backward",
            "bk",
            "right",
            "rt",
            "left",
            "lt",
            "penup",
            "pu",
            "pendown",
            "pd",
            "setxy",
            "setx",
            "sety",
            "setheading",
            "seth",
            "home",
            "clearscreen",
            "cs",
            "hideturtle",
            "ht",
            "showturtle",
            "st",
            "setpencolor",
            "setpc",
            "setpensize",
            "setps",
            "make",
            "local",
            "output",
            "stop",
            "print",
            "show",
            "random",
            "sqrt",
            "sin",
            "cos",
            "abs",
            "int",
            "round",
        ]

        result = code
        for kw in keywords:
            pattern = r"\b" + re.escape(kw) + r"\b"
            result = re.sub(pattern, kw.lower(), result, flags=re.IGNORECASE)

        return result


class PilotFormatter:
    """Formatter for PILOT code."""

    # pylint: disable=unused-argument
    def format_code(self, code: str, indent_size: int = 2) -> str:
        """Format PILOT code.

        PILOT uses single-letter commands, so formatting is mainly
        about alignment and spacing.
        """
        lines = code.split("\n")
        result = []

        for line in lines:
            stripped = line.strip()
            if not stripped:
                result.append("")
                continue

            # PILOT format: [label:] command [condition]: arguments
            # Ensure space after command letter
            if len(stripped) >= 2 and stripped[0].isalpha():
                if stripped[1] == ":":
                    # Command with colon, ensure space if needed
                    result.append(stripped)
                elif stripped[1].isalpha():
                    # Might be a label or multi-char command
                    result.append(stripped)
                else:
                    result.append(stripped)
            else:
                result.append(stripped)

        return "\n".join(result)

    def normalize_commands(self, code: str) -> str:
        """Normalize PILOT commands to uppercase."""
        lines = code.split("\n")
        result = []

        for line in lines:
            stripped = line.strip()
            if not stripped:
                result.append("")
                continue

            # PILOT commands are single letters at start
            if stripped and stripped[0].isalpha():
                # Check for label (ends with :)
                if ":" in stripped:
                    colon_pos = stripped.index(":")
                    before = stripped[:colon_pos]
                    after = stripped[colon_pos:]
                    if len(before) == 1:
                        # Single letter command
                        result.append(before.upper() + after)
                    else:
                        result.append(stripped)
                else:
                    result.append(stripped)
            else:
                result.append(stripped)

        return "\n".join(result)


class CFormatter:
    """Formatter for C code (`{}` block indentation)."""

    def format_code(self, code: str, indent_size: int = 4) -> str:
        lines = code.split("\n")
        result = []
        indent = 0
        indent_str = " " * indent_size
        for line in lines:
            stripped = line.strip()
            if not stripped:
                result.append("")
                continue
            if stripped.startswith("#"):
                result.append(stripped)
                continue
            if stripped.startswith("}"):
                indent = max(0, indent - 1)
            result.append(indent_str * indent + stripped)
            open_braces = stripped.count("{")
            close_braces = stripped.count("}")
            if stripped.startswith("}"):
                indent = max(0, indent + open_braces - (close_braces - 1))
            else:
                indent = max(0, indent + open_braces - close_braces)
        return "\n".join(result)


class PascalFormatter:
    """Formatter for Pascal code (`begin`/`end` block indentation and keyword casing)."""

    KEYWORDS = [
        "program",
        "uses",
        "var",
        "const",
        "type",
        "record",
        "array",
        "of",
        "procedure",
        "function",
        "begin",
        "end",
        "if",
        "then",
        "else",
        "for",
        "to",
        "downto",
        "do",
        "while",
        "repeat",
        "until",
        "case",
        "div",
        "mod",
        "and",
        "or",
        "not",
    ]

    def format_code(self, code: str, indent_size: int = 2) -> str:
        lines = code.split("\n")
        result = []
        indent = 0
        indent_str = " " * indent_size
        for line in lines:
            stripped = line.strip()
            if not stripped:
                result.append("")
                continue
            low = stripped.lower().rstrip(";.")
            if low in ("end", "until") or low.startswith("until "):
                indent = max(0, indent - 1)
            result.append(indent_str * indent + stripped)
            if (
                low in ("begin", "repeat", "record")
                or low.endswith(" begin")
                or low.startswith("case ")
            ):
                indent += 1
        return "\n".join(result)

    def normalize_keywords(self, code: str) -> str:
        result = code
        for kw in self.KEYWORDS:
            result = re.sub(r"\b" + re.escape(kw) + r"\b", kw, result, flags=re.IGNORECASE)
        return result


class PrologFormatter:
    """Formatter for Prolog facts, rules, and queries."""

    def format_code(self, code: str, indent_size: int = 4) -> str:
        lines = code.split("\n")
        result = []
        indent_str = " " * indent_size
        in_rule_body = False
        for line in lines:
            stripped = line.strip()
            if not stripped:
                result.append("")
                in_rule_body = False
                continue
            if stripped.startswith("%"):
                result.append(stripped)
                continue
            if in_rule_body:
                result.append(indent_str + stripped)
            else:
                result.append(stripped)
            if ":-" in stripped and not stripped.endswith("."):
                in_rule_body = True
            elif stripped.endswith("."):
                in_rule_body = False
        return "\n".join(result)


class ForthFormatter:
    """Formatter for Forth colon definitions and stack words."""

    KEYWORDS = [
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
        "+LOOP",
        "BEGIN",
        "UNTIL",
        "WHILE",
        "REPEAT",
        "AGAIN",
        "VARIABLE",
        "CONSTANT",
        "VALUE",
        "ALLOT",
        "HERE",
        "CR",
        "EMIT",
        "INCLUDE",
    ]

    def format_code(self, code: str, indent_size: int = 2) -> str:
        lines = code.split("\n")
        result = []
        in_def = False
        indent_str = " " * indent_size
        for line in lines:
            stripped = " ".join(line.strip().split())
            if not stripped:
                result.append("")
                continue
            tokens = stripped.split()
            if tokens[0] == ":":
                result.append(stripped)
                in_def = not stripped.endswith(";")
                continue
            if tokens[0] == ";":
                in_def = False
                result.append(stripped)
                continue
            result.append((indent_str if in_def else "") + stripped)
            if stripped.endswith(";"):
                in_def = False
        return "\n".join(result)

    def normalize_keywords(self, code: str) -> str:
        result = code
        for kw in self.KEYWORDS:
            result = re.sub(r"\b" + re.escape(kw) + r"\b", kw, result, flags=re.IGNORECASE)
        return result


class BrainfuckFormatter:
    """Formatter for Brainfuck code."""

    def format_code(self, code: str, indent_size: int = 2) -> str:
        lines = [ln.rstrip() for ln in code.split("\n")]
        return "\n".join(lines).strip() + ("\n" if code.endswith("\n") else "")


class PythonFormatter:
    """Formatter for Python code (normalizes indentation to 4 spaces and trims trailing whitespace)."""

    def format_code(self, code: str, indent_size: int = 4) -> str:
        lines = code.split("\n")
        result = []
        for line in lines:
            expanded = line.expandtabs(indent_size).rstrip()
            result.append(expanded)
        return "\n".join(result)


class CodeFormatter:
    """Unified code formatter for all supported languages."""

    def __init__(self):
        self.basic = BasicFormatter()
        self.logo = LogoFormatter()
        self.pilot = PilotFormatter()
        self.c_fmt = CFormatter()
        self.pascal = PascalFormatter()
        self.prolog = PrologFormatter()
        self.forth = ForthFormatter()
        self.brainfuck = BrainfuckFormatter()
        self.python = PythonFormatter()

    def format_code(
        self, code: str, language: str, indent_size: int = 4
    ) -> Tuple[str, str]:
        """Format code for the specified language.

        Args:
            code: Source code to format
            language: Language name (e.g. 'BASIC', 'LOGO', 'PILOT', 'C', 'PASCAL', 'PROLOG', 'FORTH', 'BRAINFUCK', 'PYTHON')
            indent_size: Spaces per indent level

        Returns:
            Tuple of (formatted_code, status_message)
        """
        lang = language.upper()

        try:
            if lang == "BASIC":
                formatted = self.basic.format_code(code, indent_size)
                return formatted, "✅ Formatted BASIC code"
            elif lang == "LOGO":
                formatted = self.logo.format_code(code, indent_size)
                return formatted, "✅ Formatted Logo code"
            elif lang == "PILOT":
                formatted = self.pilot.format_code(code, indent_size)
                return formatted, "✅ Formatted PILOT code"
            elif lang == "C":
                formatted = self.c_fmt.format_code(code, indent_size)
                return formatted, "✅ Formatted C code"
            elif lang == "PASCAL":
                formatted = self.pascal.format_code(code, 2)
                return formatted, "✅ Formatted Pascal code"
            elif lang == "PROLOG":
                formatted = self.prolog.format_code(code, indent_size)
                return formatted, "✅ Formatted Prolog code"
            elif lang == "FORTH":
                formatted = self.forth.format_code(code, 2)
                return formatted, "✅ Formatted Forth code"
            elif lang == "BRAINFUCK":
                formatted = self.brainfuck.format_code(code, 2)
                return formatted, "✅ Formatted Brainfuck code"
            elif lang in ("PYTHON", "PYTHON_LANG", "PYTHON3"):
                formatted = self.python.format_code(code, indent_size)
                return formatted, "✅ Formatted Python code"
            else:
                return code, f"❌ Unknown language: {language}"
        except ValueError as e:
            return code, f"❌ Format error: {e}"
        except Exception as e:  # Unexpected errors
            from .logging_config import get_logger

            logger = get_logger(__name__)
            logger.exception("Unexpected error in code formatting")
            return code, f"❌ Unexpected format error: {e}"

    def normalize_keywords(self, code: str, language: str) -> Tuple[str, str]:
        """Normalize keyword case for the specified language.

        Args:
            code: Source code
            language: Language name

        Returns:
            Tuple of (normalized_code, status_message)
        """
        lang = language.upper()

        try:
            if lang == "BASIC":
                result = self.basic.normalize_keywords(code)
                return result, "✅ Normalized BASIC keywords"
            elif lang == "LOGO":
                result = self.logo.normalize_keywords(code)
                return result, "✅ Normalized Logo keywords"
            elif lang == "PILOT":
                result = self.pilot.normalize_commands(code)
                return result, "✅ Normalized PILOT commands"
            elif lang == "PASCAL":
                result = self.pascal.normalize_keywords(code)
                return result, "✅ Normalized Pascal keywords"
            elif lang == "FORTH":
                result = self.forth.normalize_keywords(code)
                return result, "✅ Normalized Forth keywords"
            elif lang in ("C", "PROLOG", "BRAINFUCK", "PYTHON", "PYTHON_LANG", "PYTHON3"):
                return code, f"✅ Normalized {lang} code"
            else:
                return code, f"❌ Unknown language: {language}"
        except Exception as e:  # pylint: disable=broad-except
            return code, f"❌ Normalize error: {e}"

    def format_and_normalize(
        self, code: str, language: str, indent_size: int = 4
    ) -> Tuple[str, str]:
        """Format code and normalize keywords.

        Args:
            code: Source code
            language: Supported language name
            indent_size: Spaces per indent level

        Returns:
            Tuple of (formatted_code, status_message)
        """
        # First normalize keywords
        normalized, _ = self.normalize_keywords(code, language)
        # Then format
        formatted, msg = self.format_code(normalized, language, indent_size)
        return formatted, msg


# Global instance
_formatter: CodeFormatter | None = None


def get_formatter() -> CodeFormatter:
    """Get the global code formatter instance."""
    global _formatter  # pylint: disable=global-statement
    if _formatter is None:
        _formatter = CodeFormatter()
    return _formatter
