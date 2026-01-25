"""Real-time syntax validation for all supported languages."""

from dataclasses import dataclass
from enum import Enum
from typing import List, Optional

from .interpreter import Language


class SeverityLevel(Enum):
    """Severity levels for validation issues."""

    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


@dataclass
class SyntaxIssue:
    """Represents a single syntax or semantic issue."""

    line: int
    column: int
    message: str
    severity: SeverityLevel
    code: Optional[str] = None  # Error code like 'E001'


class SyntaxValidator:
    """Validates code syntax in real-time without full execution."""

    def __init__(self):
        """Initialize validator with language parsers."""
        self._cache = {}  # line_hash -> issues

    def validate(self, code: str, language: Language) -> List[SyntaxIssue]:
        """
        Validate code without executing it.

        Returns list of SyntaxIssue objects sorted by line number.
        """
        if not code.strip():
            return []

        # Check cache first
        code_hash = hash(code)
        if code_hash in self._cache:
            return self._cache[code_hash]

        issues = []

        if language == Language.BASIC:
            issues = self._validate_basic(code)
        elif language == Language.LOGO:
            issues = self._validate_logo(code)
        elif language == Language.PILOT:
            issues = self._validate_pilot(code)
        elif language == Language.PASCAL:
            issues = self._validate_pascal(code)
        elif language == Language.C:
            issues = self._validate_c(code)
        elif language == Language.FORTH:
            issues = self._validate_forth(code)
        elif language == Language.PROLOG:
            issues = self._validate_prolog(code)

        # Sort by line number
        issues.sort(key=lambda x: (x.line, x.column))

        # Cache result
        self._cache[code_hash] = issues

        # Limit cache size
        if len(self._cache) > 1000:
            self._cache.clear()

        return issues

    def _validate_basic(self, code: str) -> List[SyntaxIssue]:
        """Validate BASIC syntax."""
        issues = []
        lines = code.split("\n")

        # Check line numbers are in ascending order
        line_numbers = []
        for i, line in enumerate(lines, 1):
            stripped = line.strip()
            if not stripped or stripped.startswith("REM"):
                continue

            # Extract line number if present
            parts = stripped.split(maxsplit=1)
            if parts and parts[0].isdigit():
                line_num = int(parts[0])
                if line_numbers and line_num <= line_numbers[-1]:
                    issues.append(
                        SyntaxIssue(
                            line=i,
                            column=1,
                            message="Line numbers must be in ascending order",
                            severity=SeverityLevel.ERROR,
                            code="E001",
                        )
                    )
                line_numbers.append(line_num)

        # Check for balanced BASIC keywords
        open_structures = {
            "IF": "THEN",
            "FOR": "NEXT",
            "WHILE": "WEND",
            "DO": "LOOP",
        }
        open_count = {k: 0 for k in open_structures}

        for i, line in enumerate(lines, 1):
            stripped = line.strip().upper()

            # Count opens/closes
            for keyword in open_structures:
                if keyword in stripped and f"{keyword} " in (stripped + " "):
                    open_count[keyword] += 1

            for close_word in open_structures.values():
                if close_word in stripped:
                    keyword = [
                        k for k, v in open_structures.items() if v == close_word
                    ][0]
                    open_count[keyword] -= 1
                    if open_count[keyword] < 0:
                        issues.append(
                            SyntaxIssue(
                                line=i,
                                column=1,
                                message=(
                                    f"Unexpected {close_word}"
                                    f" without matching {keyword}"
                                ),
                                severity=SeverityLevel.ERROR,
                                code="E002",
                            )
                        )

        # Check unclosed structures
        for keyword, count in open_count.items():
            if count > 0:
                issues.append(
                    SyntaxIssue(
                        line=len(lines),
                        column=1,
                        message=(
                            f"Unclosed {keyword} structure"
                            f" (missing {open_structures[keyword]})"
                        ),
                        severity=SeverityLevel.ERROR,
                        code="E003",
                    )
                )

        return issues

    def _validate_logo(self, code: str) -> List[SyntaxIssue]:
        """Validate Logo syntax."""
        issues = []
        lines = code.split("\n")

        # Check for balanced brackets in procedure definitions
        bracket_count = 0
        paren_count = 0

        for i, line in enumerate(lines, 1):
            stripped = line.strip()

            # Count brackets for procedures
            bracket_count += stripped.count("[") - stripped.count("]")
            paren_count += stripped.count("(") - stripped.count(")")

            if bracket_count < 0:
                issues.append(
                    SyntaxIssue(
                        line=i,
                        column=stripped.find("]") + 1,
                        message="Unexpected ] without matching [",
                        severity=SeverityLevel.ERROR,
                        code="L001",
                    )
                )
                bracket_count = 0

            if paren_count < 0:
                issues.append(
                    SyntaxIssue(
                        line=i,
                        column=stripped.find(")") + 1,
                        message="Unexpected ) without matching (",
                        severity=SeverityLevel.ERROR,
                        code="L002",
                    )
                )
                paren_count = 0

        # Check unclosed brackets
        if bracket_count > 0:
            issues.append(
                SyntaxIssue(
                    line=len(lines),
                    column=1,
                    message="Unclosed [ bracket",
                    severity=SeverityLevel.ERROR,
                    code="L003",
                )
            )

        if paren_count > 0:
            issues.append(
                SyntaxIssue(
                    line=len(lines),
                    column=1,
                    message="Unclosed ( parenthesis",
                    severity=SeverityLevel.ERROR,
                    code="L004",
                )
            )

        return issues

    def _validate_pilot(self, code: str) -> List[SyntaxIssue]:
        """Validate PILOT syntax."""
        issues = []
        lines = code.split("\n")

        # Check for valid command prefixes
        valid_prefixes = {"A:", "T:", "M:", "E:", "Y:", "N:", "U:", "*:"}

        for i, line in enumerate(lines, 1):
            stripped = line.strip()
            if not stripped or stripped.startswith(";"):
                continue

            # Check for label (ends with :)
            if stripped.endswith(":") and not any(c in stripped for c in "ATMEYNU"):
                continue  # Valid label

            # Check command prefix
            if len(stripped) > 1 and stripped[1] == ":":
                prefix = stripped[0].upper() + ":"
                if prefix not in valid_prefixes:
                    issues.append(
                        SyntaxIssue(
                            line=i,
                            column=1,
                            message=f"Invalid PILOT command prefix '{prefix}'",
                            severity=SeverityLevel.ERROR,
                            code="P001",
                        )
                    )

        return issues

    def _validate_pascal(self, code: str) -> List[SyntaxIssue]:
        """Validate Pascal syntax."""
        issues = []
        lines = code.split("\n")

        # Check for balanced begin/end
        begin_count = 0
        for i, line in enumerate(lines, 1):
            stripped = line.strip().lower()
            begin_count += stripped.count("begin") - stripped.count("end")

            if begin_count < 0:
                issues.append(
                    SyntaxIssue(
                        line=i,
                        column=1,
                        message="Unexpected 'end' without matching 'begin'",
                        severity=SeverityLevel.ERROR,
                        code="PA001",
                    )
                )
                begin_count = 0

        if begin_count > 0:
            issues.append(
                SyntaxIssue(
                    line=len(lines),
                    column=1,
                    message="Unclosed 'begin' (missing 'end')",
                    severity=SeverityLevel.ERROR,
                    code="PA002",
                )
            )

        return issues

    def _validate_c(self, code: str) -> List[SyntaxIssue]:
        """Validate C syntax."""
        issues = []
        lines = code.split("\n")

        # Check for balanced braces
        brace_count = 0
        paren_count = 0

        for i, line in enumerate(lines, 1):
            # Skip comments
            if line.strip().startswith("//"):
                continue

            brace_count += line.count("{") - line.count("}")
            paren_count += line.count("(") - line.count(")")

            if brace_count < 0:
                issues.append(
                    SyntaxIssue(
                        line=i,
                        column=line.find("}") + 1,
                        message="Unexpected } without matching {",
                        severity=SeverityLevel.ERROR,
                        code="C001",
                    )
                )
                brace_count = 0

            if paren_count < 0:
                issues.append(
                    SyntaxIssue(
                        line=i,
                        column=line.find(")") + 1,
                        message="Unexpected ) without matching (",
                        severity=SeverityLevel.ERROR,
                        code="C002",
                    )
                )
                paren_count = 0

        if brace_count > 0:
            issues.append(
                SyntaxIssue(
                    line=len(lines),
                    column=1,
                    message="Unclosed { brace",
                    severity=SeverityLevel.ERROR,
                    code="C003",
                )
            )

        if paren_count > 0:
            issues.append(
                SyntaxIssue(
                    line=len(lines),
                    column=1,
                    message="Unclosed ( parenthesis",
                    severity=SeverityLevel.ERROR,
                    code="C004",
                )
            )

        return issues

    def _validate_forth(self, code: str) -> List[SyntaxIssue]:
        """Validate Forth syntax."""
        issues = []
        # Forth has minimal syntax rules; basic validation only
        lines = code.split("\n")

        # Check for unmatched quotes
        for i, line in enumerate(lines, 1):
            if line.count('"') % 2 != 0:
                issues.append(
                    SyntaxIssue(
                        line=i,
                        column=1,
                        message="Unmatched quote in string",
                        severity=SeverityLevel.WARNING,
                        code="F001",
                    )
                )

        return issues

    def _validate_prolog(self, code: str) -> List[SyntaxIssue]:
        """Validate Prolog syntax."""
        issues = []
        lines = code.split("\n")

        # Check for unmatched parentheses
        paren_count = 0
        for i, line in enumerate(lines, 1):
            # Skip comments
            if line.strip().startswith("%"):
                continue

            paren_count += line.count("(") - line.count(")")

            if paren_count < 0:
                issues.append(
                    SyntaxIssue(
                        line=i,
                        column=1,
                        message="Unexpected ) without matching (",
                        severity=SeverityLevel.ERROR,
                        code="PL001",
                    )
                )
                paren_count = 0

        if paren_count > 0:
            issues.append(
                SyntaxIssue(
                    line=len(lines),
                    column=1,
                    message="Unclosed ( parenthesis",
                    severity=SeverityLevel.ERROR,
                    code="PL002",
                )
            )

        return issues
