"""Lightweight AI-style code suggestions for the IDE.

The goal of this module is to provide deterministic, context-aware suggestions
without requiring an external service. Suggestions are derived from the current
editor content and tailored to the language patterns already present.
"""

from __future__ import annotations

from typing import List


class AISuggestions:
    """Generate small, context-aware code suggestions."""

    def __init__(self):
        self.context = ""

    def update_context(self, code: str) -> None:
        """Store the current editor contents for later suggestions."""
        self.context = code or ""

    def _infer_language(self, code: str) -> str:
        """Infer the language family from the current source."""
        upper = code.upper()
        stripped = code.strip()

        if "DEF " in upper or stripped.startswith("def ") or "IMPORT " in upper:
            return "python"
        if "REPEAT" in upper or "FD " in upper or "RT " in upper:
            return "logo"
        if "PRINT" in upper or "INPUT" in upper or "FOR " in upper:
            return "basic"
        return "generic"

    def _current_line_prefix(self, code: str, cursor_position: int) -> str:
        """Return the text on the current line up to the cursor."""
        safe_cursor = max(0, min(cursor_position, len(code)))
        current = code[:safe_cursor].splitlines()
        return current[-1] if current else ""

    def _dedupe(self, suggestions: List[str]) -> List[str]:
        """Preserve order while removing empty or repeated suggestions."""
        seen = set()
        result = []
        for suggestion in suggestions:
            cleaned = suggestion.strip("\n")
            if not cleaned or cleaned in seen:
                continue
            seen.add(cleaned)
            result.append(cleaned)
        return result[:5]

    def _python_suggestions(self, prefix: str, code: str) -> List[str]:
        suggestions: List[str] = []
        stripped = prefix.lstrip()
        indent = prefix[: len(prefix) - len(stripped)]

        if stripped.startswith("def ") or code.rstrip().endswith(":"):
            suggestions.extend(
                [
                    f"{indent}return None",
                    f'{indent}"""Describe what this function does."""',
                    f"{indent}if not value:\n{indent}    return None",
                ]
            )
        if stripped.startswith("for "):
            suggestions.append(f"{indent}print(item)")
        if "class " in code:
            suggestions.append(f"{indent}def __repr__(self):\n{indent}    return self.__class__.__name__")
        if not suggestions:
            suggestions.extend(
                [
                    "return result",
                    "if __name__ == '__main__':\n    main()",
                    "try:\n    pass\nexcept Exception as exc:\n    print(exc)",
                ]
            )
        return suggestions

    def _basic_suggestions(self, prefix: str, code: str) -> List[str]:
        suggestions: List[str] = []
        upper_prefix = prefix.upper().strip()
        upper_code = code.upper()

        if upper_prefix.startswith("FOR ") or "FOR " in upper_code:
            suggestions.append("NEXT I")
        if upper_prefix.startswith("IF ") or "IF " in upper_code:
            suggestions.append('THEN PRINT "OK"')
        suggestions.extend(
            [
                'IF X > 0 THEN PRINT "POSITIVE"',
                'INPUT "NAME"; N$',
                'GOSUB 100',
            ]
        )
        return suggestions

    def _logo_suggestions(self, _prefix: str, _code: str) -> List[str]:
        return [
            "REPEAT 4 [FD 50 RT 90]",
            "PU SETPOS [0 0] PD",
            "SETPENCOLOR \"BLUE",
            "REPEAT 36 [FD 10 RT 10]",
        ]

    def get_suggestions(self, cursor_position: int) -> List[str]:
        """Generate code suggestions based on the current context."""
        code = self.context or ""
        prefix = self._current_line_prefix(code, cursor_position)
        language = self._infer_language(code)

        if language == "python":
            suggestions = self._python_suggestions(prefix, code)
        elif language == "basic":
            suggestions = self._basic_suggestions(prefix, code)
        elif language == "logo":
            suggestions = self._logo_suggestions(prefix, code)
        else:
            suggestions = [
                "Add a short comment describing this section",
                "Extract repeated logic into a helper",
                "Validate input before using it",
            ]

        return self._dedupe(suggestions)

    def apply_suggestion(self, suggestion: str) -> str:
        """Append a selected suggestion to the current context and return it."""
        base = self.context.rstrip()
        addition = suggestion.lstrip("\n")
        combined = f"{base}\n{addition}" if base else addition
        self.context = combined
        return combined


if __name__ == "__main__":
    ai = AISuggestions()
    ai.update_context("def example(value):\n    ")
    suggestions = ai.get_suggestions(cursor_position=len(ai.context))
    for suggestion in suggestions:
        print(suggestion)
    print(ai.apply_suggestion(suggestions[0]))
