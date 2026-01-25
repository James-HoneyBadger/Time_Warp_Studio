"""Examples browser for Time Warp Studio.

Searchable, tagged examples panel with quick access.
"""

from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import List, Optional


class Language(Enum):
    """Supported languages."""

    BASIC = "basic"
    PILOT = "pilot"
    LOGO = "logo"
    PYTHON = "python"
    C = "c"
    PASCAL = "pascal"
    PROLOG = "prolog"


class Difficulty(Enum):
    """Example difficulty levels."""

    BEGINNER = "beginner"
    INTERMEDIATE = "intermediate"
    ADVANCED = "advanced"


@dataclass
class Example:
    """A single example program."""

    name: str
    description: str
    language: Language
    difficulty: Difficulty
    tags: List[str]
    file_path: Path
    code: str = ""

    def matches_search(self, query: str) -> bool:
        """Check if example matches search query."""
        query_lower = query.lower()
        return (
            query_lower in self.name.lower()
            or query_lower in self.description.lower()
            or any(query_lower in tag.lower() for tag in self.tags)
        )


class ExamplesBrowser:
    """Manage and search example programs."""

    def __init__(self, examples_dir: Path):
        self.examples_dir = examples_dir
        self.examples: List[Example] = []
        self.scan_examples()

    def scan_examples(self) -> None:
        """Scan examples directory and load metadata."""
        self.examples.clear()

        if not self.examples_dir.exists():
            return

        # Map language folders
        lang_map = {
            "basic": Language.BASIC,
            "pilot": Language.PILOT,
            "logo": Language.LOGO,
            "python": Language.PYTHON,
            "c": Language.C,
            "pascal": Language.PASCAL,
            "prolog": Language.PROLOG,
        }

        for lang_folder in self.examples_dir.iterdir():
            if not lang_folder.is_dir():
                continue

            lang_name = lang_folder.name.lower()
            if lang_name not in lang_map:
                continue

            language = lang_map[lang_name]

            for file_path in sorted(
                lang_folder.glob(f"*.{self._get_extension(language)}")
            ):
                # Extract metadata from filename
                stem = file_path.stem

                # Determine difficulty from number prefix
                if stem.startswith("0"):
                    difficulty = Difficulty.BEGINNER
                elif stem.startswith("1"):
                    difficulty = Difficulty.INTERMEDIATE
                else:
                    difficulty = Difficulty.ADVANCED

                # Extract tags from filename
                tags = []
                if "hello" in stem.lower():
                    tags.append("hello-world")
                if "recursive" in stem.lower():
                    tags.append("recursion")
                if "loop" in stem.lower():
                    tags.append("loops")
                if "function" in stem.lower() or "procedure" in stem.lower():
                    tags.append("procedures")
                if "graphic" in stem.lower() or "demo" in stem.lower():
                    tags.append("graphics")

                # Read code
                try:
                    code = file_path.read_text()
                except Exception:
                    code = ""

                example = Example(
                    name=file_path.stem,
                    description=f"Example: {file_path.stem}",
                    language=language,
                    difficulty=difficulty,
                    tags=tags,
                    file_path=file_path,
                    code=code,
                )
                self.examples.append(example)

    @staticmethod
    def _get_extension(language: Language) -> str:
        """Get file extension for language."""
        return {
            Language.BASIC: "bas",
            Language.PILOT: "pilot",
            Language.LOGO: "logo",
            Language.PYTHON: "py",
            Language.C: "c",
            Language.PASCAL: "pas",
            Language.PROLOG: "pl",
        }.get(language, "*")

    def search(
        self,
        query: str = "",
        language: Optional[Language] = None,
        difficulty: Optional[Difficulty] = None,
        tag: Optional[str] = None,
    ) -> List[Example]:
        """Search examples with filters."""
        results = self.examples

        # Filter by query
        if query:
            results = [e for e in results if e.matches_search(query)]

        # Filter by language
        if language:
            results = [e for e in results if e.language == language]

        # Filter by difficulty
        if difficulty:
            results = [e for e in results if e.difficulty == difficulty]

        # Filter by tag
        if tag:
            results = [e for e in results if tag in e.tags]

        return sorted(results, key=lambda e: e.name)

    def get_by_language(self, language: Language) -> List[Example]:
        """Get all examples for a language."""
        return self.search(language=language)

    def get_by_difficulty(self, difficulty: Difficulty) -> List[Example]:
        """Get all examples of a difficulty level."""
        return self.search(difficulty=difficulty)

    def get_featured(self, count: int = 5) -> List[Example]:
        """Get featured examples (beginner + hello-world)."""
        featured = [
            e
            for e in self.examples
            if e.difficulty == Difficulty.BEGINNER and "hello" in e.tags
        ]
        return featured[:count]

    def get_language_summary(self) -> dict:
        """Get count of examples per language."""
        summary = {}
        for lang in Language:
            count = len(self.search(language=lang))
            if count > 0:
                summary[lang.value] = count
        return summary
