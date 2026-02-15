"""Offline reference search across docs and examples."""

from dataclasses import dataclass
from difflib import SequenceMatcher
from pathlib import Path
from typing import Iterable, List, Optional, Set


@dataclass
class ReferenceEntry:
    """Indexed reference entry."""

    title: str
    path: Path
    tags: List[str]
    snippet: str
    source: str


class ReferenceIndex:
    """Build and query a local reference index."""

    def __init__(self, root: Optional[Path] = None):
        self.root = root or self._find_repo_root()
        self.entries: List[ReferenceEntry] = []
        self.tags: Set[str] = set()
        self.scan()

    def _find_repo_root(self) -> Path:
        """Find repository root by locating Examples or docs."""
        start = Path(__file__).resolve()
        for parent in [start] + list(start.parents):
            if (parent / "Examples").exists() and (parent / "docs").exists():
                return parent
        return start.parents[-1]

    def scan(self) -> None:
        """Scan docs and examples into index."""
        self.entries = []
        self.tags = set()

        for entry in self._scan_docs():
            self._add_entry(entry)

        for entry in self._scan_examples():
            self._add_entry(entry)

    def _scan_docs(self) -> Iterable[ReferenceEntry]:
        """Scan documentation markdown files."""
        docs_root = self.root / "docs"
        if not docs_root.exists():
            return []

        entries: List[ReferenceEntry] = []
        for path in docs_root.rglob("*.md"):
            title = path.stem.replace("-", " ")
            snippet = self._read_snippet(path)
            tags = self._tags_from_path(path, source="docs")
            entries.append(
                ReferenceEntry(
                    title=title,
                    path=path,
                    tags=tags,
                    snippet=snippet,
                    source="docs",
                )
            )
        return entries

    def _scan_examples(self) -> Iterable[ReferenceEntry]:
        """Scan example code files."""
        examples_root = self.root / "Examples"
        if not examples_root.exists():
            return []

        extensions = {".bas", ".pilot", ".logo", ".c", ".pas", ".pro", ".pl", ".f"}
        entries: List[ReferenceEntry] = []
        for path in examples_root.rglob("*"):
            if not path.is_file():
                continue
            if path.suffix.lower() not in extensions:
                continue
            title = path.stem.replace("_", " ")
            snippet = self._read_snippet(path)
            tags = self._tags_from_path(path, source="examples")
            entries.append(
                ReferenceEntry(
                    title=title,
                    path=path,
                    tags=tags,
                    snippet=snippet,
                    source="examples",
                )
            )
        return entries

    def _read_snippet(self, path: Path, max_lines: int = 12) -> str:
        """Read a short snippet from file."""
        try:
            lines = path.read_text(encoding="utf-8").splitlines()
        except (OSError, UnicodeDecodeError):
            return ""
        return "\n".join(lines[:max_lines])

    def _tags_from_path(self, path: Path, source: str) -> List[str]:
        """Build tags from path components and extension."""
        tags = {source, path.suffix.lstrip(".").lower()}
        for part in path.parts:
            if part.lower() in {"docs", "examples"}:
                continue
            if part.startswith("."):
                continue
            tags.add(part.lower())
        return sorted(tags)

    def _add_entry(self, entry: ReferenceEntry) -> None:
        """Add entry and update tags."""
        self.entries.append(entry)
        self.tags.update(entry.tags)

    def search(self, query: str, tag: Optional[str] = None) -> List[ReferenceEntry]:
        """Search the index with fuzzy matching and tags."""
        query = query.strip().lower()
        results = []

        for entry in self.entries:
            if tag and tag not in entry.tags:
                continue

            score = 0.0
            if query:
                score += self._fuzzy_score(query, entry.title)
                score += self._fuzzy_score(query, entry.snippet) * 0.5
                if any(query in t for t in entry.tags):
                    score += 0.6
                if query in entry.title.lower():
                    score += 0.8
            results.append((score, entry))

        results.sort(key=lambda item: item[0], reverse=True)
        return [entry for score, entry in results if score > 0 or not query]

    def _fuzzy_score(self, query: str, text: str) -> float:
        """Compute fuzzy match score for query vs text."""
        if not query or not text:
            return 0.0
        text_lower = text.lower()
        if query in text_lower:
            return 1.0
        return SequenceMatcher(None, query, text_lower).ratio()

    def get_tags(self) -> List[str]:
        """Return sorted list of tags."""
        return sorted(self.tags)
