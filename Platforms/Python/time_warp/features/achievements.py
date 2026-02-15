"""Achievement badges and progress tracking."""

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional


@dataclass
class Badge:
    """Achievement badge definition."""

    id: str
    name: str
    description: str
    unlocked: bool


class ProgressTracker:
    """Track tutorial and example progress with badges."""

    def __init__(self, root: Optional[Path] = None):
        self.root = root or self._find_repo_root()
        self.state_path = Path.home() / ".time_warp" / "progress.json"
        self.state: Dict[str, List[str]] = {
            "completed_tutorials": [],
            "completed_examples": [],
        }
        self._load()

    def _find_repo_root(self) -> Path:
        """Find repository root by locating Examples or docs."""
        start = Path(__file__).resolve()
        for parent in [start] + list(start.parents):
            if (parent / "Examples").exists() and (parent / "docs").exists():
                return parent
        return start.parents[-1]

    def _load(self) -> None:
        """Load progress from disk."""
        try:
            if self.state_path.exists():
                data = json.loads(self.state_path.read_text(encoding="utf-8"))
                if isinstance(data, dict):
                    self.state.update(data)
        except (OSError, ValueError, TypeError):
            pass

    def _save(self) -> None:
        """Persist progress to disk."""
        self.state_path.parent.mkdir(parents=True, exist_ok=True)
        try:
            self.state_path.write_text(
                json.dumps(self.state, indent=2), encoding="utf-8"
            )
        except (OSError, ValueError, TypeError):
            pass

    def get_tutorials(self) -> List[str]:
        """List tutorial markdown files."""
        tutorials_root = self.root / "docs" / "tutorials"
        if not tutorials_root.exists():
            return []
        return sorted(str(p.relative_to(self.root)) for p in tutorials_root.glob("*.md"))

    def get_examples(self) -> List[str]:
        """List example source files."""
        examples_root = self.root / "Examples"
        if not examples_root.exists():
            return []

        extensions = {".bas", ".pilot", ".logo", ".c", ".pas", ".pro", ".pl", ".f"}
        results = []
        for path in examples_root.rglob("*"):
            if not path.is_file():
                continue
            if path.suffix.lower() not in extensions:
                continue
            results.append(str(path.relative_to(self.root)))
        return sorted(results)

    def mark_tutorial_completed(self, tutorial_path: str, completed: bool) -> None:
        """Mark a tutorial as completed."""
        tutorials = set(self.state.get("completed_tutorials", []))
        if completed:
            tutorials.add(tutorial_path)
        else:
            tutorials.discard(tutorial_path)
        self.state["completed_tutorials"] = sorted(tutorials)
        self._save()

    def mark_example_completed(self, example_path: str, completed: bool) -> None:
        """Mark an example as completed."""
        examples = set(self.state.get("completed_examples", []))
        if completed:
            examples.add(example_path)
        else:
            examples.discard(example_path)
        self.state["completed_examples"] = sorted(examples)
        self._save()

    def record_example_run(self, example_path: str) -> None:
        """Record an example run as completed."""
        self.mark_example_completed(example_path, True)

    def get_progress(self) -> Dict[str, float]:
        """Get progress percentages for tutorials and examples."""
        tutorials = self.get_tutorials()
        completed_tutorials = set(self.state.get("completed_tutorials", []))
        completed_examples = set(self.state.get("completed_examples", []))

        tutorial_pct = (
            len(completed_tutorials) / max(1, len(tutorials)) * 100.0
        )
        example_total = len(self.get_examples())
        example_pct = len(completed_examples) / max(1, example_total) * 100.0

        return {
            "tutorial_percent": tutorial_pct,
            "example_percent": example_pct,
            "tutorial_total": len(tutorials),
            "example_total": example_total,
            "tutorial_completed": len(completed_tutorials),
            "example_completed": len(completed_examples),
        }

    def get_badges(self) -> List[Badge]:
        """Get badge status list."""
        tutorials = self.get_tutorials()
        completed_tutorials = set(self.state.get("completed_tutorials", []))
        completed_examples = set(self.state.get("completed_examples", []))

        badges = [
            Badge(
                id="first_tutorial",
                name="First Tutorial",
                description="Complete your first tutorial",
                unlocked=len(completed_tutorials) >= 1,
            ),
            Badge(
                id="all_tutorials",
                name="Tutorial Master",
                description="Complete all tutorials",
                unlocked=len(completed_tutorials) >= len(tutorials) and len(tutorials) > 0,
            ),
            Badge(
                id="first_example",
                name="First Example",
                description="Run your first example program",
                unlocked=len(completed_examples) >= 1,
            ),
            Badge(
                id="five_examples",
                name="Example Explorer",
                description="Complete five examples",
                unlocked=len(completed_examples) >= 5,
            ),
        ]
        return badges
