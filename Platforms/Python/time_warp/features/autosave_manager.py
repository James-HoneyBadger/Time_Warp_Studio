"""Autosave and file history manager.

Background saves with version history and restore capability.
"""

import json
import shutil
from datetime import datetime
from pathlib import Path
from typing import List, Optional


class FileVersion:
    """A single saved version of a file."""

    def __init__(self, version_num: int, timestamp: datetime, content: str):
        self.version_num = version_num
        self.timestamp = timestamp
        self.content = content
        self.size = len(content)

    def to_dict(self) -> dict:
        """Convert to dictionary."""
        return {
            "version": self.version_num,
            "timestamp": self.timestamp.isoformat(),
            "size": self.size,
        }


class FileHistory:
    """Track version history for a file."""

    def __init__(self, file_path: Path):
        self.file_path = file_path
        self.versions: List[FileVersion] = []
        self.history_dir = Path.home() / ".time_warp" / "history"
        self._ensure_history_dir()

    def _ensure_history_dir(self) -> None:
        """Create history directory if needed."""
        self.history_dir.mkdir(parents=True, exist_ok=True)

    def _get_history_file(self) -> Path:
        """Get path to history metadata file."""
        safe_name = self.file_path.name.replace("/", "_")
        return self.history_dir / f"{safe_name}.history"

    def _get_versions_dir(self) -> Path:
        """Get directory for version files."""
        safe_name = self.file_path.stem.replace("/", "_")
        versions_dir = self.history_dir / f"{safe_name}_versions"
        versions_dir.mkdir(parents=True, exist_ok=True)
        return versions_dir

    def save_version(self, content: str) -> FileVersion:
        """Save a new version."""
        version_num = len(self.versions) + 1
        timestamp = datetime.now()
        version = FileVersion(version_num, timestamp, content)

        # Save version file
        versions_dir = self._get_versions_dir()
        version_file = versions_dir / f"v{version_num}.txt"
        version_file.write_text(content)

        # Limit history to 20 versions
        self.versions.append(version)
        if len(self.versions) > 20:
            old_version = self.versions.pop(0)
            old_file = versions_dir / f"v{old_version.version_num}.txt"
            if old_file.exists():
                old_file.unlink()

        self._save_metadata()
        return version

    def get_version(self, version_num: int) -> Optional[str]:
        """Get content of a specific version."""
        if version_num < 1 or version_num > len(self.versions):
            return None

        versions_dir = self._get_versions_dir()
        version_file = versions_dir / f"v{version_num}.txt"

        if version_file.exists():
            return version_file.read_text()
        return None

    def restore_version(self, version_num: int) -> bool:
        """Restore file to a specific version."""
        content = self.get_version(version_num)
        if content is None:
            return False

        try:
            self.file_path.write_text(content)
            return True
        except Exception:
            return False

    def list_versions(self) -> List[FileVersion]:
        """Get list of all versions."""
        return list(self.versions)

    def get_latest_version(self) -> Optional[FileVersion]:
        """Get most recent version."""
        return self.versions[-1] if self.versions else None

    def clear_history(self) -> None:
        """Clear all version history."""
        versions_dir = self._get_versions_dir()
        if versions_dir.exists():
            shutil.rmtree(versions_dir)

        self.versions.clear()
        self._save_metadata()

    def _save_metadata(self) -> None:
        """Save history metadata."""
        history_file = self._get_history_file()
        metadata = {
            "file_path": str(self.file_path),
            "versions": [v.to_dict() for v in self.versions],
        }
        history_file.write_text(json.dumps(metadata, indent=2))


class AutosaveManager:
    """Manage autosave functionality."""

    def __init__(self, autosave_interval: int = 300):
        self.autosave_interval = autosave_interval  # seconds
        self.autosave_enabled = True
        self.last_autosave: dict[str, datetime] = {}
        self.file_histories: dict[str, FileHistory] = {}

    def should_autosave(self, file_path: Path) -> bool:
        """Check if file should be autosaved."""
        if not self.autosave_enabled:
            return False

        file_str = str(file_path)
        if file_str not in self.last_autosave:
            return True

        elapsed = (datetime.now() - self.last_autosave[file_str]).total_seconds()
        return elapsed >= self.autosave_interval

    def autosave_file(self, file_path: Path, content: str) -> bool:
        """Perform autosave and create version."""
        if not self.should_autosave(file_path):
            return False

        try:
            # Save file
            file_path.write_text(content)
            self.last_autosave[str(file_path)] = datetime.now()

            # Create version
            history = self._get_history(file_path)
            history.save_version(content)

            return True
        except Exception:
            return False

    def _get_history(self, file_path: Path) -> FileHistory:
        """Get or create file history."""
        file_str = str(file_path)
        if file_str not in self.file_histories:
            self.file_histories[file_str] = FileHistory(file_path)
        return self.file_histories[file_str]

    def get_file_history(self, file_path: Path) -> Optional[FileHistory]:
        """Get file history."""
        return self._get_history(file_path)

    def set_autosave_enabled(self, enabled: bool) -> None:
        """Enable/disable autosave."""
        self.autosave_enabled = enabled

    def set_autosave_interval(self, seconds: int) -> None:
        """Set autosave interval."""
        if seconds > 0:
            self.autosave_interval = seconds
