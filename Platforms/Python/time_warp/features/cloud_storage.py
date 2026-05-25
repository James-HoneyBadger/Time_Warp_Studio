"""Cloud / local-sync storage for Time Warp Studio projects.

Provides two levels of persistence beyond the regular save:

1. **Local filesystem sync** (always available)
   Copies the project directory to a configurable ``sync_path``
   (default: ``~/.time_warp/sync/``).  No credentials needed.

2. **GitHub Gist upload** (optional — requires ``requests`` and a token)
   Uploads every ``*.bas``, ``*.logo``, ``*.py``, etc. source file as a
   private GitHub Gist.  The Gist URL is written to
   ``~/.time_warp/sync/<project>/gist_url.txt``.
"""

from __future__ import annotations

import json
import logging
import shutil
from datetime import datetime, timezone
from pathlib import Path
from typing import Optional

logger = logging.getLogger(__name__)

# Default location for the local sync directory
_DEFAULT_SYNC_ROOT = Path.home() / ".time_warp" / "sync"

# Source file extensions to include in Gist uploads
_SOURCE_EXTENSIONS = {
    ".bas", ".logo", ".py", ".rb", ".lua", ".js", ".c", ".h",
    ".pas", ".pl", ".f", ".forth", ".bf", ".erl", ".ht",
    ".asm", ".s", ".a65", ".scm", ".lisp", ".cob", ".tcl", ".ps",
}


class CloudStorageManager:
    """Manages project storage: local filesystem sync and optional Gist upload.

    Configuration is read from ``~/.time_warp/config.json`` under the key
    ``"cloud_storage"``:

    .. code-block:: json

        {
            "cloud_storage": {
                "sync_path": "/home/user/my_twarp_sync",
                "github_token": "ghp_xxxxxxxxxxxx"
            }
        }

    ``github_token`` is entirely optional.  When absent, Gist upload is
    skipped gracefully.
    """

    def __init__(self) -> None:
        self._config: dict = self._load_config()
        self.sync_root = Path(
            self._config.get("sync_path", str(_DEFAULT_SYNC_ROOT))
        ).expanduser()
        self._github_token: Optional[str] = self._config.get("github_token")
        self.sync_root.mkdir(parents=True, exist_ok=True)

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def sync_to_local(self, project_path: str | Path) -> str:
        """Copy *project_path* into the local sync directory.

        Returns the destination path as a string, or an error message.
        """
        src = Path(project_path).expanduser().resolve()
        if not src.exists():
            return f"❌ Path not found: {src}"

        project_name = src.name
        timestamp = datetime.now(tz=timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        dest = self.sync_root / project_name

        try:
            if dest.exists():
                # Keep a single rolling backup
                backup = self.sync_root / f"{project_name}.bak"
                if backup.exists():
                    shutil.rmtree(backup)
                dest.rename(backup)

            if src.is_dir():
                shutil.copytree(str(src), str(dest))
            else:
                dest.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(str(src), str(dest))

            # Write a small manifest
            manifest = {
                "project": project_name,
                "source": str(src),
                "synced_at": timestamp,
            }
            (dest if src.is_dir() else dest.parent).joinpath(
                ".twarp_sync_manifest.json"
            ).write_text(json.dumps(manifest, indent=2))

            logger.info("Synced '%s' → '%s'", src, dest)
            return f"✅ Synced to {dest}"

        except OSError as exc:
            logger.error("sync_to_local failed: %s", exc)
            return f"❌ Sync failed: {exc}"

    def sync_from_local(self, project_name: str, restore_to: str | Path) -> str:
        """Restore a previously synced project to *restore_to*.

        Returns a status string.
        """
        src = self.sync_root / project_name
        if not src.exists():
            available = [p.name for p in self.sync_root.iterdir() if not p.name.endswith(".bak")]
            return (
                f"❌ No synced copy of '{project_name}' found.\n"
                f"ℹ️  Available: {', '.join(available) or 'none'}"
            )

        dest = Path(restore_to).expanduser().resolve()
        try:
            if dest.exists():
                shutil.rmtree(dest)
            if src.is_dir():
                shutil.copytree(str(src), str(dest))
            else:
                shutil.copy2(str(src), str(dest))
            logger.info("Restored '%s' → '%s'", src, dest)
            return f"✅ Restored '{project_name}' to {dest}"
        except OSError as exc:
            logger.error("sync_from_local failed: %s", exc)
            return f"❌ Restore failed: {exc}"

    def list_synced(self) -> list[str]:
        """Return names of all locally synced projects."""
        return sorted(
            p.name
            for p in self.sync_root.iterdir()
            if not p.name.endswith(".bak") and not p.name.startswith(".")
        )

    def upload_to_gist(self, project_path: str | Path) -> str:
        """Upload source files in *project_path* as a private GitHub Gist.

        Requires the ``requests`` package and a ``github_token`` in config.
        Returns a status string (URL on success, error message on failure).
        """
        if not self._github_token:
            return "ℹ️ GitHub token not configured — skipping Gist upload."

        try:
            import requests  # type: ignore[import]
        except ImportError:
            return "ℹ️ 'requests' package not installed — skipping Gist upload."

        src = Path(project_path).expanduser().resolve()
        files = self._collect_source_files(src)
        if not files:
            return f"ℹ️ No source files found in {src}"

        gist_files = {
            path.name: {"content": path.read_text(encoding="utf-8", errors="replace")}
            for path in files
        }
        payload = {
            "description": f"Time Warp Studio — {src.name}",
            "public": False,
            "files": gist_files,
        }
        headers = {
            "Authorization": f"token {self._github_token}",
            "Accept": "application/vnd.github+json",
        }

        try:
            response = requests.post(
                "https://api.github.com/gists",
                json=payload,
                headers=headers,
                timeout=15,
            )
            response.raise_for_status()
            gist_url: str = response.json().get("html_url", "")
            # Persist the URL locally
            (self.sync_root / src.name).mkdir(parents=True, exist_ok=True)
            (self.sync_root / src.name / "gist_url.txt").write_text(gist_url)
            logger.info("Gist uploaded: %s", gist_url)
            return f"✅ Gist uploaded: {gist_url}"
        except Exception as exc:  # noqa: BLE001
            logger.error("Gist upload failed: %s", exc)
            return f"❌ Gist upload failed: {exc}"

    # Backwards-compatible stubs so existing call sites still work
    def connect(self, credentials: dict) -> None:  # noqa: ARG002
        """No-op: local sync requires no credentials."""
        logger.debug("connect() called (no-op for local sync)")

    def upload_project(self, project_path: str) -> str:
        """Alias for sync_to_local() + optional Gist upload."""
        result = self.sync_to_local(project_path)
        gist = self.upload_to_gist(project_path)
        if not gist.startswith("ℹ️"):
            result += "\n" + gist
        return result

    def download_project(self, project_id: str, restore_to: str = ".") -> str:
        """Alias for sync_from_local()."""
        return self.sync_from_local(project_id, restore_to)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _load_config() -> dict:
        config_file = Path.home() / ".time_warp" / "config.json"
        if config_file.exists():
            try:
                data = json.loads(config_file.read_text(encoding="utf-8"))
                return data.get("cloud_storage", {})
            except (json.JSONDecodeError, OSError) as exc:
                logger.warning("Cannot read cloud_storage config: %s", exc)
        return {}

    @staticmethod
    def _collect_source_files(root: Path) -> list[Path]:
        if root.is_file():
            return [root] if root.suffix in _SOURCE_EXTENSIONS else []
        return [
            p for p in root.rglob("*")
            if p.is_file() and p.suffix in _SOURCE_EXTENSIONS
        ]

