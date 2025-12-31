"""Cloud Sync Engine for Time Warp IDE.

Handles synchronization between local IDE and cloud backend, including
conflict resolution, offline support, and real-time updates.
"""

from dataclasses import dataclass, field
from typing import Optional, List, Dict, Any, Callable
from enum import Enum
from datetime import datetime, timezone
import json
import hashlib
import asyncio
from pathlib import Path


class SyncStatus(str, Enum):
    """Sync status states."""
    IDLE = "idle"
    SYNCING = "syncing"
    SYNCED = "synced"
    CONFLICT = "conflict"
    OFFLINE = "offline"
    ERROR = "error"


class ConflictResolution(str, Enum):
    """Conflict resolution strategies."""
    LOCAL_WINS = "local_wins"
    CLOUD_WINS = "cloud_wins"
    MERGE = "merge"
    MANUAL = "manual"


@dataclass
class FileChange:
    """Represents a file change."""
    filename: str
    content: str
    timestamp: datetime
    version: int
    checksum: str = field(default="")

    def __post_init__(self):
        """Compute checksum after init."""
        if not self.checksum:
            self.checksum = hashlib.md5(self.content.encode()).hexdigest()


@dataclass
class SyncConflict:
    """Represents a sync conflict."""
    filename: str
    local_version: FileChange
    cloud_version: FileChange
    conflict_time: datetime = field(default_factory=lambda: datetime.now(timezone.utc))
    resolution: Optional[ConflictResolution] = None

    def can_auto_merge(self) -> bool:
        """Check if conflict can be auto-merged.

        Returns:
            True if can auto-merge, False otherwise
        """
        # Can merge if changes don't overlap
        local_lines = set(self.local_version.content.split("\n"))
        cloud_lines = set(self.cloud_version.content.split("\n"))

        overlap = local_lines & cloud_lines
        return len(overlap) == 0


@dataclass
class SyncLog:
    """Log entry for sync operations."""
    timestamp: datetime
    operation: str
    filename: Optional[str]
    status: SyncStatus
    error: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class CloudSyncEngine:
    """Manages synchronization with cloud backend."""

    def __init__(
        self,
        api_client,
        local_storage_path: Path,
        offline_mode: bool = False,
        auto_sync_interval: int = 30,
    ):
        """Initialize cloud sync engine.

        Args:
            api_client: Cloud API client instance
            local_storage_path: Path to local project storage
            offline_mode: Enable offline mode
            auto_sync_interval: Auto-sync interval in seconds
        """
        self.api_client = api_client
        self.local_storage_path = Path(local_storage_path)
        self.offline_mode = offline_mode
        self.auto_sync_interval = auto_sync_interval

        # Sync state
        self.status = SyncStatus.IDLE
        self.conflicts: List[SyncConflict] = []
        self.pending_changes: Dict[str, FileChange] = {}
        self.sync_log: List[SyncLog] = []
        self.last_sync: Optional[datetime] = None

        # Callbacks
        self.on_status_changed: Optional[Callable[[SyncStatus], None]] = None
        self.on_conflict_detected: Optional[
            Callable[[SyncConflict], None]
        ] = None
        self.on_sync_complete: Optional[Callable[[int], None]] = None

        # Ensure storage path exists
        self.local_storage_path.mkdir(parents=True, exist_ok=True)

    def register_callbacks(
        self,
        on_status_changed: Optional[Callable] = None,
        on_conflict_detected: Optional[Callable] = None,
        on_sync_complete: Optional[Callable] = None,
    ):
        """Register sync callbacks.

        Args:
            on_status_changed: Called when sync status changes
            on_conflict_detected: Called when conflict is detected
            on_sync_complete: Called when sync completes
        """
        if on_status_changed:
            self.on_status_changed = on_status_changed
        if on_conflict_detected:
            self.on_conflict_detected = on_conflict_detected
        if on_sync_complete:
            self.on_sync_complete = on_sync_complete

    def _update_status(self, new_status: SyncStatus):
        """Update sync status and trigger callback.

        Args:
            new_status: New sync status
        """
        self.status = new_status
        if self.on_status_changed:
            self.on_status_changed(new_status)

    def _log_operation(
        self,
        operation: str,
        filename: Optional[str] = None,
        status: Optional[SyncStatus] = None,
        error: Optional[str] = None,
        metadata: Optional[Dict] = None,
    ):
        """Log a sync operation.

        Args:
            operation: Operation name
            filename: Affected filename
            status: Operation status
            error: Error message if any
            metadata: Additional metadata
        """
        log_entry = SyncLog(
            timestamp=datetime.now(timezone.utc),
            operation=operation,
            filename=filename,
            status=status or self.status,
            error=error,
            metadata=metadata or {},
        )
        self.sync_log.append(log_entry)

    async def sync_project(self, project_id: str) -> bool:
        """Sync entire project with cloud.

        Args:
            project_id: Project ID to sync

        Returns:
            True if sync successful, False otherwise
        """
        self._update_status(SyncStatus.SYNCING)
        self._log_operation("sync_project", metadata={"project_id": project_id})

        try:
            if self.offline_mode:
                return await self._sync_offline(project_id)
            else:
                return await self._sync_online(project_id)

        except Exception as e:
            self._update_status(SyncStatus.ERROR)
            self._log_operation(
                "sync_project",
                status=SyncStatus.ERROR,
                error=str(e),
                metadata={"project_id": project_id},
            )
            return False

    async def _sync_online(self, project_id: str) -> bool:
        """Sync with cloud backend.

        Args:
            project_id: Project ID to sync

        Returns:
            True if successful
        """
        # Get local files
        local_files = self._get_local_files(project_id)

        # Get cloud files
        try:
            cloud_files = await self.api_client.get_project_files(project_id)
        except Exception as e:
            self._log_operation(
                "get_cloud_files",
                status=SyncStatus.ERROR,
                error=str(e),
            )
            return False

        # Detect changes and conflicts
        changes = self._detect_changes(local_files, cloud_files)
        conflicts = self._detect_conflicts(local_files, cloud_files)

        # Handle conflicts
        if conflicts:
            for conflict in conflicts:
                self._log_operation(
                    "conflict_detected",
                    filename=conflict.filename,
                    status=SyncStatus.CONFLICT,
                )
                if self.on_conflict_detected:
                    self.on_conflict_detected(conflict)
                self.conflicts.append(conflict)

        # Upload local changes
        uploaded = 0
        for filename, change in changes.items():
            try:
                await self.api_client.update_file(
                    project_id,
                    filename,
                    change.content,
                    change.version,
                )
                uploaded += 1
                self._log_operation(
                    "file_uploaded",
                    filename=filename,
                    status=SyncStatus.SYNCED,
                )
            except Exception as e:
                self._log_operation(
                    "file_upload_failed",
                    filename=filename,
                    status=SyncStatus.ERROR,
                    error=str(e),
                )

        self.last_sync = datetime.now(timezone.utc)
        self._update_status(SyncStatus.SYNCED)

        if self.on_sync_complete:
            self.on_sync_complete(uploaded)

        return True

    async def _sync_offline(self, project_id: str) -> bool:
        """Sync in offline mode (queue changes).

        Args:
            project_id: Project ID to sync

        Returns:
            True (always succeeds in offline mode)
        """
        self._log_operation(
            "offline_sync",
            metadata={"project_id": project_id, "queued_changes": len(self.pending_changes)},
        )
        self._update_status(SyncStatus.OFFLINE)
        return True

    async def resolve_conflict(
        self,
        conflict: SyncConflict,
        resolution: ConflictResolution,
    ) -> bool:
        """Resolve a sync conflict.

        Args:
            conflict: Conflict to resolve
            resolution: Resolution strategy

        Returns:
            True if resolved successfully
        """
        self._log_operation(
            "resolve_conflict",
            filename=conflict.filename,
            metadata={"resolution": resolution.value},
        )

        conflict.resolution = resolution

        if resolution == ConflictResolution.LOCAL_WINS:
            # Keep local version
            return True
        elif resolution == ConflictResolution.CLOUD_WINS:
            # Overwrite with cloud version
            return await self._apply_cloud_version(conflict)
        elif resolution == ConflictResolution.MERGE:
            # Attempt merge
            merged = await self._merge_versions(conflict)
            return merged is not None
        elif resolution == ConflictResolution.MANUAL:
            # Manual resolution required
            return False

        return False

    async def _apply_cloud_version(self, conflict: SyncConflict) -> bool:
        """Apply cloud version to local file.

        Args:
            conflict: Conflict to resolve

        Returns:
            True if successful
        """
        try:
            local_file = self.local_storage_path / conflict.filename
            local_file.write_text(conflict.cloud_version.content)
            return True
        except Exception as e:
            self._log_operation(
                "apply_cloud_version",
                filename=conflict.filename,
                status=SyncStatus.ERROR,
                error=str(e),
            )
            return False

    async def _merge_versions(
        self, conflict: SyncConflict
    ) -> Optional[str]:
        """Merge conflicting versions.

        Args:
            conflict: Conflict to merge

        Returns:
            Merged content or None if merge failed
        """
        local_lines = conflict.local_version.content.split("\n")
        cloud_lines = conflict.cloud_version.content.split("\n")

        try:
            # Simple 3-way merge
            merged_lines = []
            for local_line, cloud_line in zip(local_lines, cloud_lines):
                if local_line == cloud_line:
                    merged_lines.append(local_line)
                else:
                    # Keep both with markers
                    merged_lines.append(f"<<<<<<< LOCAL\n{local_line}")
                    merged_lines.append(f"=======\n{cloud_line}")
                    merged_lines.append(">>>>>>> CLOUD")

            merged_content = "\n".join(merged_lines)
            return merged_content

        except Exception as e:
            self._log_operation(
                "merge_failed",
                filename=conflict.filename,
                status=SyncStatus.ERROR,
                error=str(e),
            )
            return None

    def _get_local_files(self, project_id: str) -> Dict[str, FileChange]:
        """Get all local files for a project.

        Args:
            project_id: Project ID

        Returns:
            Dictionary of filename -> FileChange
        """
        project_path = self.local_storage_path / project_id
        if not project_path.exists():
            return {}

        files = {}
        for file_path in project_path.rglob("*"):
            if file_path.is_file():
                relative_path = file_path.relative_to(project_path)
                content = file_path.read_text()

                files[str(relative_path)] = FileChange(
                    filename=str(relative_path),
                    content=content,
                    timestamp=datetime.now(timezone.utc),
                    version=1,
                )

        return files

    def _detect_changes(
        self,
        local_files: Dict[str, FileChange],
        cloud_files: Dict[str, FileChange],
    ) -> Dict[str, FileChange]:
        """Detect changes in local files.

        Args:
            local_files: Local files
            cloud_files: Cloud files

        Returns:
            Dictionary of changed files
        """
        changes = {}

        for filename, local_file in local_files.items():
            cloud_file = cloud_files.get(filename)

            if cloud_file is None:
                # New file
                changes[filename] = local_file
            elif local_file.checksum != cloud_file.checksum:
                # Modified file
                changes[filename] = local_file

        return changes

    def _detect_conflicts(
        self,
        local_files: Dict[str, FileChange],
        cloud_files: Dict[str, FileChange],
    ) -> List[SyncConflict]:
        """Detect conflicts between versions.

        Args:
            local_files: Local files
            cloud_files: Cloud files

        Returns:
            List of detected conflicts
        """
        conflicts = []

        for filename, local_file in local_files.items():
            cloud_file = cloud_files.get(filename)

            if cloud_file is not None:
                if (
                    local_file.checksum != cloud_file.checksum
                    and local_file.version < cloud_file.version
                ):
                    conflict = SyncConflict(
                        filename=filename,
                        local_version=local_file,
                        cloud_version=cloud_file,
                    )
                    conflicts.append(conflict)

        return conflicts

    def queue_local_change(self, project_id: str, filename: str, content: str):
        """Queue a local file change.

        Args:
            project_id: Project ID
            filename: Filename
            content: File content
        """
        change_key = f"{project_id}/{filename}"
        self.pending_changes[change_key] = FileChange(
            filename=filename,
            content=content,
            timestamp=datetime.now(timezone.utc),
            version=1,
        )
        self._log_operation("queue_change", filename=filename)

    def get_sync_status(self) -> Dict[str, Any]:
        """Get current sync status.

        Returns:
            Dictionary with sync status information
        """
        return {
            "status": self.status.value,
            "last_sync": self.last_sync,
            "pending_changes": len(self.pending_changes),
            "conflicts": len(self.conflicts),
            "offline_mode": self.offline_mode,
            "log_entries": len(self.sync_log),
        }

    def get_sync_history(self, limit: int = 100) -> List[SyncLog]:
        """Get sync operation history.

        Args:
            limit: Maximum number of entries to return

        Returns:
            List of sync log entries
        """
        return self.sync_log[-limit:]

    def clear_conflicts(self):
        """Clear all resolved conflicts."""
        self.conflicts = [c for c in self.conflicts if c.resolution is None]
        self._log_operation("conflicts_cleared")

    def enable_offline_mode(self):
        """Enable offline mode."""
        self.offline_mode = True
        self._update_status(SyncStatus.OFFLINE)
        self._log_operation("offline_mode_enabled")

    def disable_offline_mode(self):
        """Disable offline mode."""
        self.offline_mode = False
        self._update_status(SyncStatus.IDLE)
        self._log_operation("offline_mode_disabled")
