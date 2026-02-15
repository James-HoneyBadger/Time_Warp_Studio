"""Cloud Sync Manager - IDE Integration Layer.

This module provides the integration layer between the Time Warp Studio and
the cloud synchronization engine, handling project sync, offline mode,
and user authentication.
"""

import asyncio
import logging
import threading
from datetime import datetime, timezone
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

from .api_server import TimeWarpCloudAPI
from .sync_engine import (
    CloudSyncEngine,
    ConflictResolution,
    FileChange,
    SyncConflict,
    SyncLog,
    SyncStatus,
)

logger = logging.getLogger(__name__)


class CloudAuthStatus(str, Enum):
    """Cloud authentication status."""

    LOGGED_OUT = "logged_out"
    LOGGING_IN = "logging_in"
    AUTHENTICATED = "authenticated"
    TOKEN_EXPIRED = "token_expired"
    AUTH_FAILED = "auth_failed"


class CloudSyncManager:
    """Manages cloud synchronization for Time Warp Studio."""

    def __init__(
        self,
        api_server: TimeWarpCloudAPI | None = None,
        auto_sync: bool = True,
    ):
        """Initialize Cloud Sync Manager.

        Args:
            api_server: Cloud API server instance (creates default if None)
            auto_sync: Enable automatic sync on file changes (default: True)
        """
        self.api_server: TimeWarpCloudAPI = api_server or TimeWarpCloudAPI()

        # Create sync engine with api_server and temp storage path
        import tempfile

        from pathlib import Path

        temp_dir = tempfile.mkdtemp(prefix="time_warp_sync_")
        self.sync_engine = CloudSyncEngine(
            api_client=self.api_server, local_storage_path=Path(temp_dir)
        )
        self.auto_sync: bool = auto_sync

        # Authentication state
        self.auth_status: CloudAuthStatus = CloudAuthStatus.LOGGED_OUT
        self.current_user: Dict[str, Any] | None = None
        self.access_token: str | None = None
        self.refresh_token: str | None = None

        # Project state
        self.current_project: Dict[str, Any] | None = None
        self.project_files: Dict[str, Any] = {}
        self.sync_queue: List[Dict[str, Any]] = []

        # Callbacks
        self._auth_status_changed_callbacks: List[Callable[[CloudAuthStatus], None]] = []
        self._sync_status_changed_callbacks: List[Callable[[SyncStatus], None]] = []
        self._conflict_detected_callbacks: List[Callable[[SyncConflict], None]] = []
        self._sync_complete_callbacks: List[Callable[[SyncStatus], None]] = []

        # Background sync thread
        self._sync_thread: threading.Thread | None = None
        self._sync_running = False

        # Register sync engine callbacks
        self.sync_engine.on_status_changed = self._on_sync_status_changed
        self.sync_engine.on_conflict_detected = self._on_conflict_detected

    # ========================================================================
    # Authentication Methods
    # ========================================================================

    def register_user(
        self, username: str, email: str, password: str, full_name: str | None = None
    ) -> bool:
        """Register a new user on cloud.

        Args:
            username: Username for account
            email: Email address
            password: Password (minimum 8 characters)
            full_name: Optional full name

        Returns:
            True if registration successful, False otherwise
        """
        try:
            # Check if user already exists
            if email in self.api_server.users:
                logger.warning("User %s already registered", email)
                return False

            # Create user via API server
            user_data = {
                "username": username,
                "email": email,
                "password": password,
                "full_name": full_name,
                "role": "student",
            }
            self.api_server._create_user(user_data)
            logger.info("User registered: %s (%s)", username, email)
            return True
        except Exception as e:
            logger.error("Registration failed: %s", e)
            return False

    def login(self, email: str, password: str) -> bool:
        """Login to cloud service.

        Args:
            email: Email address
            password: Password

        Returns:
            True if login successful, False otherwise
        """
        try:
            self.auth_status = CloudAuthStatus.LOGGING_IN
            self._notify_auth_status_changed()

            # Verify credentials
            user = self.api_server.users.get(email)
            if not user or user["password"] != password:
                self.auth_status = CloudAuthStatus.AUTH_FAILED
                self._notify_auth_status_changed()
                logger.error("Login failed: invalid credentials")
                return False

            # Create tokens
            tokens = self.api_server.auth_manager.create_tokens(user["id"])
            self.access_token = tokens["access_token"]
            self.refresh_token = tokens["refresh_token"]
            self.current_user = {
                "id": user["id"],
                "username": user["username"],
                "email": user["email"],
            }

            self.auth_status = CloudAuthStatus.AUTHENTICATED
            self._notify_auth_status_changed()
            logger.info("User logged in: %s", user['username'])
            return True
        except Exception as e:
            self.auth_status = CloudAuthStatus.AUTH_FAILED
            self._notify_auth_status_changed()
            logger.error("Login failed: %s", e)
            return False

    def logout(self) -> bool:
        """Logout from cloud service.

        Returns:
            True if logout successful
        """
        try:
            if self._sync_running:
                self.stop_auto_sync()

            self.access_token = None
            self.refresh_token = None
            self.current_user = None
            self.current_project = None
            self.project_files = {}

            self.auth_status = CloudAuthStatus.LOGGED_OUT
            self._notify_auth_status_changed()
            logger.info("User logged out")
            return True
        except Exception as e:
            logger.error("Logout failed: %s", e)
            return False

    def refresh_tokens(self) -> bool:
        """Refresh authentication tokens.

        Returns:
            True if refresh successful, False otherwise
        """
        try:
            if not self.refresh_token:
                return False

            # Decode refresh token to get user ID
            payload = self.api_server.auth_manager.verify_token(self.refresh_token)
            user_id = payload.get("sub")

            if not user_id:
                return False

            # Create new tokens
            tokens = self.api_server.auth_manager.create_tokens(user_id)
            self.access_token = tokens["access_token"]
            self.refresh_token = tokens["refresh_token"]

            logger.info("Tokens refreshed")
            return True
        except Exception as e:
            logger.error("Token refresh failed: %s", e)
            self.auth_status = CloudAuthStatus.TOKEN_EXPIRED
            self._notify_auth_status_changed()
            return False

    # ========================================================================
    # Project Sync Methods
    # ========================================================================

    def create_cloud_project(
        self, name: str, language: str, description: str | None = None
    ) -> Optional[str]:
        """Create a new project on cloud.

        Args:
            name: Project name
            language: Programming language
            description: Optional project description

        Returns:
            Project ID if successful, None otherwise
        """
        try:
            if not self.current_user:
                logger.warning("Not authenticated")
                return None

            project_id = str(len(self.api_server.projects) + 1)
            project = {
                "id": project_id,
                "name": name,
                "language": language,
                "description": description,
                "owner_id": self.current_user["id"],
                "is_public": False,
                "created_at": datetime.now(timezone.utc).isoformat(),
                "updated_at": datetime.now(timezone.utc).isoformat(),
                "version": 1,
            }

            self.api_server.projects[project_id] = project
            logger.info("Cloud project created: %s (%s)", name, project_id)
            return project_id
        except Exception as e:
            logger.error("Project creation failed: %s", e)
            return None

    def open_project(self, project_id: str) -> bool:
        """Open a project for syncing.

        Args:
            project_id: ID of project to open

        Returns:
            True if project opened successfully
        """
        try:
            if not self.current_user:
                logger.warning("Not authenticated")
                return False

            project = self.api_server.projects.get(project_id)
            if not project:
                logger.error("Project not found: %s", project_id)
                return False

            self.current_project = project
            self.project_files = {}
            self.sync_queue = []

            logger.info("Project opened: %s", project['name'])
            return True
        except Exception as e:
            logger.error("Failed to open project: %s", e)
            return False

    def close_project(self) -> bool:
        """Close the current project.

        Returns:
            True if project closed successfully
        """
        try:
            if self.current_project:
                project_name = self.current_project.get("name")
                self.current_project = None
                self.project_files = {}
                logger.info("Project closed: %s", project_name)
            return True
        except Exception as e:
            logger.error("Failed to close project: %s", e)
            return False

    def sync_project(self) -> bool:
        """Synchronize the current project with cloud.

        Returns:
            True if sync successful, False otherwise
        """
        try:
            if not self.current_project:
                logger.warning("No project open")
                return False

            # Detect local changes
            local_changes = self._detect_local_changes()

            # Queue changes
            for change in local_changes:
                self.sync_engine.queue_local_change(
                    self.current_project["id"],
                    change.filename,
                    change.content,
                )

            # Start sync (async engine method)
            asyncio.run(
                self.sync_engine.sync_project(self.current_project["id"])
            )

            logger.info("Project synced: %s", self.current_project['name'])
            self._notify_sync_complete()
            return True
        except Exception as e:
            logger.error("Project sync failed: %s", e)
            return False

    def add_file(self, filename: str, content: str, language: str | None = None) -> bool:
        """Add or update a file in the current project.

        Args:
            filename: Name of the file
            content: File content
            language: Programming language (defaults to project language)

        Returns:
            True if file added successfully
        """
        try:
            if not self.current_project:
                logger.warning("No project open")
                return False

            file_id = f"{filename}_{len(self.project_files)}"
            self.project_files[file_id] = {
                "id": file_id,
                "project_id": self.current_project["id"],
                "filename": filename,
                "content": content,
                "language": language or self.current_project.get("language"),
                "version": 1,
                "created_at": datetime.now(timezone.utc).isoformat(),
                "updated_at": datetime.now(timezone.utc).isoformat(),
            }

            # Queue change if auto-sync enabled
            if self.auto_sync:
                self.sync_engine.queue_local_change(
                    self.current_project["id"],
                    filename,
                    content,
                )

            logger.info("File added: %s", filename)
            return True
        except Exception as e:
            logger.error("Failed to add file: %s", e)
            return False

    def remove_file(self, filename: str) -> bool:
        """Remove a file from the current project.

        Args:
            filename: Name of the file to remove

        Returns:
            True if file removed successfully
        """
        try:
            file_id: str | None = None
            for fid, file_data in self.project_files.items():
                if file_data["filename"] == filename:
                    file_id = fid
                    break

            if not file_id:
                logger.warning("File not found: %s", filename)
                return False

            del self.project_files[file_id]

            # Queue change if auto-sync enabled
            if self.auto_sync and self.current_project:
                self.sync_engine.queue_local_change(
                    self.current_project["id"],
                    filename,
                    "",
                )

            logger.info("File removed: %s", filename)
            return True
        except Exception as e:
            logger.error("Failed to remove file: %s", e)
            return False

    # ========================================================================
    # Conflict Resolution Methods
    # ========================================================================

    def get_conflicts(self) -> List[SyncConflict]:
        """Get list of current sync conflicts.

        Returns:
            List of SyncConflict objects
        """
        return self.sync_engine.conflicts

    def resolve_conflict(
        self, conflict_id: str, resolution: ConflictResolution
    ) -> bool:
        """Resolve a sync conflict.

        Args:
            conflict_id: ID of conflict to resolve
            resolution: How to resolve the conflict

        Returns:
            True if conflict resolved successfully
        """
        try:
            # Find the conflict object by ID (uses filename as identifier)
            conflict = next(
                (c for c in self.sync_engine.conflicts if c.filename == conflict_id),
                None,
            )
            if conflict is None:
                logger.warning("Conflict not found: %s", conflict_id)
                return False
            asyncio.run(
                self.sync_engine.resolve_conflict(conflict, resolution)
            )
            logger.info("Conflict resolved: %s (%s)", conflict_id, resolution.value)
            return True
        except Exception as e:
            logger.error("Failed to resolve conflict: %s", e)
            return False

    # ========================================================================
    # Offline Mode Methods
    # ========================================================================

    def enable_offline_mode(self) -> bool:
        """Enable offline mode for local-only changes.

        Returns:
            True if offline mode enabled
        """
        try:
            self.sync_engine.offline_mode = True
            logger.info("Offline mode enabled")
            self._notify_sync_status_changed()
            return True
        except Exception as e:
            logger.error("Failed to enable offline mode: %s", e)
            return False

    def disable_offline_mode(self) -> bool:
        """Disable offline mode and sync pending changes.

        Returns:
            True if offline mode disabled and sync successful
        """
        try:
            self.sync_engine.offline_mode = False
            logger.info("Offline mode disabled")

            # Sync any pending changes
            if self.current_project and self.sync_queue:
                self.sync_project()

            self._notify_sync_status_changed()
            return True
        except Exception as e:
            logger.error("Failed to disable offline mode: %s", e)
            return False

    # ========================================================================
    # Auto-Sync Methods
    # ========================================================================

    def start_auto_sync(self, interval_seconds: int = 30) -> bool:
        """Start automatic background synchronization.

        Args:
            interval_seconds: Sync interval in seconds (default: 30)

        Returns:
            True if auto-sync started
        """
        try:
            if self._sync_running:
                logger.warning("Auto-sync already running")
                return False

            self._sync_running = True
            self._sync_thread = threading.Thread(
                target=self._auto_sync_worker,
                args=(interval_seconds,),
                daemon=True,
            )
            self._sync_thread.start()
            logger.info("Auto-sync started")
            return True
        except Exception as e:
            logger.error("Failed to start auto-sync: %s", e)
            return False

    def stop_auto_sync(self) -> bool:
        """Stop automatic synchronization.

        Returns:
            True if auto-sync stopped
        """
        try:
            self._sync_running = False
            if self._sync_thread:
                self._sync_thread.join(timeout=5)
            logger.info("Auto-sync stopped")
            return True
        except Exception as e:
            logger.error("Failed to stop auto-sync: %s", e)
            return False

    def _auto_sync_worker(self, interval_seconds: int):
        """Background worker for automatic synchronization.

        Args:
            interval_seconds: Sync interval in seconds
        """
        import time

        while self._sync_running:
            try:
                if self.current_project and self.sync_queue:
                    self.sync_project()
            except Exception as e:
                logger.error("Auto-sync error: %s", e)
            time.sleep(interval_seconds)

    # ========================================================================
    # Status and Statistics Methods
    # ========================================================================

    def get_sync_status(self) -> Dict[str, Any]:
        """Get current sync status.

        Returns:
            Current SyncStatus
        """
        return self.sync_engine.get_sync_status()

    def get_sync_history(self, limit: int = 10) -> List["SyncLog"]:
        """Get sync operation history.

        Args:
            limit: Maximum number of records to return

        Returns:
            List of sync history entries
        """
        return self.sync_engine.get_sync_history(limit)

    def get_auth_status(self) -> CloudAuthStatus:
        """Get current authentication status.

        Returns:
            Current CloudAuthStatus
        """
        return self.auth_status

    def get_current_user(self) -> Optional[Dict[str, str]]:
        """Get current authenticated user.

        Returns:
            User info dict if authenticated, None otherwise
        """
        return self.current_user

    def get_current_project(self) -> Optional[Dict[str, Any]]:
        """Get current open project.

        Returns:
            Project dict if open, None otherwise
        """
        return self.current_project

    # ========================================================================
    # Internal Helper Methods
    # ========================================================================

    def _detect_local_changes(self) -> List[FileChange]:
        """Detect changes in local project files.

        Returns:
            List of FileChange objects
        """
        changes = []
        for file_id, file_data in self.project_files.items():
            change = FileChange(
                filename=file_data["filename"],
                content=file_data["content"],
                version=file_data.get("version", 1),
                timestamp=datetime.now(timezone.utc),
            )
            changes.append(change)
        return changes

    def _on_sync_status_changed(self, status: SyncStatus):
        """Handle sync status changes.

        Args:
            status: New sync status
        """
        self._notify_sync_status_changed()

    def _on_conflict_detected(self, conflict: SyncConflict):
        """Handle conflict detection.

        Args:
            conflict: Detected conflict
        """
        self._notify_conflict_detected(conflict)

    # ========================================================================
    # Callback Registration Methods
    # ========================================================================

    def on_auth_status_changed(self, callback: Callable) -> None:
        """Register callback for auth status changes.

        Args:
            callback: Callback function(status: CloudAuthStatus)
        """
        self._auth_status_changed_callbacks.append(callback)

    def on_sync_status_changed(self, callback: Callable) -> None:
        """Register callback for sync status changes.

        Args:
            callback: Callback function(status: SyncStatus)
        """
        self._sync_status_changed_callbacks.append(callback)

    def on_conflict_detected(self, callback: Callable) -> None:
        """Register callback for conflict detection.

        Args:
            callback: Callback function(conflict: SyncConflict)
        """
        self._conflict_detected_callbacks.append(callback)

    def on_sync_complete(self, callback: Callable) -> None:
        """Register callback for sync completion.

        Args:
            callback: Callback function()
        """
        self._sync_complete_callbacks.append(callback)

    def _notify_auth_status_changed(self):
        """Notify all auth status change callbacks."""
        for callback in self._auth_status_changed_callbacks:
            try:
                callback(self.auth_status)
            except Exception as e:
                logger.error("Callback error: %s", e)

    def _notify_sync_status_changed(self):
        """Notify all sync status change callbacks."""
        for callback in self._sync_status_changed_callbacks:
            try:
                callback(self.sync_engine.get_sync_status())
            except Exception as e:
                logger.error("Callback error: %s", e)

    def _notify_conflict_detected(self, conflict: SyncConflict):
        """Notify all conflict detection callbacks.

        Args:
            conflict: Detected conflict
        """
        for callback in self._conflict_detected_callbacks:
            try:
                callback(conflict)
            except Exception as e:
                logger.error("Callback error: %s", e)

    def _notify_sync_complete(self):
        """Notify all sync complete callbacks."""
        for callback in self._sync_complete_callbacks:
            try:
                callback()
            except Exception as e:
                logger.error("Callback error: %s", e)
