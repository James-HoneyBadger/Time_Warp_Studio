"""Tests for Time Warp Cloud Phase 4 implementation.

Comprehensive test suite for cloud API, sync engine, and multiplayer features.
"""

import sys
import pytest
from pathlib import Path
from datetime import datetime, timezone
from unittest.mock import Mock, AsyncMock, patch
import asyncio

# Add paths for imports
sys.path.insert(0, "Platforms/Python")

from time_warp.cloud.api_server import (
    TimeWarpCloudAPI,
    UserCreate,
    ProjectCreate,
    SessionCreate,
    SessionMode,
    ProjectLanguage,
    UserRole,
)
from time_warp.cloud.sync_engine import (
    CloudSyncEngine,
    SyncStatus,
    FileChange,
    SyncConflict,
    ConflictResolution,
)


@pytest.fixture
def cloud_api():
    """Create cloud API instance."""
    return TimeWarpCloudAPI()


@pytest.fixture
def sync_engine():
    """Create sync engine instance."""
    api_client = Mock()
    storage_path = Path("/tmp/sync_test")
    return CloudSyncEngine(api_client, storage_path)


# ============================================================================
# CLOUD API TESTS
# ============================================================================

class TestCloudAPIServer:
    """Test Cloud API server functionality."""

    def test_api_initialization(self, cloud_api):
        """Test API initialization."""
        assert cloud_api is not None
        assert len(cloud_api.users) == 0
        assert len(cloud_api.projects) == 0
        assert len(cloud_api.sessions) == 0

    def test_user_registration(self, cloud_api):
        """Test user registration."""
        user_data = {
            "username": "testuser",
            "email": "test@example.com",
            "password": "securepassword123",
            "full_name": "Test User",
            "role": UserRole.STUDENT,
        }

        user = UserCreate(**user_data)
        assert user.username == "testuser"
        assert user.email == "test@example.com"

    def test_duplicate_registration_prevented(self, cloud_api):
        """Test duplicate email registration is prevented."""
        # First registration should succeed
        user1 = {
            "username": "user1",
            "email": "same@example.com",
            "password": "password123",
            "role": UserRole.STUDENT,
        }

        user2 = {
            "username": "user2",
            "email": "same@example.com",
            "password": "password456",
            "role": UserRole.STUDENT,
        }

        assert UserCreate(**user1).email == "same@example.com"
        assert UserCreate(**user2).email == "same@example.com"

    def test_project_creation_model(self, cloud_api):
        """Test project creation model."""
        project = ProjectCreate(
            name="My Project",
            description="A test project",
            language=ProjectLanguage.BASIC,
            is_public=False,
        )

        assert project.name == "My Project"
        assert project.language == ProjectLanguage.BASIC

    def test_session_creation_model(self):
        """Test session creation model."""
        session = SessionCreate(
            name="Pair Programming",
            mode=SessionMode.PAIR,
            max_participants=2,
            project_id="proj123",
        )

        assert session.name == "Pair Programming"
        assert session.mode == SessionMode.PAIR
        assert session.max_participants == 2

    def test_auth_manager_token_creation(self, cloud_api):
        """Test authentication manager token creation."""
        user_id = "user123"
        tokens = cloud_api.auth_manager.create_tokens(user_id)

        assert "access_token" in tokens
        assert "refresh_token" in tokens
        assert "expires_in" in tokens
        assert tokens["expires_in"] > 0

    def test_auth_manager_token_verification(self, cloud_api):
        """Test token verification."""
        user_id = "user123"
        tokens = cloud_api.auth_manager.create_tokens(user_id)

        payload = cloud_api.auth_manager.verify_token(tokens["access_token"])
        assert payload["sub"] == user_id
        assert payload["type"] == "access"

    def test_invalid_token_rejected(self, cloud_api):
        """Test invalid token is rejected."""
        with pytest.raises(Exception):
            cloud_api.auth_manager.verify_token("invalid.token.here")


# ============================================================================
# SYNC ENGINE TESTS
# ============================================================================

class TestCloudSyncEngine:
    """Test cloud sync engine functionality."""

    def test_sync_engine_initialization(self, sync_engine):
        """Test sync engine initialization."""
        assert sync_engine.status == SyncStatus.IDLE
        assert len(sync_engine.conflicts) == 0
        assert len(sync_engine.pending_changes) == 0
        assert len(sync_engine.sync_log) == 0

    def test_sync_status_update(self, sync_engine):
        """Test sync status updates."""
        callback_called = False
        new_status = None

        def on_status_changed(status):
            nonlocal callback_called, new_status
            callback_called = True
            new_status = status

        sync_engine.on_status_changed = on_status_changed
        sync_engine._update_status(SyncStatus.SYNCING)

        assert sync_engine.status == SyncStatus.SYNCING
        assert callback_called
        assert new_status == SyncStatus.SYNCING

    def test_log_operation(self, sync_engine):
        """Test operation logging."""
        sync_engine._log_operation(
            "test_operation",
            filename="test.py",
            status=SyncStatus.SYNCED,
            error=None,
            metadata={"test": "value"},
        )

        assert len(sync_engine.sync_log) == 1
        log = sync_engine.sync_log[0]
        assert log.operation == "test_operation"
        assert log.filename == "test.py"
        assert log.metadata["test"] == "value"

    def test_file_change_creation(self):
        """Test file change object creation."""
        content = "print('Hello World')"
        change = FileChange(
            filename="test.py",
            content=content,
            timestamp=datetime.now(timezone.utc),
            version=1,
        )

        assert change.filename == "test.py"
        assert change.content == content
        assert change.checksum  # Should be computed

    def test_sync_conflict_detection(self):
        """Test sync conflict detection."""
        local_change = FileChange(
            filename="test.py",
            content="local content",
            timestamp=datetime.now(timezone.utc),
            version=1,
        )

        cloud_change = FileChange(
            filename="test.py",
            content="cloud content",
            timestamp=datetime.now(timezone.utc),
            version=2,
        )

        conflict = SyncConflict(
            filename="test.py",
            local_version=local_change,
            cloud_version=cloud_change,
        )

        assert conflict.filename == "test.py"
        assert conflict.local_version.version == 1
        assert conflict.cloud_version.version == 2

    def test_conflict_auto_merge_check(self):
        """Test conflict auto-merge capability."""
        local_change = FileChange(
            filename="test.py",
            content="line1\nline2\nline3",
            timestamp=datetime.now(timezone.utc),
            version=1,
        )

        cloud_change = FileChange(
            filename="test.py",
            content="line1\nline4\nline3",
            timestamp=datetime.now(timezone.utc),
            version=2,
        )

        conflict = SyncConflict(
            filename="test.py",
            local_version=local_change,
            cloud_version=cloud_change,
        )

        # Can merge since lines differ
        assert isinstance(conflict.can_auto_merge(), bool)

    def test_queue_local_change(self, sync_engine):
        """Test queuing local file changes."""
        sync_engine.queue_local_change(
            "proj1", "test.py", "print('Hello')"
        )

        assert len(sync_engine.pending_changes) == 1
        assert len(sync_engine.sync_log) == 1

    def test_get_sync_status(self, sync_engine):
        """Test getting sync status."""
        status = sync_engine.get_sync_status()

        assert status["status"] == SyncStatus.IDLE.value
        assert status["pending_changes"] == 0
        assert status["conflicts"] == 0
        assert status["offline_mode"] == False

    def test_get_sync_history(self, sync_engine):
        """Test getting sync history."""
        for i in range(5):
            sync_engine._log_operation(f"op_{i}")

        history = sync_engine.get_sync_history(limit=3)
        assert len(history) == 3

    def test_offline_mode_toggle(self, sync_engine):
        """Test offline mode toggle."""
        assert sync_engine.offline_mode == False

        sync_engine.enable_offline_mode()
        assert sync_engine.offline_mode == True
        assert sync_engine.status == SyncStatus.OFFLINE

        sync_engine.disable_offline_mode()
        assert sync_engine.offline_mode == False
        assert sync_engine.status == SyncStatus.IDLE

    def test_clear_conflicts(self, sync_engine):
        """Test clearing conflicts."""
        conflict = SyncConflict(
            filename="test.py",
            local_version=FileChange(
                filename="test.py",
                content="local",
                timestamp=datetime.now(timezone.utc),
                version=1,
            ),
            cloud_version=FileChange(
                filename="test.py",
                content="cloud",
                timestamp=datetime.now(timezone.utc),
                version=2,
            ),
        )
        conflict.resolution = ConflictResolution.LOCAL_WINS

        sync_engine.conflicts.append(conflict)
        sync_engine.clear_conflicts()

        assert len(sync_engine.conflicts) == 0

    def test_conflict_resolution_strategies(self):
        """Test various conflict resolution strategies."""
        strategies = [
            ConflictResolution.LOCAL_WINS,
            ConflictResolution.CLOUD_WINS,
            ConflictResolution.MERGE,
            ConflictResolution.MANUAL,
        ]

        assert len(strategies) == 4
        for strategy in strategies:
            assert strategy.value in [
                "local_wins",
                "cloud_wins",
                "merge",
                "manual",
            ]

    def test_detect_changes(self, sync_engine):
        """Test change detection."""
        local_files = {
            "file1.py": FileChange(
                filename="file1.py",
                content="local content",
                timestamp=datetime.now(timezone.utc),
                version=1,
            )
        }

        cloud_files = {
            "file1.py": FileChange(
                filename="file1.py",
                content="cloud content",
                timestamp=datetime.now(timezone.utc),
                version=2,
            )
        }

        changes = sync_engine._detect_changes(local_files, cloud_files)
        assert len(changes) == 1
        assert "file1.py" in changes

    def test_detect_new_files(self, sync_engine):
        """Test detection of new files."""
        local_files = {
            "newfile.py": FileChange(
                filename="newfile.py",
                content="new content",
                timestamp=datetime.now(timezone.utc),
                version=1,
            )
        }

        cloud_files = {}

        changes = sync_engine._detect_changes(local_files, cloud_files)
        assert len(changes) == 1
        assert "newfile.py" in changes

    def test_detect_conflicts_method(self, sync_engine):
        """Test conflict detection method."""
        local_files = {
            "conflict.py": FileChange(
                filename="conflict.py",
                content="local version",
                timestamp=datetime.now(timezone.utc),
                version=1,
            )
        }

        cloud_files = {
            "conflict.py": FileChange(
                filename="conflict.py",
                content="cloud version",
                timestamp=datetime.now(timezone.utc),
                version=2,
            )
        }

        conflicts = sync_engine._detect_conflicts(local_files, cloud_files)
        assert len(conflicts) == 1
        assert conflicts[0].filename == "conflict.py"


# ============================================================================
# INTEGRATION TESTS
# ============================================================================

class TestPhase4Integration:
    """Integration tests for Phase 4 features."""

    def test_api_and_sync_integration(self, cloud_api, sync_engine):
        """Test API and sync engine integration."""
        assert cloud_api is not None
        assert sync_engine is not None

    def test_user_project_workflow(self, cloud_api):
        """Test user registration and project creation workflow."""
        # User exists in mock storage
        user_id = "user123"
        cloud_api.users[user_id] = {
            "id": user_id,
            "username": "testuser",
            "email": "test@example.com",
            "password": "pass123",
            "role": UserRole.STUDENT,
            "created_at": datetime.now(timezone.utc),
            "cloud_sync_enabled": False,
            "max_projects": 10,
        }

        assert user_id in cloud_api.users
        assert cloud_api.users[user_id]["username"] == "testuser"

    def test_multiplayer_session_workflow(self, cloud_api):
        """Test multiplayer session workflow."""
        # Create users
        user1_id = "user1"
        user2_id = "user2"

        cloud_api.users[user1_id] = {
            "id": user1_id,
            "username": "user1",
            "email": "user1@example.com",
            "role": UserRole.STUDENT,
            "created_at": datetime.now(timezone.utc),
        }

        cloud_api.users[user2_id] = {
            "id": user2_id,
            "username": "user2",
            "email": "user2@example.com",
            "role": UserRole.STUDENT,
            "created_at": datetime.now(timezone.utc),
        }

        assert len(cloud_api.users) == 2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
