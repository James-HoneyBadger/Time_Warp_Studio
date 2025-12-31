"""Tests for Cloud Sync Manager - IDE Integration Layer."""

import pytest
from datetime import datetime, timezone
from time_warp.cloud.cloud_sync_manager import CloudSyncManager, CloudAuthStatus
from time_warp.cloud.sync_engine import SyncStatus, FileChange, ConflictResolution


@pytest.fixture
def sync_manager():
    """Create a CloudSyncManager instance for testing."""
    return CloudSyncManager()


class TestCloudSyncManagerAuth:
    """Tests for authentication functionality."""

    def test_manager_initialization(self, sync_manager):
        """Test CloudSyncManager initialization."""
        assert sync_manager.auth_status == CloudAuthStatus.LOGGED_OUT
        assert sync_manager.current_user is None
        assert sync_manager.current_project is None
        assert sync_manager.auto_sync is True

    def test_user_registration(self, sync_manager):
        """Test user registration."""
        result = sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
            full_name="Test User",
        )
        assert result is True

    def test_login_success(self, sync_manager):
        """Test successful login."""
        # Register first
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )

        # Login
        result = sync_manager.login("test@example.com", "password123")
        assert result is True
        assert sync_manager.auth_status == CloudAuthStatus.AUTHENTICATED
        assert sync_manager.current_user is not None
        assert sync_manager.current_user["email"] == "test@example.com"

    def test_login_failure_invalid_email(self, sync_manager):
        """Test login failure with invalid email."""
        result = sync_manager.login("nonexistent@example.com", "password123")
        assert result is False
        assert sync_manager.auth_status == CloudAuthStatus.AUTH_FAILED

    def test_login_failure_wrong_password(self, sync_manager):
        """Test login failure with wrong password."""
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )

        result = sync_manager.login("test@example.com", "wrongpassword")
        assert result is False
        assert sync_manager.auth_status == CloudAuthStatus.AUTH_FAILED

    def test_logout(self, sync_manager):
        """Test logout."""
        # Login first
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")
        assert sync_manager.auth_status == CloudAuthStatus.AUTHENTICATED

        # Logout
        result = sync_manager.logout()
        assert result is True
        assert sync_manager.auth_status == CloudAuthStatus.LOGGED_OUT
        assert sync_manager.current_user is None
        assert sync_manager.access_token is None

    def test_token_refresh(self, sync_manager):
        """Test token refresh."""
        # Login first
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")

        old_token = sync_manager.access_token
        result = sync_manager.refresh_tokens()
        assert result is True
        assert sync_manager.access_token is not None
        # Token refresh should succeed (new tokens created)
        assert sync_manager.refresh_token is not None

    def test_auth_status_callback(self, sync_manager):
        """Test authentication status callbacks."""
        callback_statuses = []

        def auth_callback(status):
            callback_statuses.append(status)

        sync_manager.on_auth_status_changed(auth_callback)

        # Register and login
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")

        # Check callbacks were called
        assert CloudAuthStatus.LOGGING_IN in callback_statuses
        assert CloudAuthStatus.AUTHENTICATED in callback_statuses


class TestCloudSyncManagerProjects:
    """Tests for project management functionality."""

    @pytest.fixture
    def authenticated_manager(self, sync_manager):
        """Create authenticated sync manager."""
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")
        return sync_manager

    def test_create_cloud_project(self, authenticated_manager):
        """Test cloud project creation."""
        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
            description="A test project",
        )
        assert project_id is not None
        assert isinstance(project_id, str)

    def test_create_project_without_auth(self, sync_manager):
        """Test project creation without authentication."""
        project_id = sync_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        assert project_id is None

    def test_open_project(self, authenticated_manager):
        """Test opening a project."""
        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )

        result = authenticated_manager.open_project(project_id)
        assert result is True
        assert authenticated_manager.current_project is not None
        assert authenticated_manager.current_project["name"] == "Test Project"

    def test_open_nonexistent_project(self, authenticated_manager):
        """Test opening a nonexistent project."""
        result = authenticated_manager.open_project("nonexistent_id")
        assert result is False

    def test_close_project(self, authenticated_manager):
        """Test closing a project."""
        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        authenticated_manager.open_project(project_id)

        result = authenticated_manager.close_project()
        assert result is True
        assert authenticated_manager.current_project is None

    def test_add_file_to_project(self, authenticated_manager):
        """Test adding a file to a project."""
        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        authenticated_manager.open_project(project_id)

        result = authenticated_manager.add_file(
            filename="test.bas",
            content="PRINT 'Hello, World!'",
            language="BASIC",
        )
        assert result is True
        assert len(authenticated_manager.project_files) > 0

    def test_add_file_without_project(self, authenticated_manager):
        """Test adding a file without opening a project."""
        result = authenticated_manager.add_file(
            filename="test.bas",
            content="PRINT 'Hello, World!'",
        )
        assert result is False

    def test_remove_file_from_project(self, authenticated_manager):
        """Test removing a file from a project."""
        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        authenticated_manager.open_project(project_id)
        authenticated_manager.add_file(
            filename="test.bas",
            content="PRINT 'Hello, World!'",
        )

        result = authenticated_manager.remove_file("test.bas")
        assert result is True
        assert len(authenticated_manager.project_files) == 0

    def test_remove_nonexistent_file(self, authenticated_manager):
        """Test removing a file that doesn't exist."""
        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        authenticated_manager.open_project(project_id)

        result = authenticated_manager.remove_file("nonexistent.bas")
        assert result is False


class TestCloudSyncManagerSync:
    """Tests for synchronization functionality."""

    @pytest.fixture
    def authenticated_manager_with_project(self, sync_manager):
        """Create authenticated sync manager with open project."""
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")
        project_id = sync_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        sync_manager.open_project(project_id)
        return sync_manager

    def test_sync_project(self, authenticated_manager_with_project):
        """Test project synchronization."""
        authenticated_manager_with_project.add_file(
            filename="test.bas",
            content="PRINT 'Hello'",
        )

        result = authenticated_manager_with_project.sync_project()
        assert result is True

    def test_sync_without_project(self, sync_manager):
        """Test sync without open project."""
        result = sync_manager.sync_project()
        assert result is False

    def test_get_sync_status(self, authenticated_manager_with_project):
        """Test getting sync status."""
        status = authenticated_manager_with_project.get_sync_status()
        assert isinstance(status, SyncStatus)

    def test_get_sync_history(self, authenticated_manager_with_project):
        """Test getting sync history."""
        history = authenticated_manager_with_project.get_sync_history()
        assert isinstance(history, list)

    def test_detect_local_changes(self, authenticated_manager_with_project):
        """Test detecting local changes."""
        authenticated_manager_with_project.add_file(
            filename="test.bas",
            content="PRINT 'Hello'",
        )

        changes = authenticated_manager_with_project._detect_local_changes()
        assert len(changes) > 0
        assert isinstance(changes[0], FileChange)

    def test_sync_status_callback(self, authenticated_manager_with_project):
        """Test sync status callbacks."""
        callback_statuses = []

        def sync_callback(status):
            callback_statuses.append(status)

        authenticated_manager_with_project.on_sync_status_changed(sync_callback)
        authenticated_manager_with_project.sync_project()

        assert len(callback_statuses) > 0


class TestCloudSyncManagerConflicts:
    """Tests for conflict resolution functionality."""

    @pytest.fixture
    def sync_manager_with_conflict(self, sync_manager):
        """Create sync manager with detected conflict."""
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")
        project_id = sync_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        sync_manager.open_project(project_id)

        # Create a conflict manually
        from time_warp.cloud.sync_engine import SyncConflict, FileChange
        local_change = FileChange(
            filename="test.bas",
            content="LOCAL",
            timestamp=datetime.now(timezone.utc),
            version=2,
        )
        cloud_change = FileChange(
            filename="test.bas",
            content="CLOUD",
            timestamp=datetime.now(timezone.utc),
            version=1,
        )
        conflict = SyncConflict(
            filename="test.bas",
            local_version=local_change,
            cloud_version=cloud_change,
        )
        sync_manager.sync_engine.conflicts.append(conflict)
        return sync_manager

    def test_get_conflicts(self, sync_manager_with_conflict):
        """Test retrieving conflicts."""
        conflicts = sync_manager_with_conflict.get_conflicts()
        assert len(conflicts) > 0
        assert conflicts[0].filename == "test.bas"

    def test_resolve_conflict_local_wins(self, sync_manager_with_conflict):
        """Test resolving conflict with LOCAL_WINS strategy."""
        conflict_id = sync_manager_with_conflict.get_conflicts()[0].id

        result = sync_manager_with_conflict.resolve_conflict(
            conflict_id,
            ConflictResolution.LOCAL_WINS,
        )
        assert result is True

    def test_resolve_conflict_cloud_wins(self, sync_manager_with_conflict):
        """Test resolving conflict with CLOUD_WINS strategy."""
        conflict_id = sync_manager_with_conflict.get_conflicts()[0].id

        result = sync_manager_with_conflict.resolve_conflict(
            conflict_id,
            ConflictResolution.CLOUD_WINS,
        )
        assert result is True

    def test_conflict_detected_callback(self, sync_manager):
        """Test conflict detection callbacks."""
        detected_conflicts = []

        def conflict_callback(conflict):
            detected_conflicts.append(conflict)

        sync_manager.on_conflict_detected(conflict_callback)

        # Create conflict
        from time_warp.cloud.sync_engine import SyncConflict, FileChange
        local_change = FileChange(
            filename="test.bas",
            content="LOCAL",
            timestamp=datetime.now(timezone.utc),
            version=2,
        )
        cloud_change = FileChange(
            filename="test.bas",
            content="CLOUD",
            timestamp=datetime.now(timezone.utc),
            version=1,
        )
        conflict = SyncConflict(
            filename="test.bas",
            local_version=local_change,
            cloud_version=cloud_change,
        )
        if sync_manager.sync_engine.on_conflict_detected:
            sync_manager.sync_engine.on_conflict_detected(conflict)

        assert len(detected_conflicts) > 0


class TestCloudSyncManagerOfflineMode:
    """Tests for offline mode functionality."""

    @pytest.fixture
    def authenticated_manager_with_project(self, sync_manager):
        """Create authenticated sync manager with open project."""
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")
        project_id = sync_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        sync_manager.open_project(project_id)
        return sync_manager

    def test_enable_offline_mode(self, authenticated_manager_with_project):
        """Test enabling offline mode."""
        result = authenticated_manager_with_project.enable_offline_mode()
        assert result is True
        assert authenticated_manager_with_project.sync_engine.offline_mode is True

    def test_disable_offline_mode(self, authenticated_manager_with_project):
        """Test disabling offline mode."""
        authenticated_manager_with_project.enable_offline_mode()

        result = authenticated_manager_with_project.disable_offline_mode()
        assert result is True
        assert authenticated_manager_with_project.sync_engine.offline_mode is False

    def test_offline_mode_auto_sync(self, authenticated_manager_with_project):
        """Test auto-sync queuing in offline mode."""
        authenticated_manager_with_project.enable_offline_mode()
        authenticated_manager_with_project.add_file(
            filename="test.bas",
            content="PRINT 'Offline'",
        )

        # Changes should be queued
        assert len(authenticated_manager_with_project.sync_engine.change_queue) > 0


class TestCloudSyncManagerAutoSync:
    """Tests for automatic synchronization."""

    @pytest.fixture
    def authenticated_manager_with_project(self, sync_manager):
        """Create authenticated sync manager with open project."""
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")
        project_id = sync_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        sync_manager.open_project(project_id)
        return sync_manager

    def test_start_auto_sync(self, authenticated_manager_with_project):
        """Test starting auto-sync."""
        result = authenticated_manager_with_project.start_auto_sync(interval_seconds=1)
        assert result is True
        assert authenticated_manager_with_project._sync_running is True

        # Clean up
        authenticated_manager_with_project.stop_auto_sync()

    def test_stop_auto_sync(self, authenticated_manager_with_project):
        """Test stopping auto-sync."""
        authenticated_manager_with_project.start_auto_sync()

        result = authenticated_manager_with_project.stop_auto_sync()
        assert result is True
        assert authenticated_manager_with_project._sync_running is False

    def test_auto_sync_already_running(self, authenticated_manager_with_project):
        """Test starting auto-sync when already running."""
        authenticated_manager_with_project.start_auto_sync()

        result = authenticated_manager_with_project.start_auto_sync()
        assert result is False

        # Clean up
        authenticated_manager_with_project.stop_auto_sync()


class TestCloudSyncManagerStatus:
    """Tests for status and statistics methods."""

    @pytest.fixture
    def authenticated_manager(self, sync_manager):
        """Create authenticated sync manager."""
        sync_manager.register_user(
            username="testuser",
            email="test@example.com",
            password="password123",
        )
        sync_manager.login("test@example.com", "password123")
        return sync_manager

    def test_get_auth_status(self, authenticated_manager):
        """Test getting auth status."""
        status = authenticated_manager.get_auth_status()
        assert status == CloudAuthStatus.AUTHENTICATED

    def test_get_current_user(self, authenticated_manager):
        """Test getting current user."""
        user = authenticated_manager.get_current_user()
        assert user is not None
        assert user["email"] == "test@example.com"

    def test_get_current_project(self, authenticated_manager):
        """Test getting current project."""
        assert authenticated_manager.get_current_project() is None

        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        authenticated_manager.open_project(project_id)

        project = authenticated_manager.get_current_project()
        assert project is not None
        assert project["name"] == "Test Project"

    def test_sync_complete_callback(self, authenticated_manager):
        """Test sync complete callbacks."""
        project_id = authenticated_manager.create_cloud_project(
            name="Test Project",
            language="BASIC",
        )
        authenticated_manager.open_project(project_id)

        sync_completed = []

        def sync_callback():
            sync_completed.append(True)

        authenticated_manager.on_sync_complete(sync_callback)
        authenticated_manager.sync_project()

        assert len(sync_completed) > 0
