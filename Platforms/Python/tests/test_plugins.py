"""Tests for plugin system functionality."""

# The test environment sets sys.path via `tests/conftest.py`. Do not
# modify sys.path here to keep imports at the top of the file.

import tempfile
from pathlib import Path
import json

# Mock is currently unused; keep import for potential future use
# from unittest.mock import Mock

from Platforms.Python.time_warp.plugins import (
    PluginManager,
    PluginManifest,
)


class TestPluginManager:
    """Test plugin manager functionality."""

    def test_plugin_manager_initialization(self):
        """Test plugin manager initialization."""
        with tempfile.TemporaryDirectory() as temp_dir:
            plugin_dirs = [Path(temp_dir)]
            manager = PluginManager(plugin_dirs)

            assert isinstance(manager.plugin_dirs, list)
            assert len(manager.plugin_dirs) == 1
            assert manager.plugin_dirs[0] == Path(temp_dir)

    def test_discover_plugins(self):
        """Test plugin discovery."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)

            # Create a mock plugin directory with plugin.json
            plugin_dir = temp_path / "test_plugin"
            plugin_dir.mkdir()

            plugin_json = plugin_dir / "plugin.json"
            plugin_text = '{"name": "Test Plugin", "version": "1.0.0"}'
            plugin_json.write_text(plugin_text)

            # Create another directory without plugin.json (should be ignored)
            regular_dir = temp_path / "regular_dir"
            regular_dir.mkdir()

            manager = PluginManager([temp_path])
            discovered = manager.discover_plugins()

            assert "test_plugin" in discovered
            assert len(discovered) == 1

    def test_load_plugin_manifest(self):
        """Test loading plugin manifest."""
        with tempfile.TemporaryDirectory() as temp_dir:
            temp_path = Path(temp_dir)

            # Create plugin directory and manifest
            plugin_dir = temp_path / "test_plugin"
            plugin_dir.mkdir()

            manifest_data = {
                "name": "Test Plugin",
                "version": "1.0.0",
                "description": "A test plugin",
                "author": "Test Author",
                "entry_point": "__init__.py",
                "dependencies": [],
                "permissions": ["ui_access"],
                "languages": ["basic"],
                "ui_hooks": ["menu_bar"],
            }

            plugin_json = plugin_dir / "plugin.json"
            plugin_json.write_text(json.dumps(manifest_data))

            manager = PluginManager([temp_path])
            manifest = manager.load_plugin_manifest("test_plugin")

            assert manifest is not None
            assert manifest.name == "Test Plugin"
            assert manifest.version == "1.0.0"
            assert manifest.author == "Test Author"
            assert manifest.languages == ["basic"]

    def test_validate_plugin_manifest(self):
        """Test plugin manifest validation."""
        manager = PluginManager([])

        # Valid manifest
        valid_manifest = PluginManifest(
            name="Test Plugin",
            version="1.0.0",
            description="Test",
            author="Author",
            entry_point="__init__.py",
            dependencies=[],
            permissions=[],
            languages=[],
            ui_hooks=[],
        )
        assert manager.validate_plugin(valid_manifest)

        # Invalid manifest (missing name)
        invalid_manifest = PluginManifest(
            name="",
            version="1.0.0",
            description="Test",
            author="Author",
            entry_point="__init__.py",
            dependencies=[],
            permissions=[],
            languages=[],
            ui_hooks=[],
        )
        assert not manager.validate_plugin(invalid_manifest)

    def test_get_loaded_plugins(self):
        """Test getting loaded plugins list."""
        manager = PluginManager([])
        loaded = manager.get_loaded_plugins()

        assert isinstance(loaded, list)
        assert len(loaded) == 0  # No plugins loaded initially

    def test_get_plugin_info(self):
        """Test getting plugin information."""
        manager = PluginManager([])

        # Non-existent plugin
        info = manager.get_plugin_info("nonexistent")
        assert info is None

        # Would test loaded plugin info, but requires full plugin loading setup


class TestPluginManifest:
    """Test plugin manifest dataclass."""

    def test_plugin_manifest_creation(self):
        """Test creating plugin manifest."""
        manifest = PluginManifest(
            name="Test Plugin",
            version="1.0.0",
            description="A test plugin",
            author="Test Author",
            entry_point="__init__.py",
            dependencies=["requests"],
            permissions=["ui_access", "file_access"],
            languages=["basic", "logo"],
            ui_hooks=["menu_bar", "toolbar"],
        )

        assert manifest.name == "Test Plugin"
        assert manifest.version == "1.0.0"
        assert manifest.dependencies == ["requests"]
        assert manifest.permissions == ["ui_access", "file_access"]
        assert manifest.languages == ["basic", "logo"]
        assert manifest.ui_hooks == ["menu_bar", "toolbar"]
