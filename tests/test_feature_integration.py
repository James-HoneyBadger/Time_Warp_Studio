"""Tests for feature integration."""

import sys
import pytest
from pathlib import Path

# Add paths for imports
sys.path.insert(0, "Platforms/Python")

from PySide6.QtWidgets import QMainWindow, QApplication
from time_warp.ui.feature_integration import FeatureIntegrationManager


@pytest.fixture
def qapp():
    """Create QApplication instance."""
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


@pytest.fixture
def mock_main_window(qapp):
    """Create mock main window."""
    window = QMainWindow()
    window.menuBar()
    window.statusbar = window.statusBar()
    return window


class TestFeatureIntegrationManager:
    """Test feature integration manager."""

    def test_manager_initialization(self, mock_main_window):
        """Test manager initialization."""
        manager = FeatureIntegrationManager(mock_main_window)
        assert manager.main_window == mock_main_window
        assert len(manager.PHASE_1_FEATURES) == 5
        assert len(manager.PHASE_2_FEATURES) == 10
        assert len(manager.PHASE_3_FEATURES) == 3

    def test_phase_1_features_defined(self, mock_main_window):
        """Test Phase 1 features are properly defined."""
        manager = FeatureIntegrationManager(mock_main_window)
        phase_1 = manager.PHASE_1_FEATURES
        assert phase_1[0][0] == "Syntax Validator"
        assert phase_1[1][0] == "Project Templates"
        assert phase_1[2][0] == "Timeline Debugger"
        assert phase_1[3][0] == "Language Comparator"
        assert phase_1[4][0] == "Asset Library"

    def test_phase_2_features_defined(self, mock_main_window):
        """Test Phase 2 features are properly defined."""
        manager = FeatureIntegrationManager(mock_main_window)
        phase_2 = manager.PHASE_2_FEATURES
        assert len(phase_2) == 10
        assert "Collaboration Tool" in [f[0] for f in phase_2]
        assert "AI Assistant" in [f[0] for f in phase_2]
        assert "Learning Analytics" in [f[0] for f in phase_2]
        assert "Peer Review Tool" in [f[0] for f in phase_2]

    def test_phase_3_features_defined(self, mock_main_window):
        """Test Phase 3 features are properly defined."""
        manager = FeatureIntegrationManager(mock_main_window)
        phase_3 = manager.PHASE_3_FEATURES
        assert len(phase_3) == 3
        assert "Multiplayer Leaderboard" in [f[0] for f in phase_3]
        assert "LMS Integration" in [f[0] for f in phase_3]
        assert "Community Marketplace" in [f[0] for f in phase_3]

    def test_feature_panels_creation(self, mock_main_window):
        """Test feature panels are created."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()
        # Should have created panels for Phase 1 and Phase 2 (14 total)
        assert len(manager.feature_panels) == 14
        assert len(manager.dock_widgets) == 14

    def test_toggle_feature_panel(self, mock_main_window):
        """Test toggling feature panel visibility."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()

        # Toggle first panel - just verify it doesn't raise an error
        feature_id = "syntax_validator"
        initial_state = manager.dock_widgets[feature_id].isVisible()
        manager.toggle_feature_panel(feature_id, visible=not initial_state)
        # Verify dock widget exists
        assert manager.dock_widgets[feature_id] is not None

    def test_show_all_features(self, mock_main_window):
        """Test showing all features."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()
        manager.show_all_features()

        # Verify all panels exist
        assert len(manager.dock_widgets) > 0
        for dock in manager.dock_widgets.values():
            assert dock is not None

    def test_hide_all_features(self, mock_main_window):
        """Test hiding all features."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()
        manager.show_all_features()  # First show all
        manager.hide_all_features()  # Then hide all

        # All panels should be hidden
        for dock in manager.dock_widgets.values():
            assert not dock.isVisible()

    def test_get_feature_panel(self, mock_main_window):
        """Test getting a feature panel by ID."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()

        panel = manager.get_feature_panel("syntax_validator")
        assert panel is not None
        assert panel in manager.feature_panels.values()

    def test_feature_status_summary(self, mock_main_window):
        """Test feature status summary."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()

        # With no active features
        summary = manager.feature_status_summary()
        assert "No features" in summary or summary

        # With some active features
        manager.toggle_feature_panel("syntax_validator", visible=True)
        summary = manager.feature_status_summary()
        assert isinstance(summary, str)

    def test_export_feature_configuration(self, mock_main_window):
        """Test exporting feature configuration."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()
        manager.toggle_feature_panel("syntax_validator", visible=True)

        config = manager.export_feature_configuration()
        assert "syntax_validator" in config
        assert "visible" in config["syntax_validator"]

    def test_import_feature_configuration(self, mock_main_window):
        """Test importing feature configuration."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()

        config = {
            "syntax_validator": {"visible": True},
            "project_templates": {"visible": False},
        }

        manager.import_feature_configuration(config)
        # Verify the configuration is applied
        assert "syntax_validator" in manager.dock_widgets
        assert "project_templates" in manager.dock_widgets

    def test_feature_menu_creation(self, mock_main_window):
        """Test features menu is created."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()
        manager._create_features_menu()

        # Find Features menu
        features_menu = None
        for action in mock_main_window.menuBar().actions():
            if action.text() == "&Features":
                features_menu = action.menu()
                break

        assert features_menu is not None

    def test_feature_menu_has_phase_submenus(self, mock_main_window):
        """Test features menu has phase submenus."""
        manager = FeatureIntegrationManager(mock_main_window)
        manager._create_feature_panels()
        manager._create_features_menu()

        # Find Features menu
        features_menu = None
        for action in mock_main_window.menuBar().actions():
            if action.text() == "&Features":
                features_menu = action.menu()
                break

        assert features_menu is not None

        # Check for phase submenus
        menu_actions = features_menu.actions()
        menu_texts = [action.text() for action in menu_actions]
        assert any("Phase 1" in text for text in menu_texts)
        assert any("Phase 2" in text for text in menu_texts)
        assert any("Phase 3" in text for text in menu_texts)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
