import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")
QApplication = QtWidgets.QApplication
QMainWindow = QtWidgets.QMainWindow
QWidget = QtWidgets.QWidget

from time_warp.ui.feature_integration import FeatureIntegrationManager


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


class BrokenPanel(QWidget):
    def __init__(self):
        raise ValueError("broken panel")


class WorkingPanel(QWidget):
    pass


def test_failed_panel_creation_uses_placeholder_and_disables_action(qapp):
    window = QMainWindow()
    window.statusbar = window.statusBar()
    manager = FeatureIntegrationManager(window)
    manager.PHASE_1_FEATURES = [
        ("Broken Feature", "broken_feature", BrokenPanel, "test panel"),
    ]
    manager.PHASE_2_FEATURES = []

    manager.setup_features()

    panel = manager.get_feature_panel("broken_feature")
    action = manager.feature_actions["broken_feature"]

    assert panel is not None
    assert panel.property("feature_available") is False
    assert action.isEnabled() is False
    assert "Not Available" in action.text()


def test_phase3_features_are_listed_as_coming_soon(qapp):
    window = QMainWindow()
    window.statusbar = window.statusBar()
    manager = FeatureIntegrationManager(window)
    manager.PHASE_1_FEATURES = []
    manager.PHASE_2_FEATURES = []
    manager.PHASE_3_FEATURES = [
        ("Cloud Lab", "cloud_lab", None, "future feature"),
    ]

    manager.setup_features()

    action = manager.feature_actions["cloud_lab"]
    assert action.isEnabled() is False
    assert "Coming Soon" in action.text()


def test_show_all_features_skips_unavailable_panels(qapp):
    window = QMainWindow()
    window.statusbar = window.statusBar()
    manager = FeatureIntegrationManager(window)
    manager.PHASE_1_FEATURES = [
        ("Working Feature", "working_feature", WorkingPanel, "test panel"),
        ("Broken Feature", "broken_feature", BrokenPanel, "test panel"),
    ]
    manager.PHASE_2_FEATURES = []

    manager.setup_features()
    manager.show_all_features()

    assert manager.dock_widgets["working_feature"].isVisible() is True
    assert manager.dock_widgets["broken_feature"].isVisible() is False
    assert "Working Feature" in manager.feature_status_summary()
    assert "Broken Feature" not in manager.feature_status_summary()


def test_exported_feature_configuration_uses_plain_geometry_data(qapp):
    window = QMainWindow()
    window.statusbar = window.statusBar()
    manager = FeatureIntegrationManager(window)
    manager.PHASE_1_FEATURES = [
        ("Working Feature", "working_feature", WorkingPanel, "test panel"),
    ]
    manager.PHASE_2_FEATURES = []

    manager.setup_features()
    manager.toggle_feature_panel("working_feature", visible=True)
    config = manager.export_feature_configuration()

    assert config["working_feature"]["visible"] is True
    assert isinstance(config["working_feature"]["geometry"], dict)
    for key in ["x", "y", "width", "height"]:
        assert key in config["working_feature"]["geometry"]

    manager.hide_all_features()
    manager.import_feature_configuration(config)
    assert manager.dock_widgets["working_feature"].isVisible() is True
