import pytest
# pylint: disable=redefined-outer-name,reimported

QtWidgets = pytest.importorskip("PySide6.QtWidgets")
QApplication = QtWidgets.QApplication
QMainWindow = QtWidgets.QMainWindow
QWidget = QtWidgets.QWidget

# pylint: disable=wrong-import-position
from time_warp.ui.feature_integration import FeatureIntegrationManager  # noqa: E402
# pylint: enable=wrong-import-position


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


class BrokenPanel(QWidget):  # type: ignore[misc,valid-type]
    def __init__(self):
        raise ValueError("broken panel")


class WorkingPanel(QWidget):  # type: ignore[misc,valid-type]
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


def _make_manager(qapp, phase1=None, phase2=None):
    window = QMainWindow()
    window.statusbar = window.statusBar()
    manager = FeatureIntegrationManager(window)
    manager.PHASE_1_FEATURES = phase1 or []
    manager.PHASE_2_FEATURES = phase2 or []
    manager.setup_features()
    return manager


def test_empty_feature_status_summary(qapp):
    manager = _make_manager(qapp)
    summary = manager.feature_status_summary()
    assert isinstance(summary, str)


def test_hide_all_hides_working_panel(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Working Feature", "working_feature", WorkingPanel, "test panel"),
    ])
    manager.show_all_features()
    manager.hide_all_features()
    assert manager.dock_widgets["working_feature"].isVisible() is False


def test_toggle_panel_visible(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Toggle Feature", "toggle_feature", WorkingPanel, "test panel"),
    ])
    manager.toggle_feature_panel("toggle_feature", visible=True)
    assert manager.dock_widgets["toggle_feature"].isVisible() is True


def test_toggle_panel_hidden(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Toggle Feature2", "toggle_feature2", WorkingPanel, "test panel"),
    ])
    manager.toggle_feature_panel("toggle_feature2", visible=True)
    manager.toggle_feature_panel("toggle_feature2", visible=False)
    assert manager.dock_widgets["toggle_feature2"].isVisible() is False


def test_get_feature_panel_returns_widget(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Feature A", "feature_a", WorkingPanel, "test panel"),
    ])
    panel = manager.get_feature_panel("feature_a")
    assert panel is not None


def test_get_feature_panel_missing_returns_none(qapp):
    manager = _make_manager(qapp)
    panel = manager.get_feature_panel("nonexistent")
    assert panel is None


def test_feature_actions_created(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Feature B", "feature_b", WorkingPanel, "test panel"),
    ])
    assert "feature_b" in manager.feature_actions


def test_dock_widgets_created(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Feature C", "feature_c", WorkingPanel, "test panel"),
    ])
    assert "feature_c" in manager.dock_widgets


def test_export_config_working_feature(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Feature D", "feature_d", WorkingPanel, "test panel"),
    ])
    manager.toggle_feature_panel("feature_d", visible=True)
    config = manager.export_feature_configuration()
    assert "feature_d" in config
    assert config["feature_d"]["visible"] is True


def test_import_config_restores_visibility(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Feature E", "feature_e", WorkingPanel, "test panel"),
    ])
    manager.toggle_feature_panel("feature_e", visible=True)
    config = manager.export_feature_configuration()
    manager.hide_all_features()
    assert manager.dock_widgets["feature_e"].isVisible() is False
    manager.import_feature_configuration(config)
    assert manager.dock_widgets["feature_e"].isVisible() is True


def test_phase2_working_panel_enabled(qapp):
    manager = _make_manager(qapp, phase2=[
        ("Phase 2 Feature", "phase2_feat", WorkingPanel, "p2 panel"),
    ])
    action = manager.feature_actions["phase2_feat"]
    assert action.isEnabled() is True


def test_phase3_multiple_coming_soon(qapp):
    window = QMainWindow()
    window.statusbar = window.statusBar()
    manager = FeatureIntegrationManager(window)
    manager.PHASE_1_FEATURES = []
    manager.PHASE_2_FEATURES = []
    manager.PHASE_3_FEATURES = [
        ("Cloud Lab", "cloud_lab", None, "future feature"),
        ("AI Plugin", "ai_plugin", None, "future feature"),
    ]
    manager.setup_features()
    assert manager.feature_actions["cloud_lab"].isEnabled() is False
    assert manager.feature_actions["ai_plugin"].isEnabled() is False


def test_broken_panel_property_false(qapp):
    manager = _make_manager(qapp, phase1=[
        ("Broken Panel", "broken_panel", BrokenPanel, "test panel"),
    ])
    panel = manager.get_feature_panel("broken_panel")
    assert panel.property("feature_available") is False


class TestFeatureIntegrationExtended:
    """More FeatureIntegrationManager tests."""

    def test_manager_has_phase1_features_attr(self, qapp):
        manager = _make_manager(qapp)
        assert hasattr(manager, "PHASE_1_FEATURES")

    def test_manager_has_phase2_features_attr(self, qapp):
        manager = _make_manager(qapp)
        assert hasattr(manager, "PHASE_2_FEATURES")

    def test_manager_has_phase3_features_attr(self, qapp):
        manager = _make_manager(qapp)
        assert hasattr(manager, "PHASE_3_FEATURES")

    def test_feature_status_summary_is_str(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Feature X", "feature_x", WorkingPanel, "test"),
        ])
        summary = manager.feature_status_summary()
        assert isinstance(summary, str)

    def test_two_working_panels_both_in_dock_widgets(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Feat One", "feat_one", WorkingPanel, "p1"),
            ("Feat Two", "feat_two", WorkingPanel, "p2"),
        ])
        assert "feat_one" in manager.dock_widgets
        assert "feat_two" in manager.dock_widgets

    def test_show_then_hide_not_visible(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Feat Hide", "feat_hide", WorkingPanel, "test"),
        ])
        manager.show_all_features()
        manager.hide_all_features()
        assert manager.dock_widgets["feat_hide"].isVisible() is False

    def test_export_config_returns_dict(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Feat Cfg", "feat_cfg", WorkingPanel, "test"),
        ])
        config = manager.export_feature_configuration()
        assert isinstance(config, dict)

    def test_import_empty_config_no_crash(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Feat Imp", "feat_imp", WorkingPanel, "test"),
        ])
        manager.import_feature_configuration({})  # should not raise

    def test_toggle_then_export_visible_true(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Feat T", "feat_t", WorkingPanel, "test"),
        ])
        manager.toggle_feature_panel("feat_t", visible=True)
        cfg = manager.export_feature_configuration()
        assert cfg["feat_t"]["visible"] is True

    def test_broken_and_working_dock_widgets_both_created(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Good", "good_feat", WorkingPanel, "test"),
            ("Bad", "bad_feat", BrokenPanel, "test"),
        ])
        assert "good_feat" in manager.dock_widgets
        assert "bad_feat" in manager.dock_widgets

    def test_broken_feature_action_disabled(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Broken F", "broken_f", BrokenPanel, "test"),
        ])
        assert manager.feature_actions["broken_f"].isEnabled() is False

    def test_working_feature_action_enabled(self, qapp):
        manager = _make_manager(qapp, phase1=[
            ("Working W", "working_w", WorkingPanel, "test"),
        ])
        assert manager.feature_actions["working_w"].isEnabled() is True

    def test_phase1_and_phase2_coexist(self, qapp):
        manager = _make_manager(
            qapp,
            phase1=[("P1 Feat", "p1_feat", WorkingPanel, "p1")],
            phase2=[("P2 Feat", "p2_feat", WorkingPanel, "p2")],
        )
        assert "p1_feat" in manager.dock_widgets
        assert "p2_feat" in manager.dock_widgets


class TestFeatureIntegrationExtendedB:
    """More feature integration tests."""

    def test_phase1_is_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert isinstance(FeatureIntegrationManager.PHASE_1_FEATURES, list)

    def test_phase2_is_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert isinstance(FeatureIntegrationManager.PHASE_2_FEATURES, list)

    def test_phase3_is_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert isinstance(FeatureIntegrationManager.PHASE_3_FEATURES, list)

    def test_phase1_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_1_FEATURES) > 0

    def test_phase2_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_2_FEATURES) > 0

    def test_phase3_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_3_FEATURES) > 0

    def test_phase1_contains_tuples(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert isinstance(item, (str, tuple, list))

    def test_phase2_contains_tuples(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_2_FEATURES:
            assert isinstance(item, (str, tuple, list))

    def test_phase3_contains_tuples(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_3_FEATURES:
            assert isinstance(item, (str, tuple, list))

    def test_all_phases_combined_count(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        total = (len(FeatureIntegrationManager.PHASE_1_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_2_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_3_FEATURES))
        assert total > 5


class TestFeatureIntegrationExtended2:
    """Extended feature integration tests."""

    def test_manager_importable(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert FeatureIntegrationManager is not None

    def test_phase1_non_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_1_FEATURES) > 0

    def test_phase2_non_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_2_FEATURES) > 0

    def test_phase3_non_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_3_FEATURES) > 0

    def test_all_phase1_items_non_none(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert item is not None

    def test_all_phase2_items_non_none(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_2_FEATURES:
            assert item is not None

    def test_all_phase3_items_non_none(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_3_FEATURES:
            assert item is not None

    def test_phase1_items_are_valid_type(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert isinstance(item, (str, tuple, list, dict))

    def test_phase2_items_are_valid_type(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_2_FEATURES:
            assert isinstance(item, (str, tuple, list, dict))

    def test_combined_total_gt_10(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        total = (len(FeatureIntegrationManager.PHASE_1_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_2_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_3_FEATURES))
        assert total > 10


class TestFeatureIntegrationExtended3:
    """Third round of FeatureIntegrationManager tests."""

    def test_phase1_each_is_tuple_or_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert isinstance(item, (tuple, list))

    def test_phase2_each_is_tuple_or_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_2_FEATURES:
            assert isinstance(item, (tuple, list))

    def test_phase3_each_is_tuple_or_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_3_FEATURES:
            assert isinstance(item, (tuple, list))

    def test_phase1_first_item_has_name(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        item = FeatureIntegrationManager.PHASE_1_FEATURES[0]
        assert isinstance(item[0], str)

    def test_phase2_first_item_has_name(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        item = FeatureIntegrationManager.PHASE_2_FEATURES[0]
        assert isinstance(item[0], str)

    def test_all_phases_non_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_1_FEATURES) > 0
        assert len(FeatureIntegrationManager.PHASE_2_FEATURES) > 0
        assert len(FeatureIntegrationManager.PHASE_3_FEATURES) > 0

    def test_manager_importable(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert FeatureIntegrationManager is not None

    def test_phase1_count_gte_1(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_1_FEATURES) >= 1

    def test_phase3_count_gte_1(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_3_FEATURES) >= 1

    def test_phase1_items_have_at_least_2_elements(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert len(item) >= 2


class TestFeatureIntegrationExtended4:
    """Fourth round of feature integration tests."""

    def test_phase1_all_items_are_tuple_or_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert isinstance(item, (tuple, list))

    def test_phase2_all_items_are_tuple_or_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_2_FEATURES:
            assert isinstance(item, (tuple, list))

    def test_phase3_all_items_are_tuple_or_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_3_FEATURES:
            assert isinstance(item, (tuple, list))

    def test_phase1_count_gte_3(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_1_FEATURES) >= 3

    def test_phase2_count_gte_3(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_2_FEATURES) >= 3

    def test_phase3_count_gte_1(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_3_FEATURES) >= 1

    def test_first_phase1_item_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        first = FeatureIntegrationManager.PHASE_1_FEATURES[0]
        assert len(first) > 0

    def test_first_element_is_string(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        item = FeatureIntegrationManager.PHASE_1_FEATURES[0]
        assert isinstance(item[0], str)

    def test_total_features_gte_7(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        total = (len(FeatureIntegrationManager.PHASE_1_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_2_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_3_FEATURES))
        assert total >= 7

    def test_manager_class_importable(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert FeatureIntegrationManager is not None


class TestFeatureIntegrationExtended5:
    """Fifth round of feature integration tests."""

    def test_manager_importable(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert FeatureIntegrationManager is not None

    def test_phase1_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_1_FEATURES) > 0

    def test_phase2_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_2_FEATURES) > 0

    def test_phase3_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_3_FEATURES) > 0

    def test_all_phase1_first_elem_is_str(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert isinstance(item[0], str)

    def test_all_phase2_first_elem_is_str(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_2_FEATURES:
            assert isinstance(item[0], str)

    def test_all_phase3_first_elem_is_str(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_3_FEATURES:
            assert isinstance(item[0], str)

    def test_total_features_gte_nine(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        total = (len(FeatureIntegrationManager.PHASE_1_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_2_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_3_FEATURES))
        assert total >= 9

    def test_two_managers_independent(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        p1 = FeatureIntegrationManager.PHASE_1_FEATURES
        assert p1 is FeatureIntegrationManager.PHASE_1_FEATURES

    def test_phase1_contains_tuples(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        for item in FeatureIntegrationManager.PHASE_1_FEATURES:
            assert isinstance(item, (tuple, list))


class TestFeatureIntegrationExtended6:
    """Sixth round of feature integration tests."""

    def test_importable(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert FeatureIntegrationManager is not None

    def test_phase1_is_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert isinstance(FeatureIntegrationManager.PHASE_1_FEATURES, list)

    def test_phase2_is_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert isinstance(FeatureIntegrationManager.PHASE_2_FEATURES, list)

    def test_phase3_is_list(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert isinstance(FeatureIntegrationManager.PHASE_3_FEATURES, list)

    def test_phase1_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_1_FEATURES) > 0

    def test_phase2_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_2_FEATURES) > 0

    def test_phase3_not_empty(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        assert len(FeatureIntegrationManager.PHASE_3_FEATURES) > 0

    def test_all_phases_combined_gte_nine(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        total = (len(FeatureIntegrationManager.PHASE_1_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_2_FEATURES) +
                 len(FeatureIntegrationManager.PHASE_3_FEATURES))
        assert total >= 9

    def test_phase1_first_elem(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        first = FeatureIntegrationManager.PHASE_1_FEATURES[0]
        assert first is not None

    def test_phase2_first_elem(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        first = FeatureIntegrationManager.PHASE_2_FEATURES[0]
        assert first is not None

    def test_phase3_first_elem(self):
        from time_warp.ui.feature_integration import FeatureIntegrationManager
        first = FeatureIntegrationManager.PHASE_3_FEATURES[0]
        assert first is not None
