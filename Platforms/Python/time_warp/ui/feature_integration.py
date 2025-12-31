"""Feature integration module for Time Warp IDE.

This module handles integration of the 14 feature panels into the IDE window,
creating a feature menu system, and managing signals/callbacks for features.
"""

from typing import Dict, Optional

from PySide6.QtCore import Qt
from PySide6.QtGui import QAction
from PySide6.QtWidgets import QDockWidget, QWidget

from .feature_panels import (
    AccessibilityPanel,
    AIAssistantPanel,
    AssetLibraryPanel,
    CollaborationPanel,
    DebuggerPanel,
    ExecutionReplayPanel,
    ExportableExporterPanel,
    HardwareSimulatorPanel,
    LanguageComparatorPanel,
    LearningAnalyticsPanel,
    PeerReviewPanel,
    PerformanceProfilerPanel,
    ProjectTemplatesPanel,
    SyntaxValidatorPanel,
)


class FeatureIntegrationManager:
    """Manages integration of feature panels into main IDE window."""

    # Phase 1 features (5)
    PHASE_1_FEATURES = [
        (
            "Syntax Validator",
            "syntax_validator",
            SyntaxValidatorPanel,
            "real-time error detection",
        ),
        (
            "Project Templates",
            "project_templates",
            ProjectTemplatesPanel,
            "starter templates",
        ),
        (
            "Timeline Debugger",
            "timeline_debugger",
            DebuggerPanel,
            "step-through execution",
        ),
        (
            "Language Comparator",
            "language_comparator",
            LanguageComparatorPanel,
            "compare paradigms",
        ),
        ("Asset Library", "asset_library", AssetLibraryPanel, "game assets"),
    ]

    # Phase 2 features (10)
    PHASE_2_FEATURES = [
        (
            "Collaboration Tool",
            "collaboration_tool",
            CollaborationPanel,
            "pair programming",
        ),
        (
            "Performance Profiler",
            "performance_profiler",
            PerformanceProfilerPanel,
            "hotspot detection",
        ),
        (
            "Execution Replay",
            "execution_replay",
            ExecutionReplayPanel,
            "algorithm visualization",
        ),
        (
            "Hardware Simulator",
            "hardware_simulator",
            HardwareSimulatorPanel,
            "device simulation",
        ),
        (
            "AI Assistant",
            "ai_assistant",
            AIAssistantPanel,
            "knowledge-based help",
        ),
        (
            "Executable Exporter",
            "executable_exporter",
            ExportableExporterPanel,
            "multi-format export",
        ),
        (
            "Learning Analytics",
            "learning_analytics",
            LearningAnalyticsPanel,
            "progress tracking",
        ),
        (
            "Accessibility Suite",
            "accessibility_suite",
            AccessibilityPanel,
            "inclusive features",
        ),
        (
            "Peer Review Tool",
            "peer_review_tool",
            PeerReviewPanel,
            "code feedback",
        ),
        (
            "Quick Reference",
            "quick_reference",
            None,
            "syntax help",
        ),  # Reference is integrated elsewhere
    ]

    # Phase 3 features (3) - handled by interpreter integration
    PHASE_3_FEATURES = [
        (
            "Multiplayer Leaderboard",
            "multiplayer_leaderboard",
            None,
            "competitive learning",
        ),
        (
            "LMS Integration",
            "lms_integration",
            None,
            "Canvas/Google Classroom",
        ),
        (
            "Community Marketplace",
            "community_marketplace",
            None,
            "resource sharing",
        ),
    ]

    def __init__(self, main_window):
        """Initialize feature integration manager.

        Args:
            main_window: Reference to the main IDE window
        """
        self.main_window = main_window
        self.feature_panels: Dict[str, QWidget] = {}
        self.dock_widgets: Dict[str, QDockWidget] = {}
        self.feature_actions: Dict[str, QAction] = {}

    def setup_features(self):
        """Setup all features in the IDE.

        This method:
        1. Creates feature panels
        2. Creates dock widgets for each panel
        3. Adds feature menu to menu bar
        4. Connects feature signals to status bar
        """
        # Create all feature panels
        self._create_feature_panels()

        # Connect feature status signals to IDE
        self._connect_feature_signals()

    def _create_feature_panels(self):
        """Create instances of all feature panels."""
        all_features = self.PHASE_1_FEATURES + self.PHASE_2_FEATURES

        for feature_name, feature_id, panel_class, _ in all_features:
            if panel_class is None:
                continue  # Skip features without UI panels

            try:
                # Create panel instance with error handling
                try:
                    panel = panel_class()
                except TypeError:
                    # If panel requires arguments, create with empty init
                    panel = QWidget()
                    panel.setWindowTitle(feature_name)

                self.feature_panels[feature_id] = panel

                # Create dock widget for panel
                dock = QDockWidget(f"🎯 {feature_name}",
                                   self.main_window)
                dock.setWidget(panel)
                dock.setObjectName(feature_id)
                dock.setAllowedAreas(
                    Qt.DockWidgetArea.RightDockWidgetArea
                    | Qt.DockWidgetArea.BottomDockWidgetArea
                    | Qt.DockWidgetArea.LeftDockWidgetArea
                )
                dock.setVisible(False)  # Hidden by default

                self.dock_widgets[feature_id] = dock
                self.main_window.addDockWidget(
                    Qt.RightDockWidgetArea, dock)

            except (TypeError, ValueError) as e:
                print(f"❌ Error creating {feature_name} panel: {e}")
                # Create placeholder widget even if panel fails
                placeholder = QWidget()
                placeholder.setWindowTitle(f"{feature_name} (Not Available)")
                self.feature_panels[feature_id] = placeholder

                dock = QDockWidget(f"🎯 {feature_name}", self.main_window)
                dock.setWidget(placeholder)
                dock.setObjectName(feature_id)
                dock.setVisible(False)
                self.dock_widgets[feature_id] = dock
                self.main_window.addDockWidget(Qt.RightDockWidgetArea, dock)

    def _create_features_menu(self):
        """Create Features menu in menu bar."""
        menubar = self.main_window.menuBar()

        # Find insertion point (before Help if exists)
        help_menu = None

        for action in menubar.actions():
            if action.text() == "&Help":
                help_menu = action

        # Create Features menu
        features_menu = menubar.addMenu("&Features")

        # Reorganize menu bar if needed
        if help_menu:
            menubar.removeAction(help_menu)
            menubar.addMenu(
                help_menu.menu() if hasattr(help_menu, "menu") else help_menu
            )

        # Add Phase 1 submenu
        phase1_menu = features_menu.addMenu("Phase 1️⃣ - Core")
        self._add_phase_features_to_menu(phase1_menu, self.PHASE_1_FEATURES)

        # Add Phase 2 submenu
        phase2_menu = features_menu.addMenu("Phase 2️⃣ - Advanced")
        self._add_phase_features_to_menu(phase2_menu, self.PHASE_2_FEATURES)

        features_menu.addSeparator()

        # Add Phase 3 submenu (informational, handled by interpreter)
        phase3_menu = features_menu.addMenu("Phase 3️⃣ - Cloud & Community")
        self._add_phase_features_to_menu(phase3_menu, self.PHASE_3_FEATURES)

        features_menu.addSeparator()

        # Add "Show All Features" toggle
        show_all_action = QAction("&Show All Features", self.main_window)
        show_all_action.setCheckable(False)
        show_all_action.triggered.connect(self.show_all_features)
        features_menu.addAction(show_all_action)

        # Add "Hide All Features" toggle
        hide_all_action = QAction("&Hide All Features", self.main_window)
        hide_all_action.setCheckable(False)
        hide_all_action.triggered.connect(self.hide_all_features)
        features_menu.addAction(hide_all_action)

    def _add_phase_features_to_menu(self, menu, features):
        """Add feature actions to a menu.

        Args:
            menu: QMenu to add actions to
            features: List of (name, id, class, description) tuples
        """
        for feature_name, feature_id, panel_class, description in features:
            if panel_class is None:
                # Show as disabled for Phase 3 features
                action = QAction(
                    f"{feature_name} ({description})", self.main_window
                )
                action.setEnabled(False)
            else:
                # Create checkable action for toggleable panels
                action = QAction(f"✓ {feature_name}", self.main_window)
                action.setCheckable(True)
                action.setChecked(False)
                action.triggered.connect(
                    lambda checked, fid=feature_id: self.toggle_feature_panel(
                        fid
                    )
                )

            self.feature_actions[feature_id] = action
            menu.addAction(action)

    def toggle_feature_panel(
        self, feature_id: str, visible: Optional[bool] = None
    ):
        """Toggle visibility of a feature panel.

        Args:
            feature_id: ID of the feature to toggle
            visible: Optional explicit visibility state
        """
        if feature_id not in self.dock_widgets:
            return

        dock = self.dock_widgets[feature_id]
        panel = self.feature_panels.get(feature_id)

        if visible is None:
            visible = not dock.isVisible()

        dock.setVisible(visible)

        # Update action state
        if feature_id in self.feature_actions:
            self.feature_actions[feature_id].setChecked(visible)

        # Update status bar
        if panel and hasattr(panel, "status_changed"):
            if visible:
                status = f"✓ {panel.__class__.__name__} activated"
            else:
                status = f"✗ {panel.__class__.__name__} deactivated"
            self.main_window.statusbar.showMessage(status, 2000)

    def show_all_features(self):
        """Show all feature panels."""
        for feature_id in self.dock_widgets:
            self.toggle_feature_panel(feature_id, visible=True)
        self.main_window.statusbar.showMessage("✓ All features shown", 2000)

    def hide_all_features(self):
        """Hide all feature panels."""
        for feature_id in self.dock_widgets:
            self.toggle_feature_panel(feature_id, visible=False)
        msg = "✗ All features hidden"
        self.main_window.statusbar.showMessage(msg, 2000)

    def _connect_feature_signals(self):
        """Connect feature panel signals to IDE status bar and other."""
        for feature_id, panel in self.feature_panels.items():
            # Connect status_changed signal if available
            if hasattr(panel, "status_changed"):
                panel.status_changed.connect(
                    lambda status, fid=feature_id:
                    self._on_feature_status_changed(fid, status)
                )

            # Connect operation signals if available
            if hasattr(panel, "operation_started"):
                panel.operation_started.connect(
                    lambda op, fid=feature_id:
                    self._on_operation_started(fid, op)
                )

            if hasattr(panel, "operation_completed"):
                panel.operation_completed.connect(
                    lambda result, fid=feature_id:
                    self._on_operation_completed(fid, result)
                )

    def _on_feature_status_changed(self, _: str, status: str):
        """Handle feature status change.

        Args:
            _: ID of the feature (unused)
            status: Status message from the feature
        """
        # Display status in IDE status bar
        self.main_window.statusbar.showMessage(status, 3000)

    def _on_operation_started(self, _: str, operation: str):
        """Handle feature operation start.

        Args:
            _: ID of the feature (unused)
            operation: Name of the operation
        """
        msg = f"🚀 {operation} started..."
        self.main_window.statusbar.showMessage(msg, 2000)

    def _on_operation_completed(self, _: str, result: dict):
        """Handle feature operation completion.

        Args:
            _: ID of the feature (unused)
            result: Operation result (may contain success/error)
        """
        if result.get("success", True):
            self.main_window.statusbar.showMessage(
                "✅ Operation completed", 2000
            )
        else:
            error = result.get("error", "Unknown error")
            msg = f"❌ Operation failed: {error}"
            self.main_window.statusbar.showMessage(msg, 3000)

    def get_feature_panel(self, feature_id: str) -> Optional[QWidget]:
        """Get a feature panel by ID.

        Args:
            feature_id: ID of the feature

        Returns:
            Feature panel widget or None if not found
        """
        return self.feature_panels.get(feature_id)

    def feature_status_summary(self) -> str:
        """Get a summary of active features.

        Returns:
            String describing currently visible features
        """
        active_features = [
            name
            for feature_id, (name, _, _, _) in [
                (fid, info)
                for fid in self.dock_widgets
                for info in self.PHASE_1_FEATURES + self.PHASE_2_FEATURES
                if info[1] == fid
            ]
            if self.dock_widgets[feature_id].isVisible()
        ]

        if not active_features:
            return "No features currently active"

        return f"Active features: {', '.join(active_features)}"

    def register_feature_shortcut(self, feature_id: str, shortcut: str):
        """Register a keyboard shortcut for a feature.

        Args:
            feature_id: ID of the feature
            shortcut: Shortcut string (e.g., "Ctrl+Shift+A")
        """
        if feature_id in self.feature_actions:
            self.feature_actions[feature_id].setShortcut(shortcut)

    def export_feature_configuration(self) -> dict:
        """Export feature configuration for saving state.

        Returns:
            Dictionary with feature visibility states
        """
        config = {}
        for feature_id, dock in self.dock_widgets.items():
            config[feature_id] = {
                "visible": dock.isVisible(),
                "geometry": dock.geometry(),
            }
        return config

    def import_feature_configuration(self, config: dict):
        """Import feature configuration to restore state.

        Args:
            config: Dictionary with feature visibility states
        """
        for feature_id, state in config.items():
            if feature_id in self.dock_widgets:
                dock = self.dock_widgets[feature_id]
                dock.setVisible(state.get("visible", False))
