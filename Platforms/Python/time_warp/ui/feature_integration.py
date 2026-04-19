"""Feature integration module for Time Warp Studio.

This module handles integration of the 14 feature panels into the IDE window,
creating a feature menu system, and managing signals/callbacks for features.
"""

# PySide6 symbols are provided at runtime; silence pylint import resolution
# errors for these modules in this file.
# pylint: disable=no-name-in-module

from typing import Dict, Optional

from PySide6.QtCore import Qt
from PySide6.QtGui import QAction
from PySide6.QtWidgets import QDockWidget, QLabel, QVBoxLayout, QWidget

from .feature_panels import (
    AccessibilityPanel,
    AIAssistantPanel,
    AssetLibraryPanel,
    CollaborationPanel,
    ClassroomModePanel,
    DebuggerPanel,
    ErrorExplainerPanel,
    ReferenceSearchPanel,
    AchievementsPanel,
    ExecutionReplayPanel,
    ExportableExporterPanel,
    HardwareSimulatorPanel,
    LanguageComparatorPanel,
    LearningHubPanel,
    LessonModePanel,
    LearningAnalyticsPanel,
    PeerReviewPanel,
    PerformanceProfilerPanel,
    ProjectExplorerPanel,
    ProjectRunnerPanel,
    ProjectTemplatesPanel,
    SyntaxValidatorPanel,
    TurtleInspectorPanel,
)


class FeatureIntegrationManager:
    """Manages integration of feature panels into main IDE window."""

    # Phase 1 features (5)
    PHASE_1_FEATURES = [
        (
            "Learning Hub",
            "learning_hub",
            LearningHubPanel,
            "challenges, remixing, and tutor tools",
        ),
        (
            "Lesson Mode",
            "lesson_mode",
            LessonModePanel,
            "guided checkpoints",
        ),
        (
            "Error Explainer",
            "error_explainer",
            ErrorExplainerPanel,
            "friendly error tips",
        ),
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
            "Project Explorer",
            "project_explorer",
            ProjectExplorerPanel,
            "browse workspace files",
        ),
        (
            "Project Runner",
            "project_runner",
            ProjectRunnerPanel,
            "multi-tab runs",
        ),
        (
            "Classroom Mode",
            "classroom_mode",
            ClassroomModePanel,
            "presentation tools",
        ),
        (
            "Reference Search",
            "reference_search",
            ReferenceSearchPanel,
            "offline docs search",
        ),
        (
            "Achievements",
            "achievements",
            AchievementsPanel,
            "progress tracking",
        ),
        (
            "Turtle Inspector",
            "turtle_inspector",
            TurtleInspectorPanel,
            "turtle timeline",
        ),
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

    EXPERIMENTAL_FEATURE_IDS = {
        "collaboration_tool",
        "ai_assistant",
        "hardware_simulator",
        "peer_review_tool",
        "execution_replay",
        "learning_analytics",
    }

    def __init__(self, main_window):
        """Initialize feature integration manager.

        Args:
            main_window: Reference to the main IDE window
        """
        self.main_window = main_window
        self.feature_panels: Dict[str, QWidget] = {}
        self.dock_widgets: Dict[str, QDockWidget] = {}
        self.feature_actions: Dict[str, QAction] = {}
        self.feature_metadata: Dict[str, Dict[str, object]] = {}

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

        # Create features menu
        self._create_features_menu()

        # Connect feature status signals to IDE
        self._connect_feature_signals()

    def _create_unavailable_placeholder(
        self,
        feature_name: str,
        description: str,
        reason: str,
    ) -> QWidget:
        """Create a lightweight placeholder for unavailable features."""
        placeholder = QWidget()
        placeholder.setWindowTitle(f"{feature_name} (Not Available)")
        placeholder.setProperty("feature_available", False)

        layout = QVBoxLayout(placeholder)
        title = QLabel(f"{feature_name} is currently unavailable")
        title.setWordWrap(True)
        layout.addWidget(title)

        detail = QLabel(f"{description}\n\nReason: {reason}")
        detail.setWordWrap(True)
        layout.addWidget(detail)

        layout.addStretch()
        return placeholder

    def _create_feature_panels(self):
        """Create instances of all feature panels."""
        all_features = self.PHASE_1_FEATURES + self.PHASE_2_FEATURES

        for feature_name, feature_id, panel_class, description in all_features:
            if panel_class is None:
                continue  # Skip features without UI panels

            self.feature_metadata[feature_id] = {
                "name": feature_name,
                "description": description,
                "available": True,
                "experimental": feature_id in self.EXPERIMENTAL_FEATURE_IDS,
            }

            try:
                try:
                    panel = panel_class()
                except TypeError:
                    panel = QWidget()
                    panel.setWindowTitle(feature_name)

                panel.setProperty("feature_available", True)
                self.feature_panels[feature_id] = panel

                dock = QDockWidget(f"🎯 {feature_name}", self.main_window)
                dock.setWidget(panel)
                dock.setObjectName(feature_id)
                dock.setAllowedAreas(
                    Qt.DockWidgetArea.RightDockWidgetArea
                    | Qt.DockWidgetArea.BottomDockWidgetArea
                    | Qt.DockWidgetArea.LeftDockWidgetArea
                )
                dock.setVisible(False)

                self.dock_widgets[feature_id] = dock
                self.main_window.addDockWidget(Qt.RightDockWidgetArea, dock)

            except (TypeError, ValueError) as e:
                print(f"❌ Error creating {feature_name} panel: {e}")
                self.feature_metadata[feature_id]["available"] = False
                self.feature_metadata[feature_id]["reason"] = str(e)

                placeholder = self._create_unavailable_placeholder(
                    feature_name,
                    description,
                    str(e),
                )
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
            metadata = self.feature_metadata.get(feature_id, {})
            is_experimental = bool(metadata.get("experimental", False))
            is_available = bool(metadata.get("available", panel_class is not None))
            reason = str(metadata.get("reason", "Not included in this build"))

            if panel_class is None:
                action = QAction(
                    f"⏳ {feature_name} (Coming Soon — {description})",
                    self.main_window,
                )
                action.setEnabled(False)
                action.setToolTip("Planned for a future release")
            elif not is_available:
                action = QAction(
                    f"⚠ {feature_name} (Not Available)",
                    self.main_window,
                )
                action.setEnabled(False)
                action.setToolTip(reason)
            else:
                label = f"✓ {feature_name}"
                if is_experimental:
                    label = f"🧪 {feature_name}"

                action = QAction(label, self.main_window)
                action.setCheckable(True)
                action.setChecked(False)
                action.setToolTip(description)
                action.triggered.connect(
                    lambda checked, fid=feature_id: self.toggle_feature_panel(fid)
                )

            self.feature_actions[feature_id] = action
            menu.addAction(action)

    def _is_feature_available(self, feature_id: str) -> bool:
        """Return whether a feature is currently available for activation."""
        return bool(self.feature_metadata.get(feature_id, {}).get("available", True))

    def toggle_feature_panel(self, feature_id: str, visible: Optional[bool] = None):
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
            visible = not bool(dock.property("requested_visible"))

        if visible and not self._is_feature_available(feature_id):
            dock.setProperty("requested_visible", False)
            dock.hide()
            if feature_id in self.feature_actions:
                self.feature_actions[feature_id].setChecked(False)
            reason = self.feature_metadata.get(feature_id, {}).get(
                "reason", "Feature is not available in this build"
            )
            self.main_window.statusbar.showMessage(f"⚠ {reason}", 2500)
            return

        dock.setProperty("requested_visible", bool(visible))
        if visible:
            if hasattr(self.main_window, "show") and not self.main_window.isVisible():
                self.main_window.show()
            dock.show()
        else:
            dock.hide()

        if feature_id in self.feature_actions:
            self.feature_actions[feature_id].setChecked(bool(visible))

        if panel and hasattr(panel, "status_changed"):
            if visible:
                status = f"✓ {panel.__class__.__name__} activated"
            else:
                status = f"✗ {panel.__class__.__name__} deactivated"
            self.main_window.statusbar.showMessage(status, 2000)

    def show_all_features(self):
        """Show all available feature panels."""
        shown = 0
        skipped = 0
        for feature_id in self.dock_widgets:
            if self._is_feature_available(feature_id):
                self.toggle_feature_panel(feature_id, visible=True)
                shown += 1
            else:
                skipped += 1

        msg = f"✓ Shown {shown} available feature panel(s)"
        if skipped:
            msg += f" • {skipped} unavailable"
        self.main_window.statusbar.showMessage(msg, 2000)

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
                    lambda status, fid=feature_id: self._on_feature_status_changed(
                        fid,
                        status,
                    )
                )

            # Connect operation signals if available
            if hasattr(panel, "operation_started"):
                panel.operation_started.connect(
                    lambda op, fid=feature_id: self._on_operation_started(fid, op)
                )

            if hasattr(panel, "operation_completed"):
                panel.operation_completed.connect(
                    lambda result, fid=feature_id: self._on_operation_completed(
                        fid,
                        result,
                    )
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
            self.main_window.statusbar.showMessage("✅ Operation completed", 2000)
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
            if bool(self.dock_widgets[feature_id].property("requested_visible"))
            and self._is_feature_available(feature_id)
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
            geometry = dock.geometry()
            config[feature_id] = {
                "visible": bool(dock.property("requested_visible"))
                and self._is_feature_available(feature_id),
                "geometry": {
                    "x": geometry.x(),
                    "y": geometry.y(),
                    "width": geometry.width(),
                    "height": geometry.height(),
                },
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
                geometry = state.get("geometry", {})
                if isinstance(geometry, dict) and hasattr(dock, "setGeometry"):
                    dock.setGeometry(
                        geometry.get("x", 0),
                        geometry.get("y", 0),
                        geometry.get("width", 400),
                        geometry.get("height", 300),
                    )
                self.toggle_feature_panel(feature_id, visible=state.get("visible", False))
