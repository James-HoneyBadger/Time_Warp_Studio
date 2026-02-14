"""
Feature UI Panels for Time Warp Studio v6.0.0

This module provides PySide6 Qt widget panels for all 14 educational features.
Each panel wraps a core module and provides a user-friendly interface.
"""

# PySide6 symbols are provided at runtime; silence pylint import resolution
# errors for these modules in this file.
# pylint: disable=no-name-in-module

from typing import Optional

from PySide6.QtCore import Qt, Signal
from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QFileDialog,
    QFormLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QMessageBox,
    QProgressBar,
    QPushButton,
    QSpinBox,
    QSplitter,
    QTableWidget,
    QTableWidgetItem,
    QTextEdit,
    QVBoxLayout,
    QWidget,
)

from ..core.interpreter import Language
from ..core.accessibility import AccessibilityManager
from ..core.ai_assistant import LocalAIAssistant
from ..core.asset_library import AssetLibrary
from ..core.collaboration import LocalCollaborationSession, SessionManager
from ..core.debugger import CodeDebugger
from ..core.executable_exporter import ExecutableExporter
from ..core.execution_replay import (
    ExecutionReplayPlayer,
    VisualizationRecorder,
)
from ..core.hardware_simulator import HardwareSimulator
from ..core.language_comparator import MultiLanguageComparator
from ..core.learning_analytics import LearningAnalytics
from ..core.peer_review import CodeReviewSession
from ..core.performance_profiler import PerformanceProfiler
from ..core.project_templates import TemplateLibrary
from ..core.syntax_validator import SyntaxValidator


class FeaturePanelBase(QWidget):
    """Base class for all feature panels."""

    status_changed = Signal(str)  # Signal for status messages

    def __init__(self, title: str):
        super().__init__()
        self.title = title
        self.layout_main = QVBoxLayout(self)
        self.layout_main.setContentsMargins(10, 10, 10, 10)
        self.setWindowTitle(title)

    def emit_status(self, message: str, duration: int = 3000):
        """Emit status message."""
        _ = duration
        self.status_changed.emit(f"[{self.title}] {message}")


# ============================================================================
# PHASE 1 FEATURES
# ============================================================================


class SyntaxValidatorPanel(FeaturePanelBase):
    """UI for real-time syntax validation across all 7 languages."""

    def __init__(self):
        super().__init__("Syntax Validator")
        self.validator = SyntaxValidator()
        self.setup_ui()

    def setup_ui(self):
        """Setup validation panel UI."""
        # Language selector
        lang_layout = QHBoxLayout()
        lang_layout.addWidget(QLabel("Language:"))
        self.lang_combo = QComboBox()
        self.lang_combo.addItems(
            ["BASIC", "LOGO", "PILOT", "PASCAL", "C", "FORTH", "PROLOG"]
        )
        lang_layout.addWidget(self.lang_combo)
        lang_layout.addStretch()
        self.layout_main.addLayout(lang_layout)

        # Code input
        self.code_input = QTextEdit()
        self.code_input.setPlaceholderText("Enter code to validate...")
        self.code_input.textChanged.connect(self.validate)
        self.layout_main.addWidget(QLabel("Code:"))
        self.layout_main.addWidget(self.code_input)

        # Results table
        self.results_table = QTableWidget()
        self.results_table.setColumnCount(4)
        self.results_table.setHorizontalHeaderLabels(
            ["Line", "Type", "Message", "Severity"]
        )
        self.layout_main.addWidget(QLabel("Validation Results:"))
        self.layout_main.addWidget(self.results_table)

    def _normalize_language(self, language):
        """Normalize language input to Language enum."""
        if isinstance(language, Language):
            return language
        if isinstance(language, str):
            cleaned = language.strip().upper()
            mapping = {
                "BASIC": Language.BASIC,
                "LOGO": Language.LOGO,
                "PILOT": Language.PILOT,
                "PASCAL": Language.PASCAL,
                "C": Language.C,
                "FORTH": Language.FORTH,
                "PROLOG": Language.PROLOG,
            }
            return mapping.get(cleaned, Language.BASIC)
        return Language.BASIC

    def validate(self, language=None):
        """Validate current code."""
        code = self.code_input.toPlainText()
        lang = language or self.lang_combo.currentText()
        lang = self._normalize_language(lang)

        errors = self.validator.validate(code, lang)

        self.results_table.setRowCount(len(errors))
        for i, error in enumerate(errors):
            line = getattr(error, "line", "?")
            message = getattr(error, "message", "")
            severity = getattr(error, "severity", "error")
            if hasattr(severity, "value"):
                severity_value = severity.value
            else:
                severity_value = str(severity)
            self.results_table.setItem(i, 0, QTableWidgetItem(str(line)))
            self.results_table.setItem(i, 1, QTableWidgetItem("syntax"))
            self.results_table.setItem(i, 2, QTableWidgetItem(message))
            self.results_table.setItem(i, 3, QTableWidgetItem(severity_value))

        self.emit_status(f"Validated: {len(errors)} issues found")
        return len(errors)

    def validate_external(self, code: str, language: Language) -> int:
        """Validate code provided by the IDE."""
        lang_enum = self._normalize_language(language)
        try:
            self.lang_combo.setCurrentText(lang_enum.name)
        except (AttributeError, ValueError):
            pass
        self.code_input.setPlainText(code)
        return self.validate(lang_enum)


class ProjectTemplatesPanel(FeaturePanelBase):
    """UI for browsing and creating projects from templates."""

    def __init__(self):
        super().__init__("Project Templates")
        self.templates_mgr = TemplateLibrary()
        self.setup_ui()

    def setup_ui(self):
        """Setup templates panel UI."""
        # Category filter
        cat_layout = QHBoxLayout()
        cat_layout.addWidget(QLabel("Category:"))
        self.cat_combo = QComboBox()
        self.cat_combo.addItems(
            [
                "All",
                "Game",
                "Art Generation",
                "Learning",
                "Robotics",
                "Data Visualization",
                "Demo",
            ]
        )
        self.cat_combo.currentTextChanged.connect(self.refresh_templates)
        cat_layout.addWidget(self.cat_combo)
        cat_layout.addStretch()
        self.layout_main.addLayout(cat_layout)

        # Templates list
        self.templates_list = QListWidget()
        self.templates_list.itemClicked.connect(self.on_template_selected)
        self.layout_main.addWidget(QLabel("Available Templates:"))
        self.layout_main.addWidget(self.templates_list)

        # Template details
        self.details_text = QTextEdit()
        self.details_text.setReadOnly(True)
        self.layout_main.addWidget(QLabel("Description:"))
        self.layout_main.addWidget(self.details_text)

        # Create button
        create_btn = QPushButton("Create Project from Template")
        create_btn.clicked.connect(self.create_project)
        self.layout_main.addWidget(create_btn)

        self.refresh_templates()

    def refresh_templates(self):
        """Refresh templates list."""
        category_text = self.cat_combo.currentText()

        # Get all templates (TemplateLibrary.get_all() returns
        # Dict[str, Template])
        all_templates = self.templates_mgr.get_all().values()

        # Filter
        templates = []
        if category_text == "All":
            templates = list(all_templates)
        else:
            # Map "Art Generation" -> "art_generation"
            cat_val = category_text.lower().replace(" ", "_")
            templates = [
                tpl for tpl in all_templates if tpl.category.value == cat_val
            ]

        self.templates_list.clear()
        for tpl in templates:
            item = QListWidgetItem(tpl.name)
            item.setData(Qt.UserRole, tpl)
            self.templates_list.addItem(item)

    def on_template_selected(self, item):
        """Show template details."""
        template = item.data(Qt.UserRole)
        self.details_text.setText(
            template.description if template.description else "No description"
        )

    def create_project(self):
        """Create a new project from selected template."""
        item = self.templates_list.currentItem()
        if not item:
            QMessageBox.warning(
                self,
                "Error",
                "Please select a template first",
            )
            return

        template = item.data(Qt.UserRole)
        self.emit_status(f"Creating project from {template.name}...")


class DebuggerPanel(FeaturePanelBase):
    """UI for timeline-based debugging with breakpoints."""

    def __init__(self):
        super().__init__("Timeline Debugger")
        self.debugger = CodeDebugger()
        self.setup_ui()

    def setup_ui(self):
        """Setup debugger panel UI."""
        # Timeline controls
        timeline_layout = QHBoxLayout()

        self.play_btn = QPushButton("▶ Play")
        self.pause_btn = QPushButton("⏸ Pause")
        self.step_btn = QPushButton("↓ Step")
        self.reset_btn = QPushButton("⏹ Reset")

        timeline_layout.addWidget(self.play_btn)
        timeline_layout.addWidget(self.pause_btn)
        timeline_layout.addWidget(self.step_btn)
        timeline_layout.addWidget(self.reset_btn)
        timeline_layout.addStretch()

        self.layout_main.addWidget(QLabel("Execution Controls:"))
        self.layout_main.addLayout(timeline_layout)

        # Timeline slider
        self.timeline_progress = QProgressBar()
        self.timeline_progress.setValue(0)
        self.layout_main.addWidget(QLabel("Timeline:"))
        self.layout_main.addWidget(self.timeline_progress)

        # Execution stack
        self.stack_list = QListWidget()
        self.layout_main.addWidget(QLabel("Call Stack:"))
        self.layout_main.addWidget(self.stack_list)

        # Variables inspector
        self.vars_table = QTableWidget()
        self.vars_table.setColumnCount(2)
        self.vars_table.setHorizontalHeaderLabels(["Variable", "Value"])
        self.layout_main.addWidget(QLabel("Variables:"))
        self.layout_main.addWidget(self.vars_table)


class LanguageComparatorPanel(FeaturePanelBase):
    """UI for comparing same code across multiple languages."""

    def __init__(self):
        super().__init__("Language Comparator")
        self.comparator = MultiLanguageComparator()
        self.setup_ui()

    def setup_ui(self):
        """Setup comparator panel UI."""
        # Comparison selector
        comp_layout = QHBoxLayout()
        comp_layout.addWidget(QLabel("Compare:"))
        self.comp_combo = QComboBox()

        # Populate from comparator pairs
        pairs = self.comparator.get_builtin_pairs()
        for pid, (l1, l2) in pairs.items():
            self.comp_combo.addItem(f"{l1} vs {l2} ({pid})", pid)

        self.comp_combo.currentIndexChanged.connect(self.refresh_comparison)
        comp_layout.addWidget(self.comp_combo)
        comp_layout.addStretch()
        self.layout_main.addLayout(comp_layout)

        # Side-by-side code display
        splitter = QSplitter(Qt.Horizontal)

        left_layout = QVBoxLayout()
        self.label1 = QLabel("Language 1:")
        left_layout.addWidget(self.label1)
        self.lang1_text = QTextEdit()
        self.lang1_text.setReadOnly(True)
        left_layout.addWidget(self.lang1_text)

        left_widget = QWidget()
        left_widget.setLayout(left_layout)
        splitter.addWidget(left_widget)

        right_layout = QVBoxLayout()
        self.label2 = QLabel("Language 2:")
        right_layout.addWidget(self.label2)
        self.lang2_text = QTextEdit()
        self.lang2_text.setReadOnly(True)
        right_layout.addWidget(self.lang2_text)

        right_widget = QWidget()
        right_widget.setLayout(right_layout)
        splitter.addWidget(right_widget)

        self.layout_main.addWidget(splitter)

        self.refresh_comparison()

    def refresh_comparison(self):
        """Refresh comparison display."""
        index = self.comp_combo.currentIndex()
        if index < 0:
            return

        pid = self.comp_combo.itemData(index)
        pair_data = self.comparator.get_builtin_pair(pid)

        if pair_data:
            l1, l2, code1, code2 = pair_data
            self.label1.setText(f"{l1}:")
            self.label2.setText(f"{l2}:")
            self.lang1_text.setPlainText(code1)
            self.lang2_text.setPlainText(code2)


class AssetLibraryPanel(FeaturePanelBase):
    """UI for browsing and importing game development assets."""

    def __init__(self):
        super().__init__("Asset Library")
        self.library = AssetLibrary()
        self.setup_ui()

    def setup_ui(self):
        """Setup asset library panel UI."""
        # Category filter
        cat_layout = QHBoxLayout()
        cat_layout.addWidget(QLabel("Category:"))
        self.cat_combo = QComboBox()
        self.cat_combo.addItems(
            ["All", "Sprites", "Sounds", "Music", "Tiles", "Effects"]
        )
        self.cat_combo.currentTextChanged.connect(self.refresh_assets)
        cat_layout.addWidget(self.cat_combo)
        cat_layout.addStretch()
        self.layout_main.addLayout(cat_layout)

        # Assets list
        self.assets_list = QListWidget()
        self.assets_list.itemClicked.connect(self.on_asset_selected)
        self.layout_main.addWidget(QLabel("Available Assets:"))
        self.layout_main.addWidget(self.assets_list)

        # Asset preview
        self.preview_text = QTextEdit()
        self.preview_text.setReadOnly(True)
        self.layout_main.addWidget(QLabel("Preview:"))
        self.layout_main.addWidget(self.preview_text)

        # Import button
        import_btn = QPushButton("Import Asset")
        import_btn.clicked.connect(self.import_asset)
        self.layout_main.addWidget(import_btn)

        self.refresh_assets()

    def refresh_assets(self):
        """Refresh assets list."""
        category = self.cat_combo.currentText()
        all_assets = self.library.list_all()

        filtered = []
        for a in all_assets:
            atype = (
                a.asset_type.value
                if hasattr(a.asset_type, "value")
                else str(a.asset_type)
            )
            if category == "All":
                filtered.append(a)
            elif category == "Sprites" and atype == "sprite":
                filtered.append(a)
            elif (
                category in ["Sounds", "Music", "Effects"]
                and atype == "sound"
            ):
                filtered.append(a)
            elif category == "Tiles" and atype == "tileset":
                filtered.append(a)

        self.assets_list.clear()
        for asset in filtered:
            item = QListWidgetItem(asset.name)
            item.setData(Qt.UserRole, asset)
            self.assets_list.addItem(item)

    def on_asset_selected(self, item):
        """Show asset preview."""
        asset = item.data(Qt.UserRole)
        description = getattr(asset, "description", "")
        atype = (
            asset.asset_type.value
            if hasattr(asset.asset_type, "value")
            else str(asset.asset_type)
        )

        preview = f"Name: {asset.name}\n\n"
        preview += f"Type: {atype}\n"
        preview += f"Description: {description}\n"
        self.preview_text.setText(preview)

    def import_asset(self):
        """Import selected asset to project."""
        item = self.assets_list.currentItem()
        if not item:
            QMessageBox.warning(self, "Error", "Please select an asset first")
            return

        asset = item.data(Qt.UserRole)
        self.emit_status(f"Imported: {asset.name}")


# ============================================================================
# PHASE 2 FEATURES
# ============================================================================


class CollaborationPanel(FeaturePanelBase):
    """UI for real-time pair programming and collaboration."""

    def __init__(self):
        super().__init__("Collaboration Tool")
        self.session_manager = SessionManager("LocalUser")
        self.current_session: Optional[LocalCollaborationSession] = None
        self.setup_ui()

    def setup_ui(self):
        """Setup collaboration panel UI."""
        # Server connection
        server_layout = QHBoxLayout()
        server_layout.addWidget(QLabel("Server:"))
        self.server_input = QLineEdit()
        self.server_input.setText("localhost:9999")
        server_layout.addWidget(self.server_input)

        self.connect_btn = QPushButton("Connect")
        self.connect_btn.clicked.connect(self.connect_to_server)
        server_layout.addWidget(self.connect_btn)
        server_layout.addStretch()
        self.layout_main.addLayout(server_layout)

        # Active sessions
        self.sessions_list = QListWidget()
        self.layout_main.addWidget(QLabel("Active Sessions:"))
        self.layout_main.addWidget(self.sessions_list)

        # Participants
        self.participants_list = QListWidget()
        self.layout_main.addWidget(QLabel("Participants:"))
        self.layout_main.addWidget(self.participants_list)

        # Chat
        self.chat_display = QTextEdit()
        self.chat_display.setReadOnly(True)
        self.chat_display.setMaximumHeight(100)
        self.layout_main.addWidget(QLabel("Chat:"))
        self.layout_main.addWidget(self.chat_display)

        # Chat input
        chat_input_layout = QHBoxLayout()
        self.chat_input = QLineEdit()
        self.chat_send_btn = QPushButton("Send")
        self.chat_send_btn.clicked.connect(self.send_chat)
        chat_input_layout.addWidget(self.chat_input)
        chat_input_layout.addWidget(self.chat_send_btn)
        self.layout_main.addLayout(chat_input_layout)

    def connect_to_server(self):
        """Connect to collaboration server."""
        server_addr = self.server_input.text()
        self.emit_status(f"Connecting to {server_addr}...")

    def send_chat(self):
        """Send chat message."""
        msg = self.chat_input.text()
        if msg:
            self.chat_display.append(f"You: {msg}")
            self.chat_input.clear()


class PerformanceProfilerPanel(FeaturePanelBase):
    """UI for analyzing execution performance and finding bottlenecks."""

    def __init__(self):
        super().__init__("Performance Profiler")
        self.profiler = PerformanceProfiler()
        self.setup_ui()

    def setup_ui(self):
        """Setup profiler panel UI."""
        # Results table
        self.results_table = QTableWidget()
        self.results_table.setColumnCount(5)
        self.results_table.setHorizontalHeaderLabels(
            ["Line", "Function", "Calls", "Time (ms)", "% Total"]
        )
        self.layout_main.addWidget(QLabel("Performance Results:"))
        self.layout_main.addWidget(self.results_table)

        # Hotspots
        self.hotspots_list = QListWidget()
        self.layout_main.addWidget(QLabel("Slowest Lines:"))
        self.layout_main.addWidget(self.hotspots_list)

        # Statistics
        stats_layout = QHBoxLayout()
        stats_layout.addWidget(QLabel("Total Time:"))
        self.total_time_label = QLabel("0 ms")
        stats_layout.addWidget(self.total_time_label)
        stats_layout.addWidget(QLabel("Calls:"))
        self.calls_label = QLabel("0")
        stats_layout.addWidget(self.calls_label)
        stats_layout.addStretch()
        self.layout_main.addLayout(stats_layout)

    def update_from_stats(self, stats: dict):
        """Update profiler summary from execution stats."""
        duration_ms = stats.get("duration_ms", 0.0)
        line_count = stats.get("lines", 0)
        self.total_time_label.setText(f"{duration_ms:.2f} ms")
        self.calls_label.setText(str(line_count))
        self.emit_status("Performance summary updated")


class ExecutionReplayPanel(FeaturePanelBase):
    """UI for visualizing and replaying program execution."""

    def __init__(self):
        super().__init__("Execution Replay")
        self.recorder = VisualizationRecorder()
        self.replayer = ExecutionReplayPlayer(self.recorder)
        self.setup_ui()

    def setup_ui(self):
        """Setup replay panel UI."""
        # Playback controls
        control_layout = QHBoxLayout()

        self.play_btn = QPushButton("▶ Play")
        self.pause_btn = QPushButton("⏸ Pause")
        self.step_back_btn = QPushButton("⏮ Step Back")
        self.step_fwd_btn = QPushButton("Step Forward ⏭")

        control_layout.addWidget(self.play_btn)
        control_layout.addWidget(self.pause_btn)
        control_layout.addWidget(self.step_back_btn)
        control_layout.addWidget(self.step_fwd_btn)
        control_layout.addStretch()

        self.layout_main.addWidget(QLabel("Playback Controls:"))
        self.layout_main.addLayout(control_layout)

        # Timeline
        self.timeline_progress = QProgressBar()
        self.timeline_progress.setValue(0)
        self.layout_main.addWidget(QLabel("Execution Timeline:"))
        self.layout_main.addWidget(self.timeline_progress)

        # Execution trace
        self.trace_list = QListWidget()
        self.layout_main.addWidget(QLabel("Execution Trace:"))
        self.layout_main.addWidget(self.trace_list)

    def record_execution(self, stats: dict):
        """Append a short execution summary to the trace list."""
        language = stats.get("language")
        duration_ms = stats.get("duration_ms", 0.0)
        success = stats.get("successful", True)
        status = "✅" if success else "❌"
        label = f"{status} {language} run in {duration_ms:.2f} ms"
        self.trace_list.insertItem(0, label)


class HardwareSimulatorPanel(FeaturePanelBase):
    """UI for simulating hardware devices without physical hardware."""

    def __init__(self):
        super().__init__("Hardware Simulator")
        self.simulator = HardwareSimulator()
        self.setup_ui()

    def setup_ui(self):
        """Setup simulator panel UI."""
        # Device selector
        device_layout = QHBoxLayout()
        device_layout.addWidget(QLabel("Device:"))
        self.device_combo = QComboBox()
        self.device_combo.addItems(
            [
                "Arduino UNO",
                "Raspberry Pi",
                "Sensor Suite",
                "Robot Arm",
                "Smart Home Kit",
            ]
        )
        self.device_combo.currentTextChanged.connect(self.change_device)
        device_layout.addWidget(self.device_combo)
        device_layout.addStretch()
        self.layout_main.addLayout(device_layout)

        # Device controls
        self.controls_layout = QVBoxLayout()
        self.layout_main.addWidget(QLabel("Controls:"))
        self.layout_main.addLayout(self.controls_layout)

        # Sensor readings
        self.readings_table = QTableWidget()
        self.readings_table.setColumnCount(3)
        self.readings_table.setHorizontalHeaderLabels(
            ["Sensor", "Value", "Unit"]
        )
        self.layout_main.addWidget(QLabel("Sensor Readings:"))
        self.layout_main.addWidget(self.readings_table)

    def change_device(self):
        """Change simulated device."""
        device = self.device_combo.currentText()
        self.emit_status(f"Switched to {device}")


class AIAssistantPanel(FeaturePanelBase):
    """UI for AI-powered code assistance and help."""

    def __init__(self):
        super().__init__("AI Assistant")
        self.ai = LocalAIAssistant()
        self.setup_ui()

    def setup_ui(self):
        """Setup AI assistant panel UI."""
        # Query input
        self.query_input = QLineEdit()
        self.query_input.setPlaceholderText(
            "Ask a question about your code..."
        )
        self.query_input.returnPressed.connect(self.ask_question)
        self.layout_main.addWidget(QLabel("Question:"))
        self.layout_main.addWidget(self.query_input)

        # Response display
        self.response_display = QTextEdit()
        self.response_display.setReadOnly(True)
        self.layout_main.addWidget(QLabel("Assistant Response:"))
        self.layout_main.addWidget(self.response_display)

        # Suggestions
        self.suggestions_list = QListWidget()
        self.layout_main.addWidget(QLabel("Suggestions:"))
        self.layout_main.addWidget(self.suggestions_list)

        # Load knowledge button
        kb_btn = QPushButton("Load Knowledge Base")
        kb_btn.clicked.connect(self.load_knowledge)
        self.layout_main.addWidget(kb_btn)

    def ask_question(self):
        """Ask AI assistant a question."""
        query = self.query_input.text()
        if not query:
            return

        response = self.ai.query(query)
        self.response_display.setText(response)
        self.emit_status(f"AI responded to: {query[:30]}...")

    def load_knowledge(self):
        """Load knowledge base."""
        self.ai.load_knowledge_base()
        self.emit_status("Knowledge base loaded")

    def set_error_context(self, error_message: str):
        """Provide an error context for quick help."""
        suggestion = self.ai.explain_error(error_message)
        self.response_display.setText(suggestion.explanation)
        self.emit_status("AI generated an error explanation")


class ExportableExporterPanel(FeaturePanelBase):
    """UI for exporting programs to executable and other formats."""

    def __init__(self):
        super().__init__("Export to Executable")
        self.exporter = ExecutableExporter()
        self.setup_ui()

    def setup_ui(self):
        """Setup exporter panel UI."""
        # Format selector
        format_layout = QHBoxLayout()
        format_layout.addWidget(QLabel("Export Format:"))
        self.format_combo = QComboBox()
        self.format_combo.addItems(
            [
                "Windows EXE",
                "Linux Binary",
                "macOS App",
                "HTML5 Web App",
                "Shell Script",
            ]
        )
        format_layout.addWidget(self.format_combo)
        format_layout.addStretch()
        self.layout_main.addLayout(format_layout)

        # Output location
        output_layout = QHBoxLayout()
        output_layout.addWidget(QLabel("Output:"))
        self.output_path = QLineEdit()
        self.output_path.setPlaceholderText("/path/to/output")
        output_layout.addWidget(self.output_path)
        browse_btn = QPushButton("Browse...")
        browse_btn.clicked.connect(self.browse_output)
        output_layout.addWidget(browse_btn)
        self.layout_main.addLayout(output_layout)

        # Export progress
        self.progress_bar = QProgressBar()
        self.progress_bar.setValue(0)
        self.layout_main.addWidget(QLabel("Progress:"))
        self.layout_main.addWidget(self.progress_bar)

        # Options
        options_group = QGroupBox("Options")
        options_layout = QFormLayout()
        self.include_runtime = QCheckBox("Include Runtime")
        self.include_runtime.setChecked(True)
        options_layout.addRow("Include Runtime:", self.include_runtime)
        self.obfuscate = QCheckBox("Obfuscate Code")
        options_layout.addRow("Obfuscate:", self.obfuscate)
        options_group.setLayout(options_layout)
        self.layout_main.addWidget(options_group)

        # Export button
        export_btn = QPushButton("Export Program")
        export_btn.clicked.connect(self.export_program)
        self.layout_main.addWidget(export_btn)

    def browse_output(self):
        """Browse for output location."""
        path = QFileDialog.getSaveFileName(self, "Export to...")[0]
        if path:
            self.output_path.setText(path)

    def export_program(self):
        """Export current program."""
        fmt = self.format_combo.currentText()
        output = self.output_path.text()
        if not output:
            QMessageBox.warning(
                self,
                "Error",
                "Please specify output location",
            )
            return
        self.emit_status(f"Exporting to {fmt}...")


class LearningAnalyticsPanel(FeaturePanelBase):
    """UI for tracking student progress and learning analytics."""

    def __init__(self):
        super().__init__("Learning Analytics")
        self.analytics = LearningAnalytics()
        self.setup_ui()

    def setup_ui(self):
        """Setup analytics panel UI."""
        # Progress overview
        progress_layout = QHBoxLayout()
        progress_layout.addWidget(QLabel("Overall Progress:"))
        self.progress_bar = QProgressBar()
        self.progress_bar.setValue(45)
        progress_layout.addWidget(self.progress_bar)
        self.layout_main.addLayout(progress_layout)

        # Concept tracking
        self.concepts_table = QTableWidget()
        self.concepts_table.setColumnCount(4)
        self.concepts_table.setHorizontalHeaderLabels(
            ["Concept", "Mastery %", "Attempts", "Status"]
        )
        self.layout_main.addWidget(QLabel("Concept Mastery:"))
        self.layout_main.addWidget(self.concepts_table)

        # Learning path
        self.path_list = QListWidget()
        self.layout_main.addWidget(QLabel("Recommended Learning Path:"))
        self.layout_main.addWidget(self.path_list)

        # Statistics
        stats_layout = QHBoxLayout()
        stats_layout.addWidget(QLabel("Total Time:"))
        self.time_label = QLabel("0h 0m")
        stats_layout.addWidget(self.time_label)
        stats_layout.addWidget(QLabel("Problems Solved:"))
        self.problems_label = QLabel("0")
        stats_layout.addWidget(self.problems_label)
        stats_layout.addStretch()
        self.layout_main.addLayout(stats_layout)

    def record_execution(self, stats: dict):
        """Record execution stats into analytics and refresh UI."""
        language = stats.get("language", "Unknown")
        duration_ms = stats.get("duration_ms", 0.0)
        lines = stats.get("lines", 0)
        successful = stats.get("successful", True)
        self.analytics.record_program(
            name="Untitled",
            language=str(language),
            execution_time=duration_ms / 1000.0,
            successful=successful,
            lines_of_code=lines,
            concepts=[],
            errors=[],
        )
        self.refresh_from_analytics()

    def refresh_from_analytics(self):
        """Refresh UI widgets from analytics state."""
        metrics = self.analytics.get_progress_metrics()
        successful_rate = metrics.get("successful_rate", 0.0)
        self.progress_bar.setValue(int(successful_rate * 100))
        total_time_hours = metrics.get("total_time_hours", 0.0)
        hours = int(total_time_hours)
        minutes = int((total_time_hours - hours) * 60)
        self.time_label.setText(f"{hours}h {minutes}m")
        self.problems_label.setText(str(metrics.get("programs", 0)))

        concept_summary = self.analytics.get_concept_summary()
        self.concepts_table.setRowCount(len(concept_summary))
        for row, (concept, info) in enumerate(concept_summary.items()):
            mastery_pct = int(info.get("confidence", 0.0) * 100)
            attempts = info.get("attempts", 0)
            status = info.get("level", "novice")
            self.concepts_table.setItem(row, 0, QTableWidgetItem(concept))
            self.concepts_table.setItem(
                row, 1, QTableWidgetItem(f"{mastery_pct}%")
            )
            self.concepts_table.setItem(
                row, 2, QTableWidgetItem(str(attempts))
            )
            self.concepts_table.setItem(row, 3, QTableWidgetItem(status))

        self.path_list.clear()
        for step in self.analytics.get_learning_path():
            self.path_list.addItem(step)


class AccessibilityPanel(FeaturePanelBase):
    """UI for accessibility settings and inclusive features."""

    def __init__(self):
        super().__init__("Accessibility Suite")
        self.a11y = AccessibilityManager()
        self.setup_ui()

    def setup_ui(self):
        """Setup accessibility panel UI."""
        # Visual options
        visual_group = QGroupBox("Visual Settings")
        visual_layout = QFormLayout()

        self.high_contrast = QCheckBox("High Contrast Mode")
        visual_layout.addRow("High Contrast:", self.high_contrast)

        self.font_size = QSpinBox()
        self.font_size.setRange(8, 32)
        self.font_size.setValue(12)
        visual_layout.addRow("Font Size:", self.font_size)

        self.color_blind = QComboBox()
        self.color_blind.addItems(
            ["Normal", "Protanopia", "Deuteranopia", "Tritanopia"]
        )
        visual_layout.addRow("Color Blindness Mode:", self.color_blind)

        visual_group.setLayout(visual_layout)
        self.layout_main.addWidget(visual_group)

        # Audio options
        audio_group = QGroupBox("Audio Settings")
        audio_layout = QFormLayout()

        self.screen_reader = QCheckBox("Enable Screen Reader")
        audio_layout.addRow("Screen Reader:", self.screen_reader)

        self.audio_feedback = QCheckBox("Audio Feedback")
        self.audio_feedback.setChecked(True)
        audio_layout.addRow("Audio Feedback:", self.audio_feedback)

        audio_group.setLayout(audio_layout)
        self.layout_main.addWidget(audio_group)

        # Motor options
        motor_group = QGroupBox("Motor/Input Settings")
        motor_layout = QFormLayout()

        self.keyboard_only = QCheckBox("Keyboard Navigation Only")
        motor_layout.addRow("Keyboard Only:", self.keyboard_only)

        self.sticky_keys = QCheckBox("Sticky Keys")
        motor_layout.addRow("Sticky Keys:", self.sticky_keys)

        motor_group.setLayout(motor_layout)
        self.layout_main.addWidget(motor_group)

        # Apply button
        apply_btn = QPushButton("Apply Settings")
        apply_btn.clicked.connect(self.apply_settings)
        self.layout_main.addWidget(apply_btn)

    def apply_settings(self):
        """Apply accessibility settings."""
        self.emit_status("Accessibility settings applied")


class PeerReviewPanel(FeaturePanelBase):
    """UI for structured peer code review and feedback."""

    def __init__(self):
        super().__init__("Peer Review Tool")
        self.reviewer = CodeReviewSession(
            submission_id="demo",
            author="Anonymous",
            code="",
            language="BASIC",
        )
        self.setup_ui()

    def setup_ui(self):
        """Setup peer review panel UI."""
        # Review sessions
        self.sessions_list = QListWidget()
        self.sessions_list.itemClicked.connect(self.on_session_selected)
        self.layout_main.addWidget(QLabel("Review Sessions:"))
        self.layout_main.addWidget(self.sessions_list)

        # Code display
        self.code_display = QTextEdit()
        self.code_display.setReadOnly(True)
        self.layout_main.addWidget(QLabel("Code Under Review:"))
        self.layout_main.addWidget(self.code_display)

        # Comments
        self.comments_list = QListWidget()
        self.layout_main.addWidget(QLabel("Review Comments:"))
        self.layout_main.addWidget(self.comments_list)

        # Add comment
        comment_layout = QHBoxLayout()
        self.comment_input = QLineEdit()
        self.comment_input.setPlaceholderText("Add a review comment...")
        add_comment_btn = QPushButton("Add Comment")
        add_comment_btn.clicked.connect(self.add_comment)
        comment_layout.addWidget(self.comment_input)
        comment_layout.addWidget(add_comment_btn)
        self.layout_main.addLayout(comment_layout)

        # Rubric
        self.rubric_table = QTableWidget()
        self.rubric_table.setColumnCount(3)
        self.rubric_table.setHorizontalHeaderLabels(
            ["Criterion", "Score", "Feedback"]
        )
        self.layout_main.addWidget(QLabel("Review Rubric:"))
        self.layout_main.addWidget(self.rubric_table)

        # Submit review
        submit_btn = QPushButton("Submit Review")
        submit_btn.clicked.connect(self.submit_review)
        self.layout_main.addWidget(submit_btn)

    def on_session_selected(self, _item):
        """Load selected review session."""
        self.emit_status("Loaded review session")

    def add_comment(self):
        """Add a review comment."""
        comment = self.comment_input.text()
        if comment:
            item = QListWidgetItem(comment)
            self.comments_list.addItem(item)
            self.comment_input.clear()

    def submit_review(self):
        """Submit peer review."""
        self.emit_status("Review submitted successfully")


# Export all panels
__all__ = [
    "SyntaxValidatorPanel",
    "ProjectTemplatesPanel",
    "DebuggerPanel",
    "LanguageComparatorPanel",
    "AssetLibraryPanel",
    "CollaborationPanel",
    "PerformanceProfilerPanel",
    "ExecutionReplayPanel",
    "HardwareSimulatorPanel",
    "AIAssistantPanel",
    "ExportableExporterPanel",
    "LearningAnalyticsPanel",
    "AccessibilityPanel",
    "PeerReviewPanel",
]
