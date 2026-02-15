"""
Feature UI Panels for Time Warp Studio v7.0.0

This module provides PySide6 Qt widget panels for all 14 educational features.
Each panel wraps a core module and provides a user-friendly interface.
"""

# PySide6 symbols are provided at runtime; silence pylint import resolution
# errors for these modules in this file.
# pylint: disable=no-name-in-module

from typing import Optional

from PySide6.QtCore import Qt, Signal, QTimer
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
    QSlider,
)

from ..core.interpreter import Language
from ..core.accessibility import AccessibilityManager
from ..core.asset_library import AssetLibrary
from ..core.collaboration import LocalCollaborationSession, SessionManager
from ..core.debugger import CodeDebugger
from ..core.ai_assistant import LocalAIAssistant
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
from ..features.lesson_system import LessonManager, LessonStatus
from ..features.classroom_mode import ClassroomMode
from ..features.reference_search import ReferenceIndex
from ..features.achievements import ProgressTracker
from ..utils.error_hints import get_enhanced_error_message


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
            item.setData(Qt.ItemDataRole.UserRole, tpl)
            self.templates_list.addItem(item)

    def on_template_selected(self, item):
        """Show template details."""
        template = item.data(Qt.ItemDataRole.UserRole)
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

        template = item.data(Qt.ItemDataRole.UserRole)
        self.emit_status(f"Creating project from {template.name}...")


class LessonModePanel(FeaturePanelBase):
    """UI for guided lessons with checkpoints and hints."""

    lesson_started = Signal(str, str)  # (language, starter_code)
    lesson_checkpoint_ready = Signal(str, str)  # (language, starter_code)
    export_markdown_requested = Signal()
    export_pdf_requested = Signal()
    lesson_completed = Signal(str)

    def __init__(self):
        super().__init__("Lesson Mode")
        self.lesson_manager = LessonManager()
        self._hint_index = 0
        self._setup_ui()
        self._load_lessons()

    def _setup_ui(self):
        """Setup lesson mode UI."""
        splitter = QSplitter(Qt.Horizontal)

        # Lesson list
        list_widget = QWidget()
        list_layout = QVBoxLayout(list_widget)
        list_layout.setContentsMargins(0, 0, 0, 0)

        list_layout.addWidget(QLabel("Lessons:"))
        self.lesson_list = QListWidget()
        self.lesson_list.currentItemChanged.connect(
            self._on_lesson_selected
        )
        list_layout.addWidget(self.lesson_list)

        splitter.addWidget(list_widget)

        # Lesson details
        details_widget = QWidget()
        details_layout = QVBoxLayout(details_widget)
        details_layout.setContentsMargins(0, 0, 0, 0)

        self.lesson_title = QLabel("Select a lesson")
        self.lesson_title.setStyleSheet("font-weight: bold; font-size: 14px;")
        details_layout.addWidget(self.lesson_title)

        self.lesson_description = QTextEdit()
        self.lesson_description.setReadOnly(True)
        details_layout.addWidget(self.lesson_description)

        self.checkpoint_title = QLabel("Checkpoint: -")
        details_layout.addWidget(self.checkpoint_title)

        self.checkpoint_description = QTextEdit()
        self.checkpoint_description.setReadOnly(True)
        details_layout.addWidget(self.checkpoint_description)

        self.status_label = QLabel("Status: idle")
        details_layout.addWidget(self.status_label)

        button_row = QHBoxLayout()
        self.start_button = QPushButton("Start Lesson")
        self.start_button.clicked.connect(self._start_lesson)
        button_row.addWidget(self.start_button)

        self.hint_button = QPushButton("Show Hint")
        self.hint_button.clicked.connect(self._show_hint)
        self.hint_button.setEnabled(False)
        button_row.addWidget(self.hint_button)

        self.solution_button = QPushButton("Show Solution")
        self.solution_button.clicked.connect(self._show_solution)
        self.solution_button.setEnabled(False)
        button_row.addWidget(self.solution_button)

        export_md_btn = QPushButton("Export Markdown")
        export_md_btn.clicked.connect(self.export_markdown_requested.emit)
        button_row.addWidget(export_md_btn)

        export_pdf_btn = QPushButton("Export PDF")
        export_pdf_btn.clicked.connect(self.export_pdf_requested.emit)
        button_row.addWidget(export_pdf_btn)

        details_layout.addLayout(button_row)

        self.hint_output = QTextEdit()
        self.hint_output.setReadOnly(True)
        self.hint_output.setPlaceholderText("Hints and solutions appear here...")
        details_layout.addWidget(self.hint_output)

        splitter.addWidget(details_widget)
        splitter.setSizes([220, 520])

        self.layout_main.addWidget(splitter)

    def _load_lessons(self):
        """Populate lessons list."""
        self.lesson_list.clear()
        for lesson in self.lesson_manager.list_lessons():
            item = QListWidgetItem(f"📘 {lesson.title}")
            item.setData(Qt.ItemDataRole.UserRole, lesson.id)
            self.lesson_list.addItem(item)

    def _on_lesson_selected(self, current: QListWidgetItem, _previous):
        """Display details for selected lesson."""
        if current is None:
            return
        lesson_id = current.data(Qt.ItemDataRole.UserRole)
        lesson = self.lesson_manager.lessons.get(lesson_id)
        if not lesson:
            return

        self.lesson_title.setText(lesson.title)
        self.lesson_description.setText(lesson.description)
        self._update_checkpoint_ui(lesson)
        self.hint_output.clear()

    def _start_lesson(self):
        """Start the selected lesson."""
        item = self.lesson_list.currentItem()
        if not item:
            return
        lesson_id = item.data(Qt.ItemDataRole.UserRole)
        if not self.lesson_manager.start_lesson(lesson_id):
            return

        self._hint_index = 0
        lesson = self.lesson_manager.current_lesson
        if not lesson:
            return

        checkpoint = self.lesson_manager.get_current_checkpoint()
        if checkpoint:
            self.lesson_started.emit(lesson.language, checkpoint.starter_code)

        self._update_checkpoint_ui(lesson)
        self.hint_button.setEnabled(True)
        self.solution_button.setEnabled(True)
        self.emit_status(f"Lesson started: {lesson.title}")

    def _update_checkpoint_ui(self, lesson):
        """Refresh checkpoint details UI."""
        status_text = lesson.status.value if lesson else "idle"
        self.status_label.setText(f"Status: {status_text}")

        checkpoint = self.lesson_manager.get_current_checkpoint()
        if checkpoint:
            self.checkpoint_title.setText(f"Checkpoint: {checkpoint.title}")
            self.checkpoint_description.setText(checkpoint.description)
        else:
            self.checkpoint_title.setText("Checkpoint: -")
            self.checkpoint_description.setText("")

    def _show_hint(self):
        """Show next hint for the current checkpoint."""
        checkpoint = self.lesson_manager.get_current_checkpoint()
        if not checkpoint or not checkpoint.hints:
            self.hint_output.setText("No hints available.")
            return

        hint_index = min(self._hint_index, len(checkpoint.hints) - 1)
        hint = checkpoint.hints[hint_index]
        self.hint_output.setText(f"Hint {hint_index + 1}: {hint}")
        if self._hint_index < len(checkpoint.hints) - 1:
            self._hint_index += 1

    def _show_solution(self):
        """Show solution for the current checkpoint."""
        solution = self.lesson_manager.get_solution()
        if solution:
            self.hint_output.setText("Solution:\n" + solution)
        else:
            self.hint_output.setText("No solution available.")

    def handle_execution_output(self, output_text: str):
        """Check output against current checkpoint and advance if correct."""
        lesson = self.lesson_manager.current_lesson
        if not lesson:
            return

        if not self.lesson_manager.get_current_checkpoint():
            return

        if self.lesson_manager.check_output(output_text):
            advanced = self.lesson_manager.advance_checkpoint()
            if lesson.status == LessonStatus.COMPLETED:
                self.status_label.setText("Status: completed")
                self.hint_output.setText("✅ Lesson complete!")
                self.emit_status("Lesson completed")
                self.lesson_completed.emit(lesson.id)
            elif advanced:
                self._hint_index = 0
                checkpoint = self.lesson_manager.get_current_checkpoint()
                if checkpoint:
                    self.lesson_checkpoint_ready.emit(
                        lesson.language, checkpoint.starter_code
                    )
                self._update_checkpoint_ui(lesson)
                self.hint_output.setText("✅ Checkpoint passed. Next up!")
            else:
                self._update_checkpoint_ui(lesson)
        else:
            self.hint_output.setText("❌ Output did not match. Try again.")

    def get_session_snapshot(self) -> dict:
        """Return summary details for export."""
        lesson = self.lesson_manager.current_lesson
        if not lesson:
            return {
                "status": "idle",
            }

        checkpoint = self.lesson_manager.get_current_checkpoint()
        return {
            "lesson_id": lesson.id,
            "lesson_title": lesson.title,
            "lesson_description": lesson.description,
            "language": lesson.language,
            "difficulty": lesson.difficulty,
            "status": lesson.status.value,
            "checkpoint_index": lesson.current_checkpoint + 1,
            "checkpoint_total": len(lesson.checkpoints),
            "checkpoint_title": checkpoint.title if checkpoint else "",
            "checkpoint_description": checkpoint.description
            if checkpoint
            else "",
        }


class ProjectRunnerPanel(FeaturePanelBase):
    """UI for running multiple tabs as a project."""

    run_requested = Signal(list, dict)  # (tab_indices, options)
    stop_requested = Signal()
    refresh_requested = Signal()

    def __init__(self):
        super().__init__("Project Runner")
        self._setup_ui()

    def _setup_ui(self):
        """Setup project runner UI."""
        layout = self.layout_main

        layout.addWidget(QLabel("Open Tabs:"))
        self.tabs_list = QListWidget()
        layout.addWidget(self.tabs_list)

        options_group = QGroupBox("Run Options")
        options_layout = QVBoxLayout(options_group)
        self.clear_output = QCheckBox("Clear output between runs")
        self.clear_output.setChecked(True)
        self.clear_canvas = QCheckBox("Clear canvas between runs")
        self.clear_canvas.setChecked(False)
        options_layout.addWidget(self.clear_output)
        options_layout.addWidget(self.clear_canvas)
        layout.addWidget(options_group)

        button_row = QHBoxLayout()
        refresh_btn = QPushButton("Refresh Tabs")
        refresh_btn.clicked.connect(self.refresh_requested.emit)
        button_row.addWidget(refresh_btn)

        run_btn = QPushButton("Run Selected")
        run_btn.clicked.connect(self._run_selected)
        button_row.addWidget(run_btn)

        stop_btn = QPushButton("Stop Project")
        stop_btn.clicked.connect(self.stop_requested.emit)
        button_row.addWidget(stop_btn)

        layout.addLayout(button_row)

    def update_tabs(self, tabs: list[dict]):
        """Update list of available tabs."""
        self.tabs_list.clear()
        for tab in tabs:
            title = tab.get("title", "Untitled")
            language = tab.get("language", "BASIC")
            index = tab.get("index", -1)
            item = QListWidgetItem(f"{title} ({language})")
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(Qt.CheckState.Unchecked)
            item.setData(Qt.ItemDataRole.UserRole, index)
            self.tabs_list.addItem(item)

    def _run_selected(self):
        """Emit run request for selected tabs."""
        selected = []
        for i in range(self.tabs_list.count()):
            item = self.tabs_list.item(i)
            if item.checkState() == Qt.Checked:
                idx = item.data(Qt.ItemDataRole.UserRole)
                if idx is not None and idx >= 0:
                    selected.append(int(idx))

        options = {
            "clear_output": self.clear_output.isChecked(),
            "clear_canvas": self.clear_canvas.isChecked(),
        }
        if selected:
            self.run_requested.emit(selected, options)
        else:
            self.emit_status("No tabs selected")


class TurtleInspectorPanel(FeaturePanelBase):
    """Live turtle inspector with timeline scrubber."""

    snapshot_selected = Signal(object)

    def __init__(self):
        super().__init__("Turtle Inspector")
        self._snapshots = []
        self._play_timer = QTimer(self)
        self._play_timer.setInterval(120)
        self._play_timer.timeout.connect(self._advance_frame)
        self._setup_ui()

    def _setup_ui(self):
        """Setup turtle inspector UI."""
        layout = self.layout_main

        self.state_label = QLabel("No turtle data")
        self.state_label.setStyleSheet("font-weight: bold;")
        layout.addWidget(self.state_label)

        self.details_text = QTextEdit()
        self.details_text.setReadOnly(True)
        layout.addWidget(self.details_text)

        slider_row = QHBoxLayout()
        self.frame_label = QLabel("Frame 0 / 0")
        slider_row.addWidget(self.frame_label)

        self.frame_slider = QSlider(Qt.Horizontal)
        self.frame_slider.setMinimum(0)
        self.frame_slider.setMaximum(0)
        self.frame_slider.valueChanged.connect(self._on_frame_changed)
        slider_row.addWidget(self.frame_slider, 1)
        layout.addLayout(slider_row)

        buttons = QHBoxLayout()
        play_btn = QPushButton("Play")
        play_btn.clicked.connect(self._start_playback)
        buttons.addWidget(play_btn)

        pause_btn = QPushButton("Pause")
        pause_btn.clicked.connect(self._stop_playback)
        buttons.addWidget(pause_btn)

        clear_btn = QPushButton("Clear Timeline")
        clear_btn.clicked.connect(self.clear_timeline)
        buttons.addWidget(clear_btn)
        layout.addLayout(buttons)

    def add_snapshot(self, turtle_state):
        """Add a turtle state snapshot to the timeline."""
        if turtle_state is None:
            return
        self._snapshots.append(turtle_state)
        self.frame_slider.blockSignals(True)
        self.frame_slider.setMaximum(len(self._snapshots) - 1)
        self.frame_slider.setValue(len(self._snapshots) - 1)
        self.frame_slider.blockSignals(False)
        self._update_frame_display(len(self._snapshots) - 1)

    def clear_timeline(self):
        """Clear recorded turtle snapshots."""
        self._snapshots = []
        self.frame_slider.setMaximum(0)
        self.frame_slider.setValue(0)
        self.frame_label.setText("Frame 0 / 0")
        self.state_label.setText("No turtle data")
        self.details_text.clear()
        self._stop_playback()

    def _start_playback(self):
        """Start timeline playback."""
        if self._snapshots:
            self._play_timer.start()

    def _stop_playback(self):
        """Stop timeline playback."""
        self._play_timer.stop()

    def _advance_frame(self):
        """Advance to the next frame during playback."""
        if not self._snapshots:
            return
        current = self.frame_slider.value()
        next_frame = current + 1
        if next_frame >= len(self._snapshots):
            self._stop_playback()
            return
        self.frame_slider.setValue(next_frame)

    def _on_frame_changed(self, index: int):
        """Handle scrubber updates."""
        self._update_frame_display(index)
        if 0 <= index < len(self._snapshots):
            self.snapshot_selected.emit(self._snapshots[index])

    def _update_frame_display(self, index: int):
        """Update text display for the current frame."""
        total = len(self._snapshots)
        self.frame_label.setText(f"Frame {index + 1} / {total}")
        if not self._snapshots or index < 0 or index >= total:
            return

        snap = self._snapshots[index]
        x = getattr(snap, "x", 0.0)
        y = getattr(snap, "y", 0.0)
        heading = getattr(snap, "heading", 0.0)
        pen_down = getattr(snap, "pen_down", True)
        color = getattr(snap, "pen_color", (255, 255, 255))
        width = getattr(snap, "pen_width", 2.0)
        visible = getattr(snap, "visible", True)
        lines = len(getattr(snap, "lines", []) or [])

        self.state_label.setText("Live Turtle State")
        details = (
            f"Position: ({x:.2f}, {y:.2f})\n"
            f"Heading: {heading:.1f}°\n"
            f"Pen: {'down' if pen_down else 'up'}\n"
            f"Color: {color}\n"
            f"Width: {width}\n"
            f"Visible: {'yes' if visible else 'no'}\n"
            f"Lines: {lines}"
        )
        self.details_text.setText(details)


class ErrorExplainerPanel(FeaturePanelBase):
    """Explain interpreter errors with friendly tips and examples."""

    def __init__(self):
        super().__init__("Error Explainer")
        self.assistant = LocalAIAssistant()
        self._setup_ui()

    def _setup_ui(self):
        """Setup error explainer UI."""
        layout = self.layout_main

        self.error_label = QLabel("No errors yet")
        self.error_label.setStyleSheet("font-weight: bold;")
        layout.addWidget(self.error_label)

        self.enhanced_text = QTextEdit()
        self.enhanced_text.setReadOnly(True)
        layout.addWidget(self.enhanced_text)

        self.ai_text = QTextEdit()
        self.ai_text.setReadOnly(True)
        layout.addWidget(self.ai_text)

    def set_error_context(self, error_message: str, context: str = ""):
        """Update panel with an error and suggested fixes."""
        self.error_label.setText(f"Error: {error_message}")
        enhanced = get_enhanced_error_message(error_message, context)
        self.enhanced_text.setText(enhanced)

        suggestion = self.assistant.explain_error(error_message)
        self.ai_text.setText(suggestion.explanation)


class ClassroomModePanel(FeaturePanelBase):
    """UI for classroom and presentation controls."""

    start_presentation = Signal(dict)
    stop_presentation = Signal()
    export_bundle_requested = Signal(str, str)
    import_bundle_requested = Signal(str)
    broadcast_code_requested = Signal()
    collect_outputs_requested = Signal()

    def __init__(self):
        super().__init__("Classroom Mode")
        self.classroom = ClassroomMode()
        self._setup_ui()

    def _setup_ui(self):
        """Setup classroom mode UI."""
        layout = self.layout_main

        presentation_group = QGroupBox("Presentation Mode")
        presentation_layout = QFormLayout(presentation_group)

        self.font_size = QSpinBox()
        self.font_size.setRange(10, 36)
        self.font_size.setValue(16)
        presentation_layout.addRow("Font size:", self.font_size)

        self.read_only = QCheckBox("Read-only editors")
        self.read_only.setChecked(True)
        presentation_layout.addRow("", self.read_only)

        self.fullscreen = QCheckBox("Fullscreen")
        self.fullscreen.setChecked(True)
        presentation_layout.addRow("", self.fullscreen)

        self.hide_menus = QCheckBox("Hide menus")
        presentation_layout.addRow("", self.hide_menus)

        self.lock_theme = QCheckBox("Lock theme/settings")
        presentation_layout.addRow("", self.lock_theme)

        layout.addWidget(presentation_group)

        presentation_buttons = QHBoxLayout()
        start_btn = QPushButton("Start Presentation")
        start_btn.clicked.connect(self._emit_start)
        presentation_buttons.addWidget(start_btn)

        stop_btn = QPushButton("Stop Presentation")
        stop_btn.clicked.connect(self.stop_presentation.emit)
        presentation_buttons.addWidget(stop_btn)
        layout.addLayout(presentation_buttons)

        bundle_group = QGroupBox("Bundles")
        bundle_layout = QFormLayout(bundle_group)
        self.bundle_name = QLineEdit()
        self.bundle_name.setPlaceholderText("Lesson or assignment name")
        bundle_layout.addRow("Name:", self.bundle_name)
        self.bundle_desc = QLineEdit()
        self.bundle_desc.setPlaceholderText("Description")
        bundle_layout.addRow("Description:", self.bundle_desc)
        layout.addWidget(bundle_group)

        bundle_buttons = QHBoxLayout()
        export_btn = QPushButton("Export Bundle")
        export_btn.clicked.connect(self._emit_export)
        bundle_buttons.addWidget(export_btn)

        import_btn = QPushButton("Import Bundle")
        import_btn.clicked.connect(self._emit_import)
        bundle_buttons.addWidget(import_btn)
        layout.addLayout(bundle_buttons)

        classroom_buttons = QHBoxLayout()
        broadcast_btn = QPushButton("Broadcast Sample Code")
        broadcast_btn.clicked.connect(self.broadcast_code_requested.emit)
        classroom_buttons.addWidget(broadcast_btn)

        collect_btn = QPushButton("Collect Outputs")
        collect_btn.clicked.connect(self.collect_outputs_requested.emit)
        classroom_buttons.addWidget(collect_btn)
        layout.addLayout(classroom_buttons)

    def _emit_start(self):
        """Emit presentation start request."""
        settings = {
            "font_size": self.font_size.value(),
            "read_only": self.read_only.isChecked(),
            "fullscreen": self.fullscreen.isChecked(),
            "hide_menus": self.hide_menus.isChecked(),
            "lock_theme": self.lock_theme.isChecked(),
        }
        self.start_presentation.emit(settings)

    def _emit_export(self):
        """Emit bundle export request."""
        name = self.bundle_name.text().strip() or "Classroom Bundle"
        desc = self.bundle_desc.text().strip() or ""
        self.export_bundle_requested.emit(name, desc)

    def _emit_import(self):
        """Emit bundle import request with selected path."""
        path, _ = QFileDialog.getOpenFileName(
            self,
            "Import Bundle",
            "",
            "Bundle Files (*.zip);;All Files (*)",
        )
        if path:
            self.import_bundle_requested.emit(path)


class ReferenceSearchPanel(FeaturePanelBase):
    """Offline reference search across docs and examples."""

    open_reference_requested = Signal(str)

    def __init__(self):
        super().__init__("Reference Search")
        self.index = ReferenceIndex()
        self._setup_ui()
        self._populate_tags()
        self._refresh_results()

    def _setup_ui(self):
        """Setup reference search UI."""
        layout = self.layout_main

        search_row = QHBoxLayout()
        self.query_edit = QLineEdit()
        self.query_edit.setPlaceholderText("Search docs and examples...")
        self.query_edit.textChanged.connect(self._refresh_results)
        search_row.addWidget(self.query_edit, 1)

        self.tag_combo = QComboBox()
        self.tag_combo.currentIndexChanged.connect(self._refresh_results)
        search_row.addWidget(self.tag_combo)

        refresh_btn = QPushButton("Rescan")
        refresh_btn.clicked.connect(self._rescan)
        search_row.addWidget(refresh_btn)
        layout.addLayout(search_row)

        splitter = QSplitter(Qt.Horizontal)
        list_container = QWidget()
        list_layout = QVBoxLayout(list_container)
        list_layout.setContentsMargins(0, 0, 0, 0)
        list_layout.addWidget(QLabel("Results:"))
        self.results_list = QListWidget()
        self.results_list.currentItemChanged.connect(self._on_result_selected)
        list_layout.addWidget(self.results_list)
        splitter.addWidget(list_container)

        preview_container = QWidget()
        preview_layout = QVBoxLayout(preview_container)
        preview_layout.setContentsMargins(0, 0, 0, 0)
        preview_layout.addWidget(QLabel("Preview:"))
        self.preview_text = QTextEdit()
        self.preview_text.setReadOnly(True)
        preview_layout.addWidget(self.preview_text)

        open_btn = QPushButton("Open in Editor")
        open_btn.clicked.connect(self._open_selected)
        preview_layout.addWidget(open_btn)
        splitter.addWidget(preview_container)

        splitter.setSizes([300, 500])
        layout.addWidget(splitter)

    def _populate_tags(self):
        """Populate tag filter from index."""
        self.tag_combo.blockSignals(True)
        self.tag_combo.clear()
        self.tag_combo.addItem("All")
        for tag in self.index.get_tags():
            self.tag_combo.addItem(tag)
        self.tag_combo.blockSignals(False)

    def _rescan(self):
        """Rescan the index."""
        self.index.scan()
        self._populate_tags()
        self._refresh_results()

    def _refresh_results(self):
        """Refresh search results."""
        query = self.query_edit.text().strip()
        tag = self.tag_combo.currentText()
        if tag == "All":
            tag = None
        results = self.index.search(query, tag)

        self.results_list.clear()
        for entry in results:
            item = QListWidgetItem(f"{entry.title} [{entry.source}]")
            item.setData(Qt.ItemDataRole.UserRole, entry)
            self.results_list.addItem(item)

        if results:
            self.results_list.setCurrentRow(0)
        else:
            self.preview_text.clear()

    def _on_result_selected(self, current: QListWidgetItem, _previous):
        """Show preview for selected entry."""
        if not current:
            return
        entry = current.data(Qt.ItemDataRole.UserRole)
        if not entry:
            return
        meta = f"Path: {entry.path}\nTags: {', '.join(entry.tags)}\n"
        self.preview_text.setText(meta + "\n" + entry.snippet)

    def _open_selected(self):
        """Open selected reference entry in editor."""
        item = self.results_list.currentItem()
        if not item:
            return
        entry = item.data(Qt.ItemDataRole.UserRole)
        if entry:
            self.open_reference_requested.emit(str(entry.path))


class AchievementsPanel(FeaturePanelBase):
    """Track tutorial and example progress with badges."""

    def __init__(self):
        super().__init__("Achievements")
        self.tracker = ProgressTracker()
        self._setup_ui()
        self.refresh()

    def _setup_ui(self):
        """Setup achievements UI."""
        layout = self.layout_main

        self.tutorial_progress = QProgressBar()
        self.example_progress = QProgressBar()
        layout.addWidget(QLabel("Tutorial Progress"))
        layout.addWidget(self.tutorial_progress)
        layout.addWidget(QLabel("Example Progress"))
        layout.addWidget(self.example_progress)

        splitter = QSplitter(Qt.Horizontal)

        tutorial_widget = QWidget()
        tutorial_layout = QVBoxLayout(tutorial_widget)
        tutorial_layout.setContentsMargins(0, 0, 0, 0)
        tutorial_layout.addWidget(QLabel("Tutorials"))
        self.tutorial_list = QListWidget()
        self.tutorial_list.itemChanged.connect(self._on_tutorial_toggled)
        tutorial_layout.addWidget(self.tutorial_list)
        splitter.addWidget(tutorial_widget)

        example_widget = QWidget()
        example_layout = QVBoxLayout(example_widget)
        example_layout.setContentsMargins(0, 0, 0, 0)
        example_layout.addWidget(QLabel("Examples"))
        self.example_list = QListWidget()
        self.example_list.itemChanged.connect(self._on_example_toggled)
        example_layout.addWidget(self.example_list)
        splitter.addWidget(example_widget)

        splitter.setSizes([300, 300])
        layout.addWidget(splitter)

        layout.addWidget(QLabel("Badges"))
        self.badge_list = QListWidget()
        layout.addWidget(self.badge_list)

        refresh_btn = QPushButton("Refresh Progress")
        refresh_btn.clicked.connect(self.refresh)
        layout.addWidget(refresh_btn)

    def refresh(self):
        """Refresh progress lists and badges."""
        self._refresh_lists()
        self._refresh_badges()

    def record_example_run(self, path: str):
        """Mark an example as completed when run."""
        self.tracker.record_example_run(path)
        self.refresh()

    def _refresh_lists(self):
        """Refresh tutorial and example lists."""
        completed_tutorials = set(
            self.tracker.state.get("completed_tutorials", [])
        )
        completed_examples = set(
            self.tracker.state.get("completed_examples", [])
        )

        self.tutorial_list.blockSignals(True)
        self.tutorial_list.clear()
        for tutorial in self.tracker.get_tutorials():
            item = QListWidgetItem(tutorial)
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(
                Qt.CheckState.Checked
                if tutorial in completed_tutorials
                else Qt.CheckState.Unchecked
            )
            item.setData(Qt.ItemDataRole.UserRole, tutorial)
            self.tutorial_list.addItem(item)
        self.tutorial_list.blockSignals(False)

        self.example_list.blockSignals(True)
        self.example_list.clear()
        for example in self.tracker.get_examples():
            item = QListWidgetItem(example)
            item.setFlags(item.flags() | Qt.ItemFlag.ItemIsUserCheckable)
            item.setCheckState(
                Qt.CheckState.Checked
                if example in completed_examples
                else Qt.CheckState.Unchecked
            )
            item.setData(Qt.ItemDataRole.UserRole, example)
            self.example_list.addItem(item)
        self.example_list.blockSignals(False)

        progress = self.tracker.get_progress()
        self.tutorial_progress.setValue(int(progress["tutorial_percent"]))
        self.example_progress.setValue(int(progress["example_percent"]))

    def _refresh_badges(self):
        """Refresh badge list."""
        self.badge_list.clear()
        for badge in self.tracker.get_badges():
            status = "✅" if badge.unlocked else "🔒"
            item = QListWidgetItem(f"{status} {badge.name} - {badge.description}")
            self.badge_list.addItem(item)

    def _on_tutorial_toggled(self, item: QListWidgetItem):
        """Handle tutorial completion toggle."""
        path = item.data(Qt.ItemDataRole.UserRole)
        completed = item.checkState() == Qt.CheckState.Checked
        self.tracker.mark_tutorial_completed(path, completed)
        self._refresh_badges()
        self._refresh_lists()

    def _on_example_toggled(self, item: QListWidgetItem):
        """Handle example completion toggle."""
        path = item.data(Qt.ItemDataRole.UserRole)
        completed = item.checkState() == Qt.CheckState.Checked
        self.tracker.mark_example_completed(path, completed)
        self._refresh_badges()
        self._refresh_lists()


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
            item.setData(Qt.ItemDataRole.UserRole, asset)
            self.assets_list.addItem(item)

    def on_asset_selected(self, item):
        """Show asset preview."""
        asset = item.data(Qt.ItemDataRole.UserRole)
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

        asset = item.data(Qt.ItemDataRole.UserRole)
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
    "LessonModePanel",
    "ErrorExplainerPanel",
    "ClassroomModePanel",
    "ReferenceSearchPanel",
    "AchievementsPanel",
    "SyntaxValidatorPanel",
    "ProjectTemplatesPanel",
    "ProjectRunnerPanel",
    "TurtleInspectorPanel",
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
