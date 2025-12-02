"""
Guided Onboarding - First-run tutorial with interactive tasks.
"""

# pylint: disable=no-name-in-module

from __future__ import annotations
import json
from pathlib import Path
from dataclasses import dataclass
from typing import TYPE_CHECKING
from PySide6.QtWidgets import (
    QDialog,
    QVBoxLayout,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QTextEdit,
    QCheckBox,
)
from PySide6.QtCore import Signal

if TYPE_CHECKING:
    from PySide6.QtWidgets import QMainWindow


@dataclass
class OnboardingStep:  # pylint: disable=too-few-public-methods
    """Single step in the onboarding tutorial."""

    step_id: str
    title: str
    description: str
    instructions: list[str]
    task_description: str
    verification_fn: str  # Name of verification method
    hint: str | None = None


class OnboardingDialog(QDialog):
    """Interactive onboarding tutorial dialog."""

    step_completed = Signal(str)
    tutorial_finished = Signal()

    def __init__(self, parent: QMainWindow | None = None):
        super().__init__(parent)
        self.setWindowTitle("Welcome to Time Warp IDE")
        self.setMinimumSize(700, 500)
        self.setModal(False)

        self.current_step_index = 0
        self.steps = self._create_tutorial_steps()

        self._setup_ui()
        self._show_current_step()

    def _setup_ui(self):
        """Setup dialog UI."""
        layout = QVBoxLayout(self)

        # Header
        self.title_label = QLabel()
        self.title_label.setStyleSheet(
            "font-size: 18pt; font-weight: bold; margin-bottom: 10px;"
        )
        layout.addWidget(self.title_label)

        # Progress indicator
        self.progress_label = QLabel()
        layout.addWidget(self.progress_label)

        # Description
        self.description_label = QLabel()
        self.description_label.setWordWrap(True)
        self.description_label.setStyleSheet("margin: 10px 0;")
        layout.addWidget(self.description_label)

        # Instructions
        instructions_title = QLabel("Instructions:")
        instructions_title.setStyleSheet("font-weight: bold;")
        layout.addWidget(instructions_title)

        self.instructions_text = QTextEdit()
        self.instructions_text.setReadOnly(True)
        self.instructions_text.setMaximumHeight(150)
        layout.addWidget(self.instructions_text)

        # Task area
        task_title = QLabel("Your Task:")
        task_title.setStyleSheet("font-weight: bold; margin-top: 10px;")
        layout.addWidget(task_title)

        self.task_label = QLabel()
        self.task_label.setWordWrap(True)
        self.task_label.setStyleSheet(
            "background-color: #f0f0f0; padding: 10px; border-radius: 5px;"
        )
        layout.addWidget(self.task_label)

        # Hint button
        self.hint_button = QPushButton("Show Hint 💡")
        self.hint_button.clicked.connect(self._show_hint)
        layout.addWidget(self.hint_button)

        self.hint_label = QLabel()
        self.hint_label.setWordWrap(True)
        self.hint_label.setStyleSheet("color: #666; font-style: italic;")
        self.hint_label.setVisible(False)
        layout.addWidget(self.hint_label)

        # Buttons
        button_layout = QHBoxLayout()

        self.skip_checkbox = QCheckBox("Don't show this again")
        button_layout.addWidget(self.skip_checkbox)

        button_layout.addStretch()

        self.back_button = QPushButton("← Back")
        self.back_button.clicked.connect(self._previous_step)
        button_layout.addWidget(self.back_button)

        self.next_button = QPushButton("Next →")
        self.next_button.clicked.connect(self._next_step)
        button_layout.addWidget(self.next_button)

        self.finish_button = QPushButton("Finish")
        self.finish_button.clicked.connect(self._finish_tutorial)
        self.finish_button.setVisible(False)
        button_layout.addWidget(self.finish_button)

        layout.addLayout(button_layout)

    def _create_tutorial_steps(self) -> list[OnboardingStep]:
        """Create tutorial steps."""
        return [
            OnboardingStep(
                step_id="welcome",
                title="Welcome to Time Warp IDE!",
                description="Time Warp IDE is an educational multi-language "
                "programming environment with BASIC, PILOT, and Logo.",
                instructions=[
                    "This tutorial will guide you through the IDE",
                    "You can skip steps or exit at any time",
                    "Interactive tasks help you learn by doing",
                ],
                task_description="Click 'Next' to begin the tour",
                verification_fn="verify_welcome",
            ),
            OnboardingStep(
                step_id="editor_pane",
                title="The Code Editor",
                description="The editor is where you write programs. "
                "It supports BASIC, PILOT, Logo, and more.",
                instructions=[
                    "Look at the large text area on the left",
                    "This is your code editor",
                    "You can type code directly here",
                ],
                task_description='Type a simple BASIC command: PRINT "Hello"',
                verification_fn="verify_editor_input",
                hint='Try typing: PRINT "Hello World!"',
            ),
            OnboardingStep(
                step_id="run_program",
                title="Running Your Code",
                description="Execute code using the Run button or F5 key.",
                instructions=[
                    "Find the Run button in the toolbar (▶ icon)",
                    "Or press F5 on your keyboard",
                    "Output appears in the Output pane",
                ],
                task_description="Run the code you just typed",
                verification_fn="verify_code_run",
                hint="Click the green Run button or press F5",
            ),
            OnboardingStep(
                step_id="turtle_graphics",
                title="Turtle Graphics",
                description="Draw pictures using turtle commands in Logo.",
                instructions=[
                    "Clear the editor (Ctrl+A, Delete)",
                    "Type turtle commands in Logo",
                    "Watch the turtle draw on the canvas",
                ],
                task_description="Draw a square: REPEAT 4 [FD 50 RT 90]",
                verification_fn="verify_turtle_drawing",
                hint="Type: REPEAT 4 [FD 50 RT 90] then press F5",
            ),
            OnboardingStep(
                step_id="languages",
                title="Multiple Languages",
                description="Time Warp supports BASIC, PILOT, Logo, "
                "Pascal, Prolog, and C.",
                instructions=[
                    "Language is auto-detected from file extension",
                    ".bas = BASIC, .pilot = PILOT, .logo = Logo",
                    "Or use File → New to select a language",
                ],
                task_description="Try a PILOT program: T:Hello from PILOT!",
                verification_fn="verify_language_switch",
                hint="Type PILOT commands (T:, A:, M:) and run",
            ),
            OnboardingStep(
                step_id="complete",
                title="You're Ready!",
                description="You've completed the tutorial. "
                "Explore examples and documentation to learn more.",
                instructions=[
                    "Check Examples menu for sample programs",
                    "Use Help menu for documentation",
                    "Join the community for support",
                ],
                task_description="Start creating amazing programs!",
                verification_fn="verify_complete",
            ),
        ]

    def _show_current_step(self):
        """Display the current tutorial step."""
        step = self.steps[self.current_step_index]

        self.title_label.setText(step.title)
        self.progress_label.setText(
            f"Step {self.current_step_index + 1} of {len(self.steps)}"
        )
        self.description_label.setText(step.description)

        # Format instructions
        instructions_items = "".join(
            f"<li>{instruction}</li>" for instruction in step.instructions
        )
        instructions_html = f"<ul>{instructions_items}</ul>"
        self.instructions_text.setHtml(instructions_html)

        self.task_label.setText(f"✅ {step.task_description}")

        # Hint
        self.hint_label.setVisible(False)
        if step.hint:
            self.hint_button.setVisible(True)
            self.hint_label.setText(f"💡 {step.hint}")
        else:
            self.hint_button.setVisible(False)

        # Button states
        self.back_button.setEnabled(self.current_step_index > 0)

        is_last_step = self.current_step_index == len(self.steps) - 1
        self.next_button.setVisible(not is_last_step)
        self.finish_button.setVisible(is_last_step)

    def _next_step(self):
        """Move to next step."""
        if self.current_step_index < len(self.steps) - 1:
            self.step_completed.emit(self.steps[self.current_step_index].step_id)
            self.current_step_index += 1
            self._show_current_step()

    def _previous_step(self):
        """Move to previous step."""
        if self.current_step_index > 0:
            self.current_step_index -= 1
            self._show_current_step()

    def _show_hint(self):
        """Show hint for current step."""
        self.hint_label.setVisible(True)

    def _finish_tutorial(self):
        """Finish tutorial and close dialog."""
        self.step_completed.emit(self.steps[self.current_step_index].step_id)
        self.tutorial_finished.emit()
        self.accept()

    def should_skip_onboarding(self) -> bool:
        """Check if user wants to skip onboarding."""
        return self.skip_checkbox.isChecked()


class OnboardingManager:
    """Manages onboarding state and persistence."""

    def __init__(self, config_dir: Path | None = None):
        if config_dir is None:
            config_dir = Path.home() / ".Time_Warp"
        self.config_dir = config_dir
        self.config_dir.mkdir(parents=True, exist_ok=True)
        self.config_file = self.config_dir / "onboarding.json"

        self.completed_steps: set[str] = set()
        self.tutorial_completed = False
        self.skip_onboarding = False

        self._load_state()

    def _load_state(self):
        """Load onboarding state from disk."""
        if self.config_file.exists():
            try:
                with open(self.config_file, "r", encoding="utf-8") as f:
                    data = json.load(f)
                    self.completed_steps = set(data.get("completed_steps", []))
                    self.tutorial_completed = data.get("tutorial_completed", False)
                    self.skip_onboarding = data.get("skip_onboarding", False)
            except (json.JSONDecodeError, OSError):
                pass

    def _save_state(self):
        """Save onboarding state to disk."""
        data = {
            "completed_steps": list(self.completed_steps),
            "tutorial_completed": self.tutorial_completed,
            "skip_onboarding": self.skip_onboarding,
        }

        with open(self.config_file, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)

    def should_show_onboarding(self) -> bool:
        """Check if onboarding should be shown."""
        return not self.tutorial_completed and not self.skip_onboarding

    def mark_step_completed(self, step_id: str):
        """Mark a step as completed."""
        self.completed_steps.add(step_id)
        self._save_state()

    def mark_tutorial_completed(self, skip: bool = False):
        """Mark tutorial as completed."""
        self.tutorial_completed = True
        self.skip_onboarding = skip
        self._save_state()

    def reset_onboarding(self):
        """Reset onboarding state (for testing)."""
        self.completed_steps.clear()
        self.tutorial_completed = False
        self.skip_onboarding = False
        self._save_state()
