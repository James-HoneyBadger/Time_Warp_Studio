"""Lessons dock: browse the curriculum, load starter code, get hints, verify checkpoints."""

from __future__ import annotations

from typing import Callable

from PySide6.QtCore import QThread, Qt, Signal
from PySide6.QtWidgets import (
    QDockWidget,
    QHBoxLayout,
    QLabel,
    QPlainTextEdit,
    QPushButton,
    QSplitter,
    QTextBrowser,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)

from ..core.interpreter import Language
from ..features.lesson_system import LessonManager, LessonStatus, VerifyResult

_STATUS_ICON = {
    LessonStatus.NOT_STARTED: "○",
    LessonStatus.IN_PROGRESS: "◐",
    LessonStatus.COMPLETED: "●",
}


class _VerifyThread(QThread):
    """Runs checkpoint verification off the UI thread (programs may loop)."""

    finished_with = Signal(object)

    def __init__(self, manager: LessonManager, lesson_id: str, index: int, source: str):
        super().__init__()
        self._args = (lesson_id, index, source)
        self._manager = manager

    def run(self) -> None:
        self.finished_with.emit(self._manager.verify(*self._args))


class LessonPanel(QDockWidget):
    """Dockable lesson browser with checkpoint verification."""

    def __init__(
        self,
        manager: LessonManager,
        get_code: Callable[[], str],
        load_code: Callable[[Language, str], None],
        parent: QWidget | None = None,
    ) -> None:
        super().__init__("🎓 Lessons", parent)
        self.setObjectName("LessonPanel")
        self.manager = manager
        self._get_code = get_code
        self._load_code = load_code
        self._lesson_id: str | None = None
        self._index = 0
        self._hint_number = 0
        self._worker: _VerifyThread | None = None
        self._build_ui()
        self.refresh_tree()

    # ---- UI ---------------------------------------------------------------

    def _build_ui(self) -> None:
        body = QWidget()
        layout = QVBoxLayout(body)
        layout.setContentsMargins(4, 4, 4, 4)
        splitter = QSplitter(Qt.Orientation.Vertical)

        self.tree = QTreeWidget()
        self.tree.setHeaderLabels(["Lesson", "Level"])
        self.tree.setColumnWidth(0, 230)
        self.tree.currentItemChanged.connect(self._on_item_changed)
        splitter.addWidget(self.tree)

        detail = QWidget()
        dlay = QVBoxLayout(detail)
        dlay.setContentsMargins(0, 0, 0, 0)
        self.title_label = QLabel("Select a lesson")
        self.title_label.setWordWrap(True)
        self.title_label.setStyleSheet("font-weight: bold; font-size: 13px;")
        self.step_label = QLabel("")
        self.description = QTextBrowser()
        self.description.setMaximumHeight(110)
        self.feedback = QPlainTextEdit()
        self.feedback.setReadOnly(True)
        self.feedback.setPlaceholderText("Verification results appear here.")
        dlay.addWidget(self.title_label)
        dlay.addWidget(self.step_label)
        dlay.addWidget(self.description)

        row1 = QHBoxLayout()
        self.load_btn = QPushButton("📥 Load Starter")
        self.hint_btn = QPushButton("💡 Hint")
        self.verify_btn = QPushButton("✔ Verify")
        row1.addWidget(self.load_btn)
        row1.addWidget(self.hint_btn)
        row1.addWidget(self.verify_btn)
        row2 = QHBoxLayout()
        self.solution_btn = QPushButton("🔓 Show Solution")
        self.prev_btn = QPushButton("◀")
        self.next_btn = QPushButton("▶")
        self.reset_btn = QPushButton("↺ Reset")
        for btn in (self.prev_btn, self.next_btn):
            btn.setFixedWidth(32)
        self.prev_btn.setToolTip("Previous checkpoint")
        self.next_btn.setToolTip("Next unlocked checkpoint")
        row2.addWidget(self.solution_btn)
        row2.addWidget(self.prev_btn)
        row2.addWidget(self.next_btn)
        row2.addWidget(self.reset_btn)
        dlay.addLayout(row1)
        dlay.addLayout(row2)
        dlay.addWidget(self.feedback)
        splitter.addWidget(detail)
        splitter.setSizes([200, 360])
        layout.addWidget(splitter)
        self.setWidget(body)

        self.load_btn.clicked.connect(self.load_starter)
        self.hint_btn.clicked.connect(self.show_hint)
        self.verify_btn.clicked.connect(self.verify)
        self.solution_btn.clicked.connect(self.show_solution)
        self.prev_btn.clicked.connect(lambda: self._step(-1))
        self.next_btn.clicked.connect(lambda: self._step(1))
        self.reset_btn.clicked.connect(self.reset_lesson)
        self._update_detail()

    def refresh_tree(self) -> None:
        """Rebuild the lesson tree, keeping the current selection."""
        selected = self._lesson_id
        self.tree.blockSignals(True)
        self.tree.clear()
        groups: dict[Language, QTreeWidgetItem] = {}
        for lesson in self.manager.list_lessons():
            group = groups.get(lesson.language)
            if group is None:
                group = QTreeWidgetItem([lesson.language.friendly_name(), ""])
                group.setFlags(group.flags() & ~Qt.ItemFlag.ItemIsSelectable)
                self.tree.addTopLevelItem(group)
                groups[lesson.language] = group
            icon = _STATUS_ICON[self.manager.status(lesson.id)]
            item = QTreeWidgetItem([f"{icon} {lesson.title}", lesson.difficulty])
            item.setData(0, Qt.ItemDataRole.UserRole, lesson.id)
            item.setToolTip(0, lesson.description)
            group.addChild(item)
            if lesson.id == selected:
                self.tree.setCurrentItem(item)
        self.tree.expandAll()
        self.tree.blockSignals(False)

    # ---- state ------------------------------------------------------------

    def select_lesson(self, lesson_id: str) -> None:
        self._lesson_id = lesson_id
        lesson = self.manager.lessons[lesson_id]
        self._index = min(self.manager.current_index(lesson_id), len(lesson.checkpoints) - 1)
        self._hint_number = 0
        self.feedback.clear()
        self._update_detail()

    def _on_item_changed(self, item: QTreeWidgetItem | None, _prev) -> None:
        lesson_id = item.data(0, Qt.ItemDataRole.UserRole) if item else None
        if lesson_id:
            self.select_lesson(lesson_id)

    def _step(self, delta: int) -> None:
        if self._lesson_id is None:
            return
        unlocked = self.manager.current_index(self._lesson_id)
        last = len(self.manager.lessons[self._lesson_id].checkpoints) - 1
        self._index = max(0, min(self._index + delta, unlocked, last))
        self._hint_number = 0
        self.feedback.clear()
        self._update_detail()

    def _update_detail(self) -> None:
        has_lesson = self._lesson_id is not None
        for btn in (
            self.load_btn,
            self.hint_btn,
            self.verify_btn,
            self.solution_btn,
            self.prev_btn,
            self.next_btn,
            self.reset_btn,
        ):
            btn.setEnabled(has_lesson)
        if not has_lesson:
            return
        lesson = self.manager.lessons[self._lesson_id]
        cp = lesson.checkpoints[self._index]
        unlocked = self.manager.current_index(lesson.id)
        self.title_label.setText(f"{lesson.title} — {lesson.language.friendly_name()}")
        state = "done" if self._index < unlocked else "current"
        self.step_label.setText(
            f"Checkpoint {self._index + 1} of {len(lesson.checkpoints)}: {cp.title} ({state})"
        )
        self.description.setPlainText(cp.description)
        self.prev_btn.setEnabled(self._index > 0)
        self.next_btn.setEnabled(self._index < min(unlocked, len(lesson.checkpoints) - 1))

    # ---- actions ----------------------------------------------------------

    def _checkpoint(self):
        return self.manager.lessons[self._lesson_id].checkpoints[self._index]

    def load_starter(self) -> None:
        if self._lesson_id is None:
            return
        self.manager.start(self._lesson_id)
        lesson = self.manager.lessons[self._lesson_id]
        self._load_code(lesson.language, self._checkpoint().starter_code)
        self.feedback.setPlainText("Starter code loaded into the editor. Edit it, then press Verify.")
        self.refresh_tree()

    def show_hint(self) -> None:
        if self._lesson_id is None:
            return
        text = self.manager.hint(self._lesson_id, self._index, self._hint_number)
        self._hint_number += 1
        self.feedback.setPlainText(f"💡 {text}")

    def show_solution(self) -> None:
        if self._lesson_id is None:
            return
        lesson = self.manager.lessons[self._lesson_id]
        self._load_code(lesson.language, self._checkpoint().solution)
        self.feedback.setPlainText(
            "Solution loaded into the editor. Read it, run it (F9), then press Verify."
        )

    def reset_lesson(self) -> None:
        if self._lesson_id is None:
            return
        self.manager.reset(self._lesson_id)
        self.select_lesson(self._lesson_id)
        self.refresh_tree()
        self.feedback.setPlainText("Lesson progress cleared.")

    def verify(self) -> None:
        if self._lesson_id is None or self._worker is not None:
            return
        self.verify_btn.setEnabled(False)
        self.feedback.setPlainText("Running your program…")
        self._worker = _VerifyThread(
            self.manager, self._lesson_id, self._index, self._get_code()
        )
        self._worker.finished_with.connect(self._on_verified)
        self._worker.start()

    def wait_for_verification(self, timeout_ms: int = 10000) -> None:
        """Block until a pending verification finishes (used by tests)."""
        if self._worker is not None:
            self._worker.wait(timeout_ms)

    def _on_verified(self, result: VerifyResult) -> None:
        if self._worker is not None:
            self._worker.wait()
            self._worker.deleteLater()
            self._worker = None
        self.feedback.setPlainText(result.message)
        if result.passed and self._lesson_id is not None:
            last = len(self.manager.lessons[self._lesson_id].checkpoints) - 1
            if self._index < last:
                self._index += 1
                self._hint_number = 0
                self.feedback.appendPlainText(
                    f"\nNext: {self._checkpoint().title}. Press Load Starter to continue."
                )
            self.refresh_tree()
        self._update_detail()
        self.verify_btn.setEnabled(True)

    def snapshot(self) -> dict:
        """Describe the active lesson for session reports."""
        if self._lesson_id is None:
            return {"title": "Time Warp Session"}
        lesson = self.manager.lessons[self._lesson_id]
        return {
            "lesson_title": lesson.title,
            "lesson_description": lesson.description,
            "language": lesson.language.friendly_name(),
            "status": self.manager.status(lesson.id).value,
            "checkpoint_index": self.manager.current_index(lesson.id),
            "checkpoint_total": len(lesson.checkpoints),
            "checkpoint_title": self._checkpoint().title,
        }
