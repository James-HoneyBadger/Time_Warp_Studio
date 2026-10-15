"""Lessons curriculum, verification, progress persistence, and the Lessons dock."""

from __future__ import annotations

import json
import time

import pytest

from time_warp.core.interpreter import Language
from time_warp.features.lesson_system import (
    LESSONS_FILE,
    LessonManager,
    LessonStatus,
    verify_checkpoint,
)

_CURRICULUM = json.loads(LESSONS_FILE.read_text(encoding="utf-8"))["lessons"]


@pytest.fixture
def manager(tmp_path):
    return LessonManager(progress_path=tmp_path / "progress.json")


def test_every_language_has_at_least_two_lessons(manager):
    counts: dict[Language, int] = {}
    for lesson in manager.list_lessons():
        counts[lesson.language] = counts.get(lesson.language, 0) + 1
    assert set(counts) == set(Language)
    assert all(n >= 2 for n in counts.values()), counts


@pytest.mark.parametrize(
    "lesson_id,index",
    [
        (lesson["id"], i)
        for lesson in _CURRICULUM
        for i in range(len(lesson["checkpoints"]))
    ],
)
def test_solution_passes_and_starter_fails(manager, lesson_id, index):
    lesson = manager.lessons[lesson_id]
    cp = lesson.checkpoints[index]
    solved = verify_checkpoint(cp, cp.solution, lesson.language)
    assert solved.passed, solved.message
    started = verify_checkpoint(cp, cp.starter_code, lesson.language)
    assert not started.passed


def test_progress_round_trip(manager, tmp_path):
    lesson = manager.lessons["basic_first_steps"]
    assert manager.status(lesson.id) is LessonStatus.NOT_STARTED

    wrong = manager.verify(lesson.id, 0, '10 PRINT "Hello"\n')
    assert not wrong.passed
    assert "Expected output" in wrong.message
    assert manager.current_index(lesson.id) == 0

    assert manager.verify(lesson.id, 0, lesson.checkpoints[0].solution).passed
    assert manager.status(lesson.id) is LessonStatus.IN_PROGRESS
    assert manager.hint(lesson.id, 1, 5) == lesson.checkpoints[1].hints[-1]
    final = manager.verify(lesson.id, 1, lesson.checkpoints[1].solution)
    assert final.passed and "Lesson complete" in final.message

    reloaded = LessonManager(progress_path=tmp_path / "progress.json")
    assert reloaded.status(lesson.id) is LessonStatus.COMPLETED
    assert reloaded.hints_used(lesson.id) == 1

    reloaded.reset(lesson.id)
    assert reloaded.status(lesson.id) is LessonStatus.NOT_STARTED


def test_corrupt_progress_file_is_ignored(tmp_path):
    path = tmp_path / "progress.json"
    path.write_text("{not json", encoding="utf-8")
    mgr = LessonManager(progress_path=path)
    assert mgr.status("logo_shapes") is LessonStatus.NOT_STARTED
    mgr.start("logo_shapes")
    assert json.loads(path.read_text(encoding="utf-8"))["lessons"]["logo_shapes"]


def test_turtle_checkpoint_explains_open_shape(manager):
    lesson = manager.lessons["logo_shapes"]
    result = verify_checkpoint(
        lesson.checkpoints[1], "REPEAT 3 [FORWARD 100 RIGHT 90]\n", lesson.language
    )
    assert not result.passed
    assert "not closed" in result.message


def test_infinite_loop_is_contained(manager):
    lesson = manager.lessons["python_basics"]
    t0 = time.monotonic()
    result = verify_checkpoint(lesson.checkpoints[0], "while True:\n    pass\n", lesson.language)
    assert not result.passed
    assert time.monotonic() - t0 < 6


def test_lesson_dock_end_to_end(qapp, tmp_path):
    from time_warp.ui.main_window import MainWindow

    win = MainWindow()
    win.show_lessons()
    panel = win._lesson_panel
    panel.manager = LessonManager(progress_path=tmp_path / "progress.json")
    panel.refresh_tree()

    panel.select_lesson("pascal_basics")
    panel.load_starter()
    ed = win.get_current_editor()
    assert ed.toPlainText().startswith("program Hello;")
    assert win._ts(win.editor_tabs.currentIndex()).language is Language.PASCAL

    panel.verify()
    panel.wait_for_verification()
    qapp.processEvents()
    assert "Not yet" in panel.feedback.toPlainText()

    panel.show_solution()
    panel.verify()
    panel.wait_for_verification()
    qapp.processEvents()
    assert "Checkpoint passed" in panel.feedback.toPlainText()
    assert panel._index == 1
    assert panel.snapshot()["checkpoint_index"] == 1
