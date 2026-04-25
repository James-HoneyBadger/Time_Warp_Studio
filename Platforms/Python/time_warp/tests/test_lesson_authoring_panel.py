import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")
QApplication = QtWidgets.QApplication

from time_warp.features.lesson_system import LessonManager
from time_warp.ui.feature_panels import LessonAuthoringPanel


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def test_lesson_manager_round_trips_custom_lesson_payload():
    manager = LessonManager()
    payload = {
        "id": "custom_python_loops",
        "title": "Python Loops Lab",
        "description": "Practice loop basics.",
        "language": "python",
        "difficulty": "beginner",
        "checkpoints": [
            {
                "title": "Count to three",
                "description": "Print 1, 2, 3",
                "starter_code": "for i in range(1, 4):\n    print(i)",
                "expected_output": "1\n2\n3",
                "hints": ["Use range(1, 4)"],
                "solution": "for i in range(1, 4):\n    print(i)",
            }
        ],
    }

    lesson = manager.import_lesson_data(payload)
    exported = manager.export_lesson_data(lesson.id)

    assert lesson.id == payload["id"]
    assert exported["title"] == payload["title"]
    assert exported["checkpoints"][0]["expected_output"] == "1\n2\n3"


def test_lesson_authoring_panel_creates_custom_lesson(qapp):
    panel = LessonAuthoringPanel()
    seen = []
    panel.lesson_created.connect(lambda payload: seen.append(payload))

    panel.lesson_title_input.setText("Build a Spiral")
    panel.lesson_description_input.setText("Create a simple turtle spiral.")
    panel.language_combo.setCurrentText("logo")
    panel.difficulty_combo.setCurrentText("intermediate")

    panel.checkpoint_title_input.setText("Start the spiral")
    panel.checkpoint_description_input.setText("Move and turn repeatedly.")
    panel.starter_code_input.setPlainText("REPEAT 12 [FD 20 RT 30]")
    panel.expected_output_input.setText("[spiral drawn]")
    panel.hints_input.setText("Use REPEAT|Turn 30 degrees")
    panel.solution_input.setPlainText("REPEAT 12 [FD 20 RT 30]")

    panel._add_checkpoint()
    panel._register_lesson()

    assert seen
    assert seen[0]["title"] == "Build a Spiral"
    assert seen[0]["language"] == "logo"
    assert len(seen[0]["checkpoints"]) == 1
    assert "Build a Spiral" in panel.preview_output.toPlainText()
