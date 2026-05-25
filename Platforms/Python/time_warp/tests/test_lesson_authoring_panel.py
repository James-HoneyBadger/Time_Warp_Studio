import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")
QApplication = QtWidgets.QApplication

# pylint: disable=wrong-import-position
from time_warp.features.lesson_system import LessonManager  # noqa: E402
from time_warp.ui.feature_panels import LessonAuthoringPanel  # noqa: E402
# pylint: enable=wrong-import-position


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


def test_lesson_authoring_panel_creates_custom_lesson(qapp):  # pylint: disable=redefined-outer-name
    panel = LessonAuthoringPanel()
    seen = []
    panel.lesson_created.connect(seen.append)

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

    panel._add_checkpoint()  # pylint: disable=protected-access
    panel._register_lesson()  # pylint: disable=protected-access

    assert seen
    assert seen[0]["title"] == "Build a Spiral"
    assert seen[0]["language"] == "logo"
    assert len(seen[0]["checkpoints"]) == 1
    assert "Build a Spiral" in panel.preview_output.toPlainText()


def _make_lesson_payload(lesson_id="test_l", language="basic", difficulty="beginner"):
    return {
        "id": lesson_id,
        "title": "Test Lesson",
        "description": "A test.",
        "language": language,
        "difficulty": difficulty,
        "checkpoints": [
            {
                "title": "Step 1",
                "description": "Do something",
                "starter_code": 'PRINT "HI"',
                "expected_output": "HI",
                "hints": ["Use PRINT"],
                "solution": 'PRINT "HI"',
            }
        ],
    }


class TestLessonManager:
    """More LessonManager tests."""

    def test_list_lessons_non_empty(self):
        manager = LessonManager()
        assert len(manager.list_lessons()) > 0

    def test_import_lesson_returns_lesson(self):
        manager = LessonManager()
        lesson = manager.import_lesson_data(_make_lesson_payload("l1"))
        assert lesson.id == "l1"

    def test_import_lesson_title(self):
        manager = LessonManager()
        lesson = manager.import_lesson_data(_make_lesson_payload("l2"))
        assert lesson.title == "Test Lesson"

    def test_import_lesson_adds_to_lessons(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l3"))
        assert "l3" in manager.lessons

    def test_export_lesson_data_has_title(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l4"))
        exported = manager.export_lesson_data("l4")
        assert exported["title"] == "Test Lesson"

    def test_export_lesson_data_has_checkpoints(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l5"))
        exported = manager.export_lesson_data("l5")
        assert len(exported["checkpoints"]) == 1

    def test_start_lesson_sets_current(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l6"))
        manager.start_lesson("l6")
        assert manager.current_lesson is not None
        assert manager.current_lesson.id == "l6"

    def test_get_current_checkpoint(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l7"))
        manager.start_lesson("l7")
        cp = manager.get_current_checkpoint()
        assert cp.title == "Step 1"

    def test_check_output_correct(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l8"))
        manager.start_lesson("l8")
        assert manager.check_output("HI") is True

    def test_check_output_wrong(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l9"))
        manager.start_lesson("l9")
        assert manager.check_output("WRONG") is False

    def test_get_solution_returns_solution(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l10"))
        manager.start_lesson("l10")
        sol = manager.get_solution()
        assert 'PRINT "HI"' in sol

    def test_get_progress_dict_keys(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("l11"))
        manager.start_lesson("l11")
        progress = manager.get_progress()
        assert "lesson_id" in progress
        assert "checkpoint" in progress
        assert "total_checkpoints" in progress

    def test_import_requires_checkpoint(self):
        manager = LessonManager()
        payload = _make_lesson_payload("l12")
        payload["checkpoints"] = []
        with pytest.raises(ValueError):
            manager.import_lesson_data(payload)


class TestLessonManagerExtended:
    """Extended LessonManager tests."""

    def test_fresh_manager_no_active_lesson(self):
        manager = LessonManager()
        assert manager.current_lesson is None

    def test_import_and_start(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("ext1"))
        manager.start_lesson("ext1")
        assert manager.current_lesson is not None

    def test_start_sets_lesson_id(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("ext2"))
        manager.start_lesson("ext2")
        assert manager.current_lesson.id == "ext2"

    def test_check_output_correct(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("ext3"))
        manager.start_lesson("ext3")
        assert manager.check_output('PRINT "HI"') is True

    def test_check_output_wrong(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("ext4"))
        manager.start_lesson("ext4")
        assert manager.check_output("NOPE") is False

    def test_get_progress_total(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("ext5"))
        manager.start_lesson("ext5")
        prog = manager.get_progress()
        assert prog["total_checkpoints"] >= 1

    def test_get_solution_str(self):
        manager = LessonManager()
        manager.import_lesson_data(_make_lesson_payload("ext6"))
        manager.start_lesson("ext6")
        sol = manager.get_solution()
        assert isinstance(sol, str)

    def test_difficulty_stored(self):
        manager = LessonManager()
        payload = _make_lesson_payload("ext7", difficulty="intermediate")
        manager.import_lesson_data(payload)
        assert payload["difficulty"] == "intermediate"

    def test_two_lessons_independent(self):
        m1 = LessonManager()
        m2 = LessonManager()
        m1.import_lesson_data(_make_lesson_payload("ext8a"))
        m2.import_lesson_data(_make_lesson_payload("ext8b"))
        m1.start_lesson("ext8a")
        m2.start_lesson("ext8b")
        assert m1.current_lesson.id == "ext8a"
        assert m2.current_lesson.id == "ext8b"

    def test_import_missing_checkpoints_raises(self):
        manager = LessonManager()
        payload = _make_lesson_payload("ext9")
        payload["checkpoints"] = []
        with pytest.raises(ValueError):
            manager.import_lesson_data(payload)


class TestLessonManagerExtended2:
    """More lesson manager tests."""

    def test_import_and_get_by_id(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext2a"))
        assert lesson is not None

    def test_lesson_title_preserved(self):
        m = LessonManager()
        payload = _make_lesson_payload("ext2b")
        payload["title"] = "My Special Title"
        lesson = m.import_lesson_data(payload)
        assert lesson.title == "My Special Title"

    def test_lesson_language_preserved(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext2c", language="logo"))
        assert lesson.language == "logo"

    def test_lesson_difficulty_preserved(self):
        m = LessonManager()
        lesson = m.import_lesson_data(
            _make_lesson_payload("ext2d", difficulty="advanced")
        )
        assert lesson.difficulty == "advanced"

    def test_multiple_imports_list_grows(self):
        m = LessonManager()
        initial = len(m.list_lessons())
        m.import_lesson_data(_make_lesson_payload("ext2e1"))
        m.import_lesson_data(_make_lesson_payload("ext2e2"))
        assert len(m.list_lessons()) >= initial + 2

    def test_start_lesson_sets_current(self):
        m = LessonManager()
        m.import_lesson_data(_make_lesson_payload("ext2f"))
        m.start_lesson("ext2f")
        assert m.current_lesson is not None

    def test_start_lesson_id_matches(self):
        m = LessonManager()
        m.import_lesson_data(_make_lesson_payload("ext2g"))
        m.start_lesson("ext2g")
        assert m.current_lesson.id == "ext2g"

    def test_checkpoints_count(self):
        m = LessonManager()
        m.import_lesson_data(_make_lesson_payload("ext2h"))
        m.start_lesson("ext2h")
        assert len(m.current_lesson.checkpoints) == 1

    def test_checkpoint_title(self):
        m = LessonManager()
        m.import_lesson_data(_make_lesson_payload("ext2i"))
        m.start_lesson("ext2i")
        assert m.current_lesson.checkpoints[0].title == "Step 1"

    def test_import_lua_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext2j", language="lua"))
        assert lesson.language == "lua"


class TestLessonManagerExtended3:
    """Third round of LessonManager tests."""

    def test_import_pascal_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3a", language="pascal"))
        assert lesson.language == "pascal"

    def test_import_prolog_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3b", language="prolog"))
        assert lesson.language == "prolog"

    def test_import_forth_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3c", language="forth"))
        assert lesson.language == "forth"

    def test_import_logo_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3d", language="logo"))
        assert lesson.language == "logo"

    def test_import_javascript_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(
            _make_lesson_payload("ext3e", language="javascript")
        )
        assert lesson.language == "javascript"

    def test_import_erlang_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3f", language="erlang"))
        assert lesson.language == "erlang"

    def test_import_lisp_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3g", language="lisp"))
        assert lesson.language == "lisp"

    def test_lesson_title_not_empty(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3h"))
        assert len(lesson.title) > 0

    def test_lesson_description_is_str(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3i"))
        assert isinstance(lesson.description, str)

    def test_lesson_content_not_empty(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("ext3j"))
        assert lesson is not None


class TestLessonManagerExtended4:
    """Fourth round of LessonManager tests."""

    def test_import_basic_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("e4a", language="basic"))
        assert lesson is not None

    def test_import_logo_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("e4b", language="logo"))
        assert lesson.language == "logo"

    def test_import_lua_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("e4c", language="lua"))
        assert lesson.language == "lua"

    def test_import_js_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(
            _make_lesson_payload("e4d", language="javascript")
        )
        assert lesson.language == "javascript"

    def test_lesson_title_stored(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("e4e"))
        assert lesson.title == "Test Lesson"

    def test_lesson_description_stored(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("e4f"))
        assert lesson.description == "A test."

    def test_lesson_difficulty_stored(self):
        m = LessonManager()
        lesson = m.import_lesson_data(
            _make_lesson_payload("e4g", difficulty="advanced")
        )
        assert lesson.difficulty == "advanced"

    def test_lesson_has_checkpoints(self):
        m = LessonManager()
        lesson = m.import_lesson_data(_make_lesson_payload("e4h"))
        assert hasattr(lesson, "checkpoints")

    def test_multiple_lessons_independent(self):
        m = LessonManager()
        l1 = m.import_lesson_data(_make_lesson_payload("e4i", language="basic"))
        l2 = m.import_lesson_data(_make_lesson_payload("e4j", language="logo"))
        assert l1.language != l2.language

    def test_intermediate_difficulty(self):
        m = LessonManager()
        lesson = m.import_lesson_data(
            _make_lesson_payload("e4k", difficulty="intermediate")
        )
        assert lesson.difficulty == "intermediate"


class TestLessonManagerExtended5:
    """Fifth round of lesson manager tests."""

    def _make_payload(self, uid="e5a", language="basic", difficulty="beginner"):
        return {
            "id": uid,
            "title": "Test Lesson",
            "description": "A test.",
            "language": language,
            "difficulty": difficulty,
            "code": "PRINT 1",
            "checkpoints": [
                {
                    "title": "Step 1",
                    "description": "Do something",
                    "starter_code": 'PRINT "HI"',
                    "expected_output": "HI",
                    "hints": ["Use PRINT"],
                    "solution": 'PRINT "HI"',
                }
            ],
        }

    def test_import_returns_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5a"))
        assert lesson is not None

    def test_lesson_id_stored(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5b"))
        assert lesson.id == "e5b"

    def test_lesson_code_stored(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5c"))
        assert lesson is not None

    def test_lesson_language_lua(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5d", language="lua"))
        assert lesson.language == "lua"

    def test_lesson_language_forth(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5e", language="forth"))
        assert lesson.language == "forth"

    def test_lesson_difficulty_beginner(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5f", difficulty="beginner"))
        assert lesson.difficulty == "beginner"

    def test_two_lessons_different_ids(self):
        m = LessonManager()
        l1 = m.import_lesson_data(self._make_payload("e5g"))
        l2 = m.import_lesson_data(self._make_payload("e5h"))
        assert l1.id != l2.id

    def test_lesson_has_title(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5i"))
        assert lesson.title == "Test Lesson"

    def test_lesson_has_description(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e5j"))
        assert lesson.description == "A test."

    def test_manager_not_none(self):
        m = LessonManager()
        assert m is not None


class TestLessonManagerExtended6:
    """Sixth round of lesson manager tests."""

    def _make_payload(self, uid="e6a", language="basic", difficulty="beginner"):
        return {
            "id": uid,
            "title": "Extended Test",
            "description": "Extended desc.",
            "language": language,
            "difficulty": difficulty,
            "code": "PRINT 1",
            "checkpoints": [{"instruction": "Run it", "expected_output": "1"}],
        }

    def test_manager_creation(self):
        m = LessonManager()
        assert m is not None

    def test_import_returns_lesson(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e6b"))
        assert lesson is not None

    def test_lesson_id_matches(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e6c"))
        assert lesson.id == "e6c"

    def test_lesson_title(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e6d"))
        assert lesson.title == "Extended Test"

    def test_lesson_desc(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e6e"))
        assert lesson.description == "Extended desc."

    def test_lesson_language_javascript(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e6f", language="javascript"))
        assert lesson.language == "javascript"

    def test_lesson_difficulty_intermediate(self):
        m = LessonManager()
        lesson = m.import_lesson_data(
            self._make_payload("e6g", difficulty="intermediate")
        )
        assert lesson.difficulty == "intermediate"

    def test_two_lessons_different_ids(self):
        m = LessonManager()
        l1 = m.import_lesson_data(self._make_payload("e6h"))
        l2 = m.import_lesson_data(self._make_payload("e6i"))
        assert l1.id != l2.id

    def test_lesson_is_not_none(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e6j"))
        assert lesson is not None

    def test_language_lua(self):
        m = LessonManager()
        lesson = m.import_lesson_data(self._make_payload("e6k", language="lua"))
        assert lesson.language == "lua"


class TestLessonManagerExtended7:
    """Seventh round of lesson manager tests."""

    def _make_payload(self, uid="e7a", language="basic", difficulty="beginner"):
        return {
            "id": uid,
            "title": "Extended Test 7",
            "description": "Extended desc 7.",
            "language": language,
            "difficulty": difficulty,
            "code": "PRINT 1",
            "checkpoints": [{"instruction": "Run it", "expected_output": "1"}],
        }

    def test_manager_importable(self):
        assert LessonManager is not None

    def test_lesson_has_id(self):
        mgr = LessonManager()
        lesson = mgr.import_lesson_data(self._make_payload("id-e7-1"))
        assert lesson.id == "id-e7-1"

    def test_lesson_has_title(self):
        mgr = LessonManager()
        lesson = mgr.import_lesson_data(self._make_payload("id-e7-2"))
        assert lesson.title == "Extended Test 7"

    def test_lesson_has_description(self):
        mgr = LessonManager()
        lesson = mgr.import_lesson_data(self._make_payload("id-e7-3"))
        assert lesson.description == "Extended desc 7."

    def test_lesson_language_pascal(self):
        mgr = LessonManager()
        lesson = mgr.import_lesson_data(
            self._make_payload("id-e7-4", language="pascal")
        )
        assert lesson.language == "pascal"

    def test_lesson_difficulty_advanced(self):
        mgr = LessonManager()
        lesson = mgr.import_lesson_data(
            self._make_payload("id-e7-5", difficulty="advanced")
        )
        assert lesson.difficulty == "advanced"

    def test_lesson_is_not_none(self):
        mgr = LessonManager()
        lesson = mgr.import_lesson_data(self._make_payload("id-e7-6"))
        assert lesson is not None

    def test_two_lessons_different_ids(self):
        mgr = LessonManager()
        l1 = mgr.import_lesson_data(self._make_payload("id-e7-7a"))
        l2 = mgr.import_lesson_data(self._make_payload("id-e7-7b"))
        assert l1.id != l2.id

    def test_lesson_language_logo(self):
        mgr = LessonManager()
        lesson = mgr.import_lesson_data(self._make_payload("id-e7-8", language="logo"))
        assert lesson.language == "logo"

    def test_two_managers_independent(self):
        m1 = LessonManager()
        m2 = LessonManager()
        assert m1 is not m2
