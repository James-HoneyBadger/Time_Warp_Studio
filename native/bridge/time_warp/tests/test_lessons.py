"""Tests for the lesson system and the curriculum it ships.

The curriculum is **data**, so no test exercises it by accident: a lesson whose
solution does not pass, or whose starter already does, would sit in the tree
looking correct forever. ``test_the_curriculum_is_sound`` is therefore the gate
that matters -- it runs all sixty checkpoint solutions and all sixty starters
through the real engine.

The rest of the file covers the verifier's rules and the progress store, which is
the part a learner notices: a hint that never runs out, or a checkpoint that
advances twice, is felt immediately.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from time_warp.core.interpreter import Language
from time_warp.features.lesson_system import (
    DIFFICULTIES,
    LESSONS_FILE,
    LessonError,
    LessonManager,
    LessonStatus,
    Checkpoint,
    load_curriculum,
    printed_lines,
    validate_curriculum,
    verify_checkpoint,
)


@pytest.fixture()
def manager(tmp_path: Path) -> LessonManager:
    """A manager whose progress is written to a temporary file, never the user's."""
    return LessonManager(progress_path=tmp_path / "progress.json")


# ---------------------------------------------------------------------------
# The curriculum: the gate
# ---------------------------------------------------------------------------


def test_the_curriculum_is_sound() -> None:
    """Every solution passes, and no starter already passes.

    This is the test the curriculum exists to make possible. It runs the engine,
    so it is also the only thing in the project that proves the sixty programs
    are programs at all.
    """
    problems = validate_curriculum(load_curriculum(), languages=list(Language))
    assert problems == [], "\n".join(problems)


def test_every_language_in_the_studio_has_a_lesson() -> None:
    lessons = load_curriculum()
    covered = {lesson.language for lesson in lessons}
    assert covered == set(Language)


def test_the_curriculum_covers_more_than_one_lesson_per_language() -> None:
    """Two lessons each: the shape the curriculum was commissioned in."""
    lessons = load_curriculum()
    counted = {language: 0 for language in Language}
    for lesson in lessons:
        counted[lesson.language] += 1
    assert min(counted.values()) >= 2


def test_every_lesson_teaches_three_checkpoints() -> None:
    for lesson in load_curriculum():
        assert lesson.checkpoint_count >= 3, lesson.id


def test_lesson_ids_are_unique_and_typed() -> None:
    lessons = load_curriculum()
    ids = [lesson.id for lesson in lessons]
    assert len(ids) == len(set(ids))
    assert all(lesson_id.islower() or "_" in lesson_id for lesson_id in ids)


def test_every_lesson_has_objectives_and_teacher_notes() -> None:
    """The teacher's half of a lesson is authored, not generated.

    A lesson plan with no objectives is a lesson nobody can place in a scheme of
    work, which is the difference between a curriculum and a pile of exercises.
    """
    for lesson in load_curriculum():
        assert lesson.objectives, lesson.id
        assert lesson.teacher_notes, lesson.id
        assert lesson.difficulty in DIFFICULTIES, lesson.id
        assert lesson.description, lesson.id


def test_prerequisites_name_lessons_that_exist() -> None:
    lessons = load_curriculum()
    known = {lesson.id for lesson in lessons}
    for lesson in lessons:
        for prerequisite in lesson.prerequisites:
            assert prerequisite in known, f"{lesson.id} needs {prerequisite}"
            assert prerequisite != lesson.id, lesson.id


def test_a_lesson_never_requires_a_later_one() -> None:
    """Prerequisites must be taught first, or the lesson cannot be started."""
    lessons = load_curriculum()
    order = {lesson.id: index for index, lesson in enumerate(lessons)}
    for lesson in lessons:
        for prerequisite in lesson.prerequisites:
            assert order[prerequisite] < order[lesson.id], lesson.id


def test_the_shipped_curriculum_file_is_where_the_loader_looks() -> None:
    assert LESSONS_FILE.name == "lessons.json"
    assert LESSONS_FILE.exists()
    data = json.loads(LESSONS_FILE.read_text(encoding="utf-8"))
    assert data["version"] >= 1
    assert len(data["lessons"]) == len(load_curriculum())


# ---------------------------------------------------------------------------
# Reading a program's output
# ---------------------------------------------------------------------------


def test_printed_lines_ignores_the_engines_commentary() -> None:
    """A language that announces a definition must not fail a lesson about maths."""
    output = ["✅ APL: DOUBLE defined\n", "42\n"]
    assert printed_lines(output) == ["42"]


def test_printed_lines_survives_entries_without_a_newline() -> None:
    """Entries disagree about the trailing newline, and two lines must not merge."""
    assert printed_lines(["one", "two"]) == ["one", "two"]
    assert printed_lines(["one\ntwo\n"]) == ["one", "two"]


def test_printed_lines_keeps_an_interior_blank_line() -> None:
    """A program may print a blank line on purpose; only the tail is dropped."""
    assert printed_lines(["a\n", "\n", "b\n"]) == ["a", "", "b"]


def test_printed_lines_of_nothing_is_nothing() -> None:
    assert printed_lines([]) == []
    assert printed_lines(["\n"]) == []


# ---------------------------------------------------------------------------
# Verifying a checkpoint
# ---------------------------------------------------------------------------


def basic_checkpoint(**overrides) -> Checkpoint:
    fields = {
        "title": "Say hello",
        "description": "Print Hello, World!",
        "starter_code": "10 END",
        "solution": '10 PRINT "Hello, World!"\n20 END',
        "hints": ("Use PRINT.",),
        "expect_lines": ("Hello, World!",),
    }
    fields.update(overrides)
    return Checkpoint(**fields)


def test_a_correct_program_passes() -> None:
    result = verify_checkpoint(basic_checkpoint(), '10 PRINT "Hello, World!"', Language.BASIC)
    assert result.passed
    assert "passed" in result.message


def test_a_wrong_answer_lists_both_expectations() -> None:
    result = verify_checkpoint(basic_checkpoint(), "10 PRINT 99", Language.BASIC)
    assert not result.passed
    # Both sides are shown: a learner needs to see what they printed next to what
    # was asked for, not one of the two.
    assert "Hello, World!" in result.message
    assert "99" in result.message


def test_a_broken_program_is_reported_as_an_error_not_as_a_wrong_answer() -> None:
    result = verify_checkpoint(basic_checkpoint(), "10 FLARGLE", Language.BASIC)
    assert not result.passed
    assert "reported an error" in result.message


def test_nothing_to_check_is_not_a_pass() -> None:
    """An empty editor must not read as success."""
    assert not verify_checkpoint(basic_checkpoint(), "   \n  ", Language.BASIC).passed


def test_extra_output_fails_a_line_checkpoint() -> None:
    """The lines are compared as a list, so printing more is not 'close enough'."""
    result = verify_checkpoint(basic_checkpoint(), '10 PRINT "Hello, World!"\n20 PRINT "more"', Language.BASIC)
    assert not result.passed


def test_an_expect_error_checkpoint_wants_the_failure() -> None:
    checkpoint = basic_checkpoint(
        expect_lines=None, expect_error="Unknown BASIC command"
    )
    assert verify_checkpoint(checkpoint, "10 FLARGLE", Language.BASIC).passed
    assert not verify_checkpoint(checkpoint, "10 PRINT 1", Language.BASIC).passed


def test_a_program_that_waits_for_input_fails_rather_than_hanging() -> None:
    """A checkpoint cannot be checked by a person who is not there."""
    checkpoint = basic_checkpoint(expect_lines=("Name?",))
    result = verify_checkpoint(checkpoint, '10 INPUT "Name? ", N$', Language.BASIC)
    assert not result.passed


# ---------------------------------------------------------------------------
# The turtle expectations
# ---------------------------------------------------------------------------


def logo_checkpoint(**overrides) -> Checkpoint:
    fields = {
        "title": "A square",
        "description": "Draw a square.",
        "starter_code": "REPEAT 4 [ FORWARD 100 ]",
        "solution": "REPEAT 4 [ FORWARD 100 RIGHT 90 ]",
        "hints": ("Turn after each side.",),
        "expect_turtle": {"segments": 4, "returns_home": True},
    }
    fields.update(overrides)
    return Checkpoint(**fields)


def test_a_drawing_checkpoint_counts_pen_segments() -> None:
    assert verify_checkpoint(logo_checkpoint(), "REPEAT 4 [ FORWARD 100 RIGHT 90 ]", Language.LOGO).passed


def test_too_few_segments_says_how_many_were_drawn() -> None:
    result = verify_checkpoint(logo_checkpoint(), "REPEAT 3 [ FORWARD 100 RIGHT 120 ]", Language.LOGO)
    assert not result.passed
    assert "drew 3" in result.message


def test_an_open_shape_fails_a_returns_home_checkpoint() -> None:
    """Three sides of a square are four segments' worth of nothing closed."""
    result = verify_checkpoint(logo_checkpoint(), "REPEAT 3 [ FORWARD 100 RIGHT 90 ]", Language.LOGO)
    assert not result.passed


def test_a_pixel_machine_is_checked_by_its_shapes() -> None:
    """CHIP-8 paints rectangles, so counting pen segments would always find zero."""
    checkpoint = Checkpoint(
        title="Light one pixel",
        description="Draw the data.",
        starter_code="start: LD I, dot\n       HALT\ndot:   DB 0x80",
        solution="start: LD I, dot\n       DRW V0, V1, 1\n       HALT\ndot:   DB 0x80",
        hints=("Add DRW.",),
        expect_turtle={"shapes": 2},
    )
    assert verify_checkpoint(checkpoint, checkpoint.solution, Language.CHIP8).passed
    result = verify_checkpoint(checkpoint, checkpoint.starter_code, Language.CHIP8)
    assert not result.passed
    assert "painted 1" in result.message


def test_min_segments_accepts_a_longer_drawing() -> None:
    checkpoint = logo_checkpoint(expect_turtle={"min_segments": 3})
    assert verify_checkpoint(checkpoint, "REPEAT 4 [ FORWARD 100 RIGHT 90 ]", Language.LOGO).passed


# ---------------------------------------------------------------------------
# Progress
# ---------------------------------------------------------------------------


def test_a_lesson_starts_not_started(manager: LessonManager) -> None:
    lesson_id = manager.list_lessons()[0].id
    assert manager.status(lesson_id) is LessonStatus.NOT_STARTED
    assert manager.current_index(lesson_id) == 0


def test_passing_a_checkpoint_advances_the_learner(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    result = manager.verify(lesson.id, 0, lesson.checkpoints[0].solution)
    assert result.passed
    assert manager.current_index(lesson.id) == 1
    assert manager.status(lesson.id) is LessonStatus.IN_PROGRESS


def test_finishing_the_last_checkpoint_completes_the_lesson(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    for index, checkpoint in enumerate(lesson.checkpoints):
        result = manager.verify(lesson.id, index, checkpoint.solution)
        assert result.passed
    assert manager.status(lesson.id) is LessonStatus.COMPLETED
    assert "complete" in result.message.lower()


def test_a_failed_checkpoint_does_not_advance_anything(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    assert not manager.verify(lesson.id, 0, "10 END").passed
    assert manager.current_index(lesson.id) == 0
    assert manager.status(lesson.id) is LessonStatus.NOT_STARTED


def test_re_passing_an_earlier_checkpoint_does_not_go_backwards(manager: LessonManager) -> None:
    """A learner who revisits an old checkpoint keeps their place."""
    lesson = manager.list_lessons()[0]
    manager.verify(lesson.id, 0, lesson.checkpoints[0].solution)
    manager.verify(lesson.id, 1, lesson.checkpoints[1].solution)
    manager.verify(lesson.id, 0, lesson.checkpoints[0].solution)
    assert manager.current_index(lesson.id) == 2


def test_progress_survives_a_new_manager(tmp_path: Path) -> None:
    first = LessonManager(progress_path=tmp_path / "progress.json")
    lesson = first.list_lessons()[0]
    first.verify(lesson.id, 0, lesson.checkpoints[0].solution)

    second = LessonManager(progress_path=tmp_path / "progress.json")
    assert second.current_index(lesson.id) == 1


def test_reset_forgets_the_lesson(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    manager.verify(lesson.id, 0, lesson.checkpoints[0].solution)
    manager.reset(lesson.id)
    assert manager.status(lesson.id) is LessonStatus.NOT_STARTED
    assert manager.current_index(lesson.id) == 0


def test_an_unreadable_progress_file_does_not_stop_the_lessons(tmp_path: Path) -> None:
    """Losing progress is bad; refusing to open the curriculum is worse."""
    path = tmp_path / "progress.json"
    path.write_text("{ this is not json", encoding="utf-8")
    manager = LessonManager(progress_path=path)
    assert manager.status(manager.list_lessons()[0].id) is LessonStatus.NOT_STARTED


def test_progress_for_an_unknown_lesson_is_discarded(tmp_path: Path) -> None:
    """A lesson removed from the curriculum must not linger in the file."""
    path = tmp_path / "progress.json"
    path.write_text(json.dumps({"lessons": {"gone": {"checkpoint": 2}}}), encoding="utf-8")
    manager = LessonManager(progress_path=path)
    manager.save_progress()
    assert "gone" not in json.loads(path.read_text(encoding="utf-8"))["lessons"]


# ---------------------------------------------------------------------------
# Hints
# ---------------------------------------------------------------------------


def test_hints_come_out_one_at_a_time_and_are_counted(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    first = manager.hint(lesson.id, 0, 0)
    second = manager.hint(lesson.id, 0, 1)
    assert first == lesson.checkpoints[0].hints[0]
    assert second == lesson.checkpoints[0].hints[1]
    assert manager.hints_used(lesson.id) == 2


def test_asking_past_the_last_hint_gives_the_last_hint(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    checkpoint = lesson.checkpoints[0]
    assert manager.hint(lesson.id, 0, 99) == checkpoint.hints[-1]


def test_hint_calls_are_clamped_at_the_start_too(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    assert manager.hint(lesson.id, 0, -5) == lesson.checkpoints[0].hints[0]


# ---------------------------------------------------------------------------
# Refusing things that are wrong
# ---------------------------------------------------------------------------


def test_an_unknown_lesson_is_refused(manager: LessonManager) -> None:
    with pytest.raises(LessonError):
        manager.lesson("no_such_lesson")


def test_an_out_of_range_checkpoint_is_refused(manager: LessonManager) -> None:
    lesson = manager.list_lessons()[0]
    with pytest.raises(LessonError):
        manager.verify(lesson.id, 99, "10 END")


def test_a_lesson_can_be_named_by_a_prefix(manager: LessonManager) -> None:
    assert manager.find("basic_first").id == "basic_first_steps"
    assert manager.find("basic_first_steps").id == "basic_first_steps"


def test_an_ambiguous_prefix_is_refused_rather_than_guessed(manager: LessonManager) -> None:
    """Choosing one of two lessons for somebody is how they end up in the wrong one."""
    with pytest.raises(LessonError, match="matches"):
        manager.find("basic")


def test_an_unknown_prefix_is_refused(manager: LessonManager) -> None:
    with pytest.raises(LessonError):
        manager.find("nonesuch")


# ---------------------------------------------------------------------------
# What the validator refuses
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "expectations",
    [
        {},  # nothing to check against
        {"expect_lines": ["x"], "expect_turtle": {"segments": 1}},  # one is ignored
        {"expect_lines": ["x"], "expect_error": "boom"},
    ],
)
def test_the_validator_wants_exactly_one_expectation(expectations: dict) -> None:
    """None or several means a checkpoint that cannot be checked properly.

    A checkpoint with no expectation passes every learner; one with two has its
    second expectation silently ignored.
    """
    raw = {
        "title": "t",
        "description": "d",
        "starter_code": "a",
        "solution": "b",
        **expectations,
    }
    with pytest.raises(LessonError, match="exactly one expectation"):
        _checkpoint_from_raw(raw)


def _checkpoint_from_raw(raw: dict) -> Checkpoint:
    from time_warp.features.lesson_system import _checkpoint_from

    return _checkpoint_from(raw, "test_lesson")


def test_the_validator_refuses_a_starter_that_already_passes() -> None:
    from time_warp.features.lesson_system import Lesson

    lesson = Lesson(
        id="l",
        title="L",
        language=Language.BASIC,
        difficulty="beginner",
        description="d",
        checkpoints=(
            basic_checkpoint(starter_code='10 PRINT "Hello, World!"\n20 END'),
        ),
    )
    problems = validate_curriculum([lesson])
    assert any("already passes" in problem for problem in problems)


def test_the_validator_refuses_an_unflagged_starter_that_errors() -> None:
    from time_warp.features.lesson_system import Lesson

    lesson = Lesson(
        id="l",
        title="L",
        language=Language.BASIC,
        difficulty="beginner",
        description="d",
        checkpoints=(basic_checkpoint(starter_code="10 FLARGLE"),),
    )
    problems = validate_curriculum([lesson])
    assert any("reports an error" in problem for problem in problems)


def test_the_validator_refuses_a_stale_starter_is_broken_flag() -> None:
    """A flag that has stopped being true is worse than no flag: it excuses a
    starter that has quietly become harmless."""
    from time_warp.features.lesson_system import Lesson

    lesson = Lesson(
        id="l",
        title="L",
        language=Language.BASIC,
        difficulty="beginner",
        description="d",
        checkpoints=(basic_checkpoint(starter_code="10 END", starter_is_broken=True),),
    )
    problems = validate_curriculum([lesson])
    assert any("does not report an error" in problem for problem in problems)


def test_the_validator_refuses_a_language_with_no_lesson() -> None:
    from time_warp.features.lesson_system import Lesson

    lesson = Lesson(
        id="l",
        title="L",
        language=Language.BASIC,
        difficulty="beginner",
        description="d",
        checkpoints=(basic_checkpoint(),),
    )
    problems = validate_curriculum([lesson], languages=list(Language))
    assert any("no lesson covers" in problem for problem in problems)


def test_the_validator_can_skip_running_the_programs() -> None:
    """The fast pass exists for a caller that only wants the shape, and must still
    report what it can see without the engine."""
    from time_warp.features.lesson_system import Lesson

    lesson = Lesson(
        id="l",
        title="L",
        language=Language.BASIC,
        difficulty="beginner",
        description="d",
        checkpoints=(basic_checkpoint(hints=()),),
    )
    problems = validate_curriculum([lesson], check_solutions=False)
    assert any("no hints" in problem for problem in problems)
