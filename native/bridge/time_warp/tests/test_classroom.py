"""Tests for the teacher's side of the lessons.

Two properties are worth more than the rest of this file, and both are about
accidents a teacher would not forgive:

* a handout must contain the **starter code**, not the solution, and must never
  overwrite work a learner has already started;
* marking a student's file must not move the **teacher's own** place in the
  curriculum -- the teacher is also a learner in this application, and the two
  progress files are one line of code apart.
"""

from __future__ import annotations

import json
import zipfile
from pathlib import Path

import pytest

from time_warp.features.classroom_mode import (
    MAX_PRESENTATION_FONT,
    MIN_PRESENTATION_FONT,
    Assignment,
    ClassroomError,
    ClassroomMode,
    PresentationSettings,
    Student,
    StudentRoster,
    outline,
)
from time_warp.features.lesson_system import LessonError, LessonManager


@pytest.fixture()
def classroom(tmp_path: Path) -> ClassroomMode:
    """A classroom whose files all live in a temporary directory."""
    return ClassroomMode(root=tmp_path / "classroom")


# ---------------------------------------------------------------------------
# The roster
# ---------------------------------------------------------------------------


def test_a_student_gets_an_id_from_their_name() -> None:
    roster = StudentRoster()
    student = roster.add("Ada Lovelace")
    assert student.student_id == "ada-lovelace"
    assert student.name == "Ada Lovelace"


def test_a_student_needs_a_name() -> None:
    with pytest.raises(ClassroomError):
        StudentRoster().add("   ")


def test_two_students_cannot_share_an_id() -> None:
    roster = StudentRoster()
    roster.add("Ada")
    with pytest.raises(ClassroomError, match="already a student"):
        roster.add("ada")


def test_a_student_can_be_removed_and_found() -> None:
    roster = StudentRoster()
    roster.add("Ada")
    roster.add("Grace")
    assert len(roster.find("gra")) == 1
    assert roster.remove("ada") is True
    assert roster.remove("ada") is False
    assert len(roster) == 1


def test_an_unknown_student_is_refused() -> None:
    with pytest.raises(ClassroomError):
        StudentRoster().get("nobody")


def test_removing_a_student_leaves_their_work_on_disk(classroom: ClassroomMode) -> None:
    """A class list is administration; it must not delete an afternoon's work."""
    classroom.add_student("Ada")
    directory = classroom.student_directory("ada")
    (directory / "keep-me.txt").write_text("work", encoding="utf-8")

    classroom.remove_student("ada")
    assert (directory / "keep-me.txt").read_text(encoding="utf-8") == "work"


def test_a_students_name_cannot_point_outside_the_classroom(classroom: ClassroomMode) -> None:
    """A name is typed by a person, so a slash in it must not become a path."""
    student = classroom.add_student("../../etc/passwd")
    directory = student.directory(classroom.root)
    assert classroom.root.resolve() in directory.resolve().parents


def test_a_roster_survives_a_reload(tmp_path: Path) -> None:
    first = ClassroomMode(root=tmp_path / "classroom")
    first.add_student("Ada")
    second = ClassroomMode(root=tmp_path / "classroom")
    assert len(second.roster) == 1
    assert second.roster.get("ada").name == "Ada"


def test_an_unreadable_roster_does_not_stop_the_classroom(tmp_path: Path) -> None:
    root = tmp_path / "classroom"
    root.mkdir(parents=True)
    (root / "roster.json").write_text("{ not json", encoding="utf-8")
    assert len(ClassroomMode(root=root).roster) == 0


# ---------------------------------------------------------------------------
# Setting work
# ---------------------------------------------------------------------------


def test_assigning_records_the_lesson_and_the_note(classroom: ClassroomMode) -> None:
    assignment = classroom.assign(["basic_first_steps"], note="For Tuesday")
    assert assignment.lesson_ids == ["basic_first_steps"]
    assert assignment.created
    assert json.loads((classroom.root / "assignment.json").read_text())["note"] == "For Tuesday"


def test_assigning_an_unknown_lesson_is_refused(classroom: ClassroomMode) -> None:
    with pytest.raises(LessonError):
        classroom.assign(["no_such_lesson"])


def test_a_handout_is_named_for_the_language_and_the_checkpoint(
    classroom: ClassroomMode,
) -> None:
    """One file per checkpoint, named so a file list says which exercise it is."""
    assert classroom.handout_name(classroom.lesson("basic_first_steps"), 0) == "basic_first_steps-1.bas"
    assert classroom.handout_name(classroom.lesson("logo_shapes"), 2) == "logo_shapes-3.logo"
    assert classroom.handout_names(classroom.lesson("logo_shapes")) == [
        "logo_shapes-1.logo",
        "logo_shapes-2.logo",
        "logo_shapes-3.logo",
    ]


def test_distributing_hands_out_the_starter_code_not_the_solution(
    classroom: ClassroomMode,
) -> None:
    """Handing out the answer leaves nothing to teach."""
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    written = classroom.distribute()

    lesson = classroom.lesson("basic_first_steps")
    assert len(written) == lesson.checkpoint_count
    for index, path in enumerate(sorted(written)):
        assert path.read_text(encoding="utf-8") == lesson.checkpoints[index].starter_code
        assert lesson.checkpoints[index].solution not in path.read_text(encoding="utf-8")


def test_distributing_never_overwrites_started_work(classroom: ClassroomMode) -> None:
    """The commonest way to lose a learner's work is to hand out the sheet again."""
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    path = classroom.student_directory("ada") / "basic_first_steps-1.bas"
    path.write_text('10 PRINT "Hello, World!"\n', encoding="utf-8")

    written = classroom.distribute()
    assert written == []
    assert path.read_text(encoding="utf-8") == '10 PRINT "Hello, World!"\n'


def test_distributing_fills_in_the_sheets_a_learner_does_not_have_yet(
    classroom: ClassroomMode,
) -> None:
    """Handing out again is additive: what is missing arrives, what exists stays."""
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    lesson = classroom.lesson("basic_first_steps")
    missing = classroom.student_directory("ada") / classroom.handout_name(lesson, 2)
    missing.unlink()

    written = classroom.distribute()
    assert written == [missing]
    assert missing.read_text(encoding="utf-8") == lesson.checkpoints[2].starter_code


def test_distributing_with_nothing_assigned_is_refused(classroom: ClassroomMode) -> None:
    classroom.add_student("Ada")
    with pytest.raises(ClassroomError, match="nothing has been assigned"):
        classroom.distribute()


def test_distributing_to_nobody_is_refused(classroom: ClassroomMode) -> None:
    classroom.assign(["basic_first_steps"])
    with pytest.raises(ClassroomError, match="no students"):
        classroom.distribute()


def test_distributing_two_lessons_gives_a_file_per_checkpoint(
    classroom: ClassroomMode,
) -> None:
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps", "basic_loops"])
    written = classroom.distribute()
    assert len(written) == 6
    assert {path.name for path in written} == {
        "basic_first_steps-1.bas",
        "basic_first_steps-2.bas",
        "basic_first_steps-3.bas",
        "basic_loops-1.bas",
        "basic_loops-2.bas",
        "basic_loops-3.bas",
    }


# ---------------------------------------------------------------------------
# Taking work in
# ---------------------------------------------------------------------------


def test_collecting_marks_each_file_against_its_own_checkpoint(
    classroom: ClassroomMode,
) -> None:
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    lesson = classroom.lesson("basic_first_steps")
    work = classroom.student_directory("ada")
    (work / "basic_first_steps-1.bas").write_text(
        lesson.checkpoints[0].solution, encoding="utf-8"
    )

    submissions = {item.checkpoint: item for item in classroom.collect()}
    assert set(submissions) == {0, 1, 2}
    assert submissions[0].passed is True
    assert submissions[1].passed is False
    assert submissions[0].summary() == "checkpoint 1: pass"


def test_a_learner_who_has_not_started_hands_in_the_untouched_starters(
    classroom: ClassroomMode,
) -> None:
    """Distributed starters are handed back unchanged, and none of them pass."""
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    submissions = classroom.collect()
    assert len(submissions) == 3
    assert all(item.passed is False for item in submissions)
    rows = classroom.lesson_marks(submissions)
    assert rows == [
        {
            "student": "ada",
            "lesson": "basic_first_steps",
            "title": "First Steps in BASIC",
            "handed_in": 3,
            "checkpoints": 3,
            "passed": 0,
            "complete": False,
        }
    ]


def test_a_finished_lesson_is_complete(classroom: ClassroomMode) -> None:
    """Every checkpoint answered correctly rolls up to a complete lesson."""
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    lesson = classroom.lesson("basic_first_steps")
    work = classroom.student_directory("ada")
    for index, checkpoint in enumerate(lesson.checkpoints):
        (work / classroom.handout_name(lesson, index)).write_text(
            checkpoint.solution, encoding="utf-8"
        )

    rows = classroom.lesson_marks(classroom.collect())
    assert rows[0]["passed"] == 3
    assert rows[0]["complete"] is True


def test_a_missing_third_checkpoint_is_not_complete(classroom: ClassroomMode) -> None:
    """Two of three is not three of three, and the count says which."""
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    lesson = classroom.lesson("basic_first_steps")
    work = classroom.student_directory("ada")
    for index in (0, 1):
        (work / classroom.handout_name(lesson, index)).write_text(
            lesson.checkpoints[index].solution, encoding="utf-8"
        )

    rows = classroom.lesson_marks(classroom.collect())
    assert rows[0]["passed"] == 2
    assert rows[0]["complete"] is False


def test_marking_writes_no_progress_at_all(classroom: ClassroomMode) -> None:
    """Marking is a read, so it cannot move the teacher's place or anybody else's.

    A teacher is also a learner here, and a marking pass that advanced progress
    would move the teacher through the curriculum every time a class was collected.
    """
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    lesson = classroom.lesson("basic_first_steps")
    work = classroom.student_directory("ada")
    (work / classroom.handout_name(lesson, 0)).write_text(
        lesson.checkpoints[0].solution, encoding="utf-8"
    )

    classroom.collect()
    assert not (classroom.root / "progress.json").exists()
    assert not (work / "progress.json").exists()


def test_marking_reads_the_file_once(classroom: ClassroomMode) -> None:
    """A mark is taken from the bytes at collection time, not from a live editor."""
    classroom.add_student("Ada")
    lesson = classroom.lesson("basic_first_steps")
    path = classroom.student_directory("ada") / "basic_first_steps-1.bas"
    path.write_text(lesson.checkpoints[0].solution, encoding="utf-8")

    submission = classroom.mark("ada", lesson, 0, path)
    path.write_text("10 END\n", encoding="utf-8")
    assert submission.passed is True


def test_a_gap_in_the_middle_still_counts_the_later_checkpoint(
    classroom: ClassroomMode,
) -> None:
    """The exercises are independent, so 1 and 3 done is two of three, not one.

    Counting only the run up to the first gap would hide work the learner has
    actually done, and sent a teacher to re-teach something already learnt.
    """
    classroom.add_student("Ada")
    classroom.assign(["basic_first_steps"])
    classroom.distribute()

    lesson = classroom.lesson("basic_first_steps")
    work = classroom.student_directory("ada")
    for index in (0, 2):
        (work / classroom.handout_name(lesson, index)).write_text(
            lesson.checkpoints[index].solution, encoding="utf-8"
        )

    rows = classroom.lesson_marks(classroom.collect())
    assert rows[0]["passed"] == 2
    assert rows[0]["complete"] is False


# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------


def test_progress_for_a_student_who_has_done_nothing(classroom: ClassroomMode) -> None:
    classroom.add_student("Ada")
    rows = classroom.progress_for("ada")
    assert len(rows) == 20
    assert all(row["status"] == "not_started" for row in rows)


def test_class_progress_counts_what_each_student_has_finished(
    classroom: ClassroomMode,
) -> None:
    classroom.add_student("Ada")
    classroom.add_student("Grace")
    lesson = classroom.lesson("basic_first_steps")

    manager = LessonManager(progress_path=classroom.student_directory("ada") / "progress.json")
    for index, checkpoint in enumerate(lesson.checkpoints):
        manager.verify(lesson.id, index, checkpoint.solution)

    rows = {row["id"]: row for row in classroom.class_progress()}
    assert rows["ada"]["completed"] == 1
    assert rows["ada"]["started"] == 1
    assert rows["grace"]["completed"] == 0
    assert rows["grace"]["lessons"] == 20


def test_the_outline_groups_lessons_by_language(classroom: ClassroomMode) -> None:
    """Twenty lessons in a list is not a scheme of work."""
    grouped = outline(LessonManager())
    assert len(grouped) == 10
    assert "BASIC" in grouped
    assert len(grouped["BASIC"]) == 2
    assert grouped["BASIC"][0]["id"] == "basic_first_steps"


# ---------------------------------------------------------------------------
# Bundles
# ---------------------------------------------------------------------------


def test_a_bundle_round_trips_a_classroom(tmp_path: Path) -> None:
    source = ClassroomMode(root=tmp_path / "a")
    source.add_student("Ada")
    source.assign(["basic_first_steps"], note="Tuesday")
    source.distribute()
    source.set_presentation(24, fullscreen=True)
    bundle = source.create_bundle(tmp_path / "class.zip")

    target = ClassroomMode(root=tmp_path / "b")
    target.import_bundle(bundle)

    assert [student.name for student in target.roster] == ["Ada"]
    assert target.assignment.lesson_ids == ["basic_first_steps"]
    assert target.assignment.note == "Tuesday"
    assert target.presentation.font_size == 24
    assert (target.student_directory("ada") / "basic_first_steps-1.bas").is_file()


def test_importing_without_replacing_keeps_the_file_that_is_there(tmp_path: Path) -> None:
    source = ClassroomMode(root=tmp_path / "a")
    source.add_student("Ada")
    source.assign(["basic_first_steps"])
    source.distribute()
    bundle = source.create_bundle(tmp_path / "class.zip")

    target = ClassroomMode(root=tmp_path / "b")
    target.add_student("Ada")
    work = target.student_directory("ada") / "basic_first_steps-1.bas"
    work.write_text("MINE\n", encoding="utf-8")

    target.import_bundle(bundle)
    assert work.read_text(encoding="utf-8") == "MINE\n"

    target.import_bundle(bundle, replace=True)
    assert work.read_text(encoding="utf-8") != "MINE\n"


def test_a_bundle_cannot_write_outside_the_classroom(tmp_path: Path) -> None:
    """A zip is just a file, and its member names do not have to be honest."""
    hostile = tmp_path / "hostile.zip"
    with zipfile.ZipFile(hostile, "w") as bundle:
        bundle.writestr("../../escaped.txt", "no")

    target = ClassroomMode(root=tmp_path / "b")
    with pytest.raises(ClassroomError, match="outside itself"):
        target.import_bundle(hostile)
    assert not (tmp_path / "escaped.txt").exists()


def test_importing_a_bundle_that_is_not_there_is_refused(tmp_path: Path) -> None:
    with pytest.raises(ClassroomError, match="no bundle"):
        ClassroomMode(root=tmp_path / "b").import_bundle(tmp_path / "absent.zip")


# ---------------------------------------------------------------------------
# Presentation
# ---------------------------------------------------------------------------


def test_presentation_size_is_clamped_to_something_readable() -> None:
    assert PresentationSettings(font_size=2).font_size == MIN_PRESENTATION_FONT
    assert PresentationSettings(font_size=400).font_size == MAX_PRESENTATION_FONT
    assert PresentationSettings(font_size=24).font_size == 24


def test_presentation_settings_survive_a_reload(tmp_path: Path) -> None:
    first = ClassroomMode(root=tmp_path / "classroom")
    first.set_presentation(30, fullscreen=False)
    second = ClassroomMode(root=tmp_path / "classroom")
    assert second.presentation.font_size == 30
    assert second.presentation.fullscreen is False


def test_students_and_assignments_have_json_shapes() -> None:
    """The stored forms are part of the format, so they are asserted."""
    assert Student("ada", "Ada").to_dict() == {"id": "ada", "name": "Ada", "work_dir": ""}
    assert Assignment(["a"], "note", "now").to_dict() == {
        "lessons": ["a"],
        "note": "note",
        "created": "now",
    }
    assert StudentRoster.from_dict({"students": [{"id": "a", "name": "A"}]}).students[0].name == "A"
    assert StudentRoster.from_dict({"students": ["nonsense"]}).students == []
