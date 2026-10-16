"""The teacher's side of the lessons: a roster, assignments, marking and bundles.

Time Warp Studio is a teaching environment, and a teacher's work is not the same
as a learner's. They need to know *what* to teach (the lesson plans in
`lesson_system`), *who* is where, and a way to hand work out and take it back in.
This module is that half.

Nothing here runs a program by itself. Marking does, and it does it through the
same verifier a learner's own "check" uses -- so a teacher's mark and a learner's
green tick can never disagree, because they are the same function. That is the
whole reason marking lives in the engine rather than in the tool that displays it.

#### What lives where

Everything a class owns is under one directory (``~/.time_warp/classroom`` by
default), and the layout is deliberately plain enough to read, zip and hand to
somebody else:

```
classroom/
  roster.json                  the students, and where their work lives
  assignment.json              what was set most recently
  presentation.json            font size and fullscreen for teaching from the front
  students/
    <student-id>/              one directory per learner
      basic_first_steps.bas    work handed out, and handed back
      progress.json            that learner's place in the curriculum
```

A *bundle* is that directory zipped, so a class on a locked-down network can be
set up from a file rather than from a shared drive.

#### What this module deliberately does not do

It does not know about the IDE. The lesson panel drives it through the bridge and
the CLI drives it directly, and neither is privileged; a teacher who prefers the
command line can run the whole class without opening a window.
"""

from __future__ import annotations

import json
import shutil
import zipfile
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterator, Sequence

from ..core.config import APP_DATA_DIR
from ..logging_config import get_logger
from .lesson_system import (
    Lesson,
    LessonError,
    LessonManager,
    LessonStatus,
    load_curriculum,
    verify_checkpoint,
)

logger = get_logger(__name__)

#: Where a class's own files live. Beside the learner's progress, under the same
#: application directory, so one backup or wipe covers everything the studio keeps.
CLASSROOM_DIR: Path = APP_DATA_DIR / "classroom"

#: The font a lesson is projected at. Bounds are enforced rather than trusted: a
#: size of 4 is unreadable and a size of 400 makes one word fill the screen, and
#: both would be reported later as "presentation mode is broken".
MIN_PRESENTATION_FONT = 12
MAX_PRESENTATION_FONT = 48
DEFAULT_PRESENTATION_FONT = 18

#: The first extension of each language, for naming handed-out files. Kept here
#: rather than in the language module because it is a *file naming* decision, and
#: a learner opening a handed-out file needs the extension their editor expects.
_ASSIGNMENT_EXTENSIONS = {
    "BASIC": ".bas",
    "PILOT": ".pilot",
    "LOGO": ".logo",
    "COMAL": ".comal",
    "REXX": ".rexx",
    "SCHEME": ".scm",
    "FOCAL": ".foc",
    "CHIP8": ".ch8",
    "APL": ".apl",
    "SMALLTALK": ".st",
}


class ClassroomError(Exception):
    """Raised when a classroom operation cannot be carried out."""


def _now() -> str:
    """A timestamp that sorts and reads the same everywhere."""
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def _safe_name(text: str) -> str:
    """A directory name made from a student's name or id.

    Anything that is not a letter, digit, dash or underscore becomes a dash: a
    name is entered by a person, and a slash or a `.` in it must not be able to
    point the classroom at a directory outside itself.
    """
    cleaned = "".join(
        character if character.isalnum() or character in "-_" else "-"
        for character in text.strip()
    )
    return "-".join(part for part in cleaned.split("-") if part) or "student"


@dataclass
class Student:
    """One learner, and where their work is kept."""

    student_id: str
    name: str
    work_dir: str = ""

    def directory(self, root: Path) -> Path:
        """This student's directory, defaulting to their id under *root*."""
        if self.work_dir:
            return Path(self.work_dir).expanduser()
        return Path(root) / "students" / _safe_name(self.student_id)

    def to_dict(self) -> dict[str, Any]:
        return {"id": self.student_id, "name": self.name, "work_dir": self.work_dir}

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Student":
        return cls(
            student_id=str(data.get("id", "")),
            name=str(data.get("name", "")),
            work_dir=str(data.get("work_dir", "")),
        )


@dataclass
class StudentRoster:
    """The class list."""

    students: list[Student] = field(default_factory=list)

    def __iter__(self) -> Iterator[Student]:
        return iter(self.students)

    def __len__(self) -> int:
        return len(self.students)

    def add(self, name: str, student_id: str = "", work_dir: str = "") -> Student:
        """Register a student. The id defaults to a slug of the name."""
        clean_name = name.strip()
        if not clean_name:
            raise ClassroomError("a student needs a name")
        identifier = (student_id or clean_name).strip().lower().replace(" ", "-")
        if any(student.student_id == identifier for student in self.students):
            raise ClassroomError(f"there is already a student with the id {identifier!r}")
        student = Student(student_id=identifier, name=clean_name, work_dir=work_dir)
        self.students.append(student)
        return student

    def remove(self, student_id: str) -> bool:
        before = len(self.students)
        self.students = [
            student for student in self.students if student.student_id != student_id
        ]
        return len(self.students) != before

    def get(self, student_id: str) -> Student:
        for student in self.students:
            if student.student_id == student_id:
                return student
        raise ClassroomError(f"no student with the id {student_id!r}")

    def find(self, prefix: str) -> list[Student]:
        """Every student whose id or name starts with *prefix*, case-insensitively."""
        text = prefix.strip().lower()
        return [
            student
            for student in self.students
            if student.student_id.startswith(text) or student.name.lower().startswith(text)
        ]

    def to_dict(self) -> dict[str, Any]:
        return {"students": [student.to_dict() for student in self.students]}

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "StudentRoster":
        return cls(
            students=[
                Student.from_dict(raw) for raw in data.get("students", ()) if isinstance(raw, dict)
            ]
        )


@dataclass
class Assignment:
    """What has been set, and when."""

    lesson_ids: list[str]
    note: str = ""
    created: str = ""

    def to_dict(self) -> dict[str, Any]:
        return {"lessons": list(self.lesson_ids), "note": self.note, "created": self.created}

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Assignment":
        return cls(
            lesson_ids=[str(item) for item in data.get("lessons", ())],
            note=str(data.get("note", "")),
            created=str(data.get("created", "")),
        )


@dataclass
class Submission:
    """One handed-in exercise file, and whether it passes its own checkpoint."""

    student_id: str
    lesson_id: str
    checkpoint: int
    path: Path
    passed: bool = False
    message: str = ""

    @property
    def filename(self) -> str:
        return self.path.name

    def summary(self) -> str:
        verdict = "pass" if self.passed else "not yet"
        return f"checkpoint {self.checkpoint + 1}: {verdict}"


@dataclass
class PresentationSettings:
    """How a lesson is shown when it is being taught from the front."""

    font_size: int = DEFAULT_PRESENTATION_FONT
    fullscreen: bool = True

    def __post_init__(self) -> None:
        self.font_size = max(
            MIN_PRESENTATION_FONT, min(MAX_PRESENTATION_FONT, int(self.font_size))
        )

    def to_dict(self) -> dict[str, Any]:
        return {"font_size": self.font_size, "fullscreen": self.fullscreen}

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "PresentationSettings":
        return cls(
            font_size=int(data.get("font_size", DEFAULT_PRESENTATION_FONT)),
            fullscreen=bool(data.get("fullscreen", True)),
        )


class ClassroomMode:
    """A class: its roster, its assignment, its marking and its presentation size."""

    def __init__(self, root: Path | None = None, *, lessons_path: Path | None = None) -> None:
        self.root = Path(root) if root is not None else CLASSROOM_DIR
        self._lessons_path = lessons_path
        self._lessons: list[Lesson] | None = None
        self.roster = self._load_roster()
        self.assignment = self._load_assignment()
        self.presentation = self._load_presentation()

    # ---- the curriculum ----------------------------------------------------

    @property
    def lessons(self) -> list[Lesson]:
        if self._lessons is None:
            self._lessons = load_curriculum(self._lessons_path or _default_lessons_path())
        return self._lessons

    def lesson(self, lesson_id: str) -> Lesson:
        for lesson in self.lessons:
            if lesson.id == lesson_id:
                return lesson
        raise LessonError(f"no lesson with the id {lesson_id!r}")

    # ---- storage -----------------------------------------------------------

    def _write_json(self, name: str, payload: dict[str, Any]) -> Path:
        self.root.mkdir(parents=True, exist_ok=True)
        path = self.root / name
        path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")
        return path

    def _read_json(self, name: str) -> dict[str, Any]:
        path = self.root / name
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
        except FileNotFoundError:
            return {}
        except (OSError, json.JSONDecodeError) as exc:
            # A class list is worth more than a tidy log: report and carry on with
            # an empty one rather than refusing to open the classroom.
            logger.warning("Ignoring unreadable %s: %s", path, exc)
            return {}
        return data if isinstance(data, dict) else {}

    def _load_roster(self) -> StudentRoster:
        return StudentRoster.from_dict(self._read_json("roster.json"))

    def _load_assignment(self) -> Assignment:
        return Assignment.from_dict(self._read_json("assignment.json"))

    def _load_presentation(self) -> PresentationSettings:
        return PresentationSettings.from_dict(self._read_json("presentation.json"))

    def save_roster(self) -> None:
        self._write_json("roster.json", self.roster.to_dict())

    def save_assignment(self) -> None:
        self._write_json("assignment.json", self.assignment.to_dict())

    def save_presentation(self) -> None:
        self._write_json("presentation.json", self.presentation.to_dict())

    # ---- the roster --------------------------------------------------------

    def add_student(self, name: str, student_id: str = "", work_dir: str = "") -> Student:
        student = self.roster.add(name, student_id, work_dir)
        self.save_roster()
        return student

    def remove_student(self, student_id: str) -> bool:
        """Forget a student. Their directory is left alone, deliberately.

        Removing somebody from a class list is an administrative act, and it must
        not be able to delete an afternoon's work in the same keystroke.
        """
        removed = self.roster.remove(student_id)
        if removed:
            self.save_roster()
        return removed

    def student_directory(self, student_id: str) -> Path:
        directory = self.roster.get(student_id).directory(self.root)
        directory.mkdir(parents=True, exist_ok=True)
        return directory

    # ---- setting work ------------------------------------------------------

    def assign(self, lesson_ids: Sequence[str], note: str = "") -> Assignment:
        """Record what the class has been set, refusing lessons that do not exist."""
        for lesson_id in lesson_ids:
            self.lesson(lesson_id)
        self.assignment = Assignment(
            lesson_ids=list(lesson_ids), note=note, created=_now()
        )
        self.save_assignment()
        return self.assignment

    def handout_name(self, lesson: Lesson, index: int) -> str:
        """The file one learner works in for one checkpoint.

        The checkpoint number is in the name because the checkpoints are separate
        exercises: a teacher collecting a class needs to know which one a file is,
        and a learner needs to be able to tell them apart in a file list.
        """
        extension = _ASSIGNMENT_EXTENSIONS.get(lesson.language.name, ".txt")
        return f"{lesson.id}-{index + 1}{extension}"

    def handout_names(self, lesson: Lesson) -> list[str]:
        return [self.handout_name(lesson, index) for index in range(lesson.checkpoint_count)]

    def distribute(self, lesson_ids: Sequence[str] | None = None) -> list[Path]:
        """Write each assigned lesson's **starter code** into every student's directory.

        Handing out the solution instead of the starting point is the mistake this
        exists to prevent: the starter is what the lesson is built around, and a
        teacher who distributes the answer has nothing left to teach.

        Existing work is never overwritten. A learner who has already started has
        their version, and a distribution that replaced it would be an accident
        nobody forgives.
        """
        ids = list(lesson_ids if lesson_ids is not None else self.assignment.lesson_ids)
        if not ids:
            raise ClassroomError("nothing has been assigned")
        if not self.roster.students:
            raise ClassroomError("there are no students to hand work to")

        written: list[Path] = []
        for student in self.roster:
            directory = self.student_directory(student.student_id)
            for lesson_id in ids:
                lesson = self.lesson(lesson_id)
                for index, checkpoint in enumerate(lesson.checkpoints):
                    path = directory / self.handout_name(lesson, index)
                    if path.exists():
                        continue
                    path.write_text(checkpoint.starter_code, encoding="utf-8")
                    written.append(path)
        return written

    # ---- taking work in ----------------------------------------------------

    def collect(self, lesson_ids: Sequence[str] | None = None) -> list[Submission]:
        """Find every handed-out file and mark it against the checkpoint it belongs to.

        Files a learner never received are not invented: a checkpoint with no file
        comes back with no submission, so "not handed in" and "handed in and wrong"
        stay different things.
        """
        ids = list(lesson_ids if lesson_ids is not None else self.assignment.lesson_ids)
        found: list[Submission] = []
        for student in self.roster:
            directory = student.directory(self.root)
            for lesson_id in ids:
                lesson = self.lesson(lesson_id)
                for index in range(lesson.checkpoint_count):
                    path = directory / self.handout_name(lesson, index)
                    if path.is_file():
                        found.append(self.mark(student.student_id, lesson, index, path))
        return found

    def mark(
        self, student_id: str, lesson: Lesson, index: int, path: Path
    ) -> Submission:
        """Run *path* against checkpoint *index* of *lesson*.

        The file is read once, here, and the mark taken from those bytes: a learner
        who is still editing must not be able to change a mark underneath the
        teacher reading it.

        Marking deliberately does **not** touch anybody's progress. It uses the
        verifier directly rather than a `LessonManager`, which means a teacher can
        collect a class without moving a single learner's place in the curriculum --
        and without writing into their own.
        """
        source = path.read_text(encoding="utf-8")
        result = verify_checkpoint(lesson.checkpoints[index], source, lesson.language)
        return Submission(
            student_id=student_id,
            lesson_id=lesson.id,
            checkpoint=index,
            path=path,
            passed=result.passed,
            message=result.message,
        )

    def lesson_marks(self, submissions: Sequence[Submission]) -> list[dict[str, Any]]:
        """Roll collected files up into one line per learner per lesson.

        `checkpoints` counts the files that passed rather than the run up to the
        first gap: the exercises are independent, so a learner who did 1 and 3 has
        done two of three, and hiding the third would be wrong.
        """
        grouped: dict[tuple[str, str], list[Submission]] = {}
        for item in submissions:
            grouped.setdefault((item.student_id, item.lesson_id), []).append(item)
        rows: list[dict[str, Any]] = []
        for (student_id, lesson_id), items in sorted(grouped.items()):
            lesson = self.lesson(lesson_id)
            rows.append(
                {
                    "student": student_id,
                    "lesson": lesson_id,
                    "title": lesson.title,
                    "handed_in": len(items),
                    "checkpoints": lesson.checkpoint_count,
                    "passed": sum(1 for item in items if item.passed),
                    "complete": len(items) == lesson.checkpoint_count
                    and all(item.passed for item in items),
                }
            )
        return rows

    def _marker_for(self, student_id: str) -> LessonManager:
        """A manager over the student's own progress file.

        Reading a learner's progress must not read the teacher's, which is what a
        default `LessonManager` would do: the teacher is a learner in this
        application, and the two files are one line of code apart.
        """
        try:
            directory = self.student_directory(student_id)
        except ClassroomError:
            directory = self.root
        return LessonManager(
            lessons_path=self._lessons_path or _default_lessons_path(),
            progress_path=directory / "progress.json",
        )

    # ---- reporting ---------------------------------------------------------

    def progress_for(self, student_id: str) -> list[dict[str, Any]]:
        """One row per lesson for one learner."""
        return self._marker_for(student_id).progress_report()

    def class_progress(self) -> list[dict[str, Any]]:
        """One row per student, with how many lessons they have finished."""
        rows: list[dict[str, Any]] = []
        for student in self.roster:
            report = self.progress_for(student.student_id)
            completed = sum(
                1 for row in report if row["status"] == LessonStatus.COMPLETED.value
            )
            started = sum(
                1
                for row in report
                if row["status"] != LessonStatus.NOT_STARTED.value
            )
            hints = sum(int(row["hints_used"]) for row in report)
            rows.append(
                {
                    "id": student.student_id,
                    "name": student.name,
                    "completed": completed,
                    "started": started,
                    "lessons": len(report),
                    "hints_used": hints,
                }
            )
        return rows

    # ---- bundles -----------------------------------------------------------

    def create_bundle(self, output_path: Path) -> Path:
        """Zip the classroom: roster, assignment, presentation and every student's work.

        Students who have not started are included, because a bundle is also how a
        class is *set up* on another machine and an empty directory is a student
        with no work yet rather than a student to leave out.
        """
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        with zipfile.ZipFile(output_path, "w", zipfile.ZIP_DEFLATED) as bundle:
            for name in ("roster.json", "assignment.json", "presentation.json"):
                path = self.root / name
                if path.is_file():
                    bundle.write(path, name)
            for student in self.roster:
                directory = student.directory(self.root)
                if not directory.is_dir():
                    continue
                for path in sorted(directory.rglob("*")):
                    if path.is_file() and "__pycache__" not in path.parts:
                        bundle.write(path, str(path.relative_to(self.root)))
        return output_path

    def import_bundle(self, bundle_path: Path, *, replace: bool = False) -> StudentRoster:
        """Unpack a bundle into this classroom.

        `replace` decides whether existing files are overwritten, and it is a
        parameter rather than a default because the two cases are different: setting
        up a new classroom wants to be additive, and refreshing one wants to be
        authoritative.
        """
        bundle_path = Path(bundle_path)
        if not bundle_path.is_file():
            raise ClassroomError(f"no bundle at {bundle_path}")
        self.root.mkdir(parents=True, exist_ok=True)
        with zipfile.ZipFile(bundle_path) as bundle:
            for member in bundle.namelist():
                # `extract` will happily write outside the destination, so the
                # target is resolved and checked before anything is written.
                target = (self.root / member).resolve()
                if not str(target).startswith(str(self.root.resolve())):
                    raise ClassroomError(f"the bundle contains a path outside itself: {member}")
                if target.exists() and not replace:
                    continue
                if member.endswith("/"):
                    target.mkdir(parents=True, exist_ok=True)
                    continue
                target.parent.mkdir(parents=True, exist_ok=True)
                with bundle.open(member) as source, target.open("wb") as destination:
                    shutil.copyfileobj(source, destination)
        self.roster = self._load_roster()
        self.assignment = self._load_assignment()
        self.presentation = self._load_presentation()
        self.save_roster()
        return self.roster

    # ---- presentation ------------------------------------------------------

    def set_presentation(self, font_size: int, fullscreen: bool = True) -> PresentationSettings:
        self.presentation = PresentationSettings(
            font_size=font_size, fullscreen=fullscreen
        )
        self.save_presentation()
        return self.presentation


def _default_lessons_path() -> Path:
    """The shipped curriculum, resolved without importing it twice."""
    from .lesson_system import LESSONS_FILE

    return LESSONS_FILE


def language_extensions() -> dict[str, str]:
    """The handed-out file extension per language, for the tools and the docs."""
    return dict(_ASSIGNMENT_EXTENSIONS)


def outline(manager: LessonManager) -> dict[str, Any]:
    """The curriculum as a teacher wants to see it: by language, in teaching order.

    A flat list of twenty lessons is not a scheme of work. Grouping by language
    and keeping the order is what turns it into one, and it is the shape both the
    teacher tool and the lesson panel browse with.
    """
    grouped: dict[str, list[dict[str, Any]]] = {}
    for lesson in manager.list_lessons():
        grouped.setdefault(lesson.language_name, []).append(
            {
                "id": lesson.id,
                "title": lesson.title,
                "difficulty": lesson.difficulty,
                "description": lesson.description,
                "checkpoints": lesson.checkpoint_count,
                "prerequisites": list(lesson.prerequisites),
            }
        )
    return grouped
