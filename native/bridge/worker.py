#!/usr/bin/env python3
"""Runs a Time Warp Studio program for the C++ IDE and streams what happens.

Every language has a C++ executor, so the IDE never sends a *run* here: the
worker is kept for the two jobs that still need the Python engine -- the
Lessons panel's check of a learner's program, and the tools that drive this
implementation as the reference the ports are compared against (the command
matrix, the example smoke and the differential gates).

It is deliberately out of process. A program that loops forever, calls
`sys.exit`, or corrupts its own interpreter cannot take the IDE with it: the worst
case is a process that has to be killed, which the IDE can do.

#### Protocol

One JSON object per line, UTF-8, no embedded newlines.

Client to worker:

    {"t": "run", "language": "logo", "source": "...", "name": "mandala.logo"}
    {"t": "in-reply", "text": "Alice"}     # answer to an input request
    {"t": "stop"}                          # cooperative stop; the client may kill

Worker to client:

    {"t": "ready", "version": "1"}          # once, before the first run
    {"t": "out", "text": "..."}             # program output, one line at a time
    {"t": "canvas", "cmds": [ ... ]}        # batched drawing commands
    {"t": "pose", "x": .., "y": .., "h": .., "show": true}
    {"t": "err", "text": "..."}             # reported in the IDE's error panel
    {"t": "in", "prompt": "..."}            # the program wants input
    {"t": "done", "ok": true, "error": "...", "unsupported": [ ... ],
     "telemetry": { ... }, "variables": [ ... ]}

`telemetry` is the engine's own snapshot (`Interpreter.get_telemetry_snapshot`) and
`variables` is the engine's own variables table, both formatted for the panels by
`utils/inspection.py`. They ride on `done` rather than answering a request of their
own because there is exactly one moment at which they exist: the run has finished.
The raw variable mapping is *not* in `telemetry` -- the rows are its display form,
and sending both would be two copies of one fact that can disagree.

#### Lessons

The IDE's lesson panel is driven from here too, so that the *checking* of a
learner's work happens in the same engine that would run it. A second
implementation of "is this answer right" in C++ would be a second answer to the
question, and the first one to drift would be the one the learner saw.

    {"t": "lessons"}                        # the curriculum, without the answers
    {"t": "lesson-progress"}                # where the learner is in each lesson
    {"t": "lesson-hint", "lesson": "id", "checkpoint": 0}
    {"t": "lesson-solution", "lesson": "id", "checkpoint": 0}
    {"t": "lesson-check", "lesson": "id", "checkpoint": 0, "source": "..."}
    {"t": "lesson-reset", "lesson": "id"}

The answers:

    {"t": "lessons", "items": [ ... ]}
    {"t": "lesson-progress", "items": [ ... ], "completed": 3, "total": 20}
    {"t": "lesson-hint", "text": "...", "n": 1, "total": 2}
    {"t": "lesson-solution", "text": "..."}
    {"t": "lesson-check", "passed": true, "message": "...", "completed": false}
    {"t": "lesson-reset", "ok": true}

**The solutions are not in the `lessons` payload**, and that is deliberate: it is
sent to every client that opens the panel, and a lesson whose answer travels with
its questions is a lesson that has been spoiled before it is read. The worked
answer is one request away for a learner who has decided they need it.

#### Classroom

The class list and its marks belong to `features/classroom_mode.py`, which
`teach.py` writes. The panel asks through these two requests rather than reading
`roster.json` itself, so there is one reader of the format.

    {"t": "classroom-roster"}             # the class list and the assignment
    {"t": "classroom-marks"}              # every handed-in file, marked
    {"t": "classroom-presentation"}       # the size and window style for a lesson

The answers:

    {"t": "classroom-roster", "ok": true, "students": [ ... ]}
    {"t": "classroom-marks", "ok": true, "rows": [ ... ]}
    {"t": "classroom-presentation", "ok": true, "font_size": 24, "fullscreen": true}

An empty class is `ok` with no students, not an error: a new teacher has not
broken anything. Marks are refused while a program is running, because marking
runs the verifier and the verifier runs an interpreter -- the same reason a
lesson check is refused.

#### How the drawing is captured

`TurtleState` keeps append-only `lines` and `shapes` lists, so the drawing is read
by *diffing* those lists rather than by overriding every turtle method. That
choice matters: an override list has to be complete, and the one method somebody
forgets is a feature that silently stops working. A diff of an append-only list
cannot miss anything that was drawn.

The one thing a diff cannot see is a `clear()` that happens and is redrawn inside
a single polling interval, because the list is emptied and refilled before anyone
looks. `clear` and `reset` are therefore the only overrides.
"""

from __future__ import annotations

import json
import os
import sys
import threading
import time
from typing import Any, Dict, List, Optional

# Importable either as a module or as a bare script launched by path from any
# working directory: the IDE runs this by path, so finding the package is this
# file's job rather than the caller's.
if __package__ in (None, ""):
    _here = os.path.dirname(os.path.abspath(__file__))
    if _here not in sys.path:
        sys.path.insert(0, _here)

from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.core.language_registry import metadata_for  # noqa: E402
from time_warp.graphics.turtle_state import TurtleShape, TurtleState  # noqa: E402
from time_warp.utils import inspection  # noqa: E402

VERSION = "1"

# How often the drawing is flushed to the IDE while a program runs. Short enough
# that a drawing appears to be drawn, long enough that a fast program is not
# sending one message per line segment.
FLUSH_INTERVAL_SECONDS = 0.05

# How many input round-trips one program may make. A quiz legitimately reads many
# answers, so this is a runaway guard rather than a limit on the language; each
# individual wait is bounded separately by INPUT_TIMEOUT_SECONDS.
MAX_INPUT_ROUNDS = 1000

# How long an input request waits for an answer before the program continues with
# an empty value. A person needs longer than a machine, but not forever: a client
# that has gone away must not leave this process wedged.
INPUT_TIMEOUT_SECONDS = 300.0


class Emitter:
    """Writes protocol messages, safely from any thread.

    stdout is shared by the program thread (output), the flusher (drawing) and
    the main thread (errors), and a half-written line would desynchronise the
    client's parser. One lock around one `write` call is the whole invariant.
    """

    def __init__(self, stream: Any = None) -> None:
        self._stream = stream if stream is not None else sys.stdout
        self._lock = threading.Lock()

    def send(self, message: Dict[str, Any]) -> None:
        # `separators` keeps the payload compact: a drawing is mostly numbers, and
        # the default `", "` padding is real bytes over a pipe.
        try:
            line = json.dumps(message, separators=(",", ":"), ensure_ascii=False)
        except (TypeError, ValueError) as exc:  # pragma: no cover - defensive
            line = json.dumps(
                {"t": "err", "text": f"bridge: unencodable message: {exc}"}
            )
        with self._lock:
            try:
                self._stream.write(line + "\n")
                self._stream.flush()
            except (BrokenPipeError, ValueError, OSError):
                # The IDE has gone. Nothing can be reported to a pipe nobody is
                # reading, so the process ends here and now: `os._exit` rather
                # than an exception because the program thread may be inside an
                # interpreter loop that will never look at a stop flag again, and
                # an orphaned interpreter spinning forever is worse than an
                # abrupt exit.
                os._exit(0)


def _colour(rgb: Any) -> List[int]:
    """`[r, g, b]` from whatever the reference stored, clamped to a byte."""
    if not isinstance(rgb, (tuple, list)) or len(rgb) < 3:
        return [0, 0, 0]
    return [max(0, min(255, int(channel))) for channel in rgb[:3]]


def _shape_command(
    shape: TurtleShape, unsupported: List[str]
) -> Optional[Dict[str, Any]]:
    """One `TurtleShape` as a canvas command, or None when it cannot be drawn."""
    kind = shape.shape_type
    points = [[float(x), float(y)] for x, y in shape.points]
    flat: List[float] = [value for pair in points for value in pair]
    colour = _colour(shape.color)
    fill = _colour(shape.fill_color) if shape.fill_color else None

    def boxed() -> List[float]:
        """`(x, y, w, h)` of the points' bounding box."""
        if not points:
            return [0.0, 0.0, 0.0, 0.0]
        xs = [pair[0] for pair in points]
        ys = [pair[1] for pair in points]
        return [min(xs), min(ys), max(xs) - min(xs), max(ys) - min(ys)]

    if kind in ("polyline", "line"):
        return {"k": "poly", "pts": flat, "c": colour, "w": shape.width}
    if kind == "polygon":
        command: Dict[str, Any] = {
            "k": "poly",
            "pts": flat,
            "c": colour,
            "w": shape.width,
            "closed": True,
        }
        if fill is not None:
            command["fill"] = fill
        return command
    if kind == "point":
        if not points:
            return None
        return {"k": "point", "x": points[0][0], "y": points[0][1], "c": colour}
    if kind in ("rect", "ellipse"):
        x, y, width, height = boxed()
        command = {
            "k": kind,
            "x": x,
            "y": y,
            "w": width,
            "h": height,
            "c": colour,
            "lw": shape.width,
        }
        if fill is not None:
            command["fill"] = fill
        return command
    if kind == "text":
        if not points:
            return None
        return {
            "k": "text",
            "x": points[0][0],
            "y": points[0][1],
            "c": colour,
            "s": shape.text or "",
            "size": shape.font_size,
        }
    if kind == "bezier":
        # The chord endpoints of a cubic would be visibly wrong, so this is
        # reported as a gap instead of approximated.
        unsupported.append("bezier curves")
        return None
    if kind in ("rect_gradient", "ellipse_gradient"):
        # The C++ canvas does not blend gradients yet. The outline is still
        # drawn, and the gap is named rather than the shape vanishing.
        unsupported.append("gradient fills")
        x, y, width, height = boxed()
        return {
            "k": "rect" if kind == "rect_gradient" else "ellipse",
            "x": x,
            "y": y,
            "w": width,
            "h": height,
            "c": colour,
            "lw": shape.width,
        }
    if kind == "sprite":
        unsupported.append("sprites")
        return None

    unsupported.append(kind)
    return None


class Tape(TurtleState):
    """A `TurtleState` that reports what has been drawn since it was last asked.

    Subclassed rather than wrapped because the interpreters receive a
    `TurtleState` and call its methods directly; a wrapper would have to forward
    every one of them, and the one it forgot would silently stop working.
    """

    def __init__(self, emitter: Emitter) -> None:
        super().__init__()
        self._emitter = emitter
        self._sent_lines = 0
        self._sent_shapes = 0
        #: How many canvas commands have been sent, for the telemetry HUD. The
        #: turtle knows what it drew; only this side knows what was *sent*.
        self._sent_commands = 0
        # Guards the send-ordering as well as the counters: a `clear` that overtook
        # the batch of pre-clear lines would wipe a drawing and then draw it back.
        self._lock = threading.RLock()
        self.unsupported: List[str] = []

    @property
    def sent_commands(self) -> int:
        """Canvas commands sent to the IDE so far."""
        return self._sent_commands

    # -- the only overrides ---------------------------------------------

    def clear(self) -> None:
        # The lock is held across the base mutation as well as the counter reset,
        # so a flush cannot read the lists halfway through a clear and emit a batch
        # that this clear has already discarded. `drain` uses the same lock.
        with self._lock:
            super().clear()
            self._report_cleared()

    def reset(self) -> None:
        with self._lock:
            super().reset()
            self._report_cleared()
            # `reset` restores the default background as well as the default pen.
            self.report_background()

    def setbgcolor(self, r: int, g: int, b: int) -> None:
        # Not a drawing operation, so the diff cannot see it -- and the default
        # pen is white, which means a client that ignored this would draw white
        # ink on its own white canvas and appear to draw nothing at all.
        super().setbgcolor(r, g, b)
        self.report_background()

    def report_background(self) -> None:
        with self._lock:
            self._emitter.send(
                {"t": "canvas", "cmds": [{"k": "bg", "c": _colour(self.bg_color)}]}
            )

    def _report_cleared(self) -> None:
        with self._lock:
            self._sent_lines = 0
            self._sent_shapes = 0
            # Sent here rather than discovered by the diff: a clear that is
            # redrawn inside one polling interval leaves no trace in the lists,
            # and the symptom would be two drawings on top of each other.
            self._emitter.send({"t": "canvas", "cmds": [{"k": "clear"}]})

    # -- the diff ---------------------------------------------------------

    def drain(self) -> None:
        """Sends everything drawn since the last drain, then the turtle's pose.

        Text and drawing come from one place so their order is the program's
        order; the client applies them in the order it receives them.
        """
        with self._lock:
            commands: List[Dict[str, Any]] = []

            lines = self.lines
            if len(lines) < self._sent_lines:
                # The list shrank without a `clear` reaching us, which should not
                # happen; starting over is better than indexing past the end.
                self._sent_lines = 0
            while self._sent_lines < len(lines):
                line = lines[self._sent_lines]
                self._sent_lines += 1
                commands.append(
                    {
                        "k": "line",
                        "x1": float(line.start_x),
                        "y1": float(line.start_y),
                        "x2": float(line.end_x),
                        "y2": float(line.end_y),
                        "c": _colour(line.color),
                        "w": float(line.width),
                    }
                )

            shapes = self.shapes
            if len(shapes) < self._sent_shapes:
                self._sent_shapes = 0
            while self._sent_shapes < len(shapes):
                shape = shapes[self._sent_shapes]
                self._sent_shapes += 1
                command = _shape_command(shape, self.unsupported)
                if command is not None:
                    commands.append(command)

            if commands:
                self._sent_commands += len(commands)
                self._emitter.send({"t": "canvas", "cmds": commands})
            self._emitter.send(
                {
                    "t": "pose",
                    "x": float(self.x),
                    "y": float(self.y),
                    "h": float(self.heading),
                    "show": bool(self.visible),
                }
            )


class Session:
    """One program run: the interpreter, the tape, and the flush loop."""

    def __init__(self, emitter: Emitter) -> None:
        self._emitter = emitter
        self._tape = Tape(emitter)
        self._finished = threading.Event()
        self._input_lock = threading.Lock()
        self._input_ready = threading.Event()
        #: Set once the client's stdin has closed, so no prompt waits for an
        #: answer that cannot come.
        self._client_gone = False
        self._input_value = ""
        self._interpreter: Optional[Interpreter] = None
        #: Output lines sent this run, for the telemetry HUD.
        self._out_lines = 0

    # -- input ------------------------------------------------------------

    def request_input(self, prompt: str) -> str:
        """Asks the IDE for a line of input and waits for it.

        Called from the program thread, so it blocks that thread only. The flush
        loop keeps running, which is what lets the IDE redraw what has been drawn
        so far while it waits.
        """
        with self._input_lock:
            if self._client_gone:
                # Nobody can answer, now or later: a program that reads input in a
                # loop must not wait out the timeout once per question.
                return ""
            self._input_ready.clear()
            self._input_value = ""
        self._emitter.send({"t": "in", "prompt": prompt})
        if not self._input_ready.wait(INPUT_TIMEOUT_SECONDS):
            self._emitter.send(
                {
                    "t": "err",
                    "text": f"No answer to {prompt!r} arrived; continuing without it.",
                }
            )
            return ""
        with self._input_lock:
            return self._input_value

    def deliver_input(self, text: str) -> None:
        with self._input_lock:
            self._input_value = text
        self._input_ready.set()

    def release_input(self) -> None:
        """Answers this and every later prompt with nothing, because nobody is there.

        Called when stdin has closed: the client has gone, or it is a one-shot
        driver that sent one `run` and is reading the output (`printf
        '{"t":"run",...}' | worker.py` is a supported way to use this). A program
        blocked on a prompt must be allowed to finish, or that client never sees
        its `done`; and the answer has to be *standing*, because a quiz asks more
        than once and each later question would otherwise wait out the timeout.
        """
        with self._input_lock:
            self._client_gone = True
        self.deliver_input("")

    # -- running ----------------------------------------------------------

    def _flush_loop(self) -> None:
        while not self._finished.is_set():
            self._tape.drain()
            time.sleep(FLUSH_INTERVAL_SECONDS)
        # One final drain so nothing drawn in the last interval is lost.
        self._tape.drain()

    def _send_output(self, line: str) -> None:
        """Streams one line of program output, counting it for the HUD."""
        self._out_lines += 1
        self._emitter.send({"t": "out", "text": line + "\n"})

    def run(self, language: str, source: str, name: str) -> None:
        try:
            member = Language[language.upper()]
        except KeyError:
            text = f"Unknown language: {language}"
            self._emitter.send({"t": "err", "text": text})
            self._emitter.send({"t": "done", "ok": False, "error": text, "name": name})
            return

        interpreter = Interpreter(member)
        self._interpreter = interpreter
        self._out_lines = 0
        # The surface's background first, before any ink: the reference pen is
        # white on a dark canvas, and the IDE's canvas has its own defaults.
        self._tape.report_background()
        # Streamed rather than collected: the Python IDE shows output as it is
        # produced, and a program that runs for seconds without printing should
        # not look frozen in the meantime.
        interpreter.output_callback = lambda line: self._send_output(line)
        interpreter.input_callback = self.request_input

        self._finished.clear()
        flusher = threading.Thread(
            target=self._flush_loop, name="timewarp-flush", daemon=True
        )
        flusher.start()

        error = ""
        ok = True
        try:
            interpreter.load_program(source, member)
            interpreter.execute(self._tape)

            # The engine stops at an input statement and expects to be resumed:
            # `provide_input()` stores the answer and advances the program counter,
            # and the next `execute()` carries on from there. Calling it once per
            # run means every program that reads input stops dead at its first
            # prompt -- silently, with no error, which is exactly why nothing else
            # caught it. (The engine's own test helper loops for the same reason.)
            rounds = 0
            while interpreter.pending_input is not None and rounds < MAX_INPUT_ROUNDS:
                rounds += 1
                value = self.request_input(interpreter.pending_input.prompt)
                interpreter.provide_input(value)
                interpreter.execute(self._tape)
        except Exception as exc:  # noqa: BLE001 - the IDE has to see any failure
            ok = False
            error = f"{type(exc).__name__}: {exc}"
            self._emitter.send({"t": "err", "text": error})
        finally:
            self._finished.set()
            flusher.join(timeout=2.0)

        self._emitter.send(
            {
                "t": "done",
                "ok": ok,
                "error": error,
                "unsupported": sorted(set(self._tape.unsupported)),
                "name": name,
                **self._inspect(interpreter),
            }
        )

    def _inspect(self, interpreter: Interpreter) -> Dict[str, Any]:
        """The variables and telemetry tables for the panels.

        Never raises. A panel is a view of a finished run, and a view that cannot
        be built must not turn a program that ran correctly into a run that
        reported an error -- so a failure here sends empty tables and says so in
        the error panel, where it is visible without being fatal.
        """
        try:
            snapshot = dict(interpreter.get_telemetry_snapshot(self._tape))
            rows = inspection.variable_rows(interpreter.get_variables() or {})
            # The rows are the display form, so the raw mapping is dropped from
            # what is sent rather than travelling as a second copy of one fact.
            # The oscilloscope trace is for a HUD that reads audio; this one does
            # not.
            sent = {
                key: value
                for key, value in snapshot.items()
                if key not in {"variables", "audio_oscilloscope"}
            }
            return {
                "telemetry": sent,
                "telemetry_rows": inspection.telemetry_rows(
                    snapshot,
                    output_lines=self._out_lines,
                    drawing_commands=self._tape.sent_commands,
                ),
                "variables": rows,
            }
        except Exception as exc:  # noqa: BLE001 - the IDE has to keep the run
            self._emitter.send(
                {
                    "t": "err",
                    "text": f"Could not read the run's state: {type(exc).__name__}: {exc}",
                }
            )
            return {"telemetry": {}, "telemetry_rows": [], "variables": []}

    def stop(self) -> None:
        """Asks the interpreter to give up at its next check.

        Cooperative, so it is not guaranteed: the interpreter checks its budget
        between statements, and one statement that never returns cannot be
        interrupted from here. The IDE's hard kill covers that case.
        """
        interpreter = self._interpreter
        if interpreter is None:
            return
        interpreter.budget.cancelled = True
        interpreter.running = False


class LessonService:
    """The curriculum, progress, hints and checking, as the IDE asks for them.

    The manager is built once, on first use, because loading the curriculum reads
    a file and an IDE that never opens the lesson panel should not pay for it.
    """

    def __init__(self) -> None:
        self._manager: Optional[Any] = None
        self._error: Optional[str] = None

    @property
    def manager(self) -> Any:
        if self._manager is None and self._error is None:
            try:
                from time_warp.features.lesson_system import LessonManager

                self._manager = LessonManager()
            except Exception as exc:  # noqa: BLE001 - reported, not raised
                # A broken curriculum must not take the IDE down with it: the rest
                # of the studio works, and the panel is the only thing that is
                # unusable. It says so rather than going quiet.
                self._error = f"the lessons could not be loaded: {exc}"
        return self._manager

    def _fail(self, emitter: "Emitter", kind: str, note: str) -> None:
        emitter.send({"t": kind, "ok": False, "error": note})

    def _payload(self, lesson: Any, index: int) -> dict:
        """One checkpoint as the panel shows it: the prompt, never the answer."""
        checkpoint = lesson.checkpoints[index]
        return {
            "index": index,
            "title": checkpoint.title,
            "description": checkpoint.description,
            "starter": checkpoint.starter_code,
            "hints": list(checkpoint.hints),
            "expectation": checkpoint.expectation,
        }

    def _presentation(self) -> Dict[str, Any]:
        """The size a lesson is projected at, from the classroom settings.

        Read here rather than in the panel so that the size a *teacher* sets with
        `teach.py present` is the size the panel opens at, for the same reason the
        checking lives in one place: two answers to one question drift.
        """
        try:
            from time_warp.features.classroom_mode import ClassroomMode

            return ClassroomMode().presentation.to_dict()
        except Exception:  # noqa: BLE001 - a size is not worth failing over
            return {"font_size": 18, "fullscreen": True}

    def list_lessons(self, emitter: "Emitter") -> None:
        manager = self.manager
        if manager is None:
            return self._fail(emitter, "lessons", self._error or "no lessons")
        items = []
        for lesson in manager.list_lessons():
            items.append(
                {
                    "id": lesson.id,
                    "title": lesson.title,
                    "language": lesson.language_name,
                    # The *settings* key ("chip8"), not the enum member
                    # ("CHIP8"): it is what a document records as its language
                    # override, and converting on the C++ side would be a second
                    # place that knows the mapping.
                    "languageKey": metadata_for(lesson.language.name).folder_name,
                    "difficulty": lesson.difficulty,
                    "description": lesson.description,
                    "objectives": list(lesson.objectives),
                    "prerequisites": list(lesson.prerequisites),
                    "checkpoints": [
                        self._payload(lesson, index)
                        for index in range(lesson.checkpoint_count)
                    ],
                }
            )
        emitter.send(
            {"t": "lessons", "items": items, "presentation": self._presentation()}
        )

    def send_progress(self, emitter: "Emitter") -> None:
        manager = self.manager
        if manager is None:
            return self._fail(emitter, "lesson-progress", self._error or "no lessons")
        done, total = manager.completed_count()
        emitter.send(
            {
                "t": "lesson-progress",
                "items": manager.progress_report(),
                "completed": done,
                "total": total,
            }
        )

    def hint(
        self, emitter: "Emitter", message: Dict[str, Any], solution: bool = False
    ) -> None:
        manager = self.manager
        if manager is None:
            return self._fail(emitter, "lesson-hint", self._error or "no lessons")
        lesson_id = str(message.get("lesson", ""))
        index = int(message.get("checkpoint", 0))
        try:
            lesson = manager.lesson(lesson_id)
            if solution:
                text = lesson.checkpoints[index].solution
                emitter.send({"t": "lesson-solution", "ok": True, "text": text})
                return
            already = manager.hints_used(lesson_id)
            text = manager.hint(lesson_id, index, already)
        except Exception as exc:  # noqa: BLE001 - reported, not raised
            return self._fail(
                emitter, "lesson-solution" if solution else "lesson-hint", str(exc)
            )
        emitter.send(
            {
                "t": "lesson-hint",
                "ok": True,
                "text": text,
                "n": min(already + 1, len(lesson.checkpoints[index].hints)),
                "total": len(lesson.checkpoints[index].hints),
            }
        )

    def check(self, emitter: "Emitter", message: Dict[str, Any]) -> None:
        manager = self.manager
        if manager is None:
            return self._fail(emitter, "lesson-check", self._error or "no lessons")
        lesson_id = str(message.get("lesson", ""))
        index = int(message.get("checkpoint", 0))
        source = str(message.get("source", ""))
        try:
            result = manager.verify(lesson_id, index, source)
            lesson = manager.lesson(lesson_id)
            done, total = manager.completed_count()
        except Exception as exc:  # noqa: BLE001 - reported, not raised
            return self._fail(emitter, "lesson-check", str(exc))
        emitter.send(
            {
                "t": "lesson-check",
                "ok": True,
                "passed": bool(result.passed),
                "message": result.message,
                "lesson": lesson_id,
                "checkpoint": index,
                "completed": manager.status(lesson_id).value == "completed",
                "finished": done,
                "total": total,
                "checkpoints": lesson.checkpoint_count,
            }
        )

    def reset(self, emitter: "Emitter", message: Dict[str, Any]) -> None:
        manager = self.manager
        if manager is None:
            return self._fail(emitter, "lesson-reset", self._error or "no lessons")
        lesson_id = str(message.get("lesson", ""))
        try:
            manager.reset(lesson_id)
        except Exception as exc:  # noqa: BLE001 - reported, not raised
            return self._fail(emitter, "lesson-reset", str(exc))
        # The reply names the lesson rather than saying only "done": a panel can
        # have a reset in flight while the learner clicks another lesson, and
        # "something was reset" would leave it guessing which.
        emitter.send({"t": "lesson-reset", "ok": True, "lesson": lesson_id})


class ClassroomService:
    """The class list and its marks, as the classroom panel asks for them.

    The roster format belongs to `classroom_mode.py`, which `teach.py` also
    writes. The panel therefore reads it through this service rather than
    parsing `roster.json` itself: two readers of one file drift, and the one that
    drifts is the one a teacher is looking at.

    Marks come from the engine's own `collect()`/`lesson_marks()`, so the panel's
    verdict on a learner's file and `teach.py collect`'s verdict are one
    function's answer.
    """

    def _fail(self, emitter: "Emitter", kind: str, note: str) -> None:
        emitter.send({"t": kind, "ok": False, "error": note})

    def _load(self, emitter: "Emitter", kind: str) -> Any:
        """A fresh view of the class files, or None after reporting why not.

        Built per request rather than cached: `teach.py` writes these files while
        the IDE is open, and a cached class would make Refresh show the class as
        it was when the tab first opened. Reading two small JSON files costs less
        than a stale mark sheet is worth.
        """
        try:
            from time_warp.features.classroom_mode import ClassroomMode

            return ClassroomMode()
        except Exception as exc:  # noqa: BLE001 - reported, not raised
            # A broken class list must not take the IDE down: the rest of the
            # studio works, and the panel says what is wrong instead of showing
            # an empty class as if there were one.
            self._fail(emitter, kind, f"the class list could not be loaded: {exc}")
            return None

    def roster(self, emitter: "Emitter") -> None:
        mode = self._load(emitter, "classroom-roster")
        if mode is None:
            return
        students = []
        for student in mode.roster:
            # `directory()` rather than `student_directory()`: reading the class
            # list must not create a folder for somebody, which is a side effect
            # a teacher would see as the panel having "done" something.
            students.append(
                {
                    "id": student.student_id,
                    "name": student.name,
                    "directory": str(student.directory(mode.root)),
                }
            )
        emitter.send(
            {
                "t": "classroom-roster",
                "ok": True,
                "students": students,
                "assignment": {
                    "lessons": list(mode.assignment.lesson_ids),
                    "note": mode.assignment.note,
                },
                "presentation": mode.presentation.to_dict(),
            }
        )

    def presentation(self, emitter: "Emitter") -> None:
        """The size and window style a lesson is taught at.

        A request of its own because presentation mode is entered without opening
        the classroom page: learning a font size must not cost a walk over every
        learner's folder, which is what asking for the marks would do.
        """
        mode = self._load(emitter, "classroom-presentation")
        if mode is None:
            return
        settings = mode.presentation.to_dict()
        emitter.send(
            {
                "t": "classroom-presentation",
                "ok": True,
                "font_size": settings["font_size"],
                "fullscreen": settings["fullscreen"],
            }
        )

    def marks(self, emitter: "Emitter") -> None:
        mode = self._load(emitter, "classroom-marks")
        if mode is None:
            return
        if not mode.roster.students:
            # Not an error: an empty class has no marks, and the panel shows its
            # empty state. Reporting it as a failure would make a new teacher
            # think something was broken.
            return emitter.send({"t": "classroom-marks", "ok": True, "rows": []})
        try:
            submissions = mode.collect()
            rows = mode.lesson_marks(submissions)
        except Exception as exc:  # noqa: BLE001 - reported, not raised
            return self._fail(emitter, "classroom-marks", str(exc))
        emitter.send({"t": "classroom-marks", "ok": True, "rows": rows})


def main(argv: Optional[List[str]] = None) -> int:
    del argv
    emitter = Emitter(sys.stdout)
    # The protocol owns stdout from here on. A stray `print` in the interpreter or
    # in a library would otherwise land in the middle of a message and desynchronise
    # the client's parser, which is a failure that looks like corruption rather
    # than like a bug. Redirecting it to stderr keeps it visible and harmless.
    sys.stdout = sys.stderr

    emitter.send({"t": "ready", "version": VERSION})

    session: Optional[Session] = None
    program: Optional[threading.Thread] = None
    lessons = LessonService()
    classroom = ClassroomService()
    for raw in sys.stdin:
        raw = raw.strip()
        if not raw:
            continue
        try:
            message = json.loads(raw)
        except ValueError as exc:
            emitter.send({"t": "err", "text": f"bridge: malformed request: {exc}"})
            continue

        kind = message.get("t")
        if kind == "run":
            # One program at a time: finish the previous one before starting the
            # next, or two interpretations interleave their drawing.
            if program is not None:
                program.join()
            session = Session(emitter)
            # The program runs on its own thread so this loop stays free to hand
            # it the two messages it can only receive *while* it runs: `in-reply`
            # and `stop`. Running it inline -- which is what this did -- meant a
            # program that asked for input could never be answered, because the
            # reply sat unread in the pipe behind the blocked loop. That is why
            # every program using INPUT stopped dead at its first prompt, and why
            # cooperative stop never took effect (only the client's hard kill ever
            # worked, which hid it).
            program = threading.Thread(
                target=session.run,
                args=(
                    str(message.get("language", "")),
                    str(message.get("source", "")),
                    str(message.get("name", "")),
                ),
                name="timewarp-program",
                daemon=True,
            )
            program.start()
        elif kind == "in-reply":
            if session is not None:
                session.deliver_input(str(message.get("text", "")))
        elif kind == "stop":
            if session is not None:
                session.stop()
        elif kind == "lessons":
            lessons.list_lessons(emitter)
        elif kind == "lesson-progress":
            lessons.send_progress(emitter)
        elif kind == "lesson-hint":
            lessons.hint(emitter, message)
        elif kind == "lesson-solution":
            lessons.hint(emitter, message, solution=True)
        elif kind == "lesson-check":
            # Checking a lesson runs an interpreter, and a program run is *also*
            # an interpreter. Two of them at once would share the engine's module
            # state, so this refuses rather than racing: the panel's answer is
            # "stop the program first", which is also the useful thing to be told.
            if program is not None and program.is_alive():
                emitter.send(
                    {
                        "t": "lesson-check",
                        "ok": False,
                        "error": "a program is running; stop it before checking a lesson",
                    }
                )
            else:
                lessons.check(emitter, message)
        elif kind == "lesson-reset":
            lessons.reset(emitter, message)
        elif kind == "classroom-roster":
            classroom.roster(emitter)
        elif kind == "classroom-presentation":
            classroom.presentation(emitter)
        elif kind == "classroom-marks":
            # Marking runs the verifier, and the verifier runs an interpreter:
            # the same race `lesson-check` refuses above. The class list is a
            # file read and is safe to serve at any time.
            if program is not None and program.is_alive():
                emitter.send(
                    {
                        "t": "classroom-marks",
                        "ok": False,
                        "error": "a program is running; stop it before collecting the marks",
                    }
                )
            else:
                classroom.marks(emitter)
        else:
            emitter.send({"t": "err", "text": f"bridge: unknown request {kind!r}"})

    # stdin has closed, which means the client has gone or has nothing more to
    # say. A program still waiting on a prompt gets an empty answer so it can
    # finish and emit its `done`, then this gives it a moment to do so before
    # leaving: a daemon thread must not be what keeps a client's orphan alive.
    if session is not None:
        session.release_input()
    if program is not None:
        program.join(timeout=5.0)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
