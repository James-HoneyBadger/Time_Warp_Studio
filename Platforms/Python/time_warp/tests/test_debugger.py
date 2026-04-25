"""Tests for time_warp.core.debugger module.

Covers both the pure timeline/dataclass layer and full integration tests of
the interpreter's debug machinery (step-through, breakpoints, stop-while-
paused, Python settrace, call_stack property, timeline navigation/export).
"""

import threading

import pytest

from time_warp.core.debugger import (
    ExecutionFrame,
    ExecutionState,
    ExecutionTimeline,
    VariableSnapshot,
)
from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _run_debug(interp: Interpreter, on_pause, timeout: float = 10.0) -> bool:
    """Run *interp* in a daemon thread; return True if it finished cleanly."""
    interp.set_debug_callback(on_pause)
    t = threading.Thread(
        target=lambda: interp.execute(TurtleState()), daemon=True
    )
    t.start()
    t.join(timeout)
    return not t.is_alive()


# ---------------------------------------------------------------------------
# Unit tests — timeline dataclasses
# ---------------------------------------------------------------------------

def test_execution_state_enum():
    """Test ExecutionState enum values."""
    assert ExecutionState.STOPPED.value == "stopped"
    assert ExecutionState.RUNNING.value == "running"
    assert ExecutionState.PAUSED.value == "paused"
    assert ExecutionState.STEP.value == "step"
    assert ExecutionState.FINISHED.value == "finished"
    assert ExecutionState.ERROR.value == "error"


def test_variable_snapshot():
    """Test VariableSnapshot dataclass."""
    snapshot = VariableSnapshot(
        name="x",
        value=42,
        type_name="int",
        line=10,
        timestamp=1234567890,
    )
    assert snapshot.name == "x"
    assert snapshot.value == 42
    assert snapshot.type_name == "int"
    assert snapshot.line == 10
    assert snapshot.timestamp == 1234567890


def test_execution_frame():
    """Test ExecutionFrame dataclass."""
    frame = ExecutionFrame(
        line=5,
        line_content="PRINT X",
        statement_index=1,
        statement_total=3,
        variables={"X": 10},
        state=ExecutionState.RUNNING,
        error=None,
        stack_depth=0,
        timestamp=1234567890,
    )
    assert frame.line == 5
    assert frame.line_content == "PRINT X"
    assert frame.statement_index == 1
    assert frame.statement_total == 3
    assert frame.variables == {"X": 10}
    assert frame.state == ExecutionState.RUNNING
    assert frame.error is None
    assert frame.stack_depth == 0
    assert frame.timestamp == 1234567890


def test_execution_timeline_add_and_navigate():
    """Basic timeline add / navigate operations."""
    timeline = ExecutionTimeline()

    assert len(timeline.frames) == 0
    assert timeline.current_frame_index == -1
    assert timeline.is_at_end()

    frame1 = ExecutionFrame(line=1, line_content="LET X = 5")
    timeline.add_frame(frame1)

    assert len(timeline.frames) == 1
    assert timeline.current_frame_index == 0
    assert not timeline.is_at_end()

    frame2 = ExecutionFrame(line=2, line_content="PRINT X")
    timeline.add_frame(frame2)

    assert len(timeline.frames) == 2
    assert timeline.current_frame_index == 1

    timeline.go_to_frame(0)
    assert timeline.current_frame_index == 0
    assert timeline.get_current_frame() == frame1

    timeline.step_forward()
    assert timeline.current_frame_index == 1
    assert timeline.get_current_frame() == frame2

    timeline.step_backward()
    assert timeline.current_frame_index == 0

    timeline.clear()
    assert len(timeline.frames) == 0
    assert timeline.current_frame_index == -1


def test_timeline_with_error():
    """Timeline correctly stores error frames."""
    timeline = ExecutionTimeline()
    timeline.add_frame(ExecutionFrame(line=1, line_content="LET X = 5"))
    timeline.add_frame(
        ExecutionFrame(
            line=2,
            line_content="PRINT Y",
            state=ExecutionState.ERROR,
            error="NameError: name 'Y' is not defined",
        )
    )
    assert len(timeline.frames) == 2
    assert timeline.frames[1].state == ExecutionState.ERROR
    assert "NameError" in timeline.frames[1].error


def test_timeline_breakpoints():
    """Set / toggle / remove breakpoints on timeline."""
    tl = ExecutionTimeline()
    tl.set_breakpoint(10)
    tl.set_breakpoint(20)
    assert 10 in tl.breakpoints
    assert 20 in tl.breakpoints

    tl.toggle_breakpoint(10)
    assert 10 not in tl.breakpoints

    tl.remove_breakpoint(20)
    assert 20 not in tl.breakpoints


def test_timeline_navigation_bounds():
    """step_forward / step_backward respect list bounds."""
    tl = ExecutionTimeline()
    for n in range(3):
        tl.add_frame(ExecutionFrame(line=n + 1, line_content=f"LINE {n + 1}"))

    tl.go_to_frame(0)
    tl.step_backward()  # should be a no-op (already at start)
    assert tl.current_frame_index == 0

    tl.go_to_frame(2)
    tl.step_forward()  # no-op at end
    assert tl.current_frame_index == 2


def test_timeline_get_frames_around():
    """get_frames_around returns frames within context lines."""
    tl = ExecutionTimeline()
    for n in range(10):
        tl.add_frame(ExecutionFrame(line=n + 1, line_content="x"))
    nearby = tl.get_frames_around(5, context=2)
    lines = [f.line for f in nearby]
    assert all(abs(l - 5) <= 2 for l in lines)


def test_timeline_execution_flow():
    """get_execution_flow returns the ordered line sequence."""
    tl = ExecutionTimeline()
    for ln in [1, 2, 1, 3]:  # loop back to 1
        tl.add_frame(ExecutionFrame(line=ln, line_content="x"))
    assert tl.get_execution_flow() == [1, 2, 1, 3]


def test_timeline_export_json():
    """export_timeline_json produces a valid serialisable dict."""
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.record_frame(line=5, line_content="LET X=1", variables={"X": 1})
    tl.record_frame(line=6, line_content="PRINT X", variables={"X": 1})
    data = tl.export_timeline_json()

    assert data["total_frames"] == 2
    assert len(data["frames"]) == 2
    assert data["frames"][0]["line"] == 5
    assert data["frames"][0]["variables"]["X"] == "1"


def test_timeline_variable_history():
    """Variable history is tracked per-name across record_frame calls."""
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.record_frame(line=1, line_content="X=1", variables={"X": 1})
    tl.record_frame(line=2, line_content="X=2", variables={"X": 2})
    tl.record_frame(line=3, line_content="X=3", variables={"X": 3})

    hist = tl.get_variable_history("X")
    assert len(hist) == 3
    assert [s.value for s in hist] == [1, 2, 3]


# ---------------------------------------------------------------------------
# Integration tests — interpreter debug machinery
# ---------------------------------------------------------------------------

def _make_basic_interp(code: str):
    i = Interpreter(language=Language.BASIC)
    i.load_program(code, Language.BASIC)
    return i


def test_stop_while_paused_does_not_hang():
    """Bug fix: calling stop (running=False + debug_event.set) must unblock
    an interpreter thread that is waiting inside _do_debug_pause."""
    interp = _make_basic_interp("10 LET X=1\n20 PRINT X\n")
    interp.set_debug_mode(True)
    interp.step_mode = True

    paused = threading.Event()
    interp.set_debug_callback(lambda l, v: paused.set())  # Never resumes

    t = threading.Thread(
        target=lambda: interp.execute(TurtleState()), daemon=True
    )
    t.start()
    assert paused.wait(timeout=5), "interpreter should have paused"

    # Simulate stop_debug
    interp.running = False
    interp.debug_event.set()

    t.join(timeout=3)
    assert not t.is_alive(), "thread must exit after stop — was hanging before fix"


def test_debug_step_cycle_records_all_frames():
    """Stepping through every line produces one pause and one frame per line."""
    interp = _make_basic_interp(
        "10 LET X=1\n20 LET X=X+1\n30 PRINT X\n"
    )
    tl = ExecutionTimeline()
    tl.start_recording()
    interp.set_debug_mode(True)
    interp.set_debug_timeline(tl)
    interp.step_mode = True

    pauses = []
    def _step(line, variables):
        pauses.append(line)
        interp.step_execution()

    finished = _run_debug(interp, _step)

    assert finished, "step-through cycle must complete without hanging"
    assert pauses == [1, 2, 3], f"expected pauses at lines 1,2,3 but got {pauses}"
    assert len(tl.frames) == 3, f"expected 3 frames, got {len(tl.frames)}"
    assert interp.output == ["2"], f"output mismatch: {interp.output}"


def test_breakpoint_pauses_at_correct_physical_line():
    """A breakpoint set on physical editor line N pauses exactly once at N."""
    code = "10 LET X=1\n20 LET Y=2\n30 LET Z=X+Y\n40 PRINT Z\n"
    interp = _make_basic_interp(code)
    interp.set_debug_mode(True)
    interp.step_mode = False
    interp.add_breakpoint(3)  # physical line 3 == BASIC statement "30 LET Z=X+Y"

    hits = []
    def _on_bp(line, variables):
        hits.append(line)
        interp.resume_execution()

    finished = _run_debug(interp, _on_bp)

    assert finished, "breakpoint session must finish"
    assert hits == [3], f"expected single pause at line 3, got {hits}"
    assert interp.output == ["3"], f"output mismatch: {interp.output}"


def test_multiple_breakpoints_all_fire():
    """Multiple breakpoints all fire in order, and continue resumes between them."""
    code = "\n".join(f"{n * 10} PRINT {n}" for n in range(1, 6))
    interp = _make_basic_interp(code)
    interp.set_debug_mode(True)
    interp.step_mode = False
    interp.add_breakpoint(2)
    interp.add_breakpoint(4)

    hits = []
    def _on_bp(line, variables):
        hits.append(line)
        interp.resume_execution()

    finished = _run_debug(interp, _on_bp)

    assert finished, "multi-breakpoint session must finish"
    assert hits == [2, 4], f"expected pauses at 2 and 4, got {hits}"
    assert interp.output == ["1", "2", "3", "4", "5"]


def test_call_stack_property_exists_and_returns_list():
    """Interpreter.call_stack must exist and return a list for all languages."""
    for lang in (Language.BASIC, Language.PASCAL):
        i = Interpreter(language=lang)
        assert hasattr(i, "call_stack"), f"call_stack missing for {lang}"
        assert isinstance(i.call_stack, list), f"call_stack not a list for {lang}"


def test_python_debug_step_fires_per_source_line():
    """Python programs in debug mode should pause once per source line."""
    code = "x = 1\ny = x + 1\nz = y * 2\n"
    interp = Interpreter(language=Language.PYTHON)
    interp.load_program(code, Language.PYTHON)
    tl = ExecutionTimeline()
    tl.start_recording()
    interp.set_debug_mode(True)
    interp.set_debug_timeline(tl)
    interp.step_mode = True

    pauses = []
    def _step(line, variables):
        pauses.append(line)
        interp.step_execution()

    finished = _run_debug(interp, _step)

    assert finished, "Python debug must finish without hanging"
    assert len(pauses) >= 3, f"expected ≥3 pauses for 3 source lines (got {pauses})"
    assert len(tl.frames) >= 3, f"expected ≥3 timeline frames (got {len(tl.frames)})"


def test_python_debug_stop_while_paused_does_not_hang():
    """Stopping a Python debug session while paused must not leave the thread hung."""
    code = "x = 1\nx += 1\nx += 1\n"
    interp = Interpreter(language=Language.PYTHON)
    interp.load_program(code, Language.PYTHON)
    interp.set_debug_mode(True)
    interp.step_mode = True

    paused = threading.Event()
    interp.set_debug_callback(lambda l, v: paused.set())

    t = threading.Thread(
        target=lambda: interp.execute(TurtleState()), daemon=True
    )
    t.start()
    assert paused.wait(timeout=5), "Python debug should have paused"

    interp.running = False
    interp.debug_event.set()
    t.join(timeout=3)
    assert not t.is_alive(), "Python debug thread must exit after stop"


def test_whole_program_language_shows_observation_notice():
    """Non-steppable whole-program languages emit a debug observation notice."""
    interp = Interpreter(language=Language.LUA)
    interp.load_program('print("hello")', Language.LUA)
    interp.set_debug_mode(True)
    interp.execute(TurtleState())

    notice_found = any(
        "observation mode" in line.lower() or "not supported" in line.lower()
        for line in interp.output
    )
    assert notice_found, (
        "Expected a debug notice for Lua observation mode, "
        f"but output was: {interp.output}"
    )


def test_debug_resume_runs_to_completion():
    """After pausing at a breakpoint, resume_execution runs the rest of the program."""
    interp = _make_basic_interp(
        "10 LET X=1\n20 LET X=X+1\n30 LET X=X+1\n40 PRINT X\n"
    )
    interp.set_debug_mode(True)
    interp.step_mode = False
    interp.add_breakpoint(1)  # break at the first line only

    hits = []
    def _on_bp(line, variables):
        hits.append(line)
        interp.resume_execution()  # continue to end

    finished = _run_debug(interp, _on_bp)

    assert finished, "resume must run the program to completion"
    assert hits == [1], f"should pause once at line 1 (got {hits})"
    assert interp.output == ["3"], f"final X should be 3 (got {interp.output})"


def test_timeline_records_variable_changes():
    """Each frame snapshot captures the variables at that point in time."""
    interp = _make_basic_interp("10 LET X=5\n20 LET X=X+3\n30 PRINT X\n")
    tl = ExecutionTimeline()
    tl.start_recording()
    interp.set_debug_mode(True)
    interp.set_debug_timeline(tl)
    interp.step_mode = True

    def _step(l, v): interp.step_execution()

    finished = _run_debug(interp, _step)

    assert finished
    assert tl.get_execution_flow() == [1, 2, 3]
    hist = tl.get_variable_history("X")
    assert len(hist) >= 1
    # Frames are recorded AFTER execution, so the frame for line 2
    # (LET X=X+3) shows X=8 (5 + 3).
    frame_after_line2 = next(
        (f for f in tl.frames if f.line == 2), None
    )
    assert frame_after_line2 is not None
    assert frame_after_line2.variables.get("X") in (8, 8.0, "8", "8.0")

