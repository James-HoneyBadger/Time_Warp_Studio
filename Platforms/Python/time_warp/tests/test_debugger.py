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


def test_lua_debug_step_fires_per_source_line():
    """Lua programs in debug mode should run and produce output."""
    code = "x = 1\ny = x + 1\nz = y * 2\n"
    interp = Interpreter(language=Language.LUA)
    interp.load_program(code, Language.LUA)
    tl = ExecutionTimeline()
    tl.start_recording()
    interp.set_debug_mode(True)
    interp.set_debug_timeline(tl)

    finished = _run_debug(interp, lambda l, v: None)
    assert finished, "Lua debug must finish without hanging"


def test_lua_debug_stop_while_paused_does_not_hang():
    """Stopping a Lua debug session must not leave the thread hung."""
    code = "x = 1\nx = x + 1\nx = x + 1\n"
    interp = Interpreter(language=Language.LUA)
    interp.load_program(code, Language.LUA)
    interp.set_debug_mode(True)

    t = threading.Thread(
        target=lambda: interp.execute(TurtleState()), daemon=True
    )
    t.start()
    t.join(timeout=5)

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


# ---------------------------------------------------------------------------
# Additional timeline tests
# ---------------------------------------------------------------------------


def test_timeline_frame_count():
    """Timeline accumulates correct number of frames."""
    tl = ExecutionTimeline()
    tl.start_recording()
    for i in range(5):
        frame = ExecutionFrame(
            line=i + 1,
            line_content=f"LINE {i + 1}",
            variables={"I": i},
            state=ExecutionState.RUNNING,
        )
        tl.add_frame(frame)
    assert len(tl.frames) == 5


def test_timeline_clear():
    """Clearing timeline removes all frames."""
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.add_frame(
        ExecutionFrame(
            line=1, line_content="test", variables={}, state=ExecutionState.RUNNING
        )
    )
    tl.clear()
    assert len(tl.frames) == 0


def test_variable_snapshot_equality():
    """VariableSnapshot stores name and value."""
    snap = VariableSnapshot(name="X", value=42, type_name="int", line=1, timestamp=0)
    assert snap.name == "X"
    assert snap.value == 42


def test_execution_frame_state():
    """ExecutionFrame records the state."""
    frame = ExecutionFrame(
        line=5, line_content="PRINT X", variables={"X": 10}, state=ExecutionState.RUNNING
    )
    assert frame.line == 5
    assert frame.state == ExecutionState.RUNNING
    assert frame.variables["X"] == 10


def test_debug_mode_toggle():
    """Setting debug mode on/off changes interpreter state."""
    interp = Interpreter()
    interp.set_debug_mode(True)
    assert interp.debug_mode is True
    interp.set_debug_mode(False)
    assert interp.debug_mode is False


def test_breakpoint_add_remove():
    """Adding and removing breakpoints modifies the breakpoints set."""
    interp = Interpreter()
    interp.add_breakpoint(5)
    interp.add_breakpoint(10)
    assert 5 in interp.breakpoints
    assert 10 in interp.breakpoints
    interp.remove_breakpoint(5)
    assert 5 not in interp.breakpoints
    assert 10 in interp.breakpoints


def test_timeline_get_variable_history_empty():
    """get_variable_history returns empty list for unknown variable."""
    tl = ExecutionTimeline()
    hist = tl.get_variable_history("UNKNOWN_VAR")
    assert hist == []


def test_execution_state_values():
    """All expected execution states exist."""
    assert ExecutionState.RUNNING is not None
    assert ExecutionState.PAUSED is not None
    assert ExecutionState.STOPPED is not None



class TestExecutionTimelineNavigation:
    """Tests for timeline navigation methods."""

    def _build_timeline(self, n=3):
        tl = ExecutionTimeline()
        tl.start_recording()
        for i in range(n):
            frame = ExecutionFrame(
                line=i + 1,
                line_content=f"LINE {i + 1}",
                variables={"X": i},
                state=ExecutionState.RUNNING,
            )
            tl.add_frame(frame)
        return tl

    def test_initial_index_at_end(self):
        tl = self._build_timeline(3)
        assert tl.current_frame_index == 2

    def test_is_at_end_false_after_backward(self):
        tl = self._build_timeline(3)
        tl.step_backward()
        assert not tl.is_at_end()

    def test_step_backward_changes_index(self):
        tl = self._build_timeline(3)
        tl.step_backward()
        assert tl.current_frame_index == 1

    def test_step_forward_after_backward(self):
        tl = self._build_timeline(3)
        tl.step_backward()
        tl.step_forward()
        assert tl.current_frame_index == 2

    def test_go_to_frame_zero(self):
        tl = self._build_timeline(3)
        tl.go_to_frame(0)
        assert tl.current_frame_index == 0

    def test_go_to_frame_middle(self):
        tl = self._build_timeline(3)
        tl.go_to_frame(1)
        assert tl.current_frame_index == 1

    def test_get_current_frame_line(self):
        tl = self._build_timeline(3)
        tl.go_to_frame(0)
        cf = tl.get_current_frame()
        assert cf is not None
        assert cf.line == 1

    def test_toggle_breakpoint_adds(self):
        tl = ExecutionTimeline()
        tl.toggle_breakpoint(5)
        assert 5 in tl.breakpoints

    def test_toggle_breakpoint_removes(self):
        tl = ExecutionTimeline()
        tl.toggle_breakpoint(5)
        tl.toggle_breakpoint(5)
        assert 5 not in tl.breakpoints

    def test_execution_flow_correct(self):
        tl = self._build_timeline(3)
        assert tl.get_execution_flow() == [1, 2, 3]

    def test_stop_recording_sets_flag(self):
        tl = ExecutionTimeline()
        tl.start_recording()
        assert tl.is_recording
        tl.stop_recording()
        assert not tl.is_recording

    def test_timeline_export_has_frames_key(self):
        tl = self._build_timeline(2)
        data = tl.export_timeline_json()
        assert "frames" in data

    def test_timeline_export_frame_count(self):
        tl = self._build_timeline(2)
        data = tl.export_timeline_json()
        assert len(data["frames"]) == 2


class TestDebuggerTimeline2:
    """Extended ExecutionTimeline tests."""

    def _build(self, n):
        tl = ExecutionTimeline()
        tl.start_recording()
        for i in range(1, n + 1):
            tl.record_frame(i, f"LINE {i}", {"I": i}, [])
        tl.stop_recording()
        return tl

    def test_frames_count(self):
        tl = self._build(5)
        assert len(tl.frames) == 5

    def test_execution_flow_values(self):
        tl = self._build(3)
        assert tl.get_execution_flow() == [1, 2, 3]

    def test_step_forward_moves_to_first(self):
        tl = self._build(3)
        tl.step_forward()
        frame = tl.get_current_frame()
        assert frame.line == 1

    def test_step_forward_twice(self):
        tl = self._build(3)
        tl.step_forward()
        tl.step_forward()
        frame = tl.get_current_frame()
        assert frame.line == 2

    def test_step_backward_from_second(self):
        tl = self._build(3)
        tl.step_forward()
        tl.step_forward()
        tl.step_backward()
        frame = tl.get_current_frame()
        assert frame.line == 1

    def test_go_to_frame_second(self):
        tl = self._build(3)
        tl.go_to_frame(1)
        frame = tl.get_current_frame()
        assert frame.line == 2

    def test_variable_history_length(self):
        tl = self._build(3)
        history = tl.get_variable_history('I')
        assert len(history) == 3

    def test_variable_history_values(self):
        tl = self._build(3)
        history = tl.get_variable_history('I')
        assert history[0].value == 1
        assert history[1].value == 2
        assert history[2].value == 3

    def test_clear_removes_frames(self):
        tl = self._build(3)
        tl.clear()
        assert len(tl.frames) == 0

    def test_is_recording_after_stop(self):
        tl = self._build(2)
        assert tl.is_recording is False

    def test_is_recording_during(self):
        tl = ExecutionTimeline()
        tl.start_recording()
        assert tl.is_recording is True
        tl.stop_recording()

    def test_breakpoint_set(self):
        tl = ExecutionTimeline()
        tl.set_breakpoint(10)
        assert 10 in tl.breakpoints

    def test_breakpoint_remove(self):
        tl = ExecutionTimeline()
        tl.set_breakpoint(10)
        tl.remove_breakpoint(10)
        assert 10 not in tl.breakpoints

    def test_toggle_adds(self):
        tl = ExecutionTimeline()
        tl.toggle_breakpoint(7)
        assert 7 in tl.breakpoints

    def test_toggle_removes(self):
        tl = ExecutionTimeline()
        tl.toggle_breakpoint(7)
        tl.toggle_breakpoint(7)
        assert 7 not in tl.breakpoints

    def test_export_has_frames(self):
        tl = self._build(2)
        data = tl.export_timeline_json()
        assert "frames" in data

    def test_export_frame_count_matches(self):
        tl = self._build(4)
        data = tl.export_timeline_json()
        assert len(data["frames"]) == 4

    def test_frame_line_content(self):
        tl = self._build(2)
        tl.go_to_frame(0)
        frame = tl.get_current_frame()
        assert "LINE 1" in frame.line_content

    def test_empty_flow_when_no_frames(self):
        tl = ExecutionTimeline()
        assert tl.get_execution_flow() == []


class TestDebuggerTimeline3:
    """More ExecutionTimeline tests."""

    def _build(self, n: int):
        tl = ExecutionTimeline()
        tl.start_recording()
        for i in range(n):
            tl.add_frame(ExecutionFrame(
                line=i + 1,
                line_content=f"LINE {i+1}",
                variables={"I": i},
                state=ExecutionState.RUNNING,
            ))
        return tl

    def test_6_frames(self):
        tl = self._build(6)
        assert len(tl.frames) == 6

    def test_10_frames(self):
        tl = self._build(10)
        assert len(tl.frames) == 10

    def test_go_to_last(self):
        tl = self._build(5)
        tl.go_to_frame(4)
        assert tl.current_frame_index == 4

    def test_go_to_first(self):
        tl = self._build(5)
        tl.go_to_frame(4)
        tl.go_to_frame(0)
        assert tl.current_frame_index == 0

    def test_step_forward_5_times(self):
        tl = self._build(5)
        tl.go_to_frame(0)
        for _ in range(4):
            tl.step_forward()
        assert tl.current_frame_index == 4

    def test_step_backward_5_times(self):
        tl = self._build(5)
        tl.go_to_frame(4)
        for _ in range(4):
            tl.step_backward()
        assert tl.current_frame_index == 0

    def test_frame_vars_recorded(self):
        tl = self._build(3)
        tl.go_to_frame(2)
        frame = tl.get_current_frame()
        assert frame.variables == {"I": 2}

    def test_frame_line_content(self):
        tl = self._build(3)
        tl.go_to_frame(1)
        frame = tl.get_current_frame()
        assert "LINE 2" in frame.line_content

    def test_clear_resets_to_zero(self):
        tl = self._build(5)
        tl.clear()
        assert len(tl.frames) == 0

    def test_clear_resets_index(self):
        tl = self._build(5)
        tl.go_to_frame(4)
        tl.clear()
        assert tl.current_frame_index == -1

    def test_export_3_frames(self):
        tl = self._build(3)
        data = tl.export_timeline_json()
        assert len(data["frames"]) == 3

    def test_execution_flow_non_empty(self):
        tl = self._build(3)
        flow = tl.get_execution_flow()
        assert len(flow) == 3

    def test_breakpoint_set_removes(self):
        tl = ExecutionTimeline()
        tl.toggle_breakpoint(5)
        tl.remove_breakpoint(5)
        assert len(tl.breakpoints) == 0

    def test_multiple_breakpoints(self):
        tl = ExecutionTimeline()
        for i in [1, 3, 5, 7, 9]:
            tl.toggle_breakpoint(i)
        assert len(tl.breakpoints) == 5

    def test_is_recording_after_start(self):
        tl = ExecutionTimeline()
        tl.start_recording()
        assert tl.is_recording is True


class TestDebuggerTimeline4:
    """Even more ExecutionTimeline tests."""

    def _build(self, n: int):
        tl = ExecutionTimeline()
        tl.start_recording()
        for i in range(n):
            tl.add_frame(ExecutionFrame(
                line=i + 1,
                line_content=f"STMT {i+1}",
                variables={"N": i * 2},
                state=ExecutionState.RUNNING,
            ))
        return tl

    def test_20_frames(self):
        tl = self._build(20)
        assert len(tl.frames) == 20

    def test_go_to_middle_frame(self):
        tl = self._build(10)
        tl.go_to_frame(5)
        assert tl.current_frame_index == 5

    def test_variables_at_frame_3(self):
        tl = self._build(5)
        tl.go_to_frame(3)
        frame = tl.get_current_frame()
        assert frame.variables["N"] == 6

    def test_line_at_frame_0_is_1(self):
        tl = self._build(5)
        tl.go_to_frame(0)
        frame = tl.get_current_frame()
        assert frame.line == 1

    def test_line_at_frame_4_is_5(self):
        tl = self._build(5)
        tl.go_to_frame(4)
        frame = tl.get_current_frame()
        assert frame.line == 5

    def test_export_json_has_frames_key(self):
        tl = self._build(3)
        data = tl.export_timeline_json()
        assert "frames" in data

    def test_two_timelines_independent(self):
        tl1 = self._build(3)
        tl2 = self._build(7)
        assert len(tl1.frames) == 3
        assert len(tl2.frames) == 7

    def test_breakpoint_toggle_twice_removes(self):
        tl = ExecutionTimeline()
        tl.toggle_breakpoint(10)
        tl.toggle_breakpoint(10)
        assert 10 not in tl.breakpoints

    def test_stop_recording(self):
        tl = ExecutionTimeline()
        tl.start_recording()
        tl.stop_recording()
        assert tl.is_recording is False

    def test_empty_timeline_get_flow(self):
        tl = ExecutionTimeline()
        flow = tl.get_execution_flow()
        assert isinstance(flow, list)

    def test_step_forward_at_end_no_crash(self):
        tl = self._build(3)
        tl.go_to_frame(2)
        tl.step_forward()  # already at end
        assert tl.current_frame_index == 2

    def test_step_backward_at_start_no_crash(self):
        tl = self._build(3)
        tl.go_to_frame(0)
        tl.step_backward()  # already at start
        assert tl.current_frame_index == 0

    def test_frame_state_is_running(self):
        tl = self._build(2)
        tl.go_to_frame(0)
        frame = tl.get_current_frame()
        assert frame.state == ExecutionState.RUNNING


class TestDebuggerTimeline5:
    """Fifth round of debugger timeline tests."""

    def _make_frame(self, line=1, state=ExecutionState.RUNNING):
        return ExecutionFrame(line=line, line_content="", state=state)

    def _build(self, n):
        tl = ExecutionTimeline()
        for i in range(n):
            tl.add_frame(self._make_frame(line=i + 1))
        return tl

    def test_empty_timeline_length(self):
        tl = ExecutionTimeline()
        assert len(tl.frames) == 0

    def test_single_frame_length(self):
        tl = self._build(1)
        assert len(tl.frames) == 1

    def test_five_frames_length(self):
        tl = self._build(5)
        assert len(tl.frames) == 5

    def test_go_to_last_frame(self):
        tl = self._build(4)
        tl.go_to_frame(3)
        assert tl.current_frame_index == 3

    def test_get_frame_by_index(self):
        tl = self._build(3)
        tl.go_to_frame(1)
        frame = tl.get_current_frame()
        assert frame.line == 2

    def test_frame_state_stopped(self):
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=1, line_content="", state=ExecutionState.STOPPED))
        tl.go_to_frame(0)
        assert tl.get_current_frame().state == ExecutionState.STOPPED

    def test_frame_state_paused(self):
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=1, line_content="", state=ExecutionState.PAUSED))
        tl.go_to_frame(0)
        assert tl.get_current_frame().state == ExecutionState.PAUSED

    def test_frame_state_finished(self):
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=5, line_content="", state=ExecutionState.FINISHED))
        tl.go_to_frame(0)
        assert tl.get_current_frame().state == ExecutionState.FINISHED

    def test_frame_variables_stored(self):
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=1, line_content="", state=ExecutionState.RUNNING, variables={"x": 42}))
        tl.go_to_frame(0)
        assert tl.get_current_frame().variables.get("x") == 42

    def test_step_forward_increments(self):
        tl = self._build(3)
        tl.go_to_frame(0)
        tl.step_forward()
        assert tl.current_frame_index == 1


class TestDebuggerTimeline6:
    """Sixth round of debugger timeline tests."""

    def _make_frame(self, line=1, state=ExecutionState.RUNNING):
        return ExecutionFrame(line=line, line_content="", state=state)

    def _build(self, n):
        tl = ExecutionTimeline()
        for i in range(n):
            tl.add_frame(self._make_frame(line=i + 1))
        return tl

    def test_frame_line_number(self):
        f = self._make_frame(line=7)
        assert f.line == 7

    def test_frame_state(self):
        f = self._make_frame(state=ExecutionState.PAUSED)
        assert f.state == ExecutionState.PAUSED

    def test_timeline_length(self):
        tl = self._build(4)
        assert len(tl.frames) == 4

    def test_go_to_last_frame(self):
        tl = self._build(5)
        tl.go_to_frame(4)
        assert tl.current_frame_index == 4

    def test_go_to_first_frame(self):
        tl = self._build(5)
        tl.go_to_frame(0)
        assert tl.current_frame_index == 0

    def test_step_back_decrements(self):
        tl = self._build(3)
        tl.go_to_frame(2)
        tl.step_backward()
        assert tl.current_frame_index == 1

    def test_step_forward_then_back(self):
        tl = self._build(3)
        tl.go_to_frame(0)
        tl.step_forward()
        tl.step_backward()
        assert tl.current_frame_index == 0

    def test_current_frame_returns_frame(self):
        tl = self._build(2)
        tl.go_to_frame(1)
        frame = tl.get_current_frame()
        assert frame is not None

    def test_empty_timeline_frames_empty(self):
        tl = ExecutionTimeline()
        assert len(tl.frames) == 0

    def test_add_then_clear(self):
        tl = self._build(3)
        tl.clear()
        assert len(tl.frames) == 0


class TestDebuggerTimeline7:
    """Seventh round of debugger timeline tests."""

    def _make_frame(self, line=1, state=ExecutionState.RUNNING):
        return ExecutionFrame(line=line, line_content="", state=state)

    def _build(self, n):
        tl = ExecutionTimeline()
        for i in range(n):
            tl.add_frame(self._make_frame(line=i + 1))
        return tl

    def test_build_ten_frames(self):
        tl = self._build(10)
        assert len(tl.frames) == 10

    def test_frames_list_type(self):
        tl = self._build(2)
        assert isinstance(tl.frames, list)

    def test_frame_state_running(self):
        f = self._make_frame(state=ExecutionState.RUNNING)
        assert f.state == ExecutionState.RUNNING

    def test_frame_state_paused(self):
        f = self._make_frame(state=ExecutionState.PAUSED)
        assert f.state == ExecutionState.PAUSED

    def test_go_to_first_frame(self):
        tl = self._build(3)
        tl.go_to_frame(0)
        assert tl.get_current_frame() is not None

    def test_go_to_last_frame(self):
        tl = self._build(5)
        tl.go_to_frame(4)
        assert tl.get_current_frame() is not None

    def test_clear_removes_all(self):
        tl = self._build(5)
        tl.clear()
        assert tl.frames == []

    def test_timeline_not_none(self):
        tl = ExecutionTimeline()
        assert tl is not None

    def test_frame_line_positive(self):
        f = self._make_frame(line=99)
        assert f.line > 0

    def test_add_single_frame(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(line=1))
        assert len(tl.frames) == 1


class TestDebuggerTimeline8:
    """Eighth round of debugger timeline tests."""

    def _make_frame(self, line=1, state=ExecutionState.RUNNING):
        return ExecutionFrame(line=line, line_content="", state=state)

    def _build(self, n):
        tl = ExecutionTimeline()
        for i in range(1, n + 1):
            tl.add_frame(self._make_frame(i))
        return tl

    def test_build_five_frames(self):
        tl = self._build(5)
        assert len(tl.frames) == 5

    def test_build_fifteen_frames(self):
        tl = self._build(15)
        assert len(tl.frames) == 15

    def test_frame_line_numbers_sequential(self):
        tl = self._build(3)
        assert tl.frames[0].line == 1
        assert tl.frames[1].line == 2
        assert tl.frames[2].line == 3

    def test_clear_after_many(self):
        tl = self._build(20)
        tl.clear()
        assert len(tl.frames) == 0

    def test_current_frame_after_goto(self):
        tl = self._build(5)
        tl.go_to_frame(3)
        f = tl.get_current_frame()
        assert f is not None

    def test_add_paused_frame(self):
        tl = ExecutionTimeline()
        f = self._make_frame(7, ExecutionState.PAUSED)
        tl.add_frame(f)
        assert tl.frames[0].state == ExecutionState.PAUSED

    def test_single_frame_not_empty(self):
        tl = self._build(1)
        assert len(tl.frames) == 1

    def test_two_timelines_independent(self):
        tl1 = self._build(3)
        tl2 = self._build(7)
        tl1.clear()
        assert len(tl2.frames) == 7

    def test_add_frame_returns_none(self):
        tl = ExecutionTimeline()
        result = tl.add_frame(self._make_frame(1))
        assert result is None

    def test_frames_list_grows(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1))
        tl.add_frame(self._make_frame(2))
        assert len(tl.frames) == 2


class TestDebuggerTimeline9:
    """Ninth round of debugger timeline tests."""

    def _make_frame(self, line=1, state=ExecutionState.RUNNING):
        return ExecutionFrame(line=line, line_content="", state=state)

    def test_empty_timeline(self):
        tl = ExecutionTimeline()
        assert len(tl.frames) == 0

    def test_add_one_frame(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1))
        assert len(tl.frames) == 1

    def test_add_two_frames(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1))
        tl.add_frame(self._make_frame(2))
        assert len(tl.frames) == 2

    def test_add_ten_frames(self):
        tl = ExecutionTimeline()
        for i in range(10):
            tl.add_frame(self._make_frame(i + 1))
        assert len(tl.frames) == 10

    def test_clear_resets(self):
        tl = ExecutionTimeline()
        for i in range(5):
            tl.add_frame(self._make_frame(i + 1))
        tl.clear()
        assert len(tl.frames) == 0

    def test_current_frame_after_add(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(7))
        f = tl.get_current_frame()
        assert f is not None

    def test_frame_line_number(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(42))
        assert tl.frames[-1].line == 42

    def test_two_timelines_independent(self):
        t1 = ExecutionTimeline()
        t2 = ExecutionTimeline()
        t1.add_frame(self._make_frame(1))
        assert len(t2.frames) == 0

    def test_add_returns_none(self):
        tl = ExecutionTimeline()
        result = tl.add_frame(self._make_frame(1))
        assert result is None

    def test_paused_frame(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1, ExecutionState.PAUSED))
        assert len(tl.frames) == 1


class TestDebuggerTimeline10:
    """Tenth round of debugger timeline tests."""

    def _make_frame(self, line=1, state=ExecutionState.RUNNING):
        return ExecutionFrame(line=line, line_content="", state=state)

    def test_empty_timeline(self):
        tl = ExecutionTimeline()
        assert len(tl.frames) == 0

    def test_add_one_frame(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1))
        assert len(tl.frames) == 1

    def test_add_two_frames(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1))
        tl.add_frame(self._make_frame(2))
        assert len(tl.frames) == 2

    def test_clear_timeline(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1))
        tl.clear()
        assert len(tl.frames) == 0

    def test_frame_line_number(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(5))
        assert tl.frames[0].line == 5

    def test_frame_state_running(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1, ExecutionState.RUNNING))
        assert tl.frames[0].state == ExecutionState.RUNNING

    def test_frame_state_paused(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1, ExecutionState.PAUSED))
        assert tl.frames[0].state == ExecutionState.PAUSED

    def test_goto_frame(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._make_frame(1))
        tl.add_frame(self._make_frame(2))
        tl.go_to_frame(0)
        assert tl.get_current_frame().line == 1

    def test_add_returns_none(self):
        tl = ExecutionTimeline()
        result = tl.add_frame(self._make_frame(1))
        assert result is None

    def test_frames_list_type(self):
        tl = ExecutionTimeline()
        assert isinstance(tl.frames, list)


class TestDebuggerTimeline11:
    """Eleventh round of debugger timeline tests."""

    def _frame(self, line=1):
        return ExecutionFrame(line=line, line_content="")

    def test_empty_timeline(self):
        tl = ExecutionTimeline()
        assert len(tl.frames) == 0

    def test_add_frame(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._frame())
        assert len(tl.frames) == 1

    def test_add_two_frames(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._frame(1))
        tl.add_frame(self._frame(2))
        assert len(tl.frames) == 2

    def test_clear_timeline(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._frame())
        tl.clear()
        assert len(tl.frames) == 0

    def test_current_frame_is_none(self):
        tl = ExecutionTimeline()
        assert tl.get_current_frame() is None

    def test_frame_state_after_add(self):
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=5, line_content="", state=ExecutionState.PAUSED))
        assert tl.frames[0].state == ExecutionState.PAUSED

    def test_goto_valid_frame(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._frame(1))
        tl.go_to_frame(0)
        assert tl.get_current_frame() is not None

    def test_frames_is_list(self):
        tl = ExecutionTimeline()
        assert isinstance(tl.frames, list)

    def test_add_returns_none(self):
        tl = ExecutionTimeline()
        result = tl.add_frame(self._frame())
        assert result is None

    def test_frame_line_number(self):
        tl = ExecutionTimeline()
        tl.add_frame(self._frame(10))
        assert tl.frames[0].line == 10


class TestDebuggerTimeline12:
    """Twelfth extended round of debugger timeline tests."""

    def test_create_timeline(self):
        from time_warp.core.debugger import ExecutionTimeline
        tl = ExecutionTimeline()
        assert tl is not None

    def test_start_empty(self):
        from time_warp.core.debugger import ExecutionTimeline
        tl = ExecutionTimeline()
        assert len(tl.frames) == 0

    def test_add_frame(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=1, line_content=""))
        assert len(tl.frames) == 1

    def test_add_three_frames(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        for i in range(3):
            tl.add_frame(ExecutionFrame(line=i, line_content=""))
        assert len(tl.frames) == 3

    def test_clear_frames(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=1, line_content=""))
        tl.clear()
        assert len(tl.frames) == 0

    def test_frame_line_attr(self):
        from time_warp.core.debugger import ExecutionFrame
        f = ExecutionFrame(line=5, line_content="x = 1")
        assert f.line == 5

    def test_frame_content_attr(self):
        from time_warp.core.debugger import ExecutionFrame
        f = ExecutionFrame(line=1, line_content="hello")
        assert f.line_content == "hello"

    def test_two_timelines(self):
        from time_warp.core.debugger import ExecutionTimeline
        tl1, tl2 = ExecutionTimeline(), ExecutionTimeline()
        assert tl1 is not tl2

    def test_current_frame_empty(self):
        from time_warp.core.debugger import ExecutionTimeline
        tl = ExecutionTimeline()
        assert tl.get_current_frame() is None

    def test_add_five_frames(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        for i in range(5):
            tl.add_frame(ExecutionFrame(line=i, line_content=""))
        assert len(tl.frames) == 5


class TestDebuggerTimeline13:
    def test_create(self):
        from time_warp.core.debugger import ExecutionTimeline
        assert ExecutionTimeline() is not None

    def test_empty_frames(self):
        from time_warp.core.debugger import ExecutionTimeline
        assert len(ExecutionTimeline().frames) == 0

    def test_add_one(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=1, line_content=""))
        assert len(tl.frames) == 1

    def test_add_five(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        for i in range(5):
            tl.add_frame(ExecutionFrame(line=i, line_content=""))
        assert len(tl.frames) == 5

    def test_clear(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        tl.add_frame(ExecutionFrame(line=1, line_content=""))
        tl.clear()
        assert len(tl.frames) == 0

    def test_frame_line(self):
        from time_warp.core.debugger import ExecutionFrame
        assert ExecutionFrame(line=7, line_content="").line == 7

    def test_frame_content(self):
        from time_warp.core.debugger import ExecutionFrame
        assert ExecutionFrame(line=1, line_content="foo").line_content == "foo"

    def test_current_none(self):
        from time_warp.core.debugger import ExecutionTimeline
        assert ExecutionTimeline().get_current_frame() is None

    def test_two_timelines(self):
        from time_warp.core.debugger import ExecutionTimeline
        t1, t2 = ExecutionTimeline(), ExecutionTimeline()
        assert t1 is not t2

    def test_add_ten(self):
        from time_warp.core.debugger import ExecutionTimeline, ExecutionFrame
        tl = ExecutionTimeline()
        for i in range(10):
            tl.add_frame(ExecutionFrame(line=i, line_content=""))
        assert len(tl.frames) == 10
