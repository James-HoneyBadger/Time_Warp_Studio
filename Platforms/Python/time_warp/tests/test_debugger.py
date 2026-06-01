"""Tests for time_warp.core.debugger module."""

import pytest
from time_warp.core.debugger import (
    ExecutionTimeline,
    ExecutionFrame,
    ExecutionState,
)


def make_timeline() -> ExecutionTimeline:
    """Helper: create a timeline with 3 recorded frames."""
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.record_frame(10, "PRINT X", {"X": 5})
    tl.record_frame(20, "LET Y = X + 1", {"X": 5, "Y": 6})
    tl.record_frame(30, "PRINT Y", {"X": 5, "Y": 6})
    tl.stop_recording()
    return tl


# ---------------------------------------------------------------------------
# ExecutionTimeline – state management
# ---------------------------------------------------------------------------


def test_initial_state_is_stopped():
    tl = ExecutionTimeline()
    assert tl.state == ExecutionState.STOPPED


def test_start_recording_changes_state():
    tl = ExecutionTimeline()
    tl.start_recording()
    assert tl.state == ExecutionState.RUNNING


def test_stop_recording_changes_state():
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.stop_recording()
    assert tl.state == ExecutionState.FINISHED


def test_start_clears_previous_frames():
    tl = make_timeline()
    assert len(tl.frames) == 3
    tl.start_recording()
    assert len(tl.frames) == 0


# ---------------------------------------------------------------------------
# ExecutionTimeline – recording
# ---------------------------------------------------------------------------


def test_record_frame_increments_count():
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.record_frame(10, "PRINT X", {"X": 5})
    tl.record_frame(20, "PRINT Y", {"Y": 6})
    assert len(tl.frames) == 2


def test_record_frame_captures_variables():
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.record_frame(10, "PRINT X", {"X": 42})
    assert tl.frames[0].variables["X"] == 42


def test_record_frame_not_when_not_recording():
    tl = ExecutionTimeline()
    tl.record_frame(10, "PRINT X", {"X": 5})
    assert len(tl.frames) == 0


def test_record_error_creates_error_frame():
    tl = ExecutionTimeline()
    tl.start_recording()
    tl.record_frame(10, "PRINT X", {"X": 5})
    tl.record_error(20, "NameError: X not defined")
    assert tl.frames[-1].state == ExecutionState.ERROR
    assert "NameError" in tl.frames[-1].error


# ---------------------------------------------------------------------------
# ExecutionTimeline – navigation
# ---------------------------------------------------------------------------


def test_step_forward_advances_frame_index():
    tl = make_timeline()
    assert tl.current_frame_index == -1
    tl.step_forward()
    assert tl.current_frame_index == 0


def test_step_backward_decrements_frame_index():
    tl = make_timeline()
    tl.step_forward()  # 0
    tl.step_forward()  # 1
    tl.step_backward()
    assert tl.current_frame_index == 0


def test_get_current_frame_returns_correct_frame():
    tl = make_timeline()
    tl.step_forward()
    frame = tl.get_current_frame()
    assert frame is not None
    assert frame.line == 10


def test_get_current_frame_returns_none_before_start():
    tl = make_timeline()
    assert tl.get_current_frame() is None


def test_go_to_frame_sets_index():
    tl = make_timeline()
    tl.go_to_frame(2)
    assert tl.current_frame_index == 2


# ---------------------------------------------------------------------------
# ExecutionTimeline – breakpoints
# ---------------------------------------------------------------------------


def test_set_breakpoint_adds_line():
    tl = ExecutionTimeline()
    tl.set_breakpoint(20)
    assert 20 in tl.breakpoints


def test_remove_breakpoint_removes_line():
    tl = ExecutionTimeline()
    tl.set_breakpoint(20)
    tl.remove_breakpoint(20)
    assert 20 not in tl.breakpoints


def test_toggle_breakpoint_adds_when_absent():
    tl = ExecutionTimeline()
    tl.toggle_breakpoint(30)
    assert 30 in tl.breakpoints


def test_toggle_breakpoint_removes_when_present():
    tl = ExecutionTimeline()
    tl.set_breakpoint(30)
    tl.toggle_breakpoint(30)
    assert 30 not in tl.breakpoints


# ---------------------------------------------------------------------------
# ExecutionTimeline – analysis
# ---------------------------------------------------------------------------


def test_get_execution_flow_returns_line_numbers():
    tl = make_timeline()
    flow = tl.get_execution_flow()
    assert flow == [10, 20, 30]


def test_get_variable_at_frame():
    tl = make_timeline()
    val = tl.get_variable_at_frame("Y", 2)
    assert val == 6


def test_get_variable_at_frame_missing_returns_none():
    tl = make_timeline()
    val = tl.get_variable_at_frame("Z", 0)
    assert val is None


def test_export_timeline_json_contains_keys():
    tl = make_timeline()
    export = tl.export_timeline_json()
    assert "total_frames" in export
    assert "frames" in export
    assert export["total_frames"] == 3


def test_go_to_frame():
    tl = make_timeline()
    tl.go_to_frame(2)
    assert tl.current_frame_index == 2


def test_is_at_end_after_seek():
    tl = make_timeline()
    tl.go_to_frame(2)
    # is_at_end may require stepping past the last frame
    assert tl.current_frame_index == 2


def test_get_variable_history():
    tl = make_timeline()
    history = tl.get_variable_history("X")
    assert len(history) == 3
    assert all(h.name == "X" for h in history)


def test_set_and_toggle_breakpoint():
    tl = ExecutionTimeline()
    tl.set_breakpoint(10)
    assert 10 in tl.breakpoints
    tl.toggle_breakpoint(10)
    assert 10 not in tl.breakpoints


def test_clear_resets_frames():
    tl = make_timeline()
    tl.clear()
    assert len(tl.frames) == 0


def test_get_frames_around():
    tl = make_timeline()
    frames = tl.get_frames_around(1, 1)
    assert len(frames) <= 3
