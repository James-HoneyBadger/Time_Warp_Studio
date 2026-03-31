"""Tests for time_warp.core.debugger module."""

import time

from time_warp.core.debugger import (
    ExecutionFrame,
    ExecutionState,
    ExecutionTimeline,
    VariableSnapshot,
)


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


def test_execution_timeline():
    """Test ExecutionTimeline class."""
    timeline = ExecutionTimeline()

    # Initially empty
    assert len(timeline.frames) == 0
    assert timeline.current_frame_index == -1
    assert timeline.is_at_end()

    # Add a frame
    frame = ExecutionFrame(line=1, line_content="LET X = 5")
    timeline.add_frame(frame)

    assert len(timeline.frames) == 1
    assert timeline.current_frame_index == 0
    assert not timeline.is_at_end()

    # Add another frame
    frame2 = ExecutionFrame(line=2, line_content="PRINT X")
    timeline.add_frame(frame2)

    assert len(timeline.frames) == 2
    assert timeline.current_frame_index == 1

    # Test navigation
    timeline.go_to_frame(0)
    assert timeline.current_frame_index == 0
    assert timeline.get_current_frame() == frame

    timeline.step_forward()
    assert timeline.current_frame_index == 1
    assert timeline.get_current_frame() == frame2

    timeline.step_backward()
    assert timeline.current_frame_index == 0

    # Test clear
    timeline.clear()
    assert len(timeline.frames) == 0
    assert timeline.current_frame_index == -1


def test_timeline_with_error():
    """Test timeline with error frames."""
    timeline = ExecutionTimeline()

    # Normal frame
    frame1 = ExecutionFrame(line=1, line_content="LET X = 5")
    timeline.add_frame(frame1)

    # Error frame
    frame2 = ExecutionFrame(
        line=2,
        line_content="PRINT Y",
        state=ExecutionState.ERROR,
        error="NameError: name 'Y' is not defined",
    )
    timeline.add_frame(frame2)

    assert len(timeline.frames) == 2
    assert timeline.frames[1].state == ExecutionState.ERROR
    assert "NameError" in timeline.frames[1].error