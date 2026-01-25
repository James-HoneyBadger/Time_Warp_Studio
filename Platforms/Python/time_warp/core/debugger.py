"""Execution timeline debugger for stepping through code."""

import sys
import traceback
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable, Dict, List, Optional


class ExecutionState(Enum):
    """States during code execution."""

    STOPPED = "stopped"
    RUNNING = "running"
    PAUSED = "paused"
    STEP = "step"
    FINISHED = "finished"
    ERROR = "error"


@dataclass
class VariableSnapshot:
    """Snapshot of a variable at a point in time."""

    name: str
    value: Any
    type_name: str
    line: int
    timestamp: int


@dataclass
class ExecutionFrame:
    """A single step in code execution."""

    line: int
    line_content: str
    variables: Dict[str, Any] = field(default_factory=dict)
    state: ExecutionState = ExecutionState.RUNNING
    error: Optional[str] = None
    stack_depth: int = 0
    timestamp: int = 0


class ExecutionTimeline:
    """Records and replays code execution for debugging."""

    def __init__(self):
        """Initialize timeline recorder."""
        self.frames: List[ExecutionFrame] = []
        self.current_frame_index = 0
        self.is_recording = False
        self.breakpoints: set = set()  # Set of line numbers
        self.variables_history: Dict[str, List[VariableSnapshot]] = {}
        self.state = ExecutionState.STOPPED
        self.pause_requested = False
        self._callbacks: List[Callable] = []

    def start_recording(self):
        """Start recording execution frames."""
        self.frames.clear()
        self.current_frame_index = 0
        self.is_recording = True
        self.state = ExecutionState.RUNNING
        self.variables_history.clear()

    def stop_recording(self):
        """Stop recording execution frames."""
        self.is_recording = False
        self.state = ExecutionState.FINISHED
        self._trigger_callbacks("finished")

    def record_frame(
        self,
        line: int,
        line_content: str,
        variables: Dict[str, Any],
        stack_depth: int = 0,
    ):
        """Record a single execution frame."""
        if not self.is_recording:
            return

        frame = ExecutionFrame(
            line=line,
            line_content=line_content,
            variables=variables.copy(),
            state=ExecutionState.RUNNING,
            stack_depth=stack_depth,
            timestamp=len(self.frames),
        )

        self.frames.append(frame)

        # Update variable history
        for var_name, var_value in variables.items():
            if var_name not in self.variables_history:
                self.variables_history[var_name] = []

            snapshot = VariableSnapshot(
                name=var_name,
                value=var_value,
                type_name=type(var_value).__name__,
                line=line,
                timestamp=len(self.frames),
            )
            self.variables_history[var_name].append(snapshot)

        # Check for breakpoints
        if line in self.breakpoints:
            self.pause_at_line(line)

        self._trigger_callbacks("frame_recorded", frame)

    def record_error(self, line: int, error_msg: str):
        """Record an error frame."""
        frame = ExecutionFrame(
            line=line,
            line_content="",
            state=ExecutionState.ERROR,
            error=error_msg,
            timestamp=len(self.frames),
        )
        self.frames.append(frame)
        self.state = ExecutionState.ERROR
        self._trigger_callbacks("error", frame)

    def pause_at_line(self, line: int):
        """Pause execution at a specific line."""
        self.state = ExecutionState.PAUSED
        if self.frames:
            self.frames[-1].state = ExecutionState.PAUSED
        self._trigger_callbacks("paused", line)

    def resume(self):
        """Resume execution."""
        self.state = ExecutionState.RUNNING
        self.pause_requested = False
        self._trigger_callbacks("resumed")

    def step_forward(self):
        """Move to next frame."""
        if self.current_frame_index < len(self.frames) - 1:
            self.current_frame_index += 1
            self._trigger_callbacks("stepped", self.frames[self.current_frame_index])

    def step_backward(self):
        """Move to previous frame."""
        if self.current_frame_index > 0:
            self.current_frame_index -= 1
            self._trigger_callbacks("stepped", self.frames[self.current_frame_index])

    def set_breakpoint(self, line: int):
        """Set a breakpoint at a line."""
        self.breakpoints.add(line)
        self._trigger_callbacks("breakpoint_set", line)

    def remove_breakpoint(self, line: int):
        """Remove a breakpoint."""
        self.breakpoints.discard(line)
        self._trigger_callbacks("breakpoint_removed", line)

    def toggle_breakpoint(self, line: int):
        """Toggle a breakpoint."""
        if line in self.breakpoints:
            self.remove_breakpoint(line)
        else:
            self.set_breakpoint(line)

    def get_current_frame(self) -> Optional[ExecutionFrame]:
        """Get the current execution frame."""
        if 0 <= self.current_frame_index < len(self.frames):
            return self.frames[self.current_frame_index]
        return None

    def get_variable_at_frame(self, var_name: str, frame_index: int) -> Any:
        """Get variable value at a specific frame."""
        if frame_index < len(self.frames):
            return self.frames[frame_index].variables.get(var_name)
        return None

    def get_variable_history(self, var_name: str) -> List[VariableSnapshot]:
        """Get complete history of a variable."""
        return self.variables_history.get(var_name, [])

    def get_frames_around(self, line: int, context: int = 5) -> List[ExecutionFrame]:
        """Get frames around a specific line for context."""
        relevant_frames = [f for f in self.frames if abs(f.line - line) <= context]
        return relevant_frames

    def get_execution_flow(self) -> List[int]:
        """Get the sequence of lines executed (execution path)."""
        return [f.line for f in self.frames]

    def export_timeline_json(self) -> Dict[str, Any]:
        """Export timeline as JSON-serializable dict."""
        frames_data = []
        for frame in self.frames:
            frame_data = {
                "line": frame.line,
                "content": frame.line_content,
                "state": frame.state.value,
                "stack_depth": frame.stack_depth,
                "timestamp": frame.timestamp,
                "variables": {k: str(v) for k, v in frame.variables.items()},
            }
            if frame.error:
                frame_data["error"] = frame.error
            frames_data.append(frame_data)

        return {
            "total_frames": len(self.frames),
            "execution_state": self.state.value,
            "breakpoints": list(self.breakpoints),
            "frames": frames_data,
        }

    def on_event(self, callback: Callable):
        """Register callback for timeline events."""
        self._callbacks.append(callback)
        return callback

    def _trigger_callbacks(self, event_type: str, *args):
        """Trigger all registered callbacks."""
        for callback in self._callbacks:
            try:
                callback(event_type, *args)
            except Exception as e:
                print(f"Callback error: {e}")


class DebuggerTracer:
    """Hooks into Python's execution to trace code."""

    def __init__(self, timeline: ExecutionTimeline):
        """Initialize tracer."""
        self.timeline = timeline
        self.current_locals = {}
        self.in_user_code = True

    def trace_calls(self, frame, event, arg):
        """Trace function for sys.settrace()."""
        # Only trace user code, not library code
        if "site-packages" in frame.f_code.co_filename:
            return None

        if event == "line":
            # Record line execution
            line_no = frame.f_lineno
            code = frame.f_code.co_name
            variables = {
                k: v for k, v in frame.f_locals.items() if not k.startswith("_")
            }

            self.timeline.record_frame(
                line=line_no,
                line_content=f"in {code}",
                variables=variables,
                stack_depth=len(traceback.extract_stack()),
            )

            return self.trace_calls

        elif event == "exception":
            # Record exceptions
            exc_type, exc_value, exc_tb = arg
            self.timeline.record_error(
                line=frame.f_lineno,
                error_msg=f"{exc_type.__name__}: {exc_value}",
            )
            return self.trace_calls

        return self.trace_calls

    def start(self):
        """Start tracing."""
        sys.settrace(self.trace_calls)

    def stop(self):
        """Stop tracing."""
        sys.settrace(None)


class CodeDebugger:
    """High-level debugger interface."""

    def __init__(self):
        """Initialize debugger."""
        self.timeline = ExecutionTimeline()
        self.tracer = DebuggerTracer(self.timeline)

    def debug_code(self, code_string: str, globals_dict: Optional[Dict] = None):
        """Execute code with debugging enabled."""
        self.timeline.start_recording()
        self.tracer.start()

        try:
            exec_globals = globals_dict or {}
            exec(code_string, exec_globals)
        except Exception as e:
            # Record the error
            self.timeline.record_error(line=1, error_msg=str(e))
        finally:
            self.tracer.stop()
            self.timeline.stop_recording()

    def get_timeline(self) -> ExecutionTimeline:
        """Get the execution timeline."""
        return self.timeline

    def add_breakpoint(self, line: int):
        """Add a breakpoint."""
        self.timeline.set_breakpoint(line)

    def remove_breakpoint(self, line: int):
        """Remove a breakpoint."""
        self.timeline.remove_breakpoint(line)

    def get_report(self) -> str:
        """Generate a debug report."""
        report = []
        report.append("=" * 60)
        report.append("EXECUTION TIMELINE REPORT")
        report.append("=" * 60)
        report.append(f"Total frames: {len(self.timeline.frames)}")
        report.append(f"Final state: {self.timeline.state.value}")
        report.append("")

        if self.timeline.frames:
            report.append("EXECUTION FLOW:")
            for i, frame in enumerate(self.timeline.frames[:20]):  # Show first 20
                report.append(f"  {i}: Line {frame.line} - {frame.state.value}")
                if frame.variables:
                    for var, val in list(frame.variables.items())[
                        :3
                    ]:  # Show first 3 vars
                        report.append(f"     {var} = {val}")

        return "\n".join(report)
