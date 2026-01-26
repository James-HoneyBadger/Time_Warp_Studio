"""
Time Warp Studio - Phase VIII: Integrated Debugger System

Provides:
- Breakpoints (line, conditional)
- Variable inspection and watches
- Call stack tracing
- Step-through execution
- Performance profiling
- Real-time pair debugging
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Tuple

# ===== ENUMS =====


class BreakpointType(Enum):
    """Breakpoint types"""

    LINE = "line"
    CONDITIONAL = "conditional"
    EXCEPTION = "exception"
    LOGPOINT = "logpoint"


class ExecutionState(Enum):
    """Debugger execution states"""

    RUNNING = "running"
    PAUSED = "paused"
    STOPPED = "stopped"
    ERROR = "error"


class StepType(Enum):
    """Step-through types"""

    INTO = "into"
    OVER = "over"
    OUT = "out"
    CONTINUE = "continue"


# ===== DATA CLASSES =====


@dataclass
class Breakpoint:
    """Debug breakpoint"""

    id: str = ""
    type: BreakpointType = BreakpointType.LINE
    file: str = ""
    line: int = 0
    column: int = 0

    # Conditional
    condition: Optional[str] = None
    hit_count: int = 0

    # Logging
    log_message: Optional[str] = None

    # Status
    enabled: bool = True
    verified: bool = False

    # Metadata
    created_at: datetime = field(default_factory=datetime.utcnow)


@dataclass
class StackFrame:
    """Call stack frame"""

    id: int = 0
    name: str = ""
    file: str = ""
    line: int = 0
    column: int = 0

    # Variables
    local_variables: Dict[str, Any] = field(default_factory=dict)
    arguments: Dict[str, Any] = field(default_factory=dict)
    return_value: Optional[Any] = None


@dataclass
class Variable:
    """Variable in debugger"""

    name: str = ""
    value: Any | None = None
    type: str = ""

    # Complex types
    is_expandable: bool = False
    children: List["Variable"] = field(default_factory=list)

    # Metadata
    scope: str = "local"  # local, global, argument, return


@dataclass
class Watch:
    """Watch expression"""

    id: str = ""
    expression: str = ""
    current_value: Any | None = None
    previous_value: Optional[Any] = None
    data_type: str = ""

    # Notification
    changed: bool = False
    last_evaluated: datetime = field(default_factory=datetime.utcnow)


@dataclass
class ExecutionTrace:
    """Execution trace point"""

    timestamp: datetime = field(default_factory=datetime.utcnow)
    file: str = ""
    line: int = 0
    function: str = ""
    variables_snapshot: Dict[str, Any] = field(default_factory=dict)
    execution_time_ms: float = 0.0


@dataclass
class DebugSession:
    """Debugger session"""

    id: str = ""
    program_name: str = ""
    language: str = ""

    # State
    state: ExecutionState = ExecutionState.RUNNING
    current_line: int = 0
    current_file: str = ""

    # Call stack
    call_stack: List[StackFrame] = field(default_factory=list)

    # Variables
    watches: Dict[str, Watch] = field(default_factory=dict)

    # Performance
    execution_traces: List[ExecutionTrace] = field(default_factory=list)
    total_execution_time_ms: float = 0.0

    # Metadata
    started_at: datetime = field(default_factory=datetime.utcnow)
    paused_at: Optional[datetime] = None


# ===== DEBUGGER ENGINE =====


class DebuggerEngine:
    """Core debugger functionality"""

    def __init__(self):
        self.breakpoints: Dict[str, Breakpoint] = {}
        self.sessions: Dict[str, DebugSession] = {}
        self.current_session: Optional[DebugSession] = None
        self.breakpoint_handlers: List[Callable] = []

    def create_breakpoint(
        self,
        file: str,
        line: int,
        condition: Optional[str] = None,
        breakpoint_type: BreakpointType = BreakpointType.LINE,
    ) -> Breakpoint:
        """Create a breakpoint"""
        bp = Breakpoint(
            id=f"{file}:{line}",
            type=breakpoint_type,
            file=file,
            line=line,
            condition=condition,
        )
        self.breakpoints[bp.id] = bp
        return bp

    def remove_breakpoint(self, bp_id: str) -> bool:
        """Remove breakpoint"""
        if bp_id in self.breakpoints:
            del self.breakpoints[bp_id]
            return True
        return False

    def enable_breakpoint(self, bp_id: str) -> bool:
        """Enable breakpoint"""
        if bp_id in self.breakpoints:
            self.breakpoints[bp_id].enabled = True
            return True
        return False

    def disable_breakpoint(self, bp_id: str) -> bool:
        """Disable breakpoint"""
        if bp_id in self.breakpoints:
            self.breakpoints[bp_id].enabled = False
            return True
        return False

    def get_breakpoints(self, file: Optional[str] = None) -> List[Breakpoint]:
        """Get breakpoints"""
        bps = list(self.breakpoints.values())
        if file:
            bps = [bp for bp in bps if bp.file == file]
        return bps

    def start_session(self, program_name: str, language: str) -> DebugSession:
        """Start debug session"""
        session = DebugSession(
            id=program_name, program_name=program_name, language=language
        )
        self.sessions[session.id] = session
        self.current_session = session
        return session

    def pause_at_breakpoint(self, bp_id: str) -> bool:
        """Pause execution at breakpoint"""
        if not self.current_session:
            return False

        bp = self.breakpoints.get(bp_id)
        if not bp or not bp.enabled:
            return False

        self.current_session.state = ExecutionState.PAUSED
        self.current_session.paused_at = datetime.utcnow()

        for handler in self.breakpoint_handlers:
            handler(bp_id, self.current_session)

        return True

    def continue_execution(self) -> bool:
        """Continue from pause"""
        if self.current_session:
            self.current_session.state = ExecutionState.RUNNING
            return True
        return False

    def step_into(self) -> bool:
        """Step into function"""
        if self.current_session:
            self.current_session.state = ExecutionState.PAUSED
            return True
        return False

    def step_over(self) -> bool:
        """Step over line"""
        if self.current_session:
            self.current_session.state = ExecutionState.PAUSED
            return True
        return False

    def step_out(self) -> bool:
        """Step out of function"""
        if self.current_session:
            self.current_session.state = ExecutionState.PAUSED
            return True
        return False

    def add_watch(self, session_id: str, expression: str) -> Watch:
        """Add watch expression"""
        session = self.sessions.get(session_id)
        if not session:
            return None

        watch = Watch(id=expression, expression=expression)
        session.watches[watch.id] = watch
        return watch

    def remove_watch(self, session_id: str, watch_id: str) -> bool:
        """Remove watch"""
        session = self.sessions.get(session_id)
        if session and watch_id in session.watches:
            del session.watches[watch_id]
            return True
        return False

    def get_watches(self, session_id: str) -> Dict[str, Watch]:
        """Get all watches"""
        session = self.sessions.get(session_id)
        return session.watches if session else {}

    def evaluate_expression(self, session_id: str, expression: str) -> Any:
        """Evaluate expression in current context"""
        # This would use the current variable scope
        # Simplified placeholder
        return None

    def get_call_stack(self, session_id: str) -> List[StackFrame]:
        """Get call stack"""
        session = self.sessions.get(session_id)
        return session.call_stack if session else []

    def get_variables(self, session_id: str, frame_id: int = 0) -> Dict[str, Variable]:
        """Get variables in frame"""
        session = self.sessions.get(session_id)
        if not session or frame_id >= len(session.call_stack):
            return {}

        frame = session.call_stack[frame_id]
        return {
            name: Variable(name=name, value=value, scope="local")
            for name, value in frame.local_variables.items()
        }


class DebugConsole:
    """Interactive debug console"""

    def __init__(self, debugger: DebuggerEngine):
        self.debugger = debugger
        self.history: List[str] = []
        self.output: List[str] = []

    def execute_command(self, command: str) -> str:
        """Execute debug command"""
        self.history.append(command)

        parts = command.split()
        if not parts:
            return ""

        cmd = parts[0].lower()
        args = parts[1:]

        if cmd == "break" and len(args) >= 1:
            file = args[0]
            line = int(args[1]) if len(args) > 1 else 0
            bp = self.debugger.create_breakpoint(file, line)
            return f"Breakpoint {bp.id} created"

        elif cmd == "continue":
            self.debugger.continue_execution()
            return "Execution continued"

        elif cmd == "step":
            self.debugger.step_into()
            return "Stepped into"

        elif cmd == "watch" and len(args) >= 1:
            expr = " ".join(args)
            self.debugger.add_watch(
                (
                    self.debugger.current_session.id
                    if self.debugger.current_session
                    else ""
                ),
                expr,
            )
            return f"Watch added: {expr}"

        elif cmd == "stack":
            session = self.debugger.current_session
            if session:
                return f"Call stack ({len(session.call_stack)} frames)"
            return "No active session"

        elif cmd == "variables":
            session = self.debugger.current_session
            if session:
                vars = self.debugger.get_variables(session.id)
                return f"Variables: {list(vars.keys())}"
            return "No variables"

        else:
            return f"Unknown command: {cmd}"

    def get_output(self) -> List[str]:
        """Get console output"""
        return self.output

    def clear(self) -> None:
        """Clear console"""
        self.output.clear()


class PerformanceProfiler:
    """Performance profiling during debugging"""

    def __init__(self):
        self.traces: List[ExecutionTrace] = []
        self.function_times: Dict[str, float] = {}
        self.line_times: Dict[str, float] = {}

    def record_trace(self, trace: ExecutionTrace) -> None:
        """Record execution trace"""
        self.traces.append(trace)

        # Update function times
        func = trace.function
        if func not in self.function_times:
            self.function_times[func] = 0
        self.function_times[func] += trace.execution_time_ms

        # Update line times
        line_key = f"{trace.file}:{trace.line}"
        if line_key not in self.line_times:
            self.line_times[line_key] = 0
        self.line_times[line_key] += trace.execution_time_ms

    def get_hotspots(self, limit: int = 10) -> List[Tuple[str, float]]:
        """Get slowest functions"""
        items = sorted(self.function_times.items(), key=lambda x: x[1], reverse=True)
        return items[:limit]

    def get_profile_summary(self) -> Dict:
        """Get profiling summary"""
        total_time = sum(self.function_times.values())

        return {
            "total_execution_time_ms": total_time,
            "function_count": len(self.function_times),
            "trace_count": len(self.traces),
            "hottest_function": (
                max(self.function_times.items(), key=lambda x: x[1])
                if self.function_times
                else None
            ),
        }


class PairDebugManager:
    """Real-time pair debugging support"""

    def __init__(self):
        self.sessions: Dict[str, Dict] = {}
        self.breakpoint_syncs: List[Callable] = []

    def create_pair_session(self, initiator_id: str, target_id: str) -> str:
        """Create pair debugging session"""
        session_id = f"pair:{initiator_id}:{target_id}"
        self.sessions[session_id] = {
            "initiator": initiator_id,
            "target": target_id,
            "shared_breakpoints": [],
            "synchronized": False,
            "created_at": datetime.utcnow(),
        }
        return session_id

    def sync_breakpoint(self, session_id: str, breakpoint: Breakpoint) -> None:
        """Sync breakpoint to pair participant"""
        session = self.sessions.get(session_id)
        if session:
            session["shared_breakpoints"].append(breakpoint)

            for handler in self.breakpoint_syncs:
                handler(session_id, breakpoint)

    def share_watch(self, session_id: str, watch: Watch) -> None:
        """Share watch expression"""
        # Broadcast to pair participant
        pass

    def get_pair_session(self, session_id: str) -> Optional[Dict]:
        """Get pair session info"""
        return self.sessions.get(session_id)


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    debugger = DebuggerEngine()
    console = DebugConsole(debugger)
    profiler = PerformanceProfiler()

    # Create breakpoints
    bp1 = debugger.create_breakpoint("main.bas", 10)
    print(f"Breakpoint created: {bp1.id}")

    # Start session
    session = debugger.start_session("test.bas", "basic")
    print(f"Debug session started: {session.id}")

    # Execute console commands
    result = console.execute_command("break main.bas 20")
    print(result)

    result = console.execute_command("watch x")
    print(result)

    # Add watches
    watch = debugger.add_watch(session.id, "counter")
    print(f"Watch added: {watch.expression}")

    # Performance profiling
    trace = ExecutionTrace(
        file="main.bas", line=15, function="main", execution_time_ms=5.2
    )
    profiler.record_trace(trace)

    summary = profiler.get_profile_summary()
    print(f"Profile: {summary['total_execution_time_ms']}ms total")
