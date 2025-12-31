"""
Performance profiler for Time Warp IDE.
Tracks execution time per line.
"""

import time
from dataclasses import dataclass, field
from typing import Dict, List, Optional


@dataclass
class LineProfile:
    """Profile data for a single line."""

    line_number: int
    execution_count: int = 0
    total_time_ms: float = 0.0
    min_time_ms: float = float("inf")
    max_time_ms: float = 0.0
    last_time_ms: float = 0.0

    @property
    def avg_time_ms(self) -> float:
        """Average execution time in milliseconds."""
        if self.execution_count == 0:
            return 0.0
        return self.total_time_ms / self.execution_count


@dataclass
class ProfileSession:
    """A profiling session for a program run."""

    start_time: float = 0.0
    end_time: float = 0.0
    line_profiles: Dict[int, LineProfile] = field(default_factory=dict)
    execution_order: List[int] = field(default_factory=list)
    total_lines_executed: int = 0

    @property
    def total_time_ms(self) -> float:
        """Total execution time in milliseconds."""
        return (self.end_time - self.start_time) * 1000

    def get_hotspots(self, top_n: int = 10) -> List[LineProfile]:
        """Get the lines that consumed the most time."""
        sorted_profiles = sorted(
            self.line_profiles.values(),
            key=lambda p: p.total_time_ms,
            reverse=True,
        )
        return sorted_profiles[:top_n]

    def get_most_executed(self, top_n: int = 10) -> List[LineProfile]:
        """Get the lines executed most frequently."""
        sorted_profiles = sorted(
            self.line_profiles.values(),
            key=lambda p: p.execution_count,
            reverse=True,
        )
        return sorted_profiles[:top_n]


class PerformanceProfiler:
    """Tracks execution time per line for BASIC/Logo/PILOT programs."""

    def __init__(self):
        self.enabled = False
        self.current_session: Optional[ProfileSession] = None
        self.sessions: List[ProfileSession] = []
        self._line_start_time: float = 0.0
        self._current_line: int = 0

    def start_session(self):
        """Start a new profiling session."""
        self.current_session = ProfileSession()
        self.current_session.start_time = time.perf_counter()
        self.enabled = True

    def end_session(self) -> Optional[ProfileSession]:
        """End the current profiling session."""
        if self.current_session is None:
            return None

        self.current_session.end_time = time.perf_counter()
        session = self.current_session
        self.sessions.append(session)
        self.current_session = None
        self.enabled = False
        return session

    def start_line(self, line_number: int):
        """Mark the start of executing a line."""
        if not self.enabled or self.current_session is None:
            return

        self._current_line = line_number
        self._line_start_time = time.perf_counter()

    def end_line(self):
        """Mark the end of executing the current line."""
        if not self.enabled or self.current_session is None:
            return

        end_time = time.perf_counter()
        elapsed_ms = (end_time - self._line_start_time) * 1000

        line_num = self._current_line
        session = self.current_session

        # Get or create line profile
        if line_num not in session.line_profiles:
            session.line_profiles[line_num] = LineProfile(line_number=line_num)

        profile = session.line_profiles[line_num]
        profile.execution_count += 1
        profile.total_time_ms += elapsed_ms
        profile.last_time_ms = elapsed_ms
        profile.min_time_ms = min(profile.min_time_ms, elapsed_ms)
        profile.max_time_ms = max(profile.max_time_ms, elapsed_ms)

        session.execution_order.append(line_num)
        session.total_lines_executed += 1

    def get_report(self, session: Optional[ProfileSession] = None) -> str:
        """Generate a text report for a profiling session."""
        if session is None:
            session = self.sessions[-1] if self.sessions else None

        if session is None:
            return "No profiling data available."

        lines = []
        lines.append("=" * 60)
        lines.append("PERFORMANCE PROFILE REPORT")
        lines.append("=" * 60)
        lines.append("")
        lines.append(f"Total execution time: {session.total_time_ms:.2f} ms")
        lines.append(f"Total lines executed: {session.total_lines_executed}")
        lines.append(f"Unique lines: {len(session.line_profiles)}")
        lines.append("")

        # Hotspots
        lines.append("-" * 60)
        lines.append("TOP 10 HOTSPOTS (by total time)")
        lines.append("-" * 60)
        lines.append(
            f"{'Line':>6} {'Count':>8} {'Total(ms)':>12} "
            f"{'Avg(ms)':>10} {'Max(ms)':>10}"
        )
        lines.append("-" * 60)

        for profile in session.get_hotspots(10):
            lines.append(
                f"{profile.line_number:>6} {profile.execution_count:>8} "
                f"{profile.total_time_ms:>12.3f} {profile.avg_time_ms:>10.3f} "
                f"{profile.max_time_ms:>10.3f}"
            )

        lines.append("")

        # Most executed
        lines.append("-" * 60)
        lines.append("TOP 10 MOST EXECUTED LINES")
        lines.append("-" * 60)
        lines.append(
            f"{'Line':>6} {'Count':>8} {'Total(ms)':>12} {'Avg(ms)':>10}"
        )
        lines.append("-" * 60)

        for profile in session.get_most_executed(10):
            lines.append(
                f"{profile.line_number:>6} {profile.execution_count:>8} "
                f"{profile.total_time_ms:>12.3f} {profile.avg_time_ms:>10.3f}"
            )

        lines.append("")
        lines.append("=" * 60)

        return "\n".join(lines)

    def get_line_colors(
        self, session: Optional[ProfileSession] = None
    ) -> Dict[int, str]:
        """Get color codes for lines based on execution time.

        Returns dict mapping line numbers to color names:
        - 'hot': Lines consuming most time (red)
        - 'warm': Lines consuming moderate time (orange)
        - 'cool': Lines consuming little time (green)
        - 'cold': Lines never executed (gray)
        """
        if session is None:
            session = self.sessions[-1] if self.sessions else None

        if session is None:
            return {}

        colors: Dict[int, str] = {}

        if not session.line_profiles:
            return colors

        # Calculate thresholds based on max time
        max_time = max(p.total_time_ms for p in session.line_profiles.values())
        if max_time == 0:
            return {line: "cool" for line in session.line_profiles}

        hot_threshold = max_time * 0.7
        warm_threshold = max_time * 0.3

        for line_num, profile in session.line_profiles.items():
            if profile.total_time_ms >= hot_threshold:
                colors[line_num] = "hot"
            elif profile.total_time_ms >= warm_threshold:
                colors[line_num] = "warm"
            else:
                colors[line_num] = "cool"

        return colors

    def clear(self):
        """Clear all profiling data."""
        self.sessions.clear()
        self.current_session = None
        self.enabled = False


# Global instance
_profiler: Optional[PerformanceProfiler] = None


def get_profiler() -> PerformanceProfiler:
    """Get the global profiler instance."""
    global _profiler  # pylint: disable=global-statement
    if _profiler is None:
        _profiler = PerformanceProfiler()
    return _profiler
