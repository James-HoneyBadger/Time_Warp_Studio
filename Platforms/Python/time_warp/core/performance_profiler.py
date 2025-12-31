"""Performance profiler for identifying slow code sections."""

from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple
from enum import Enum
import time
import sys


class TimeUnit(Enum):
    """Time units for display."""
    MICROSECONDS = "μs"
    MILLISECONDS = "ms"
    SECONDS = "s"


@dataclass
class LineProfile:
    """Profile data for a single line."""
    line_no: int
    line_content: str
    call_count: int
    total_time: float  # milliseconds
    avg_time: float  # milliseconds
    self_time: float  # milliseconds (excluding function calls)
    time_percentage: float  # percentage of total time
    is_hotspot: bool = False


@dataclass
class FunctionProfile:
    """Profile data for a function."""
    name: str
    call_count: int
    total_time: float
    avg_time: float
    lines: List[LineProfile]


class PerformanceProfiler:
    """Profiles code execution and identifies slow sections."""
    
    def __init__(self, hotspot_threshold: float = 5.0):
        """
        Initialize profiler.
        
        Args:
            hotspot_threshold: Percentage of total time to mark as hotspot (default 5%)
        """
        self.hotspot_threshold = hotspot_threshold
        self.line_profiles: Dict[int, LineProfile] = {}
        self.function_profiles: Dict[str, FunctionProfile] = {}
        self.total_time = 0.0
        self.start_time: Optional[float] = None
        self.is_profiling = False
        self._line_times: Dict[int, List[float]] = {}
        self._call_counts: Dict[int, int] = {}
    
    def start_profiling(self):
        """Start profiling."""
        self.line_profiles.clear()
        self.function_profiles.clear()
        self._line_times.clear()
        self._call_counts.clear()
        self.start_time = time.perf_counter()
        self.is_profiling = True
    
    def stop_profiling(self):
        """Stop profiling and calculate metrics."""
        if self.start_time:
            self.total_time = (time.perf_counter() - self.start_time) * 1000  # Convert to ms
        self.is_profiling = False
        self._calculate_hotspots()
    
    def record_line_execution(self, line_no: int, line_content: str, execution_time: float):
        """
        Record execution time for a line.
        
        Args:
            line_no: Line number
            line_content: Source code content
            execution_time: Execution time in milliseconds
        """
        if line_no not in self._line_times:
            self._line_times[line_no] = []
            self._call_counts[line_no] = 0
        
        self._line_times[line_no].append(execution_time)
        self._call_counts[line_no] += 1
    
    def _calculate_hotspots(self):
        """Calculate hotspots from recorded line times."""
        for line_no, times in self._line_times.items():
            total = sum(times)
            avg = total / len(times) if times else 0
            percentage = (total / self.total_time * 100) if self.total_time > 0 else 0
            
            profile = LineProfile(
                line_no=line_no,
                line_content="",
                call_count=self._call_counts[line_no],
                total_time=total,
                avg_time=avg,
                self_time=total,
                time_percentage=percentage,
                is_hotspot=percentage >= self.hotspot_threshold
            )
            self.line_profiles[line_no] = profile
    
    def get_hotspots(self, limit: int = 10) -> List[LineProfile]:
        """Get slowest lines (hotspots)."""
        profiles = sorted(
            self.line_profiles.values(),
            key=lambda x: x.total_time,
            reverse=True
        )
        return profiles[:limit]
    
    def get_line_profile(self, line_no: int) -> Optional[LineProfile]:
        """Get profile for a specific line."""
        return self.line_profiles.get(line_no)
    
    def get_slowest_functions(self, limit: int = 5) -> List[FunctionProfile]:
        """Get slowest functions."""
        profiles = sorted(
            self.function_profiles.values(),
            key=lambda x: x.total_time,
            reverse=True
        )
        return profiles[:limit]
    
    def generate_report(self) -> str:
        """Generate a profiling report."""
        lines = []
        lines.append("=" * 70)
        lines.append("PERFORMANCE PROFILE REPORT")
        lines.append("=" * 70)
        lines.append(f"Total Execution Time: {self.total_time:.3f}ms")
        lines.append("")
        
        hotspots = self.get_hotspots(10)
        if hotspots:
            lines.append("TOP 10 HOTSPOTS (Slowest Lines):")
            lines.append("-" * 70)
            lines.append(f"{'Line':<6} {'Time (ms)':<12} {'Calls':<8} {'Avg (ms)':<12} {'% Total':<10}")
            lines.append("-" * 70)
            
            for profile in hotspots:
                lines.append(
                    f"{profile.line_no:<6} "
                    f"{profile.total_time:<12.3f} "
                    f"{profile.call_count:<8} "
                    f"{profile.avg_time:<12.3f} "
                    f"{profile.time_percentage:<10.1f}%"
                )
        
        lines.append("")
        lines.append("=" * 70)
        
        return "\n".join(lines)
    
    def export_json(self) -> Dict:
        """Export profile data as JSON-compatible dict."""
        return {
            'total_time': self.total_time,
            'hotspots': [
                {
                    'line': p.line_no,
                    'total_time': p.total_time,
                    'call_count': p.call_count,
                    'avg_time': p.avg_time,
                    'percentage': p.time_percentage,
                    'is_hotspot': p.is_hotspot,
                }
                for p in self.get_hotspots(20)
            ]
        }


class LineExecutionTracer:
    """Traces line-by-line execution for profiling."""
    
    def __init__(self, profiler: PerformanceProfiler):
        """Initialize tracer."""
        self.profiler = profiler
        self.last_time = time.perf_counter()
        self.frame_times: Dict[int, float] = {}
    
    def trace_lines(self, frame, event, arg):
        """Trace function for sys.settrace()."""
        if not self.profiler.is_profiling:
            return None
        
        if event == 'line':
            current_time = time.perf_counter()
            line_no = frame.f_lineno
            
            # Calculate time since last line
            elapsed = (current_time - self.last_time) * 1000  # Convert to ms
            
            # Record execution time for previous line
            if line_no in self.frame_times:
                self.profiler.record_line_execution(
                    line_no,
                    "code line",
                    elapsed
                )
            
            self.last_time = current_time
            return self.trace_lines
        
        return self.trace_lines
    
    def start(self):
        """Start tracing."""
        self.last_time = time.perf_counter()
        sys.settrace(self.trace_lines)
    
    def stop(self):
        """Stop tracing."""
        sys.settrace(None)


class OptimizationSuggester:
    """Suggests optimizations based on profile data."""
    
    @staticmethod
    def get_suggestions(profiler: PerformanceProfiler) -> List[str]:
        """Generate optimization suggestions."""
        suggestions = []
        
        hotspots = profiler.get_hotspots(5)
        
        if not hotspots:
            return ["No hotspots detected. Code is well-optimized!"]
        
        # Check for loops with high call counts
        for profile in hotspots:
            if profile.call_count > 1000:
                suggestions.append(
                    f"Line {profile.line_no}: Called {profile.call_count} times. "
                    f"Consider moving loop to outer scope or optimizing the expression."
                )
            elif profile.call_count > 100:
                suggestions.append(
                    f"Line {profile.line_no}: High call count ({profile.call_count}). "
                    f"Check for unnecessary nested loops."
                )
        
        # Check for lines taking >20% of total time
        for profile in hotspots:
            if profile.time_percentage > 20:
                suggestions.append(
                    f"Line {profile.line_no}: Uses {profile.time_percentage:.1f}% of total time. "
                    f"This is a major bottleneck."
                )
        
        if not suggestions:
            suggestions.append(
                "No critical hotspots found. Code is reasonably well-optimized."
            )
        
        return suggestions


class ExecutionTimeVisualizer:
    """Visualizes execution time graphically."""
    
    @staticmethod
    def get_bar_chart(profiler: PerformanceProfiler, width: int = 50) -> str:
        """Generate ASCII bar chart of hotspots."""
        hotspots = profiler.get_hotspots(10)
        if not hotspots:
            return "No profiling data available"
        
        lines = []
        max_time = max(h.total_time for h in hotspots) if hotspots else 1
        
        lines.append("HOTSPOTS - VISUAL REPRESENTATION")
        lines.append("=" * 70)
        
        for profile in hotspots:
            bar_width = int((profile.total_time / max_time) * width)
            bar = "█" * bar_width
            percentage = (profile.total_time / profiler.total_time * 100) if profiler.total_time > 0 else 0
            
            lines.append(
                f"Line {profile.line_no:<5} {bar:<50} {percentage:>5.1f}% ({profile.total_time:.2f}ms)"
            )
        
        return "\n".join(lines)
    
    @staticmethod
    def get_heatmap_markers(profiler: PerformanceProfiler) -> Dict[int, str]:
        """Get visual markers for lines (for editor highlighting)."""
        markers = {}
        hotspots = profiler.get_hotspots(20)
        
        for profile in hotspots:
            if profile.time_percentage > 10:
                # Red - critical hotspot
                markers[profile.line_no] = "🔴"
            elif profile.time_percentage > 5:
                # Orange - moderate hotspot
                markers[profile.line_no] = "🟠"
            elif profile.time_percentage > 2:
                # Yellow - minor hotspot
                markers[profile.line_no] = "🟡"
        
        return markers
