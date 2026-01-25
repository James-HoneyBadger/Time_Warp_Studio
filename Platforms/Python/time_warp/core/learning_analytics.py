"""Learning analytics and student progress tracking."""

import json
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Callable, Dict, List, Optional


class ConceptType(Enum):
    """Programming concepts."""

    LOOPS = "loops"
    CONDITIONALS = "conditionals"
    FUNCTIONS = "functions"
    VARIABLES = "variables"
    ARRAYS = "arrays"
    STRINGS = "strings"
    RECURSION = "recursion"
    DATA_STRUCTURES = "data_structures"
    GRAPHICS = "graphics"
    INPUT_OUTPUT = "input_output"
    OPERATORS = "operators"


class ErrorCategory(Enum):
    """Error types."""

    SYNTAX = "syntax"
    RUNTIME = "runtime"
    LOGIC = "logic"
    TYPE = "type"
    INDEX = "index"


@dataclass
class ConceptMastery:
    """Track mastery of a concept."""

    concept: ConceptType
    first_seen: datetime
    attempts: int = 0
    successes: int = 0
    last_success: Optional[datetime] = None
    confidence: float = 0.0  # 0.0-1.0

    def update_success(self):
        """Record successful attempt."""
        self.attempts += 1
        self.successes += 1
        self.last_success = datetime.now()
        self.confidence = min(1.0, self.successes / max(1, self.attempts))

    def update_failure(self):
        """Record failed attempt."""
        self.attempts += 1
        self.confidence = self.successes / max(1, self.attempts)

    def get_mastery_level(self) -> str:
        """Get mastery level: novice, intermediate, advanced."""
        if self.confidence < 0.3:
            return "novice"
        elif self.confidence < 0.7:
            return "intermediate"
        else:
            return "advanced"


@dataclass
class ErrorPattern:
    """Track error patterns."""

    category: ErrorCategory
    message: str
    line_number: Optional[int] = None
    count: int = 0
    first_occurrence: datetime = field(default_factory=datetime.now)
    last_occurrence: datetime = field(default_factory=datetime.now)

    def increment(self):
        """Record another occurrence."""
        self.count += 1
        self.last_occurrence = datetime.now()


@dataclass
class ProgramSubmission:
    """Track program submission."""

    name: str
    language: str
    timestamp: datetime
    execution_time: float  # seconds
    successful: bool
    lines_of_code: int
    concepts_used: List[ConceptType] = field(default_factory=list)
    errors: List[ErrorPattern] = field(default_factory=list)


class LearningAnalytics:
    """Comprehensive learning analytics system."""

    def __init__(self, student_name: str = "Anonymous"):
        """Initialize analytics system."""
        self.student_name = student_name
        self.created_at = datetime.now()

        # Concept tracking
        self.concept_mastery: Dict[ConceptType, ConceptMastery] = {}
        for concept in ConceptType:
            self.concept_mastery[concept] = ConceptMastery(
                concept=concept, first_seen=datetime.now()
            )

        # Error tracking
        self.error_patterns: Dict[str, ErrorPattern] = {}

        # Program submissions
        self.programs: List[ProgramSubmission] = []

        # Time tracking
        self.total_coding_time: float = 0.0  # seconds
        self.session_start: Optional[datetime] = None

        # Progress metrics
        self.streak: int = 0  # consecutive successes
        self.best_streak: int = 0

        # Callbacks
        self.callbacks: Dict[str, List[Callable]] = {}

    def on_event(self, event_name: str, callback: Callable):
        """Register event callback."""
        if event_name not in self.callbacks:
            self.callbacks[event_name] = []
        self.callbacks[event_name].append(callback)

    def _trigger_callbacks(self, event_name: str, **kwargs):
        """Trigger callbacks."""
        if event_name in self.callbacks:
            for callback in self.callbacks[event_name]:
                callback(**kwargs)

    def start_session(self):
        """Start tracking time."""
        self.session_start = datetime.now()
        self._trigger_callbacks("session_started")

    def end_session(self) -> float:
        """End session and return total time."""
        if self.session_start:
            session_duration = (datetime.now() - self.session_start).total_seconds()
            self.total_coding_time += session_duration
            self._trigger_callbacks("session_ended", duration=session_duration)
            return session_duration
        return 0.0

    def record_program(
        self,
        name: str,
        language: str,
        execution_time: float,
        successful: bool,
        lines_of_code: int,
        concepts: Optional[List[ConceptType]] = None,
        errors: Optional[List[ErrorPattern]] = None,
    ):
        """Record program submission."""
        concepts = concepts or []
        errors = errors or []

        submission = ProgramSubmission(
            name=name,
            language=language,
            timestamp=datetime.now(),
            execution_time=execution_time,
            successful=successful,
            lines_of_code=lines_of_code,
            concepts_used=concepts,
            errors=errors,
        )

        self.programs.append(submission)

        # Update concept mastery
        for concept in concepts:
            if successful:
                self.concept_mastery[concept].update_success()
                self.streak += 1
                self.best_streak = max(self.best_streak, self.streak)
            else:
                self.concept_mastery[concept].update_failure()
                self.streak = 0

        # Track errors
        for error in errors:
            error_key = f"{error.category.value}:{error.message}"
            if error_key in self.error_patterns:
                self.error_patterns[error_key].increment()
            else:
                self.error_patterns[error_key] = error

        self._trigger_callbacks(
            "program_recorded", submission=submission, successful=successful
        )

    def get_concept_summary(self) -> Dict:
        """Get summary of concept mastery."""
        summary = {}
        for concept, mastery in self.concept_mastery.items():
            summary[concept.value] = {
                "level": mastery.get_mastery_level(),
                "confidence": mastery.confidence,
                "attempts": mastery.attempts,
                "successes": mastery.successes,
                "success_rate": mastery.successes / max(1, mastery.attempts),
            }
        return summary

    def get_error_analysis(self) -> Dict:
        """Analyze error patterns."""
        analysis = {
            "total_errors": len(self.error_patterns),
            "most_common": [],
            "by_category": {},
        }

        # Sort by frequency
        sorted_errors = sorted(
            self.error_patterns.items(), key=lambda x: x[1].count, reverse=True
        )

        # Most common errors
        for error_key, pattern in sorted_errors[:5]:
            analysis["most_common"].append(
                {
                    "message": pattern.message,
                    "count": pattern.count,
                    "category": pattern.category.value,
                }
            )

        # Errors by category
        for error_key, pattern in self.error_patterns.items():
            cat = pattern.category.value
            if cat not in analysis["by_category"]:
                analysis["by_category"][cat] = []
            analysis["by_category"][cat].append(
                {"message": pattern.message, "count": pattern.count}
            )

        return analysis

    def get_progress_metrics(self) -> Dict:
        """Get overall progress metrics."""
        if not self.programs:
            return {
                "programs": 0,
                "successful_rate": 0.0,
                "avg_execution_time": 0.0,
                "total_lines_written": 0,
                "coding_sessions": 0,
                "total_time_hours": 0.0,
                "streak": 0,
                "best_streak": 0,
                "concepts_mastered": 0,
            }

        successes = sum(1 for p in self.programs if p.successful)

        mastered = sum(
            1
            for m in self.concept_mastery.values()
            if m.get_mastery_level() == "advanced"
        )

        return {
            "programs": len(self.programs),
            "successful_rate": successes / len(self.programs),
            "avg_execution_time": sum(p.execution_time for p in self.programs)
            / len(self.programs),
            "total_lines_written": sum(p.lines_of_code for p in self.programs),
            "coding_sessions": 1,
            "total_time_hours": self.total_coding_time / 3600.0,
            "streak": self.streak,
            "best_streak": self.best_streak,
            "concepts_mastered": mastered,
        }

    def get_recommended_concepts(self) -> List[str]:
        """Get recommended concepts to learn next."""
        recommendations = []

        for concept, mastery in self.concept_mastery.items():
            if mastery.get_mastery_level() == "novice":
                recommendations.append(f"Learn {concept.value}")
            elif mastery.get_mastery_level() == "intermediate":
                recommendations.append(f"Master {concept.value}")

        return recommendations[:3]  # Top 3 recommendations

    def get_learning_path(self) -> List[str]:
        """Get recommended learning path."""
        path = []

        # Suggested order: variables → operators → I/O → conditions → loops →
        # functions → arrays
        suggested_order = [
            ConceptType.VARIABLES,
            ConceptType.OPERATORS,
            ConceptType.INPUT_OUTPUT,
            ConceptType.CONDITIONALS,
            ConceptType.LOOPS,
            ConceptType.FUNCTIONS,
            ConceptType.ARRAYS,
            ConceptType.STRINGS,
            ConceptType.RECURSION,
            ConceptType.GRAPHICS,
        ]

        for concept in suggested_order:
            mastery = self.concept_mastery[concept]
            if mastery.get_mastery_level() in ["novice", "intermediate"]:
                path.append(concept.value)

        return path

    def export_report(self, output_path: Optional[Path] = None) -> str:
        """Export learning report as formatted text."""
        report = f"""
    ╔════════════════════════════════════════════════════════════╗
    ║       TIME WARP STUDIO - LEARNING ANALYTICS REPORT         ║
    ╚════════════════════════════════════════════════════════════╝

    Student: {self.student_name}
    Report Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

    ═══════════════════════════════════════════════════════════
    PROGRESS SUMMARY
    ═══════════════════════════════════════════════════════════
    """

        metrics = self.get_progress_metrics()
        report += f"""
    Programs Written: {metrics['programs']}
    Success Rate: {metrics['successful_rate'] * 100:.1f}%
    Lines of Code: {metrics['total_lines_written']}
    Coding Time: {metrics['total_time_hours']:.1f} hours
    Current Streak: {metrics['streak']}
    Best Streak: {metrics['best_streak']}
    Concepts Mastered: {metrics['concepts_mastered']}/10

    ═══════════════════════════════════════════════════════════
    CONCEPT MASTERY
    ═══════════════════════════════════════════════════════════
    """

        concept_summary = self.get_concept_summary()
        for concept, info in concept_summary.items():
            level = info["level"].upper()
            bar = "█" * int(info["confidence"] * 10) + "░" * (
                10 - int(info["confidence"] * 10)
            )
            report += f"\n{
                concept:20} [{bar}] {
                info['confidence'] *
                100:3.0f}% ({level})"

        # Error analysis
        error_analysis = self.get_error_analysis()
        report += """

═══════════════════════════════════════════════════════════
ERROR ANALYSIS
═══════════════════════════════════════════════════════════

Total Errors Encountered: {error_analysis['total_errors']}
"""

        if error_analysis["most_common"]:
            report += "\nMost Common Errors:\n"
            for i, error in enumerate(error_analysis["most_common"], 1):
                report += f"  {i}. {
                    error['message']} ({
                    error['category']}) - {
                    error['count']} times\n"

        # Recommendations
        report += """

═══════════════════════════════════════════════════════════
RECOMMENDATIONS
═══════════════════════════════════════════════════════════

Learning Path:
"""
        for i, concept in enumerate(self.get_learning_path(), 1):
            report += f"  {i}. {concept}\n"

        report += """
Next Steps:
"""
        for rec in self.get_recommended_concepts():
            report += f"  • {rec}\n"

        report += "\n═══════════════════════════════════════════════════════════\n"

        if output_path:
            with open(output_path, "w") as f:
                f.write(report)

        return report

    def export_json(self, output_path: Optional[Path] = None) -> str:
        """Export analytics data as JSON."""
        data = {
            "student": self.student_name,
            "created": self.created_at.isoformat(),
            "metrics": self.get_progress_metrics(),
            "concepts": self.get_concept_summary(),
            "errors": self.get_error_analysis(),
            "programs": [
                {
                    "name": p.name,
                    "language": p.language,
                    "timestamp": p.timestamp.isoformat(),
                    "successful": p.successful,
                    "lines": p.lines_of_code,
                    "execution_time": p.execution_time,
                    "concepts": [c.value for c in p.concepts_used],
                }
                for p in self.programs
            ],
        }

        json_str = json.dumps(data, indent=2, default=str)

        if output_path:
            with open(output_path, "w") as f:
                f.write(json_str)

        return json_str

    def get_comparison_stats(self, other: "LearningAnalytics") -> Dict:
        """Compare with another student's analytics."""
        return {
            "programs": {
                "self": len(self.programs),
                "other": len(other.programs),
                "difference": len(self.programs) - len(other.programs),
            },
            "success_rate": {
                "self": self.get_progress_metrics()["successful_rate"],
                "other": other.get_progress_metrics()["successful_rate"],
            },
            "coding_time": {
                "self": self.total_coding_time / 3600.0,
                "other": other.total_coding_time / 3600.0,
            },
            "concepts_mastered": {
                "self": self.get_progress_metrics()["concepts_mastered"],
                "other": other.get_progress_metrics()["concepts_mastered"],
            },
        }


class ClassroomAnalytics:
    """Aggregate analytics for a classroom."""

    def __init__(self, class_name: str = "Class"):
        """Initialize classroom analytics."""
        self.class_name = class_name
        self.students: Dict[str, LearningAnalytics] = {}

    def add_student(self, name: str) -> LearningAnalytics:
        """Add student to classroom."""
        analytics = LearningAnalytics(name)
        self.students[name] = analytics
        return analytics

    def get_class_summary(self) -> Dict:
        """Get summary statistics for entire class."""
        if not self.students:
            return {}

        all_programs = sum(len(s.programs) for s in self.students.values())
        avg_success = sum(
            s.get_progress_metrics()["successful_rate"] for s in self.students.values()
        ) / len(self.students)

        return {
            "total_students": len(self.students),
            "total_programs": all_programs,
            "avg_success_rate": avg_success,
            "avg_lines_per_student": all_programs / len(self.students),
            "students": {
                name: analytics.get_progress_metrics()
                for name, analytics in self.students.items()
            },
        }
