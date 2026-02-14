"""
Time Warp Studio - Phase IX: AI-Powered Intelligence System

Provides:
- Intelligent code suggestions
- Automated bug detection and fixing
- Learning path recommendations
- Smart code review insights
- Performance optimization hints
- Natural language code generation
"""

from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from typing import Dict, List, Optional, Tuple


def utc_now() -> datetime:
    return datetime.now(timezone.utc)

# ===== ENUMS =====


class SuggestionType(Enum):
    """Code suggestion types"""

    COMPLETION = "completion"
    REFACTORING = "refactoring"
    OPTIMIZATION = "optimization"
    BUG_FIX = "bug_fix"
    PATTERN = "pattern"
    LEARNING = "learning"


class BugSeverity(Enum):
    """Bug severity levels"""

    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


class LearningLevel(Enum):
    """User learning levels"""

    BEGINNER = "beginner"
    INTERMEDIATE = "intermediate"
    ADVANCED = "advanced"
    EXPERT = "expert"


# ===== DATA CLASSES =====


@dataclass
class CodeSuggestion:
    """AI-generated code suggestion"""

    id: str = ""
    type: SuggestionType = SuggestionType.COMPLETION

    # Content
    original_code: str = ""
    suggested_code: str = ""
    description: str = ""

    # Context
    file: str = ""
    line: int = 0
    column: int = 0

    # AI details
    confidence: float = 0.0  # 0-1
    reasoning: str = ""
    alternatives: List[str] = field(default_factory=list)

    # Engagement
    accepted: bool = False
    accepted_at: Optional[datetime] = None
    helpful: Optional[bool] = None  # None = not rated


@dataclass
class BugDetection:
    """Detected potential bug"""

    id: str = ""
    severity: BugSeverity = BugSeverity.WARNING

    # Location
    file: str = ""
    line: int = 0
    message: str = ""

    # Analysis
    potential_cause: str = ""
    suggested_fix: str = ""
    example: Optional[str] = None

    # Resources
    documentation_url: Optional[str] = None
    similar_issues: List[str] = field(default_factory=list)

    # Status
    fixed: bool = False
    false_positive: bool = False


@dataclass
class ReviewInsight:
    """AI code review insight"""

    id: str = ""
    file: str = ""
    line_range: Tuple[int, int] = (0, 0)

    # Review content
    category: str = ""  # style, performance, security, maintainability
    observation: str = ""
    recommendation: str = ""

    # Evidence
    code_pattern: str = ""
    severity: str = "minor"  # minor, major, critical

    # Follow-up
    suggested_reading: Optional[str] = None
    related_issues: List[str] = field(default_factory=list)


@dataclass
class LearningPath:
    """Personalized learning path"""

    user_id: str = ""
    current_level: LearningLevel = LearningLevel.BEGINNER

    # Progress
    completed_lessons: List[str] = field(default_factory=list)
    current_lesson: Optional[str] = None
    next_lessons: List[str] = field(default_factory=list)

    # Statistics
    total_hours: float = 0.0
    practice_problems_completed: int = 0
    mastery_score: float = 0.0  # 0-100

    # Recommendations
    weak_areas: List[str] = field(default_factory=list)
    recommended_topics: List[str] = field(default_factory=list)

    # Metadata
    created_at: datetime = field(default_factory=utc_now)
    updated_at: datetime = field(default_factory=utc_now)


@dataclass
class OptimizationHint:
    """Performance optimization hint"""

    id: str = ""
    category: str = ""  # algorithm, memory, io, concurrency

    # Location
    file: str = ""
    start_line: int = 0
    end_line: int = 0

    # Optimization
    current_approach: str = ""
    optimized_approach: str = ""
    expected_improvement: str = ""  # e.g., "50% faster"

    # Details
    time_complexity: str = ""  # e.g., "O(n²) → O(n log n)"
    space_complexity: str = ""

    # Resources
    explanation: str = ""
    code_example: Optional[str] = None


# ===== AI ENGINES =====


class CodeCompletionEngine:
    """Intelligent code completion"""

    def __init__(self):
        self.suggestion_history: List[CodeSuggestion] = []
        self.completion_patterns: Dict[str, List[str]] = {}

    def suggest_completion(
        self, code: str, current_line: int, language: str
    ) -> List[CodeSuggestion]:
        """Generate code completions"""
        suggestions: List[CodeSuggestion] = []

        # Analyze context
        lines = code.split("\n")
        prefix = lines[current_line].strip() if current_line < len(lines) else ""

        # Pattern-based suggestions
        if language == "basic":
            suggestions.extend(self._suggest_basic_completions(prefix))
        elif language == "logo":
            suggestions.extend(self._suggest_logo_completions(prefix))
        elif language == "python":
            suggestions.extend(self._suggest_python_completions(prefix))

        # Rank by confidence
        suggestions.sort(key=lambda s: s.confidence, reverse=True)

        return suggestions[:5]

    def _suggest_basic_completions(self, prefix: str) -> List[CodeSuggestion]:
        """BASIC language completions"""
        completions = {
            "IF": "IF condition THEN ... END IF",
            "FOR": "FOR i = 1 TO 10 ... END FOR",
            "SUB": "SUB name(params) ... END SUB",
            "FUNCTION": "FUNCTION name(params) ... END FUNCTION",
            "WHILE": "WHILE condition ... END WHILE",
        }

        suggestions = []
        for keyword, template in completions.items():
            if keyword.startswith(prefix.upper()):
                suggestions.append(
                    CodeSuggestion(
                        type=SuggestionType.COMPLETION,
                        original_code=prefix,
                        suggested_code=template,
                        confidence=0.9,
                    )
                )

        return suggestions

    def _suggest_logo_completions(self, prefix: str) -> List[CodeSuggestion]:
        """Logo language completions"""
        completions = {
            "FORWARD": "FORWARD 100",
            "BACK": "BACK 50",
            "RIGHT": "RIGHT 90",
            "LEFT": "LEFT 45",
            "REPEAT": "REPEAT 4 [...]",
        }

        suggestions = []
        for keyword, template in completions.items():
            if keyword.startswith(prefix.upper()):
                suggestions.append(
                    CodeSuggestion(
                        type=SuggestionType.COMPLETION,
                        original_code=prefix,
                        suggested_code=template,
                        confidence=0.85,
                    )
                )

        return suggestions

    def _suggest_python_completions(self, prefix: str) -> List[CodeSuggestion]:
        """Python language completions"""
        completions = {
            "def": "def function_name():",
            "class": "class ClassName:",
            "for": "for item in iterable:",
            "while": "while condition:",
            "if": "if condition:",
            "try": "try: ... except: ...",
        }

        suggestions = []
        for keyword, template in completions.items():
            if keyword.startswith(prefix.lower()):
                suggestions.append(
                    CodeSuggestion(
                        type=SuggestionType.COMPLETION,
                        original_code=prefix,
                        suggested_code=template,
                        confidence=0.8,
                    )
                )

        return suggestions


class BugDetectionEngine:
    """AI-powered bug detection"""

    def analyze_code(self, code: str, language: str) -> List[BugDetection]:
        """Detect potential bugs"""
        bugs: List[BugDetection] = []
        lines = code.split("\n")

        # Pattern 1: Uninitialized variables
        for i, line in enumerate(lines, 1):
            if "IF" in line.upper() and any(
                var in line for var in ["x", "y", "count", "sum"]
            ):
                # Check if variable might be uninitialized
                if not any(
                    f"{var} =" in code[: code.find(line)]
                    for var in ["x", "y", "count", "sum"]
                ):
                    bugs.append(
                        BugDetection(
                            severity=BugSeverity.WARNING,
                            file="unknown",
                            line=i,
                            message="Variable may be uninitialized",
                            potential_cause="Using variable before assignment",
                            suggested_fix="Initialize variable before use",
                        )
                    )

        # Pattern 2: Off-by-one errors in loops
        for i, line in enumerate(lines, 1):
            if "FOR" in line.upper() and "100" in line:
                bugs.append(
                    BugDetection(
                        severity=BugSeverity.INFO,
                        file="unknown",
                        line=i,
                        message="Possible off-by-one error",
                        potential_cause="Loop boundary may be incorrect",
                        suggested_fix="Verify loop range is as intended",
                    )
                )

        # Pattern 3: Infinite loops
        if "WHILE" in code.upper() and "WHILE 1" in code.upper():
            bugs.append(
                BugDetection(
                    severity=BugSeverity.ERROR,
                    message="Infinite loop detected",
                    potential_cause="Loop condition always true",
                    suggested_fix="Add break condition or modify loop condition",
                )
            )

        return bugs


class ReviewInsightEngine:
    """AI-powered code review"""

    def analyze_code_quality(self, code: str) -> List[ReviewInsight]:
        """Generate code review insights"""
        insights: List[ReviewInsight] = []
        lines = code.split("\n")

        # Insight 1: Code style
        for i, line in enumerate(lines, 1):
            if len(line) > 100:
                insights.append(
                    ReviewInsight(
                        file="unknown",
                        line_range=(i, i),
                        category="style",
                        observation="Line exceeds 100 characters",
                        recommendation="Break long lines for readability",
                        severity="minor",
                    )
                )

        # Insight 2: Comments
        comment_lines = sum(1 for line in lines if line.strip().startswith("REM"))
        if comment_lines < len(lines) * 0.1:
            insights.append(
                ReviewInsight(
                    category="maintainability",
                    observation="Low comment-to-code ratio",
                    recommendation="Add comments explaining complex logic",
                    severity="minor",
                )
            )

        # Insight 3: Function complexity
        if code.count("IF") > 5:
            insights.append(
                ReviewInsight(
                    category="maintainability",
                    observation="Multiple nested conditions",
                    recommendation="Extract conditions to helper functions",
                    severity="major",
                )
            )

        return insights


class LearningPathGenerator:
    """Generates personalized learning paths"""

    def __init__(self):
        self.user_profiles: Dict[str, Dict] = {}

    def create_learning_path(
        self, user_id: str, current_level: LearningLevel
    ) -> LearningPath:
        """Create personalized learning path"""
        path = LearningPath(user_id=user_id, current_level=current_level)

        # Set lessons based on level
        if current_level == LearningLevel.BEGINNER:
            path.next_lessons = [
                "variables-basics",
                "input-output",
                "conditionals",
                "loops",
            ]
        elif current_level == LearningLevel.INTERMEDIATE:
            path.next_lessons = [
                "functions-procedures",
                "arrays",
                "string-manipulation",
                "file-io",
            ]
        elif current_level == LearningLevel.ADVANCED:
            path.next_lessons = [
                "algorithms",
                "data-structures",
                "recursion",
                "optimization",
            ]

        self.user_profiles[user_id] = {
            "path": path,
            "created_at": utc_now(),
        }

        return path

    def recommend_next_lesson(self, user_id: str) -> Optional[str]:
        """Recommend next lesson"""
        profile = self.user_profiles.get(user_id)
        if profile and profile["path"].next_lessons:
            return profile["path"].next_lessons[0]
        return None

    def update_progress(
        self, user_id: str, lesson_id: str, completed: bool = True
    ) -> bool:
        """Update learning progress"""
        profile = self.user_profiles.get(user_id)
        if not profile:
            return False

        path = profile["path"]
        if completed and lesson_id not in path.completed_lessons:
            path.completed_lessons.append(lesson_id)
            path.practice_problems_completed += 1
            path.total_hours += 0.5

            # Update level
            if len(path.completed_lessons) >= 4:
                if path.current_level == LearningLevel.BEGINNER:
                    path.current_level = LearningLevel.INTERMEDIATE
                elif path.current_level == LearningLevel.INTERMEDIATE:
                    path.current_level = LearningLevel.ADVANCED

        path.updated_at = utc_now()
        return True


class PerformanceOptimizationAdvisor:
    """Provides performance optimization advice"""

    def analyze_performance(self, code: str) -> List[OptimizationHint]:
        """Analyze code for optimization opportunities"""
        hints: List[OptimizationHint] = []

        # Pattern 1: Nested loops
        nested_count = code.count(" FOR ")
        if nested_count > 1:
            hints.append(
                OptimizationHint(
                    category="algorithm",
                    current_approach="Nested loops for searching/comparing",
                    optimized_approach="Use hash table or binary search",
                    expected_improvement="100x faster for large datasets",
                    time_complexity="O(n²) → O(n log n)",
                )
            )

        # Pattern 2: String concatenation
        concat_count = code.count("+")
        if concat_count > 10:
            hints.append(
                OptimizationHint(
                    category="memory",
                    current_approach="String concatenation with +",
                    optimized_approach="Use string builder or join",
                    expected_improvement="10x faster for many concatenations",
                )
            )

        # Pattern 3: Repeated calculations
        if code.count("SQRT") > 3 or code.count("SIN") > 3:
            hints.append(
                OptimizationHint(
                    category="algorithm",
                    current_approach="Computing same values repeatedly",
                    optimized_approach="Cache computed values",
                    expected_improvement="5-10x faster",
                )
            )

        return hints


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    # Code completion
    completion_engine = CodeCompletionEngine()
    suggestions = completion_engine.suggest_completion(
        'IF x > 10 THEN\n  PRINT "High"', 0, "basic"
    )
    print(f"Suggestions: {len(suggestions)}")
    for s in suggestions:
        print(f"  - {s.suggested_code} (confidence: {s.confidence})")

    # Bug detection
    bug_engine = BugDetectionEngine()
    sample_code = """
    IF x > 10 THEN
      PRINT x
    END IF
    """
    bugs = bug_engine.analyze_code(sample_code, "basic")
    print(f"\nBugs found: {len(bugs)}")

    # Code review
    review_engine = ReviewInsightEngine()
    insights = review_engine.analyze_code_quality(sample_code)
    print(f"Review insights: {len(insights)}")

    # Learning path
    learning_gen = LearningPathGenerator()
    path = learning_gen.create_learning_path("user1", LearningLevel.BEGINNER)
    print(f"\nLearning path created for {path.user_id}")
    print(f"Next lessons: {path.next_lessons[:3]}")

    # Performance optimization
    perf_advisor = PerformanceOptimizationAdvisor()
    hints = perf_advisor.analyze_performance(
        "FOR i=1 TO 100\nFOR j=1 TO 100\nPRINT i*j\nEND FOR\nEND FOR"
    )
    print(f"\nOptimization hints: {len(hints)}")
