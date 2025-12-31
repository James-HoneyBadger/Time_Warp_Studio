"""
Time Warp Studio - Advanced IDE Refactoring Tools

Provides:
- Code refactoring suggestions
- Automated code improvements
- Complexity analysis
- Code smells detection
- Performance optimization recommendations
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple, Set
from enum import Enum
import re

# ===== ENUMS =====

class RefactoringSuggestionType(Enum):
    """Types of refactoring suggestions"""
    EXTRACT_FUNCTION = "extract_function"
    RENAME_VARIABLE = "rename_variable"
    SIMPLIFY_LOGIC = "simplify_logic"
    REMOVE_DUPLICATION = "remove_duplication"
    IMPROVE_READABILITY = "improve_readability"
    OPTIMIZE_PERFORMANCE = "optimize_performance"
    FIX_CODE_SMELL = "fix_code_smell"
    STANDARDIZE_STYLE = "standardize_style"

class CodeSmellType(Enum):
    """Code smell types"""
    LONG_METHOD = "long_method"
    LONG_PARAMETER_LIST = "long_parameter_list"
    DUPLICATE_CODE = "duplicate_code"
    DEAD_CODE = "dead_code"
    COMPLEXITY = "complexity"
    MAGIC_NUMBERS = "magic_numbers"
    INCONSISTENT_NAMING = "inconsistent_naming"
    LARGE_CLASS = "large_class"

class ComplexityLevel(Enum):
    """Cyclomatic complexity levels"""
    LOW = "low"
    MODERATE = "moderate"
    HIGH = "high"
    VERY_HIGH = "very_high"

# ===== DATA CLASSES =====

@dataclass
class CodeMetrics:
    """Code metrics"""
    lines_of_code: int = 0
    cyclomatic_complexity: int = 0
    cognitive_complexity: int = 0
    nesting_depth: int = 0
    average_line_length: int = 0
    number_of_functions: int = 0
    number_of_variables: int = 0
    number_of_comments: int = 0
    comment_ratio: float = 0.0  # Comments / LOC
    duplication_ratio: float = 0.0  # Duplicated lines / total lines

@dataclass
class RefactoringSuggestion:
    """Refactoring suggestion"""
    id: str = ""
    type: RefactoringSuggestionType = RefactoringSuggestionType.IMPROVE_READABILITY
    title: str = ""
    description: str = ""
    
    # Location
    start_line: int = 0
    end_line: int = 0
    start_column: int = 0
    end_column: int = 0
    
    # Details
    affected_code: str = ""
    suggested_code: str = ""
    
    # Severity and impact
    severity: str = "info"  # info, warning, error
    impact_score: float = 0.0  # 0-1 impact on code quality
    effort_score: float = 0.0  # 0-1 effort to implement
    
    # Reasoning
    rationale: str = ""
    references: List[str] = field(default_factory=list)
    
    # Auto-apply capability
    auto_fixable: bool = False
    fix_code: Optional[str] = None

@dataclass
class CodeSmell:
    """Detected code smell"""
    id: str = ""
    smell_type: CodeSmellType = CodeSmellType.LONG_METHOD
    title: str = ""
    description: str = ""
    
    # Location
    start_line: int = 0
    end_line: int = 0
    
    # Severity
    severity: str = "warning"
    confidence: float = 1.0  # 0-1 confidence in detection
    
    # Remediation
    remediation_tips: List[str] = field(default_factory=list)

@dataclass
class DuplicationBlock:
    """Code duplication block"""
    id: str = ""
    start_line: int = 0
    end_line: int = 0
    code: str = ""
    occurrences: List[Tuple[int, int]] = field(default_factory=list)  # (start, end) tuples
    similarity: float = 1.0  # 0-1 similarity score

@dataclass
class OptimizationOpportunity:
    """Performance optimization opportunity"""
    id: str = ""
    title: str = ""
    description: str = ""
    
    # Location
    start_line: int = 0
    end_line: int = 0
    
    # Impact
    expected_speedup: float = 1.0  # 1.5 = 50% faster
    expected_memory_reduction: float = 0.0  # KB saved
    complexity_reduction: str = ""  # e.g., "O(n²) → O(n log n)"
    
    # Details
    current_approach: str = ""
    suggested_approach: str = ""
    resources: List[str] = field(default_factory=list)

# ===== ANALYZERS =====

class CodeMetricsAnalyzer:
    """Analyzes code metrics"""
    
    @staticmethod
    def analyze(code: str) -> CodeMetrics:
        """Analyze code metrics"""
        lines = code.split('\n')
        metrics = CodeMetrics()
        
        # Lines of code (excluding comments and blank lines)
        non_empty_lines = [
            l.strip() for l in lines
            if l.strip() and not l.strip().startswith('REM')
            and not l.strip().startswith('\\')
        ]
        metrics.lines_of_code = len(non_empty_lines)
        
        # Comment lines
        comment_lines = [
            l for l in lines
            if l.strip().startswith('REM') or l.strip().startswith('\\')
        ]
        metrics.number_of_comments = len(comment_lines)
        metrics.comment_ratio = (
            len(comment_lines) / len(lines) if lines else 0
        )
        
        # Average line length
        if non_empty_lines:
            metrics.average_line_length = (
                sum(len(l) for l in non_empty_lines) // len(non_empty_lines)
            )
        
        # Cyclomatic complexity (simplified)
        metrics.cyclomatic_complexity = (
            code.count(' IF ') + code.count(' ELSE ') +
            code.count(' FOR ') + code.count(' WHILE ') +
            code.count(' CASE ')
        ) + 1
        
        # Nesting depth
        max_depth = 0
        current_depth = 0
        for line in lines:
            stripped = line.strip().upper()
            if any(kw in stripped for kw in ['IF', 'FOR', 'WHILE', 'DO']):
                current_depth += 1
                max_depth = max(max_depth, current_depth)
            elif any(kw in stripped for kw in ['END IF', 'END FOR', 'END WHILE', 'END DO']):
                current_depth = max(0, current_depth - 1)
        metrics.nesting_depth = max_depth
        
        # Functions (SUB/FUNCTION declarations)
        metrics.number_of_functions = (
            code.count(' SUB ') + code.count(' FUNCTION ')
        )
        
        # Variables
        var_pattern = r'(DIM|LET|VAR)\s+\w+'
        metrics.number_of_variables = len(re.findall(var_pattern, code, re.IGNORECASE))
        
        return metrics

class CodeSmellDetector:
    """Detects code smells"""
    
    @staticmethod
    def detect(code: str, metrics: CodeMetrics) -> List[CodeSmell]:
        """Detect code smells"""
        smells: List[CodeSmell] = []
        lines = code.split('\n')
        
        # Long method detection (LOC > 50)
        if metrics.lines_of_code > 50:
            smells.append(CodeSmell(
                smell_type=CodeSmellType.LONG_METHOD,
                title="Long Method",
                description="This method is too long and does too many things",
                start_line=1,
                end_line=len(lines),
                severity="warning",
                remediation_tips=[
                    "Extract smaller functions",
                    "Separate concerns",
                    "Use helper methods"
                ]
            ))
        
        # High complexity detection
        if metrics.cyclomatic_complexity > 10:
            smells.append(CodeSmell(
                smell_type=CodeSmellType.COMPLEXITY,
                title="High Complexity",
                description="Complex control flow makes code hard to understand",
                start_line=1,
                end_line=len(lines),
                severity="error",
                remediation_tips=[
                    "Simplify conditionals",
                    "Extract helper functions",
                    "Use lookup tables instead of conditionals"
                ]
            ))
        
        # Deep nesting detection
        if metrics.nesting_depth > 5:
            smells.append(CodeSmell(
                smell_type=CodeSmellType.COMPLEXITY,
                title="Deep Nesting",
                description="Deeply nested code is hard to follow",
                start_line=1,
                end_line=len(lines),
                severity="warning",
                remediation_tips=[
                    "Extract methods",
                    "Use early returns",
                    "Refactor conditionals"
                ]
            ))
        
        # Magic numbers detection
        magic_num_pattern = r'\b\d{3,}\b'
        magic_numbers = re.findall(magic_num_pattern, code)
        if len(magic_numbers) > 3:
            smells.append(CodeSmell(
                smell_type=CodeSmellType.MAGIC_NUMBERS,
                title="Magic Numbers",
                description="Hard-coded numbers without explanation",
                start_line=1,
                end_line=len(lines),
                severity="info",
                remediation_tips=[
                    "Extract to named constants",
                    "Document the meaning",
                    "Use configuration files"
                ]
            ))
        
        # Inconsistent naming detection
        if metrics.number_of_variables > 5:
            var_names = re.findall(r'\b[a-zA-Z]\w*\b', code)
            naming_styles = {
                'snake_case': len([n for n in var_names if '_' in n]),
                'camelCase': len([n for n in var_names if n[0].islower() and any(c.isupper() for c in n)]),
                'PascalCase': len([n for n in var_names if n[0].isupper()]),
                'UPPERCASE': len([n for n in var_names if n.isupper()])
            }
            
            style_count = sum(1 for c in naming_styles.values() if c > 0)
            if style_count > 2:
                smells.append(CodeSmell(
                    smell_type=CodeSmellType.INCONSISTENT_NAMING,
                    title="Inconsistent Naming",
                    description="Variables use inconsistent naming conventions",
                    start_line=1,
                    end_line=len(lines),
                    severity="info",
                    remediation_tips=[
                        "Choose one naming convention",
                        "Apply consistently throughout",
                        "Use IDE refactoring tools"
                    ]
                ))
        
        return smells

class DuplicationDetector:
    """Detects code duplication"""
    
    @staticmethod
    def detect(code: str, min_block_size: int = 5) -> List[DuplicationBlock]:
        """Detect duplicate code blocks"""
        lines = code.split('\n')
        duplications: List[DuplicationBlock] = []
        seen_blocks: Dict[str, List[int]] = {}
        
        # Create blocks of N lines and track them
        for i in range(len(lines) - min_block_size + 1):
            block = '\n'.join(lines[i:i+min_block_size])
            normalized = block.lower().strip()
            
            if normalized:
                if normalized not in seen_blocks:
                    seen_blocks[normalized] = []
                seen_blocks[normalized].append(i)
        
        # Find duplicates (blocks appearing more than once)
        for block, positions in seen_blocks.items():
            if len(positions) > 1:
                dup = DuplicationBlock(
                    code=block,
                    occurrences=[
                        (pos, pos + min_block_size - 1)
                        for pos in positions
                    ]
                )
                duplications.append(dup)
        
        return duplications

class RefactoringEngine:
    """Suggests refactorings"""
    
    @staticmethod
    def suggest(code: str, metrics: CodeMetrics, smells: List[CodeSmell]) -> List[RefactoringSuggestion]:
        """Generate refactoring suggestions"""
        suggestions: List[RefactoringSuggestion] = []
        
        # Suggestion 1: Extract functions for long methods
        if metrics.lines_of_code > 50:
            suggestions.append(RefactoringSuggestion(
                type=RefactoringSuggestionType.EXTRACT_FUNCTION,
                title="Extract Function",
                description="Break down long functions into smaller ones",
                affected_code=code[:100] + "...",
                severity="warning",
                impact_score=0.8,
                effort_score=0.5,
                rationale="Functions under 20 lines are easier to test and maintain"
            ))
        
        # Suggestion 2: Simplify logic for high complexity
        if metrics.cyclomatic_complexity > 10:
            suggestions.append(RefactoringSuggestion(
                type=RefactoringSuggestionType.SIMPLIFY_LOGIC,
                title="Simplify Logic",
                description="Use guard clauses and early returns to reduce nesting",
                severity="warning",
                impact_score=0.7,
                effort_score=0.6
            ))
        
        # Suggestion 3: Add comments
        if metrics.comment_ratio < 0.1:
            suggestions.append(RefactoringSuggestion(
                type=RefactoringSuggestionType.IMPROVE_READABILITY,
                title="Add Comments",
                description="Add comments explaining complex logic",
                severity="info",
                impact_score=0.5,
                effort_score=0.3
            ))
        
        return suggestions

class OptimizationAnalyzer:
    """Analyzes optimization opportunities"""
    
    @staticmethod
    def analyze(code: str, metrics: CodeMetrics) -> List[OptimizationOpportunity]:
        """Find optimization opportunities"""
        opportunities: List[OptimizationOpportunity] = []
        
        # Loop optimization
        if code.count(' FOR ') > 2:
            opportunities.append(OptimizationOpportunity(
                title="Optimize Loop Operations",
                description="Move invariant operations outside loops",
                expected_speedup=1.3,
                complexity_reduction="Similar",
                current_approach="Computing values inside loops",
                suggested_approach="Pre-compute outside loops",
                resources=["Loop optimization guide"]
            ))
        
        # Early termination
        if code.count(' FOR ') > 0 and code.count(' IF ') > 2:
            opportunities.append(OptimizationOpportunity(
                title="Add Early Termination",
                description="Exit loops early when conditions are met",
                expected_speedup=1.2,
                complexity_reduction="Similar",
                resources=["Early exit patterns"]
            ))
        
        return opportunities

# ===== INTEGRATED REFACTORING SERVICE =====

class RefactoringService:
    """Complete refactoring analysis and suggestions"""
    
    def __init__(self):
        self.metrics_analyzer = CodeMetricsAnalyzer()
        self.smell_detector = CodeSmellDetector()
        self.duplication_detector = DuplicationDetector()
        self.refactoring_engine = RefactoringEngine()
        self.optimization_analyzer = OptimizationAnalyzer()
    
    def analyze_code(self, code: str) -> Dict:
        """Complete code analysis"""
        # Compute metrics
        metrics = self.metrics_analyzer.analyze(code)
        
        # Detect smells
        smells = self.smell_detector.detect(code, metrics)
        
        # Detect duplications
        duplications = self.duplication_detector.detect(code)
        
        # Generate suggestions
        suggestions = self.refactoring_engine.suggest(code, metrics, smells)
        
        # Find optimizations
        optimizations = self.optimization_analyzer.analyze(code, metrics)
        
        return {
            'metrics': metrics,
            'smells': smells,
            'duplications': duplications,
            'suggestions': suggestions,
            'optimizations': optimizations,
            'summary': {
                'code_quality_score': self._compute_quality_score(metrics, smells),
                'refactoring_priority': self._compute_priority(smells),
                'total_issues': len(smells) + len(duplications)
            }
        }
    
    def _compute_quality_score(self, metrics: CodeMetrics, smells: List[CodeSmell]) -> float:
        """Compute overall code quality score (0-100)"""
        score = 100.0
        
        # Penalize for complexity
        if metrics.cyclomatic_complexity > 10:
            score -= 20
        elif metrics.cyclomatic_complexity > 5:
            score -= 10
        
        # Penalize for smells
        score -= len(smells) * 5
        
        # Reward for comments
        if metrics.comment_ratio > 0.2:
            score += 10
        
        return max(0, min(100, score))
    
    def _compute_priority(self, smells: List[CodeSmell]) -> str:
        """Compute refactoring priority"""
        error_count = sum(1 for s in smells if s.severity == 'error')
        warning_count = sum(1 for s in smells if s.severity == 'warning')
        
        if error_count > 0:
            return "critical"
        elif warning_count > 2:
            return "high"
        elif warning_count > 0:
            return "medium"
        else:
            return "low"

# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    sample_code = """
    REM Complex calculation
    INPUT x
    INPUT y
    IF x > 0 THEN
        IF y > 0 THEN
            IF x + y > 100 THEN
                PRINT "Large"
            ELSE
                PRINT "Medium"
            END IF
        ELSE
            PRINT "Y is negative"
        END IF
    ELSE
        PRINT "X is negative"
    END IF
    """
    
    service = RefactoringService()
    analysis = service.analyze_code(sample_code)
    
    print(f"Quality Score: {analysis['summary']['code_quality_score']:.1f}/100")
    print(f"Issues Found: {analysis['summary']['total_issues']}")
    print(f"Priority: {analysis['summary']['refactoring_priority'].upper()}")
    print(f"\nMetrics:")
    print(f"  LOC: {analysis['metrics'].lines_of_code}")
    print(f"  Complexity: {analysis['metrics'].cyclomatic_complexity}")
    print(f"  Nesting: {analysis['metrics'].nesting_depth}")
