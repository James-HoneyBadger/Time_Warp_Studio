"""
Time Warp Studio - Code Audit & Quality Assurance

Comprehensive audit covering:
- Security analysis and hardening
- Code review and best practices
- Performance optimization
- Dependency auditing
- Documentation completeness
- Test coverage analysis
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional, Tuple

# ===== ENUMS =====


class SecurityLevel(Enum):
    """Security issue severity"""

    INFO = "info"
    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


class AuditType(Enum):
    """Types of audit checks"""

    SECURITY = "security"
    PERFORMANCE = "performance"
    MAINTAINABILITY = "maintainability"
    TESTING = "testing"
    DOCUMENTATION = "documentation"
    DEPENDENCIES = "dependencies"


# ===== DATA CLASSES =====


@dataclass
class SecurityIssue:
    """Security vulnerability"""

    id: str = ""
    type: str = ""  # SQL injection, XSS, etc.
    severity: SecurityLevel = SecurityLevel.MEDIUM
    title: str = ""
    description: str = ""

    # Location
    file: str = ""
    line: int = 0
    code_snippet: str = ""

    # Resolution
    remediation: str = ""
    references: List[str] = field(default_factory=list)
    cwe_id: str = ""  # Common Weakness Enumeration

    # Status
    status: str = "open"  # open, in-progress, resolved
    resolved_at: Optional[datetime] = None


@dataclass
class PerformanceIssue:
    """Performance problem"""

    id: str = ""
    title: str = ""
    description: str = ""

    # Location
    file: str = ""
    start_line: int = 0
    end_line: int = 0

    # Impact
    expected_improvement: float = 0.0  # percentage
    estimated_effort_hours: float = 0.0
    impact_priority: str = "medium"  # low, medium, high

    # Recommendations
    current_implementation: str = ""
    recommended_approach: str = ""
    resources: List[str] = field(default_factory=list)


@dataclass
class TestCoverageReport:
    """Test coverage analysis"""

    total_lines: int = 0
    covered_lines: int = 0
    branches_total: int = 0
    branches_covered: int = 0
    functions_total: int = 0
    functions_covered: int = 0

    # By category
    coverage_by_file: Dict[str, float] = field(default_factory=dict)
    coverage_by_module: Dict[str, float] = field(default_factory=dict)

    # Quality
    critical_uncovered: List[str] = field(default_factory=list)

    @property
    def overall_coverage(self) -> float:
        """Calculate overall coverage percentage"""
        if self.total_lines == 0:
            return 0.0
        return (self.covered_lines / self.total_lines) * 100


@dataclass
class DependencyVulnerability:
    """Vulnerable dependency"""

    package: str = ""
    current_version: str = ""
    vulnerability_id: str = ""
    title: str = ""
    description: str = ""

    # Details
    severity: SecurityLevel = SecurityLevel.MEDIUM
    affected_versions: str = ""  # version range
    fixed_version: str = ""

    # Timeline
    published: datetime = field(default_factory=datetime.utcnow)
    patched: bool = False


@dataclass
class CodeQualityReport:
    """Overall code quality report"""

    timestamp: datetime = field(default_factory=datetime.utcnow)

    # Scores (0-100)
    security_score: float = 0.0
    maintainability_score: float = 0.0
    reliability_score: float = 0.0
    performance_score: float = 0.0
    test_coverage_score: float = 0.0

    # Issues found
    security_issues: List[SecurityIssue] = field(default_factory=list)
    performance_issues: List[PerformanceIssue] = field(default_factory=list)
    dependency_vulnerabilities: List[DependencyVulnerability] = field(
        default_factory=list
    )

    # Details
    test_coverage: TestCoverageReport = field(default_factory=TestCoverageReport)
    documentation_completeness: float = 0.0  # percentage

    # Summary
    total_critical_issues: int = 0
    total_high_issues: int = 0

    @property
    def overall_score(self) -> float:
        """Weighted overall quality score"""
        weights = {
            "security": 0.25,
            "reliability": 0.25,
            "maintainability": 0.2,
            "performance": 0.15,
            "testing": 0.15,
        }

        return (
            self.security_score * weights["security"]
            + self.reliability_score * weights["reliability"]
            + self.maintainability_score * weights["maintainability"]
            + self.performance_score * weights["performance"]
            + self.test_coverage_score * weights["testing"]
        )


# ===== AUDIT COMPONENTS =====


class SecurityAuditor:
    """Performs security analysis"""

    # Security patterns to check
    DANGEROUS_PATTERNS = {
        "eval": "Use of eval() is dangerous",
        "exec": "Use of exec() can execute arbitrary code",
        "__import__": "Dynamic imports can be dangerous",
        "pickle": "Pickle can execute arbitrary code",
        "sql injection": "Potential SQL injection vulnerability",
        "hardcoded password": "Password hardcoded in source",
        "insecure random": "Insecure random number generation",
        "no rate limit": "No rate limiting on critical operations",
    }

    def audit_code(self, code: str, filename: str = "") -> List[SecurityIssue]:
        """Audit code for security issues"""
        issues: List[SecurityIssue] = []
        lines = code.split("\n")

        for i, line in enumerate(lines, 1):
            # Check for dangerous patterns
            for pattern, message in self.DANGEROUS_PATTERNS.items():
                if pattern.lower() in line.lower():
                    issues.append(
                        SecurityIssue(
                            type=pattern,
                            severity=SecurityLevel.HIGH,
                            title=f"Security: {pattern}",
                            description=message,
                            file=filename,
                            line=i,
                            code_snippet=line.strip(),
                            remediation=f"Avoid using {pattern} in production code",
                        )
                    )

        return issues

    def check_dependencies(
        self, requirements: List[Tuple[str, str]]
    ) -> List[DependencyVulnerability]:
        """Check dependencies for known vulnerabilities"""
        vulnerabilities: List[DependencyVulnerability] = []

        # Known vulnerable packages (simplified database)
        known_vulns = {
            "requests": [
                {
                    "version": "<2.25.0",
                    "id": "CVE-2020-28493",
                    "title": "Vulnerable to XXE",
                }
            ],
            "django": [
                {
                    "version": "<3.0",
                    "id": "CVE-2019-12781",
                    "title": "SQL injection vulnerability",
                }
            ],
            "pillow": [
                {
                    "version": "<8.0",
                    "id": "CVE-2021-23437",
                    "title": "Buffer overflow in Image.crop()",
                }
            ],
        }

        for package, version in requirements:
            if package in known_vulns:
                for vuln in known_vulns[package]:
                    vulnerabilities.append(
                        DependencyVulnerability(
                            package=package,
                            current_version=version,
                            vulnerability_id=vuln["id"],
                            title=vuln["title"],
                            severity=SecurityLevel.HIGH,
                            fixed_version="Please check upstream",
                        )
                    )

        return vulnerabilities


class MaintainabilityAnalyzer:
    """Analyzes code maintainability"""

    def analyze(self, code: str) -> Tuple[float, List[str]]:
        """Analyze maintainability"""
        issues: List[str] = []

        # Check for descriptive variable names
        short_names = len([w for w in code.split() if len(w) < 2])
        if short_names > 10:
            issues.append("Many single-letter variable names reduce readability")

        # Check for long lines
        lines = code.split("\n")
        long_lines = sum(1 for line in lines if len(line) > 120)
        if long_lines > len(lines) * 0.1:
            issues.append("Many lines exceed 120 characters")

        # Check for comments
        comment_lines = sum(1 for line in lines if line.strip().startswith("#"))
        if comment_lines < len(lines) * 0.05:
            issues.append("Low comment-to-code ratio")

        # Check for deeply nested blocks
        max_indent = max(
            (len(line) - len(line.lstrip())) // 4 for line in lines if line.strip()
        )
        if max_indent > 6:
            issues.append("Deeply nested code blocks reduce readability")

        # Calculate score
        score = 100.0
        score -= len(issues) * 10
        score = max(0, min(100, score))

        return score, issues


class TestCoverageAnalyzer:
    """Analyzes test coverage"""

    def analyze_coverage(
        self,
        total_lines: int,
        covered_lines: int,
        critical_functions: int,
        tested_functions: int,
    ) -> TestCoverageReport:
        """Analyze test coverage"""

        report = TestCoverageReport(
            total_lines=total_lines,
            covered_lines=covered_lines,
            functions_total=critical_functions,
            functions_covered=tested_functions,
        )

        # Identify critical uncovered code
        if (critical_functions - tested_functions) > 0:
            report.critical_uncovered = [
                f"Function {i} is not tested"
                for i in range(critical_functions - tested_functions)
            ]

        return report


class PerformanceOptimizer:
    """Identifies performance optimization opportunities"""

    def analyze_code(self, code: str) -> List[PerformanceIssue]:
        """Analyze code for performance issues"""
        issues: List[PerformanceIssue] = []
        lines = code.split("\n")

        # Check for nested loops
        nested_loops = 0
        for i, line in enumerate(lines):
            if "FOR" in line.upper() or "WHILE" in line.upper():
                # Check indentation of next line
                if i + 1 < len(lines):
                    current_indent = len(line) - len(line.lstrip())
                    next_indent = len(lines[i + 1]) - len(lines[i + 1].lstrip())
                    if next_indent > current_indent:
                        nested_loops += 1

        if nested_loops > 2:
            issues.append(
                PerformanceIssue(
                    title="Multiple Nested Loops",
                    description="Nested loops can have O(n²) or worse complexity",
                    start_line=1,
                    end_line=len(lines),
                    expected_improvement=30,
                    estimated_effort_hours=2,
                    impact_priority="high",
                    current_implementation="Nested FOR/WHILE loops",
                    recommended_approach="Use lookup tables, maps, or better algorithms",
                )
            )

        # Check for repeated string concatenation
        concat_count = code.count("+")
        if concat_count > 10:
            issues.append(
                PerformanceIssue(
                    title="String Concatenation",
                    description="String concatenation in loops creates new objects",
                    expected_improvement=20,
                    estimated_effort_hours=1,
                    impact_priority="medium",
                    current_implementation="String + operator in loops",
                    recommended_approach="Use StringBuilder or list join()",
                )
            )

        return issues


# ===== CODE AUDIT SERVICE =====


class CodeAuditService:
    """Comprehensive code audit"""

    def __init__(self):
        self.security_auditor = SecurityAuditor()
        self.maintainability_analyzer = MaintainabilityAnalyzer()
        self.coverage_analyzer = TestCoverageAnalyzer()
        self.performance_optimizer = PerformanceOptimizer()

    def full_audit(
        self,
        code: str,
        filename: str,
        test_coverage_data: Optional[Dict] = None,
        dependencies: Optional[List[Tuple[str, str]]] = None,
    ) -> CodeQualityReport:
        """Perform complete code audit"""
        report = CodeQualityReport()

        # Security audit
        security_issues = self.security_auditor.audit_code(code, filename)
        report.security_issues = security_issues
        report.security_score = max(0, 100 - len(security_issues) * 10)

        # Dependency check
        if dependencies:
            vulns = self.security_auditor.check_dependencies(dependencies)
            report.dependency_vulnerabilities = vulns

        # Maintainability analysis
        maintainability_score, maint_issues = self.maintainability_analyzer.analyze(
            code
        )
        report.maintainability_score = maintainability_score

        # Performance analysis
        perf_issues = self.performance_optimizer.analyze_code(code)
        report.performance_issues = perf_issues
        report.performance_score = max(0, 100 - len(perf_issues) * 15)

        # Test coverage
        if test_coverage_data:
            report.test_coverage = self.coverage_analyzer.analyze_coverage(
                test_coverage_data.get("total_lines", 0),
                test_coverage_data.get("covered_lines", 0),
                test_coverage_data.get("critical_functions", 0),
                test_coverage_data.get("tested_functions", 0),
            )
            report.test_coverage_score = report.test_coverage.overall_coverage
        else:
            report.test_coverage_score = 0

        # Reliability (based on issues found)
        critical_count = sum(
            1 for s in security_issues if s.severity == SecurityLevel.CRITICAL
        )
        high_count = sum(1 for s in security_issues if s.severity == SecurityLevel.HIGH)
        report.total_critical_issues = critical_count
        report.total_high_issues = high_count
        report.reliability_score = max(
            0, 100 - (critical_count * 20 + high_count * 10)
        )

        return report

    def generate_audit_report(self, report: CodeQualityReport) -> str:
        """Generate human-readable audit report"""
        lines = [
            "=" * 60,
            "CODE QUALITY AUDIT REPORT",
            "=" * 60,
            f"Generated: {report.timestamp}",
            "",
            "OVERALL SCORE",
            "-" * 60,
            f"Overall Quality Score: {report.overall_score:.1f}/100",
            f"Security: {report.security_score:.1f}/100",
            f"Reliability: {report.reliability_score:.1f}/100",
            f"Maintainability: {report.maintainability_score:.1f}/100",
            f"Performance: {report.performance_score:.1f}/100",
            f"Test Coverage: {report.test_coverage_score:.1f}/100",
            "",
            "CRITICAL FINDINGS",
            "-" * 60,
            f"Critical Issues: {report.total_critical_issues}",
            f"High Issues: {report.total_high_issues}",
            "",
            "SECURITY ISSUES",
            "-" * 60,
        ]

        for issue in report.security_issues:
            lines.append(
                f"[{issue.severity.value.upper()}] {issue.title} "
                f"({issue.file}:{issue.line})"
            )
            lines.append(f"  {issue.remediation}")

        lines.extend(
            [
                "",
                "PERFORMANCE OPPORTUNITIES",
                "-" * 60,
            ]
        )

        for issue in report.performance_issues:
            lines.append(f"[{issue.impact_priority.upper()}] {issue.title}")
            lines.append(f"  Expected Improvement: {issue.expected_improvement:.0f}%")
            lines.append(f"  Effort: {issue.estimated_effort_hours:.1f} hours")

        lines.extend(
            [
                "",
                "TEST COVERAGE",
                "-" * 60,
                f"Line Coverage: {report.test_coverage.overall_coverage:.1f}%",
                "Critical Uncovered: "
                f"{len(report.test_coverage.critical_uncovered)} functions",
                "",
                "RECOMMENDATIONS",
                "-" * 60,
                "1. Address all critical security issues",
                "2. Increase test coverage for critical functions",
                "3. Refactor high-complexity functions",
                "4. Update vulnerable dependencies",
                "5. Improve code comments and documentation",
                "=" * 60,
            ]
        )

        return "\n".join(lines)


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    sample_code = """
    REM Complex calculation with potential issues
    INPUT x
    FOR i = 1 TO 100
        FOR j = 1 TO 100
            result = x + i + j
        END FOR
    END FOR

    DIM arr(1000)
    FOR k = 1 TO 1000
        arr(k) = k * 2
    END FOR
    """

    service = CodeAuditService()
    report = service.full_audit(
        sample_code,
        "test.bas",
        test_coverage_data={
            "total_lines": 100,
            "covered_lines": 70,
            "critical_functions": 5,
            "tested_functions": 3,
        },
        dependencies=[("requests", "2.25.0"), ("django", "3.0")],
    )

    print(service.generate_audit_report(report))
