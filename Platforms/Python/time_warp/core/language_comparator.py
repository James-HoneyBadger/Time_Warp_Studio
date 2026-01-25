"""Multi-language comparison pane for side-by-side code execution."""

from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Optional, Tuple

from .interpreter import Interpreter, Language


class ComparisonMode(Enum):
    """How to compare code execution."""

    SYNTAX = "syntax"  # Show syntax highlighting for both
    EXECUTION = "execution"  # Show output side-by-side
    TIMING = "timing"  # Compare execution times
    VISUALIZATION = "visualization"  # Show turtle graphics together


@dataclass
class LanguageComparison:
    """Comparison result for two language implementations."""

    language1: Language
    language2: Language
    code1: str
    code2: str
    output1: str
    output2: str
    time1: float  # Execution time in ms
    time2: float
    differences: List[str]  # Key differences found
    mode: ComparisonMode


class MultiLanguageComparator:
    """Compares equivalent programs in different languages."""

    def __init__(self):
        """Initialize comparator with interpreter."""
        self.interpreter = Interpreter()
        self._builtin_pairs = self._initialize_builtin_pairs()

    def _initialize_builtin_pairs(
        self,
    ) -> Dict[str, Tuple[str, str, str, str]]:
        """Initialize built-in language pair examples.

        Returns: Dict[key] = (lang1, lang2, code1, code2)
        """
        pairs = {}

        # Hello World comparison
        pairs["hello_world"] = (
            "BASIC",
            "LOGO",
            '10 PRINT "Hello, World!"\n20 END',
            "PRINT [Hello World]",
        )

        # Loops comparison
        pairs["loop_count"] = (
            "BASIC",
            "LOGO",
            "10 FOR I = 1 TO 5\n20 PRINT I\n30 NEXT I\n40 END",
            "REPEAT 5 [PRINT REPCOUNT]",
        )

        # Square drawing comparison
        pairs["square"] = (
            "BASIC",
            "LOGO",
            """10 REM Draw square using coordinates
20 PRINT "Drawing a square"
30 FOR I = 1 TO 4
40 PRINT "Side "; I
50 NEXT I
60 END""",
            """TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

SQUARE 100""",
        )

        # Variables and calculation
        pairs["calculation"] = (
            "BASIC",
            "C",
            """10 X = 5
20 Y = 10
30 Z = X + Y
40 PRINT "Sum is "; Z
50 END""",
            """#include <stdio.h>
int main() {
  int x = 5, y = 10;
  int z = x + y;
  printf("Sum is %d\\n", z);
  return 0;
}""",
        )

        # Conditional logic
        pairs["conditional"] = (
            "BASIC",
            "PILOT",
            """10 INPUT "Enter a number: ", N
20 IF N > 0 THEN PRINT "Positive"
30 IF N < 0 THEN PRINT "Negative"
40 IF N = 0 THEN PRINT "Zero"
50 END""",
            """T: Enter a number
U: ACCEPT
A: What is the sign?
Y: POSITIVE
T: It's positive!
JA: DONE
N:
T: It's negative or zero.
*: DONE""",
        )

        return pairs

    def get_builtin_pairs(self) -> Dict[str, Tuple[str, str]]:
        """Get available built-in comparison pairs."""
        return {k: (v[0], v[1]) for k, v in self._builtin_pairs.items()}

    def get_builtin_pair(self, pair_id: str) -> Optional[Tuple[str, str, str, str]]:
        """Get a specific built-in pair by ID."""
        return self._builtin_pairs.get(pair_id)

    def compare(
        self,
        language1: Language,
        code1: str,
        language2: Language,
        code2: str,
        mode: ComparisonMode = ComparisonMode.EXECUTION,
    ) -> LanguageComparison:
        """
        Compare two language implementations.

        Args:
            language1: First language
            code1: Code in first language
            language2: Second language
            code2: Code in second language
            mode: Comparison mode (execution, syntax, etc.)

        Returns:
            LanguageComparison with results
        """
        # Execute both
        result1 = self.interpreter.execute(code1, language1)
        result2 = self.interpreter.execute(code2, language2)

        # Parse results
        output1 = result1.get("output", "")
        output2 = result2.get("output", "")
        error1 = result1.get("error")
        error2 = result2.get("error")
        time1 = result1.get("execution_time", 0.0)
        time2 = result2.get("execution_time", 0.0)

        # Find differences
        differences = self._analyze_differences(
            language1,
            language2,
            code1,
            code2,
            output1,
            output2,
            error1,
            error2,
        )

        return LanguageComparison(
            language1=language1,
            language2=language2,
            code1=code1,
            code2=code2,
            output1=output1 if not error1 else f"ERROR: {error1}",
            output2=output2 if not error2 else f"ERROR: {error2}",
            time1=time1,
            time2=time2,
            differences=differences,
            mode=mode,
        )

    def _analyze_differences(
        self,
        lang1: Language,
        lang2: Language,
        code1: str,
        code2: str,
        out1: str,
        out2: str,
        err1: Optional[str],
        err2: Optional[str],
    ) -> List[str]:
        """Analyze and document differences between implementations."""
        differences = []

        # Check for errors
        if err1 and not err2:
            differences.append(f"❌ {lang1.value} has error, {lang2.value} succeeds")
        elif err2 and not err1:
            differences.append(f"❌ {lang2.value} has error, {lang1.value} succeeds")
        elif err1 and err2:
            differences.append("⚠️ Both have errors (different languages?)")

        # Check output differences
        if out1.strip() != out2.strip():
            differences.append("📊 Different output")
            if len(out1) != len(out2):
                differences.append(f"   Length: {len(out1)} vs {len(out2)} chars")
        else:
            differences.append("✅ Output matches")

        # Code structure differences
        lines1 = len(code1.split("\n"))
        lines2 = len(code2.split("\n"))
        if lines1 != lines2:
            differences.append(f"📝 Code length: {lines1} vs {lines2} lines")

        # Language paradigm differences
        paradigm_notes = self._get_paradigm_notes(lang1, lang2)
        differences.extend(paradigm_notes)

        return differences

    def _get_paradigm_notes(self, lang1: Language, lang2: Language) -> List[str]:
        """Get notes about language paradigm differences."""
        notes = []

        cats1 = self._categorize_language(lang1)
        cats2 = self._categorize_language(lang2)

        if "procedural" in cats1 and "declarative" in cats2:
            notes.append("🔄 Procedural vs Declarative paradigm")
        elif "graphics" in cats1 and "graphics" in cats2:
            notes.append("🎨 Both have graphics capabilities")
        elif "graphics" in cats1:
            notes.append(f"🎨 {lang1.value} has graphics, {lang2.value} does not")
        elif "graphics" in cats2:
            notes.append(f"🎨 {lang2.value} has graphics, {lang1.value} does not")

        return notes

    def _categorize_language(self, language: Language) -> set:
        """Categorize a language by its characteristics."""
        categories = set()

        if language in [Language.BASIC, Language.C, Language.PASCAL]:
            categories.add("procedural")

        if language in [Language.PROLOG]:
            categories.add("declarative")

        if language in [Language.LOGO]:
            categories.add("graphics")

        return categories

    def get_comparison_metrics(self, comparison: LanguageComparison) -> Dict[str, any]:
        """Get metrics about the comparison."""
        return {
            "languages": f"{comparison.language1.value} vs {comparison.language2.value}",
            "code_lines": (
                len(comparison.code1.split("\n")),
                len(comparison.code2.split("\n")),
            ),
            "output_length": (
                len(comparison.output1),
                len(comparison.output2),
            ),
            "execution_time": (comparison.time1, comparison.time2),
            "time_ratio": (
                comparison.time2 / comparison.time1 if comparison.time1 > 0 else 0
            ),
            "output_matches": comparison.output1.strip() == comparison.output2.strip(),
            "difference_count": len(comparison.differences),
        }


class ComparisonRenderer:
    """Renders comparison results for display."""

    @staticmethod
    def render_html(comparison: LanguageComparison) -> str:
        """Render comparison as HTML."""
        metrics = MultiLanguageComparator().get_comparison_metrics(comparison)

        html = """
        <html>
        <head>
            <title>Language Comparison</title>
            <style>
                body {{ font-family: monospace; margin: 10px; }}
                .header {{ background: #f0f0f0; padding: 10px; margin: 10px 0; }}
                .code {{ background: #fff; border: 1px solid #ccc; padding: 10px; margin: 10px 0; }}
                .output {{ background: #f9f9f9; border-left: 3px solid #0066cc; padding: 10px; }}
                .diff {{ color: #cc0000; }}
                .match {{ color: #00cc00; }}
                table {{ width: 100%; border-collapse: collapse; }}
                th, td {{ padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }}
            </style>
        </head>
        <body>
            <h2>Language Comparison Report</h2>

            <div class="header">
                <strong>{comparison.language1.value}</strong> vs <strong>{comparison.language2.value}</strong>
            </div>

            <table>
                <tr>
                    <th>{comparison.language1.value}</th>
                    <th>{comparison.language2.value}</th>
                </tr>
                <tr>
                    <td class="code"><pre>{comparison.code1}</pre></td>
                    <td class="code"><pre>{comparison.code2}</pre></td>
                </tr>
                <tr>
                    <td class="output"><strong>Output:</strong><br>{comparison.output1}</td>
                    <td class="output"><strong>Output:</strong><br>{comparison.output2}</td>
                </tr>
            </table>

            <h3>Analysis</h3>
            <ul>
        """

        for diff in comparison.differences:
            if "✅" in diff:
                html += f'<li class="match">{diff}</li>'
            else:
                html += f"<li>{diff}</li>"

        html += """
            </ul>

            <h3>Metrics</h3>
            <table>
                <tr>
                    <th>Metric</th>
                    <th>Value</th>
                </tr>
        """

        metrics_list = [
            ("Execution Time 1", f"{metrics['execution_time'][0]:.3f}ms"),
            ("Execution Time 2", f"{metrics['execution_time'][1]:.3f}ms"),
            ("Time Ratio", f"{metrics['time_ratio']:.2f}x"),
            ("Output Match", "Yes" if metrics["output_matches"] else "No"),
        ]

        for metric_name, metric_val in metrics_list:
            html += f"<tr><td>{metric_name}</td><td>{metric_val}</td></tr>"

        html += """
            </table>
        </body>
        </html>
        """
        return html

    @staticmethod
    def render_text(comparison: LanguageComparison) -> str:
        """Render comparison as plain text."""
        lines = []
        lines.append("=" * 70)
        lines.append(f"LANGUAGE COMPARISON: {
                comparison.language1.value} vs {
                comparison.language2.value}")
        lines.append("=" * 70)
        lines.append("")

        lines.append(f"[{comparison.language1.value} CODE]")
        lines.append("-" * 35)
        lines.append(comparison.code1)
        lines.append("")

        lines.append(f"[{comparison.language2.value} CODE]")
        lines.append("-" * 35)
        lines.append(comparison.code2)
        lines.append("")

        lines.append("[OUTPUT COMPARISON]")
        lines.append("-" * 35)
        lines.append(f"{comparison.language1.value} Output: {comparison.output1}")
        lines.append(f"{comparison.language2.value} Output: {comparison.output2}")
        lines.append("")

        lines.append("[ANALYSIS]")
        for diff in comparison.differences:
            lines.append(f"  • {diff}")

        return "\n".join(lines)
