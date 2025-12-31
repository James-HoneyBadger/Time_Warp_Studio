"""
Code snippets library for Time Warp IDE.
Provides insertable code templates for common patterns in all supported languages.
"""

from dataclasses import dataclass
from typing import Dict, List, Optional


@dataclass
class CodeSnippet:
    """A code snippet template."""

    name: str
    description: str
    code: str
    language: str  # BASIC, PILOT, LOGO, or ALL
    category: str
    cursor_position: Optional[int] = None  # Where to place cursor after insert


# BASIC Snippets
BASIC_SNIPPETS: List[CodeSnippet] = [
    CodeSnippet(
        name="Hello World",
        description="Simple Hello World program",
        code='10 PRINT "Hello, World!"\n20 END',
        language="BASIC",
        category="Basics",
    ),
    CodeSnippet(
        name="FOR Loop",
        description="Basic FOR...NEXT loop",
        code="10 FOR I = 1 TO 10\n20   PRINT I\n30 NEXT I",
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="WHILE Loop",
        description="WHILE...WEND loop",
        code=(
            "10 X = 1\n"
            "20 WHILE X <= 10\n"
            "30   PRINT X\n"
            "40   X = X + 1\n"
            "50 WEND"
        ),
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="DO...LOOP UNTIL",
        description="DO...LOOP with UNTIL condition",
        code=(
            "10 X = 1\n"
            "20 DO\n"
            "30   PRINT X\n"
            "40   X = X + 1\n"
            "50 LOOP UNTIL X > 10"
        ),
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="IF...THEN...ELSE",
        description="Conditional statement",
        code=(
            '10 INPUT "Enter a number: "; N\n'
            '20 IF N > 0 THEN PRINT "Positive"\n'
            '30 IF N < 0 THEN PRINT "Negative"\n'
            '40 IF N = 0 THEN PRINT "Zero"'
        ),
        language="BASIC",
        category="Control Flow",
    ),
    CodeSnippet(
        name="Subroutine",
        description="GOSUB subroutine pattern",
        code=(
            '10 PRINT "Start"\n'
            "20 GOSUB 100\n"
            '30 PRINT "End"\n'
            "40 END\n"
            "100 REM Subroutine\n"
            '110 PRINT "In subroutine"\n'
            "120 RETURN"
        ),
        language="BASIC",
        category="Subroutines",
    ),
    CodeSnippet(
        name="Input Example",
        description="Get user input",
        code=(
            '10 INPUT "What is your name? "; NAME$\n'
            '20 PRINT "Hello, "; NAME$'
        ),
        language="BASIC",
        category="Input/Output",
    ),
    CodeSnippet(
        name="Array Usage",
        description="Declare and use an array",
        code=(
            "10 DIM A(10)\n"
            "20 FOR I = 0 TO 10\n"
            "30   A(I) = I * 2\n"
            "40 NEXT I\n"
            "50 FOR I = 0 TO 10\n"
            "60   PRINT A(I)\n"
            "70 NEXT I"
        ),
        language="BASIC",
        category="Data Structures",
    ),
    CodeSnippet(
        name="DATA/READ",
        description="Use DATA and READ statements",
        code=(
            '10 DATA 10, 20, 30, "Hello", "World"\n'
            "20 FOR I = 1 TO 3\n"
            "30   READ N\n"
            "40   PRINT N\n"
            "50 NEXT I\n"
            "60 READ A$, B$\n"
            '70 PRINT A$; " "; B$'
        ),
        language="BASIC",
        category="Data Structures",
    ),
    CodeSnippet(
        name="Random Number",
        description="Generate random numbers",
        code=(
            "10 FOR I = 1 TO 5\n"
            "20   R = INT(RND * 100) + 1\n"
            "30   PRINT R\n"
            "40 NEXT I"
        ),
        language="BASIC",
        category="Math",
    ),
    CodeSnippet(
        name="Line Drawing",
        description="Draw lines on screen",
        code=(
            "10 CLS\n"
            "20 LINE (0, 0)-(100, 100)\n"
            "30 LINE (100, 0)-(0, 100)\n"
            "40 LINE (50, 0)-(50, 100)"
        ),
        language="BASIC",
        category="Graphics",
    ),
    CodeSnippet(
        name="Circle Drawing",
        description="Draw circles on screen",
        code=(
            "10 CLS\n"
            "20 CIRCLE (0, 0), 50\n"
            "30 CIRCLE (0, 0), 75\n"
            "40 CIRCLE (0, 0), 100"
        ),
        language="BASIC",
        category="Graphics",
    ),
    CodeSnippet(
        name="Game Loop (INKEY$)",
        description="Basic game loop with keyboard input",
        code=(
            "10 CLS\n"
            "20 X = 160\n"
            "30 Y = 100\n"
            "40 REM Game Loop\n"
            "50 K$ = INKEY$\n"
            '60 IF K$ = "w" THEN Y = Y - 5\n'
            '70 IF K$ = "s" THEN Y = Y + 5\n'
            '80 IF K$ = "a" THEN X = X - 5\n'
            '90 IF K$ = "d" THEN X = X + 5\n'
            '100 IF K$ = "q" THEN END\n'
            "110 CIRCLE (X, Y), 10\n"
            "120 GOTO 50"
        ),
        language="BASIC",
        category="Games",
    ),
    CodeSnippet(
        name="Timer Example",
        description="Using TIMER for timing",
        code=(
            "10 T = TIMER\n"
            '20 PRINT "Press any key..."\n'
            '30 K$ = ""\n'
            '40 WHILE K$ = ""\n'
            "50   K$ = INKEY$\n"
            "60 WEND\n"
            '70 PRINT "Elapsed: "; TIMER - T; " seconds"'
        ),
        language="BASIC",
        category="Games",
    ),
    CodeSnippet(
        name="Sound Effect",
        description="Play sound effects",
        code=(
            "10 BEEP\n"
            "20 SOUND 440, 10\n"
            "30 SOUND 880, 10\n"
            "40 SOUND 1320, 10"
        ),
        language="BASIC",
        category="Sound",
    ),
]

# PILOT Snippets
PILOT_SNIPPETS: List[CodeSnippet] = [
    CodeSnippet(
        name="Hello World",
        description="Simple Hello World in PILOT",
        code="T:Hello, World!\nE:",
        language="PILOT",
        category="Basics",
    ),
    CodeSnippet(
        name="Question/Answer",
        description="Simple quiz pattern",
        code=(
            "T:What is 2 + 2?\n"
            "A:\n"
            "M:4,four\n"
            "TY:Correct!\n"
            "TN:The answer is 4.\n"
            "E:"
        ),
        language="PILOT",
        category="Basics",
    ),
    CodeSnippet(
        name="Multiple Choice Quiz",
        description="Multiple choice question",
        code=(
            "T:What color is the sky?\n"
            "T:A) Red  B) Blue  C) Green\n"
            "A:\n"
            "M:B,b,blue\n"
            "TY:Correct! The sky is blue.\n"
            "TN:Sorry, the answer is B) Blue.\n"
            "E:"
        ),
        language="PILOT",
        category="Education",
    ),
    CodeSnippet(
        name="Loop with Counter",
        description="Loop using compute and jump",
        code=(
            "C:COUNT=0\n"
            "*LOOP\n"
            "C:COUNT=COUNT+1\n"
            "T:Count is *COUNT*\n"
            "J(COUNT<5):*LOOP\n"
            "T:Done counting!\n"
            "E:"
        ),
        language="PILOT",
        category="Control Flow",
    ),
    CodeSnippet(
        name="Math Quiz",
        description="Simple addition quiz",
        code=(
            "C:A=5\n"
            "C:B=3\n"
            "T:What is *A* + *B*?\n"
            "A:\n"
            "M:8,eight\n"
            "TY:Correct! *A* + *B* = 8\n"
            "TN:Sorry, *A* + *B* = 8\n"
            "E:"
        ),
        language="PILOT",
        category="Education",
    ),
    CodeSnippet(
        name="Subroutine Pattern",
        description="Using subroutine calls",
        code=(
            "T:Starting program\n"
            "S:*GREET\n"
            "T:Back to main\n"
            "E:\n"
            "*GREET\n"
            "T:Hello from subroutine!\n"
            "R:"
        ),
        language="PILOT",
        category="Subroutines",
    ),
]

# Logo Snippets
LOGO_SNIPPETS: List[CodeSnippet] = [
    CodeSnippet(
        name="Square",
        description="Draw a square",
        code="REPEAT 4 [FD 100 RT 90]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Triangle",
        description="Draw an equilateral triangle",
        code="REPEAT 3 [FD 100 RT 120]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Circle",
        description="Draw a circle using small steps",
        code="REPEAT 360 [FD 1 RT 1]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Star",
        description="Draw a 5-pointed star",
        code="REPEAT 5 [FD 100 RT 144]",
        language="LOGO",
        category="Shapes",
    ),
    CodeSnippet(
        name="Spiral Square",
        description="Draw a square spiral",
        code=(
            "TO SPIRAL :SIZE\n"
            "  IF :SIZE > 200 [STOP]\n"
            "  FD :SIZE RT 90\n"
            "  SPIRAL :SIZE + 5\n"
            "END\n"
            "SPIRAL 5"
        ),
        language="LOGO",
        category="Patterns",
    ),
    CodeSnippet(
        name="Recursive Tree",
        description="Draw a recursive tree",
        code=(
            "TO TREE :SIZE\n"
            "  IF :SIZE < 5 [STOP]\n"
            "  FD :SIZE\n"
            "  LT 30 TREE :SIZE * 0.7\n"
            "  RT 60 TREE :SIZE * 0.7\n"
            "  LT 30 BK :SIZE\n"
            "END\n"
            "TREE 50"
        ),
        language="LOGO",
        category="Patterns",
    ),
    CodeSnippet(
        name="Colorful Pattern",
        description="Draw with changing colors",
        code="REPEAT 36 [\n  SETPC [255 0 0]\n  FD 100\n  RT 170\n]",
        language="LOGO",
        category="Patterns",
    ),
    CodeSnippet(
        name="Procedure Example",
        description="Define and use a procedure",
        code=(
            "TO HEXAGON :SIZE\n"
            "  REPEAT 6 [FD :SIZE RT 60]\n"
            "END\n"
            "HEXAGON 50\n"
            "RT 60\n"
            "HEXAGON 50"
        ),
        language="LOGO",
        category="Procedures",
    ),
    CodeSnippet(
        name="Flower Pattern",
        description="Draw a flower using circles",
        code="REPEAT 36 [\n  REPEAT 360 [FD 0.5 RT 1]\n  RT 10\n]",
        language="LOGO",
        category="Patterns",
    ),
]


class SnippetLibrary:
    """Manager for code snippets."""

    def __init__(self):
        self.snippets: Dict[str, List[CodeSnippet]] = {
            "BASIC": BASIC_SNIPPETS.copy(),
            "PILOT": PILOT_SNIPPETS.copy(),
            "LOGO": LOGO_SNIPPETS.copy(),
        }

    def get_snippets(self, language: str) -> List[CodeSnippet]:
        """Get all snippets for a language."""
        return self.snippets.get(language.upper(), [])

    def get_categories(self, language: str) -> List[str]:
        """Get all categories for a language."""
        snippets = self.get_snippets(language)
        categories = sorted(set(s.category for s in snippets))
        return categories

    def get_by_category(
        self,
        language: str,
        category: str,
    ) -> List[CodeSnippet]:
        """Get snippets filtered by category."""
        snippets = self.get_snippets(language)
        return [s for s in snippets if s.category == category]

    def search(self, language: str, query: str) -> List[CodeSnippet]:
        """Search snippets by name or description."""
        snippets = self.get_snippets(language)
        query_lower = query.lower()
        return [
            s
            for s in snippets
            if query_lower in s.name.lower()
            or query_lower in s.description.lower()
        ]

    def add_snippet(self, snippet: CodeSnippet):
        """Add a custom snippet."""
        lang = snippet.language.upper()
        if lang not in self.snippets:
            self.snippets[lang] = []
        self.snippets[lang].append(snippet)


# Global singleton
_snippet_library: Optional[SnippetLibrary] = None


def get_snippet_library() -> SnippetLibrary:
    """Get or create the global snippet library."""
    global _snippet_library  # pylint: disable=global-statement
    if _snippet_library is None:
        _snippet_library = SnippetLibrary()
    return _snippet_library
