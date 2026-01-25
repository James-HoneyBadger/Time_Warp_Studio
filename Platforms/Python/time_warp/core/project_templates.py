"""Project templates system for quick-start projects."""

import json
from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Optional


class TemplateCategory(Enum):
    """Categories of project templates."""

    GAME = "game"
    DATA_VIZ = "data_visualization"
    ROBOTICS = "robotics"
    ART = "art_generation"
    LEARNING = "learning"
    DEMO = "demo"


@dataclass
class Template:
    """Project template definition."""

    name: str
    description: str
    category: TemplateCategory
    language: str  # 'BASIC', 'LOGO', 'PILOT', etc.
    difficulty: str  # 'beginner', 'intermediate', 'advanced'
    code: str  # Template code
    tags: List[str]
    author: Optional[str] = None
    min_version: str = "5.1.0"


class TemplateLibrary:
    """Manages built-in project templates."""

    # Built-in templates
    TEMPLATES: Dict[str, Template] = {}

    @classmethod
    def initialize(cls):
        """Initialize template library with built-in templates."""

        # BASIC: Guessing Game
        cls.TEMPLATES["basic_guessing_game"] = Template(
            name="Number Guessing Game",
            description="Interactive game where the player guesses a random number",
            category=TemplateCategory.GAME,
            language="BASIC",
            difficulty="beginner",
            code="""10 REM Number Guessing Game
20 RANDOMIZE
30 SECRET = INT(RND * 100) + 1
40 GUESSES = 0
50 PRINT "I'm thinking of a number 1-100"
60 PRINT "Try to guess it!"
70 PRINT
80 INPUT "Enter your guess: ", GUESS
90 GUESSES = GUESSES + 1
100 IF GUESS = SECRET THEN GOTO 200
110 IF GUESS < SECRET THEN PRINT "Too low, try again"
120 IF GUESS > SECRET THEN PRINT "Too high, try again"
130 PRINT
140 GOTO 80
200 PRINT
210 PRINT "You got it! The number was "; SECRET
220 PRINT "You guessed it in "; GUESSES; " tries"
230 END
""",
            tags=["game", "loops", "conditionals", "random"],
        )

        # BASIC: Calculator
        cls.TEMPLATES["basic_calculator"] = Template(
            name="Simple Calculator",
            description="A basic arithmetic calculator",
            category=TemplateCategory.LEARNING,
            language="BASIC",
            difficulty="beginner",
            code="""10 REM Simple Calculator
20 PRINT "=== Simple Calculator ==="
30 PRINT
40 INPUT "Enter first number: ", A
50 INPUT "Enter operator (+, -, *, /): ", OP$
60 INPUT "Enter second number: ", B
70 PRINT
80 IF OP$ = "+" THEN RESULT = A + B
90 IF OP$ = "-" THEN RESULT = A - B
100 IF OP$ = "*" THEN RESULT = A * B
110 IF OP$ = "/" THEN RESULT = A / B
120 PRINT A; " "; OP$; " "; B; " = "; RESULT
130 END
""",
            tags=["calculator", "arithmetic", "conditionals"],
        )

        # LOGO: Colorful Spiral
        cls.TEMPLATES["logo_spiral"] = Template(
            name="Colorful Spiral",
            description="Draw a mesmerizing colorful spiral pattern",
            category=TemplateCategory.ART,
            language="LOGO",
            difficulty="beginner",
            code="""TO SPIRAL :SIZE :ANGLE :COLOR
  SETPENCOLOR :COLOR
  FORWARD :SIZE
  RIGHT :ANGLE
  IF :SIZE > 1 THEN SPIRAL :SIZE - 1 :ANGLE :COLOR + 1
END

CLEARSCREEN
PENUP
SETPOSITION [-50 50]
PENDOWN
SPIRAL 100 5 0
""",
            tags=["graphics", "recursion", "colors", "turtle"],
        )

        # LOGO: Nested Squares
        cls.TEMPLATES["logo_squares"] = Template(
            name="Nested Squares",
            description="Draw beautiful nested square patterns",
            category=TemplateCategory.ART,
            language="LOGO",
            difficulty="beginner",
            code="""TO SQUARE :SIZE
  REPEAT 4 [
    FORWARD :SIZE
    RIGHT 90
  ]
END

TO NESTED :SIZE :REDUCTION :COUNT
  IF :COUNT = 0 THEN STOP
  SQUARE :SIZE
  PENUP
  SETPOSITION [0 0]
  PENDOWN
  NESTED :SIZE - :REDUCTION :REDUCTION :COUNT - 1
END

CLEARSCREEN
PENUP
SETPOSITION [-100 100]
PENDOWN
NESTED 200 20 10
""",
            tags=["graphics", "loops", "procedures", "turtle"],
        )

        # LOGO: Tree Generator
        cls.TEMPLATES["logo_tree"] = Template(
            name="Fractal Tree",
            description="Generate a beautiful recursive tree",
            category=TemplateCategory.ART,
            language="LOGO",
            difficulty="intermediate",
            code="""TO TREE :SIZE :ANGLE
  IF :SIZE < 2 THEN STOP

  FORWARD :SIZE
  RIGHT :ANGLE
  TREE :SIZE * 0.7 :ANGLE
  LEFT :ANGLE * 2
  TREE :SIZE * 0.7 :ANGLE
  RIGHT :ANGLE
  BACKWARD :SIZE
END

CLEARSCREEN
PENUP
SETPOSITION [0 -200]
PENDOWN
TREE 100 25
""",
            tags=["graphics", "recursion", "fractals"],
        )

        # BASIC: Data Analysis
        cls.TEMPLATES["basic_data_viz"] = Template(
            name="Data Analyzer",
            description="Read numbers and compute statistics",
            category=TemplateCategory.DATA_VIZ,
            language="BASIC",
            difficulty="intermediate",
            code="""10 REM Data Analyzer
20 DIM DATA(100)
30 COUNT = 0
40 SUM = 0
50 PRINT "Enter numbers (0 to end):"
60 INPUT NUM
70 IF NUM = 0 THEN GOTO 100
80 COUNT = COUNT + 1
90 DATA(COUNT) = NUM
95 SUM = SUM + NUM
96 GOTO 60
100 PRINT
110 PRINT "Statistics:"
120 PRINT "Count: "; COUNT
130 PRINT "Sum: "; SUM
140 PRINT "Average: "; SUM / COUNT
150 END
""",
            tags=["arrays", "loops", "statistics", "math"],
        )

        # PILOT: Quiz Game
        cls.TEMPLATES["pilot_quiz"] = Template(
            name="Simple Quiz",
            description="Create an interactive quiz using PILOT",
            category=TemplateCategory.LEARNING,
            language="PILOT",
            difficulty="intermediate",
            code="""T: Welcome to the Time Warp Quiz!
A: What is 2 + 2?
U: ACCEPT
Y: 4
T: Correct!
JA: DONE
N:
T: Try again...
JMP: ACCEPT

*: DONE
T: Thanks for playing!
E:
""",
            tags=["quiz", "educational", "interaction"],
        )

        # BASIC: Robotics Simulation
        cls.TEMPLATES["basic_robotics"] = Template(
            name="Robot Navigator",
            description="Simulate a robot moving on a grid",
            category=TemplateCategory.ROBOTICS,
            language="BASIC",
            difficulty="intermediate",
            code="""10 REM Simple Robot Navigator
20 X = 50
30 Y = 50
40 DIR = 0
50 PRINT "Robot at "; X; ","; Y
60 INPUT "Command (F=forward, L=left, R=right, Q=quit): ", CMD$
70 IF CMD$ = "Q" THEN END
80 IF CMD$ = "F" THEN X = X + 10
90 IF CMD$ = "L" THEN DIR = DIR + 1
100 IF CMD$ = "R" THEN DIR = DIR - 1
110 PRINT "Robot at "; X; ","; Y; " Direction: "; DIR
120 GOTO 60
""",
            tags=["robotics", "simulation", "loops", "input"],
        )

    @classmethod
    def get_all(cls) -> Dict[str, Template]:
        """Get all available templates."""
        if not cls.TEMPLATES:
            cls.initialize()
        return cls.TEMPLATES

    @classmethod
    def get_by_category(cls, category: TemplateCategory) -> List[Template]:
        """Get templates by category."""
        if not cls.TEMPLATES:
            cls.initialize()
        return [t for t in cls.TEMPLATES.values() if t.category == category]

    @classmethod
    def get_by_language(cls, language: str) -> List[Template]:
        """Get templates by language."""
        if not cls.TEMPLATES:
            cls.initialize()
        return [t for t in cls.TEMPLATES.values() if t.language == language]

    @classmethod
    def get_by_id(cls, template_id: str) -> Optional[Template]:
        """Get a specific template by ID."""
        if not cls.TEMPLATES:
            cls.initialize()
        return cls.TEMPLATES.get(template_id)

    @classmethod
    def search(cls, query: str) -> List[Template]:
        """Search templates by name, description, or tags."""
        if not cls.TEMPLATES:
            cls.initialize()

        query_lower = query.lower()
        results = []

        for template in cls.TEMPLATES.values():
            if (
                query_lower in template.name.lower()
                or query_lower in template.description.lower()
                or any(query_lower in tag.lower() for tag in template.tags)
            ):
                results.append(template)

        return results

    @classmethod
    def get_popular(cls, limit: int = 5) -> List[Template]:
        """Get popular/featured templates."""
        if not cls.TEMPLATES:
            cls.initialize()

        featured_ids = [
            "logo_spiral",
            "basic_guessing_game",
            "logo_tree",
            "basic_calculator",
            "logo_squares",
        ]

        templates = []
        for template_id in featured_ids[:limit]:
            if template_id in cls.TEMPLATES:
                templates.append(cls.TEMPLATES[template_id])

        return templates

    @classmethod
    def get_for_beginner(cls) -> List[Template]:
        """Get beginner-friendly templates."""
        if not cls.TEMPLATES:
            cls.initialize()
        return [t for t in cls.TEMPLATES.values() if t.difficulty == "beginner"]

    @classmethod
    def export_as_json(cls) -> str:
        """Export all templates as JSON."""
        if not cls.TEMPLATES:
            cls.initialize()

        templates_data = []
        for template_id, template in cls.TEMPLATES.items():
            templates_data.append(
                {
                    "id": template_id,
                    "name": template.name,
                    "description": template.description,
                    "category": template.category.value,
                    "language": template.language,
                    "difficulty": template.difficulty,
                    "tags": template.tags,
                    "author": template.author,
                }
            )

        return json.dumps(templates_data, indent=2)
