# pylint: disable=too-few-public-methods
"""
L-System fractal generator for Time Warp IDE.
Implements Lindenmayer systems for fractal generation.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, TYPE_CHECKING

if TYPE_CHECKING:
    from ..graphics.turtle_state import TurtleState


@dataclass
class LSystemRule:
    """A production rule for an L-System."""

    predecessor: str
    successor: str
    probability: float = 1.0  # For stochastic L-Systems


@dataclass
class LSystem:
    """Lindenmayer System definition."""

    name: str
    axiom: str
    rules: Dict[str, str] = field(default_factory=dict)
    angle: float = 90.0  # Default turn angle in degrees
    description: str = ""

    def iterate(self, iterations: int) -> str:
        """Apply production rules for n iterations.

        Args:
            iterations: Number of iterations to apply

        Returns:
            The resulting string after applying rules
        """
        current = self.axiom

        for _ in range(iterations):
            result = ""
            for char in current:
                if char in self.rules:
                    result += self.rules[char]
                else:
                    result += char
            current = result

        return current


# Pre-defined L-System fractals
LSYSTEM_PRESETS: Dict[str, LSystem] = {
    "KOCH": LSystem(
        name="Koch Snowflake",
        axiom="F--F--F",
        rules={"F": "F+F--F+F"},
        angle=60.0,
        description="Classic Koch snowflake curve",
    ),
    "SIERPINSKI": LSystem(
        name="Sierpinski Triangle",
        axiom="F-G-G",
        rules={"F": "F-G+F+G-F", "G": "GG"},
        angle=120.0,
        description="Sierpinski triangle gasket",
    ),
    "DRAGON": LSystem(
        name="Dragon Curve",
        axiom="FX",
        rules={"X": "X+YF+", "Y": "-FX-Y"},
        angle=90.0,
        description="Heighway dragon curve",
    ),
    "PLANT": LSystem(
        name="Fractal Plant",
        axiom="X",
        rules={"X": "F+[[X]-X]-F[-FX]+X", "F": "FF"},
        angle=25.0,
        description="Realistic plant branching",
    ),
    "TREE": LSystem(
        name="Binary Tree",
        axiom="0",
        rules={"1": "11", "0": "1[0]0"},
        angle=45.0,
        description="Simple binary tree",
    ),
    "HILBERT": LSystem(
        name="Hilbert Curve",
        axiom="A",
        rules={"A": "-BF+AFA+FB-", "B": "+AF-BFB-FA+"},
        angle=90.0,
        description="Space-filling Hilbert curve",
    ),
    "PEANO": LSystem(
        name="Peano Curve",
        axiom="X",
        rules={"X": "XFYFX+F+YFXFY-F-XFYFX", "Y": "YFXFY-F-XFYFX+F+YFXFY"},
        angle=90.0,
        description="Peano space-filling curve",
    ),
    "GOSPER": LSystem(
        name="Gosper Curve",
        axiom="A",
        rules={"A": "A-B--B+A++AA+B-", "B": "+A-BB--B-A++A+B"},
        angle=60.0,
        description="Gosper flowsnake curve",
    ),
    "LEVY": LSystem(
        name="Levy C Curve",
        axiom="F",
        rules={"F": "+F--F+"},
        angle=45.0,
        description="Levy C curve",
    ),
    "SQUARE": LSystem(
        name="Square Curve",
        axiom="F+F+F+F",
        rules={"F": "F+F-F-FF+F+F-F"},
        angle=90.0,
        description="Square Koch variant",
    ),
    "CRYSTAL": LSystem(
        name="Crystal",
        axiom="F+F+F+F",
        rules={"F": "FF+F++F+F"},
        angle=90.0,
        description="Crystal-like growth pattern",
    ),
    "RINGS": LSystem(
        name="Rings",
        axiom="F+F+F+F",
        rules={"F": "FF+F+F+F+F+F-F"},
        angle=90.0,
        description="Interlocking ring pattern",
    ),
    "BUSH": LSystem(
        name="Bush",
        axiom="Y",
        rules={"X": "X[-FFF][+FFF]FX", "Y": "YFX[+Y][-Y]"},
        angle=25.7,
        description="Dense bush pattern",
    ),
    "SEAWEED": LSystem(
        name="Seaweed",
        axiom="F",
        rules={"F": "FF-[-F+F+F]+[+F-F-F]"},
        angle=22.5,
        description="Seaweed-like growth",
    ),
    "PENROSE": LSystem(
        name="Penrose Tiling",
        axiom="[7]++[7]++[7]++[7]++[7]",
        rules={
            "6": "81++91----71[-81----61]++",
            "7": "+81--91[---61--71]+",
            "8": "-61++71[+++81++91]-",
            "9": "--81++++61[+91++++71]--71",
            "1": "",
        },
        angle=36.0,
        description="Penrose P3 tiling approximation",
    ),
}


class LSystemRenderer:
    """Renders L-System strings using turtle graphics."""

    def __init__(self):
        self.stack: List[Tuple[float, float, float]] = []

    def render(
        self,
        turtle: "TurtleState",
        lsystem_string: str,
        angle: float,
        step_size: float = 10.0,
    ) -> str:
        """Render an L-System string using turtle graphics.

        L-System alphabet:
        - F, G, 0, 1, 6, 7, 8, 9: Move forward while drawing
        - f, g: Move forward without drawing
        - +: Turn right by angle
        - -: Turn left by angle
        - |: Turn 180 degrees
        - [: Push position and angle onto stack
        - ]: Pop position and angle from stack
        - A-E, X, Y: Non-drawing symbols (for rules)

        Args:
            turtle: TurtleState to draw with
            lsystem_string: The expanded L-System string
            angle: Turn angle in degrees
            step_size: Length of each forward step

        Returns:
            Status message
        """
        self.stack.clear()
        moves = 0

        for char in lsystem_string:
            if char in "FG01":
                # Move forward while drawing
                turtle.forward(step_size)
                moves += 1

            elif char in "6789":
                # Also drawing symbols for some L-systems
                turtle.forward(step_size)
                moves += 1

            elif char in "fg":
                # Move forward without drawing
                turtle.penup()
                turtle.forward(step_size)
                turtle.pendown()
                moves += 1

            elif char == "+":
                # Turn right
                turtle.right(angle)

            elif char == "-":
                # Turn left
                turtle.left(angle)

            elif char == "|":
                # Turn 180 degrees
                turtle.right(180)

            elif char == "[":
                # Push state
                self.stack.append((turtle.x, turtle.y, turtle.angle))

            elif char == "]":
                # Pop state
                if self.stack:
                    x, y, a = self.stack.pop()
                    turtle.penup()
                    turtle.goto(x, y)
                    turtle.setheading(a)
                    turtle.pendown()

            # Ignore other characters (variables like A, B, X, Y)

        return f"🌿 L-System rendered ({moves} moves)\n"


class FractalGenerator:
    """High-level fractal generation interface."""

    def __init__(self):
        self.renderer = LSystemRenderer()
        self.presets = LSYSTEM_PRESETS

    def get_preset_names(self) -> List[str]:
        """Get list of available preset names."""
        return list(self.presets.keys())

    def get_preset(self, name: str) -> Optional[LSystem]:
        """Get a preset L-System by name."""
        return self.presets.get(name.upper())

    def create_custom(
        self,
        axiom: str,
        rules: Dict[str, str],
        angle: float = 90.0,
        name: str = "Custom",
    ) -> LSystem:
        """Create a custom L-System."""
        return LSystem(name=name, axiom=axiom, rules=rules, angle=angle)

    def generate(
        self,
        turtle: "TurtleState",
        lsystem: LSystem,
        iterations: int = 4,
        step_size: float = 10.0,
    ) -> str:
        """Generate and render an L-System fractal.

        Args:
            turtle: TurtleState to draw with
            lsystem: The L-System definition
            iterations: Number of iterations
            step_size: Length of each forward step

        Returns:
            Status message
        """
        # Generate the string
        result_string = lsystem.iterate(iterations)

        # Render it
        return self.renderer.render(turtle, result_string, lsystem.angle, step_size)

    def draw_preset(
        self,
        turtle: "TurtleState",
        preset_name: str,
        iterations: int = 4,
        step_size: float = 10.0,
    ) -> str:
        """Draw a preset fractal.

        Args:
            turtle: TurtleState to draw with
            preset_name: Name of the preset (KOCH, DRAGON, PLANT, etc.)
            iterations: Number of iterations
            step_size: Length of each forward step

        Returns:
            Status message
        """
        lsystem = self.get_preset(preset_name)
        if not lsystem:
            available = ", ".join(self.get_preset_names())
            return f"❌ Unknown fractal: {preset_name}. Available: {available}\n"

        return self.generate(turtle, lsystem, iterations, step_size)


# Global instance
_fractal_generator: Optional[FractalGenerator] = None


def get_fractal_generator() -> FractalGenerator:
    """Get the global fractal generator instance."""
    global _fractal_generator  # pylint: disable=global-statement
    if _fractal_generator is None:
        _fractal_generator = FractalGenerator()
    return _fractal_generator
