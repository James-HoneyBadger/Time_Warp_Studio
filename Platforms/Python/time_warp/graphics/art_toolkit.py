"""
Procedural Art Toolkit - Generators for fractals, L-systems, and harmonic motion.
"""

from __future__ import annotations
import math
import random
from dataclasses import dataclass


@dataclass
class LSystemRule:
    """L-System production rule."""

    symbol: str
    replacement: str


class LSystem:
    """Lindenmayer system generator for procedural patterns."""

    def __init__(self, axiom: str, rules: list[LSystemRule], angle: float = 90.0):
        self.axiom = axiom
        self.rules = {rule.symbol: rule.replacement for rule in rules}
        self.angle = angle

    def generate(self, iterations: int) -> str:
        """Generate L-system string after n iterations."""
        current = self.axiom

        for _ in range(iterations):
            next_gen = []
            for char in current:
                next_gen.append(self.rules.get(char, char))
            current = "".join(next_gen)

        return current

    def to_turtle_commands(
        self, lsystem_string: str, step_size: float = 10.0
    ) -> list[str]:
        """Convert L-system string to turtle commands."""
        commands = []
        stack = []

        for char in lsystem_string:
            if char == "F":
                commands.append(f"FD {step_size}")
            elif char == "f":
                commands.append("PU")
                commands.append(f"FD {step_size}")
                commands.append("PD")
            elif char == "+":
                commands.append(f"RT {self.angle}")
            elif char == "-":
                commands.append(f"LT {self.angle}")
            elif char == "[":
                stack.append("PUSH")
                commands.append("# Save position")
            elif char == "]":
                if stack:
                    stack.pop()
                commands.append("# Restore position")

        return commands


# Predefined L-Systems
KOCH_CURVE = LSystem(
    axiom="F",
    rules=[LSystemRule("F", "F+F-F-F+F")],
    angle=90.0,
)

SIERPINSKI_TRIANGLE = LSystem(
    axiom="F-G-G",
    rules=[
        LSystemRule("F", "F-G+F+G-F"),
        LSystemRule("G", "GG"),
    ],
    angle=120.0,
)

DRAGON_CURVE = LSystem(
    axiom="FX",
    rules=[
        LSystemRule("X", "X+YF+"),
        LSystemRule("Y", "-FX-Y"),
    ],
    angle=90.0,
)

PLANT = LSystem(
    axiom="X",
    rules=[
        LSystemRule("X", "F+[[X]-X]-F[-FX]+X"),
        LSystemRule("F", "FF"),
    ],
    angle=25.0,
)


class FractalGenerator:
    """Recursive fractal pattern generator."""

    @staticmethod
    def koch_snowflake(order: int, size: float = 100.0) -> list[str]:
        """Generate Koch snowflake turtle commands."""

        def koch_segment(length: float, depth: int) -> list[str]:
            if depth == 0:
                return [f"FD {length}"]

            commands = []
            seg_len = length / 3
            commands.extend(koch_segment(seg_len, depth - 1))
            commands.append("LT 60")
            commands.extend(koch_segment(seg_len, depth - 1))
            commands.append("RT 120")
            commands.extend(koch_segment(seg_len, depth - 1))
            commands.append("LT 60")
            commands.extend(koch_segment(seg_len, depth - 1))
            return commands

        commands = []
        for _ in range(3):
            commands.extend(koch_segment(size, order))
            commands.append("RT 120")

        return commands

    @staticmethod
    def sierpinski_carpet(
        order: int, size: float = 243.0
    ) -> list[tuple[float, float, float]]:
        """Generate Sierpinski carpet as list of (x, y, size) squares."""

        def subdivide(
            x: float, y: float, s: float, depth: int
        ) -> list[tuple[float, float, float]]:
            if depth == 0:
                return [(x, y, s)]

            squares = []
            third = s / 3

            for i in range(3):
                for j in range(3):
                    if i == 1 and j == 1:
                        continue
                    squares.extend(
                        subdivide(x + i * third, y + j * third, third, depth - 1)
                    )

            return squares

        return subdivide(0, 0, size, order)

    @staticmethod
    def mandelbrot_set(
        width: int = 800,
        height: int = 600,
        max_iter: int = 100,
        xmin: float = -2.5,
        xmax: float = 1.0,
        ymin: float = -1.0,
        ymax: float = 1.0,
    ) -> list[list[int]]:
        """Generate Mandelbrot set iteration counts."""
        result = []

        for py in range(height):
            row = []
            y0 = ymin + (ymax - ymin) * py / height

            for px in range(width):
                x0 = xmin + (xmax - xmin) * px / width
                x, y = 0.0, 0.0
                iteration = 0

                while x * x + y * y <= 4.0 and iteration < max_iter:
                    xtemp = x * x - y * y + x0
                    y = 2 * x * y + y0
                    x = xtemp
                    iteration += 1

                row.append(iteration)

            result.append(row)

        return result


class HarmonicMotion:
    """Harmonic and wave pattern generator."""

    @staticmethod
    def lissajous(
        a: int = 3,
        b: int = 2,
        delta: float = math.pi / 2,
        steps: int = 1000,
        scale: float = 100.0,
    ) -> list[tuple[float, float]]:
        """Generate Lissajous curve points."""
        points = []

        for i in range(steps):
            t = 2 * math.pi * i / steps
            x = scale * math.sin(a * t + delta)
            y = scale * math.sin(b * t)
            points.append((x, y))

        return points

    @staticmethod
    def rose_curve(
        n: int = 5,
        d: int = 2,
        steps: int = 1000,
        scale: float = 100.0,
    ) -> list[tuple[float, float]]:
        """Generate rose curve points (polar equation r = cos(n/d * theta))."""
        points = []
        k = n / d
        revolutions = d if n % 2 == 0 else 2 * d

        for i in range(steps):
            theta = 2 * math.pi * revolutions * i / steps
            r = scale * math.cos(k * theta)
            x = r * math.cos(theta)
            y = r * math.sin(theta)
            points.append((x, y))

        return points

    @staticmethod
    def spiral(
        turns: int = 5,
        growth: float = 10.0,
        steps: int = 500,
    ) -> list[tuple[float, float, float]]:
        """Generate Archimedean spiral points with heading."""
        points = []

        for i in range(steps):
            theta = 2 * math.pi * turns * i / steps
            r = growth * theta
            x = r * math.cos(theta)
            y = r * math.sin(theta)
            heading = math.degrees(theta)
            points.append((x, y, heading))

        return points


class RandomArtGenerator:
    """Seedable random art pattern generator."""

    def __init__(self, seed: int | None = None):
        self.rng = random.Random(seed)

    def random_walk(
        self,
        steps: int = 100,
        step_size: float = 10.0,
        turn_range: float = 45.0,
    ) -> list[str]:
        """Generate random walk turtle commands."""
        commands = []

        for _ in range(steps):
            turn = self.rng.uniform(-turn_range, turn_range)
            commands.append(f"RT {turn:.1f}")
            commands.append(f"FD {step_size}")

        return commands

    def noise_field(
        self,
        width: int,
        height: int,
        scale: float = 0.1,
        octaves: int = 4,
    ) -> list[list[float]]:
        """Generate Perlin-like noise field (simplified)."""
        field = []

        for y in range(height):
            row = []
            for x in range(width):
                value = 0.0
                amplitude = 1.0

                for octave in range(octaves):
                    freq = 2**octave * scale
                    value += amplitude * (
                        math.sin(x * freq + self.rng.random())
                        * math.cos(y * freq + self.rng.random())
                    )
                    amplitude *= 0.5

                row.append(value)

            field.append(row)

        return field


# Preset patterns
PRESET_PATTERNS = {
    "koch_snowflake": lambda: KOCH_CURVE.to_turtle_commands(
        KOCH_CURVE.generate(4), step_size=5
    ),
    "dragon_curve": lambda: DRAGON_CURVE.to_turtle_commands(
        DRAGON_CURVE.generate(10), step_size=5
    ),
    "plant": lambda: PLANT.to_turtle_commands(PLANT.generate(5), step_size=3),
    "lissajous_3_2": lambda: HarmonicMotion.lissajous(3, 2, math.pi / 2, 1000, 100),
    "rose_7": lambda: HarmonicMotion.rose_curve(7, 2, 1000, 100),
}
