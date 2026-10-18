#!/usr/bin/env python3
"""Compare the native language ports with the Python reference.

The native engine is only a port if it behaves like the thing it replaces, and
"looks right" is not a test. This harness runs the same program through both
implementations and compares three things exactly:

  * the output lines, including the ❌ messages;
  * every drawing call, in order, with the palette resolved the same way;
  * the variables the run left behind.

Corpus: every row of `command_table.py` for the language, plus every example
program. Rows that use a random function are reported and skipped, because two
generators seeded from the OS cannot agree by construction.

    python3 native/tools/lang_differential_check.py [--verbose]
"""

from __future__ import annotations

import argparse
import math
import re
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
BRIDGE_ROOT = REPO_ROOT / "native" / "bridge"
sys.path.insert(0, str(BRIDGE_ROOT))

from time_warp.core.interpreter import Interpreter, Language  # noqa: E402
from time_warp.graphics.turtle_state import TurtleShape  # noqa: E402
from time_warp.tests import command_table  # noqa: E402

CLI = REPO_ROOT / "native" / "build" / "lang_cli"

#: Programs that use these can never agree: the reference seeds from the OS.
RANDOM_MARKERS = ("FRAN", "RND", "RANDOM")


class _RecordedShapes(list):
    """A `TurtleState.shapes` list that also notes each shape as a drawing op.

    The reference's executors add to `shapes` directly rather than calling a
    draw method, so the trace has to be taken at the list. The op text is the
    one `tools/trace_host.hpp` prints for the same shape.
    """

    def __init__(self, owner: "TraceTurtle") -> None:
        super().__init__()
        self._owner = owner

    def append(self, shape) -> None:  # type: ignore[override]
        owner = self._owner
        points = " ".join(
            f"{owner._number(x)} {owner._number(y)}" for x, y in shape.points
        )
        colour = owner._colour(shape.color)
        if shape.shape_type == "text":
            owner.ops.append(f"text {points} {shape.font_size} {shape.text or ''}")
        else:
            fill = owner._colour(shape.fill_color) if shape.fill_color else "-"
            owner.ops.append(
                f"polygon {points} {colour} {fill} {owner._number(shape.width)}"
            )
        super().append(shape)


class TraceTurtle:
    """A turtle that records what was asked of it, as the C++ TraceHost does."""

    def __init__(self) -> None:
        self.ops: list[str] = []
        # The engine's own starting palette, not the IDE's: an executor asks the
        # turtle to resolve an index, and the answer has to be the engine's.
        from time_warp.graphics.turtle_state import DEFAULT_PALETTE_16

        self.palette = dict(DEFAULT_PALETTE_16)
        self.pen_color = (255, 255, 255)
        self.bg_color = (10, 10, 20)
        self.pen_width = 2.0
        self.pen_down = True
        self.x = 0.0
        self.y = 0.0
        self.heading = 0.0
        self.is_3d = False
        self.screen_mode = "wrap"
        self.shapes = _RecordedShapes(self)
        # The engine records every drawn line so `end_fill` can build a polygon
        # from them; without it a filled shape has no outline to fill.
        self.lines: list[tuple[float, float, float, float]] = []
        self._filling = False
        self._fill_start = 0
        self._last_fillable_shape_index: int | None = None
        # FILLED installs a callback here to watch the turtle move; the double
        # only has to exist.
        self.on_change = None

    @property
    def angle(self) -> float:
        """Alias for heading, which the fractal renderer reads."""
        return self.heading

    @angle.setter
    def angle(self, value: float) -> None:
        self.heading = value % 360.0

    @staticmethod
    def _number(value: float) -> str:
        return str(int(value)) if float(value).is_integer() else repr(float(value))

    @staticmethod
    def _colour(value) -> str:
        red, green, blue = value
        return f"#{red:02X}{green:02X}{blue:02X}"

    def _notify_change(self) -> None:
        """The reference's change hook: FILLED watches the turtle through it."""
        if self.on_change:
            self.on_change()

    def _walk(self, distance: float) -> None:
        radians = math.radians(self.heading)
        self.x += distance * math.sin(radians)
        self.y += distance * math.cos(radians)

    def forward(self, distance: float) -> None:
        self.ops.append(f"forward {self._number(distance)}")
        self._notify_change()
        before = (self.x, self.y)
        self._walk(distance)
        self._record_line(before, (self.x, self.y))

    def back(self, distance: float) -> None:
        self.ops.append(f"back {self._number(distance)}")
        self._notify_change()
        before = (self.x, self.y)
        self._walk(-distance)
        self._record_line(before, (self.x, self.y))

    def right(self, degrees: float) -> None:
        self.ops.append(f"right {self._number(degrees)}")
        self._notify_change()
        self.heading = (self.heading + degrees) % 360.0

    def left(self, degrees: float) -> None:
        self.ops.append(f"left {self._number(degrees)}")
        self._notify_change()
        self.heading = (self.heading - degrees) % 360.0

    def _record_line(
        self, start: tuple[float, float], end: tuple[float, float]
    ) -> None:
        """The engine's `TurtleLine`: only a pen-down move draws one."""
        if self.pen_down:
            self.lines.append((start[0], start[1], end[0], end[1]))

    def goto(self, x: float, y: float) -> None:
        self.ops.append(f"goto {self._number(x)} {self._number(y)}")
        self._notify_change()
        self._record_line((self.x, self.y), (x, y))
        self.x = x
        self.y = y

    def setx(self, x: float) -> None:
        self.goto(x, self.y)

    def sety(self, y: float) -> None:
        self.goto(self.x, y)

    def setheading(self, degrees: float) -> None:
        self.ops.append(f"heading {self._number(degrees)}")
        self._notify_change()
        self.heading = degrees % 360.0

    def home(self) -> None:
        self.ops.append("home")
        self.x = 0.0
        self.y = 0.0
        self.heading = 0.0

    def clear(self) -> None:
        self.ops.append("clear")

    def reset(self) -> None:
        self.ops.append("reset")
        self.x = 0.0
        self.y = 0.0
        self.heading = 0.0
        self.pen_down = True
        self.pen_color = (255, 255, 255)
        self.pen_width = 2.0

    def enable_3d(self) -> None:
        self.ops.append("3d on")
        self.is_3d = True

    def disable_3d(self) -> None:
        self.ops.append("3d off")
        self.is_3d = False

    def pitch(self, degrees: float) -> None:
        self.ops.append(f"pitch {self._number(degrees)}")

    def roll(self, degrees: float) -> None:
        self.ops.append(f"roll {self._number(degrees)}")

    def penup(self) -> None:
        self.ops.append("penup")
        self.pen_down = False

    def pendown(self) -> None:
        self.ops.append("pendown")
        self.pen_down = True

    def setpenwidth(self, width: float) -> None:
        self.ops.append(f"width {self._number(width)}")
        self.pen_width = width

    def setcolor(self, red: int, green: int, blue: int) -> None:
        self.pen_color = (red, green, blue)
        self.ops.append(f"colour {self._colour((red, green, blue))}")

    def pencolor(self, colour) -> None:
        """Logo's `SETPENCOLOR "RED` route: only the resulting colour matters."""
        from time_warp.graphics.turtle_state import COLOR_NAMES

        if isinstance(colour, str) and colour.upper() in COLOR_NAMES:
            self.pen_color = COLOR_NAMES[colour.upper()]
            self.ops.append(f"colour {self._colour(self.pen_color)}")
        elif isinstance(colour, tuple) and len(colour) == 3:
            self.pen_color = colour
            self.ops.append(f"colour {self._colour(colour)}")

    def setbgcolor(self, red: int, green: int, blue: int) -> None:
        self.ops.append(f"background {self._colour((red, green, blue))}")

    def circle(self, radius: float, extent: float = 360.0) -> None:
        self.ops.append(f"circle {self._number(radius)}")

    def draw_rect(self, x0, y0, x1, y1, fill_color=None) -> None:
        self.ops.append(
            "rect "
            f"{self._number(x0)} {self._number(y0)} "
            f"{self._number(x1)} {self._number(y1)} "
            f"{self._colour(fill_color)}"
        )

    def draw_text(self, x, y, text, color=None, font_size=12, align="left") -> None:
        self.shapes.append(
            TurtleShape(
                shape_type="text",
                points=[(x, y)],
                color=color or self.pen_color,
                text=text,
                font_size=font_size,
                align=align,
            )
        )

    def draw_point(self, x, y, color=None, **_kwargs) -> None:
        self.ops.append(
            f"point {self._number(x)} {self._number(y)} "
            f"{self._colour(color or self.pen_color)}"
        )

    def draw_polygon(self, points, color=None, fill_color=None, width=None) -> None:
        self.shapes.append(
            TurtleShape(
                shape_type="polygon",
                points=list(points),
                color=color or self.pen_color,
                width=width if width is not None else self.pen_width,
                fill_color=fill_color,
            )
        )

    def fill_last_shape(self, fill_color=None) -> bool:
        self.ops.append(f"fill {self._colour(fill_color or self.pen_color)}")
        if self._last_fillable_shape_index is None:
            return False
        if self._last_fillable_shape_index >= len(self.shapes):
            return False
        self.shapes[self._last_fillable_shape_index].fill_color = (
            fill_color or self.pen_color
        )
        return True

    def draw_line(self, x1, y1, x2, y2, color=None, width=None) -> None:
        self.shapes.append(
            TurtleShape(
                shape_type="line",
                points=[(x1, y1), (x2, y2)],
                color=color or self.pen_color,
                width=width if width is not None else self.pen_width,
            )
        )

    def begin_fill(self) -> None:
        self._filling = True
        self._fill_start = len(self.lines)

    def end_fill(self) -> None:
        if not self._filling:
            return
        self._filling = False
        fill_lines = self.lines[self._fill_start :]
        if not fill_lines:
            return
        points = [(fill_lines[0][0], fill_lines[0][1])]
        points.extend((line[2], line[3]) for line in fill_lines)
        if len(points) >= 3:
            self.shapes.append(
                TurtleShape(
                    shape_type="polygon",
                    points=points,
                    color=self.pen_color,
                    width=self.pen_width,
                    fill_color=self.pen_color,
                )
            )
            self._last_fillable_shape_index = len(self.shapes) - 1

    def cycle_palette(
        self, start_index: int = 1, end_index: int = 15, step: int = 1
    ) -> None:
        low = max(0, min(255, int(start_index)))
        high = max(low, min(255, int(end_index)))
        indices = list(range(low, high + 1))
        if len(indices) <= 1:
            return
        colors = [self._palette_color(i) for i in indices]
        shift = int(step) % len(colors)
        rotated = colors[-shift:] + colors[:-shift]
        for index, colour in zip(indices, rotated):
            self.palette[index] = colour

    def showturtle(self) -> None:
        self.ops.append("show")

    def hideturtle(self) -> None:
        self.ops.append("hide")

    def set_svga_mode(self, width: int = 800, height: int = 600) -> None:
        self.ops.append(f"svga {width} {height}")

    def set_palette_color(self, index: int, red: int, green: int, blue: int) -> None:
        self.palette[index] = (red, green, blue)
        self.ops.append(f"palette {index} {self._colour((red, green, blue))}")

    def _palette_color(self, index: int):
        if index in self.palette:
            return self.palette[index]
        if 0 <= index <= 255:
            return (index, index, index)
        return self.pen_color

    def resolve_color(self, value, default=None):
        """The engine's resolution: name, hex, "r g b", or palette index."""
        from time_warp.graphics.turtle_state import COLOR_NAMES

        if value is None:
            return default if default is not None else self.pen_color
        if isinstance(value, (list, tuple)) and len(value) == 3:
            return tuple(max(0, min(255, int(v))) for v in value)
        if isinstance(value, str):
            colour = value.strip()
            if colour.startswith("#") and len(colour) == 7:
                try:
                    return (
                        int(colour[1:3], 16),
                        int(colour[3:5], 16),
                        int(colour[5:7], 16),
                    )
                except ValueError:
                    return default if default is not None else self.pen_color
            if colour.upper() in COLOR_NAMES:
                return COLOR_NAMES[colour.upper()]
            parts = colour.split()
            if len(parts) == 3:
                try:
                    red, green, blue = (int(part) for part in parts)
                except ValueError:
                    return default if default is not None else self.pen_color
                return (
                    max(0, min(255, red)),
                    max(0, min(255, green)),
                    max(0, min(255, blue)),
                )
            try:
                return self._palette_color(int(colour))
            except ValueError:
                return default if default is not None else self.pen_color
        if isinstance(value, (int, float)):
            return self._palette_color(int(value))
        return default if default is not None else self.pen_color


_NUMBER = re.compile(r"[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?")


def normalise_variables(
    values: dict[str, tuple[str, str]],
) -> dict[str, tuple[str, str]]:
    """One vocabulary for both sides.

    Python calls 7.0 a `float` and shows it as `7.0`; the panel calls it a
    `number` and shows `7`, because the learner wrote `7`. Those are the same
    fact, so the comparison is on the value and on the word a person reads.
    """
    result: dict[str, tuple[str, str]] = {}
    for name, (kind, value) in values.items():
        if kind in {"float", "number", "int", "boolean"}:
            result[name] = ("number", format(float(value), ".12g"))
        elif kind in {"str", "text"}:
            result[name] = ("text", value)
        else:
            result[name] = (kind, value)
    return result


def normalise(text: str) -> str:
    """One spelling for a number, so 50 and 50.0 compare equal."""

    def replace(match: re.Match[str]) -> str:
        try:
            return format(float(match.group(0)), ".12g")
        except ValueError:  # pragma: no cover - the regex only matches numbers
            return match.group(0)

    return _NUMBER.sub(replace, text)


def run_python(language: Language, source: str):
    """The reference: output, drawing calls and variables.

    Output is collected from the callback, which is what the worker streams.
    `execute()` returns a copy of the engine's *whole* buffer, so accumulating
    it per input round repeats everything printed before the prompt -- a test
    helper's artefact, not the engine's behaviour.
    """
    interpreter = Interpreter()
    turtle = TraceTurtle()
    interpreter.input_callback = lambda _prompt: ""
    output: list[str] = []
    interpreter.output_callback = output.append
    interpreter.load_program(source, language=language)
    # Enough rounds for the longest example (twenty questions, each prompting).
    for _ in range(500):
        interpreter.execute(turtle)
        if interpreter.pending_input is not None:
            interpreter.provide_input("")
            continue
        break
    variables = {
        name: (type(value).__name__, str(value))
        for name, value in interpreter.variables.items()
        if not name.endswith(("#", "%", "!", "&"))
    }
    # The reference keeps strings in their own store; the native engine has one
    # variable map, so the comparison has to see both halves of the reference.
    for name, value in interpreter.string_variables.items():
        variables[name] = ("str", str(value))
    return output, turtle.ops, variables


def run_native(key: str, source: str):
    """The port: the same three things, parsed from lang_cli."""
    completed = subprocess.run(
        [str(CLI), key],
        input=source,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    if completed.returncode != 0:
        raise RuntimeError(
            f"lang_cli exited {completed.returncode}: {completed.stderr.strip()}"
        )
    output: list[str] = []
    drawing: list[str] = []
    variables: dict[str, tuple[str, str]] = {}
    for line in completed.stdout.splitlines():
        parts = line.split("\t")
        if parts[0] == "OUT":
            output.append(parts[1] if len(parts) > 1 else "")
        elif parts[0] == "DRAW":
            drawing.append(parts[1] if len(parts) > 1 else "")
        elif parts[0] == "VAR" and len(parts) >= 4:
            variables[parts[1]] = (parts[2], parts[3])
    return output, drawing, variables


def corpus(language: Language, key: str):
    """Rows from the command table plus the example files."""
    rows = getattr(command_table, key.upper(), [])
    programs = [(f"row:{row.command}", row.source) for row in rows]
    for path in sorted((REPO_ROOT / "Examples" / key).glob("*")):
        if path.is_file():
            programs.append(
                (str(path.relative_to(REPO_ROOT)), path.read_text(encoding="utf-8"))
            )
    return programs


def compare(name: str, language: Language, key: str, source: str) -> tuple[bool, str]:
    expected_output, expected_drawing, expected_variables = run_python(language, source)
    actual_output, actual_drawing, actual_variables = run_native(key, source)

    got_output = [normalise(line) for line in actual_output]
    want_output = [normalise(line) for line in expected_output]
    got_drawing = [normalise(op) for op in actual_drawing]
    want_drawing = [normalise(op) for op in expected_drawing]

    problems: list[str] = []
    if got_output != want_output:
        problems.append(f"output:\n  python: {want_output}\n  native: {got_output}")
    if got_drawing != want_drawing:
        first = next(
            (
                index
                for index, (a, b) in enumerate(zip(got_drawing, want_drawing))
                if a != b
            ),
            min(len(got_drawing), len(want_drawing)),
        )
        problems.append(
            "drawing differs at "
            f"{first}: python {want_drawing[first : first + 3]} "
            f"native {got_drawing[first : first + 3]}"
        )
    if normalise_variables(actual_variables) != normalise_variables(expected_variables):
        problems.append(
            f"variables:\n  python: {expected_variables}\n  native: {actual_variables}"
        )
    return not problems, f"{name}\n  " + "\n  ".join(problems)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--verbose", action="store_true")
    parser.add_argument(
        "--language",
        action="append",
        help="limit to one language (repeatable)",
    )
    args = parser.parse_args()

    if not CLI.exists():
        print(
            f"missing {CLI}; build it with `cmake --build native/build`",
            file=sys.stderr,
        )
        return 2

    # The native side is the only place that knows which languages have an
    # executor: `executor_for` returns null for the ones still on the bridge.
    # Keeping a second list here meant a new port had to be registered twice,
    # and forgetting the second copy looked like a language with no corpus.
    ported = subprocess.run(
        [str(CLI), "--ported"], capture_output=True, text=True, check=True
    ).stdout.split()
    languages = {key: Language[key.upper()] for key in ported}
    chosen = args.language or ported
    missing = [key for key in chosen if key not in languages]
    if missing:
        print(
            f"no native executor for: {', '.join(missing)} "
            f"(ported: {', '.join(ported)})",
            file=sys.stderr,
        )
        return 2

    agree = 0
    skipped: list[str] = []
    failures: list[str] = []
    for key in chosen:
        language = languages[key]
        for name, source in corpus(language, key):
            if any(marker in source.upper() for marker in RANDOM_MARKERS):
                skipped.append(f"{key}: {name} (random)")
                continue
            ok, detail = compare(name, language, key, source)
            if ok:
                agree += 1
                if args.verbose:
                    print(f"agree  {key}: {name}")
            else:
                failures.append(f"{key}: {detail}")

    for entry in skipped:
        print(f"skip   {entry}")
    for failure in failures:
        print(f"DIFFER {failure}")
    print(
        f"\n{agree} agreed, {len(failures)} differed, {len(skipped)} skipped "
        f"({', '.join(chosen)})"
    )
    return 0 if not failures else 1


if __name__ == "__main__":
    raise SystemExit(main())
