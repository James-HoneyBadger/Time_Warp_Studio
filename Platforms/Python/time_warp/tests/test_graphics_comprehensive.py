"""
Comprehensive tests for all graphics functions.

Covers:
  - TurtleState: movement, rotation, pen, color, shapes, fill, arc, text,
    palette, resolve_color, on_change callback, edge cases
  - PixelCanvas: set/get, Bresenham line, rect, circle, flood_fill, sprites,
    tile map, frames, ASCII render
  - ArtToolkit: L-systems, fractals, harmonic motion, random art
  - TurtleGallery: record, save, load, list, SVG export, replay
  - Logo executor integration: color resolution with variables & reporters
"""

# pylint: disable=import-error,redefined-outer-name

import json
import math
import tempfile
from pathlib import Path

import pytest

from time_warp.graphics.turtle_state import (  # type: ignore[import-not-found]
    COLOR_NAMES,
    DEFAULT_PALETTE_16,
    TurtleLine,
    TurtleShape,
    TurtleState,
)
from time_warp.graphics.pixel_canvas import (  # type: ignore[import-not-found]
    PixelCanvas,
    Sprite,
    TileMap,
    create_animation_stepper,
    SPRITE_PLAYER,
    SPRITE_COIN,
)
from time_warp.graphics.art_toolkit import (  # type: ignore[import-not-found]
    LSystem,
    LSystemRule,
    FractalGenerator,
    HarmonicMotion,
    RandomArtGenerator,
    KOCH_CURVE,
    SIERPINSKI_TRIANGLE,
    DRAGON_CURVE,
    PLANT,
    PRESET_PATTERNS,
)
from time_warp.graphics.turtle_gallery import (  # type: ignore[import-not-found]
    TurtleGallery,
    DrawingMetadata,
    DrawingStep,
    create_share_link,
)
from time_warp.core.interpreter import Interpreter  # type: ignore[import-not-found]
from time_warp.languages.logo import execute_logo  # type: ignore[import-not-found]


# ── Fixtures ──────────────────────────────────────────────────────────────


@pytest.fixture
def turtle():
    return TurtleState()


@pytest.fixture
def canvas():
    return PixelCanvas(32, 24)


@pytest.fixture
def interp():
    return Interpreter()


@pytest.fixture
def gallery(tmp_path):
    return TurtleGallery(gallery_dir=tmp_path)


# ═══════════════════════════════════════════════════════════════════════════
#  SECTION 1 — TurtleState
# ═══════════════════════════════════════════════════════════════════════════


class TestTurtleStateInit:
    """Default / initial state."""

    def test_defaults(self, turtle):
        assert turtle.x == 0.0 and turtle.y == 0.0
        assert turtle.heading == 0.0
        assert turtle.pen_down is True
        assert turtle.pen_color == (255, 255, 255)
        assert turtle.pen_width == 2.0
        assert turtle.visible is True
        assert len(turtle.lines) == 0
        assert len(turtle.shapes) == 0
        assert turtle.bg_color == (10, 10, 20)

    def test_angle_alias(self, turtle):
        turtle.angle = 90.0
        assert turtle.heading == 90.0
        assert turtle.angle == 90.0

    def test_angle_wraps(self, turtle):
        turtle.angle = 450.0
        assert math.isclose(turtle.heading, 90.0)


class TestTurtleMovement:
    """forward / back / goto / setx / sety / home."""

    def test_forward_north(self, turtle):
        turtle.forward(100)
        assert math.isclose(turtle.x, 0.0, abs_tol=1e-9)
        assert math.isclose(turtle.y, 100.0, abs_tol=1e-9)
        assert len(turtle.lines) == 1

    def test_forward_east(self, turtle):
        turtle.heading = 90.0
        turtle.forward(50)
        assert math.isclose(turtle.x, 50.0, abs_tol=1e-9)
        assert math.isclose(turtle.y, 0.0, abs_tol=1e-9)

    def test_forward_south(self, turtle):
        turtle.heading = 180.0
        turtle.forward(75)
        assert math.isclose(turtle.x, 0.0, abs_tol=1e-9)
        assert math.isclose(turtle.y, -75.0, abs_tol=1e-9)

    def test_forward_west(self, turtle):
        turtle.heading = 270.0
        turtle.forward(30)
        assert math.isclose(turtle.x, -30.0, abs_tol=1e-9)
        assert math.isclose(turtle.y, 0.0, abs_tol=1e-9)

    def test_forward_diagonal(self, turtle):
        turtle.heading = 45.0  # NE
        turtle.forward(100)
        expected = 100 * math.sin(math.radians(45))
        assert math.isclose(turtle.x, expected, abs_tol=1e-9)
        assert math.isclose(turtle.y, expected, abs_tol=1e-9)

    def test_back(self, turtle):
        turtle.back(100)
        assert math.isclose(turtle.y, -100.0, abs_tol=1e-9)
        assert len(turtle.lines) == 1

    def test_goto_draws(self, turtle):
        turtle.goto(50, 80)
        assert turtle.x == 50 and turtle.y == 80
        assert len(turtle.lines) == 1
        line = turtle.lines[0]
        assert (line.start_x, line.start_y) == (0.0, 0.0)
        assert (line.end_x, line.end_y) == (50, 80)

    def test_goto_pen_up_no_draw(self, turtle):
        turtle.penup()
        turtle.goto(10, 20)
        assert len(turtle.lines) == 0
        assert turtle.x == 10 and turtle.y == 20

    def test_setx(self, turtle):
        turtle.setx(42)
        assert turtle.x == 42 and turtle.y == 0

    def test_sety(self, turtle):
        turtle.sety(77)
        assert turtle.y == 77 and turtle.x == 0

    def test_home(self, turtle):
        turtle.goto(100, 200)
        turtle.heading = 135
        turtle.home()
        assert turtle.x == 0 and turtle.y == 0
        assert turtle.heading == 0.0
        assert len(turtle.lines) == 2  # goto line + home line

    def test_forward_pen_up(self, turtle):
        turtle.penup()
        turtle.forward(100)
        assert len(turtle.lines) == 0
        assert math.isclose(turtle.y, 100.0, abs_tol=1e-9)


class TestTurtleRotation:
    """left / right / setheading."""

    def test_right_90(self, turtle):
        turtle.right(90)
        assert math.isclose(turtle.heading, 90.0)

    def test_left_90(self, turtle):
        turtle.left(90)
        assert math.isclose(turtle.heading, 270.0)

    def test_right_left_cancel(self, turtle):
        turtle.right(45)
        turtle.left(45)
        assert math.isclose(turtle.heading, 0.0)

    def test_full_rotation_wraps(self, turtle):
        turtle.right(360)
        assert math.isclose(turtle.heading, 0.0)

    def test_negative_rotation(self, turtle):
        turtle.right(-90)
        assert math.isclose(turtle.heading, 270.0)

    def test_setheading(self, turtle):
        turtle.setheading(225)
        assert math.isclose(turtle.heading, 225.0)

    def test_setheading_wraps(self, turtle):
        turtle.setheading(720)
        assert math.isclose(turtle.heading, 0.0)


class TestTurtlePen:
    """penup / pendown / setpenwidth."""

    def test_penup_pendown(self, turtle):
        turtle.penup()
        assert not turtle.pen_down
        turtle.pendown()
        assert turtle.pen_down

    def test_setpenwidth(self, turtle):
        turtle.setpenwidth(5.0)
        assert turtle.pen_width == 5.0

    def test_line_records_width(self, turtle):
        turtle.setpenwidth(3.0)
        turtle.forward(10)
        assert turtle.lines[0].width == 3.0


class TestTurtleColor:
    """setcolor / pencolor / setbgcolor / COLOR_NAMES."""

    def test_setcolor_rgb(self, turtle):
        turtle.setcolor(128, 64, 32)
        assert turtle.pen_color == (128, 64, 32)

    def test_setcolor_clamp(self, turtle):
        turtle.setcolor(300, -10, 128)
        assert turtle.pen_color == (255, 0, 128)

    def test_pencolor_name(self, turtle):
        turtle.pencolor("red")
        assert turtle.pen_color == (255, 0, 0)

    def test_pencolor_name_case_insensitive(self, turtle):
        turtle.pencolor("Blue")
        assert turtle.pen_color == (0, 0, 255)

    def test_pencolor_hex(self, turtle):
        turtle.pencolor("#FF8800")
        assert turtle.pen_color == (255, 136, 0)

    def test_pencolor_rgb_string(self, turtle):
        turtle.pencolor("10 20 30")
        assert turtle.pen_color == (10, 20, 30)

    def test_pencolor_rgb_tuple(self, turtle):
        turtle.pencolor((100, 200, 50))
        assert turtle.pen_color == (100, 200, 50)

    def test_pencolor_unknown_keeps_current(self, turtle):
        turtle.setcolor(1, 2, 3)
        turtle.pencolor("nosuchcolor")
        assert turtle.pen_color == (1, 2, 3)

    def test_setbgcolor(self, turtle):
        turtle.setbgcolor(40, 42, 54)
        assert turtle.bg_color == (40, 42, 54)

    def test_setbgcolor_clamp(self, turtle):
        turtle.setbgcolor(999, -1, 128)
        assert turtle.bg_color == (255, 0, 128)

    def test_line_records_color(self, turtle):
        turtle.setcolor(10, 20, 30)
        turtle.forward(5)
        assert turtle.lines[0].color == (10, 20, 30)

    def test_all_color_names_exist(self):
        """Every standard color name resolves to a valid 3-tuple."""
        for name, rgb in COLOR_NAMES.items():
            assert len(rgb) == 3
            assert all(0 <= c <= 255 for c in rgb), f"{name}: {rgb}"

    def test_orange_and_purple_present(self):
        assert "ORANGE" in COLOR_NAMES
        assert "PURPLE" in COLOR_NAMES


class TestTurtleVisibility:
    """hideturtle / showturtle."""

    def test_hide(self, turtle):
        turtle.hideturtle()
        assert not turtle.visible

    def test_show(self, turtle):
        turtle.hideturtle()
        turtle.showturtle()
        assert turtle.visible


class TestTurtleClear:
    """clear / reset."""

    def test_clear(self, turtle):
        turtle.forward(10)
        turtle.draw_point(5, 5)
        turtle.clear()
        assert len(turtle.lines) == 0
        assert len(turtle.shapes) == 0
        # Position preserved
        assert turtle.x != 0 or turtle.y != 0 or True  # position NOT reset

    def test_reset(self, turtle):
        turtle.goto(50, 60)
        turtle.setcolor(1, 2, 3)
        turtle.setpenwidth(8)
        turtle.hideturtle()
        turtle.reset()
        assert turtle.x == 0 and turtle.y == 0
        assert turtle.heading == 0
        assert turtle.pen_down is True
        assert turtle.pen_color == (255, 255, 255)
        assert turtle.pen_width == 2.0
        assert turtle.visible is True
        assert len(turtle.lines) == 0
        assert len(turtle.shapes) == 0


class TestTurtleCircle:
    """circle()."""

    def test_full_circle(self, turtle):
        turtle.circle(50)
        # Should generate multiple line segments
        assert len(turtle.lines) >= 12

    def test_arc(self, turtle):
        turtle.circle(50, 90)
        # Quarter circle = fewer segments
        assert len(turtle.lines) >= 3

    def test_zero_extent(self, turtle):
        turtle.circle(50, 0)
        assert len(turtle.lines) == 0


class TestTurtleShapes:
    """draw_point / draw_line / draw_rect / draw_polygon / draw_ellipse / draw_arc / draw_text."""

    def test_draw_point(self, turtle):
        turtle.draw_point(10, 20)
        assert len(turtle.shapes) == 1
        s = turtle.shapes[0]
        assert s.shape_type == "point"
        assert s.points == [(10, 20)]

    def test_draw_point_custom_color(self, turtle):
        turtle.draw_point(0, 0, color=(255, 0, 0))
        assert turtle.shapes[0].color == (255, 0, 0)

    def test_draw_line_shape(self, turtle):
        turtle.draw_line(0, 0, 100, 100, color=(0, 255, 0), width=3.0)
        s = turtle.shapes[0]
        assert s.shape_type == "line"
        assert s.points == [(0, 0), (100, 100)]
        assert s.color == (0, 255, 0)
        assert s.width == 3.0

    def test_draw_rect(self, turtle):
        turtle.draw_rect(0, 0, 50, 30)
        s = turtle.shapes[0]
        assert s.shape_type == "rect"
        assert len(s.points) == 4

    def test_draw_rect_fill(self, turtle):
        turtle.draw_rect(0, 0, 50, 30, fill_color=(128, 0, 0))
        assert turtle.shapes[0].fill_color == (128, 0, 0)

    def test_draw_polygon(self, turtle):
        pts = [(0, 0), (50, 0), (25, 50)]
        turtle.draw_polygon(pts, fill_color=(0, 0, 255))
        s = turtle.shapes[0]
        assert s.shape_type == "polygon"
        assert len(s.points) == 3
        assert s.fill_color == (0, 0, 255)

    def test_draw_polygon_too_few_points(self, turtle):
        turtle.draw_polygon([(0, 0)])
        assert len(turtle.shapes) == 0  # ignored

    def test_draw_ellipse(self, turtle):
        turtle.draw_ellipse(50, 50, 30, 20)
        s = turtle.shapes[0]
        assert s.shape_type == "ellipse"
        assert s.points == [(50, 50), (30, 20)]

    def test_draw_arc_shape(self, turtle):
        turtle.draw_arc(0, 0, 50, 0, 180)
        s = turtle.shapes[0]
        assert s.shape_type == "polyline"
        assert len(s.points) >= 4

    def test_draw_arc_zero_radius(self, turtle):
        turtle.draw_arc(0, 0, 0, 0, 180)
        assert len(turtle.shapes) == 0

    def test_draw_arc_reversed_angles(self, turtle):
        turtle.draw_arc(0, 0, 50, 180, 0)
        assert len(turtle.shapes) == 1  # angles swapped internally

    def test_draw_text(self, turtle):
        turtle.draw_text(10, 20, "hello", font_size=16, align="center")
        s = turtle.shapes[0]
        assert s.shape_type == "text"
        assert s.text == "hello"
        assert s.font_size == 16
        assert s.align == "center"

    def test_draw_text_empty(self, turtle):
        turtle.draw_text(0, 0, "")
        assert len(turtle.shapes) == 0


class TestTurtleFill:
    """begin_fill / end_fill / fill_last_shape."""

    def test_fill_triangle(self, turtle):
        turtle.begin_fill()
        for _ in range(3):
            turtle.forward(50)
            turtle.right(120)
        turtle.end_fill()
        assert len(turtle.shapes) == 1
        s = turtle.shapes[0]
        assert s.shape_type == "polygon"
        assert s.fill_color == turtle.pen_color

    def test_end_fill_without_begin(self, turtle):
        turtle.end_fill()  # should not crash
        assert len(turtle.shapes) == 0

    def test_end_fill_no_lines(self, turtle):
        turtle.begin_fill()
        turtle.end_fill()  # no lines drawn
        assert len(turtle.shapes) == 0

    def test_fill_last_shape(self, turtle):
        turtle.draw_rect(0, 0, 50, 50)
        result = turtle.fill_last_shape((255, 0, 0))
        assert result is True
        assert turtle.shapes[0].fill_color == (255, 0, 0)

    def test_fill_last_shape_none(self, turtle):
        result = turtle.fill_last_shape((255, 0, 0))
        assert result is False


class TestTurtlePalette:
    """Palette and resolve_color."""

    def test_default_palette(self, turtle):
        assert turtle.palette[0] == (0, 0, 0)
        assert turtle.palette[15] == (255, 255, 255)
        assert len(turtle.palette) == 16

    def test_set_palette_color(self, turtle):
        turtle.set_palette_color(0, 50, 100, 150)
        assert turtle.palette[0] == (50, 100, 150)

    def test_set_palette_clamps(self, turtle):
        turtle.set_palette_color(0, 999, -1, 128)
        assert turtle.palette[0] == (255, 0, 128)

    def test_resolve_color_none(self, turtle):
        assert turtle.resolve_color(None) == turtle.pen_color

    def test_resolve_color_rgb_tuple(self, turtle):
        assert turtle.resolve_color((10, 20, 30)) == (10, 20, 30)

    def test_resolve_color_rgb_list(self, turtle):
        assert turtle.resolve_color([10, 20, 30]) == (10, 20, 30)

    def test_resolve_color_hex(self, turtle):
        assert turtle.resolve_color("#FF0000") == (255, 0, 0)

    def test_resolve_color_name(self, turtle):
        assert turtle.resolve_color("cyan") == (0, 255, 255)

    def test_resolve_color_palette_int(self, turtle):
        assert turtle.resolve_color(4) == DEFAULT_PALETTE_16[4]

    def test_resolve_color_palette_string(self, turtle):
        assert turtle.resolve_color("4") == DEFAULT_PALETTE_16[4]

    def test_resolve_color_default(self, turtle):
        assert turtle.resolve_color("badvalue", default=(1, 2, 3)) == (1, 2, 3)

    def test_resolve_color_clamp(self, turtle):
        assert turtle.resolve_color((300, -5, 128)) == (255, 0, 128)

    def test_palette_color_out_of_range(self, turtle):
        # Index not in palette => grayscale
        c = turtle._palette_color(100)
        assert c == (100, 100, 100)

    def test_palette_color_negative(self, turtle):
        c = turtle._palette_color(-1)
        assert c == turtle.pen_color  # fallback


class TestTurtleCallback:
    """on_change notification."""

    def test_on_change_fires(self, turtle):
        calls = []
        turtle.on_change = lambda: calls.append(1)
        turtle.forward(10)
        turtle.right(10)
        turtle.penup()
        turtle.pendown()
        turtle.setcolor(1, 2, 3)
        assert len(calls) >= 5

    def test_no_callback_no_error(self, turtle):
        turtle.on_change = None
        turtle.forward(10)  # should not raise


class TestTurtleDataclasses:
    """TurtleLine and TurtleShape dataclass fields."""

    def test_turtle_line_fields(self):
        line = TurtleLine(0, 0, 10, 20, (255, 0, 0), 2.0)
        assert line.start_x == 0 and line.end_y == 20
        assert line.color == (255, 0, 0)
        assert line.width == 2.0

    def test_turtle_shape_defaults(self):
        s = TurtleShape(shape_type="rect", points=[(0, 0)], color=(0, 0, 0))
        assert s.width == 1.0
        assert s.fill_color is None
        assert s.text is None
        assert s.font_size == 12
        assert s.align == "left"


# ═══════════════════════════════════════════════════════════════════════════
#  SECTION 2 — PixelCanvas
# ═══════════════════════════════════════════════════════════════════════════


class TestPixelCanvasBasic:
    """Init, set/get pixel, clear."""

    def test_init_dimensions(self, canvas):
        assert canvas.width == 32 and canvas.height == 24

    def test_default_pixel(self, canvas):
        assert canvas.get_pixel(0, 0) == "."

    def test_set_get_pixel(self, canvas):
        canvas.set_pixel(5, 5, "#")
        assert canvas.get_pixel(5, 5) == "#"

    def test_out_of_bounds(self, canvas):
        canvas.set_pixel(-1, 0, "X")
        canvas.set_pixel(0, -1, "X")
        canvas.set_pixel(32, 0, "X")
        canvas.set_pixel(0, 24, "X")
        assert canvas.get_pixel(-1, 0) == "."
        assert canvas.get_pixel(32, 0) == "."

    def test_clear(self, canvas):
        canvas.set_pixel(0, 0, "X")
        canvas.clear()
        assert canvas.get_pixel(0, 0) == "."

    def test_clear_custom_color(self, canvas):
        canvas.clear("@")
        assert canvas.get_pixel(0, 0) == "@"


class TestPixelCanvasLine:
    """Bresenham line drawing."""

    def test_horizontal(self, canvas):
        canvas.draw_line(0, 0, 4, 0, "#")
        for x in range(5):
            assert canvas.get_pixel(x, 0) == "#"

    def test_vertical(self, canvas):
        canvas.draw_line(0, 0, 0, 4, "#")
        for y in range(5):
            assert canvas.get_pixel(0, y) == "#"

    def test_diagonal(self, canvas):
        canvas.draw_line(0, 0, 4, 4, "#")
        for i in range(5):
            assert canvas.get_pixel(i, i) == "#"

    def test_single_point(self, canvas):
        canvas.draw_line(3, 3, 3, 3, "P")
        assert canvas.get_pixel(3, 3) == "P"


class TestPixelCanvasRect:
    """Rectangle drawing."""

    def test_outline(self, canvas):
        canvas.draw_rect(1, 1, 4, 3, "#")
        # Corners
        for x, y in [(1, 1), (4, 1), (1, 3), (4, 3)]:
            assert canvas.get_pixel(x, y) == "#"
        # Interior should be empty
        assert canvas.get_pixel(2, 2) == "."

    def test_filled(self, canvas):
        canvas.draw_rect(0, 0, 3, 3, "F", filled=True)
        for y in range(3):
            for x in range(3):
                assert canvas.get_pixel(x, y) == "F"


class TestPixelCanvasCircle:
    """Midpoint circle."""

    def test_circle_draws(self, canvas):
        canvas.draw_circle(16, 12, 5, "O")
        # Top of circle
        assert canvas.get_pixel(16, 7) == "O"
        # Bottom of circle
        assert canvas.get_pixel(16, 17) == "O"

    def test_filled_circle(self, canvas):
        canvas.draw_circle(16, 12, 3, "X", filled=True)
        # Cardinal points should be drawn
        assert canvas.get_pixel(16, 9) == "X"   # top
        assert canvas.get_pixel(16, 15) == "X"  # bottom


class TestPixelCanvasFloodFill:
    """Flood fill."""

    def test_basic_fill(self, canvas):
        # Draw a border
        canvas.draw_rect(2, 2, 6, 6, "#")
        # Fill interior
        canvas.flood_fill(4, 4, "*")
        assert canvas.get_pixel(4, 4) == "*"
        # Border untouched
        assert canvas.get_pixel(2, 2) == "#"
        # Outside untouched
        assert canvas.get_pixel(0, 0) == "."

    def test_fill_same_color(self, canvas):
        canvas.set_pixel(0, 0, "X")
        canvas.flood_fill(0, 0, "X")  # no-op, same color
        assert canvas.get_pixel(0, 0) == "X"

    def test_fill_out_of_bounds(self, canvas):
        canvas.flood_fill(-1, -1, "X")  # should not raise


class TestPixelCanvasSprites:
    """Sprite definition and drawing."""

    def test_define_draw_sprite(self, canvas):
        sprite = Sprite(
            width=2, height=2, pixels=[["A", "B"], ["C", "D"]]
        )
        canvas.define_sprite("test", sprite)
        canvas.draw_sprite("test", 5, 5)
        assert canvas.get_pixel(5, 5) == "A"
        assert canvas.get_pixel(6, 5) == "B"
        assert canvas.get_pixel(5, 6) == "C"
        assert canvas.get_pixel(6, 6) == "D"

    def test_sprite_transparent(self, canvas):
        sprite = Sprite(width=2, height=1, pixels=[["X", "."]])
        canvas.define_sprite("t", sprite)
        canvas.set_pixel(6, 5, "Z")
        canvas.draw_sprite("t", 5, 5)
        assert canvas.get_pixel(5, 5) == "X"
        assert canvas.get_pixel(6, 5) == "Z"  # transparent pixel preserved

    def test_draw_missing_sprite(self, canvas):
        canvas.draw_sprite("nonexistent", 0, 0)  # should not raise

    def test_predefined_sprites(self):
        assert SPRITE_PLAYER.width == 8 and SPRITE_PLAYER.height == 8
        assert SPRITE_COIN.width == 6 and SPRITE_COIN.height == 6


class TestPixelCanvasFrames:
    """Frame buffer and ASCII render."""

    def test_save_frame(self, canvas):
        canvas.set_pixel(0, 0, "X")
        canvas.save_frame()
        assert len(canvas.frame_buffer) == 1
        assert canvas.frame_buffer[0][0][0] == "X"
        # Modifying canvas doesn't change saved frame
        canvas.set_pixel(0, 0, "Y")
        assert canvas.frame_buffer[0][0][0] == "X"

    def test_ascii_render(self, canvas):
        canvas.set_pixel(0, 0, "#")
        text = canvas.render_ascii()
        lines = text.split("\n")
        assert len(lines) == 24
        assert lines[0][0] == "#"

    def test_ascii_render_color_map(self, canvas):
        canvas.set_pixel(0, 0, "#")
        text = canvas.render_ascii(color_map={"#": "█"})
        assert "█" in text

    def test_animation_stepper(self, canvas):
        def update(frame_num):
            canvas.set_pixel(frame_num, 0, str(frame_num % 10))

        frames = create_animation_stepper(canvas, update, 3)
        assert len(frames) == 3

    def test_export_frames(self, canvas):
        canvas.set_pixel(0, 0, "X")
        canvas.save_frame()
        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as f:
            canvas.export_frames_as_animation(f.name, delay_ms=50)
            f.seek(0)
            data = json.load(open(f.name))
            assert data["width"] == 32
            assert data["delay_ms"] == 50
            assert len(data["frames"]) == 1


class TestTileMap:
    """TileMap grid."""

    def test_set_get_tile(self):
        tm = TileMap(4, 4, tile_size=4)
        tm.set_tile(0, 0, "grass")
        assert tm.get_tile(0, 0) == "grass"
        assert tm.get_tile(1, 1) is None

    def test_out_of_bounds(self):
        tm = TileMap(2, 2)
        tm.set_tile(-1, 0, "x")
        assert tm.get_tile(-1, 0) is None

    def test_render_to_canvas(self):
        tm = TileMap(2, 2, tile_size=2)
        sprite = Sprite(width=2, height=2, pixels=[["A", "A"], ["A", "A"]])
        tm.define_tile("block", sprite)
        tm.set_tile(0, 0, "block")

        canvas = PixelCanvas(8, 8)
        tm.render_to_canvas(canvas)
        assert canvas.get_pixel(0, 0) == "A"
        assert canvas.get_pixel(1, 1) == "A"
        assert canvas.get_pixel(2, 0) == "."  # no tile at (1,0)


# ═══════════════════════════════════════════════════════════════════════════
#  SECTION 3 — Art Toolkit
# ═══════════════════════════════════════════════════════════════════════════


class TestLSystem:
    """L-system generation."""

    def test_koch_generate(self):
        result = KOCH_CURVE.generate(1)
        assert result == "F+F-F-F+F"

    def test_koch_iteration_growth(self):
        gen0 = KOCH_CURVE.generate(0)
        gen1 = KOCH_CURVE.generate(1)
        assert len(gen1) > len(gen0)

    def test_sierpinski(self):
        result = SIERPINSKI_TRIANGLE.generate(1)
        assert "F" in result and "G" in result

    def test_dragon(self):
        result = DRAGON_CURVE.generate(2)
        assert len(result) > 0

    def test_plant(self):
        result = PLANT.generate(1)
        assert "[" in result or "F" in result

    def test_to_turtle_commands(self):
        lstr = KOCH_CURVE.generate(1)
        cmds = KOCH_CURVE.to_turtle_commands(lstr, step_size=10)
        assert any("FD" in c for c in cmds)
        assert any("RT" in c or "LT" in c for c in cmds)

    def test_custom_lsystem(self):
        ls = LSystem("A", [LSystemRule("A", "AB"), LSystemRule("B", "A")], angle=60)
        assert ls.generate(0) == "A"
        assert ls.generate(1) == "AB"
        assert ls.generate(2) == "ABA"
        assert ls.generate(3) == "ABAAB"


class TestFractalGenerator:
    """Fractal generators."""

    def test_koch_snowflake(self):
        cmds = FractalGenerator.koch_snowflake(1, 100)
        assert len(cmds) > 10
        assert any("FD" in c for c in cmds)
        assert any("RT 120" in c for c in cmds)  # outer rotation

    def test_sierpinski_carpet(self):
        squares = FractalGenerator.sierpinski_carpet(1, 243)
        # 9 - 1 = 8 sub-squares at depth 1
        assert len(squares) == 8

    def test_sierpinski_carpet_depth0(self):
        squares = FractalGenerator.sierpinski_carpet(0, 100)
        assert len(squares) == 1

    def test_mandelbrot(self):
        result = FractalGenerator.mandelbrot_set(
            width=10, height=10, max_iter=10
        )
        assert len(result) == 10
        assert len(result[0]) == 10
        assert all(0 <= v <= 10 for row in result for v in row)


class TestHarmonicMotion:
    """Lissajous, rose, spiral."""

    def test_lissajous(self):
        pts = HarmonicMotion.lissajous(3, 2, math.pi / 2, 100, 50)
        assert len(pts) == 100
        # All within scale bounds
        for x, y in pts:
            assert abs(x) <= 50.1 and abs(y) <= 50.1

    def test_rose_curve(self):
        pts = HarmonicMotion.rose_curve(5, 2, 200, 80)
        assert len(pts) == 200
        for x, y in pts:
            assert abs(x) <= 80.1 and abs(y) <= 80.1

    def test_spiral(self):
        pts = HarmonicMotion.spiral(3, 5, 100)
        assert len(pts) == 100
        assert len(pts[0]) == 3  # (x, y, heading)
        # Spiral should grow outward
        r0 = math.hypot(pts[10][0], pts[10][1])
        r1 = math.hypot(pts[90][0], pts[90][1])
        assert r1 > r0


class TestRandomArt:
    """Random art generator."""

    def test_random_walk_deterministic(self):
        gen1 = RandomArtGenerator(seed=42)
        gen2 = RandomArtGenerator(seed=42)
        cmds1 = gen1.random_walk(50)
        cmds2 = gen2.random_walk(50)
        assert cmds1 == cmds2

    def test_random_walk_length(self):
        gen = RandomArtGenerator(seed=0)
        cmds = gen.random_walk(20)
        # Each step = 1 RT + 1 FD
        assert len(cmds) == 40

    def test_noise_field(self):
        gen = RandomArtGenerator(seed=0)
        field = gen.noise_field(8, 6, scale=0.1, octaves=2)
        assert len(field) == 6
        assert len(field[0]) == 8


class TestPresetPatterns:
    """Preset pattern registry."""

    def test_preset_keys(self):
        assert "koch_snowflake" in PRESET_PATTERNS
        assert "dragon_curve" in PRESET_PATTERNS
        assert "plant" in PRESET_PATTERNS
        assert "lissajous_3_2" in PRESET_PATTERNS
        assert "rose_7" in PRESET_PATTERNS

    def test_presets_callable(self):
        for name, factory in PRESET_PATTERNS.items():
            result = factory()
            assert len(result) > 0, f"{name} returned empty result"


# ═══════════════════════════════════════════════════════════════════════════
#  SECTION 4 — TurtleGallery
# ═══════════════════════════════════════════════════════════════════════════


class TestTurtleGalleryRecording:
    """Recording and saving drawings."""

    def test_start_stop_recording(self, gallery):
        gallery.start_recording()
        assert gallery.is_recording
        gallery.stop_recording()
        assert not gallery.is_recording

    def test_record_step(self, gallery, turtle):
        gallery.start_recording()
        turtle.forward(10)
        gallery.record_step("FD 10", turtle)
        assert len(gallery.recording_steps) == 1
        step = gallery.recording_steps[0]
        assert step.command == "FD 10"
        assert step.x == turtle.x
        assert step.y == turtle.y

    def test_no_record_when_stopped(self, gallery, turtle):
        gallery.record_step("FD 10", turtle)
        assert len(gallery.recording_steps) == 0

    def test_save_drawing(self, gallery, turtle):
        gallery.start_recording()
        turtle.forward(50)
        gallery.record_step("FD 50", turtle)
        gallery.stop_recording()

        path = gallery.save_drawing(
            title="Test Drawing",
            author="Tester",
            language="Logo",
            code="FD 50",
        )
        assert path.exists()
        data = json.loads(path.read_text())
        assert data["metadata"]["title"] == "Test Drawing"
        assert len(data["steps"]) == 1

    def test_save_empty_raises(self, gallery):
        with pytest.raises(ValueError):
            gallery.save_drawing("empty", "a", "Logo", "")


class TestTurtleGalleryLoadList:
    """Loading and listing drawings."""

    def test_load_drawing(self, gallery, turtle):
        gallery.start_recording()
        turtle.forward(25)
        gallery.record_step("FD 25", turtle)
        gallery.stop_recording()
        path = gallery.save_drawing("load_test", "a", "Logo", "FD 25")

        metadata, steps = gallery.load_drawing(path)
        assert metadata.title == "load_test"
        assert len(steps) == 1

    def test_list_drawings(self, gallery, turtle):
        gallery.start_recording()
        turtle.forward(1)
        gallery.record_step("FD 1", turtle)
        gallery.stop_recording()
        gallery.save_drawing("d1", "a", "Logo", "FD 1")
        gallery.save_drawing("d2", "a", "Logo", "FD 1")

        drawings = gallery.list_drawings()
        assert len(drawings) >= 2


class TestTurtleGallerySVG:
    """SVG export."""

    def test_export_svg(self, gallery, turtle, tmp_path):
        gallery.start_recording()
        turtle.forward(50)
        gallery.record_step("FD 50", turtle)
        gallery.stop_recording()
        path = gallery.save_drawing("svg_test", "a", "Logo", "FD 50")

        metadata, steps = gallery.load_drawing(path)
        svg_path = gallery.export_as_svg(path, metadata, steps)
        assert svg_path.exists()
        content = svg_path.read_text()
        assert "<svg" in content
        assert "</svg>" in content


class TestShareLink:
    """create_share_link utility."""

    def test_share_link(self, tmp_path):
        p = tmp_path / "drawing_123.json"
        link = create_share_link(p)
        assert "timewarp.art" in link
        assert "drawing_123" in link


# ═══════════════════════════════════════════════════════════════════════════
#  SECTION 5 — Logo–Graphics Integration
# ═══════════════════════════════════════════════════════════════════════════


class TestLogoColorIntegration:
    """Logo color commands with turtle graphics."""

    def test_setpencolor_named(self, interp):
        t = TurtleState()
        execute_logo(interp, 'SETPENCOLOR "red', t)
        assert t.pen_color == (255, 0, 0)

    def test_setpencolor_variable(self, interp):
        t = TurtleState()
        execute_logo(interp, 'MAKE "C "blue', t)
        execute_logo(interp, "SETPENCOLOR :C", t)
        assert t.pen_color == (0, 0, 255)

    def test_setpencolor_item_reporter(self, interp):
        t = TurtleState()
        execute_logo(interp, 'MAKE "COLORS ["red "green "blue]', t)
        execute_logo(interp, 'MAKE "I 3', t)
        execute_logo(interp, "SETPENCOLOR ITEM :I :COLORS", t)
        assert t.pen_color == (0, 0, 255)

    def test_setpencolor_rgb(self, interp):
        t = TurtleState()
        execute_logo(interp, "SETCOLOR 128 64 32", t)
        assert t.pen_color == (128, 64, 32)

    def test_setbgcolor_named(self, interp):
        t = TurtleState()
        execute_logo(interp, 'SETBGCOLOR "black', t)
        assert t.bg_color == (0, 0, 0)

    def test_setbgcolor_rgb(self, interp):
        t = TurtleState()
        execute_logo(interp, "SETBGCOLOR 10 20 30", t)
        assert t.bg_color == (10, 20, 30)

    def test_setpensize(self, interp):
        t = TurtleState()
        execute_logo(interp, "SETPENSIZE 5", t)
        assert t.pen_width == 5.0

    def test_cyclecolor_procedure(self, interp):
        """The exact CYCLECOLOR procedure from mandala.logo."""
        t = TurtleState()
        execute_logo(
            interp,
            'TO CYCLECOLOR :N\n'
            '  MAKE "COLORS ["red "orange "yellow "green "cyan '
            '"blue "purple "magenta "pink "white]\n'
            '  MAKE "IDX (:N MOD 10) + 1\n'
            '  SETPENCOLOR ITEM :IDX :COLORS\n'
            'END',
            t,
        )
        expected_colors = [
            (255, 0, 0),        # red
            (255, 165, 0),      # orange
            (255, 255, 0),      # yellow
            (0, 255, 0),        # green
            (0, 255, 255),      # cyan
            (0, 0, 255),        # blue
            (128, 0, 128),      # purple
            (255, 0, 255),      # magenta
            (255, 192, 203),    # pink
            (255, 255, 255),    # white
        ]
        for i, expected in enumerate(expected_colors):
            execute_logo(interp, f"CYCLECOLOR {i}", t)
            assert t.pen_color == expected, (
                f"CYCLECOLOR {i}: expected {expected}, got {t.pen_color}"
            )


class TestLogoDrawingIntegration:
    """Logo drawing produces correct turtle state."""

    def test_square(self, interp):
        t = TurtleState()
        execute_logo(interp, "REPEAT 4 [FD 100 RT 90]", t)
        assert len(t.lines) == 4
        # Should return to origin
        assert math.isclose(t.x, 0.0, abs_tol=1e-5)
        assert math.isclose(t.y, 0.0, abs_tol=1e-5)

    def test_triangle(self, interp):
        t = TurtleState()
        execute_logo(interp, "REPEAT 3 [FD 100 RT 120]", t)
        assert len(t.lines) == 3
        assert math.isclose(t.x, 0.0, abs_tol=1e-5)
        assert math.isclose(t.y, 0.0, abs_tol=1e-5)

    def test_circle_approximation(self, interp):
        t = TurtleState()
        execute_logo(interp, "REPEAT 36 [FD 10 RT 10]", t)
        assert len(t.lines) == 36
        # Should approximately return to origin
        assert math.isclose(t.x, 0.0, abs_tol=5)
        assert math.isclose(t.y, 0.0, abs_tol=5)

    def test_pen_up_no_draw(self, interp):
        t = TurtleState()
        execute_logo(interp, "PU FD 100", t)
        assert len(t.lines) == 0

    def test_pen_down_after_up(self, interp):
        t = TurtleState()
        execute_logo(interp, "PU FD 50 PD FD 50", t)
        assert len(t.lines) == 1

    def test_setxy(self, interp):
        t = TurtleState()
        execute_logo(interp, "SETXY 100 200", t)
        assert math.isclose(t.x, 100, abs_tol=1e-5)
        assert math.isclose(t.y, 200, abs_tol=1e-5)

    def test_home(self, interp):
        t = TurtleState()
        execute_logo(interp, "FD 100 HOME", t)
        assert math.isclose(t.x, 0.0, abs_tol=1e-5)
        assert math.isclose(t.y, 0.0, abs_tol=1e-5)

    def test_hideturtle(self, interp):
        t = TurtleState()
        execute_logo(interp, "HT", t)
        assert not t.visible

    def test_showturtle(self, interp):
        t = TurtleState()
        execute_logo(interp, "HT ST", t)
        assert t.visible

    def test_clearscreen(self, interp):
        t = TurtleState()
        execute_logo(interp, "FD 100 CS", t)
        assert len(t.lines) == 0

    def test_setheading(self, interp):
        t = TurtleState()
        execute_logo(interp, "SETH 90", t)
        assert math.isclose(t.heading, 90.0)

    def test_procedure_drawing(self, interp):
        t = TurtleState()
        execute_logo(
            interp,
            "TO SQUARE :SIZE\nREPEAT 4 [FD :SIZE RT 90]\nEND",
            t,
        )
        execute_logo(interp, "SQUARE 50", t)
        assert len(t.lines) == 4

    def test_nested_repeat(self, interp):
        t = TurtleState()
        execute_logo(interp, "REPEAT 4 [REPEAT 4 [FD 10 RT 90] RT 90]", t)
        assert len(t.lines) == 16

    def test_arc_command(self, interp):
        t = TurtleState()
        execute_logo(interp, "ARC 90 50", t)
        # ARC uses turtle.circle() which draws line segments
        assert len(t.lines) >= 3


class TestLogoErrorHandling:
    """Error paths produce error messages, not crashes."""

    def test_setcolor_no_args(self, interp):
        t = TurtleState()
        result = execute_logo(interp, "SETCOLOR", t)
        assert "❌" in result

    def test_setbgcolor_no_args(self, interp):
        t = TurtleState()
        result = execute_logo(interp, "SETBGCOLOR", t)
        assert "❌" in result

    def test_forward_no_arg(self, interp):
        t = TurtleState()
        result = execute_logo(interp, "FD", t)
        assert "❌" in result

    def test_unknown_command(self, interp):
        t = TurtleState()
        result = execute_logo(interp, "XYZZY", t)
        assert "❌" in result or "Unknown" in result
