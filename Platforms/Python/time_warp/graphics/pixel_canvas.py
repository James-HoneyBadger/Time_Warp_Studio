"""
Pixel Canvas Mode - 2D grid APIs for sprite drawing, tile maps, and step animations.
"""

# pylint: disable=too-many-arguments,too-many-positional-arguments,duplicate-code

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Callable


@dataclass
class Sprite:
    """Sprite definition with pixel data."""

    width: int
    height: int
    pixels: list[list[str]]  # 2D grid of color codes
    hotspot_x: int = 0
    hotspot_y: int = 0


class PixelCanvas:
    """2D pixel grid for sprite-based graphics."""

    def __init__(self, width: int = 128, height: int = 96):
        self.width = width
        self.height = height
        self.grid: list[list[str]] = [
            ["." for _ in range(width)] for _ in range(height)
        ]
        self.sprites: dict[str, Sprite] = {}
        self.frame_buffer: list[list[list[str]]] = []

    def clear(self, color: str = "."):
        """Clear canvas to background color."""
        self.grid = [
            [color for _ in range(self.width)] for _ in range(self.height)
        ]

    def set_pixel(self, x: int, y: int, color: str):
        """Set individual pixel."""
        if 0 <= x < self.width and 0 <= y < self.height:
            self.grid[y][x] = color

    def get_pixel(self, x: int, y: int) -> str:
        """Get pixel color."""
        if 0 <= x < self.width and 0 <= y < self.height:
            return self.grid[y][x]
        return "."

    def draw_line(self, x0: int, y0: int, x1: int, y1: int, color: str):
        """Draw line using Bresenham's algorithm."""
        dx = abs(x1 - x0)
        dy = abs(y1 - y0)
        sx = 1 if x0 < x1 else -1
        sy = 1 if y0 < y1 else -1
        err = dx - dy

        x, y = x0, y0

        while True:
            self.set_pixel(x, y, color)

            if x == x1 and y == y1:
                break

            e2 = 2 * err
            if e2 > -dy:
                err -= dy
                x += sx
            if e2 < dx:
                err += dx
                y += sy

    def draw_rect(
        self,
        x: int,
        y: int,
        width: int,
        height: int,
        color: str,
        filled: bool = False,
    ):
        """Draw rectangle."""
        if filled:
            for dy in range(height):
                for dx in range(width):
                    self.set_pixel(x + dx, y + dy, color)
        else:
            # Top and bottom
            for dx in range(width):
                self.set_pixel(x + dx, y, color)
                self.set_pixel(x + dx, y + height - 1, color)
            # Left and right
            for dy in range(height):
                self.set_pixel(x, y + dy, color)
                self.set_pixel(x + width - 1, y + dy, color)

    def draw_circle(
        self, cx: int, cy: int, radius: int, color: str, filled: bool = False
    ):
        """Draw circle using midpoint algorithm."""
        x = 0
        y = radius
        d = 1 - radius

        def plot_circle_points(xc: int, yc: int, x: int, y: int):
            points = [
                (xc + x, yc + y),
                (xc - x, yc + y),
                (xc + x, yc - y),
                (xc - x, yc - y),
                (xc + y, yc + x),
                (xc - y, yc + x),
                (xc + y, yc - x),
                (xc - y, yc - x),
            ]
            for px, py in points:
                if filled:
                    # Fill horizontal spans
                    if abs(px - xc) <= abs(x):
                        self.set_pixel(px, py, color)
                else:
                    self.set_pixel(px, py, color)

        while x <= y:
            plot_circle_points(cx, cy, x, y)

            if d < 0:
                d += 2 * x + 3
            else:
                d += 2 * (x - y) + 5
                y -= 1
            x += 1

    def define_sprite(self, name: str, sprite: Sprite):
        """Register a sprite definition."""
        self.sprites[name] = sprite

    def draw_sprite(self, name: str, x: int, y: int, transparent: str = "."):
        """Draw sprite at position."""
        if name not in self.sprites:
            return

        sprite = self.sprites[name]
        for sy in range(sprite.height):
            for sx in range(sprite.width):
                pixel = sprite.pixels[sy][sx]
                if pixel != transparent:
                    self.set_pixel(
                        x + sx - sprite.hotspot_x,
                        y + sy - sprite.hotspot_y,
                        pixel,
                    )

    def flood_fill(self, x: int, y: int, new_color: str):
        """Flood fill from (x, y) with new color."""
        if not (0 <= x < self.width and 0 <= y < self.height):
            return

        old_color = self.grid[y][x]
        if old_color == new_color:
            return

        stack = [(x, y)]

        while stack:
            cx, cy = stack.pop()

            if not (0 <= cx < self.width and 0 <= cy < self.height):
                continue

            if self.grid[cy][cx] != old_color:
                continue

            self.grid[cy][cx] = new_color

            stack.extend(
                [
                    (cx + 1, cy),
                    (cx - 1, cy),
                    (cx, cy + 1),
                    (cx, cy - 1),
                ]
            )

    def save_frame(self):
        """Save current canvas state to frame buffer."""
        frame = [row[:] for row in self.grid]
        self.frame_buffer.append(frame)

    def render_ascii(self, color_map: dict[str, str] | None = None) -> str:
        """Render canvas as ASCII art."""
        if color_map is None:
            color_map = {}

        lines = []
        for row in self.grid:
            line = "".join(color_map.get(pixel, pixel) for pixel in row)
            lines.append(line)

        return "\n".join(lines)

    def export_frames_as_animation(self, filename: str, delay_ms: int = 100):
        """Export frame buffer as animation data (JSON format)."""

        data = {
            "width": self.width,
            "height": self.height,
            "delay_ms": delay_ms,
            "frames": self.frame_buffer,
        }

        with open(filename, "w", encoding="utf-8") as f:
            json.dump(data, f)


class TileMap:
    """Grid-based tile map for game-like graphics."""

    def __init__(self, width: int, height: int, tile_size: int = 8):
        self.width = width
        self.height = height
        self.tile_size = tile_size
        self.tiles: dict[str, Sprite] = {}
        self.map_grid: list[list[str | None]] = [
            [None for _ in range(width)] for _ in range(height)
        ]

    def define_tile(self, tile_id: str, sprite: Sprite):
        """Define a tile type."""
        self.tiles[tile_id] = sprite

    def set_tile(self, x: int, y: int, tile_id: str | None):
        """Set tile at grid position."""
        if 0 <= x < self.width and 0 <= y < self.height:
            self.map_grid[y][x] = tile_id

    def get_tile(self, x: int, y: int) -> str | None:
        """Get tile at grid position."""
        if 0 <= x < self.width and 0 <= y < self.height:
            return self.map_grid[y][x]
        return None

    def render_to_canvas(
        self, canvas: PixelCanvas, offset_x: int = 0, offset_y: int = 0
    ):
        """Render tile map to pixel canvas."""
        for ty in range(self.height):
            for tx in range(self.width):
                tile_id = self.map_grid[ty][tx]
                if tile_id and tile_id in self.tiles:
                    sprite = self.tiles[tile_id]
                    px = offset_x + tx * self.tile_size
                    py = offset_y + ty * self.tile_size

                    for sy in range(sprite.height):
                        for sx in range(sprite.width):
                            canvas.set_pixel(
                                px + sx,
                                py + sy,
                                sprite.pixels[sy][sx],
                            )


# Predefined 8x8 sprites for common uses
SPRITE_PLAYER = Sprite(
    width=8,
    height=8,
    pixels=[
        [".", ".", "#", "#", "#", "#", ".", "."],
        [".", "#", "O", "#", "#", "O", "#", "."],
        [".", "#", "#", "#", "#", "#", "#", "."],
        [".", ".", "#", "#", "#", "#", ".", "."],
        [".", ".", "#", ".", ".", "#", ".", "."],
        [".", "#", "#", "#", "#", "#", "#", "."],
        ["#", "#", ".", "#", "#", ".", "#", "#"],
        ["#", ".", ".", "#", "#", ".", ".", "#"],
    ],
    hotspot_x=4,
    hotspot_y=4,
)

SPRITE_COIN = Sprite(
    width=6,
    height=6,
    pixels=[
        [".", "$", "$", "$", "$", "."],
        ["$", "$", "Y", "Y", "$", "$"],
        ["$", "Y", "Y", "Y", "Y", "$"],
        ["$", "Y", "Y", "Y", "Y", "$"],
        ["$", "$", "Y", "Y", "$", "$"],
        [".", "$", "$", "$", "$", "."],
    ],
    hotspot_x=3,
    hotspot_y=3,
)


def create_animation_stepper(
    canvas: PixelCanvas,
    update_fn: Callable[[int], None],
    frames: int,
) -> list[list[list[str]]]:
    """Helper to create step-by-step animations."""
    for frame_num in range(frames):
        canvas.clear()
        update_fn(frame_num)
        canvas.save_frame()

    return canvas.frame_buffer
