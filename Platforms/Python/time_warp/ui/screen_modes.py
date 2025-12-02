"""Screen mode definitions for retro computing display modes.

This module provides classic screen modes from vintage computers,
supporting both text modes (character-based) and graphics modes.
"""

from dataclasses import dataclass
from enum import Enum, auto
from typing import Tuple


class ModeType(Enum):
    """Type of screen mode."""

    TEXT = auto()  # Character-based text mode
    GRAPHICS = auto()  # Pixel-based graphics mode
    MIXED = auto()  # Combined text and graphics


@dataclass
class ScreenMode:
    """Definition of a screen mode."""

    name: str
    mode_number: int
    mode_type: ModeType
    width: int  # Pixels for graphics, columns for text
    height: int  # Pixels for graphics, rows for text
    colors: int  # Number of colors available
    description: str
    char_width: int = 8  # Character cell width in pixels
    char_height: int = 8  # Character cell height in pixels
    aspect_ratio: float = 1.0  # Pixel aspect ratio (for CRT simulation)

    @property
    def resolution_str(self) -> str:
        """Get resolution as a string."""
        if self.mode_type == ModeType.TEXT:
            return f"{self.width}×{self.height} chars"
        return f"{self.width}×{self.height}"

    @property
    def pixel_size(self) -> Tuple[int, int]:
        """Get total pixel dimensions (for text modes, calculates from chars)."""
        if self.mode_type == ModeType.TEXT:
            return (self.width * self.char_width, self.height * self.char_height)
        return (self.width, self.height)


class ScreenModeManager:
    """Manages available screen modes and current mode."""

    def __init__(self):
        self.modes = {
            # === CLASSIC TEXT MODES ===
            0: ScreenMode(
                name="40×25 Color Text",
                mode_number=0,
                mode_type=ModeType.TEXT,
                width=40,
                height=25,
                colors=16,
                description="CGA 40-column color text mode (IBM PC)",
                char_width=8,
                char_height=8,
            ),
            1: ScreenMode(
                name="80×25 Color Text",
                mode_number=1,
                mode_type=ModeType.TEXT,
                width=80,
                height=25,
                colors=16,
                description="Standard 80-column text mode",
                char_width=8,
                char_height=16,
            ),
            2: ScreenMode(
                name="80×50 High-Res Text",
                mode_number=2,
                mode_type=ModeType.TEXT,
                width=80,
                height=50,
                colors=16,
                description="VGA 80×50 text mode",
                char_width=8,
                char_height=8,
            ),
            # === CGA/EGA GRAPHICS MODES ===
            3: ScreenMode(
                name="320×200 CGA",
                mode_number=3,
                mode_type=ModeType.GRAPHICS,
                width=320,
                height=200,
                colors=4,
                description="CGA 4-color graphics (cyan/magenta/white)",
                aspect_ratio=1.2,  # CGA pixels were taller
            ),
            4: ScreenMode(
                name="640×200 CGA Hi-Res",
                mode_number=4,
                mode_type=ModeType.GRAPHICS,
                width=640,
                height=200,
                colors=2,
                description="CGA 2-color high resolution",
                aspect_ratio=2.4,  # Very wide pixels
            ),
            5: ScreenMode(
                name="320×200 EGA",
                mode_number=5,
                mode_type=ModeType.GRAPHICS,
                width=320,
                height=200,
                colors=16,
                description="EGA 16-color medium resolution",
                aspect_ratio=1.2,
            ),
            6: ScreenMode(
                name="640×350 EGA",
                mode_number=6,
                mode_type=ModeType.GRAPHICS,
                width=640,
                height=350,
                colors=16,
                description="EGA 16-color high resolution",
                aspect_ratio=1.37,
            ),
            # === VGA GRAPHICS MODES ===
            7: ScreenMode(
                name="320×200 VGA",
                mode_number=7,
                mode_type=ModeType.GRAPHICS,
                width=320,
                height=200,
                colors=256,
                description="VGA Mode 13h - 256 colors (classic gaming)",
                aspect_ratio=1.2,
            ),
            8: ScreenMode(
                name="640×480 VGA",
                mode_number=8,
                mode_type=ModeType.GRAPHICS,
                width=640,
                height=480,
                colors=16,
                description="VGA standard graphics mode",
                aspect_ratio=1.0,
            ),
            # === VINTAGE COMPUTER MODES ===
            9: ScreenMode(
                name="40×24 Apple II",
                mode_number=9,
                mode_type=ModeType.TEXT,
                width=40,
                height=24,
                colors=1,
                description="Apple II text mode (green phosphor)",
                char_width=7,
                char_height=8,
            ),
            10: ScreenMode(
                name="280×192 Apple II Hi-Res",
                mode_number=10,
                mode_type=ModeType.GRAPHICS,
                width=280,
                height=192,
                colors=6,
                description="Apple II high-resolution graphics",
                aspect_ratio=1.0,
            ),
            11: ScreenMode(
                name="40×25 C64",
                mode_number=11,
                mode_type=ModeType.TEXT,
                width=40,
                height=25,
                colors=16,
                description="Commodore 64 text mode",
                char_width=8,
                char_height=8,
            ),
            12: ScreenMode(
                name="320×200 C64",
                mode_number=12,
                mode_type=ModeType.GRAPHICS,
                width=320,
                height=200,
                colors=16,
                description="Commodore 64 multicolor bitmap",
                aspect_ratio=1.0,
            ),
            13: ScreenMode(
                name="256×192 ZX Spectrum",
                mode_number=13,
                mode_type=ModeType.GRAPHICS,
                width=256,
                height=192,
                colors=15,
                description="ZX Spectrum graphics (with attributes)",
                aspect_ratio=1.0,
            ),
        }

        self.current_mode = 1  # Default to 80×25 text

    def get_mode(self, mode_number: int) -> ScreenMode:
        """Get a screen mode by number."""
        return self.modes.get(mode_number, self.modes[1])

    def set_mode(self, mode_number: int) -> ScreenMode:
        """Set the current screen mode."""
        if mode_number in self.modes:
            self.current_mode = mode_number
        return self.get_current_mode()

    def get_current_mode(self) -> ScreenMode:
        """Get the current screen mode."""
        return self.modes[self.current_mode]

    def get_text_modes(self) -> list:
        """Get all text modes."""
        return [m for m in self.modes.values() if m.mode_type == ModeType.TEXT]

    def get_graphics_modes(self) -> list:
        """Get all graphics modes."""
        return [m for m in self.modes.values() if m.mode_type == ModeType.GRAPHICS]

    def get_all_modes(self) -> list:
        """Get all modes sorted by number."""
        return sorted(self.modes.values(), key=lambda m: m.mode_number)


# CGA color palette (4-color modes)
CGA_PALETTE_1 = [
    (0, 0, 0),  # Black
    (0, 255, 255),  # Cyan
    (255, 0, 255),  # Magenta
    (255, 255, 255),  # White
]

CGA_PALETTE_2 = [
    (0, 0, 0),  # Black
    (0, 255, 0),  # Green
    (255, 0, 0),  # Red
    (255, 255, 0),  # Yellow (brown)
]

# CGA 16-color text palette
CGA_16_COLORS = [
    (0, 0, 0),  # 0: Black
    (0, 0, 170),  # 1: Blue
    (0, 170, 0),  # 2: Green
    (0, 170, 170),  # 3: Cyan
    (170, 0, 0),  # 4: Red
    (170, 0, 170),  # 5: Magenta
    (170, 85, 0),  # 6: Brown
    (170, 170, 170),  # 7: Light Gray
    (85, 85, 85),  # 8: Dark Gray
    (85, 85, 255),  # 9: Light Blue
    (85, 255, 85),  # 10: Light Green
    (85, 255, 255),  # 11: Light Cyan
    (255, 85, 85),  # 12: Light Red
    (255, 85, 255),  # 13: Light Magenta
    (255, 255, 85),  # 14: Yellow
    (255, 255, 255),  # 15: White
]

# C64 color palette
C64_COLORS = [
    (0, 0, 0),  # 0: Black
    (255, 255, 255),  # 1: White
    (136, 0, 0),  # 2: Red
    (170, 255, 238),  # 3: Cyan
    (204, 68, 204),  # 4: Purple
    (0, 204, 85),  # 5: Green
    (0, 0, 170),  # 6: Blue
    (238, 238, 119),  # 7: Yellow
    (221, 136, 85),  # 8: Orange
    (102, 68, 0),  # 9: Brown
    (255, 119, 119),  # 10: Light Red
    (51, 51, 51),  # 11: Dark Gray
    (119, 119, 119),  # 12: Gray
    (170, 255, 102),  # 13: Light Green
    (0, 136, 255),  # 14: Light Blue
    (187, 187, 187),  # 15: Light Gray
]

# ASCII art character set for graphics simulation
ASCII_SHADING = " .:-=+*#%@"  # Light to dark
ASCII_BLOCKS = " ░▒▓█"  # Block characters


def rgb_to_ascii(r: int, g: int, b: int, use_blocks: bool = False) -> str:
    """Convert RGB color to ASCII character based on brightness."""
    # Calculate luminance
    luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255.0

    chars = ASCII_BLOCKS if use_blocks else ASCII_SHADING
    index = int(luminance * (len(chars) - 1))
    return chars[index]


def quantize_to_palette(
    r: int, g: int, b: int, palette: list
) -> Tuple[int, Tuple[int, int, int]]:
    """Quantize RGB color to nearest palette color.

    Returns (palette_index, (r, g, b)).
    """
    min_dist = float("inf")
    best_index = 0
    best_color = palette[0]

    for i, (pr, pg, pb) in enumerate(palette):
        # Euclidean distance in RGB space
        dist = (r - pr) ** 2 + (g - pg) ** 2 + (b - pb) ** 2
        if dist < min_dist:
            min_dist = dist
            best_index = i
            best_color = (pr, pg, pb)

    return best_index, best_color
