# UI and Creative Features Documentation

This document covers the user interface enhancements and creative tooling features in Time Warp IDE.

## Table of Contents

1. [Error Explorer](#error-explorer)
2. [Focus Mode](#focus-mode)
3. [Guided Onboarding](#guided-onboarding)
4. [Accessibility Themes](#accessibility-themes)
5. [Turtle Gallery](#turtle-gallery)
6. [Procedural Art Toolkit](#procedural-art-toolkit)
7. [Pixel Canvas Mode](#pixel-canvas-mode)

---

## Error Explorer

**Location:** `Platforms/Python/time_warp/ui/error_explorer.py`

Rich error navigator providing:
- **Error List**: Tree view with line numbers, error types, and messages
- **Jump to Line**: Double-click or keyboard shortcut to navigate to errors
- **Error Details**: Full error information with suggestions and state context
- **Color Coding**: Visual distinction between Syntax, Runtime, and Warnings

### Usage

```python
from Platforms.Python.time_warp.ui.error_explorer import ErrorExplorerWidget

# Create and add to main window
error_explorer = ErrorExplorerWidget(self)
self.addDockWidget(Qt.RightDockWidgetArea, error_explorer)

# Connect to jump-to-line signal
error_explorer.error_selected.connect(self.jump_to_line)

# Parse interpreter output
error_explorer.parse_interpreter_output(output_lines, interpreter)
```

### Features

- **Auto-parsing**: Extracts errors from interpreter output (looks for ❌ prefix)
- **Suggestions**: Displays suggestions when available (💡 prefix)
- **State Inspection**: Shows variables and call stack depth at error time
- **Copy Error**: Quick copy error message to clipboard

---

## Focus Mode

**Location:** `Platforms/Python/time_warp/ui/focus_mode.py`

Minimal UI toggle for distraction-free coding.

### Usage

```python
from Platforms/Python/time_warp.ui.focus_mode import FocusModeManager

# Initialize
focus_manager = FocusModeManager(main_window)

# Toggle focus mode (F11)
focus_manager.toggle_focus_mode()

# Programmatic control
focus_manager.enable_focus_mode()
focus_manager.disable_focus_mode()

# Toggle tooltips
focus_manager.toggle_tooltips()
```

### What Gets Hidden

- Menu bar
- Status bar  
- Toolbars
- Non-essential dock widgets (keeps editor and output visible)

### Keyboard Shortcuts

- `F11`: Toggle focus mode
- `Ctrl+Shift+T`: Toggle tooltips
- `Alt+M`: Toggle menu visibility
- `Alt+S`: Toggle status bar

---

## Guided Onboarding

**Location:** `Platforms/Python/time_warp/ui/onboarding.py`

First-run tutorial with interactive tasks.

### Usage

```python
from Platforms.Python.time_warp.ui.onboarding import (
    OnboardingDialog,
    OnboardingManager,
)

# Check if onboarding should be shown
manager = OnboardingManager()
if manager.should_show_onboarding():
    dialog = OnboardingDialog(main_window)
    dialog.step_completed.connect(manager.mark_step_completed)
    dialog.tutorial_finished.connect(
        lambda: manager.mark_tutorial_completed(dialog.should_skip_onboarding())
    )
    dialog.show()
```

### Tutorial Steps

1. **Welcome**: Introduction to Time Warp IDE
2. **Code Editor**: Learn the editor interface
3. **Run Program**: Execute first program
4. **Turtle Graphics**: Draw with turtle commands
5. **Multiple Languages**: Switch between BASIC, PILOT, Logo
6. **Complete**: Finish tutorial with resources

### Persistence

Onboarding state is saved to `~/.Time_Warp/onboarding.json`:

```json
{
  "completed_steps": ["welcome", "editor_pane", "run_program"],
  "tutorial_completed": false,
  "skip_onboarding": false
}
```

---

## Accessibility Themes

**Location:** `Platforms/Python/time_warp/ui/accessibility.py`

High-contrast and dyslexia-friendly themes.

### Available Themes

1. **High Contrast Dark**: Maximum contrast with black background
2. **High Contrast Light**: Maximum contrast with white background
3. **Dyslexia Friendly**: Cream background with OpenDyslexic font
4. **Accessible Dark Blue**: Navy background with enhanced contrast

### Usage

```python
from Platforms.Python.time_warp.ui.accessibility import (
    get_accessibility_theme,
    apply_accessibility_stylesheet,
)

# Get theme
theme = get_accessibility_theme("high_contrast_dark")

# Apply stylesheet
stylesheet = apply_accessibility_stylesheet(theme)
main_window.setStyleSheet(stylesheet)
```

### Features

- **High Contrast Levels**: Standard, high, maximum
- **Dyslexia-Friendly Fonts**: OpenDyslexic (with fallback to Arial)
- **Increased Line Spacing**: 1.5x - 1.8x for better readability
- **Large Touch Targets**: Minimum 36px height for buttons
- **Keyboard-First Workflows**: All actions accessible via keyboard

### Keyboard Shortcuts

See `KEYBOARD_SHORTCUTS` dict or call `get_keyboard_shortcuts_help()`.

---

## Turtle Gallery

**Location:** `Platforms/Python/time_warp/graphics/turtle_gallery.py`

Save, share, and replay turtle drawings.

### Usage

```python
from Platforms.Python.time_warp.graphics.turtle_gallery import TurtleGallery

gallery = TurtleGallery()

# Start recording
gallery.start_recording()

# Record each turtle command
gallery.record_step("FD 50", turtle, color="blue")
gallery.record_step("RT 90", turtle, color="blue")

# Stop and save
gallery.stop_recording()
filepath = gallery.save_drawing(
    title="My Square",
    author="Student Name",
    language="Logo",
    code="REPEAT 4 [FD 50 RT 90]",
)

# Load and replay
metadata, steps = gallery.load_drawing(filepath)
for step, elapsed, total in gallery.replay_generator(steps, speed=1.0):
    # Update UI with step data
    print(f"{step.command} at ({step.x}, {step.y})")
```

### File Format

Drawings are saved as JSON in `~/.Time_Warp/gallery/`:

```json
{
  "metadata": {
    "title": "My Square",
    "author": "Student",
    "created_at": "2025-01-01T12:00:00",
    "language": "Logo",
    "code": "REPEAT 4 [FD 50 RT 90]",
    "duration_ms": 1250,
    "step_count": 8
  },
  "steps": [
    {
      "timestamp_ms": 0,
      "command": "FD 50",
      "x": 50.0,
      "y": 0.0,
      "heading": 0.0,
      "pen_down": true,
      "color": "black"
    }
  ]
}
```

### Export Options

- **SVG Export**: `export_as_svg()` creates vector graphics
- **Share Links**: `create_share_link()` (placeholder for web integration)

---

## Procedural Art Toolkit

**Location:** `Platforms/Python/time_warp/graphics/art_toolkit.py`

Generators for fractals, L-systems, and harmonic motion.

### L-Systems

```python
from Platforms.Python.time_warp.graphics.art_toolkit import (
    KOCH_CURVE,
    DRAGON_CURVE,
    PLANT,
)

# Generate L-system
lsystem_string = KOCH_CURVE.generate(iterations=4)

# Convert to turtle commands
commands = KOCH_CURVE.to_turtle_commands(lsystem_string, step_size=5)

# Execute in Logo
for cmd in commands:
    interpreter.execute_command(cmd)
```

### Fractals

```python
from Platforms.Python.time_warp.graphics.art_toolkit import FractalGenerator

# Koch snowflake
commands = FractalGenerator.koch_snowflake(order=3, size=100.0)

# Sierpinski carpet (returns list of square positions)
squares = FractalGenerator.sierpinski_carpet(order=3, size=243.0)

# Mandelbrot set (returns 2D array of iteration counts)
mandelbrot = FractalGenerator.mandelbrot_set(width=800, height=600, max_iter=100)
```

### Harmonic Motion

```python
from Platforms.Python.time_warp.graphics.art_toolkit import HarmonicMotion

# Lissajous curve
points = HarmonicMotion.lissajous(a=3, b=2, delta=math.pi/2, steps=1000)

# Rose curve
points = HarmonicMotion.rose_curve(n=7, d=2, steps=1000, scale=100.0)

# Archimedean spiral
points = HarmonicMotion.spiral(turns=5, growth=10.0, steps=500)
```

### Random Art (Seedable)

```python
from Platforms.Python.time_warp.graphics.art_toolkit import RandomArtGenerator

# Create generator with seed for reproducibility
art = RandomArtGenerator(seed=42)

# Random walk
commands = art.random_walk(steps=100, step_size=10.0, turn_range=45.0)

# Noise field (Perlin-like)
field = art.noise_field(width=100, height=100, scale=0.1, octaves=4)
```

### Preset Patterns

```python
from Platforms.Python.time_warp.graphics.art_toolkit import PRESET_PATTERNS

# Available presets:
# - koch_snowflake
# - dragon_curve
# - plant
# - lissajous_3_2
# - rose_7

pattern = PRESET_PATTERNS["dragon_curve"]()
```

---

## Pixel Canvas Mode

**Location:** `Platforms/Python/time_warp/graphics/pixel_canvas.py`

2D grid APIs for sprite drawing, tile maps, and animations.

### Basic Usage

```python
from Platforms.Python.time_warp.graphics.pixel_canvas import PixelCanvas

# Create canvas
canvas = PixelCanvas(width=128, height=96)

# Set pixels
canvas.set_pixel(10, 20, "#")
canvas.set_pixel(11, 20, "#")

# Draw shapes
canvas.draw_line(0, 0, 50, 50, "*")
canvas.draw_rect(10, 10, 20, 15, "#", filled=True)
canvas.draw_circle(64, 48, 20, "O", filled=False)

# Flood fill
canvas.flood_fill(5, 5, "~")

# Render as ASCII
ascii_art = canvas.render_ascii()
print(ascii_art)
```

### Sprites

```python
from Platforms.Python.time_warp.graphics.pixel_canvas import (
    Sprite,
    SPRITE_PLAYER,
    SPRITE_COIN,
)

# Define custom sprite
my_sprite = Sprite(
    width=8,
    height=8,
    pixels=[
        [".", ".", "#", "#", "#", "#", ".", "."],
        [".", "#", ".", ".", ".", ".", "#", "."],
        # ... 6 more rows
    ],
    hotspot_x=4,
    hotspot_y=4,
)

# Register and draw
canvas.define_sprite("player", my_sprite)
canvas.draw_sprite("player", x=50, y=40, transparent=".")
```

### Tile Maps

```python
from Platforms.Python.time_warp.graphics.pixel_canvas import TileMap

# Create tile map
tilemap = TileMap(width=16, height=12, tile_size=8)

# Define tiles
tilemap.define_tile("grass", grass_sprite)
tilemap.define_tile("wall", wall_sprite)

# Set tiles
tilemap.set_tile(0, 0, "grass")
tilemap.set_tile(1, 0, "wall")

# Render to canvas
tilemap.render_to_canvas(canvas, offset_x=0, offset_y=0)
```

### Animations

```python
from Platforms.Python.time_warp.graphics.pixel_canvas import create_animation_stepper

# Create animation
def update_frame(frame_num):
    x = frame_num * 2
    canvas.set_pixel(x, 50, "*")

frames = create_animation_stepper(canvas, update_frame, frames=50)

# Export
canvas.export_frames_as_animation("animation.json", delay_ms=100)
```

### Integration with Languages

To expose pixel canvas to BASIC/PILOT/Logo, add commands:

```python
# In language executor
if command.startswith("SETPIXEL"):
    # Parse: SETPIXEL x, y, color
    canvas.set_pixel(x, y, color)

if command.startswith("DRAWLINE"):
    # Parse: DRAWLINE x1, y1, x2, y2, color
    canvas.draw_line(x1, y1, x2, y2, color)
```

---

## Integration with Main IDE

### Recommended Main Window Setup

```python
from PySide6.QtWidgets import QMainWindow
from Platforms.Python.time_warp.ui.error_explorer import ErrorExplorerWidget
from Platforms.Python.time_warp.ui.focus_mode import FocusModeManager
from Platforms.Python.time_warp.ui.onboarding import OnboardingManager, OnboardingDialog
from Platforms.Python.time_warp.ui.accessibility import get_accessibility_theme

class TimeWarpMainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        
        # Error Explorer
        self.error_explorer = ErrorExplorerWidget(self)
        self.addDockWidget(Qt.RightDockWidgetArea, self.error_explorer)
        self.error_explorer.error_selected.connect(self.jump_to_line)
        
        # Focus Mode
        self.focus_manager = FocusModeManager(self)
        
        # Onboarding
        self.onboarding = OnboardingManager()
        if self.onboarding.should_show_onboarding():
            self.show_onboarding_dialog()
        
        # Setup keyboard shortcuts
        self.setup_shortcuts()
    
    def setup_shortcuts(self):
        # F11: Toggle focus mode
        QShortcut(QKeySequence("F11"), self, self.focus_manager.toggle_focus_mode)
        
        # F8/Shift+F8: Navigate errors
        QShortcut(QKeySequence("F8"), self, self.next_error)
        QShortcut(QKeySequence("Shift+F8"), self, self.previous_error)
```

---

## Testing

All components include docstrings and type hints. Test with:

```bash
# Unit tests
pytest Tests/test_error_explorer.py -v
pytest Tests/test_accessibility.py -v
pytest Tests/test_turtle_gallery.py -v
pytest Tests/test_art_toolkit.py -v
pytest Tests/test_pixel_canvas.py -v

# Integration tests
python -m Platforms.Python.time_warp.graphics.turtle_gallery --demo
python -m Platforms.Python.time_warp.graphics.art_toolkit --demo
```

---

## Future Enhancements

- **Error Explorer**: Syntax error previews, auto-fix suggestions
- **Focus Mode**: Zen mode timer, distraction-free writing stats
- **Onboarding**: Video tutorials, interactive challenges
- **Accessibility**: Screen reader support, voice commands
- **Turtle Gallery**: Web sharing platform, community gallery
- **Art Toolkit**: More fractals (Julia set, etc.), 3D fractals
- **Pixel Canvas**: Animation timeline editor, sprite sheet exporter
