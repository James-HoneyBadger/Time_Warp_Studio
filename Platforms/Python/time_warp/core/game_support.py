"""
Game support utilities for Time Warp IDE.
Provides INKEY$, timer, collision detection, and sound support.
"""

import threading
import time
from collections import deque
from dataclasses import dataclass, field
from typing import Deque, Dict, Optional, Tuple

# Try to import sound library (optional)
try:
    from PySide6.QtMultimedia import QSoundEffect

    SOUND_AVAILABLE = True
except ImportError:
    SOUND_AVAILABLE = False


@dataclass
class KeyBuffer:
    """
    Thread-safe keyboard buffer for INKEY$ support.

    Stores recent key presses in a FIFO queue, allowing
    BASIC programs to poll for keyboard input without blocking.
    """

    buffer: Deque[str] = field(default_factory=lambda: deque(maxlen=32))
    _lock: threading.Lock = field(default_factory=threading.Lock)

    def push(self, key: str):
        """Add a key to the buffer."""
        with self._lock:
            self.buffer.append(key)

    def pop(self) -> str:
        """Get and remove the oldest key, or empty string if none."""
        with self._lock:
            if self.buffer:
                return self.buffer.popleft()
            return ""

    def peek(self) -> str:
        """Look at the oldest key without removing it."""
        with self._lock:
            if self.buffer:
                return self.buffer[0]
            return ""

    def clear(self):
        """Clear all buffered keys."""
        with self._lock:
            self.buffer.clear()

    def is_empty(self) -> bool:
        """Check if buffer is empty."""
        with self._lock:
            return len(self.buffer) == 0


@dataclass
class Timer:
    """
    Timer for game loops and ON TIMER events.

    Provides both polling-based timing (TIMER variable)
    and event-based timing (ON TIMER GOSUB).
    """

    start_time: float = field(default_factory=time.time)
    intervals: Dict[int, Tuple[float, int, bool]] = field(default_factory=dict)
    # intervals[id] = (interval_seconds, target_line, enabled)

    def reset(self):
        """Reset the timer to current time."""
        self.start_time = time.time()

    def elapsed(self) -> float:
        """Get seconds elapsed since start/reset."""
        return time.time() - self.start_time

    def elapsed_ticks(self) -> int:
        """Get elapsed time in 1/18.2 second ticks (like DOS TIMER)."""
        return int(self.elapsed() * 18.2)

    def set_interval(
        self,
        timer_id: int,
        interval: float,
        target_line: int,
        enabled: bool = True,
    ):
        """Set up a repeating timer interval."""
        self.intervals[timer_id] = (interval, target_line, enabled)

    def clear_interval(self, timer_id: int):
        """Remove a timer interval."""
        if timer_id in self.intervals:
            del self.intervals[timer_id]

    def enable_interval(self, timer_id: int, enabled: bool):
        """Enable or disable a timer interval."""
        if timer_id in self.intervals:
            interval, target, _ = self.intervals[timer_id]
            self.intervals[timer_id] = (interval, target, enabled)


@dataclass
class Sprite:
    """Simple sprite for collision detection."""

    x: float
    y: float
    width: float
    height: float
    name: str = ""


class CollisionDetector:
    """
    Simple collision detection for game development.

    Supports:
    - Point-in-rectangle detection (POINT function)
    - Rectangle-rectangle collision (COLLISION function)
    - Turtle-sprite collision detection
    """

    def __init__(self):
        self.sprites: Dict[str, Sprite] = {}

    def register_sprite(
        self,
        name: str,
        x: float,
        y: float,
        width: float,
        height: float,
    ):
        """Register a sprite for collision detection."""
        self.sprites[name] = Sprite(x, y, width, height, name)

    def update_sprite(self, name: str, x: float, y: float):
        """Update sprite position."""
        if name in self.sprites:
            self.sprites[name].x = x
            self.sprites[name].y = y

    def remove_sprite(self, name: str):
        """Remove a sprite."""
        if name in self.sprites:
            del self.sprites[name]

    def point_in_rect(
        self,
        px: float,
        py: float,
        rx: float,
        ry: float,
        rw: float,
        rh: float,
    ) -> bool:
        """Check if point (px, py) is inside rectangle."""
        return rx <= px <= rx + rw and ry <= py <= ry + rh

    def rect_collision(
        self,
        x1: float,
        y1: float,
        w1: float,
        h1: float,
        x2: float,
        y2: float,
        w2: float,
        h2: float,
    ) -> bool:
        """Check if two rectangles overlap."""
        return x1 < x2 + w2 and x1 + w1 > x2 and y1 < y2 + h2 and y1 + h1 > y2

    def sprite_collision(self, name1: str, name2: str) -> bool:
        """Check collision between two named sprites."""
        if name1 not in self.sprites or name2 not in self.sprites:
            return False
        s1 = self.sprites[name1]
        s2 = self.sprites[name2]
        return self.rect_collision(
            s1.x, s1.y, s1.width, s1.height, s2.x, s2.y, s2.width, s2.height
        )

    def turtle_collision(
        self,
        turtle_x: float,
        turtle_y: float,
        sprite_name: str,
    ) -> bool:
        """Check if turtle position collides with a sprite."""
        if sprite_name not in self.sprites:
            return False
        sprite = self.sprites[sprite_name]
        # Turtle is treated as a point
        return self.point_in_rect(
            turtle_x, turtle_y, sprite.x, sprite.y, sprite.width, sprite.height
        )


class SoundPlayer:
    """
    Simple sound player for BEEP and SOUND commands.

    Generates basic tones using Qt multimedia or system beep.
    """

    def __init__(self):
        self.sound_enabled = SOUND_AVAILABLE
        self._sound_effect: Optional["QSoundEffect"] = None

    def beep(self):
        """Play a simple beep sound."""
        try:
            # Try system bell first
            print("\a", end="", flush=True)
        except (OSError, IOError):
            pass

    # pylint: disable=unused-argument
    def play_tone(self, frequency: int, duration_ms: int):
        """
        Play a tone at specified frequency for duration.

        Note: Full tone generation requires additional audio libraries.
        This is a placeholder that falls back to beep.
        """
        # For a full implementation, we would generate a sine wave
        # and play it using QAudioOutput or similar.
        # For now, just beep
        self.beep()


class GameState:
    """
    Central game state manager.

    Combines keyboard buffer, timer, collision detection,
    and sound into a unified game support system.
    """

    def __init__(self):
        self.key_buffer = KeyBuffer()
        self.timer = Timer()
        self.collision = CollisionDetector()
        self.sound = SoundPlayer()
        self.turtle_speed: int = 0  # 0 = fastest, 10 = slowest

    def reset(self):
        """Reset all game state."""
        self.key_buffer.clear()
        self.timer.reset()
        self.collision.sprites.clear()
        self.turtle_speed = 0

    def get_time_string(self) -> str:
        """Get current time as HH:MM:SS string."""
        t = time.localtime()
        return f"{t.tm_hour:02d}:{t.tm_min:02d}:{t.tm_sec:02d}"

    def get_date_string(self) -> str:
        """Get current date as MM-DD-YYYY string."""
        t = time.localtime()
        return f"{t.tm_mon:02d}-{t.tm_mday:02d}-{t.tm_year}"

    def get_timer_value(self) -> float:
        """Get timer value (seconds since midnight, like GW-BASIC)."""
        t = time.localtime()
        return t.tm_hour * 3600 + t.tm_min * 60 + t.tm_sec + time.time() % 1


# Global game state instance
_game_state: Optional[GameState] = None


def get_game_state() -> GameState:
    """Get or create the global game state instance."""
    global _game_state  # pylint: disable=global-statement
    if _game_state is None:
        _game_state = GameState()
    return _game_state
