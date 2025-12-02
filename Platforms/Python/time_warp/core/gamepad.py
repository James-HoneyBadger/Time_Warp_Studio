"""
Gamepad input abstraction layer with multi-backend support.

Provides unified access to game controllers via pygame or inputs library,
with automatic backend selection and graceful degradation when neither
is available. Exposes controller state through the BASIC JOYBUTTON and
JOYAXIS functions.

Optional dependencies:
    - pygame: Preferred backend, best cross-platform support
    - inputs: Fallback for Linux systems without pygame
"""

import threading
import time
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Optional


@dataclass
class GamepadState:
    """
    Snapshot of all inputs from a single gamepad.

    Button fields are True when pressed. Axis fields range from -1.0 to 1.0
    for sticks and 0.0 to 1.0 for triggers. The generic buttons/axes dicts
    store raw values for non-standard controllers.
    """

    buttons: Dict[str, bool] = field(default_factory=dict)
    axes: Dict[str, float] = field(default_factory=dict)

    dpad_up: bool = False
    dpad_down: bool = False
    dpad_left: bool = False
    dpad_right: bool = False

    button_a: bool = False
    button_b: bool = False
    button_x: bool = False
    button_y: bool = False
    button_start: bool = False
    button_select: bool = False
    button_lb: bool = False
    button_rb: bool = False

    left_x: float = 0.0
    left_y: float = 0.0
    right_x: float = 0.0
    right_y: float = 0.0

    left_trigger: float = 0.0
    right_trigger: float = 0.0

    connected: bool = False
    name: str = ""


class GamepadManager:
    """
    Threaded gamepad polling manager with callback support.

    On initialization, attempts to load pygame, then inputs library.
    Call start() to begin background polling at ~60Hz. Register callbacks
    via add_callback() to receive state updates, or poll directly with
    get_button()/get_axis() for BASIC integration.
    """

    def __init__(self):
        self.gamepads: Dict[int, GamepadState] = {}
        self._running = False
        self._thread: Optional[threading.Thread] = None
        self._backend: Optional[str] = None
        self._pygame_joysticks: Dict[int, object] = {}
        self._callbacks: List[Callable[[int, GamepadState], None]] = []
        self._initialize_backend()

    def _initialize_backend(self):
        """Detect and initialize the best available input library."""
        try:
            # pylint: disable=import-outside-toplevel
            import pygame

            pygame.init()
            pygame.joystick.init()
            self._backend = "pygame"
            return
        except (ImportError, RuntimeError):
            pass

        try:
            # pylint: disable=import-outside-toplevel
            import inputs  # noqa: F401

            _ = inputs.get_gamepad
            self._backend = "inputs"
            return
        except ImportError:
            pass

        self._backend = None

    @property
    def available(self) -> bool:
        """True if a gamepad backend was successfully loaded."""
        return self._backend is not None

    @property
    def backend_name(self) -> str:
        """Name of active backend: 'pygame', 'inputs', or 'none'."""
        return self._backend or "none"

    def start(self):
        """Begin background polling thread. No-op if already running."""
        if self._running or not self.available:
            return

        self._running = True
        self._thread = threading.Thread(target=self._poll_loop, daemon=True)
        self._thread.start()

    def stop(self):
        """Stop polling thread and wait for clean shutdown."""
        self._running = False
        if self._thread:
            self._thread.join(timeout=1.0)
            self._thread = None

    def _poll_loop(self):
        """Background loop dispatching to backend-specific poll method."""
        while self._running:
            try:
                if self._backend == "pygame":
                    self._poll_pygame()
                elif self._backend == "inputs":
                    self._poll_inputs()
            except Exception:  # pylint: disable=broad-except
                pass

            time.sleep(0.016)

    def _poll_pygame(self):
        """Read all connected joysticks via pygame."""
        # pylint: disable=import-outside-toplevel,import-error
        import pygame

        pygame.event.pump()

        num_joysticks = pygame.joystick.get_count()

        for i in range(num_joysticks):
            if i not in self._pygame_joysticks:
                joy = pygame.joystick.Joystick(i)
                joy.init()
                self._pygame_joysticks[i] = joy
                self.gamepads[i] = GamepadState(connected=True, name=joy.get_name())

            joy = self._pygame_joysticks[i]
            state = self.gamepads[i]

            num_axes = joy.get_numaxes()
            if num_axes > 0:
                state.left_x = joy.get_axis(0)
            if num_axes > 1:
                state.left_y = joy.get_axis(1)
            if num_axes > 2:
                state.right_x = joy.get_axis(2)
            if num_axes > 3:
                state.right_y = joy.get_axis(3)
            if num_axes > 4:
                state.left_trigger = (joy.get_axis(4) + 1) / 2
            if num_axes > 5:
                state.right_trigger = (joy.get_axis(5) + 1) / 2

            num_buttons = joy.get_numbuttons()
            if num_buttons > 0:
                state.button_a = joy.get_button(0)
            if num_buttons > 1:
                state.button_b = joy.get_button(1)
            if num_buttons > 2:
                state.button_x = joy.get_button(2)
            if num_buttons > 3:
                state.button_y = joy.get_button(3)
            if num_buttons > 4:
                state.button_lb = joy.get_button(4)
            if num_buttons > 5:
                state.button_rb = joy.get_button(5)
            if num_buttons > 6:
                state.button_select = joy.get_button(6)
            if num_buttons > 7:
                state.button_start = joy.get_button(7)

            num_hats = joy.get_numhats()
            if num_hats > 0:
                hat = joy.get_hat(0)
                state.dpad_left = hat[0] < 0
                state.dpad_right = hat[0] > 0
                state.dpad_down = hat[1] < 0
                state.dpad_up = hat[1] > 0

            for callback in self._callbacks:
                callback(i, state)

    def _poll_inputs(self):
        """Read gamepad events via inputs library (Linux evdev)."""
        # pylint: disable=import-outside-toplevel,import-error
        import inputs

        try:
            events = inputs.get_gamepad()
            for event in events:
                if 0 not in self.gamepads:
                    self.gamepads[0] = GamepadState(connected=True, name="Gamepad")

                state = self.gamepads[0]

                if event.ev_type == "Absolute":
                    if event.code == "ABS_X":
                        state.left_x = event.state / 32768.0
                    elif event.code == "ABS_Y":
                        state.left_y = event.state / 32768.0
                    elif event.code == "ABS_RX":
                        state.right_x = event.state / 32768.0
                    elif event.code == "ABS_RY":
                        state.right_y = event.state / 32768.0
                    elif event.code == "ABS_Z":
                        state.left_trigger = event.state / 255.0
                    elif event.code == "ABS_RZ":
                        state.right_trigger = event.state / 255.0
                    elif event.code == "ABS_HAT0X":
                        state.dpad_left = event.state < 0
                        state.dpad_right = event.state > 0
                    elif event.code == "ABS_HAT0Y":
                        state.dpad_up = event.state < 0
                        state.dpad_down = event.state > 0

                elif event.ev_type == "Key":
                    pressed = event.state == 1
                    if event.code == "BTN_SOUTH":
                        state.button_a = pressed
                    elif event.code == "BTN_EAST":
                        state.button_b = pressed
                    elif event.code == "BTN_WEST":
                        state.button_x = pressed
                    elif event.code == "BTN_NORTH":
                        state.button_y = pressed
                    elif event.code == "BTN_TL":
                        state.button_lb = pressed
                    elif event.code == "BTN_TR":
                        state.button_rb = pressed
                    elif event.code == "BTN_SELECT":
                        state.button_select = pressed
                    elif event.code == "BTN_START":
                        state.button_start = pressed

                for callback in self._callbacks:
                    callback(0, state)

        except inputs.UnpluggedError:
            if 0 in self.gamepads:
                self.gamepads[0].connected = False

    def get_state(self, gamepad_id: int = 0) -> Optional[GamepadState]:
        """Return full state snapshot for a gamepad, or None if not connected."""
        return self.gamepads.get(gamepad_id)

    def add_callback(self, callback: Callable[[int, GamepadState], None]):
        """Register a function to call on each poll with (gamepad_id, state)."""
        self._callbacks.append(callback)

    def remove_callback(self, callback: Callable[[int, GamepadState], None]):
        """Unregister a previously added callback."""
        if callback in self._callbacks:
            self._callbacks.remove(callback)

    def get_connected_count(self) -> int:
        """Number of currently connected gamepads."""
        return sum(1 for g in self.gamepads.values() if g.connected)

    def get_button(self, button_name: str, gamepad_id: int = 0) -> bool:
        """
        Query a button by name for BASIC JOYBUTTON function.

        Valid names: A, B, X, Y, LB, RB, START, SELECT,
                     DPAD_UP, DPAD_DOWN, DPAD_LEFT, DPAD_RIGHT
        """
        state = self.get_state(gamepad_id)
        if not state:
            return False

        button_map = {
            "A": state.button_a,
            "B": state.button_b,
            "X": state.button_x,
            "Y": state.button_y,
            "LB": state.button_lb,
            "RB": state.button_rb,
            "START": state.button_start,
            "SELECT": state.button_select,
            "DPAD_UP": state.dpad_up,
            "DPAD_DOWN": state.dpad_down,
            "DPAD_LEFT": state.dpad_left,
            "DPAD_RIGHT": state.dpad_right,
        }

        return button_map.get(button_name.upper(), False)

    def get_axis(self, axis_name: str, gamepad_id: int = 0) -> float:
        """
        Query an axis by name for BASIC JOYAXIS function.

        Valid names: LEFT_X, LEFT_Y, RIGHT_X, RIGHT_Y,
                     LEFT_TRIGGER, RIGHT_TRIGGER
        Returns 0.0 if gamepad not connected.
        """
        state = self.get_state(gamepad_id)
        if not state:
            return 0.0

        axis_map = {
            "LEFT_X": state.left_x,
            "LEFT_Y": state.left_y,
            "RIGHT_X": state.right_x,
            "RIGHT_Y": state.right_y,
            "LEFT_TRIGGER": state.left_trigger,
            "RIGHT_TRIGGER": state.right_trigger,
        }

        return axis_map.get(axis_name.upper(), 0.0)


_gamepad_manager: Optional[GamepadManager] = None


def get_gamepad_manager() -> GamepadManager:
    """Return the singleton GamepadManager instance, creating it if needed."""
    global _gamepad_manager  # pylint: disable=global-statement
    if _gamepad_manager is None:
        _gamepad_manager = GamepadManager()
    return _gamepad_manager
