"""Accessibility features for inclusive education."""

from dataclasses import dataclass
from enum import Enum
from typing import Callable, Dict, List, Optional


class AccessibilityFeature(Enum):
    """Accessibility features."""

    SCREEN_READER = "screen_reader"
    HIGH_CONTRAST = "high_contrast"
    KEYBOARD_NAVIGATION = "keyboard_navigation"
    TEXT_TO_SPEECH = "text_to_speech"
    SPEECH_TO_TEXT = "speech_to_text"
    DYSLEXIA_FONT = "dyslexia_font"
    FOCUS_HIGHLIGHT = "focus_highlight"
    MAGNIFICATION = "magnification"
    COLOR_BLIND_MODE = "color_blind_mode"
    AUDIO_CUES = "audio_cues"


class ColorBlindType(Enum):
    """Types of color blindness."""

    PROTANOPIA = "protanopia"  # Red-blind
    DEUTERANOPIA = "deuteranopia"  # Green-blind
    TRITANOPIA = "tritanopia"  # Blue-yellow blind
    ACHROMATOPSIA = "achromatopsia"  # Complete color blindness


@dataclass
class AccessibilitySettings:
    """User accessibility preferences."""

    screen_reader_enabled: bool = False
    high_contrast_enabled: bool = False
    keyboard_only_mode: bool = False
    text_to_speech_enabled: bool = False
    speech_to_text_enabled: bool = False
    dyslexia_friendly_font: bool = False
    focus_highlight_enabled: bool = True
    magnification_level: float = 1.0  # 1.0 = normal
    color_blind_mode: Optional[ColorBlindType] = None
    audio_cues_enabled: bool = True
    font_size_multiplier: float = 1.0  # 1.0 = normal
    line_spacing_multiplier: float = 1.0  # 1.0 = normal


class AccessibilityManager:
    """Manage accessibility features."""

    def __init__(self):
        """Initialize accessibility manager."""
        self.settings = AccessibilitySettings()
        self.callbacks: Dict[str, List[Callable]] = {}
        self.enabled_features: set = set()

    def on_event(self, event_name: str, callback: Callable):
        """Register event callback."""
        if event_name not in self.callbacks:
            self.callbacks[event_name] = []
        self.callbacks[event_name].append(callback)

    def _trigger_callbacks(self, event_name: str, **kwargs):
        """Trigger callbacks."""
        if event_name in self.callbacks:
            for callback in self.callbacks[event_name]:
                callback(**kwargs)

    def enable_feature(self, feature: AccessibilityFeature):
        """Enable accessibility feature."""
        self.enabled_features.add(feature)

        if feature == AccessibilityFeature.SCREEN_READER:
            self.settings.screen_reader_enabled = True
        elif feature == AccessibilityFeature.HIGH_CONTRAST:
            self.settings.high_contrast_enabled = True
        elif feature == AccessibilityFeature.KEYBOARD_NAVIGATION:
            self.settings.keyboard_only_mode = True
        elif feature == AccessibilityFeature.TEXT_TO_SPEECH:
            self.settings.text_to_speech_enabled = True
        elif feature == AccessibilityFeature.SPEECH_TO_TEXT:
            self.settings.speech_to_text_enabled = True
        elif feature == AccessibilityFeature.DYSLEXIA_FONT:
            self.settings.dyslexia_friendly_font = True
        elif feature == AccessibilityFeature.AUDIO_CUES:
            self.settings.audio_cues_enabled = True

        self._trigger_callbacks("feature_enabled", feature=feature)

    def disable_feature(self, feature: AccessibilityFeature):
        """Disable accessibility feature."""
        self.enabled_features.discard(feature)

        if feature == AccessibilityFeature.SCREEN_READER:
            self.settings.screen_reader_enabled = False
        elif feature == AccessibilityFeature.HIGH_CONTRAST:
            self.settings.high_contrast_enabled = False
        elif feature == AccessibilityFeature.KEYBOARD_NAVIGATION:
            self.settings.keyboard_only_mode = False
        elif feature == AccessibilityFeature.TEXT_TO_SPEECH:
            self.settings.text_to_speech_enabled = False
        elif feature == AccessibilityFeature.SPEECH_TO_TEXT:
            self.settings.speech_to_text_enabled = False
        elif feature == AccessibilityFeature.DYSLEXIA_FONT:
            self.settings.dyslexia_friendly_font = False
        elif feature == AccessibilityFeature.AUDIO_CUES:
            self.settings.audio_cues_enabled = False

        self._trigger_callbacks("feature_disabled", feature=feature)

    def set_color_blind_mode(self, mode: Optional[ColorBlindType]):
        """Set color blind mode."""
        self.settings.color_blind_mode = mode
        if mode:
            self.enabled_features.add(AccessibilityFeature.COLOR_BLIND_MODE)
        else:
            self.enabled_features.discard(
                AccessibilityFeature.COLOR_BLIND_MODE
            )

        self._trigger_callbacks("color_blind_mode_changed", mode=mode)

    def set_magnification(self, level: float):
        """Set magnification level (1.0 = normal)."""
        self.settings.magnification_level = max(1.0, min(3.0, level))
        if level != 1.0:
            self.enabled_features.add(AccessibilityFeature.MAGNIFICATION)
        else:
            self.enabled_features.discard(AccessibilityFeature.MAGNIFICATION)

        self._trigger_callbacks(
            "magnification_changed", level=self.settings.magnification_level
        )

    def set_font_size(self, multiplier: float):
        """Set font size multiplier (1.0 = normal)."""
        self.settings.font_size_multiplier = max(0.5, min(2.0, multiplier))
        self._trigger_callbacks(
            "font_size_changed", multiplier=self.settings.font_size_multiplier
        )

    def set_line_spacing(self, multiplier: float):
        """Set line spacing multiplier (1.0 = normal)."""
        self.settings.line_spacing_multiplier = max(0.8, min(2.0, multiplier))
        self._trigger_callbacks(
            "line_spacing_changed",
            multiplier=self.settings.line_spacing_multiplier,
        )

    def get_enabled_features(self) -> List[str]:
        """Get list of enabled features."""
        return [f.value for f in self.enabled_features]

    def is_feature_enabled(self, feature: AccessibilityFeature) -> bool:
        """Check if feature is enabled."""
        return feature in self.enabled_features


class ScreenReaderSupport:
    """Screen reader support and ARIA labels."""

    @staticmethod
    def generate_aria_label(
        element_type: str, content: str, state: str = ""
    ) -> str:
        """Generate ARIA label for UI element."""
        labels = {
            "button": f"Button: {content}",
            "input": f"Text input: {content}",
            "checkbox": f"Checkbox {state}: {content}",
            "radio": f"Radio button {state}: {content}",
            "dropdown": f"Dropdown: {content}",
            "textfield": f"Text field: {content}",
            "canvas": f"Graphics canvas: {content}",
            "output": f"Output: {content}",
            "code": f"Code: {content}",
            "error": f"Error: {content}",
            "warning": f"Warning: {content}",
            "success": f"Success: {content}",
        }
        return labels.get(element_type, content)

    @staticmethod
    def describe_syntax_error(line_number: int, message: str) -> str:
        """Create accessible description of syntax error."""
        return f"Syntax error on line {line_number}: {message}"

    @staticmethod
    def describe_program_output(output: str) -> str:
        """Create accessible description of program output."""
        lines = output.strip().split("\n")
        if len(lines) == 1:
            return f"Program output: {output}"
        else:
            return f"Program output with {len(lines)} lines of text"

    @staticmethod
    def describe_turtle_graphics(turtle_state: Dict) -> str:
        """Create accessible description of turtle state."""
        return (
            f"Turtle at position {
                turtle_state.get(
                    'x',
                    0)}, {
                turtle_state.get(
                    'y',
                    0)} "
            f"facing {
                        turtle_state.get(
                            'angle',
                            0)} degrees, "
            f"pen is {
                                'down' if turtle_state.get(
                                    'pen_down',
                                    True) else 'up'}"
        )


class TextToSpeechEngine:
    """Text-to-speech for program output and UI."""

    def __init__(self):
        """Initialize TTS engine."""
        self.enabled = False
        self.rate = 1.0  # 0.5-2.0
        self.pitch = 1.0  # 0.5-2.0
        self.callbacks: Dict[str, List[Callable]] = {}

    def on_event(self, event_name: str, callback: Callable):
        """Register event callback."""
        if event_name not in self.callbacks:
            self.callbacks[event_name] = []
        self.callbacks[event_name].append(callback)

    def _trigger_callbacks(self, event_name: str, **kwargs):
        """Trigger callbacks."""
        if event_name in self.callbacks:
            for callback in self.callbacks[event_name]:
                callback(**kwargs)

    def speak(self, text: str, priority: str = "normal"):
        """Speak text."""
        if not self.enabled:
            return

        try:
            import pyttsx3

            engine = pyttsx3.init()
            engine.setProperty("rate", 150 * self.rate)
            engine.setProperty("pitch", self.pitch)
            engine.say(text)
            engine.runAndWait()

            self._trigger_callbacks("speech_started", text=text)

        except ImportError:
            # Fallback: silently fail if pyttsx3 not installed
            pass
        except Exception as e:
            self._trigger_callbacks("speech_error", error=str(e))

    def speak_error(self, error_message: str):
        """Speak error message with emphasis."""
        self.speak(f"Error: {error_message}", priority="high")

    def speak_output(self, output: str):
        """Speak program output."""
        # Limit length to avoid long speeches
        if len(output) > 500:
            output = output[:500] + "..."
        self.speak(f"Output: {output}")

    def set_rate(self, rate: float):
        """Set speech rate (0.5-2.0)."""
        self.rate = max(0.5, min(2.0, rate))

    def set_pitch(self, pitch: float):
        """Set speech pitch (0.5-2.0)."""
        self.pitch = max(0.5, min(2.0, pitch))


class SpeechToTextEngine:
    """Speech-to-text for hands-free input."""

    def __init__(self):
        """Initialize STT engine."""
        self.enabled = False
        self.language = "en-US"
        self.callbacks: Dict[str, List[Callable]] = {}

    def on_event(self, event_name: str, callback: Callable):
        """Register event callback."""
        if event_name not in self.callbacks:
            self.callbacks[event_name] = []
        self.callbacks[event_name].append(callback)

    def _trigger_callbacks(self, event_name: str, **kwargs):
        """Trigger callbacks."""
        if event_name in self.callbacks:
            for callback in self.callbacks[event_name]:
                callback(**kwargs)

    def listen(self) -> Optional[str]:
        """Listen for speech input."""
        if not self.enabled:
            return None

        try:
            import speech_recognition as sr

            recognizer = sr.Recognizer()

            with sr.Microphone() as source:
                audio = recognizer.listen(source, timeout=5)

            text = recognizer.recognize_google(audio, language=self.language)
            self._trigger_callbacks("text_recognized", text=text)
            return text

        except ImportError:
            self._trigger_callbacks(
                "error", message="Speech recognition not installed"
            )
            return None
        except Exception as e:
            self._trigger_callbacks("error", message=str(e))
            return None


class AccessibilityTheme:
    """Accessible color themes."""

    # High contrast themes
    HIGH_CONTRAST = {
        "background": "#000000",
        "foreground": "#FFFFFF",
        "selection": "#FFFF00",
        "text": "#FFFFFF",
        "error": "#FF0000",
        "success": "#00FF00",
        "warning": "#FFFF00",
    }

    # Color blind friendly (Protanopia - Red-blind)
    PROTANOPIA = {
        "background": "#FFFFFF",
        "foreground": "#000000",
        "selection": "#0173B2",
        "error": "#CC78BC",
        "success": "#029E73",
        "warning": "#DE8F05",
    }

    # Color blind friendly (Deuteranopia - Green-blind)
    DEUTERANOPIA = {
        "background": "#FFFFFF",
        "foreground": "#000000",
        "selection": "#0173B2",
        "error": "#D55E00",
        "success": "#0173B2",
        "warning": "#CC78BC",
    }

    # Color blind friendly (Tritanopia - Blue-yellow blind)
    TRITANOPIA = {
        "background": "#FFFFFF",
        "foreground": "#000000",
        "selection": "#005F73",
        "error": "#CA6702",
        "success": "#005F73",
        "warning": "#EE9B00",
    }

    @staticmethod
    def get_theme(color_blind_type: Optional[ColorBlindType]) -> Dict:
        """Get accessible color theme."""
        themes = {
            ColorBlindType.PROTANOPIA: AccessibilityTheme.PROTANOPIA,
            ColorBlindType.DEUTERANOPIA: AccessibilityTheme.DEUTERANOPIA,
            ColorBlindType.TRITANOPIA: AccessibilityTheme.TRITANOPIA,
        }
        return themes.get(color_blind_type, AccessibilityTheme.HIGH_CONTRAST)


class AccessibleKeyboardLayout:
    """Keyboard shortcuts for accessibility."""

    # Navigation
    NAVIGATE_UP = "Up Arrow"
    NAVIGATE_DOWN = "Down Arrow"
    NAVIGATE_LEFT = "Left Arrow"
    NAVIGATE_RIGHT = "Right Arrow"

    # Editor
    RUN_PROGRAM = "Ctrl+Enter"
    STOP_PROGRAM = "Ctrl+Shift+Enter"
    CLEAR_OUTPUT = "Ctrl+L"

    # Accessibility
    TOGGLE_SCREEN_READER = "Ctrl+Alt+S"
    TOGGLE_HIGH_CONTRAST = "Ctrl+Alt+C"
    INCREASE_FONT = "Ctrl+="
    DECREASE_FONT = "Ctrl+-"
    RESET_FONT = "Ctrl+0"

    # Focus
    FOCUS_EDITOR = "Alt+E"
    FOCUS_OUTPUT = "Alt+O"
    FOCUS_HELP = "Alt+H"
    NEXT_ELEMENT = "Tab"
    PREV_ELEMENT = "Shift+Tab"

    @staticmethod
    def get_all_shortcuts() -> Dict[str, str]:
        """Get all keyboard shortcuts."""
        return {
            "navigate_up": AccessibleKeyboardLayout.NAVIGATE_UP,
            "navigate_down": AccessibleKeyboardLayout.NAVIGATE_DOWN,
            "navigate_left": AccessibleKeyboardLayout.NAVIGATE_LEFT,
            "navigate_right": AccessibleKeyboardLayout.NAVIGATE_RIGHT,
            "run_program": AccessibleKeyboardLayout.RUN_PROGRAM,
            "stop_program": AccessibleKeyboardLayout.STOP_PROGRAM,
            "clear_output": AccessibleKeyboardLayout.CLEAR_OUTPUT,
            "toggle_screen_reader": AccessibleKeyboardLayout.TOGGLE_SCREEN_READER,
            "toggle_high_contrast": AccessibleKeyboardLayout.TOGGLE_HIGH_CONTRAST,
            "increase_font": AccessibleKeyboardLayout.INCREASE_FONT,
            "decrease_font": AccessibleKeyboardLayout.DECREASE_FONT,
            "reset_font": AccessibleKeyboardLayout.RESET_FONT,
            "focus_editor": AccessibleKeyboardLayout.FOCUS_EDITOR,
            "focus_output": AccessibleKeyboardLayout.FOCUS_OUTPUT,
            "focus_help": AccessibleKeyboardLayout.FOCUS_HELP,
            "next_element": AccessibleKeyboardLayout.NEXT_ELEMENT,
            "prev_element": AccessibleKeyboardLayout.PREV_ELEMENT,
        }
