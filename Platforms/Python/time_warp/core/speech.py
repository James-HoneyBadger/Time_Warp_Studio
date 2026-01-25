"""
Speech synthesis support for Time Warp Studio.
Provides SAY command for text-to-speech.
"""

from typing import Optional


class SpeechSynthesizer:
    """Text-to-speech synthesizer using available backends."""

    def __init__(self):
        self._engine = None
        self._backend = None
        self._rate = 150  # Words per minute
        self._volume = 1.0  # 0.0 to 1.0
        self._voice = None
        self._initialize()

    def _initialize(self):
        """Initialize the speech engine with the best available backend."""
        # Try pyttsx3 first (cross-platform)
        try:
            import pyttsx3  # pylint: disable=import-outside-toplevel

            self._engine = pyttsx3.init()
            self._backend = "pyttsx3"
            self._engine.setProperty("rate", self._rate)
            self._engine.setProperty("volume", self._volume)
            return
        except (ImportError, RuntimeError):
            pass

        # Try Qt speech (if PySide6 is available)
        try:
            # pylint: disable=import-outside-toplevel
            from PySide6.QtTextToSpeech import QTextToSpeech

            self._engine = QTextToSpeech()
            self._backend = "qt"
            return
        except (ImportError, RuntimeError):
            pass

        # Try espeak on Linux
        try:
            import subprocess  # pylint: disable=import-outside-toplevel

            result = subprocess.run(
                ["which", "espeak"],
                capture_output=True,
                text=True,
                check=False,
            )
            if result.returncode == 0:
                self._backend = "espeak"
                return
        except (FileNotFoundError, OSError):
            pass

        # Try macOS say command
        try:
            import platform  # pylint: disable=import-outside-toplevel
            import subprocess  # pylint: disable=import-outside-toplevel

            if platform.system() == "Darwin":
                result = subprocess.run(
                    ["which", "say"],
                    capture_output=True,
                    text=True,
                    check=False,
                )
                if result.returncode == 0:
                    self._backend = "macos"
                    return
        except (FileNotFoundError, OSError):
            pass

        # No backend available
        self._backend = None

    @property
    def available(self) -> bool:
        """Check if speech synthesis is available."""
        return self._backend is not None

    @property
    def backend_name(self) -> str:
        """Get the name of the active backend."""
        return self._backend or "none"

    def set_rate(self, rate: int):
        """Set speech rate (words per minute).

        Args:
            rate: Speech rate, typically 100-300
        """
        self._rate = max(50, min(400, rate))

        if self._backend == "pyttsx3" and self._engine:
            self._engine.setProperty("rate", self._rate)

    def set_volume(self, volume: float):
        """Set speech volume.

        Args:
            volume: Volume level 0.0 to 1.0
        """
        self._volume = max(0.0, min(1.0, volume))

        if self._backend == "pyttsx3" and self._engine:
            self._engine.setProperty("volume", self._volume)
        elif self._backend == "qt" and self._engine:
            self._engine.setVolume(self._volume)

    def set_voice(self, voice_id: str):
        """Set the voice to use.

        Args:
            voice_id: Voice identifier (backend-specific)
        """
        self._voice = voice_id

        if self._backend == "pyttsx3" and self._engine:
            voices = self._engine.getProperty("voices")
            for voice in voices:
                if voice_id.lower() in voice.id.lower():
                    self._engine.setProperty("voice", voice.id)
                    break

    def get_voices(self) -> list:
        """Get list of available voices."""
        if self._backend == "pyttsx3" and self._engine:
            voices = self._engine.getProperty("voices")
            return [{"id": v.id, "name": v.name} for v in voices]
        elif self._backend == "qt" and self._engine:
            return [
                {"id": str(i), "name": v.name()}
                for i, v in enumerate(self._engine.availableVoices())
            ]
        return []

    def say(self, text: str, block: bool = False) -> str:
        """Speak the given text.

        Args:
            text: Text to speak
            block: If True, wait for speech to complete

        Returns:
            Status message
        """
        if not self.available:
            return "❌ Speech synthesis not available\n"

        if not text.strip():
            return ""

        try:
            if self._backend == "pyttsx3":
                self._engine.say(text)
                if block:
                    self._engine.runAndWait()
                else:
                    # Run in separate thread for non-blocking
                    import threading  # pylint: disable=import-outside-toplevel

                    thread = threading.Thread(target=self._engine.runAndWait)
                    thread.daemon = True
                    thread.start()

            elif self._backend == "qt":
                self._engine.say(text)

            elif self._backend == "espeak":
                import subprocess  # pylint: disable=import-outside-toplevel

                cmd = ["espeak", "-s", str(self._rate)]
                if self._voice:
                    cmd.extend(["-v", self._voice])
                cmd.append(text)

                if block:
                    subprocess.run(cmd, check=False)
                else:
                    subprocess.Popen(  # pylint: disable=consider-using-with
                        cmd,
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                    )

            elif self._backend == "macos":
                import subprocess  # pylint: disable=import-outside-toplevel

                cmd = ["say", "-r", str(self._rate)]
                if self._voice:
                    cmd.extend(["-v", self._voice])
                cmd.append(text)

                if block:
                    subprocess.run(cmd, check=False)
                else:
                    subprocess.Popen(  # pylint: disable=consider-using-with
                        cmd,
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL,
                    )

            return f"🗣️ {text[:50]}{'...' if len(text) > 50 else ''}\n"

        except Exception as e:  # pylint: disable=broad-except
            return f"❌ Speech error: {e}\n"

    def stop(self):
        """Stop any ongoing speech."""
        if self._backend == "pyttsx3" and self._engine:
            self._engine.stop()
        elif self._backend == "qt" and self._engine:
            self._engine.stop()
        # espeak and macos don't have easy stop mechanisms


# Global instance
_synthesizer: Optional[SpeechSynthesizer] = None


def get_synthesizer() -> SpeechSynthesizer:
    """Get the global speech synthesizer instance."""
    global _synthesizer  # pylint: disable=global-statement
    if _synthesizer is None:
        _synthesizer = SpeechSynthesizer()
    return _synthesizer
