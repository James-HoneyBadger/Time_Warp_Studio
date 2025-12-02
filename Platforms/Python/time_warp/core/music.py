"""
Music support for Time Warp IDE.
Implements PLAY command with MML (Music Macro Language) notation.
"""

import math
import struct
import wave
from dataclasses import dataclass
from io import BytesIO
from typing import List, Optional

# Note frequencies (A4 = 440Hz standard tuning)
# Format: note_name -> (octave_offset, semitone_from_C)
NOTE_MAP = {
    "C": 0,
    "D": 2,
    "E": 4,
    "F": 5,
    "G": 7,
    "A": 9,
    "B": 11,
}

# Standard tempo is 120 BPM, quarter note = 500ms
DEFAULT_TEMPO = 120
DEFAULT_OCTAVE = 4
DEFAULT_LENGTH = 4  # Quarter note
DEFAULT_VOLUME = 10  # 0-15 scale


@dataclass
class MusicNote:
    """Represents a single musical note or rest."""

    frequency: float  # Hz, 0 for rest
    duration_ms: int  # Duration in milliseconds
    volume: float  # 0.0 to 1.0


class MMLParser:
    """Parse Music Macro Language (MML) strings into note sequences."""

    def __init__(self):
        self.octave = DEFAULT_OCTAVE
        self.length = DEFAULT_LENGTH
        self.tempo = DEFAULT_TEMPO
        self.volume = DEFAULT_VOLUME / 15.0
        self.staccato = False

    def reset(self):
        """Reset parser state to defaults."""
        self.octave = DEFAULT_OCTAVE
        self.length = DEFAULT_LENGTH
        self.tempo = DEFAULT_TEMPO
        self.volume = DEFAULT_VOLUME / 15.0
        self.staccato = False

    def note_to_frequency(
        self, note: str, octave: int, sharp: bool = False, flat: bool = False
    ) -> float:
        """Convert note name to frequency in Hz."""
        if note not in NOTE_MAP:
            return 0.0

        semitone = NOTE_MAP[note]
        if sharp:
            semitone += 1
        if flat:
            semitone -= 1

        # Calculate semitones from A4 (440Hz)
        # A4 is in octave 4, at semitone 9 from C
        semitones_from_a4 = (octave - 4) * 12 + (semitone - 9)

        # Frequency = 440 * 2^(n/12)
        return 440.0 * (2 ** (semitones_from_a4 / 12.0))

    def duration_to_ms(self, note_length: int, dotted: bool = False) -> int:
        """Convert note length to milliseconds based on tempo."""
        # At tempo T, a whole note = (4 * 60000) / T ms
        whole_note_ms = (4 * 60000) / self.tempo
        duration = whole_note_ms / note_length

        if dotted:
            duration *= 1.5

        if self.staccato:
            duration *= 0.75

        return int(duration)

    def parse(self, mml_string: str) -> List[MusicNote]:
        """Parse MML string into list of MusicNote objects.

        MML Commands:
        - A-G: Play note (optionally followed by # or + for sharp, - for flat)
        - R or P: Rest
        - O n: Set octave (1-8)
        - < : Decrease octave
        - > : Increase octave
        - L n: Set default note length (1=whole, 2=half, 4=quarter, etc.)
        - T n: Set tempo (32-255 BPM)
        - V n: Set volume (0-15)
        - N n: Play note by MIDI number (0-127)
        - . : Dotted note (after note/length)
        - MS: Staccato (short notes)
        - MN: Normal length
        - ML: Legato (connected notes)
        """
        self.reset()
        notes: List[MusicNote] = []
        mml = mml_string.upper().replace(" ", "")
        i = 0

        while i < len(mml):
            ch = mml[i]

            # Note commands A-G
            if ch in "ABCDEFG":
                note_name = ch
                i += 1
                sharp = False
                flat = False
                custom_length = None
                dotted = False

                # Check for sharp/flat
                if i < len(mml) and mml[i] in "#+-":
                    if mml[i] in "#+":
                        sharp = True
                    else:
                        flat = True
                    i += 1

                # Check for length number
                length_str = ""
                while i < len(mml) and mml[i].isdigit():
                    length_str += mml[i]
                    i += 1
                if length_str:
                    custom_length = int(length_str)

                # Check for dotted
                if i < len(mml) and mml[i] == ".":
                    dotted = True
                    i += 1

                freq = self.note_to_frequency(note_name, self.octave, sharp, flat)
                length = custom_length if custom_length else self.length
                duration = self.duration_to_ms(length, dotted)

                notes.append(MusicNote(freq, duration, self.volume))

            # Rest
            elif ch in "RP":
                i += 1
                custom_length = None
                dotted = False

                # Check for length number
                length_str = ""
                while i < len(mml) and mml[i].isdigit():
                    length_str += mml[i]
                    i += 1
                if length_str:
                    custom_length = int(length_str)

                # Check for dotted
                if i < len(mml) and mml[i] == ".":
                    dotted = True
                    i += 1

                length = custom_length if custom_length else self.length
                duration = self.duration_to_ms(length, dotted)

                notes.append(MusicNote(0.0, duration, 0.0))  # Rest

            # Octave commands
            elif ch == "O":
                i += 1
                octave_str = ""
                while i < len(mml) and mml[i].isdigit():
                    octave_str += mml[i]
                    i += 1
                if octave_str:
                    self.octave = max(1, min(8, int(octave_str)))

            elif ch == "<":
                self.octave = max(1, self.octave - 1)
                i += 1

            elif ch == ">":
                self.octave = min(8, self.octave + 1)
                i += 1

            # Length command
            elif ch == "L":
                i += 1
                length_str = ""
                while i < len(mml) and mml[i].isdigit():
                    length_str += mml[i]
                    i += 1
                if length_str:
                    self.length = max(1, min(64, int(length_str)))

            # Tempo command
            elif ch == "T":
                i += 1
                tempo_str = ""
                while i < len(mml) and mml[i].isdigit():
                    tempo_str += mml[i]
                    i += 1
                if tempo_str:
                    self.tempo = max(32, min(255, int(tempo_str)))

            # Volume command
            elif ch == "V":
                i += 1
                vol_str = ""
                while i < len(mml) and mml[i].isdigit():
                    vol_str += mml[i]
                    i += 1
                if vol_str:
                    self.volume = max(0, min(15, int(vol_str))) / 15.0

            # Note by number
            elif ch == "N":
                i += 1
                note_num_str = ""
                while i < len(mml) and mml[i].isdigit():
                    note_num_str += mml[i]
                    i += 1
                if note_num_str:
                    note_num = int(note_num_str)
                    # MIDI note to frequency: f = 440 * 2^((n-69)/12)
                    freq = 440.0 * (2 ** ((note_num - 69) / 12.0))
                    duration = self.duration_to_ms(self.length)
                    notes.append(MusicNote(freq, duration, self.volume))

            # Music style commands
            elif ch == "M":
                i += 1
                if i < len(mml):
                    style = mml[i]
                    if style == "S":
                        self.staccato = True
                    elif style in "NL":
                        self.staccato = False
                    i += 1

            else:
                # Skip unknown character
                i += 1

        return notes


class MusicPlayer:
    """Generates and plays music from MusicNote sequences."""

    def __init__(self, sample_rate: int = 44100):
        self.sample_rate = sample_rate
        self.parser = MMLParser()

    def generate_wave(
        self,
        notes: List[MusicNote],
        waveform: str = "square",
    ) -> bytes:
        """Generate WAV audio data from notes.

        Args:
            notes: List of MusicNote objects
            waveform: 'sine', 'square', 'triangle', or 'sawtooth'

        Returns:
            WAV file data as bytes
        """
        samples: List[int] = []

        for note in notes:
            num_samples = int(self.sample_rate * note.duration_ms / 1000)

            if note.frequency <= 0:
                # Rest - silence
                samples.extend([0] * num_samples)
            else:
                # Generate waveform
                period = self.sample_rate / note.frequency

                for i in range(num_samples):
                    t = i / self.sample_rate
                    phase = (i % period) / period

                    if waveform == "sine":
                        value = math.sin(2 * math.pi * note.frequency * t)
                    elif waveform == "square":
                        value = 1.0 if phase < 0.5 else -1.0
                    elif waveform == "triangle":
                        value = 4 * abs(phase - 0.5) - 1
                    elif waveform == "sawtooth":
                        value = 2 * phase - 1
                    else:
                        value = math.sin(2 * math.pi * note.frequency * t)

                    # Apply volume and convert to 16-bit
                    sample = int(value * note.volume * 32767 * 0.3)  # 0.3 for headroom
                    samples.append(max(-32768, min(32767, sample)))

        # Create WAV file in memory
        buffer = BytesIO()
        # pylint: disable=no-member
        with wave.open(buffer, "wb") as wav:  # type: wave.Wave_write
            wav.setnchannels(1)
            wav.setsampwidth(2)
            wav.setframerate(self.sample_rate)
            wav.writeframes(struct.pack(f"<{len(samples)}h", *samples))

        return buffer.getvalue()

    def parse_and_generate(self, mml_string: str, waveform: str = "square") -> bytes:
        """Parse MML string and generate WAV data."""
        notes = self.parser.parse(mml_string)
        return self.generate_wave(notes, waveform)


class SoundEffectsLibrary:
    """Pre-built retro sound effects."""

    def __init__(self):
        self.player = MusicPlayer()

    def get_effect(self, name: str) -> Optional[bytes]:
        """Get a pre-built sound effect by name."""
        effects = {
            "LASER": "T200 O6 L32 E D C B A G F E D C",
            "EXPLOSION": "T180 O2 L16 N30 N28 N26 N24 N22 N20 N18 N16 N14 N12",
            "POWERUP": "T200 O4 L16 C E G >C E G >C",
            "COIN": "T200 O6 L16 B >E",
            "JUMP": "T200 O3 L32 C D E F G A B >C D E",
            "HURT": "T150 O3 L8 E- D C",
            "GAMEOVER": "T80 O3 L4 E D C <B L2 <A",
            "LEVELUP": "T160 O4 L16 C E G >C <G E C E G >C E G >C",
            "BEEP": "T120 O5 L16 A",
            "ERROR": "T100 O3 L8 A- A- R4 A-",
            "SUCCESS": "T140 O4 L8 C E G >C",
            "MENU": "T180 O5 L32 A R A",
            "BLIP": "T200 O6 L64 C",
            "WARP": "T200 O4 L32 C D E F G A B >C D E F G A B >C",
            "ALARM": "T200 O5 L16 A R A R A R A R A R A R A R A R",
            "PICKUP": "T200 O5 L32 E G >C",
        }

        mml = effects.get(name.upper())
        if mml:
            return self.player.parse_and_generate(mml, "square")
        return None

    def list_effects(self) -> List[str]:
        """List all available sound effects."""
        return [
            "LASER",
            "EXPLOSION",
            "POWERUP",
            "COIN",
            "JUMP",
            "HURT",
            "GAMEOVER",
            "LEVELUP",
            "BEEP",
            "ERROR",
            "SUCCESS",
            "MENU",
            "BLIP",
            "WARP",
            "ALARM",
            "PICKUP",
        ]


# Global instances
_music_player: Optional[MusicPlayer] = None
_sound_effects: Optional[SoundEffectsLibrary] = None


def get_music_player() -> MusicPlayer:
    """Get the global music player instance."""
    global _music_player  # pylint: disable=global-statement
    if _music_player is None:
        _music_player = MusicPlayer()
    return _music_player


def get_sound_effects() -> SoundEffectsLibrary:
    """Get the global sound effects library."""
    global _sound_effects  # pylint: disable=global-statement
    if _sound_effects is None:
        _sound_effects = SoundEffectsLibrary()
    return _sound_effects
