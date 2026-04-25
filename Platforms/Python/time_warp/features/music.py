"""
Music support for Time Warp Studio.
Implements PLAY command with MML (Music Macro Language) notation.

Enhanced features (v10+):
- ADSR envelope shaping per note
- Pulse waveform with configurable duty cycle
- White-noise waveform
- 4-channel polyphonic synthesis (mix multiple MML streams)
- Extended MML: @W (waveform), @D (ADSR), @C (channel), chord notation [CEG]
"""

import math
import random
import struct
import wave
from dataclasses import dataclass, field
from io import BytesIO
from typing import Dict, List, Optional, Tuple

# Note frequencies (A4 = 440Hz standard tuning)
NOTE_MAP = {
    "C": 0,
    "D": 2,
    "E": 4,
    "F": 5,
    "G": 7,
    "A": 9,
    "B": 11,
}

DEFAULT_TEMPO = 120
DEFAULT_OCTAVE = 4
DEFAULT_LENGTH = 4   # Quarter note
DEFAULT_VOLUME = 10  # 0-15 scale
SAMPLE_RATE = 44100


@dataclass
class ADSREnvelope:
    """ADSR (Attack-Decay-Sustain-Release) amplitude envelope.

    All times are in milliseconds; sustain_level is 0.0–1.0.
    """

    attack_ms: float = 5.0
    decay_ms: float = 50.0
    sustain_level: float = 0.7
    release_ms: float = 30.0

    def apply(self, samples: List[float], sample_rate: int, note_duration_ms: float) -> List[float]:
        """Return a new list with the envelope applied."""
        n = len(samples)
        if n == 0:
            return samples

        atk = int(sample_rate * self.attack_ms / 1000)
        dec = int(sample_rate * self.decay_ms / 1000)
        rel = int(sample_rate * self.release_ms / 1000)
        # Release starts at the end of the note, capped at n
        rel_start = max(0, n - rel)

        result = []
        for i, s in enumerate(samples):
            if i < atk:
                env = i / atk if atk else 1.0
            elif i < atk + dec:
                t = (i - atk) / dec if dec else 1.0
                env = 1.0 - t * (1.0 - self.sustain_level)
            elif i < rel_start:
                env = self.sustain_level
            else:
                t = (i - rel_start) / rel if rel else 1.0
                env = self.sustain_level * max(0.0, 1.0 - t)
            result.append(s * env)
        return result


@dataclass
class MusicNote:
    """Represents a single musical note or rest."""

    frequency: float      # Hz, 0 for rest
    duration_ms: int      # Duration in milliseconds
    volume: float         # 0.0 to 1.0
    waveform: str = "square"   # 'square','sine','triangle','sawtooth','pulse','noise'
    duty: float = 0.5          # Pulse duty cycle (0.0–1.0)
    adsr: Optional[ADSREnvelope] = None


@dataclass
class MusicChannel:
    """One of up to 4 polyphonic synthesis channels."""

    channel_id: int
    notes: List[MusicNote] = field(default_factory=list)
    waveform: str = "square"
    duty: float = 0.5
    adsr: Optional[ADSREnvelope] = None


class MMLParser:
    """Parse Music Macro Language (MML) strings into note sequences.

    Standard MML commands:
      A-G   Play note (optionally followed by # or + for sharp, - for flat)
      R,P   Rest
      On    Set octave (1-8)
      < >   Decrease / increase octave
      Ln    Set default note length (1=whole, 4=quarter…)
      Tn    Set tempo (32-255 BPM)
      Vn    Set volume (0-15)
      Nn    Play note by MIDI number (0-127)
      .     Dotted note
      MS    Staccato  MN / ML  Normal / legato

    Extended MML commands (Time Warp v10+):
      @Wn   Select waveform: 0=square, 1=sine, 2=triangle, 3=sawtooth,
                             4=pulse, 5=noise
      @Dn,n,n,n  Set ADSR: attack_ms, decay_ms, sustain(0-15), release_ms
      @Pn   Set pulse duty cycle (0-99, represents %)
      @Cn   Select channel (0-3)
      [CEG] Chord — play notes C, E, G simultaneously (same duration)
    """

    WAVEFORMS = {
        "0": "square",
        "1": "sine",
        "2": "triangle",
        "3": "sawtooth",
        "4": "pulse",
        "5": "noise",
    }

    def __init__(self):
        self.octave = DEFAULT_OCTAVE
        self.length = DEFAULT_LENGTH
        self.tempo = DEFAULT_TEMPO
        self.volume = DEFAULT_VOLUME / 15.0
        self.staccato = False
        self.waveform = "square"
        self.duty = 0.5
        self.adsr: Optional[ADSREnvelope] = None
        self.channel = 0

    def reset(self):
        self.octave = DEFAULT_OCTAVE
        self.length = DEFAULT_LENGTH
        self.tempo = DEFAULT_TEMPO
        self.volume = DEFAULT_VOLUME / 15.0
        self.staccato = False
        self.waveform = "square"
        self.duty = 0.5
        self.adsr = None
        self.channel = 0

    def note_to_frequency(
        self, note: str, octave: int, sharp: bool = False, flat: bool = False
    ) -> float:
        if note not in NOTE_MAP:
            return 0.0
        semitone = NOTE_MAP[note]
        if sharp:
            semitone += 1
        if flat:
            semitone -= 1
        semitones_from_a4 = (octave - 4) * 12 + (semitone - 9)
        return 440.0 * (2 ** (semitones_from_a4 / 12.0))

    def duration_to_ms(self, note_length: int, dotted: bool = False) -> int:
        whole_note_ms = (4 * 60000) / self.tempo
        duration = whole_note_ms / note_length
        if dotted:
            duration *= 1.5
        if self.staccato:
            duration *= 0.75
        return int(duration)

    def _make_note(self, freq: float, duration_ms: int) -> MusicNote:
        return MusicNote(
            frequency=freq,
            duration_ms=duration_ms,
            volume=self.volume,
            waveform=self.waveform,
            duty=self.duty,
            adsr=self.adsr,
        )

    def _read_int(self, mml: str, i: int) -> Tuple[int, int]:
        """Read integer at position i; return (value, new_i)."""
        s = ""
        while i < len(mml) and mml[i].isdigit():
            s += mml[i]
            i += 1
        return (int(s) if s else 0, i)

    def parse(self, mml_string: str) -> List[MusicNote]:
        self.reset()
        notes: List[MusicNote] = []
        mml = mml_string.upper().replace(" ", "")
        i = 0

        while i < len(mml):
            ch = mml[i]

            # ---- Note A-G --------------------------------------------------
            if ch in "ABCDEFG":
                note_name = ch
                i += 1
                sharp = flat = False
                custom_length = None
                dotted = False

                if i < len(mml) and mml[i] in "#+-":
                    sharp = mml[i] in "#+"
                    flat = mml[i] == "-"
                    i += 1

                length_str = ""
                while i < len(mml) and mml[i].isdigit():
                    length_str += mml[i]
                    i += 1
                if length_str:
                    custom_length = int(length_str)

                if i < len(mml) and mml[i] == ".":
                    dotted = True
                    i += 1

                freq = self.note_to_frequency(note_name, self.octave, sharp, flat)
                length = custom_length if custom_length else self.length
                duration = self.duration_to_ms(length, dotted)
                notes.append(self._make_note(freq, duration))

            # ---- Chord [CEG] -----------------------------------------------
            elif ch == "[":
                i += 1
                chord_notes_raw = []
                while i < len(mml) and mml[i] != "]":
                    c = mml[i]
                    if c in "ABCDEFG":
                        n = c
                        i += 1
                        sharp = flat = False
                        if i < len(mml) and mml[i] in "#+-":
                            sharp = mml[i] in "#+"
                            flat = mml[i] == "-"
                            i += 1
                        chord_notes_raw.append((n, sharp, flat))
                    else:
                        i += 1
                if i < len(mml) and mml[i] == "]":
                    i += 1

                # Optional length after ']'
                custom_length = None
                dotted = False
                length_str = ""
                while i < len(mml) and mml[i].isdigit():
                    length_str += mml[i]
                    i += 1
                if length_str:
                    custom_length = int(length_str)
                if i < len(mml) and mml[i] == ".":
                    dotted = True
                    i += 1

                length = custom_length if custom_length else self.length
                duration = self.duration_to_ms(length, dotted)

                # Each chord note gets the same duration; they'll be mixed
                # downstream.  We tag the first note as chord_start and the
                # rest as chord_member via the 'waveform' field convention
                # (simulated by the player summing them).
                for idx, (nn, sh, fl) in enumerate(chord_notes_raw):
                    freq = self.note_to_frequency(nn, self.octave, sh, fl)
                    n = self._make_note(freq, duration)
                    # Mark extra chord members with a special waveform prefix
                    # the player can detect and overlap.
                    if idx > 0:
                        n.waveform = "chord:" + n.waveform
                    notes.append(n)

            # ---- Rest R/P --------------------------------------------------
            elif ch in "RP":
                i += 1
                custom_length = None
                dotted = False

                length_str = ""
                while i < len(mml) and mml[i].isdigit():
                    length_str += mml[i]
                    i += 1
                if length_str:
                    custom_length = int(length_str)

                if i < len(mml) and mml[i] == ".":
                    dotted = True
                    i += 1

                length = custom_length if custom_length else self.length
                duration = self.duration_to_ms(length, dotted)
                notes.append(MusicNote(0.0, duration, 0.0))

            # ---- Octave ---------------------------------------------------
            elif ch == "O":
                i += 1
                val, i = self._read_int(mml, i)
                self.octave = max(1, min(8, val))

            elif ch == "<":
                self.octave = max(1, self.octave - 1)
                i += 1

            elif ch == ">":
                self.octave = min(8, self.octave + 1)
                i += 1

            # ---- Length ---------------------------------------------------
            elif ch == "L":
                i += 1
                val, i = self._read_int(mml, i)
                self.length = max(1, min(64, val))

            # ---- Tempo ----------------------------------------------------
            elif ch == "T":
                i += 1
                val, i = self._read_int(mml, i)
                self.tempo = max(32, min(255, val))

            # ---- Volume ---------------------------------------------------
            elif ch == "V":
                i += 1
                val, i = self._read_int(mml, i)
                self.volume = max(0, min(15, val)) / 15.0

            # ---- Note by MIDI number --------------------------------------
            elif ch == "N":
                i += 1
                val, i = self._read_int(mml, i)
                freq = 440.0 * (2 ** ((val - 69) / 12.0))
                duration = self.duration_to_ms(self.length)
                notes.append(self._make_note(freq, duration))

            # ---- Style commands -------------------------------------------
            elif ch == "M":
                i += 1
                if i < len(mml):
                    style = mml[i]
                    if style == "S":
                        self.staccato = True
                    elif style in "NL":
                        self.staccato = False
                    i += 1

            # ---- Extended @commands (Time Warp v10) -----------------------
            elif ch == "@":
                i += 1
                if i >= len(mml):
                    continue
                cmd = mml[i]
                i += 1

                if cmd == "W":
                    # @Wn — select waveform
                    val, i = self._read_int(mml, i)
                    self.waveform = self.WAVEFORMS.get(str(val), "square")

                elif cmd == "D":
                    # @Datk,dcy,sus,rel — set ADSR (sus is 0-15)
                    atk, i = self._read_int(mml, i)
                    if i < len(mml) and mml[i] == ",":
                        i += 1
                    dcy, i = self._read_int(mml, i)
                    if i < len(mml) and mml[i] == ",":
                        i += 1
                    sus, i = self._read_int(mml, i)
                    if i < len(mml) and mml[i] == ",":
                        i += 1
                    rel, i = self._read_int(mml, i)
                    self.adsr = ADSREnvelope(
                        attack_ms=float(atk),
                        decay_ms=float(dcy),
                        sustain_level=max(0.0, min(15, sus)) / 15.0,
                        release_ms=float(rel),
                    )

                elif cmd == "P":
                    # @Pn — pulse duty cycle (0-99 → 0.0-0.99)
                    val, i = self._read_int(mml, i)
                    self.duty = max(0.01, min(0.99, val / 100.0))

                elif cmd == "C":
                    # @Cn — select channel (0-3)
                    val, i = self._read_int(mml, i)
                    self.channel = max(0, min(3, val))

            else:
                i += 1

        return notes


class MusicPlayer:
    """Generates and plays music from MusicNote sequences.

    Supports up to 4 polyphonic channels — parse each with a separate
    MMLParser and pass them to ``mix_channels()``; or call the convenience
    ``parse_and_generate()`` for single-channel use.
    """

    def __init__(self, sample_rate: int = SAMPLE_RATE):
        self.sample_rate = sample_rate
        self.parser = MMLParser()

    # ------------------------------------------------------------------
    # Waveform synthesis
    # ------------------------------------------------------------------

    def _generate_note_samples(self, note: MusicNote) -> List[float]:
        """Return raw float samples (±1.0) for a single note."""
        num_samples = int(self.sample_rate * note.duration_ms / 1000)
        if note.frequency <= 0 or num_samples == 0:
            return [0.0] * num_samples

        wf = note.waveform
        if wf.startswith("chord:"):
            wf = wf[6:]  # strip marker, use underlying waveform

        freq = note.frequency
        sr = self.sample_rate
        samples: List[float] = []
        duty = max(0.01, min(0.99, note.duty))

        for idx in range(num_samples):
            t = idx / sr
            phase = (freq * t) % 1.0  # normalised phase [0,1)

            if wf == "sine":
                v = math.sin(2 * math.pi * freq * t)
            elif wf == "square":
                v = 1.0 if phase < 0.5 else -1.0
            elif wf == "triangle":
                v = 4 * abs(phase - 0.5) - 1.0
            elif wf == "sawtooth":
                v = 2 * phase - 1.0
            elif wf == "pulse":
                v = 1.0 if phase < duty else -1.0
            elif wf == "noise":
                v = random.uniform(-1.0, 1.0)
            else:
                v = 1.0 if phase < 0.5 else -1.0  # default square

            samples.append(v)

        # Apply ADSR envelope
        if note.adsr:
            samples = note.adsr.apply(samples, self.sample_rate, float(note.duration_ms))

        return samples

    def generate_wave(
        self,
        notes: List[MusicNote],
        waveform: str = "square",
    ) -> bytes:
        """Generate WAV audio data from a note list.

        Args:
            notes:    List of MusicNote objects.
            waveform: Fallback waveform used when a note has no explicit
                      waveform set ('square', 'sine', 'triangle', 'sawtooth',
                      'pulse', 'noise').

        Returns:
            WAV file data as bytes (mono 16-bit 44100 Hz).
        """
        all_samples: List[float] = []
        # Chord accumulation: collect simultaneous chord members
        chord_buffer: List[List[float]] = []

        for note in notes:
            is_chord_member = note.waveform.startswith("chord:")
            # Use per-note waveform or global fallback
            if note.waveform in ("square", "sine", "triangle", "sawtooth", "pulse", "noise"):
                pass  # already set
            elif not is_chord_member:
                note.waveform = waveform

            raw = self._generate_note_samples(note)

            if is_chord_member:
                chord_buffer.append(raw)
            else:
                # If there was a pending chord, mix it into the previous note samples
                if chord_buffer:
                    for ch_samples in chord_buffer:
                        overlap = min(len(all_samples), len(ch_samples))
                        for k in range(overlap):
                            all_samples[-overlap + k] += ch_samples[k] * note.volume * 0.3
                    chord_buffer.clear()
                scaled = [s * note.volume * 0.3 for s in raw]
                all_samples.extend(scaled)

        # Flush any remaining chord buffer
        if chord_buffer:
            for ch_samples in chord_buffer:
                overlap = min(len(all_samples), len(ch_samples))
                for k in range(overlap):
                    all_samples[-overlap + k] += ch_samples[k] * 0.3

        # Convert to 16-bit integers
        int_samples = [max(-32768, min(32767, int(s * 32767))) for s in all_samples]

        buffer = BytesIO()
        # pylint: disable=no-member
        with wave.open(buffer, "wb") as wav:  # type: ignore[call-overload]
            wav.setnchannels(1)
            wav.setsampwidth(2)
            wav.setframerate(self.sample_rate)
            wav.writeframes(struct.pack(f"<{len(int_samples)}h", *int_samples))

        return buffer.getvalue()

    # ------------------------------------------------------------------
    # Multi-channel mixing
    # ------------------------------------------------------------------

    def mix_channels(self, channels: List[MusicChannel]) -> bytes:
        """Mix up to 4 channel note sequences into a single stereo WAV.

        Each channel is synthesised independently then summed.  The result
        is mono 16-bit 44100 Hz (same format as ``generate_wave``).

        Args:
            channels: List of up to 4 MusicChannel objects, each with their
                      own note sequence and waveform settings.

        Returns:
            WAV file data as bytes.
        """
        if not channels:
            return self.generate_wave([])

        # Generate samples per channel
        channel_samples: List[List[float]] = []
        max_len = 0

        for ch in channels[:4]:
            ch_buf: List[float] = []
            for note in ch.notes:
                if note.waveform == "square" and ch.waveform != "square":
                    note.waveform = ch.waveform
                    note.duty = ch.duty
                    note.adsr = note.adsr or ch.adsr
                raw = self._generate_note_samples(note)
                ch_buf.extend(r * note.volume * 0.25 for r in raw)
            channel_samples.append(ch_buf)
            max_len = max(max_len, len(ch_buf))

        # Mix (sum + clamp)
        mixed: List[float] = [0.0] * max_len
        for ch_buf in channel_samples:
            for k, v in enumerate(ch_buf):
                mixed[k] += v

        int_samples = [max(-32768, min(32767, int(s * 32767))) for s in mixed]

        buffer = BytesIO()
        with wave.open(buffer, "wb") as wav:  # type: ignore[call-overload]
            wav.setnchannels(1)
            wav.setsampwidth(2)
            wav.setframerate(self.sample_rate)
            wav.writeframes(struct.pack(f"<{len(int_samples)}h", *int_samples))

        return buffer.getvalue()

    def parse_and_generate(self, mml_string: str, waveform: str = "square") -> bytes:
        """Parse MML string and generate WAV data (single channel)."""
        notes = self.parser.parse(mml_string)
        return self.generate_wave(notes, waveform)

    def parse_multichannel(self, channel_mml: Dict[int, str]) -> bytes:
        """Parse multiple MML strings (keyed by channel 0-3) and mix.

        Example::

            wav = player.parse_multichannel({
                0: "@W0 T120 O4 L4 CDEFGAB",   # sine melody
                1: "@W1 T120 O3 L2 CG",          # square bass
            })
        """
        channels = []
        for ch_id, mml in channel_mml.items():
            parser = MMLParser()
            notes = parser.parse(mml)
            ch = MusicChannel(
                channel_id=ch_id,
                notes=notes,
                waveform=parser.waveform,
                duty=parser.duty,
                adsr=parser.adsr,
            )
            channels.append(ch)
        return self.mix_channels(channels)


class SoundEffectsLibrary:
    """Pre-built retro sound effects."""

    def __init__(self):
        self.player = MusicPlayer()

    def get_effect(self, name: str) -> Optional[bytes]:
        effects = {
            "LASER":    "T200 O6 L32 E D C B A G F E D C",
            "EXPLOSION":"T180 O2 L16 N30 N28 N26 N24 N22 N20 N18 N16 N14 N12",
            "POWERUP":  "T200 O4 L16 C E G >C E G >C",
            "COIN":     "T200 O6 L16 B >E",
            "JUMP":     "T200 O3 L32 C D E F G A B >C D E",
            "HURT":     "T150 O3 L8 E- D C",
            "GAMEOVER": "T80 O3 L4 E D C <B L2 <A",
            "LEVELUP":  "T160 O4 L16 C E G >C <G E C E G >C E G >C",
            "BEEP":     "T120 O5 L16 A",
            "ERROR":    "T100 O3 L8 A- A- R4 A-",
            "SUCCESS":  "T140 O4 L8 C E G >C",
            "MENU":     "T180 O5 L32 A R A",
            "BLIP":     "T200 O6 L64 C",
            "WARP":     "T200 O4 L32 C D E F G A B >C D E F G A B >C",
            "ALARM":    "T200 O5 L16 A R A R A R A R A R A R A R A R",
            "PICKUP":   "T200 O5 L32 E G >C",
            # Extended effects using new waveforms
            "DRONE":    "@W2 T60 O2 L1 C",      # triangle drone
            "BUZZ":     "@W4 @P30 T160 O5 L8 A A A A",  # pulse buzz
            "STATIC":   "@W5 T200 L32 N60 N60 N60 N60",  # noise burst
            "BEEP2":    "@W1 T140 O5 L8 [CE]",   # sine chord beep
        }
        mml = effects.get(name.upper())
        if mml:
            return self.player.parse_and_generate(mml, "square")
        return None

    def list_effects(self) -> List[str]:
        return [
            "LASER", "EXPLOSION", "POWERUP", "COIN", "JUMP", "HURT",
            "GAMEOVER", "LEVELUP", "BEEP", "ERROR", "SUCCESS", "MENU",
            "BLIP", "WARP", "ALARM", "PICKUP",
            "DRONE", "BUZZ", "STATIC", "BEEP2",
        ]


# Global singleton helpers
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

