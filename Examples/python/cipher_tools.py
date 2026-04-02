#!/usr/bin/env python3
"""
╔══════════════════════════════════════════════════════╗
║        CRYPTOGRAPHY TOOLKIT - Cipher Library         ║
║  Classical and modern text ciphers. Genuinely        ║
║  useful for encoding, education, puzzle solving.     ║
║  Includes: Caesar, Vigenere, Playfair, Rail Fence,   ║
║  Atbash, ROT13, Base64, Morse, frequency analysis.   ║
╚══════════════════════════════════════════════════════╝
"""
import string
import base64
from collections import Counter

# ─── UTILITIES ───────────────────────────────────────────


def clean(text: str) -> str:
    """Remove non-alpha characters and uppercase."""
    return "".join(c.upper() for c in text if c.isalpha())


def frequency_analysis(text: str) -> dict[str, float]:
    """Return letter frequency percentages."""
    t = clean(text)
    n = len(t)
    if n == 0:
        return {}
    freq = Counter(t)
    return {ch: round(count / n * 100, 2) for ch, count in
            sorted(freq.items(), key=lambda x: -x[1])}


# ─── CAESAR CIPHER ──────────────────────────────────────

def caesar_encode(text: str, shift: int) -> str:
    result = []
    for ch in text:
        if ch.isalpha():
            base = ord('A') if ch.isupper() else ord('a')
            result.append(chr((ord(ch) - base + shift) % 26 + base))
        else:
            result.append(ch)
    return "".join(result)


def caesar_decode(text: str, shift: int) -> str:
    return caesar_encode(text, -shift)


def caesar_brute_force(ciphertext: str) -> list[tuple[int, str]]:
    """Try all 26 shifts."""
    return [(i, caesar_decode(ciphertext, i)) for i in range(26)]


# ─── ROT13 ───────────────────────────────────────────────

def rot13(text: str) -> str:
    return caesar_encode(text, 13)


# ─── ATBASH CIPHER ──────────────────────────────────────

def atbash(text: str) -> str:
    result = []
    for ch in text:
        if ch.isalpha():
            base = ord('A') if ch.isupper() else ord('a')
            result.append(chr(base + 25 - (ord(ch) - base)))
        else:
            result.append(ch)
    return "".join(result)


# ─── VIGENÈRE CIPHER ────────────────────────────────────

def vigenere_encode(text: str, key: str) -> str:
    key = clean(key)
    result = []
    ki = 0
    for ch in text:
        if ch.isalpha():
            shift = ord(key[ki % len(key)]) - ord('A')
            base = ord('A') if ch.isupper() else ord('a')
            result.append(chr((ord(ch) - base + shift) % 26 + base))
            ki += 1
        else:
            result.append(ch)
    return "".join(result)


def vigenere_decode(text: str, key: str) -> str:
    key = clean(key)
    result = []
    ki = 0
    for ch in text:
        if ch.isalpha():
            shift = ord(key[ki % len(key)]) - ord('A')
            base = ord('A') if ch.isupper() else ord('a')
            result.append(chr((ord(ch) - base - shift) % 26 + base))
            ki += 1
        else:
            result.append(ch)
    return "".join(result)


# ─── RAIL FENCE CIPHER ──────────────────────────────────

def rail_fence_encode(text: str, rails: int) -> str:
    fence = [[] for _ in range(rails)]
    rail, direction = 0, 1
    for ch in text:
        fence[rail].append(ch)
        if rail == 0:
            direction = 1
        elif rail == rails - 1:
            direction = -1
        rail += direction
    return "".join("".join(r) for r in fence)


def rail_fence_decode(text: str, rails: int) -> str:
    n = len(text)
    indices = []
    rail, direction = 0, 1
    for i in range(n):
        indices.append(rail)
        if rail == 0:
            direction = 1
        elif rail == rails - 1:
            direction = -1
        rail += direction
    order = sorted(range(n), key=lambda i: indices[i])
    result = [''] * n
    for pos, char in zip(order, text):
        result[pos] = char
    return "".join(result)


# ─── MORSE CODE ─────────────────────────────────────────

MORSE = {
    'A': '.-', 'B': '-...', 'C': '-.-.', 'D': '-..',
    'E': '.', 'F': '..-.', 'G': '--.', 'H': '....',
    'I': '..', 'J': '.---', 'K': '-.-', 'L': '.-..',
    'M': '--', 'N': '-.', 'O': '---', 'P': '.--.',
    'Q': '--.-', 'R': '.-.', 'S': '...', 'T': '-',
    'U': '..-', 'V': '...-', 'W': '.--', 'X': '-..-',
    'Y': '-.--', 'Z': '--..',
    '0': '-----', '1': '.----', '2': '..---', '3': '...--',
    '4': '....-', '5': '.....', '6': '-....', '7': '--...',
    '8': '---..', '9': '----.',
    ' ': '/'
}
MORSE_REV = {v: k for k, v in MORSE.items()}


def morse_encode(text: str) -> str:
    return " ".join(MORSE.get(c.upper(), '?') for c in text)


def morse_decode(morse: str) -> str:
    result = []
    for code in morse.split(" "):
        if code == '/':
            result.append(' ')
        else:
            result.append(MORSE_REV.get(code, '?'))
    return "".join(result)


# ─── BASE64 ──────────────────────────────────────────────

def b64_encode(text: str) -> str:
    return base64.b64encode(text.encode()).decode()


def b64_decode(text: str) -> str:
    return base64.b64decode(text.encode()).decode()


# ─── SUBSTITUTION CIPHER ────────────────────────────────

class SubstitutionCipher:
    """Simple monoalphabetic substitution with a keyword."""

    def __init__(self, keyword: str):
        seen = set()
        key_chars = []
        for c in (keyword + string.ascii_uppercase):
            c = c.upper()
            if c not in seen and c.isalpha():
                seen.add(c)
                key_chars.append(c)
        self.alphabet = string.ascii_uppercase
        self.key = "".join(key_chars)
        self.encode_map = dict(zip(self.alphabet, self.key))
        self.decode_map = dict(zip(self.key, self.alphabet))

    def encode(self, text: str) -> str:
        return "".join(self.encode_map.get(c.upper(), c) for c in text)

    def decode(self, text: str) -> str:
        return "".join(self.decode_map.get(c.upper(), c) for c in text)


# ─── DEMO ────────────────────────────────────────────────

def print_section(title: str):
    print(f"\n{'═' * 55}")
    print(f"  {title}")
    print(f"{'─' * 55}")


def demo():
    original = "Hello World! This is a secret message."
    print("╔══════════════════════════════════════════════╗")
    print("║         PYTHON CRYPTOGRAPHY TOOLKIT          ║")
    print("╚══════════════════════════════════════════════╝")
    print(f"\nOriginal: {original}")

    print_section("CAESAR CIPHER")
    enc = caesar_encode(original, 13)
    dec = caesar_decode(enc, 13)
    print(f"  Encoded (shift=13):  {enc}")
    print(f"  Decoded:             {dec}")
    print(f"  ROT13 (self-inverse): {rot13(enc)}")

    print_section("BRUTE FORCE CAESAR")
    bf = caesar_brute_force("Khoor Zruog")
    for shift, text in bf[:5]:
        print(f"  Shift {shift:2d}: {text}")
    print("  ...")

    print_section("ATBASH CIPHER (Mirror alphabet)")
    enc = atbash(original)
    dec = atbash(enc)
    print(f"  Encoded: {enc}")
    print(f"  Decoded: {dec}")

    print_section("VIGENERE CIPHER")
    key = "PYTHON"
    enc = vigenere_encode(original, key)
    dec = vigenere_decode(enc, key)
    print(f"  Key:     {key}")
    print(f"  Encoded: {enc}")
    print(f"  Decoded: {dec}")

    print_section("RAIL FENCE CIPHER")
    msg = "WEAREDISCOVEREDRUNATONCE"
    enc = rail_fence_encode(msg, 3)
    dec = rail_fence_decode(enc, 3)
    print(f"  Original: {msg}")
    print(f"  Encoded (3 rails): {enc}")
    print(f"  Decoded:           {dec}")

    print_section("MORSE CODE")
    msg = "SOS HELLO"
    enc = morse_encode(msg)
    dec = morse_decode(enc)
    print(f"  Text:    {msg}")
    print(f"  Morse:   {enc}")
    print(f"  Decoded: {dec}")

    print_section("BASE64 ENCODING")
    b64 = b64_encode(original)
    decoded = b64_decode(b64)
    print(f"  B64: {b64}")
    print(f"  Decoded: {decoded}")

    print_section("SUBSTITUTION CIPHER")
    sc = SubstitutionCipher("PYTHON")
    enc = sc.encode("Hello World")
    dec = sc.decode(enc)
    print("  Key: PYTHON")
    print(f"  Cipher alphabet: {sc.key}")
    print(f"  Encoded: {enc}")
    print(f"  Decoded: {dec}")

    print_section("FREQUENCY ANALYSIS")
    sample = ("ETAOIN SHRDLU - the frequency order of letters in English. "
              "The letter E appears most often, then T, then A...")
    freq = frequency_analysis(sample)
    print("  Letter frequencies:")
    for letter, pct in list(freq.items())[:8]:
        bar = "█" * int(pct)
        print(f"    {letter}: {pct:5.2f}%  {bar}")

    print("\n✓ Cryptography toolkit demo complete!")


if __name__ == "__main__":
    demo()
