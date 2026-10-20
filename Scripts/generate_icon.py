#!/usr/bin/env python3
"""Draw the application icon and write it in the three formats the packages use.

The design is the one the project has always used -- a dark field, ten
concentric rings (one per language) and a single amber square in the middle --
rendered here without Pillow so that the icons can be regenerated on a machine
that has nothing but Python:

    packaging/linux/time-warp-studio.png     the desktop entry's icon
    packaging/windows/time-warp-studio.ico   16/24/32/48/64/128/256, PNG payloads
    packaging/macos/time-warp-studio.icns    16..512, PNG payloads

Every size is rendered from the geometry rather than downscaled from one
bitmap, which is what keeps the 16-pixel icon legible instead of a smudge.
"""

from __future__ import annotations

import struct
import zlib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

BACKGROUND = (20, 20, 40)
SQUARE = (255, 200, 50)
# Ten rings, one per language, the same spacing the 256-pixel original used.
RING_COUNT = 10
RING_INNER = 20
RING_OUTER = 236
RING_WIDTH = 2
SQUARE_BOX = (100, 100, 156, 156)
DESIGN_SIZE = 256


# Supersampling per axis. Enough that the 2-pixel ring survives at 16 px;
# large sizes need less of it and cost quadratically more.
def supersample(size: int) -> int:
    return 4 if size <= 128 else 2


def ring_colour(index: int) -> tuple[int, int, int]:
    return (100 + index * 15, 200, 255)


def render(size: int) -> bytes:
    """Return `size` x `size` RGBA pixels, anti-aliased by supersampling."""
    scale = size / DESIGN_SIZE
    samples = supersample(size)
    rings = []
    for index in range(RING_COUNT):
        inset = RING_INNER + index * 10
        outer_radius = (DESIGN_SIZE / 2 - inset) * scale
        thickness = RING_WIDTH * scale * (size / DESIGN_SIZE) ** 0.5
        # A ring thinner than a pixel would disappear at small sizes; widen it
        # just enough to keep a visible line rather than a faint one.
        thickness = max(thickness, 0.9)
        rings.append((outer_radius, thickness, ring_colour(index)))
    square = tuple(value * scale for value in SQUARE_BOX)

    pixels = bytearray(size * size * 4)
    inverse = 1.0 / (samples * samples)
    for y in range(size):
        for x in range(size):
            red = green = blue = 0
            for sub_y in range(samples):
                for sub_x in range(samples):
                    sx = x + (sub_x + 0.5) / samples
                    sy = y + (sub_y + 0.5) / samples
                    colour = sample(sx, sy, size, rings, square)
                    red += colour[0]
                    green += colour[1]
                    blue += colour[2]
            offset = (y * size + x) * 4
            pixels[offset] = int(red * inverse + 0.5)
            pixels[offset + 1] = int(green * inverse + 0.5)
            pixels[offset + 2] = int(blue * inverse + 0.5)
            pixels[offset + 3] = 255
    return bytes(pixels)


def sample(x: float, y: float, size: int, rings, square) -> tuple[int, int, int]:
    left, top, right, bottom = square
    if left <= x < right and top <= y < bottom:
        return SQUARE
    centre = size / 2
    dx = x - centre
    dy = y - centre
    distance = (dx * dx + dy * dy) ** 0.5
    for radius, thickness, colour in rings:
        if radius - thickness <= distance <= radius:
            return colour
    return BACKGROUND


def png(pixels: bytes, size: int) -> bytes:
    """Encode RGBA pixels as a PNG with no interlace and one filter byte a row."""
    stride = size * 4
    raw = bytearray()
    for y in range(size):
        raw.append(0)
        raw.extend(pixels[y * stride : (y + 1) * stride])

    def chunk(tag: bytes, payload: bytes) -> bytes:
        return (
            struct.pack(">I", len(payload))
            + tag
            + payload
            + struct.pack(">I", zlib.crc32(tag + payload) & 0xFFFFFFFF)
        )

    header = struct.pack(">IIBBBBB", size, size, 8, 6, 0, 0, 0)
    return (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", header)
        + chunk(b"IDAT", zlib.compress(bytes(raw), 9))
        + chunk(b"IEND", b"")
    )


def bmp_entry(pixels: bytes, size: int) -> bytes:
    """One 32-bit DIB icon image, the form every Windows icon loader reads.

    The small sizes are carried this way rather than as PNGs because the icon
    also goes to NSIS, which reads the installer's icon itself and does not
    decode PNG-compressed entries.
    """
    header = struct.pack(
        "<IiiHHIIiiII",
        40,
        size,
        size * 2,  # the DIB's height covers the colour image *and* the mask
        1,
        32,
        0,
        size * size * 4,
        0,
        0,
        0,
        0,
    )
    stride = size * 4
    body = bytearray()
    for y in range(size - 1, -1, -1):  # bottom-up
        row = pixels[y * stride : (y + 1) * stride]
        for x in range(size):
            red, green, blue, alpha = row[x * 4 : x * 4 + 4]
            body.extend((blue, green, red, alpha))
    # The AND mask: one bit a pixel, all zero because the alpha channel is what
    # decides transparency. Rows are padded to four bytes.
    mask_stride = ((size + 31) // 32) * 4
    return header + bytes(body) + b"\x00" * (mask_stride * size)


def write_ico(path: Path, pixels: dict[int, bytes], pngs: dict[int, bytes]) -> None:
    sizes = sorted(pixels)
    header = struct.pack("<HHH", 0, 1, len(sizes))
    offset = len(header) + 16 * len(sizes)
    entries = bytearray()
    payload = bytearray()
    for size in sizes:
        data = pngs[size] if size >= 128 else bmp_entry(pixels[size], size)
        entries.extend(
            struct.pack(
                "<BBBBHHII",
                size if size < 256 else 0,
                size if size < 256 else 0,
                0,
                0,
                1,
                32,
                len(data),
                offset,
            )
        )
        payload.extend(data)
        offset += len(data)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(header + bytes(entries) + bytes(payload))


def write_icns(path: Path, images: dict[int, bytes]) -> None:
    # One entry per size; every one is a PNG, which macOS has accepted since 10.7.
    types = {
        16: b"icp4",
        32: b"icp5",
        64: b"icp6",
        128: b"ic07",
        256: b"ic08",
        512: b"ic09",
    }
    body = bytearray()
    for size in sorted(images):
        tag = types[size]
        data = images[size]
        body.extend(tag + struct.pack(">I", len(data) + 8) + data)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(b"icns" + struct.pack(">I", len(body) + 8) + bytes(body))


def main() -> None:
    sizes = [16, 24, 32, 48, 64, 128, 256, 512]
    pixels = {size: render(size) for size in sizes}
    rendered = {size: png(pixels[size], size) for size in sizes}

    linux = ROOT / "packaging" / "linux" / "time-warp-studio.png"
    linux.parent.mkdir(parents=True, exist_ok=True)
    linux.write_bytes(rendered[256])
    print(f"wrote {linux.relative_to(ROOT)}")

    # The .ico keeps 24 for the Windows small-icon list and the rest for Explorer.
    # 512 is not an ICO size -- the format's directory entry cannot name it -- so
    # it goes to the .icns only.
    ico_sizes = (16, 24, 32, 48, 64, 128, 256)
    ico = ROOT / "packaging" / "windows" / "time-warp-studio.ico"
    write_ico(
        ico,
        {size: pixels[size] for size in ico_sizes},
        {size: rendered[size] for size in ico_sizes},
    )
    print(f"wrote {ico.relative_to(ROOT)}")

    # The .icns wants 512 as the Retina 256; 24/48/64 are ICO-only sizes.
    icns = ROOT / "packaging" / "macos" / "time-warp-studio.icns"
    write_icns(icns, {size: rendered[size] for size in (16, 32, 64, 128, 256, 512)})
    print(f"wrote {icns.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
