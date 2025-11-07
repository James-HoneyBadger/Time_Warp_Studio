#!/usr/bin/env python3
"""Generate placeholder icons and screenshots for packaging.

Requires: Pillow (pip install Pillow)
Basic usage:
    python3 scripts/generate_placeholders.py

CLI options:
- --icons-only / --screenshots-only to limit which assets are produced
- --root PATH to override the repo root (defaults to project root)
- --glyph TEXT to customize the icon glyph (default: "T")
- --count N to change the number of screenshots (default: 3)
"""

from pathlib import Path
import argparse

try:
    from PIL import Image, ImageDraw, ImageFont
except ImportError:
    print("Pillow not installed. Install with: pip install Pillow")
    raise


def _measure_text(
    draw: "ImageDraw.ImageDraw",
    text: str,
    font: "ImageFont.FreeTypeFont | ImageFont.ImageFont",
) -> tuple[int, int]:
    """Measure text width/height with a preferred API and safe fallbacks."""
    # Pillow provides multiple APIs over time; try the most robust ones first.
    # 1) font.getbbox (newer Pillow)
    try:
        x0, y0, x1, y1 = font.getbbox(text)  # type: ignore[attr-defined]
        return int(x1 - x0), int(y1 - y0)
    except AttributeError:
        pass
    # 2) draw.textbbox (newer Pillow)
    try:
        x0, y0, x1, y1 = draw.textbbox(
            (0, 0), text, font=font
        )  # type: ignore[attr-defined]
        return int(x1 - x0), int(y1 - y0)
    except AttributeError:
        pass
    # 3) font.getsize (older Pillow)
    try:
        return font.getsize(text)  # type: ignore[attr-defined]
    except AttributeError:
        # Final fallback: approximate using character count
        return max(8, len(text) * 8), 16


def generate_icons(root: Path, glyph: str = "T") -> None:
    """Generate icon PNGs inside packaging/macos/icon.iconset."""
    iconset_dir = root / "packaging" / "macos" / "icon.iconset"
    iconset_dir.mkdir(parents=True, exist_ok=True)

    # Icon sizes and names for icon.iconset
    sizes = [
        (16, "icon_16x16.png"),
        (32, "icon_16x16@2x.png"),
        (32, "icon_32x32.png"),
        (64, "icon_32x32@2x.png"),
        (128, "icon_128x128.png"),
        (256, "icon_128x128@2x.png"),
        (256, "icon_256x256.png"),
        (512, "icon_256x256@2x.png"),
        (512, "icon_512x512.png"),
        (1024, "icon_512x512@2x.png"),
    ]

    print(f"Generating icon PNGs in {iconset_dir} (glyph='{glyph}')")
    for size, name in sizes:
        img = Image.new("RGBA", (size, size), (30, 144, 255, 255))
        draw = ImageDraw.Draw(img)
        try:
            font = ImageFont.truetype(
                "/System/Library/Fonts/Helvetica.ttc", int(size * 0.6)
            )
        except OSError:
            font = ImageFont.load_default()

        w, h = _measure_text(draw, glyph, font)
        draw.text(
            ((size - w) / 2, (size - h) / 2),
            glyph,
            font=font,
            fill=(255, 255, 255, 255),
        )
        img.save(iconset_dir / name, format="PNG")


def generate_screenshots(
    root: Path, count: int = 3, title_prefix: str = "Time Warp — Screenshot"
) -> None:
    """Generate placeholder screenshots into fastlane metadata folder."""
    shots_dir = root / "fastlane" / "metadata" / "en-US" / "screenshots"
    shots_dir.mkdir(parents=True, exist_ok=True)

    print(f"Generating {count} placeholder screenshot(s) in {shots_dir}")
    for i in range(1, count + 1):
        width, height = (1280, 800)
        img = Image.new(
            "RGB",
            (width, height),
            (10 + i * 30, 20 + i * 20, 40 + i * 10),
        )
        draw = ImageDraw.Draw(img)
        title = f"{title_prefix} {i}"
        try:
            font = ImageFont.truetype(
                "/System/Library/Fonts/Helvetica.ttc",
                48,
            )
        except OSError:
            font = ImageFont.load_default()

        tw, th = _measure_text(draw, title, font)
        draw.text(
            ((width - tw) / 2, (height - th) / 2),
            title,
            font=font,
            fill=(255, 255, 255),
        )
        out_path = shots_dir / f"screenshot_{i}.png"
        img.save(out_path, format="PNG")


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Generate placeholder icons and screenshots. By default, both "
            "icons and screenshots are produced."
        )
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--icons-only",
        action="store_true",
        help="Generate only icon PNGs",
    )
    group.add_argument(
        "--screenshots-only",
        action="store_true",
        help="Generate only screenshots",
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=None,
        help=(
            "Override project root (defaults to repo root inferred from this "
            "script)."
        ),
    )
    parser.add_argument(
        "--glyph",
        type=str,
        default="T",
        help="Glyph letter to place on icons (default: 'T')",
    )
    parser.add_argument(
        "--count",
        type=int,
        default=3,
        help="Number of screenshots to generate (default: 3)",
    )
    parser.add_argument(
        "--title-prefix",
        type=str,
        default="Time Warp — Screenshot",
        help=(
            "Prefix text used for screenshots (default: 'Time Warp — " "Screenshot')"
        ),
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()
    root = args.root or Path(__file__).resolve().parents[1]
    # Ensure resource directories exist
    (root / "packaging" / "macos" / "resources").mkdir(
        parents=True,
        exist_ok=True,
    )

    do_icons = True
    do_screens = True
    if args.icons_only:
        do_screens = False
    elif args.screenshots_only:
        do_icons = False

    if do_icons:
        generate_icons(root, glyph=args.glyph)
    if do_screens:
        generate_screenshots(root, count=args.count, title_prefix=args.title_prefix)

    print(
        "Done. Run (optional): ./scripts/make_icns.sh packaging/macos/"
        "icon.iconset packaging/macos/resources/app.icns"
    )


if __name__ == "__main__":
    main()
