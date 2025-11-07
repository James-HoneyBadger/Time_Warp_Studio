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
from typing import Iterable, Optional, Tuple

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


def _parse_color(
    value: str, *, with_alpha: bool = False
) -> Tuple[int, int, int] | Tuple[int, int, int, int]:
    """Parse '#RRGGBB' or 'R,G,B[,A]' into a color tuple."""
    v = value.strip()
    if v.startswith("#"):
        hx = v[1:]
        if len(hx) == 6:
            r, g, b = int(hx[0:2], 16), int(hx[2:4], 16), int(hx[4:6], 16)
            return (r, g, b, 255) if with_alpha else (r, g, b)
        if len(hx) == 8:
            r, g, b = int(hx[0:2], 16), int(hx[2:4], 16), int(hx[4:6], 16)
            a = int(hx[6:8], 16)
            return (r, g, b, a) if with_alpha else (r, g, b)
        raise ValueError(f"Invalid hex color: {value}")
    parts = [p.strip() for p in v.split(",") if p.strip()]
    nums = [int(p) for p in parts]
    if with_alpha:
        if len(nums) == 3:
            return nums[0], nums[1], nums[2], 255
        if len(nums) == 4:
            return nums[0], nums[1], nums[2], nums[3]
    else:
        if len(nums) == 3:
            return nums[0], nums[1], nums[2]
    raise ValueError("Color must be '#RRGGBB' or 'R,G,B' or 'R,G,B,A'")


def _parse_sizes_csv(csv: Optional[str]) -> list[int]:
    if not csv:
        return [16, 32, 128, 256, 512]
    out: list[int] = []
    for tok in csv.split(","):
        tok = tok.strip()
        if tok:
            out.append(int(tok))
    return out


def _load_font(pt_size: int, font_path: Optional[Path] = None):
    try:
        if font_path:
            return ImageFont.truetype(str(font_path), pt_size)
    except OSError:
        pass
    for candidate in (
        "/System/Library/Fonts/Helvetica.ttc",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    ):
        try:
            return ImageFont.truetype(candidate, pt_size)
        except OSError:
            continue
    return ImageFont.load_default()


def generate_icons(
    root: Path,
    glyph_text: str = "T",
    *,
    sizes: Optional[Iterable[int]] = None,
    bg_rgba: Tuple[int, int, int, int] = (30, 144, 255, 255),
    fg_rgba: Tuple[int, int, int, int] = (255, 255, 255, 255),
    out_dir: Optional[Path] = None,
    font_path: Optional[Path] = None,
    verbose: bool = False,
    dry_run: bool = False,
) -> None:
    """Generate icon PNGs inside packaging/macos/icon.iconset."""
    iconset_dir = (
        (out_dir / "icon.iconset")
        if out_dir
        else root / "packaging" / "macos" / "icon.iconset"
    )
    if not dry_run:
        iconset_dir.mkdir(parents=True, exist_ok=True)

    base_sizes = list(sizes) if sizes is not None else [16, 32, 128, 256, 512]
    entries: list[tuple[int, str]] = []
    for s in base_sizes:
        entries.append((s, f"icon_{s}x{s}.png"))
        entries.append((s * 2, f"icon_{s}x{s}@2x.png"))

    glyph = (glyph_text or "T")[0]
    if verbose:
        print(
            "Generating icon PNGs in "
            f"{iconset_dir} (glyph='{glyph}', sizes={base_sizes})"
        )
    else:
        print(f"Generating icon PNGs in {iconset_dir}")
    for size, name in entries:
        if dry_run:
            print(f"[dry-run] icon {name} {size}x{size}")
            continue
        img = Image.new("RGBA", (size, size), bg_rgba)
        draw = ImageDraw.Draw(img)
        font = _load_font(int(size * 0.6), font_path)

        w, h = _measure_text(draw, glyph, font)
        draw.text(
            ((size - w) / 2, (size - h) / 2),
            glyph,
            font=font,
            fill=fg_rgba,
        )
        img.save(iconset_dir / name, format="PNG")


def generate_screenshots(
    root: Path,
    count: int = 3,
    title_prefix: str = "Time Warp — Screenshot",
    *,
    out_dir: Optional[Path] = None,
    font_path: Optional[Path] = None,
    size: Tuple[int, int] = (1280, 800),
    bg_rgb: Tuple[int, int, int] = (40, 60, 100),
    fg_rgb: Tuple[int, int, int] = (255, 255, 255),
    verbose: bool = False,
    dry_run: bool = False,
) -> None:
    """Generate placeholder screenshots into fastlane metadata folder."""
    shots_dir = (
        (out_dir / "screenshots")
        if out_dir
        else root / "fastlane" / "metadata" / "en-US" / "screenshots"
    )
    if not dry_run:
        shots_dir.mkdir(parents=True, exist_ok=True)
    if verbose:
        print(
            f"Generating {count} placeholder screenshot(s) in "
            f"{shots_dir} size={size}"
        )
    else:
        print(f"Generating placeholder screenshots in {shots_dir}")
    for i in range(1, count + 1):
        width, height = size
        if dry_run:
            print(f"[dry-run] screenshot_{i}.png {width}x{height}")
            continue
        img = Image.new(
            "RGB",
            (width, height),
            (
                min(255, bg_rgb[0] + i * 10),
                min(255, bg_rgb[1] + i * 10),
                min(255, bg_rgb[2] + i * 10),
            ),
        )
        draw = ImageDraw.Draw(img)
        title = f"{title_prefix} {i}"
        font = _load_font(48, font_path)

        tw, th = _measure_text(draw, title, font)
        draw.text(
            ((width - tw) / 2, (height - th) / 2),
            title,
            font=font,
            fill=fg_rgb,
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
        "--out-dir",
        type=Path,
        default=None,
        help=(
            "Override output base directory. Icons -> OUT/icon.iconset, "
            "screens -> OUT/screenshots"
        ),
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
        "--text",
        type=str,
        default=None,
        help=(
            "Common text: used as icon glyph (first character) and as "
            "screenshot title prefix."
        ),
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
    parser.add_argument(
        "--font",
        type=Path,
        default=None,
        help="Path to a .ttf/.ttc font file to use",
    )
    parser.add_argument(
        "--bg-color",
        type=str,
        default="#1e90ff",
        help="Background color: '#RRGGBB' or 'R,G,B[,A]'",
    )
    parser.add_argument(
        "--fg-color",
        type=str,
        default="#ffffff",
        help="Foreground/text color: '#RRGGBB' or 'R,G,B[,A]'",
    )
    parser.add_argument(
        "--sizes",
        type=str,
        default=None,
        help="Comma-separated icon base sizes (e.g. '16,32,128,256,512')",
    )
    parser.add_argument(
        "--screenshot-size",
        type=str,
        default="1280x800",
        help="Screenshot size as WxH (default: 1280x800)",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable verbose logging",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Preview outputs without creating directories or files",
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

    # Compute configuration
    text = args.text
    glyph_text = text or "T"
    title_prefix = text or args.title_prefix
    # Colors
    icon_bg = _parse_color(args.bg_color, with_alpha=True)
    icon_fg_t = _parse_color(args.fg_color, with_alpha=True)
    icon_fg = (
        icon_fg_t
        if len(icon_fg_t) == 4
        else (icon_fg_t[0], icon_fg_t[1], icon_fg_t[2], 255)
    )
    ss_bg = _parse_color(args.bg_color)
    ss_fg_t = _parse_color(args.fg_color)
    ss_fg = (ss_fg_t[0], ss_fg_t[1], ss_fg_t[2])
    # Sizes
    base_sizes = _parse_sizes_csv(args.sizes)
    w_str, h_str = str(args.screenshot_size).lower().split("x", 1)
    ss_size = (int(w_str), int(h_str))

    if do_icons:
        generate_icons(
            root,
            glyph_text=glyph_text,
            sizes=base_sizes,
            bg_rgba=icon_bg,  # type: ignore[arg-type]
            fg_rgba=icon_fg,  # type: ignore[arg-type]
            out_dir=args.out_dir,
            font_path=args.font,
            verbose=args.verbose,
            dry_run=args.dry_run,
        )
    if do_screens:
        generate_screenshots(
            root,
            count=args.count,
            title_prefix=title_prefix,
            out_dir=args.out_dir,
            font_path=args.font,
            size=ss_size,
            bg_rgb=ss_bg,  # type: ignore[arg-type]
            fg_rgb=ss_fg,  # type: ignore[arg-type]
            verbose=args.verbose,
            dry_run=args.dry_run,
        )

    print(
        "Done. Run (optional): ./scripts/make_icns.sh packaging/macos/"
        "icon.iconset packaging/macos/resources/app.icns"
    )


if __name__ == "__main__":
    main()
