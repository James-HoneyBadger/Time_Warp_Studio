#!/usr/bin/env python3
"""
Generate placeholder icon PNGs (icon.iconset) and Fastlane screenshot placeholders.
Requires: Pillow (pip install Pillow)
Usage: python3 scripts/generate_placeholders.py
Generates:
 - packaging/macos/icon.iconset/*.png
 - packaging/macos/resources/app.icns (via make_icns.sh if you run it)
 - fastlane/metadata/en-US/screenshots/*.png
"""
import os
from pathlib import Path

try:
    from PIL import Image, ImageDraw, ImageFont
except Exception as exc:
    print("Pillow not installed. Install with: pip install Pillow")
    raise

root = Path(__file__).resolve().parents[1]
iconset_dir = root / "packaging" / "macos" / "icon.iconset"
res_dir = root / "packaging" / "macos" / "resources"
shots_dir = root / "fastlane" / "metadata" / "en-US" / "screenshots"

iconset_dir.mkdir(parents=True, exist_ok=True)
res_dir.mkdir(parents=True, exist_ok=True)
shots_dir.mkdir(parents=True, exist_ok=True)

# sizes for icon.iconset
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

print(f"Generating icon PNGs in {iconset_dir}")
for size, name in sizes:
    img = Image.new("RGBA", (size, size), (30, 144, 255, 255))
    draw = ImageDraw.Draw(img)
    # draw simple text (first letter)
    text = "T"
    try:
        font = ImageFont.truetype(
            "/System/Library/Fonts/Helvetica.ttc", int(size * 0.6)
        )
    except Exception:
        font = ImageFont.load_default()
    w, h = draw.textsize(text, font=font)
    draw.text(
        ((size - w) / 2, (size - h) / 2), text, font=font, fill=(255, 255, 255, 255)
    )
    img.save(iconset_dir / name, format="PNG")

print(f"Generating placeholder screenshots in {shots_dir}")
for i in range(1, 4):
    w, h = (1280, 800)
    img = Image.new("RGB", (w, h), (10 + i * 30, 20 + i * 20, 40 + i * 10))
    draw = ImageDraw.Draw(img)
    title = f"Time Warp — Screenshot {i}"
    try:
        font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 48)
    except Exception:
        font = ImageFont.load_default()
    tw, th = draw.textsize(title, font=font)
    draw.text(((w - tw) / 2, (h - th) / 2), title, font=font, fill=(255, 255, 255))
    out_path = shots_dir / f"screenshot_{i}.png"
    img.save(out_path, format="PNG")

print(
    "Done. Run: ./scripts/make_icns.sh packaging/macos/icon.iconset packaging/macos/resources/app.icns"
)
