from pathlib import Path

from PIL import Image, ImageDraw


def create_icon():
    # Create a 256x256 image with a dark blue background
    img = Image.new('RGBA', (256, 256), color=(20, 20, 40, 255))
    d = ImageDraw.Draw(img)

    # Draw a "warp" spiral or circle
    for i in range(10):
        d.ellipse([20 + i * 10, 20 + i * 10, 236 - i * 10, 236 - i * 10], outline=(100 + i * 15, 200, 255), width=2)

    # Draw text "TW"
    # (Skipping text for simplicity without loading fonts, just shapes)
    d.rectangle([100, 100, 156, 156], fill=(255, 200, 50))

    output = (
        Path(__file__).resolve().parents[1]
        / "packaging"
        / "linux"
        / "time-warp-studio.png"
    )
    output.parent.mkdir(parents=True, exist_ok=True)
    img.save(output)
    print(f"Icon written to {output}")


if __name__ == "__main__":
    create_icon()
