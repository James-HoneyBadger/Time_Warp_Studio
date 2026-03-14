"""Fractal Explorer — Generate text-based fractal patterns.

Demonstrates: recursion, complex math, string formatting, generators.
"""


def mandelbrot_char(c: complex, max_iter: int = 30) -> str:
    """Return a character representing how quickly c escapes."""
    z = 0 + 0j
    chars = " .:-=+*#%@"
    for i in range(max_iter):
        z = z * z + c
        if abs(z) > 2:
            return chars[i % len(chars)]
    return "█"


def render_mandelbrot(
    x_min=-2.0, x_max=1.0, y_min=-1.2, y_max=1.2, width=72, height=28
):
    """Render the Mandelbrot set as text art."""
    print("╔" + "═" * width + "╗")
    print("║" + "  Mandelbrot Set".center(width) + "║")
    print("╠" + "═" * width + "╣")

    for row in range(height):
        y = y_min + (y_max - y_min) * row / height
        line = ""
        for col in range(width):
            x = x_min + (x_max - x_min) * col / width
            line += mandelbrot_char(complex(x, y))
        print("║" + line + "║")

    print("╚" + "═" * width + "╝")


def sierpinski_triangle(order: int):
    """Print Sierpinski triangle using Pascal's triangle mod 2."""
    size = 2**order
    row = [0] * (2 * size + 1)
    row[size] = 1
    for i in range(size):
        line = ""
        for j in range(len(row)):
            if row[j]:
                line += "▲"
            else:
                line += " "
        print(line.rstrip())
        new_row = [0] * len(row)
        for j in range(1, len(row) - 1):
            new_row[j] = row[j - 1] ^ row[j + 1]
        row = new_row


def koch_curve_points(order, length=1.0, start=(0, 0), angle=0):
    """Generate points along a Koch curve (recursive generator)."""
    import math

    if order == 0:
        dx = length * math.cos(math.radians(angle))
        dy = length * math.sin(math.radians(angle))
        yield (start[0] + dx, start[1] + dy)
    else:
        seg = length / 3
        for a in [0, 60, -120, 60]:
            angle += a
            yield from koch_curve_points(order - 1, seg, start, angle)
            dx = seg * math.cos(math.radians(angle))
            dy = seg * math.sin(math.radians(angle))
            for _ in range(3 ** (order - 1)):
                pass  # advance through sub-generator
            start = (start[0] + dx, start[1] + dy)


def dragon_curve(iterations: int):
    """Generate dragon curve sequence as L/R turns."""
    sequence = "R"
    for _ in range(iterations - 1):
        flipped = ""
        for ch in reversed(sequence):
            flipped += "L" if ch == "R" else "R"
        sequence = sequence + "R" + flipped
    return sequence


def print_dragon(iterations=10, scale=1):
    """Render a dragon curve as text art."""
    turns = dragon_curve(iterations)
    # Trace the path
    dx = [0, 1, 0, -1]  # N, E, S, W
    dy = [-1, 0, 1, 0]
    direction = 1  # Start facing East
    x, y = 0, 0
    points = {(x, y)}

    for turn in turns:
        x += dx[direction]
        y += dy[direction]
        points.add((x, y))
        if turn == "R":
            direction = (direction + 1) % 4
        else:
            direction = (direction - 1) % 4

    # Find bounds
    min_x = min(p[0] for p in points)
    max_x = max(p[0] for p in points)
    min_y = min(p[1] for p in points)
    max_y = max(p[1] for p in points)

    # Render
    for row in range(min_y, max_y + 1):
        line = ""
        for col in range(min_x, max_x + 1):
            if (col, row) in points:
                line += "█"
            else:
                line += " "
        print(line.rstrip())


# ── Main ──
print("🔬 Fractal Explorer")
print("=" * 40)
print()

print("1. Mandelbrot Set")
print("-" * 40)
render_mandelbrot(width=60, height=22)
print()

print("2. Sierpinski Triangle (order 5)")
print("-" * 40)
sierpinski_triangle(5)
print()

print("3. Dragon Curve (10 iterations)")
print("-" * 40)
print_dragon(10)
print()

print("✅ Fractal exploration complete!")
