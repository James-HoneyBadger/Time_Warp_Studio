# Turtle graphics in Python
# Draws a colorful Sierpinski-like nested pattern

import math

def draw_polygon(sides, size):
    angle = 360 / sides
    for _ in range(sides):
        forward(size)
        right(angle)

def draw_star(points, size):
    angle = 180 - (180 * (points - 2) / points)
    skip = 2
    for _ in range(points):
        forward(size)
        right(180 - angle * skip)

print("Drawing polygon pattern...")

pendown()
home()

# Draw concentric polygons with rotating offset
shapes = [
    (3, 60, (255, 80, 80)),    # triangle
    (4, 55, (80, 200, 80)),    # square
    (5, 50, (80, 80, 255)),    # pentagon
    (6, 45, (255, 200, 80)),   # hexagon
    (7, 40, (200, 80, 255)),   # heptagon
    (8, 35, (80, 220, 220)),   # octagon
]

for sides, size, (r, g, b) in shapes:
    color(r, g, b)
    pensize(2)
    goto(0, 0)
    setheading(0)
    draw_polygon(sides, size)

print("Pattern complete!")
