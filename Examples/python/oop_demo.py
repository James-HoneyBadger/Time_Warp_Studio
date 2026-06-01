"""
OOP Demo — Python
Classes, inheritance, dunder methods, properties.
"""

import math


class Shape:
    """Abstract base for all shapes."""

    def area(self):
        raise NotImplementedError

    def perimeter(self):
        raise NotImplementedError

    def __str__(self):
        return (f"{self.__class__.__name__}: "
                f"area={self.area():.2f}, perimeter={self.perimeter():.2f}")


class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return math.pi * self.radius ** 2

    def perimeter(self):
        return 2 * math.pi * self.radius


class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height

    def area(self):
        return self.width * self.height

    def perimeter(self):
        return 2 * (self.width + self.height)


class Square(Rectangle):
    def __init__(self, side):
        super().__init__(side, side)

    def __repr__(self):
        return f"Square(side={self.width})"


class Triangle(Shape):
    def __init__(self, a, b, c):
        self.a, self.b, self.c = a, b, c

    def perimeter(self):
        return self.a + self.b + self.c

    def area(self):
        s = self.perimeter() / 2
        return math.sqrt(s * (s - self.a) * (s - self.b) * (s - self.c))


# ── Operator overloading demo ─────────────────────────────────────────
class Vector2D:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vector2D(self.x + other.x, self.y + other.y)

    def __mul__(self, scalar):
        return Vector2D(self.x * scalar, self.y * scalar)

    def magnitude(self):
        return math.sqrt(self.x**2 + self.y**2)

    def __repr__(self):
        return f"Vector2D({self.x}, {self.y})"


# ── Demo ──────────────────────────────────────────────────────────────
shapes = [Circle(5), Rectangle(4, 6), Square(3), Triangle(3, 4, 5)]

print("=== Shapes ===")
for shape in shapes:
    print(f"  {shape}")

print("\n=== Polymorphism: total area ===")
total = sum(s.area() for s in shapes)
print(f"  Total area: {total:.2f}")

print("\n=== isinstance checks ===")
for shape in shapes:
    print(f"  {shape.__class__.__name__} is Shape: {isinstance(shape, Shape)}")

print("\n=== Vector2D (operator overloading) ===")
v1 = Vector2D(3, 4)
v2 = Vector2D(1, 2)
print(f"  v1 = {v1}, magnitude = {v1.magnitude():.2f}")
print(f"  v2 = {v2}")
print(f"  v1 + v2 = {v1 + v2}")
print(f"  v1 * 3 = {v1 * 3}")
