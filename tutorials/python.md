# Python Tutorial

## Introduction

Python (1991) is a modern multi-paradigm language celebrated for its clean, readable syntax. Time Warp Studio runs Python programs in a sandboxed environment with access to the standard library and the integrated turtle graphics canvas.

**Key characteristics:**
- Indentation defines code blocks (no braces or `end`)
- Dynamic typing with optional type hints
- Batteries included — rich standard library
- Multi-paradigm: procedural, object-oriented, and functional

## Hello World

```python
print("Hello, World!")
```

## Variables and Types

```python
# Numbers
x = 42
pi = 3.14159
big = 10 ** 20      # arbitrary-precision int

# Strings
name = "Alice"
greeting = f"Hello, {name}!"  # f-string
multi = """
This is a
multi-line string
"""

# Booleans
is_valid = True
is_empty = False

# None
result = None
```

## Arithmetic

```python
print(3 + 4)        # 7
print(10 - 3)       # 7
print(6 * 7)        # 42
print(10 / 3)       # 3.3333... (float division)
print(10 // 3)      # 3        (floor division)
print(17 % 5)       # 2        (modulo)
print(2 ** 10)      # 1024
print(abs(-7))      # 7

import math
print(math.sqrt(16))  # 4.0
print(math.factorial(6))  # 720
```

## Strings

```python
s = "Hello, World!"
print(len(s))            # 13
print(s.upper())         # HELLO, WORLD!
print(s.lower())         # hello, world!
print(s[7:12])           # World    (slice)
print(s.replace("World", "Python"))
print(s.split(", "))     # ['Hello', 'World!']
print("  hi  ".strip())  # hi
print("ha" * 3)          # hahaha
print("World" in s)      # True
```

## Lists

```python
arr = [1, 2, 3, 4, 5]

print(len(arr))          # 5
print(arr[0])            # 1
print(arr[-1])           # 5 (from end)
print(arr[1:4])          # [2, 3, 4]

# List comprehensions
squares = [n**2 for n in arr]
evens   = [n for n in arr if n % 2 == 0]
print(squares)  # [1, 4, 9, 16, 25]
print(evens)    # [2, 4]

# Common methods
arr.append(6)
arr.extend([7, 8])
arr.pop()               # removes 8
print(sorted(arr))
print(sum(arr))
print(min(arr), max(arr))
```

## Dictionaries

```python
person = {"name": "Alice", "age": 30}

print(person["name"])     # Alice
person["email"] = "alice@example.com"
del person["age"]

for key, val in person.items():
    print(f"{key}: {val}")

print("name" in person)   # True
print(person.get("phone", "unknown"))  # unknown
```

## Control Flow

```python
x = 42

# if / elif / else
if x > 100:
    print("Very large")
elif x > 10:
    print("Large")
else:
    print("Small")

# Ternary
label = "positive" if x > 0 else "non-positive"

# match (Python 3.10+)
match x:
    case 0:
        print("zero")
    case 42:
        print("the answer")
    case _:
        print("other")
```

## Loops

```python
# for over range
for i in range(1, 6):
    print(i)

# for over list
for item in ["apple", "banana", "cherry"]:
    print(item)

# enumerate
for i, item in enumerate(["a", "b", "c"]):
    print(f"{i}: {item}")

# while
n = 10
while n > 0:
    print(n)
    n -= 2

# break / continue
for i in range(10):
    if i == 5:
        break
    if i % 2 == 0:
        continue
    print(i)
```

## Functions

```python
def greet(name="World"):
    return f"Hello, {name}!"

print(greet())          # Hello, World!
print(greet("Alice"))   # Hello, Alice!

def factorial(n):
    return 1 if n <= 1 else n * factorial(n - 1)

print(factorial(6))     # 720

# *args and **kwargs
def show(*args, **kwargs):
    for arg in args:
        print(arg)
    for k, v in kwargs.items():
        print(f"{k}={v}")

show(1, 2, 3, name="Alice", age=30)

# Lambda
square = lambda n: n * n
print(square(7))        # 49
```

## Classes

```python
class Animal:
    def __init__(self, name, sound="..."):
        self.name = name
        self.sound = sound

    def speak(self):
        return f"{self.name} says {self.sound}"

    def __str__(self):
        return f"Animal({self.name})"

class Dog(Animal):
    def __init__(self, name):
        super().__init__(name, "Woof!")

    def fetch(self, item):
        return f"{self.name} fetches the {item}!"

dog = Dog("Rex")
print(dog.speak())          # Rex says Woof!
print(dog.fetch("ball"))    # Rex fetches the ball!
print(dog)                  # Animal(Rex)
```

## Turtle Graphics

```python
forward(100)
right(90)
forward(100)
right(90)
forward(100)
right(90)
forward(100)

color("blue")
penwidth(3)
circle(50)
```

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Print | `print(expr)` |
| Input | `input("prompt")` |
| Variable | `name = value` |
| F-string | `f"text {expr}"` |
| List | `[a, b, c]` |
| Dict | `{key: val}` |
| List comprehension | `[f(x) for x in lst if cond]` |
| Function | `def name(args): ...` |
| Class | `class Name(Base): ...` |
| Import | `import module` |
| Range | `range(start, stop, step)` |
| Slice | `lst[start:stop:step]` |
| Lambda | `lambda args: expr` |
