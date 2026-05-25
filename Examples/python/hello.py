# Hello World in Python (sandboxed)
# Demonstrates basic Python syntax inside Time Warp Studio

name = "World"
print(f"Hello, {name}!")

# Greet several people
people = ["Alice", "Bob", "Charlie"]
for person in people:
    print(f"Hello, {person}!")

# String operations
greeting = "hello, python"
print(greeting.upper())
print(greeting.title())
print(f"Length: {len(greeting)}")

# Numbers and arithmetic
x, y = 42, 7
print(f"{x} / {y} = {x / y:.2f}")
print(f"{x} // {y} = {x // y}")
print(f"{x} % {y} = {x % y}")
print(f"{x} ** 2 = {x ** 2}")

# Ranges and list comprehension
squares = [i ** 2 for i in range(1, 6)]
print("Squares:", squares)

# Dictionary
person = {"name": "Ada Lovelace", "born": 1815, "field": "Mathematics"}
for key, value in person.items():
    print(f"  {key}: {value}")

print("Done!")
