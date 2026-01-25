# Python Programming Tutorial

Python is a powerful, general-purpose programming language included in Time Warp Studio. It provides access to modern programming capabilities while maintaining educational clarity.

## Quick Start

Python programs in Time Warp are straightforward:

```python
print("Hello, World!")
name = input("What's your name? ")
print(f"Hello, {name}!")
```

Run this program:
1. Select "Python" from the language dropdown
2. Paste the code into the editor
3. Click Run (Ctrl+R)

## Basic Syntax

### Comments

```python
# This is a comment
x = 5  # Inline comment

"""
This is a multi-line comment
or docstring
"""
```

### Variables and Data Types

```python
# Numbers
age = 25
height = 5.9

# Strings
name = "Alice"
city = 'New York'

# Booleans
is_student = True
is_teacher = False

# Lists
numbers = [1, 2, 3, 4, 5]
colors = ["red", "green", "blue"]

# Dictionaries
person = {"name": "Bob", "age": 30}
```

### Output: print()

```python
print("Hello, World!")
print("Name:", "Alice")
print(10 + 5)
print(f"I am {age} years old")  # f-string
```

### Input: input()

```python
name = input("Enter your name: ")
print(f"Hello, {name}!")

age = int(input("Enter your age: "))
height = float(input("Enter your height: "))
```

## Operators

### Arithmetic

```python
a = 10
b = 3

print(a + b)   # 13 (addition)
print(a - b)   # 7 (subtraction)
print(a * b)   # 30 (multiplication)
print(a / b)   # 3.333... (division)
print(a // b)  # 3 (floor division)
print(a % b)   # 1 (remainder/modulo)
print(a ** b)  # 1000 (exponentiation)
```

### Comparison

```python
x = 10
print(x == 10)    # True (equal)
print(x != 10)    # False (not equal)
print(x > 5)      # True (greater than)
print(x < 20)     # True (less than)
print(x >= 10)    # True (greater than or equal)
print(x <= 15)    # True (less than or equal)
```

### Logical

```python
age = 25
is_student = True

if age >= 18 and is_student:
    print("Adult student")

if age < 18 or is_student:
    print("Young or student")

if not is_student:
    print("Not a student")
```

## Control Flow

### if/elif/else

```python
score = 85

if score >= 90:
    print("Grade: A")
elif score >= 80:
    print("Grade: B")
elif score >= 70:
    print("Grade: C")
else:
    print("Grade: F")
```

### Ternary Operator

```python
age = 20
status = "Adult" if age >= 18 else "Minor"
print(status)  # Adult
```

### while Loop

```python
count = 0
while count < 5:
    print(f"Count: {count}")
    count += 1
```

### for Loop

```python
# Loop over a range
for i in range(5):
    print(f"Number: {i}")

# Loop over a list
colors = ["red", "green", "blue"]
for color in colors:
    print(f"Color: {color}")

# Loop with index
for i, color in enumerate(colors):
    print(f"{i}: {color}")
```

### break and continue

```python
for i in range(10):
    if i == 5:
        break  # Exit loop
    print(i)

for i in range(10):
    if i == 3:
        continue  # Skip this iteration
    print(i)
```

## Functions

### Basic Functions

```python
def greet():
    print("Hello!")

greet()  # Call the function
```

### Functions with Parameters

```python
def greet(name):
    print(f"Hello, {name}!")

greet("Alice")
greet("Bob")
```

### Return Values

```python
def add(a, b):
    return a + b

result = add(3, 5)
print(result)  # 8
```

### Default Parameters

```python
def greet(name="Friend"):
    print(f"Hello, {name}!")

greet()           # Hello, Friend!
greet("Alice")    # Hello, Alice!
```

### Keyword Arguments

```python
def print_info(name, age, city):
    print(f"{name}, {age}, {city}")

print_info(name="Alice", city="NYC", age=25)
print_info("Bob", age=30, city="LA")
```

## Data Structures

### Lists

```python
numbers = [1, 2, 3, 4, 5]

# Access elements
print(numbers[0])      # 1
print(numbers[-1])     # 5

# Add elements
numbers.append(6)
numbers.insert(0, 0)

# Remove elements
numbers.remove(3)
numbers.pop(0)

# List operations
print(len(numbers))    # length
print(3 in numbers)    # membership
print(numbers[:3])     # slice
```

### Dictionaries

```python
person = {
    "name": "Alice",
    "age": 25,
    "city": "NYC"
}

# Access
print(person["name"])           # Alice
print(person.get("age"))        # 25

# Modify
person["age"] = 26
person["job"] = "Engineer"

# Delete
del person["city"]

# Iterate
for key, value in person.items():
    print(f"{key}: {value}")
```

### Tuples (Immutable Lists)

```python
coordinates = (10, 20)
print(coordinates[0])  # 10

x, y = coordinates  # Unpacking
print(x, y)
```

### Sets

```python
fruits = {"apple", "banana", "cherry"}

# Add
fruits.add("date")

# Remove
fruits.remove("banana")

# Operations
set1 = {1, 2, 3}
set2 = {2, 3, 4}
print(set1 & set2)   # {2, 3} (intersection)
print(set1 | set2)   # {1, 2, 3, 4} (union)
```

## String Operations

```python
text = "Hello, World!"

# Length
print(len(text))  # 13

# Case
print(text.upper())
print(text.lower())

# Replace
print(text.replace("World", "Python"))

# Split and Join
words = text.split(", ")
print(" | ".join(words))

# Check if substring exists
print("World" in text)  # True

# Formatting
name = "Alice"
age = 25
print(f"{name} is {age} years old")
print("{} is {} years old".format(name, age))
```

## Complete Example Programs

### Grade Calculator

```python
def calculate_grade(score):
    if score >= 90:
        return "A"
    elif score >= 80:
        return "B"
    elif score >= 70:
        return "C"
    elif score >= 60:
        return "D"
    else:
        return "F"

def main():
    print("=== Grade Calculator ===")
    while True:
        try:
            score = float(input("Enter score (0-100): "))
            if 0 <= score <= 100:
                grade = calculate_grade(score)
                print(f"Grade: {grade}")
                break
            else:
                print("Please enter a score between 0 and 100")
        except ValueError:
            print("Invalid input. Please enter a number.")

main()
```

### Number Guessing Game

```python
import random

def guess_game():
    number = random.randint(1, 100)
    attempts = 0
    guessed = False
    
    print("=== Guessing Game ===")
    print("I'm thinking of a number between 1 and 100")
    
    while not guessed:
        try:
            guess = int(input("Make a guess: "))
            attempts += 1
            
            if guess < number:
                print("Too low!")
            elif guess > number:
                print("Too high!")
            else:
                print(f"Correct! You guessed it in {attempts} attempts!")
                guessed = True
        except ValueError:
            print("Please enter a valid number")

guess_game()
```

### Student Manager

```python
students = {}

def add_student():
    name = input("Student name: ")
    age = int(input("Student age: "))
    grade = input("Student grade: ")
    students[name] = {"age": age, "grade": grade}
    print(f"Added {name}")

def list_students():
    if not students:
        print("No students in the system")
    for name, info in students.items():
        print(f"{name}: Age {info['age']}, Grade {info['grade']}")

def main():
    while True:
        print("\n=== Student Manager ===")
        print("1. Add student")
        print("2. List students")
        print("3. Exit")
        choice = input("Choose: ")
        
        if choice == "1":
            add_student()
        elif choice == "2":
            list_students()
        elif choice == "3":
            print("Goodbye!")
            break
        else:
            print("Invalid choice")

main()
```

### Unit Converter

```python
def celsius_to_fahrenheit(c):
    return (c * 9/5) + 32

def kilometers_to_miles(km):
    return km * 0.621371

def pounds_to_kg(lbs):
    return lbs * 0.453592

def main():
    print("=== Unit Converter ===")
    while True:
        print("\n1. Celsius to Fahrenheit")
        print("2. Kilometers to Miles")
        print("3. Pounds to Kg")
        print("4. Exit")
        
        choice = input("Choose: ")
        
        if choice == "1":
            c = float(input("Celsius: "))
            print(f"{c}°C = {celsius_to_fahrenheit(c):.2f}°F")
        elif choice == "2":
            km = float(input("Kilometers: "))
            print(f"{km} km = {kilometers_to_miles(km):.2f} miles")
        elif choice == "3":
            lbs = float(input("Pounds: "))
            print(f"{lbs} lbs = {pounds_to_kg(lbs):.2f} kg")
        elif choice == "4":
            break

main()
```

## Tips and Best Practices

1. **Use Meaningful Names**: `age` is better than `a`
2. **Keep Functions Small**: Each function should do one thing
3. **Use Comments**: Explain complex logic
4. **Handle Errors**: Use try/except for user input
5. **Test Regularly**: Run your code frequently

## Common Mistakes to Avoid

```python
# ❌ Wrong: Comparing strings to numbers
age = "25"
if age > 18:  # Error!
    pass

# ✅ Right: Convert to number first
age = int(input("Age: "))
if age > 18:
    pass

# ❌ Wrong: Forgetting to convert input
score = input("Score: ")
total = score + 10  # String concatenation, not math!

# ✅ Right: Convert to appropriate type
score = int(input("Score: "))
total = score + 10

# ❌ Wrong: Mutable default arguments
def add_item(item, list=[]):
    list.append(item)
    return list

# ✅ Right: Use None as default
def add_item(item, list=None):
    if list is None:
        list = []
    list.append(item)
    return list
```

## Python Built-in Functions Reference

| Function | Purpose | Example |
|----------|---------|---------|
| `print()` | Output text | `print("Hello")` |
| `input()` | Get user input | `name = input("Name: ")` |
| `int()` | Convert to integer | `int("42")` → 42 |
| `float()` | Convert to float | `float("3.14")` → 3.14 |
| `str()` | Convert to string | `str(42)` → "42" |
| `len()` | Get length | `len("hello")` → 5 |
| `range()` | Create sequence | `range(5)` → [0, 1, 2, 3, 4] |
| `sum()` | Sum values | `sum([1, 2, 3])` → 6 |
| `min()` | Find minimum | `min([1, 5, 3])` → 1 |
| `max()` | Find maximum | `max([1, 5, 3])` → 5 |
| `sorted()` | Sort list | `sorted([3, 1, 2])` → [1, 2, 3] |
| `list()` | Create list | `list("abc")` → ['a', 'b', 'c'] |
| `dict()` | Create dictionary | `dict([('a', 1)])` → {'a': 1} |
| `enumerate()` | Index and value | `enumerate(['a', 'b'])` → [(0, 'a'), (1, 'b')] |
| `zip()` | Combine lists | `zip([1, 2], ['a', 'b'])` → [(1, 'a'), (2, 'b')] |

## Running Python Programs in Time Warp Studio

1. Create a `.py` file with your program
2. Select "Python" from the language dropdown
3. Paste your code or load the file
4. Click Run or press Ctrl+R
5. Interact with the program in the Output panel

## Next Steps

- Learn [BASIC for classic programming](basic.md)
- Explore [PILOT for interactive lessons](pilot.md)
- Try [Logo for turtle graphics](logo.md)
- Check out [example programs](../Examples/) in the repository

Happy Python programming!
