# =============================================
#  Python Comprehensive Demo - Time Warp Studio
# =============================================

# --- Hello World ---
print("===== HELLO WORLD =====")
print("Welcome to Python!")
print()

# --- Variables and Types ---
print("===== VARIABLES & TYPES =====")
x = 10
y = 3
pi = 3.14159
name = "Python"
flag = True
print(f"int: {x}")
print(f"float: {pi}")
print(f"string: {name}")
print(f"bool: {flag}")
print()

# --- Arithmetic ---
print("===== ARITHMETIC =====")
print(f"x + y = {x + y}")
print(f"x - y = {x - y}")
print(f"x * y = {x * y}")
print(f"x / y = {x / y}")
print(f"x // y = {x // y}")
print(f"x % y = {x % y}")
print(f"2 ** 10 = {2 ** 10}")
print()

# --- Strings ---
print("===== STRINGS =====")
s = "Hello World"
print(f"upper: {s.upper()}")
print(f"lower: {s.lower()}")
print(f"split: {s.split()}")
print(f"replace: {s.replace('World', 'Python')}")
print(f"join: {'-'.join(['a', 'b', 'c'])}")
print(f"strip: {'  hi  '.strip()}")
print(f"len: {len(s)}")
print()

# --- Lists ---
print("===== LISTS =====")
nums = [5, 2, 8, 1, 9, 3]
print(f"list: {nums}")
print(f"sorted: {sorted(nums)}")
print(f"sum: {sum(nums)}")
print(f"min: {min(nums)}")
print(f"max: {max(nums)}")
print(f"len: {len(nums)}")
squares = [i**2 for i in range(6)]
print(f"squares: {squares}")
print()

# --- Dictionaries ---
print("===== DICTIONARIES =====")
d = {"name": "Alice", "age": 30, "city": "Wonderland"}
print(f"dict: {d}")
print(f"name: {d['name']}")
print(f"keys: {list(d.keys())}")
print()

# --- Control Flow ---
print("===== CONTROL FLOW =====")
for i in range(1, 6):
    if i % 2 == 0:
        print(f"{i} is even")
    else:
        print(f"{i} is odd")
print()

# --- While Loop ---
print("===== WHILE LOOP =====")
n = 1
while n <= 5:
    print(n, end=" ")
    n += 1
print()
print()

# --- Functions ---
print("===== FUNCTIONS =====")
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(f"5! = {factorial(5)}")
print(f"10! = {factorial(10)}")

def greet(name, greeting="Hello"):
    return f"{greeting}, {name}!"

print(greet("World"))
print(greet("Python", "Welcome"))
print()

# --- Lambda and Higher-Order ---
print("===== LAMBDA & MAP/FILTER =====")
double = lambda x: x * 2
print(f"double(5) = {double(5)}")
print(f"map: {list(map(lambda x: x**2, range(5)))}")
print(f"filter: {list(filter(lambda x: x > 3, range(8)))}")
print()

# --- Classes ---
print("===== CLASSES =====")
class Animal:
    def __init__(self, name, sound):
        self.name = name
        self.sound = sound
    def speak(self):
        return f"{self.name} says {self.sound}"

dog = Animal("Rex", "Woof")
cat = Animal("Whiskers", "Meow")
print(dog.speak())
print(cat.speak())
print()

# --- Exception Handling ---
print("===== EXCEPTIONS =====")
try:
    result = 10 / 0
except ZeroDivisionError as e:
    print(f"Caught: {e}")
print()

# --- Math ---
print("===== MATH =====")
import math
print(f"sqrt(16) = {math.sqrt(16)}")
print(f"pi = {math.pi}")
print(f"factorial(6) = {math.factorial(6)}")
print()

print("===== DONE =====")
