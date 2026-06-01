"""
Decorators and Functional Python
Closures, decorators, functools, and higher-order functions.
"""

import time
import functools


# ── Decorator basics ──────────────────────────────────────────────────
def timer(func):
    """Measure execution time of a function."""
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        start = time.perf_counter()
        result = func(*args, **kwargs)
        elapsed = time.perf_counter() - start
        print(f"  {func.__name__} took {elapsed:.6f}s")
        return result
    return wrapper


def memoize(func):
    """Cache function results (manual LRU)."""
    cache = {}
    @functools.wraps(func)
    def wrapper(*args):
        if args not in cache:
            cache[args] = func(*args)
        return cache[args]
    return wrapper


def repeat(n):
    """Decorator factory: run a function n times."""
    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            for _ in range(n):
                result = func(*args, **kwargs)
            return result
        return wrapper
    return decorator


# ── Decorated functions ───────────────────────────────────────────────
@timer
def slow_sum(n):
    return sum(range(n))


@memoize
def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)


@repeat(3)
def greet(name):
    print(f"  Hello, {name}!")


# ── Closures ──────────────────────────────────────────────────────────
def make_multiplier(factor):
    def multiply(x):
        return x * factor
    return multiply


def make_counter(start=0):
    count = [start]
    def increment(by=1):
        count[0] += by
        return count[0]
    return increment


# ── Higher-order functions ────────────────────────────────────────────
numbers = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]

# ── Demo ──────────────────────────────────────────────────────────────
print("=== Timer decorator ===")
slow_sum(1_000_000)

print("\n=== Memoized Fibonacci ===")
print(f"  fib(10) = {fib(10)}")
print(f"  fib(20) = {fib(20)}")
print(f"  fib(30) = {fib(30)}")

print("\n=== Repeat decorator ===")
greet("World")

print("\n=== Closures ===")
double = make_multiplier(2)
triple = make_multiplier(3)
print(f"  double(7) = {double(7)}")
print(f"  triple(7) = {triple(7)}")

counter = make_counter(10)
print(f"  counter() = {counter()}")
print(f"  counter(5) = {counter(5)}")

print("\n=== Higher-order functions ===")
print(f"  numbers = {numbers}")
print(f"  sorted   = {sorted(numbers)}")
print(f"  evens    = {list(filter(lambda x: x % 2 == 0, numbers))}")
print(f"  doubled  = {list(map(lambda x: x * 2, numbers))}")
print(f"  sum      = {functools.reduce(lambda a, b: a + b, numbers)}")
