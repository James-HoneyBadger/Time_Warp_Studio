"""
Generators and Iterators — Python
Demonstrates lazy evaluation with generators and custom iterators.
"""

# ── Infinite sequence generators ──────────────────────────────────────
def fibonacci():
    """Infinite Fibonacci generator."""
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b


def primes():
    """Infinite prime number generator (sieve of Eratosthenes)."""
    composites = set()
    n = 2
    while True:
        if n not in composites:
            yield n
            composites.update(range(n * n, n * n + n * 100, n))
        n += 1


def count_up(start=0, step=1):
    """Infinite counter."""
    n = start
    while True:
        yield n
        n += step


# ── Generator expressions ─────────────────────────────────────────────
def squares_up_to(limit):
    return (x * x for x in range(limit))


# ── Custom iterator class ─────────────────────────────────────────────
class Countdown:
    def __init__(self, start):
        self.current = start

    def __iter__(self):
        return self

    def __next__(self):
        if self.current < 0:
            raise StopIteration
        value = self.current
        self.current -= 1
        return value


# ── Pipeline pattern with generators ─────────────────────────────────
def take(n, gen):
    for _ in range(n):
        yield next(gen)


def where(predicate, gen):
    for item in gen:
        if predicate(item):
            yield item


def transform(fn, gen):
    for item in gen:
        yield fn(item)


# ── Demo ──────────────────────────────────────────────────────────────
print("=== First 10 Fibonacci numbers ===")
fib = fibonacci()
print(" ", list(take(10, fib)))

print("\n=== First 15 primes ===")
print(" ", list(take(15, primes())))

print("\n=== Squares up to 10 ===")
print(" ", list(squares_up_to(10)))

print("\n=== Countdown iterator ===")
print(" ", list(Countdown(5)))

print("\n=== Generator pipeline: even Fibonacci < 100 ===")
fib2 = fibonacci()
even_fibs = where(lambda x: x % 2 == 0, fib2)
small_fibs = where(lambda x: x < 100, even_fibs)
print(" ", list(small_fibs))

print("\n=== Lazy evaluation: sum of first 100 odd squares ===")
odd_squares = (x*x for x in range(1, 200, 2))
print(f"  Sum = {sum(odd_squares)}")
