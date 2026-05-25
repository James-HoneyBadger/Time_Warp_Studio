# List comprehensions and functional patterns in Python
# Demonstrates Python's powerful data processing idioms

import math
from functools import reduce

print("=== List Comprehensions ===")
# Basic
squares = [x ** 2 for x in range(1, 11)]
print("Squares 1-10:", squares)

# Filtered
evens = [x for x in range(1, 21) if x % 2 == 0]
print("Even 1-20:", evens)

# Nested
matrix = [[i * j for j in range(1, 5)] for i in range(1, 5)]
print("4x4 multiplication table:")
for row in matrix:
    print("  ", row)

# Flattened
flat = [x for row in matrix for x in row]
print("Flattened:", flat[:8], "...")

# String processing
words = "the quick brown fox jumps over the lazy dog".split()
long_words = [w.upper() for w in words if len(w) > 3]
print("Words > 3 chars:", long_words)

print("\n=== Dictionary Comprehensions ===")
word_lengths = {w: len(w) for w in words}
print("Word lengths:")
for word, length in sorted(word_lengths.items(), key=lambda x: -x[1])[:5]:
    print(f"  {word!r}: {length}")

print("\n=== Set Comprehensions ===")
letter_set = {c.upper() for w in words for c in w}
print(f"Unique letters: {len(letter_set)}")
print("Letters:", sorted(letter_set))

print("\n=== Functional Programming ===")
nums = list(range(1, 11))

# map / filter
doubled_odds = list(map(lambda x: x * 2, filter(lambda x: x % 2 != 0, nums)))
print("Doubled odds:", doubled_odds)

# reduce
product = reduce(lambda a, b: a * b, nums)
print(f"Product of 1-10: {product}")

factorial_10 = reduce(lambda a, b: a * b, range(1, 11))
print(f"10! = {factorial_10}")

# zip and enumerate
letters = 'abcdefghij'
pairs = list(zip(nums, letters))
print("Zipped pairs:", pairs)

print("\n=== Generator Expressions ===")
# Lazy evaluation
gen_sum = sum(x ** 2 for x in range(1, 101))
print(f"Sum of squares 1-100: {gen_sum}")

# Prime check via comprehension
def is_prime(n):
    return n > 1 and all(n % i != 0 for i in range(2, int(math.sqrt(n)) + 1))

primes_under_50 = [x for x in range(2, 51) if is_prime(x)]
print("Primes under 50:", primes_under_50)
