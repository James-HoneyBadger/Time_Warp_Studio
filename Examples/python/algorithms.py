# Classic algorithms in Python
# Demonstrates sorting, searching, and mathematical algorithms

import math

# --- Sorting algorithms ---

def bubble_sort(lst):
    arr = list(lst)
    n = len(arr)
    for i in range(n):
        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
    return arr


def merge_sort(lst):
    if len(lst) <= 1:
        return list(lst)
    mid = len(lst) // 2
    left = merge_sort(lst[:mid])
    right = merge_sort(lst[mid:])
    result = []
    i = j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i]); i += 1
        else:
            result.append(right[j]); j += 1
    return result + left[i:] + right[j:]


def binary_search(arr, target):
    lo, hi = 0, len(arr) - 1
    while lo <= hi:
        mid = (lo + hi) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            lo = mid + 1
        else:
            hi = mid - 1
    return -1


# --- Mathematical algorithms ---

def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True


def sieve_of_eratosthenes(limit):
    sieve = [True] * (limit + 1)
    sieve[0] = sieve[1] = False
    for i in range(2, int(math.sqrt(limit)) + 1):
        if sieve[i]:
            for j in range(i * i, limit + 1, i):
                sieve[j] = False
    return [i for i, v in enumerate(sieve) if v]


def gcd(a, b):
    while b:
        a, b = b, a % b
    return a


def fibonacci(n):
    a, b = 0, 1
    result = []
    for _ in range(n):
        result.append(a)
        a, b = b, a + b
    return result


# --- Run demonstrations ---

print("=== Sorting ===")
data = [64, 34, 25, 12, 22, 11, 90]
print(f"Original:   {data}")
print(f"Bubble:     {bubble_sort(data)}")
print(f"Merge:      {merge_sort(data)}")

print("\n=== Binary Search ===")
sorted_data = merge_sort(data)
for target in [25, 90, 5]:
    idx = binary_search(sorted_data, target)
    if idx >= 0:
        print(f"Found {target} at index {idx}")
    else:
        print(f"{target} not found")

print("\n=== Primes (Sieve, up to 50) ===")
primes = sieve_of_eratosthenes(50)
print(primes)

print("\n=== Fibonacci (first 12) ===")
print(fibonacci(12))

print("\n=== GCD Examples ===")
pairs = [(48, 18), (100, 75), (17, 5)]
for a, b in pairs:
    print(f"gcd({a}, {b}) = {gcd(a, b)}")
