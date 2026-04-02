import random

# Minimal fuzzing helpers for test_fuzzing.py


def generate_random_basic_code():
    # Returns a trivial BASIC program (placeholder)
    return f'PRINT {random.randint(1, 100)}'


def generate_random_logo_code():
    # Returns a trivial Logo command (placeholder)
    return f'FORWARD {random.randint(10, 100)}'


def generate_random_python_code():
    # Returns a trivial Python print statement (placeholder)
    return f'print({random.randint(1, 100)})'
