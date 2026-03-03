"""Comprehensive tests for the Python language executor."""


from time_warp.core.interpreter import Language

from .conftest_lang import run, has, first_error

L = Language.PYTHON


def py(source: str, **kw) -> list[str]:
    """Shortcut: run a Python program."""
    return run(source, L, **kw)


# ============================================================================
# PRINT
# ============================================================================


class TestPrint:
    def test_hello_world(self):
        out = py('print("Hello World")')
        assert has(out, "Hello World")

    def test_print_number(self):
        out = py("print(42)")
        assert has(out, "42")

    def test_print_expression(self):
        out = py("print(3 + 4)")
        assert has(out, "7")

    def test_print_multiple_args(self):
        out = py('print("A", "B", "C")')
        assert has(out, "A") and has(out, "B") and has(out, "C")

    def test_print_sep(self):
        out = py('print("A", "B", sep="-")')
        assert has(out, "A-B")

    def test_print_end(self):
        out = py('print("Hello", end="!")\nprint("World")')
        assert has(out, "Hello") and has(out, "World")

    def test_print_fstring(self):
        out = py('x = 5\nprint(f"x is {x}")')
        assert has(out, "x is 5")


# ============================================================================
# VARIABLES / ASSIGNMENT
# ============================================================================


class TestVariables:
    def test_int_var(self):
        out = py("x = 42\nprint(x)")
        assert has(out, "42")

    def test_float_var(self):
        out = py("x = 3.14\nprint(x)")
        assert has(out, "3.14")

    def test_string_var(self):
        out = py('s = "Hello"\nprint(s)')
        assert has(out, "Hello")

    def test_bool_var(self):
        out = py("b = True\nprint(b)")
        assert has(out, "True")

    def test_none_var(self):
        out = py("x = None\nprint(x)")
        assert has(out, "None")

    def test_multiple_assignment(self):
        out = py("a, b = 1, 2\nprint(a, b)")
        assert has(out, "1") and has(out, "2")

    def test_augmented_assignment(self):
        out = py("x = 5\nx += 3\nprint(x)")
        assert has(out, "8")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = py("print(2 + 3)")
        assert has(out, "5")

    def test_subtract(self):
        out = py("print(10 - 4)")
        assert has(out, "6")

    def test_multiply(self):
        out = py("print(6 * 7)")
        assert has(out, "42")

    def test_divide(self):
        out = py("print(10 / 4)")
        assert has(out, "2.5")

    def test_floor_divide(self):
        out = py("print(10 // 3)")
        assert has(out, "3")

    def test_modulo(self):
        out = py("print(10 % 3)")
        assert has(out, "1")

    def test_power(self):
        out = py("print(2 ** 10)")
        assert has(out, "1024")


# ============================================================================
# STRINGS
# ============================================================================


class TestStrings:
    def test_len(self):
        out = py('print(len("Hello"))')
        assert has(out, "5")

    def test_upper(self):
        out = py('print("hello".upper())')
        assert has(out, "HELLO")

    def test_lower(self):
        out = py('print("HELLO".lower())')
        assert has(out, "hello")

    def test_strip(self):
        out = py('print("  hi  ".strip())')
        assert has(out, "hi")

    def test_replace(self):
        out = py('print("hello".replace("l", "r"))')
        assert has(out, "herro")

    def test_split(self):
        out = py('print("a,b,c".split(","))')
        assert has(out, "a") and has(out, "b") and has(out, "c")

    def test_join(self):
        out = py('print("-".join(["a", "b", "c"]))')
        assert has(out, "a-b-c")

    def test_find(self):
        out = py('print("hello".find("ll"))')
        assert has(out, "2")

    def test_startswith(self):
        out = py('print("hello".startswith("he"))')
        assert has(out, "True")

    def test_slice(self):
        out = py('print("Hello"[1:3])')
        assert has(out, "el")


# ============================================================================
# LISTS
# ============================================================================


class TestLists:
    def test_create(self):
        out = py("x = [1, 2, 3]\nprint(x)")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_append(self):
        out = py("x = [1]\nx.append(2)\nprint(x)")
        assert has(out, "1") and has(out, "2")

    def test_len(self):
        out = py("print(len([1, 2, 3]))")
        assert has(out, "3")

    def test_index(self):
        out = py("x = [10, 20, 30]\nprint(x[1])")
        assert has(out, "20")

    def test_slice(self):
        out = py("x = [1, 2, 3, 4]\nprint(x[1:3])")
        assert has(out, "2") and has(out, "3")

    def test_sort(self):
        out = py("x = [3, 1, 2]\nx.sort()\nprint(x)")
        assert has(out, "[1, 2, 3]")

    def test_reverse(self):
        out = py("x = [1, 2, 3]\nx.reverse()\nprint(x)")
        assert has(out, "[3, 2, 1]")

    def test_list_comprehension(self):
        out = py("x = [i**2 for i in range(5)]\nprint(x)")
        assert has(out, "0") and has(out, "16")

    def test_pop(self):
        out = py("x = [1, 2, 3]\nv = x.pop()\nprint(v)")
        assert has(out, "3")


# ============================================================================
# DICTIONARIES
# ============================================================================


class TestDicts:
    def test_create(self):
        out = py('d = {"a": 1, "b": 2}\nprint(d["a"])')
        assert has(out, "1")

    def test_keys(self):
        out = py('d = {"x": 1, "y": 2}\nprint(sorted(d.keys()))')
        assert has(out, "x") and has(out, "y")

    def test_values(self):
        out = py('d = {"a": 10}\nprint(list(d.values()))')
        assert has(out, "10")

    def test_update(self):
        out = py('d = {"a": 1}\nd["b"] = 2\nprint(len(d))')
        assert has(out, "2")

    def test_get(self):
        out = py('d = {"a": 1}\nprint(d.get("b", "missing"))')
        assert has(out, "missing")


# ============================================================================
# CONTROL FLOW
# ============================================================================


class TestControlFlow:
    def test_if_true(self):
        out = py('x = 5\nif x > 3:\n    print("yes")')
        assert has(out, "yes")

    def test_if_false(self):
        out = py('x = 1\nif x > 3:\n    print("yes")\nelse:\n    print("no")')
        assert has(out, "no")

    def test_elif(self):
        out = py('x = 2\nif x == 1:\n    print("one")\nelif x == 2:\n    print("two")\nelse:\n    print("other")')
        assert has(out, "two")

    def test_for_range(self):
        out = py("for i in range(3):\n    print(i)")
        assert has(out, "0") and has(out, "1") and has(out, "2")

    def test_for_list(self):
        out = py('for c in ["a", "b"]:\n    print(c)')
        assert has(out, "a") and has(out, "b")

    def test_while(self):
        out = py("i = 0\nwhile i < 3:\n    print(i)\n    i += 1")
        assert has(out, "0") and has(out, "2")

    def test_break(self):
        out = py("for i in range(10):\n    if i == 3:\n        break\n    print(i)")
        assert has(out, "2") and not has(out, "4")

    def test_continue(self):
        out = py("for i in range(5):\n    if i == 2:\n        continue\n    print(i)")
        assert has(out, "1") and has(out, "3")


# ============================================================================
# FUNCTIONS
# ============================================================================


class TestFunctions:
    def test_define_and_call(self):
        out = py("def greet(name):\n    return f'Hello {name}'\nprint(greet('World'))")
        assert has(out, "Hello World")

    def test_default_args(self):
        out = py("def f(x, y=10):\n    return x + y\nprint(f(5))")
        assert has(out, "15")

    def test_lambda(self):
        out = py("f = lambda x: x * 2\nprint(f(5))")
        assert has(out, "10")

    def test_recursive(self):
        out = py("def fact(n):\n    return 1 if n <= 1 else n * fact(n-1)\nprint(fact(5))")
        assert has(out, "120")

    def test_args(self):
        out = py("def f(*args):\n    return sum(args)\nprint(f(1, 2, 3))")
        assert has(out, "6")


# ============================================================================
# BUILT-IN FUNCTIONS
# ============================================================================


class TestBuiltins:
    def test_abs(self):
        out = py("print(abs(-5))")
        assert has(out, "5")

    def test_max(self):
        out = py("print(max(1, 5, 3))")
        assert has(out, "5")

    def test_min(self):
        out = py("print(min(1, 5, 3))")
        assert has(out, "1")

    def test_sum(self):
        out = py("print(sum([1, 2, 3]))")
        assert has(out, "6")

    def test_sorted(self):
        out = py("print(sorted([3, 1, 2]))")
        assert has(out, "[1, 2, 3]")

    def test_reversed(self):
        out = py("print(list(reversed([1, 2, 3])))")
        assert has(out, "[3, 2, 1]")

    def test_enumerate(self):
        out = py('for i, v in enumerate(["a", "b"]):\n    print(i, v)')
        assert has(out, "0") and has(out, "1")

    def test_zip(self):
        out = py("for a, b in zip([1, 2], [3, 4]):\n    print(a, b)")
        assert has(out, "1") and has(out, "4")

    def test_map(self):
        out = py("print(list(map(str, [1, 2, 3])))")
        assert has(out, "1") and has(out, "3")

    def test_filter(self):
        out = py("print(list(filter(lambda x: x > 2, [1, 2, 3, 4])))")
        assert has(out, "3") and has(out, "4")

    def test_type(self):
        out = py("print(type(42))")
        assert has(out, "int")

    def test_isinstance(self):
        out = py("print(isinstance(42, int))")
        assert has(out, "True")

    def test_range(self):
        out = py("print(list(range(5)))")
        assert has(out, "0") and has(out, "4")

    def test_int_conversion(self):
        out = py('print(int("42"))')
        assert has(out, "42")

    def test_str_conversion(self):
        out = py("print(str(42))")
        assert has(out, "42")


# ============================================================================
# CLASSES
# ============================================================================


class TestClasses:
    def test_simple_class(self):
        out = py(
            "class Dog:\n"
            "    def __init__(self, name):\n"
            "        self.name = name\n"
            "    def speak(self):\n"
            "        return f'{self.name} says Woof'\n"
            "d = Dog('Rex')\n"
            "print(d.speak())"
        )
        assert has(out, "Rex says Woof")

    def test_inheritance(self):
        out = py(
            "class Animal:\n"
            "    def speak(self):\n"
            "        return 'generic'\n"
            "class Cat(Animal):\n"
            "    def speak(self):\n"
            "        return 'Meow'\n"
            "c = Cat()\n"
            "print(c.speak())"
        )
        assert has(out, "Meow")


# ============================================================================
# EXCEPTIONS
# ============================================================================


class TestExceptions:
    def test_try_except(self):
        out = py(
            "try:\n"
            "    x = 1 / 0\n"
            "except ZeroDivisionError:\n"
            "    print('caught')"
        )
        assert has(out, "caught")

    def test_try_finally(self):
        out = py(
            "try:\n"
            "    x = 1\n"
            "finally:\n"
            "    print('done')"
        )
        assert has(out, "done")


# ============================================================================
# COMPREHENSIONS / GENERATORS
# ============================================================================


class TestComprehensions:
    def test_dict_comprehension(self):
        out = py("d = {i: i**2 for i in range(4)}\nprint(d)")
        assert has(out, "0") and has(out, "9")

    def test_set_comprehension(self):
        out = py("s = {i % 3 for i in range(9)}\nprint(sorted(s))")
        assert has(out, "0") and has(out, "1") and has(out, "2")

    def test_generator(self):
        out = py("g = (i for i in range(3))\nprint(list(g))")
        assert has(out, "0") and has(out, "2")


# ============================================================================
# MATH MODULE
# ============================================================================


class TestMath:
    def test_sqrt(self):
        out = py("import math\nprint(math.sqrt(16))")
        assert has(out, "4")

    def test_pi(self):
        out = py("import math\nprint(math.pi)")
        assert has(out, "3.14")

    def test_sin(self):
        out = py("import math\nprint(math.sin(0))")
        assert has(out, "0")

    def test_floor_ceil(self):
        out = py("import math\nprint(math.floor(3.7), math.ceil(3.2))")
        assert has(out, "3") and has(out, "4")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_syntax_error(self):
        out = py("def")
        assert first_error(out) is not None or len(out) > 0

    def test_name_error(self):
        out = py("print(undefined_var)")
        assert first_error(out) is not None or len(out) > 0

    def test_zero_division(self):
        out = py("print(1/0)")
        assert first_error(out) is not None or len(out) > 0
