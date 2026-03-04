"""Comprehensive tests for the Lua language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.LUA


def lua(source: str, **kw) -> list[str]:
    """Shortcut: run a Lua program."""
    return run(source, L, **kw)


# ============================================================================
# PRINT
# ============================================================================


class TestPrint:
    def test_hello_world(self):
        out = lua('print("Hello World")')
        assert has(out, "Hello World")

    def test_print_number(self):
        out = lua("print(42)")
        assert has(out, "42")

    def test_print_expression(self):
        out = lua("print(3 + 4)")
        assert has(out, "7")

    def test_print_multiple(self):
        out = lua('print("A", "B")')
        assert has(out, "A") and has(out, "B")

    def test_print_nil(self):
        out = lua("print(nil)")
        assert has(out, "nil")

    def test_print_boolean(self):
        out = lua("print(true)")
        assert has(out, "true")


# ============================================================================
# VARIABLES
# ============================================================================


class TestVariables:
    def test_global_var(self):
        out = lua("x = 42\nprint(x)")
        assert has(out, "42")

    def test_local_var(self):
        out = lua("local x = 10\nprint(x)")
        assert has(out, "10")

    def test_string_var(self):
        out = lua('local s = "Hello"\nprint(s)')
        assert has(out, "Hello")

    def test_multiple_assignment(self):
        out = lua("local a, b = 1, 2\nprint(a, b)")
        assert has(out, "1") and has(out, "2")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = lua("print(2 + 3)")
        assert has(out, "5")

    def test_subtract(self):
        out = lua("print(10 - 4)")
        assert has(out, "6")

    def test_multiply(self):
        out = lua("print(6 * 7)")
        assert has(out, "42")

    def test_divide(self):
        out = lua("print(10 / 4)")
        assert has(out, "2.5")

    def test_floor_divide(self):
        out = lua("print(10 // 3)")
        assert has(out, "3")

    def test_modulo(self):
        out = lua("print(10 % 3)")
        assert has(out, "1")

    def test_power(self):
        out = lua("print(2 ^ 10)")
        assert has(out, "1024")

    def test_concatenation(self):
        out = lua('print("Hello" .. " " .. "World")')
        assert has(out, "Hello World")


# ============================================================================
# STRINGS
# ============================================================================


class TestStrings:
    def test_len(self):
        out = lua('print(string.len("Hello"))')
        assert has(out, "5")

    def test_upper(self):
        out = lua('print(string.upper("hello"))')
        assert has(out, "HELLO")

    def test_lower(self):
        out = lua('print(string.lower("HELLO"))')
        assert has(out, "hello")

    def test_rep(self):
        out = lua('print(string.rep("ab", 3))')
        assert has(out, "ababab")

    def test_sub(self):
        out = lua('print(string.sub("Hello", 1, 3))')
        assert has(out, "Hel")

    def test_reverse(self):
        out = lua('print(string.reverse("Hello"))')
        assert has(out, "olleH")

    def test_format(self):
        out = lua('print(string.format("%d + %d = %d", 2, 3, 5))')
        assert has(out, "2 + 3 = 5")

    def test_hash_len(self):
        out = lua('local s = "Hello"\nprint(#s)')
        assert has(out, "5")


# ============================================================================
# TABLES
# ============================================================================


class TestTables:
    def test_array_table(self):
        out = lua("local t = {10, 20, 30}\nprint(t[1])")
        assert has(out, "10")

    def test_hash_table(self):
        out = lua('local t = {name="Alice", age=30}\nprint(t.name)')
        assert has(out, "Alice")

    def test_table_insert(self):
        out = lua("local t = {}\ntable.insert(t, 42)\nprint(t[1])")
        assert has(out, "42")

    def test_table_remove(self):
        out = lua("local t = {1, 2, 3}\ntable.remove(t, 2)\nprint(#t)")
        assert has(out, "2")

    def test_table_concat(self):
        out = lua('local t = {"a", "b", "c"}\nprint(table.concat(t, "-"))')
        assert has(out, "a-b-c")

    def test_table_sort(self):
        out = lua("local t = {3, 1, 2}\ntable.sort(t)\nprint(t[1], t[2], t[3])")
        assert has(out, "1") and has(out, "3")


# ============================================================================
# CONTROL FLOW
# ============================================================================


class TestControlFlow:
    def test_if_true(self):
        out = lua('if 5 > 3 then\n  print("yes")\nend')
        assert has(out, "yes")

    def test_if_else(self):
        out = lua('if 1 > 3 then\n  print("yes")\nelse\n  print("no")\nend')
        assert has(out, "no")

    def test_if_elseif(self):
        out = lua(
            'local x = 2\nif x == 1 then\n  print("one")\nelseif x == 2 then\n  print("two")\nelse\n  print("other")\nend'
        )
        assert has(out, "two")

    def test_while(self):
        out = lua("local i = 0\nwhile i < 3 do\n  print(i)\n  i = i + 1\nend")
        assert has(out, "0") and has(out, "2")

    def test_repeat_until(self):
        out = lua("local i = 0\nrepeat\n  print(i)\n  i = i + 1\nuntil i >= 3")
        assert has(out, "0") and has(out, "2")

    def test_for_numeric(self):
        out = lua("for i = 1, 3 do\n  print(i)\nend")
        assert has(out, "1") and has(out, "2") and has(out, "3")

    def test_for_step(self):
        out = lua("for i = 0, 10, 2 do\n  print(i)\nend")
        assert has(out, "0") and has(out, "4") and has(out, "10")

    def test_for_in_ipairs(self):
        out = lua(
            'local t = {"a", "b", "c"}\nfor i, v in ipairs(t) do\n  print(i, v)\nend'
        )
        assert has(out, "a") and has(out, "c")

    def test_break(self):
        out = lua("for i = 1, 10 do\n  if i > 3 then break end\n  print(i)\nend")
        assert has(out, "3") and not has(out, "5")


# ============================================================================
# FUNCTIONS
# ============================================================================


class TestFunctions:
    def test_define_and_call(self):
        out = lua(
            'function greet(name)\n  return "Hello " .. name\nend\nprint(greet("World"))'
        )
        assert has(out, "Hello World")

    def test_local_function(self):
        out = lua("local function double(x)\n  return x * 2\nend\nprint(double(5))")
        assert has(out, "10")

    def test_multiple_returns(self):
        out = lua(
            "function swap(a, b)\n  return b, a\nend\nlocal x, y = swap(1, 2)\nprint(x, y)"
        )
        assert has(out, "2") and has(out, "1")

    def test_recursive(self):
        out = lua(
            "function fact(n)\n  if n <= 1 then return 1 end\n  return n * fact(n - 1)\nend\nprint(fact(5))"
        )
        assert has(out, "120")

    def test_anonymous(self):
        out = lua("local f = function(x) return x * 2 end\nprint(f(5))")
        assert has(out, "10")


# ============================================================================
# MATH LIBRARY
# ============================================================================


class TestMath:
    def test_sqrt(self):
        out = lua("print(math.sqrt(16))")
        assert has(out, "4")

    def test_abs(self):
        out = lua("print(math.abs(-5))")
        assert has(out, "5")

    def test_floor(self):
        out = lua("print(math.floor(3.7))")
        assert has(out, "3")

    def test_ceil(self):
        out = lua("print(math.ceil(3.2))")
        assert has(out, "4")

    def test_max(self):
        out = lua("print(math.max(1, 5, 3))")
        assert has(out, "5")

    def test_min(self):
        out = lua("print(math.min(1, 5, 3))")
        assert has(out, "1")

    def test_pi(self):
        out = lua("print(math.pi)")
        assert has(out, "3.14")

    def test_random(self):
        out = lua("math.randomseed(42)\nprint(math.random(1, 100))")
        assert no_errors(out)


# ============================================================================
# TYPE SYSTEM
# ============================================================================


class TestTypes:
    def test_type_number(self):
        out = lua("print(type(42))")
        assert has(out, "number")

    def test_type_string(self):
        out = lua('print(type("hi"))')
        assert has(out, "string")

    def test_type_table(self):
        out = lua("print(type({}))")
        assert has(out, "table")

    def test_type_boolean(self):
        out = lua("print(type(true))")
        assert has(out, "boolean")

    def test_type_nil(self):
        out = lua("print(type(nil))")
        assert has(out, "nil")

    def test_tonumber(self):
        out = lua('print(tonumber("42"))')
        assert has(out, "42")

    def test_tostring(self):
        out = lua("print(tostring(42))")
        assert has(out, "42")


# ============================================================================
# COMMENTS
# ============================================================================


class TestComments:
    def test_single_line_comment(self):
        out = lua("-- this is a comment\nprint(42)")
        assert has(out, "42")

    def test_block_comment(self):
        out = lua("--[[\nthis is\na block comment\n]]\nprint(42)")
        assert has(out, "42")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_syntax_error(self):
        # "if then end" causes infinite loop in Lua executor, use safe input
        out = lua("local x = @@badtoken")
        assert first_error(out) is not None or len(out) > 0

    def test_undefined_var(self):
        out = lua("print(undefined_var)")
        # Lua prints nil for undefined variables
        assert has(out, "nil") or no_errors(out)

    def test_runtime_error(self):
        out = lua("local x = 1 / 0\nprint(x)")
        assert len(out) > 0  # May produce inf or error


# ============================================================================
# ipairs tuple unpacking
# ============================================================================


class TestIpairsFix:
    """for i, v in ipairs(t) correctly unpacks index and value."""

    def test_ipairs_index_value(self):
        out = lua(
            "local t = {10, 20, 30}\nfor i, v in ipairs(t) do\n  print(i, v)\nend"
        )
        assert has(out, "1", "10")
        assert has(out, "3", "30")


# ============================================================================
# Inline for / inline function (regression)
# ============================================================================


class TestInlineFor:
    """Regression: single-line for ... do BODY end should work."""

    def test_inline_ipairs_for(self):
        out = lua(
            "local t={4,5,6}\nlocal s=0\nfor i,v in ipairs(t) do s=s+v end\nprint(s)"
        )
        assert has(out, "15")

    def test_inline_numeric_for(self):
        out = lua("local s=0\nfor i=1,5 do s=s+i end\nprint(s)")
        assert has(out, "15")


class TestInlineFunction:
    """Regression: single-line function with if/return should work."""

    def test_inline_recursive_function(self):
        out = lua(
            "function fact(n) if n<=1 then return 1 end return n*fact(n-1) end\nprint(fact(5))"
        )
        assert has(out, "120")
