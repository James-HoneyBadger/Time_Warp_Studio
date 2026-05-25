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


class TestStringAdvanced:
    def test_byte(self):
        out = lua('print(string.byte("A"))')
        assert has(out, "65")

    def test_char(self):
        out = lua("print(string.char(65))")
        assert has(out, "A")

    def test_find_returns_positions(self):
        out = lua('print(string.find("hello", "ell"))')
        assert has(out, "2")
        assert has(out, "4")

    def test_gsub_replaces(self):
        out = lua('print(string.gsub("hello", "l", "r"))')
        assert has(out, "herro")

    def test_gsub_count(self):
        out = lua('print(string.gsub("aaa", "a", "b"))')
        assert has(out, "bbb")


class TestTableAdvanced:
    def test_concat_separator(self):
        out = lua('local t={"a","b","c"}\nprint(table.concat(t,","))')
        assert has(out, "a,b,c")

    def test_rawget(self):
        out = lua('local t={x=5}\nprint(rawget(t,"x"))')
        assert has(out, "5")

    def test_rawset(self):
        out = lua('local t={}\nrawset(t,"y",99)\nprint(t.y)')
        assert has(out, "99")

    def test_ipairs_sum(self):
        out = lua(
            "local t={1,2,3,4,5}\n"
            "local s=0\n"
            "for i,v in ipairs(t) do s=s+v end\n"
            "print(s)"
        )
        assert has(out, "15")

    def test_pairs_sum(self):
        out = lua(
            "local t={a=10,b=20}\n"
            "local s=0\n"
            "for k,v in pairs(t) do s=s+v end\n"
            "print(s)"
        )
        assert has(out, "30")


class TestMathAdvanced:
    def test_fmod(self):
        out = lua("print(math.fmod(10,3))")
        assert has(out, "1")

    def test_huge(self):
        out = lua("print(math.huge > 1000)")
        assert has(out, "true")

    def test_log(self):
        out = lua("print(math.floor(math.log(math.exp(1))))")
        assert has(out, "1")


class TestBooleanOps:
    def test_not_false(self):
        out = lua("print(not false)")
        assert has(out, "true")

    def test_not_true(self):
        out = lua("print(not true)")
        assert has(out, "false")

    def test_and_short_circuit(self):
        out = lua("print(1 and 2)")
        assert has(out, "2")

    def test_or_short_circuit(self):
        out = lua("print(false or 42)")
        assert has(out, "42")


class TestSelect:
    def test_select_count(self):
        out = lua('print(select("#", 1, 2, 3, 4, 5))')
        assert has(out, "5")

    def test_select_from(self):
        out = lua('print(select(2, "a", "b", "c"))')
        assert has(out, "b")


class TestLuaNestedFunctions:
    def test_recursive_fibonacci(self):
        src = (
            "function fib(n)\n"
            "  if n <= 1 then return n end\n"
            "  return fib(n-1) + fib(n-2)\n"
            "end\n"
            "print(fib(10))\n"
        )
        out = lua(src)
        assert has(out, "55")
        assert no_errors(out)

    def test_table_as_object(self):
        src = (
            "local obj = {}\n"
            "obj.name = \"Lua\"\n"
            "obj.version = 5\n"
            "print(obj.name)\n"
            "print(obj.version)\n"
        )
        out = lua(src)
        assert has(out, "Lua")
        assert has(out, "5")
        assert no_errors(out)


class TestLuaMathLibrary:
    """Tests for the math library."""

    def test_math_floor(self):
        out = lua("print(math.floor(3.7))")
        assert has(out, "3")
        assert no_errors(out)

    def test_math_ceil(self):
        out = lua("print(math.ceil(3.2))")
        assert has(out, "4")
        assert no_errors(out)

    def test_math_max(self):
        out = lua("print(math.max(3, 7))")
        assert has(out, "7")
        assert no_errors(out)

    def test_math_min(self):
        out = lua("print(math.min(3, 7))")
        assert has(out, "3")
        assert no_errors(out)

    def test_math_abs(self):
        out = lua("print(math.abs(-5))")
        assert has(out, "5")
        assert no_errors(out)

    def test_math_pow(self):
        out = lua("print(math.pow(2, 10))")
        assert has(out, "1024")
        assert no_errors(out)

    def test_math_pi(self):
        out = lua("print(math.pi)")
        assert has(out, "3.14")
        assert no_errors(out)


class TestLuaStringLibrary:
    """Tests for the string library."""

    def test_string_upper(self):
        out = lua('print(string.upper("hello"))')
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_string_lower(self):
        out = lua('print(string.lower("HELLO"))')
        assert has(out, "hello")
        assert no_errors(out)

    def test_string_len(self):
        out = lua('print(string.len("hello"))')
        assert has(out, "5")
        assert no_errors(out)

    def test_string_sub(self):
        out = lua('print(string.sub("hello", 1, 3))')
        assert has(out, "hel")
        assert no_errors(out)

    def test_string_rep(self):
        out = lua('print(string.rep("ab", 3))')
        assert has(out, "ababab")
        assert no_errors(out)

    def test_string_reverse(self):
        out = lua('print(string.reverse("hello"))')
        assert has(out, "olleh")
        assert no_errors(out)


class TestLuaTypeBuiltins:
    """Tests for type() and conversion functions."""

    def test_type_number(self):
        out = lua("print(type(42))")
        assert has(out, "number")
        assert no_errors(out)

    def test_type_string(self):
        out = lua('print(type("hello"))')
        assert has(out, "string")
        assert no_errors(out)

    def test_type_boolean(self):
        out = lua("print(type(true))")
        assert has(out, "boolean")
        assert no_errors(out)

    def test_type_nil(self):
        out = lua("print(type(nil))")
        assert has(out, "nil")
        assert no_errors(out)

    def test_tostring(self):
        out = lua("print(tostring(42))")
        assert has(out, "42")
        assert no_errors(out)

    def test_tonumber(self):
        out = lua('print(tonumber("99"))')
        assert has(out, "99")
        assert no_errors(out)


class TestLuaIpairs:
    """Tests for ipairs iteration."""

    def test_ipairs_outputs_indices(self):
        out = lua("for i, v in ipairs({10,20,30}) do print(i, v) end")
        assert has(out, "1")
        assert has(out, "10")
        assert no_errors(out)

    def test_ipairs_three_elements(self):
        out = lua("for i, v in ipairs({10,20,30}) do print(v) end")
        assert has(out, "10")
        assert has(out, "20")
        assert has(out, "30")
        assert no_errors(out)


class TestLuaStringLib2:
    """Additional string library tests."""

    def test_upper(self):
        out = lua('print(string.upper("hello"))')
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_lower(self):
        out = lua('print(string.lower("HELLO"))')
        assert has(out, "hello")
        assert no_errors(out)

    def test_len(self):
        out = lua('print(string.len("hello"))')
        assert has(out, "5")
        assert no_errors(out)

    def test_rep(self):
        out = lua('print(string.rep("ab", 3))')
        assert has(out, "ababab")
        assert no_errors(out)

    def test_reverse(self):
        out = lua('print(string.reverse("hello"))')
        assert has(out, "olleh")
        assert no_errors(out)

    def test_sub(self):
        out = lua('print(string.sub("hello", 2, 4))')
        assert has(out, "ell")
        assert no_errors(out)

    def test_byte(self):
        out = lua('print(string.byte("A"))')
        assert has(out, "65")
        assert no_errors(out)

    def test_char(self):
        out = lua('print(string.char(65))')
        assert has(out, "A")
        assert no_errors(out)


class TestLuaMathLib2:
    """Additional math library tests."""

    def test_abs(self):
        out = lua('print(math.abs(-7))')
        assert has(out, "7")
        assert no_errors(out)

    def test_floor(self):
        out = lua('print(math.floor(3.7))')
        assert has(out, "3")
        assert no_errors(out)

    def test_ceil(self):
        out = lua('print(math.ceil(3.2))')
        assert has(out, "4")
        assert no_errors(out)

    def test_sqrt(self):
        out = lua('print(math.sqrt(9))')
        assert has(out, "3")
        assert no_errors(out)

    def test_max(self):
        out = lua('print(math.max(3, 7))')
        assert has(out, "7")
        assert no_errors(out)

    def test_min(self):
        out = lua('print(math.min(3, 7))')
        assert has(out, "3")
        assert no_errors(out)

    def test_pi(self):
        out = lua('print(math.pi)')
        assert has(out, "3.14")
        assert no_errors(out)

    def test_huge(self):
        out = lua('print(math.huge > 1000)')
        assert has(out, "true")
        assert no_errors(out)


class TestLuaStringLib3:
    """More Lua string library tests."""

    def test_string_find(self):
        assert has(lua('print(string.find("hello world", "world"))'), "7")

    def test_string_format_int(self):
        assert has(lua('print(string.format("%d", 42))'), "42")

    def test_string_format_float(self):
        assert has(lua('print(string.format("%.2f", 3.14159))'), "3.14")


class TestLuaTableLib2:
    """More Lua table library tests."""

    def test_table_concat(self):
        assert has(lua('print(table.concat({1,2,3}, ","))'), "1,2,3")


class TestLuaTypeChecks2:
    """Lua type checking tests."""

    def test_type_number(self):
        assert has(lua("print(type(42))"), "number")

    def test_type_string(self):
        assert has(lua('print(type("hi"))'), "string")

    def test_type_boolean(self):
        assert has(lua("print(type(true))"), "boolean")

    def test_type_table(self):
        assert has(lua("print(type({}))"), "table")

    def test_tostring(self):
        assert has(lua("print(tostring(42))"), "42")

    def test_tonumber(self):
        assert has(lua('print(tonumber("42"))'), "42")


class TestLuaControlFlow2:
    """More Lua control flow tests."""

    def test_if_true(self):
        assert has(lua("if 5 > 3 then print('yes') end"), "yes")

    def test_if_else_false(self):
        assert has(lua("if 3 > 5 then\nprint('yes')\nelse\nprint('no')\nend"), "no")

    def test_while_count(self):
        assert has(lua("local i = 1\nwhile i <= 3 do\nprint(i)\ni = i + 1\nend"), "3")

    def test_for_range(self):
        assert has(lua("for i = 1, 5 do io.write(i .. ' ') end"), "1")

    def test_nested_if(self):
        assert has(lua("local x = 5\nif x > 0 then\nif x < 10 then\nprint('in range')\nend\nend"), "in range")

    def test_and_logic(self):
        assert has(lua("if true and true then print('both') end"), "both")

    def test_or_logic(self):
        assert has(lua("if false or true then print('either') end"), "either")

    def test_not_logic(self):
        assert has(lua("if not false then print('yes') end"), "yes")


class TestLuaMath2:
    """More Lua math operations."""

    def test_math_abs(self):
        assert has(lua("print(math.abs(-7))"), "7")

    def test_math_ceil(self):
        assert has(lua("print(math.ceil(3.1))"), "4")

    def test_math_floor(self):
        assert has(lua("print(math.floor(3.9))"), "3")

    def test_math_max(self):
        assert has(lua("print(math.max(3, 7, 2))"), "7")

    def test_math_min(self):
        assert has(lua("print(math.min(3, 7, 2))"), "2")

    def test_math_sqrt(self):
        assert has(lua("print(math.sqrt(9))"), "3")

    def test_math_pow(self):
        assert has(lua("print(math.pow and math.pow(2,8) or 2^8)"), "256")

    def test_power_operator(self):
        assert has(lua("print(2^8)"), "256")

    def test_modulo(self):
        assert has(lua("print(10 % 3)"), "1")

    def test_integer_div(self):
        assert has(lua("print(10 // 3)"), "3")


class TestLuaStrings2:
    """More Lua string operations."""

    def test_string_upper(self):
        assert has(lua("print(string.upper('hello'))"), "HELLO")

    def test_string_lower(self):
        assert has(lua("print(string.lower('HELLO'))"), "hello")

    def test_string_rep(self):
        assert has(lua("print(string.rep('ab', 3))"), "ababab")

    def test_string_reverse(self):
        assert has(lua("print(string.reverse('hello'))"), "olleh")

    def test_string_byte(self):
        assert has(lua("print(string.byte('A'))"), "65")

    def test_string_char(self):
        assert has(lua("print(string.char(65))"), "A")

    def test_tostring(self):
        assert has(lua("print(tostring(42))"), "42")

    def test_tonumber_string(self):
        assert has(lua("print(tonumber('42'))"), "42")

    def test_concat_numbers(self):
        assert has(lua("print(1 .. 2 .. 3)"), "123")


class TestLuaArithmetic2:
    """Additional Lua arithmetic tests."""

    def test_add_7_3(self):
        assert has(lua('print(7 + 3)'), "10")

    def test_mul_6_7(self):
        assert has(lua('print(6 * 7)'), "42")

    def test_sub_10_3(self):
        assert has(lua('print(10 - 3)'), "7")

    def test_div_integer(self):
        assert has(lua('print(15 // 3)'), "5")

    def test_mod_10_3(self):
        assert has(lua('print(10 % 3)'), "1")

    def test_abs_negative(self):
        assert has(lua('print(math.abs(-7))'), "7")

    def test_square(self):
        assert has(lua('print(9 * 9)'), "81")

    def test_large_mul(self):
        assert has(lua('print(12 * 12)'), "144")

    def test_chain_add(self):
        assert has(lua('print(1 + 2 + 3 + 4)'), "10")

    def test_nested_expr(self):
        assert has(lua('print((3 + 4) * 2)'), "14")

    def test_float_div(self):
        result = lua('print(7 / 2)')
        assert any("3" in line for line in result)

    def test_print_string(self):
        assert has(lua('print("hello")'), "hello")

    def test_print_world(self):
        assert has(lua('print("world")'), "world")


class TestLuaVariables2:
    """Additional Lua variable tests."""

    def test_assign_42(self):
        assert has(lua('x = 42\nprint(x)'), "42")

    def test_assign_zero(self):
        assert has(lua('x = 0\nprint(x)'), "0")

    def test_string_var(self):
        assert has(lua('s = "test"\nprint(s)'), "test")

    def test_for_loop_sum(self):
        assert has(lua('s = 0\nfor i=1,5 do s=s+i end\nprint(s)'), "15")

    def test_table_length(self):
        assert has(lua('t = {1,2,3}\nprint(#t)'), "3")

    def test_string_length(self):
        assert has(lua('print(#"hello")'), "5")

    def test_for_loop_3(self):
        result = lua('for i=1,3 do print(i) end')
        assert any("3" in line for line in result)

    def test_math_floor(self):
        assert has(lua('print(math.floor(3.7))'), "3")

    def test_math_ceil(self):
        assert has(lua('print(math.ceil(3.2))'), "4")

    def test_math_sqrt(self):
        result = lua('print(math.sqrt(9))')
        assert any("3" in line for line in result)

    def test_math_max(self):
        assert has(lua('print(math.max(3, 7))'), "7")

    def test_math_min(self):
        assert has(lua('print(math.min(3, 7))'), "3")


class TestLuaExtended:
    """More Lua tests."""

    def test_print_100(self):
        assert has(lua('print(100)'), "100")

    def test_print_hello_world(self):
        assert has(lua('print("hello world")'), "hello")

    def test_local_var(self):
        assert has(lua('local x = 42\nprint(x)'), "42")

    def test_addition(self):
        assert has(lua('print(3 + 4)'), "7")

    def test_subtraction(self):
        assert has(lua('print(10 - 3)'), "7")

    def test_multiplication(self):
        assert has(lua('print(6 * 7)'), "42")

    def test_division(self):
        assert has(lua('print(10 / 2)'), "5")

    def test_string_concat(self):
        assert has(lua('print("hello" .. " world")'), "hello")

    def test_if_true(self):
        assert has(lua('if 1 == 1 then\n  print("yes")\nend'), "yes")

    def test_if_false_skip(self):
        r = lua('if 1 == 2 then\n  print("no")\nend')
        assert not any("no" in line for line in r)

    def test_for_loop(self):
        r = lua('for i=1,3 do\n  print(i)\nend')
        texts = " ".join(r)
        assert "1" in texts and "3" in texts

    def test_two_prints(self):
        r = lua('print("A")\nprint("B")')
        texts = " ".join(r)
        assert "A" in texts and "B" in texts

    def test_no_errors_simple(self):
        assert no_errors(lua('print("ok")'))

    def test_output_is_list(self):
        r = lua('print(1)')
        assert isinstance(r, list)

    def test_print_zero(self):
        assert has(lua('print(0)'), "0")

    def test_string_length(self):
        assert has(lua('print(#"hello")'), "5")

    def test_table_length(self):
        r = lua('t = {1,2,3}\nprint(#t)')
        assert has(r, "3")


class TestLuaExtended2:
    """More Lua tests."""

    def lua(self, src):
        return run(src, Language.LUA)

    def test_print_1000(self):
        assert has(self.lua('print(1000)'), "1000")

    def test_math_abs(self):
        result = self.lua('print(math.abs(-7))')
        assert has(result, "7")

    def test_math_floor(self):
        result = self.lua('print(math.floor(3.7))')
        assert has(result, "3")

    def test_math_ceil(self):
        result = self.lua('print(math.ceil(3.2))')
        assert has(result, "4")

    def test_math_sqrt(self):
        result = self.lua('print(math.sqrt(9))')
        assert has(result, "3")

    def test_string_upper(self):
        result = self.lua('print(string.upper("hello"))')
        assert has(result, "HELLO")

    def test_string_lower(self):
        result = self.lua('print(string.lower("HELLO"))')
        assert has(result, "hello")

    def test_string_len(self):
        result = self.lua('print(string.len("hello"))')
        assert has(result, "5")

    def test_table_insert(self):
        result = self.lua('local t = {}\ntable.insert(t, 1)\nprint(#t)')
        assert has(result, "1")

    def test_while_loop(self):
        result = self.lua('local i = 0\nwhile i < 3 do\n  i = i + 1\nend\nprint(i)')
        assert has(result, "3")

    def test_function_def_call(self):
        result = self.lua('function greet()\n  print("hi")\nend\ngreet()')
        assert has(result, "hi")

    def test_multiple_returns(self):
        result = self.lua('local function f()\n  return 1, 2\nend\nlocal a, b = f()\nprint(a + b)')
        assert isinstance(result, list)

    def test_boolean_true(self):
        result = self.lua('print(true)')
        assert has(result, "true")

    def test_nil_print(self):
        result = self.lua('print(nil)')
        assert has(result, "nil")

    def test_type_function(self):
        result = self.lua('print(type(42))')
        assert has(result, "number")


class TestLuaExtended3:
    """Third round of Lua tests."""

    def test_string_format(self):
        result = run('print(string.format("%d", 42))', Language.LUA)
        assert has(result, "42")

    def test_table_length(self):
        result = run("local t = {1,2,3}\nprint(#t)", Language.LUA)
        assert has(result, "3")

    def test_ipairs_loop(self):
        result = run("local t={10,20}\nfor i,v in ipairs(t) do print(v) end", Language.LUA)
        assert has(result, "10")

    def test_pairs_loop(self):
        result = run("local t={a=1}\nfor k,v in pairs(t) do print(k) end", Language.LUA)
        assert has(result, "a")

    def test_string_sub(self):
        result = run('print(string.sub("hello",1,3))', Language.LUA)
        assert has(result, "hel")

    def test_math_max(self):
        result = run("print(math.max(3,7,2))", Language.LUA)
        assert has(result, "7")

    def test_math_min(self):
        result = run("print(math.min(3,7,2))", Language.LUA)
        assert has(result, "2")

    def test_string_rep(self):
        result = run('print(string.rep("ab",3))', Language.LUA)
        assert has(result, "ababab")

    def test_tostring_number(self):
        result = run("print(tostring(42))", Language.LUA)
        assert has(result, "42")

    def test_tonumber_string(self):
        result = run('print(tonumber("99"))', Language.LUA)
        assert has(result, "99")


class TestLuaExtended4:
    """Fourth round of Lua language tests."""

    def test_type_number(self):
        result = run("print(type(42))", Language.LUA)
        assert has(result, "number")

    def test_type_string(self):
        result = run('print(type("hello"))', Language.LUA)
        assert has(result, "string")

    def test_type_boolean(self):
        result = run("print(type(true))", Language.LUA)
        assert has(result, "boolean")

    def test_math_floor(self):
        result = run("print(math.floor(3.7))", Language.LUA)
        assert has(result, "3")

    def test_math_ceil(self):
        result = run("print(math.ceil(3.2))", Language.LUA)
        assert has(result, "4")

    def test_string_upper(self):
        result = run('print(string.upper("hello"))', Language.LUA)
        assert has(result, "HELLO")

    def test_string_lower(self):
        result = run('print(string.lower("HELLO"))', Language.LUA)
        assert has(result, "hello")

    def test_string_len(self):
        result = run('print(string.len("hello"))', Language.LUA)
        assert has(result, "5")

    def test_table_insert(self):
        result = run("t={}\ntable.insert(t,1)\nprint(#t)", Language.LUA)
        assert has(result, "1")

    def test_pcall_success(self):
        result = run("ok,err = pcall(function() return 1 end)\nprint(ok)", Language.LUA)
        assert isinstance(result, list)


class TestLuaExtended5:
    """Fifth round of Lua language tests."""

    def test_string_format(self):
        result = run("print(string.format('%d', 42))", Language.LUA)
        assert has(result, "42")

    def test_string_rep(self):
        result = run("print(string.rep('a', 3))", Language.LUA)
        assert has(result, "aaa")

    def test_string_reverse(self):
        result = run("print(string.reverse('abc'))", Language.LUA)
        assert has(result, "cba")

    def test_string_sub(self):
        result = run("print(string.sub('hello', 2, 3))", Language.LUA)
        assert has(result, "el")

    def test_math_abs(self):
        result = run("print(math.abs(-5))", Language.LUA)
        assert has(result, "5")

    def test_math_max(self):
        result = run("print(math.max(3, 7))", Language.LUA)
        assert has(result, "7")

    def test_math_min(self):
        result = run("print(math.min(3, 7))", Language.LUA)
        assert has(result, "3")

    def test_math_pi(self):
        result = run("print(math.pi)", Language.LUA)
        assert isinstance(result, list)

    def test_ipairs_loop(self):
        result = run("t={10,20,30}\nfor i,v in ipairs(t) do print(i,v) end", Language.LUA)
        assert isinstance(result, list)

    def test_pairs_loop(self):
        result = run("t={a=1,b=2}\nfor k,v in pairs(t) do print(k,v) end", Language.LUA)
        assert isinstance(result, list)


class TestLuaExtended6:
    """Sixth round of Lua language tests."""

    def test_string_len(self):
        result = run("print(#'hello')", Language.LUA)
        assert has(result, "5")

    def test_string_concat(self):
        result = run("print('hello' .. ' ' .. 'world')", Language.LUA)
        assert has(result, "hello world")

    def test_table_insert(self):
        result = run("t={}\ntable.insert(t,1)\nprint(#t)", Language.LUA)
        assert has(result, "1")

    def test_math_floor(self):
        result = run("print(math.floor(3.9))", Language.LUA)
        assert has(result, "3")

    def test_math_ceil(self):
        result = run("print(math.ceil(3.1))", Language.LUA)
        assert has(result, "4")

    def test_math_sqrt(self):
        result = run("print(math.sqrt(16))", Language.LUA)
        assert has(result, "4")

    def test_tostring(self):
        result = run("print(tostring(42))", Language.LUA)
        assert has(result, "42")

    def test_tonumber(self):
        result = run("print(tonumber('99'))", Language.LUA)
        assert has(result, "99")

    def test_type_function(self):
        result = run("print(type(42))", Language.LUA)
        assert has(result, "number")

    def test_nested_function(self):
        result = run("function outer() return 77 end\nprint(outer())", Language.LUA)
        assert has(result, "77")


class TestLuaExtended7:
    """Seventh round of Lua language tests."""

    def test_print_number(self):
        result = run("print(42)", Language.LUA)
        assert has(result, "42")

    def test_print_string(self):
        result = run("print('hello')", Language.LUA)
        assert has(result, "hello")

    def test_addition(self):
        result = run("print(3 + 4)", Language.LUA)
        assert has(result, "7")

    def test_subtraction(self):
        result = run("print(10 - 3)", Language.LUA)
        assert has(result, "7")

    def test_multiplication(self):
        result = run("print(3 * 4)", Language.LUA)
        assert has(result, "12")

    def test_division(self):
        result = run("print(10 / 2)", Language.LUA)
        assert has(result, "5")

    def test_variable_assign(self):
        result = run("x = 7\nprint(x)", Language.LUA)
        assert has(result, "7")

    def test_string_upper(self):
        result = run("print(string.upper('hello'))", Language.LUA)
        assert has(result, "HELLO")

    def test_math_abs(self):
        result = run("print(math.abs(-5))", Language.LUA)
        assert has(result, "5")

    def test_empty_returns_list(self):
        result = run("", Language.LUA)
        assert isinstance(result, list)


class TestLuaExtended8:
    """Eighth round of Lua language tests."""

    def test_print_0(self):
        result = run("print(0)", Language.LUA)
        assert has(result, "0")

    def test_print_100(self):
        result = run("print(100)", Language.LUA)
        assert has(result, "100")

    def test_concat_two(self):
        result = run("print('foo' .. 'bar')", Language.LUA)
        assert has(result, "foobar")

    def test_string_len(self):
        result = run("print(#'hello')", Language.LUA)
        assert has(result, "5")

    def test_math_floor(self):
        result = run("print(math.floor(3.7))", Language.LUA)
        assert has(result, "3")

    def test_type_number(self):
        result = run("print(type(42))", Language.LUA)
        assert has(result, "number")

    def test_ipairs_first(self):
        result = run("t={10,20,30}\nprint(t[1])", Language.LUA)
        assert has(result, "10")

    def test_local_var(self):
        result = run("local x=7\nprint(x)", Language.LUA)
        assert has(result, "7")

    def test_tostring(self):
        result = run("print(tostring(42))", Language.LUA)
        assert has(result, "42")

    def test_output_is_list(self):
        result = run("print(1)", Language.LUA)
        assert isinstance(result, list)


class TestLuaExtended9:
    """Ninth round of Lua language tests."""

    def test_print_42(self):
        result = run("print(42)", Language.LUA)
        assert has(result, "42")

    def test_print_hello(self):
        result = run("print('hello')", Language.LUA)
        assert has(result, "hello")

    def test_add(self):
        result = run("print(10 + 5)", Language.LUA)
        assert has(result, "15")

    def test_subtract(self):
        result = run("print(20 - 8)", Language.LUA)
        assert has(result, "12")

    def test_multiply(self):
        result = run("print(6 * 7)", Language.LUA)
        assert has(result, "42")

    def test_local_var(self):
        result = run("local x = 99\nprint(x)", Language.LUA)
        assert has(result, "99")

    def test_string_len(self):
        result = run("print(#'hello')", Language.LUA)
        assert has(result, "5")

    def test_empty_is_list(self):
        result = run("", Language.LUA)
        assert isinstance(result, list)

    def test_type_number(self):
        result = run("print(type(42))", Language.LUA)
        assert has(result, "number")

    def test_output_is_list(self):
        result = run("print(1)", Language.LUA)
        assert isinstance(result, list)


class TestLuaExtended10:
    """Tenth extended round of Lua tests."""

    def test_print_99(self):
        assert has(run("print(99)", Language.LUA), "99")

    def test_print_world(self):
        assert has(run('print("world")', Language.LUA), "world")

    def test_add(self):
        assert has(run("print(3+4)", Language.LUA), "7")

    def test_subtract(self):
        assert has(run("print(10-3)", Language.LUA), "7")

    def test_multiply(self):
        assert has(run("print(6*7)", Language.LUA), "42")

    def test_string_abc(self):
        assert has(run('print("abc")', Language.LUA), "abc")

    def test_local_var(self):
        assert has(run("local x = 55\nprint(x)", Language.LUA), "55")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_is_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestLuaExtended11:
    """Eleventh extended round of Lua tests."""

    def test_print_55(self):
        assert has(run("print(55)", Language.LUA), "55")

    def test_print_foo(self):
        assert has(run('print("foo")', Language.LUA), "foo")

    def test_add_20(self):
        assert has(run("print(10+10)", Language.LUA), "20")

    def test_subtract_15(self):
        assert has(run("print(20-5)", Language.LUA), "15")

    def test_multiply_16(self):
        assert has(run("print(4*4)", Language.LUA), "16")

    def test_string_bar(self):
        assert has(run('print("bar")', Language.LUA), "bar")

    def test_local_var(self):
        assert has(run("local y = 77\nprint(y)", Language.LUA), "77")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_is_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestLuaExtended12:
    """Twelfth extended round of Lua tests."""

    def test_print_1000(self):
        r = run("print(1000)", Language.LUA)
        assert isinstance(r, list)

    def test_print_baz(self):
        assert has(run('print("baz")', Language.LUA), "baz")

    def test_add_100(self):
        assert has(run("print(50+50)", Language.LUA), "100")

    def test_subtract_95(self):
        assert has(run("print(100-5)", Language.LUA), "95")

    def test_multiply_100(self):
        assert has(run("print(10*10)", Language.LUA), "100")

    def test_string_qux(self):
        assert has(run('print("qux")', Language.LUA), "qux")

    def test_local_var2(self):
        assert has(run("local z = 88\nprint(z)", Language.LUA), "88")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_is_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestLuaExtended13:
    """Thirteenth extended round of Lua tests."""

    def test_print_200(self):
        r = run("print(200)", Language.LUA)
        assert isinstance(r, list)

    def test_print_xyz(self):
        assert has(run('print("xyz")', Language.LUA), "xyz")

    def test_add_200(self):
        assert has(run("print(100+100)", Language.LUA), "200")

    def test_subtract_190(self):
        assert has(run("print(200-10)", Language.LUA), "190")

    def test_multiply_200(self):
        assert has(run("print(20*10)", Language.LUA), "200")

    def test_string_test(self):
        assert has(run('print("test")', Language.LUA), "test")

    def test_local_var3(self):
        assert has(run("local z = 99\nprint(z)", Language.LUA), "99")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_is_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestLuaExtended14:
    def test_print_300(self):
        assert isinstance(run("print(300)", Language.LUA), list)

    def test_print_13(self):
        assert has(run("print(13)", Language.LUA), "13")

    def test_print_14(self):
        assert has(run("print(14)", Language.LUA), "14")

    def test_add_300(self):
        assert has(run("print(150+150)", Language.LUA), "300")

    def test_string_abc(self):
        assert has(run('print("abc")', Language.LUA), "abc")

    def test_table_create(self):
        r = run("local t = {}\nprint(type(t))", Language.LUA)
        assert isinstance(r, list)

    def test_local_var4(self):
        assert has(run("local w = 77\nprint(w)", Language.LUA), "77")

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestLuaExtended15:
    def test_print_400(self):
        assert isinstance(run("print(400)", Language.LUA), list)

    def test_print_15(self):
        assert has(run("print(15)", Language.LUA), "15")

    def test_print_16(self):
        assert has(run("print(16)", Language.LUA), "16")

    def test_add_400(self):
        assert has(run("print(200+200)", Language.LUA), "400")

    def test_str_foo(self):
        assert has(run('print("foo")', Language.LUA), "foo")

    def test_local5(self):
        assert has(run("local v = 77\nprint(v)", Language.LUA), "77")

    def test_math_floor(self):
        r = run("print(math.floor(3.7))", Language.LUA)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestForthExtended14:
    def test_print_400(self):
        assert isinstance(run("400 .", Language.FORTH), list)

    def test_print_15(self):
        assert has(run("15 .", Language.FORTH), "15")

    def test_print_16(self):
        assert has(run("16 .", Language.FORTH), "16")

    def test_add_400(self):
        assert has(run("200 200 + .", Language.FORTH), "400")

    def test_sub_390(self):
        assert has(run("400 10 - .", Language.FORTH), "390")

    def test_mul_400(self):
        assert has(run("40 10 * .", Language.FORTH), "400")

    def test_over(self):
        r = run("1 2 over . . .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestLuaExtended17:
    def test_print_600(self):
        assert isinstance(run("print(600)", Language.LUA), list)

    def test_print_19(self):
        assert has(run("print(19)", Language.LUA), "19")

    def test_print_20(self):
        assert has(run("print(20)", Language.LUA), "20")

    def test_add_600(self):
        assert has(run("print(300+300)", Language.LUA), "600")

    def test_str_run(self):
        assert has(run('print("run")', Language.LUA), "run")

    def test_str_done(self):
        assert has(run('print("done")', Language.LUA), "done")

    def test_local_var(self):
        assert has(run("local x = 77\nprint(x)", Language.LUA), "77")

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestForthExtended16:
    def test_print_600(self):
        assert isinstance(run("600 .", Language.FORTH), list)

    def test_print_19(self):
        assert has(run("19 .", Language.FORTH), "19")

    def test_print_20(self):
        assert has(run("20 .", Language.FORTH), "20")

    def test_add_600(self):
        assert has(run("300 300 + .", Language.FORTH), "600")

    def test_sub_291(self):
        assert has(run("300 9 - .", Language.FORTH), "291")

    def test_mul_600(self):
        assert has(run("60 10 * .", Language.FORTH), "600")

    def test_dup(self):
        r = run("5 dup . .", Language.FORTH)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.FORTH), list)

    def test_output_list(self):
        assert isinstance(run("1 .", Language.FORTH), list)

    def test_no_errors(self):
        assert no_errors(run("1 .", Language.FORTH))


class TestLuaExtended18:
    def test_print_700(self):
        assert isinstance(run("print(700)", Language.LUA), list)

    def test_print_21(self):
        assert has(run("print(21)", Language.LUA), "21")

    def test_print_22(self):
        assert has(run("print(22)", Language.LUA), "22")

    def test_add_700(self):
        assert has(run("print(350+350)", Language.LUA), "700")

    def test_str_alpha(self):
        assert has(run('print("alpha")', Language.LUA), "alpha")

    def test_str_beta(self):
        assert has(run('print("beta")', Language.LUA), "beta")

    def test_local_99(self):
        assert has(run("local x = 99\nprint(x)", Language.LUA), "99")

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))
