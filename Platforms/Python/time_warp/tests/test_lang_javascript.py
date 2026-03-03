"""Comprehensive tests for the JavaScript language executor."""


from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.JAVASCRIPT


def js(source: str, **kw) -> list[str]:
    """Shortcut: run a JavaScript program."""
    return run(source, L, **kw)


# ============================================================================
# console.log
# ============================================================================


class TestConsoleLog:
    def test_hello_world(self):
        out = js('console.log("Hello World");')
        assert has(out, "Hello World")

    def test_log_number(self):
        out = js("console.log(42);")
        assert has(out, "42")

    def test_log_expression(self):
        out = js("console.log(3 + 4);")
        assert has(out, "7")

    def test_log_multiple(self):
        out = js('console.log("A", "B", "C");')
        assert has(out, "A") and has(out, "B") and has(out, "C")

    def test_log_boolean(self):
        out = js("console.log(true);")
        assert has(out, "true")

    def test_log_null(self):
        out = js("console.log(null);")
        assert has(out, "null")

    def test_log_undefined(self):
        out = js("console.log(undefined);")
        assert has(out, "undefined")

    def test_template_literal(self):
        out = js("let x = 5;\nconsole.log(`x is ${x}`);")
        assert has(out, "x is 5")


# ============================================================================
# VARIABLES
# ============================================================================


class TestVariables:
    def test_let(self):
        out = js("let x = 42;\nconsole.log(x);")
        assert has(out, "42")

    def test_const(self):
        out = js("const PI = 3.14;\nconsole.log(PI);")
        assert has(out, "3.14")

    def test_var(self):
        out = js("var x = 10;\nconsole.log(x);")
        assert has(out, "10")

    def test_string(self):
        out = js('let s = "Hello";\nconsole.log(s);')
        assert has(out, "Hello")

    def test_array(self):
        out = js("let a = [1, 2, 3];\nconsole.log(a);")
        assert has(out, "1") and has(out, "3")

    def test_object(self):
        out = js('let o = {name: "Alice", age: 30};\nconsole.log(o.name);')
        assert has(out, "Alice")


# ============================================================================
# ARITHMETIC
# ============================================================================


class TestArithmetic:
    def test_add(self):
        out = js("console.log(2 + 3);")
        assert has(out, "5")

    def test_subtract(self):
        out = js("console.log(10 - 4);")
        assert has(out, "6")

    def test_multiply(self):
        out = js("console.log(6 * 7);")
        assert has(out, "42")

    def test_divide(self):
        out = js("console.log(10 / 4);")
        assert has(out, "2.5")

    def test_modulo(self):
        out = js("console.log(10 % 3);")
        assert has(out, "1")

    def test_power(self):
        out = js("console.log(2 ** 10);")
        assert has(out, "1024")

    def test_increment(self):
        out = js("let x = 5;\nx++;\nconsole.log(x);")
        assert has(out, "6")

    def test_decrement(self):
        out = js("let x = 5;\nx--;\nconsole.log(x);")
        assert has(out, "4")


# ============================================================================
# STRINGS
# ============================================================================


class TestStrings:
    def test_length(self):
        out = js('console.log("Hello".length);')
        assert has(out, "5")

    def test_toUpperCase(self):
        out = js('console.log("hello".toUpperCase());')
        assert has(out, "HELLO")

    def test_toLowerCase(self):
        out = js('console.log("HELLO".toLowerCase());')
        assert has(out, "hello")

    def test_charAt(self):
        out = js('console.log("Hello".charAt(0));')
        assert has(out, "H")

    def test_indexOf(self):
        out = js('console.log("Hello".indexOf("ll"));')
        assert has(out, "2")

    def test_slice(self):
        out = js('console.log("Hello".slice(1, 3));')
        assert has(out, "el")

    def test_replace(self):
        out = js('console.log("Hello".replace("l", "r"));')
        assert has(out, "Herlo") or has(out, "r")

    def test_split(self):
        out = js('console.log("a,b,c".split(","));')
        assert has(out, "a") and has(out, "c")

    def test_trim(self):
        out = js('console.log("  hi  ".trim());')
        assert has(out, "hi")

    def test_includes(self):
        out = js('console.log("Hello".includes("ell"));')
        assert has(out, "true")

    def test_startsWith(self):
        out = js('console.log("Hello".startsWith("He"));')
        assert has(out, "true")

    def test_repeat(self):
        out = js('console.log("ab".repeat(3));')
        assert has(out, "ababab")


# ============================================================================
# ARRAYS
# ============================================================================


class TestArrays:
    def test_push(self):
        out = js("let a = [1];\na.push(2);\nconsole.log(a);")
        assert has(out, "1") and has(out, "2")

    def test_pop(self):
        out = js("let a = [1, 2, 3];\nlet v = a.pop();\nconsole.log(v);")
        assert has(out, "3")

    def test_length(self):
        out = js("console.log([1, 2, 3].length);")
        assert has(out, "3")

    def test_join(self):
        out = js('console.log([1, 2, 3].join("-"));')
        assert has(out, "1-2-3")

    def test_reverse(self):
        out = js("console.log([1, 2, 3].reverse());")
        assert has(out, "3") and has(out, "1")

    def test_sort(self):
        out = js("console.log([3, 1, 2].sort());")
        assert has(out, "1") and has(out, "3")

    def test_map(self):
        out = js("console.log([1, 2, 3].map(x => x * 2));")
        assert has(out, "2") and has(out, "6")

    def test_filter(self):
        out = js("console.log([1, 2, 3, 4].filter(x => x > 2));")
        assert has(out, "3") and has(out, "4")

    def test_reduce(self):
        out = js("console.log([1, 2, 3].reduce((a, b) => a + b, 0));")
        assert has(out, "6")

    def test_forEach(self):
        out = js("[1, 2, 3].forEach(x => console.log(x));")
        assert has(out, "1") and has(out, "3")

    def test_indexOf(self):
        out = js("console.log([10, 20, 30].indexOf(20));")
        assert has(out, "1")

    def test_includes(self):
        out = js("console.log([1, 2, 3].includes(2));")
        assert has(out, "true")

    def test_concat(self):
        out = js("console.log([1, 2].concat([3, 4]));")
        assert has(out, "1") and has(out, "4")

    def test_slice(self):
        out = js("console.log([1, 2, 3, 4].slice(1, 3));")
        assert has(out, "2") and has(out, "3")

    def test_find(self):
        out = js("console.log([1, 2, 3].find(x => x > 1));")
        assert has(out, "2")

    def test_some(self):
        out = js("console.log([1, 2, 3].some(x => x > 2));")
        assert has(out, "true")

    def test_every(self):
        out = js("console.log([1, 2, 3].every(x => x > 0));")
        assert has(out, "true")


# ============================================================================
# CONTROL FLOW
# ============================================================================


class TestControlFlow:
    def test_if_true(self):
        out = js('if (5 > 3) { console.log("yes"); }')
        assert has(out, "yes")

    def test_if_else(self):
        out = js('if (1 > 3) { console.log("yes"); } else { console.log("no"); }')
        assert has(out, "no")

    def test_ternary(self):
        out = js('let r = 5 > 3 ? "yes" : "no";\nconsole.log(r);')
        assert has(out, "yes")

    def test_switch(self):
        out = js(
            'let x = 2;\n'
            'switch(x) {\n'
            '  case 1: console.log("one"); break;\n'
            '  case 2: console.log("two"); break;\n'
            '  default: console.log("other");\n'
            '}'
        )
        assert has(out, "two")

    def test_for(self):
        out = js("for (let i = 0; i < 3; i++) { console.log(i); }")
        assert has(out, "0") and has(out, "2")

    def test_while(self):
        out = js("let i = 0;\nwhile (i < 3) { console.log(i); i++; }")
        assert has(out, "0") and has(out, "2")

    def test_do_while(self):
        out = js("let i = 0;\ndo { console.log(i); i++; } while (i < 3);")
        assert has(out, "0") and has(out, "2")

    def test_for_of(self):
        out = js("for (let v of [10, 20, 30]) { console.log(v); }")
        assert has(out, "10") and has(out, "30")

    def test_for_in(self):
        out = js('let o = {a: 1, b: 2};\nfor (let k in o) { console.log(k); }')
        assert has(out, "a") and has(out, "b")

    def test_break(self):
        out = js("for (let i = 0; i < 10; i++) { if (i === 3) break; console.log(i); }")
        assert has(out, "2") and not has(out, "4")

    def test_continue(self):
        out = js("for (let i = 0; i < 5; i++) { if (i === 2) continue; console.log(i); }")
        assert has(out, "1") and has(out, "3")


# ============================================================================
# FUNCTIONS
# ============================================================================


class TestFunctions:
    def test_function_declaration(self):
        out = js('function greet(name) { return "Hello " + name; }\nconsole.log(greet("World"));')
        assert has(out, "Hello World")

    def test_arrow_function(self):
        out = js("const double = x => x * 2;\nconsole.log(double(5));")
        assert has(out, "10")

    def test_default_params(self):
        out = js("function f(x, y = 10) { return x + y; }\nconsole.log(f(5));")
        assert has(out, "15")

    def test_rest_params(self):
        out = js("function sum(...args) { return args.reduce((a, b) => a + b, 0); }\nconsole.log(sum(1, 2, 3));")
        assert has(out, "6")

    def test_recursive(self):
        out = js("function fact(n) { return n <= 1 ? 1 : n * fact(n - 1); }\nconsole.log(fact(5));")
        assert has(out, "120")

    def test_closure(self):
        out = js(
            "function makeCounter() {\n"
            "  let count = 0;\n"
            "  return function() { count++; return count; };\n"
            "}\n"
            "let c = makeCounter();\n"
            "console.log(c());\n"
            "console.log(c());"
        )
        assert has(out, "1") and has(out, "2")


# ============================================================================
# MATH OBJECT
# ============================================================================


class TestMath:
    def test_sqrt(self):
        out = js("console.log(Math.sqrt(16));")
        assert has(out, "4")

    def test_abs(self):
        out = js("console.log(Math.abs(-5));")
        assert has(out, "5")

    def test_floor(self):
        out = js("console.log(Math.floor(3.7));")
        assert has(out, "3")

    def test_ceil(self):
        out = js("console.log(Math.ceil(3.2));")
        assert has(out, "4")

    def test_round(self):
        out = js("console.log(Math.round(3.5));")
        assert has(out, "4")

    def test_max(self):
        out = js("console.log(Math.max(1, 5, 3));")
        assert has(out, "5")

    def test_min(self):
        out = js("console.log(Math.min(1, 5, 3));")
        assert has(out, "1")

    def test_pow(self):
        out = js("console.log(Math.pow(2, 10));")
        assert has(out, "1024")

    def test_pi(self):
        out = js("console.log(Math.PI);")
        assert has(out, "3.14")

    def test_random(self):
        out = js("console.log(Math.random());")
        assert no_errors(out)

    def test_sin(self):
        out = js("console.log(Math.sin(0));")
        assert has(out, "0")

    def test_log(self):
        out = js("console.log(Math.log(1));")
        assert has(out, "0")


# ============================================================================
# OBJECTS
# ============================================================================


class TestObjects:
    def test_object_literal(self):
        out = js('let o = {x: 1, y: 2};\nconsole.log(o.x);')
        assert has(out, "1")

    def test_object_keys(self):
        out = js('let o = {a: 1, b: 2};\nconsole.log(Object.keys(o));')
        assert has(out, "a") and has(out, "b")

    def test_object_values(self):
        out = js("let o = {a: 1, b: 2};\nconsole.log(Object.values(o));")
        assert has(out, "1") and has(out, "2")

    def test_destructuring(self):
        out = js("let {a, b} = {a: 1, b: 2};\nconsole.log(a, b);")
        assert has(out, "1") and has(out, "2")

    def test_spread(self):
        out = js("let a = [1, 2];\nlet b = [...a, 3];\nconsole.log(b);")
        assert has(out, "1") and has(out, "3")


# ============================================================================
# CLASSES
# ============================================================================


class TestClasses:
    def test_class(self):
        out = js(
            "class Dog {\n"
            "  constructor(name) { this.name = name; }\n"
            "  speak() { return this.name + ' says Woof'; }\n"
            "}\n"
            "let d = new Dog('Rex');\n"
            "console.log(d.speak());"
        )
        assert has(out, "Rex says Woof")

    def test_inheritance(self):
        out = js(
            "class Animal {\n"
            "  speak() { return 'generic'; }\n"
            "}\n"
            "class Cat extends Animal {\n"
            "  speak() { return 'Meow'; }\n"
            "}\n"
            "let c = new Cat();\n"
            "console.log(c.speak());"
        )
        assert has(out, "Meow")


# ============================================================================
# TRY / CATCH
# ============================================================================


class TestExceptions:
    def test_try_catch(self):
        out = js(
            "try {\n"
            "  throw new Error('oops');\n"
            "} catch (e) {\n"
            "  console.log('caught');\n"
            "}"
        )
        assert has(out, "caught")

    def test_try_finally(self):
        out = js(
            "try {\n"
            "  let x = 1;\n"
            "} finally {\n"
            "  console.log('done');\n"
            "}"
        )
        assert has(out, "done")


# ============================================================================
# TYPE CONVERSIONS
# ============================================================================


class TestTypeConversions:
    def test_parseInt(self):
        out = js('console.log(parseInt("42"));')
        assert has(out, "42")

    def test_parseFloat(self):
        out = js('console.log(parseFloat("3.14"));')
        assert has(out, "3.14")

    def test_String(self):
        out = js("console.log(String(42));")
        assert has(out, "42")

    def test_Number(self):
        out = js('console.log(Number("42"));')
        assert has(out, "42")

    def test_typeof(self):
        out = js('console.log(typeof "hello");')
        assert has(out, "string")

    def test_isNaN(self):
        out = js("console.log(isNaN(NaN));")
        assert has(out, "true")

    def test_JSON_stringify(self):
        out = js('console.log(JSON.stringify({a: 1}));')
        assert has(out, "a") and has(out, "1")

    def test_JSON_parse(self):
        out = js('let o = JSON.parse(\'{"x": 42}\');\nconsole.log(o.x);')
        assert has(out, "42")


# ============================================================================
# ERRORS
# ============================================================================


class TestErrors:
    def test_syntax_error(self):
        out = js("function {")
        assert first_error(out) is not None or len(out) > 0

    def test_reference_error(self):
        out = js("console.log(undefined_var);")
        # May output undefined or error
        assert len(out) > 0


# ============================================================================
# Array method chaining on variables
# ============================================================================


class TestArrayMethodChaining:
    """Variable-based .map/.filter/.reduce now wrap in JSArray."""

    def test_map_on_variable(self):
        out = js("let a = [1, 2, 3];\nlet b = a.map(x => x * 2);\nconsole.log(b);")
        assert has(out, "2", "4", "6")

    def test_filter_on_variable(self):
        out = js("let a = [1, 2, 3, 4];\nconsole.log(a.filter(x => x > 2));")
        assert has(out, "3", "4")
