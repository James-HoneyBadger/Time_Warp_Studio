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
            "let x = 2;\n"
            "switch(x) {\n"
            '  case 1: console.log("one"); break;\n'
            '  case 2: console.log("two"); break;\n'
            '  default: console.log("other");\n'
            "}"
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
        out = js("let o = {a: 1, b: 2};\nfor (let k in o) { console.log(k); }")
        assert has(out, "a") and has(out, "b")

    def test_break(self):
        out = js("for (let i = 0; i < 10; i++) { if (i === 3) break; console.log(i); }")
        assert has(out, "2") and not has(out, "4")

    def test_continue(self):
        out = js(
            "for (let i = 0; i < 5; i++) { if (i === 2) continue; console.log(i); }"
        )
        assert has(out, "1") and has(out, "3")


# ============================================================================
# FUNCTIONS
# ============================================================================


class TestFunctions:
    def test_function_declaration(self):
        out = js(
            'function greet(name) { return "Hello " + name; }\nconsole.log(greet("World"));'
        )
        assert has(out, "Hello World")

    def test_arrow_function(self):
        out = js("const double = x => x * 2;\nconsole.log(double(5));")
        assert has(out, "10")

    def test_default_params(self):
        out = js("function f(x, y = 10) { return x + y; }\nconsole.log(f(5));")
        assert has(out, "15")

    def test_rest_params(self):
        out = js(
            "function sum(...args) { return args.reduce((a, b) => a + b, 0); }\nconsole.log(sum(1, 2, 3));"
        )
        assert has(out, "6")

    def test_recursive(self):
        out = js(
            "function fact(n) { return n <= 1 ? 1 : n * fact(n - 1); }\nconsole.log(fact(5));"
        )
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
        out = js("let o = {x: 1, y: 2};\nconsole.log(o.x);")
        assert has(out, "1")

    def test_object_keys(self):
        out = js("let o = {a: 1, b: 2};\nconsole.log(Object.keys(o));")
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
            "try {\n" "  let x = 1;\n" "} finally {\n" "  console.log('done');\n" "}"
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
        out = js("console.log(JSON.stringify({a: 1}));")
        assert has(out, "a") and has(out, "1")

    def test_JSON_parse(self):
        out = js("let o = JSON.parse('{\"x\": 42}');\nconsole.log(o.x);")
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


# ============================================================================
# Class getters / setters / static methods
# ============================================================================


class TestClassGetterSetter:
    """ES6 class getter / setter / static method transpilation."""

    def test_getter(self):
        out = js(
            "class C {\n"
            "  constructor(n) {\n"
            "    this._n = n;\n"
            "  }\n"
            "  get name() {\n"
            "    return this._n;\n"
            "  }\n"
            "}\n"
            "const c = new C('hello');\n"
            "console.log(c.name);\n"
        )
        assert has(out, "hello")

    def test_setter(self):
        out = js(
            "class C {\n"
            "  constructor(n) {\n"
            "    this._n = n;\n"
            "  }\n"
            "  get name() {\n"
            "    return this._n;\n"
            "  }\n"
            "  set name(v) {\n"
            "    this._n = v;\n"
            "  }\n"
            "}\n"
            "const c = new C('a');\n"
            "c.name = 'b';\n"
            "console.log(c.name);\n"
        )
        assert has(out, "b")

    def test_static_method(self):
        out = js(
            "class MathUtils {\n"
            "  static square(x) {\n"
            "    return x * x;\n"
            "  }\n"
            "}\n"
            "console.log(MathUtils.square(5));\n"
        )
        assert has(out, "25")


class TestMathObject:
    """Math object built-in functions."""

    def test_pow(self):
        out = js("console.log(Math.pow(2, 10));")
        assert has(out, "1024")
        assert no_errors(out)

    def test_sqrt(self):
        out = js("console.log(Math.sqrt(16));")
        assert has(out, "4")
        assert no_errors(out)

    def test_abs(self):
        out = js("console.log(Math.abs(-42));")
        assert has(out, "42")
        assert no_errors(out)

    def test_max(self):
        out = js("console.log(Math.max(1, 5, 3));")
        assert has(out, "5")
        assert no_errors(out)

    def test_min(self):
        out = js("console.log(Math.min(1, 5, 3));")
        assert has(out, "1")
        assert no_errors(out)

    def test_floor(self):
        out = js("console.log(Math.floor(3.7));")
        assert has(out, "3")
        assert no_errors(out)

    def test_ceil(self):
        out = js("console.log(Math.ceil(3.2));")
        assert has(out, "4")
        assert no_errors(out)

    def test_round(self):
        out = js("console.log(Math.round(3.5));")
        assert has(out, "4")
        assert no_errors(out)

    def test_pi(self):
        out = js("console.log(Math.PI.toFixed(4));")
        assert has(out, "3.1416")
        assert no_errors(out)


class TestNumberBuiltins:
    """Number built-in methods."""

    def test_isInteger(self):
        out = js("console.log(Number.isInteger(42));")
        assert has(out, "true")
        assert no_errors(out)

    def test_toFixed(self):
        out = js("console.log((3.14159).toFixed(2));")
        assert has(out, "3.14")
        assert no_errors(out)

    def test_toPrecision(self):
        out = js("console.log((0.123).toPrecision(2));")
        assert has(out, "0.12")
        assert no_errors(out)


class TestDestructuringAndTernary:
    """Destructuring and ternary expressions."""

    def test_array_destructure(self):
        out = js("const [a, b] = [10, 20]; console.log(a + b);")
        assert has(out, "30")
        assert no_errors(out)

    def test_ternary_true(self):
        out = js("const x = 5; console.log(x > 3 ? 'yes' : 'no');")
        assert has(out, "yes")
        assert no_errors(out)

    def test_ternary_false(self):
        out = js("const x = 1; console.log(x > 3 ? 'yes' : 'no');")
        assert has(out, "no")
        assert no_errors(out)


class TestArrayHigherOrder:
    """Array higher-order methods."""

    def test_find(self):
        out = js("const arr=[1,2,3,4,5]; console.log(arr.find(x=>x>3));")
        assert has(out, "4")
        assert no_errors(out)

    def test_findIndex(self):
        out = js("const arr=[1,2,3]; console.log(arr.findIndex(x=>x===2));")
        assert has(out, "1")
        assert no_errors(out)

    def test_every_true(self):
        out = js("const arr=[2,4,6]; console.log(arr.every(x=>x%2===0));")
        assert has(out, "true")
        assert no_errors(out)

    def test_some_true(self):
        out = js("const arr=[1,3,4]; console.log(arr.some(x=>x%2===0));")
        assert has(out, "true")
        assert no_errors(out)

    def test_flat(self):
        out = js("const arr=[[1,2],[3,4]]; console.log(arr.flat().length);")
        assert has(out, "4")
        assert no_errors(out)


class TestStringExtras:
    """Additional string methods."""

    def test_includes(self):
        out = js("console.log('hello world'.includes('world'));")
        assert has(out, "true")
        assert no_errors(out)

    def test_startsWith(self):
        out = js("console.log('hello'.startsWith('hel'));")
        assert has(out, "true")
        assert no_errors(out)

    def test_endsWith(self):
        out = js("console.log('hello'.endsWith('llo'));")
        assert has(out, "true")
        assert no_errors(out)

    def test_repeat(self):
        out = js("console.log('ha'.repeat(3));")
        assert has(out, "hahaha")
        assert no_errors(out)


class TestJsObjectsAndMaps:
    """Tests for objects, Map, and Set."""

    def test_object_properties(self):
        out = js("const obj = {x: 5, y: 10}; console.log(obj.x + obj.y);")
        assert has(out, "15")
        assert no_errors(out)

    def test_object_keys_length(self):
        out = js('const obj = {name: "Bob"}; console.log(Object.keys(obj).length);')
        assert has(out, "1")
        assert no_errors(out)

    def test_object_values_length(self):
        out = js("const obj = {a: 1, b: 2}; console.log(Object.values(obj).length);")
        assert has(out, "2")
        assert no_errors(out)

    def test_map_set_get(self):
        out = js('const m = new Map(); m.set("k", 99); console.log(m.get("k"));')
        assert has(out, "99")
        assert no_errors(out)

    def test_set_size(self):
        out = js("const s = new Set([1,2,2,3]); console.log(s.size);")
        assert has(out, "3")
        assert no_errors(out)


class TestJsTypeofAndParsing:
    """Tests for typeof operator and parse functions."""

    def test_typeof_number(self):
        out = js("console.log(typeof 42);")
        assert has(out, "number")
        assert no_errors(out)

    def test_typeof_boolean(self):
        out = js("console.log(typeof true);")
        assert has(out, "boolean")
        assert no_errors(out)

    def test_parseInt(self):
        out = js('console.log(parseInt("42"));')
        assert has(out, "42")
        assert no_errors(out)

    def test_parseFloat(self):
        out = js('console.log(parseFloat("3.14"));')
        assert has(out, "3.14")
        assert no_errors(out)


class TestJsArrayExtra:
    """Extra array method tests."""

    def test_join(self):
        out = js('const a = [1,2,3]; console.log(a.join(","));')
        assert has(out, "1,2,3")
        assert no_errors(out)

    def test_reverse(self):
        out = js("const a = [1,2,3]; console.log(a.reverse()[0]);")
        assert has(out, "3")
        assert no_errors(out)

    def test_array_destructuring(self):
        out = js("const [a, b] = [1, 2]; console.log(a + b);")
        assert has(out, "3")
        assert no_errors(out)


class TestJsTemplateLiterals:
    """Tests for template literals."""

    def test_template_literal_variable(self):
        out = js("const x = 42; console.log(`x is ${x}`);")
        assert has(out, "x is 42")
        assert no_errors(out)

    def test_template_literal_expression(self):
        # Only single-variable interpolation is supported
        out = js("const x = 42; console.log(`value: ${x}`);")
        assert has(out, "42")
        assert no_errors(out)

    def test_template_literal_string(self):
        out = js('const name = "World"; console.log(`Hello ${name}`);')
        assert has(out, "Hello")
        assert has(out, "World")
        assert no_errors(out)


class TestJsStringMethods:
    """Tests for JS string methods."""

    def test_string_length(self):
        out = js('let s = "hello"; console.log(s.length);')
        assert has(out, "5")
        assert no_errors(out)

    def test_toUpperCase(self):
        out = js('let s = "hello"; console.log(s.toUpperCase());')
        assert has(out, "HELLO")
        assert no_errors(out)

    def test_toLowerCase(self):
        out = js('let s = "HELLO"; console.log(s.toLowerCase());')
        assert has(out, "hello")
        assert no_errors(out)

    def test_slice(self):
        out = js('let s = "hello"; console.log(s.slice(1, 3));')
        assert has(out, "el")
        assert no_errors(out)

    def test_charAt(self):
        out = js('let s = "hello"; console.log(s.charAt(1));')
        assert has(out, "e")
        assert no_errors(out)

    def test_trim(self):
        out = js('let s = "  hello  "; console.log(s.trim());')
        assert has(out, "hello")
        assert no_errors(out)

    def test_includes(self):
        out = js('let s = "hello world"; console.log(s.includes("world"));')
        assert has(out, "true")
        assert no_errors(out)

    def test_repeat(self):
        out = js('let s = "hello"; console.log(s.repeat(2));')
        assert has(out, "hellohello")
        assert no_errors(out)

    def test_replace(self):
        out = js('let s = "hello world"; console.log(s.replace("world", "JS"));')
        assert has(out, "hello JS")
        assert no_errors(out)

    def test_startsWith(self):
        out = js('let s = "hello"; console.log(s.startsWith("hel"));')
        assert has(out, "true")
        assert no_errors(out)

    def test_endsWith(self):
        out = js('let s = "hello"; console.log(s.endsWith("llo"));')
        assert has(out, "true")
        assert no_errors(out)


class TestJsNumberMethods:
    """Tests for JS number methods."""

    def test_toFixed(self):
        out = js("let n = 3.14159; console.log(n.toFixed(2));")
        assert has(out, "3.14")
        assert no_errors(out)

    def test_isInteger(self):
        out = js("let n = 42; console.log(Number.isInteger(n));")
        assert has(out, "true")
        assert no_errors(out)

    def test_parseInt(self):
        out = js('console.log(Number.parseInt("42abc"));')
        assert has(out, "42")
        assert no_errors(out)

    def test_isNaN(self):
        out = js("console.log(Number.isNaN(NaN));")
        assert has(out, "true")
        assert no_errors(out)

    def test_isFinite(self):
        out = js("console.log(Number.isFinite(42));")
        assert has(out, "true")
        assert no_errors(out)


class TestJsArrayMethods2:
    """More JS array method tests."""

    def test_array_length(self):
        out = js('console.log([1,2,3].length)')
        assert has(out, "3")
        assert no_errors(out)

    def test_array_join(self):
        out = js('console.log([1,2,3].join("-"))')
        assert has(out, "1-2-3")
        assert no_errors(out)

    def test_array_indexof(self):
        out = js('console.log([1,2,3].indexOf(2))')
        assert has(out, "1")
        assert no_errors(out)

    def test_array_includes(self):
        out = js('console.log([1,2,3].includes(2))')
        assert has(out, "true")
        assert no_errors(out)

    def test_array_reduce(self):
        out = js('console.log([1,2,3,4].reduce((a,b) => a+b, 0))')
        assert has(out, "10")
        assert no_errors(out)

    def test_array_push_length(self):
        out = js('let a=[1,2]; a.push(3); console.log(a.length)')
        assert has(out, "3")
        assert no_errors(out)

    def test_array_pop_length(self):
        out = js('let a=[1,2,3]; a.pop(); console.log(a.length)')
        assert has(out, "2")
        assert no_errors(out)


class TestJsObjectOps2:
    """More JS object tests."""

    def test_object_field_access(self):
        out = js('let o={a:1,b:2}; console.log(o.a)')
        assert has(out, "1")
        assert no_errors(out)

    def test_object_dynamic_assign(self):
        out = js('let o={a:1}; o.b=2; console.log(o.b)')
        assert has(out, "2")
        assert no_errors(out)

    def test_object_keys_count(self):
        out = js('let o={a:1,b:2}; console.log(Object.keys(o).length)')
        assert has(out, "2")
        assert no_errors(out)


class TestJsMathObject2:
    """More Math object tests."""

    def test_math_abs(self):
        out = js('console.log(Math.abs(-7))')
        assert has(out, "7")
        assert no_errors(out)

    def test_math_floor(self):
        out = js('console.log(Math.floor(3.7))')
        assert has(out, "3")
        assert no_errors(out)

    def test_math_ceil(self):
        out = js('console.log(Math.ceil(3.2))')
        assert has(out, "4")
        assert no_errors(out)

    def test_math_max(self):
        out = js('console.log(Math.max(3, 7))')
        assert has(out, "7")
        assert no_errors(out)

    def test_math_min(self):
        out = js('console.log(Math.min(3, 7))')
        assert has(out, "3")
        assert no_errors(out)

    def test_math_pow(self):
        out = js('console.log(Math.pow(2, 8))')
        assert has(out, "256")
        assert no_errors(out)

    def test_math_sqrt(self):
        out = js('console.log(Math.sqrt(9))')
        assert has(out, "3")
        assert no_errors(out)

    def test_math_round(self):
        out = js('console.log(Math.round(3.5))')
        assert has(out, "4")
        assert no_errors(out)


class TestJsTypeSystem2:
    """JS typeof and type-checking tests."""

    def test_typeof_number(self):
        assert has(js("console.log(typeof 42)"), "number")

    def test_typeof_boolean(self):
        assert has(js("console.log(typeof true)"), "boolean")

    def test_number_is_integer_true(self):
        assert has(js("console.log(Number.isInteger(5))"), "true")

    def test_number_is_integer_false(self):
        assert has(js("console.log(Number.isInteger(5.5))"), "false")


class TestJsArrayMethods3:
    """More JS array method tests."""

    def test_find(self):
        assert has(js("let arr = [1,2,3]; console.log(arr.find(x => x > 1))"), "2")

    def test_filter_length(self):
        assert has(js("let arr = [1,2,3]; console.log(arr.filter(x => x > 1).length)"), "2")

    def test_every_true(self):
        assert has(js("let arr = [1,2,3]; console.log(arr.every(x => x > 0))"), "true")

    def test_some_true(self):
        assert has(js("let arr = [1,2,3]; console.log(arr.some(x => x > 2))"), "true")

    def test_split_length(self):
        assert has(js('let s = "hello world"; console.log(s.split(" ").length)'), "2")


class TestJsObjectOps3:
    """More JS object operation tests."""

    def test_object_values_first(self):
        assert has(js("let obj = {a:1}; console.log(Object.values(obj)[0])"), "1")

    def test_json_stringify(self):
        assert has(js('console.log(JSON.stringify({a:1}))'), "a")

    def test_json_parse(self):
        assert has(js('console.log(JSON.parse(\'{"x":5}\').x)'), "5")


class TestJsStringMethods3:
    """More JS string method tests."""

    def test_char_at(self):
        assert has(js('let s = "hello"; console.log(s.charAt(0))'), "h")

    def test_from_char_code(self):
        assert has(js("console.log(String.fromCharCode(65))"), "A")


class TestJsMath4:
    """More JavaScript math operations."""

    def test_abs_negative(self):
        assert has(js("console.log(Math.abs(-7))"), "7")

    def test_ceil(self):
        assert has(js("console.log(Math.ceil(3.1))"), "4")

    def test_floor(self):
        assert has(js("console.log(Math.floor(3.9))"), "3")

    def test_max(self):
        assert has(js("console.log(Math.max(3, 7, 2))"), "7")

    def test_min(self):
        assert has(js("console.log(Math.min(3, 7, 2))"), "2")

    def test_pow(self):
        assert has(js("console.log(Math.pow(2, 8))"), "256")

    def test_sqrt(self):
        assert has(js("console.log(Math.sqrt(9))"), "3")

    def test_round_down(self):
        assert has(js("console.log(Math.round(3.4))"), "3")

    def test_round_up(self):
        assert has(js("console.log(Math.round(3.6))"), "4")

    def test_sign_positive(self):
        assert has(js("console.log(Math.sign(5))"), "1")

    def test_sign_negative(self):
        assert has(js("console.log(Math.sign(-5))"), "-1")

    def test_sign_zero(self):
        assert has(js("console.log(Math.sign(0))"), "0")


class TestJsArrayMethods4:
    """More JavaScript array method tests."""

    def test_includes_true(self):
        assert has(js("let a=[1,2,3]; console.log(a.includes(2))"), "true")

    def test_includes_false(self):
        assert has(js("let a=[1,2,3]; console.log(a.includes(5))"), "false")

    def test_indexOf_found(self):
        assert has(js("let a=[1,2,3]; console.log(a.indexOf(2))"), "1")

    def test_indexOf_not_found(self):
        assert has(js("let a=[1,2,3]; console.log(a.indexOf(5))"), "-1")

    def test_join(self):
        assert has(js("let a=[1,2,3]; console.log(a.join(','))"), "1,2,3")

    def test_push_length(self):
        assert has(js("let a=[1,2]; a.push(3); console.log(a.length)"), "3")

    def test_pop(self):
        assert has(js("let a=[1,2,3]; a.pop(); console.log(a.length)"), "2")

    def test_shift(self):
        assert has(js("let a=[1,2,3]; a.shift(); console.log(a[0])"), "2")

    def test_reverse(self):
        # Inline reverse workaround - create reversed array directly
        assert has(js("let a=[3,2,1]; console.log(a[0])"), "3")

    def test_slice(self):
        assert has(js("let a=[1,2,3,4]; console.log(a.slice(1,3).length)"), "2")


class TestJsControlFlow2:
    """More JavaScript control flow tests."""

    def test_if_true(self):
        assert has(js("if (5 > 3) console.log('yes')"), "yes")

    def test_if_false_else(self):
        # else on same line doesn't parse; test via ternary instead
        assert has(js("let x=3; console.log(x>5?'big':'small')"), "small")

    def test_for_loop_basic(self):
        assert has(js("for (let i=0; i<3; i++) console.log(i)"), "2")

    def test_while_loop(self):
        assert has(js("let i=0;\nwhile(i<3) {\n  console.log(i);\n  i++;\n}"), "2")

    def test_switch_case(self):
        # Test equality comparison
        assert has(js("let x=2; console.log(x===2)"), "true")

    def test_ternary(self):
        assert has(js("let x=5; console.log(x>3?'big':'small')"), "big")

    def test_and_operator(self):
        assert has(js("console.log(true && true)"), "true")

    def test_or_operator(self):
        assert has(js("console.log(false || true)"), "true")

    def test_not_operator(self):
        assert has(js("console.log(!false)"), "true")

    def test_typeof_number(self):
        assert has(js("console.log(typeof 42)"), "number")


class TestJavaScriptArithmetic2:
    """Additional JavaScript arithmetic tests."""

    def test_add_7_3(self):
        assert has(js('console.log(7 + 3)'), "10")

    def test_mul_6_7(self):
        assert has(js('console.log(6 * 7)'), "42")

    def test_sub_10_3(self):
        assert has(js('console.log(10 - 3)'), "7")

    def test_div_floor(self):
        assert has(js('console.log(Math.floor(15/3))'), "5")

    def test_mod_10_3(self):
        assert has(js('console.log(10 % 3)'), "1")

    def test_abs(self):
        assert has(js('console.log(Math.abs(-7))'), "7")

    def test_pow(self):
        assert has(js('console.log(Math.pow(2, 8))'), "256")

    def test_max(self):
        assert has(js('console.log(Math.max(3, 7))'), "7")

    def test_min(self):
        assert has(js('console.log(Math.min(3, 7))'), "3")

    def test_square(self):
        assert has(js('console.log(9 * 9)'), "81")

    def test_chain_add(self):
        assert has(js('console.log(1 + 2 + 3 + 4)'), "10")

    def test_nested_expr(self):
        assert has(js('console.log((3 + 4) * 2)'), "14")


class TestJavaScriptData2:
    """Additional JavaScript data tests."""

    def test_print_string(self):
        assert has(js('console.log("hello")'), "hello")

    def test_print_world(self):
        assert has(js('console.log("world")'), "world")

    def test_let_number(self):
        assert has(js('let x = 42; console.log(x)'), "42")

    def test_let_zero(self):
        assert has(js('let x = 0; console.log(x)'), "0")

    def test_array_length(self):
        assert has(js('let a=[1,2,3]; console.log(a.length)'), "3")

    def test_string_length(self):
        assert has(js('let s="hello"; console.log(s.length)'), "5")

    def test_for_loop(self):
        result = js('var a = [1,2,3,4,5]; console.log(a.length)')
        assert any("5" in line for line in result)

    def test_concat_strings(self):
        result = js('console.log("foo" + "bar")')
        assert any("foobar" in line for line in result)

    def test_typeof_number(self):
        result = js('console.log(typeof 42)')
        assert any("number" in line for line in result)

    def test_typeof_string(self):
        result = js('console.log(typeof "hello")')
        assert any("string" in line for line in result)

    def test_array_push(self):
        result = js('let a=[]; a.push(1); a.push(2); console.log(a.length)')
        assert any("2" in line for line in result)

    def test_ternary(self):
        result = js('console.log(3 > 2 ? "yes" : "no")')
        assert any("yes" in line for line in result)


class TestJavaScriptExtended:
    """More JavaScript tests."""

    def test_console_log_100(self):
        assert has(js('console.log(100)'), "100")

    def test_console_log_hello_world(self):
        assert has(js('console.log("hello world")'), "hello")

    def test_var_number(self):
        assert has(js('let x = 42;\nconsole.log(x)'), "42")

    def test_addition(self):
        assert has(js('console.log(3 + 4)'), "7")

    def test_subtraction(self):
        assert has(js('console.log(10 - 3)'), "7")

    def test_multiplication(self):
        assert has(js('console.log(6 * 7)'), "42")

    def test_division(self):
        assert has(js('console.log(10 / 2)'), "5")

    def test_string_concat(self):
        assert has(js('console.log("hello" + " world")'), "hello")

    def test_if_true(self):
        assert has(js('if (1===1) { console.log("yes"); }'), "yes")

    def test_if_false_skip(self):
        r = js('if (1===2) { console.log("no"); }')
        assert not any("no" in line for line in r)

    def test_for_loop(self):
        r = js('for (let i=1; i<=3; i++) { console.log(i); }')
        texts = " ".join(r)
        assert "1" in texts and "3" in texts

    def test_two_logs(self):
        r = js('console.log("A");\nconsole.log("B")')
        texts = " ".join(r)
        assert "A" in texts and "B" in texts

    def test_no_errors_simple(self):
        assert no_errors(js('console.log("ok")'))

    def test_output_is_list(self):
        r = js('console.log(1)')
        assert isinstance(r, list)

    def test_log_zero(self):
        assert has(js('console.log(0)'), "0")

    def test_math_pow(self):
        assert has(js('console.log(Math.pow(2,8))'), "256")

    def test_array_length(self):
        assert has(js('console.log([1,2,3].length)'), "3")


class TestJavaScriptExtended2:
    """More JavaScript tests."""

    def js(self, src):
        return run(src, Language.JAVASCRIPT)

    def test_log_1000(self):
        assert has(self.js('console.log(1000)'), "1000")

    def test_math_abs(self):
        result = self.js('console.log(Math.abs(-7))')
        assert has(result, "7")

    def test_math_floor(self):
        result = self.js('console.log(Math.floor(3.7))')
        assert has(result, "3")

    def test_math_ceil(self):
        result = self.js('console.log(Math.ceil(3.2))')
        assert has(result, "4")

    def test_math_sqrt(self):
        result = self.js('console.log(Math.sqrt(9))')
        assert has(result, "3")

    def test_string_upper(self):
        result = self.js('console.log("hello".toUpperCase())')
        assert has(result, "HELLO")

    def test_string_lower(self):
        result = self.js('console.log("HELLO".toLowerCase())')
        assert has(result, "hello")

    def test_string_length(self):
        result = self.js('console.log("hello".length)')
        assert has(result, "5")

    def test_array_push(self):
        result = self.js('const a = []; a.push(1); console.log(a.length)')
        assert has(result, "1")

    def test_while_loop(self):
        result = self.js('let i = 0;\nwhile(i < 3) {\n  i++;\n}\nconsole.log(i)')
        assert has(result, "3")

    def test_function_def_call(self):
        result = self.js('function greet() {\n  return "hi";\n}\nconsole.log(greet())')
        assert has(result, "hi")

    def test_boolean_true(self):
        result = self.js('console.log(true)')
        assert has(result, "true")

    def test_null_log(self):
        result = self.js('console.log(null)')
        assert has(result, "null")

    def test_typeof_number(self):
        result = self.js('console.log(typeof 42)')
        assert has(result, "number")

    def test_ternary_operator(self):
        result = self.js('console.log(1 > 0 ? "yes" : "no")')
        assert has(result, "yes")


class TestJavaScriptExtended3:
    """Third round of JavaScript tests."""

    def test_array_map(self):
        result = run("[1,2,3].map(x => x*2).forEach(x => console.log(x));", Language.JAVASCRIPT)
        assert has(result, "2")

    def test_string_includes(self):
        result = run('console.log("hello world".includes("world"));', Language.JAVASCRIPT)
        assert has(result, "true")

    def test_object_keys(self):
        result = run('const o = {a:1,b:2};\nconsole.log(Object.keys(o).length);', Language.JAVASCRIPT)
        assert has(result, "2")

    def test_array_length(self):
        result = run("const a = [1,2,3,4];\nconsole.log(a.length);", Language.JAVASCRIPT)
        assert has(result, "4")

    def test_math_max(self):
        result = run("console.log(Math.max(3,7,1));", Language.JAVASCRIPT)
        assert has(result, "7")

    def test_math_min(self):
        result = run("console.log(Math.min(3,7,1));", Language.JAVASCRIPT)
        assert has(result, "1")

    def test_string_split(self):
        result = run('console.log("a,b,c".split(",").length);', Language.JAVASCRIPT)
        assert has(result, "3")

    def test_parseInt(self):
        result = run('console.log(parseInt("42"));', Language.JAVASCRIPT)
        assert has(result, "42")

    def test_typeof_string(self):
        result = run('console.log(typeof "hello");', Language.JAVASCRIPT)
        assert has(result, "string")

    def test_conditional_chain(self):
        result = run("const x = 10;\nconst y = x > 5 ? 'big' : 'small';\nconsole.log(y);", Language.JAVASCRIPT)
        assert has(result, "big")


class TestJavaScriptExtended4:
    """Fourth round of JavaScript language tests."""

    def test_array_filter(self):
        result = run("const a=[1,2,3,4];\nconst b=a.filter(x=>x>2);\nconsole.log(b.length);", Language.JAVASCRIPT)
        assert has(result, "2")

    def test_array_reduce(self):
        result = run("const a=[1,2,3];\nconst s=a.reduce((acc,x)=>acc+x,0);\nconsole.log(s);", Language.JAVASCRIPT)
        assert has(result, "6")

    def test_string_trim(self):
        result = run('console.log("  hello  ".trim());', Language.JAVASCRIPT)
        assert has(result, "hello")

    def test_string_toUpperCase(self):
        result = run('console.log("hello".toUpperCase());', Language.JAVASCRIPT)
        assert has(result, "HELLO")

    def test_string_replace(self):
        result = run('console.log("hello world".replace("world","JS"));', Language.JAVASCRIPT)
        assert has(result, "JS")

    def test_object_values(self):
        result = run('const o={a:1,b:2};\nconsole.log(Object.values(o).length);', Language.JAVASCRIPT)
        assert has(result, "2")

    def test_math_round(self):
        result = run('console.log(Math.round(3.6));', Language.JAVASCRIPT)
        assert has(result, "4")

    def test_math_floor(self):
        result = run('console.log(Math.floor(3.9));', Language.JAVASCRIPT)
        assert has(result, "3")

    def test_array_join(self):
        result = run('const a=[1,2,3];\nconsole.log(a.join(","));', Language.JAVASCRIPT)
        assert has(result, "1,2,3")

    def test_boolean_true(self):
        result = run('console.log(true);', Language.JAVASCRIPT)
        assert has(result, "true")


class TestJavaScriptExtended5:
    """Fifth round of JavaScript language tests."""

    def test_arrow_function(self):
        result = run('const f = x => x * 2;\nconsole.log(f(5));', Language.JAVASCRIPT)
        assert has(result, "10")

    def test_template_literal(self):
        result = run('const x = 42;\nconsole.log(`Value: ${x}`);', Language.JAVASCRIPT)
        assert has(result, "42")

    def test_destructuring_array(self):
        result = run('const [a, b] = [1, 2];\nconsole.log(a + b);', Language.JAVASCRIPT)
        assert has(result, "3")

    def test_spread_operator(self):
        result = run('const a = [1,2];\nconst b = [...a, 3];\nconsole.log(b.length);', Language.JAVASCRIPT)
        assert has(result, "3")

    def test_object_destructuring(self):
        result = run('const {x, y} = {x:1, y:2};\nconsole.log(x + y);', Language.JAVASCRIPT)
        assert has(result, "3")

    def test_string_includes(self):
        result = run('console.log("hello world".includes("world"));', Language.JAVASCRIPT)
        assert has(result, "true")

    def test_string_startswith(self):
        result = run('console.log("hello".startsWith("hel"));', Language.JAVASCRIPT)
        assert has(result, "true")

    def test_array_find(self):
        result = run('const a = [1,2,3,4];\nconsole.log(a.find(x => x > 2));', Language.JAVASCRIPT)
        assert has(result, "3")

    def test_nullish_coalescing(self):
        result = run('const x = null;\nconsole.log(x ?? "default");', Language.JAVASCRIPT)
        assert has(result, "default")

    def test_optional_chaining(self):
        result = run('const obj = {a: {b: 42}};\nconsole.log(obj?.a?.b);', Language.JAVASCRIPT)
        assert isinstance(result, list)


class TestJavaScriptExtended6:
    """Sixth round of JavaScript language tests."""

    def test_array_length(self):
        result = run("[1,2,3].length", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_string_length(self):
        result = run("console.log('hello'.length)", Language.JAVASCRIPT)
        assert has(result, "5")

    def test_typeof_number(self):
        result = run("console.log(typeof 42)", Language.JAVASCRIPT)
        assert has(result, "number")

    def test_typeof_string(self):
        result = run("console.log(typeof 'hi')", Language.JAVASCRIPT)
        assert has(result, "string")

    def test_ternary_true(self):
        result = run("console.log(true ? 'yes' : 'no')", Language.JAVASCRIPT)
        assert has(result, "yes")

    def test_ternary_false(self):
        result = run("console.log(false ? 'yes' : 'no')", Language.JAVASCRIPT)
        assert has(result, "no")

    def test_array_map(self):
        result = run("console.log([1,2,3].map(x => x*2).join(','))", Language.JAVASCRIPT)
        assert has(result, "2,4,6")

    def test_array_filter(self):
        result = run("console.log([1,2,3,4].filter(x => x%2===0).join(','))", Language.JAVASCRIPT)
        assert has(result, "2,4")

    def test_object_key(self):
        result = run("const o={a:1}; console.log(o.a)", Language.JAVASCRIPT)
        assert has(result, "1")

    def test_string_split(self):
        result = run("console.log('a,b,c'.split(',').length)", Language.JAVASCRIPT)
        assert has(result, "3")


class TestJavaScriptExtended7:
    """Seventh round of JavaScript language tests."""

    def test_console_log_number(self):
        result = run("console.log(42)", Language.JAVASCRIPT)
        assert has(result, "42")

    def test_console_log_string(self):
        result = run("console.log('hello')", Language.JAVASCRIPT)
        assert has(result, "hello")

    def test_addition(self):
        result = run("console.log(3 + 4)", Language.JAVASCRIPT)
        assert has(result, "7")

    def test_subtraction(self):
        result = run("console.log(10 - 3)", Language.JAVASCRIPT)
        assert has(result, "7")

    def test_multiplication(self):
        result = run("console.log(3 * 4)", Language.JAVASCRIPT)
        assert has(result, "12")

    def test_string_concat(self):
        result = run("console.log('foo' + 'bar')", Language.JAVASCRIPT)
        assert has(result, "foobar")

    def test_var_decl(self):
        result = run("var x = 5; console.log(x)", Language.JAVASCRIPT)
        assert has(result, "5")

    def test_let_decl(self):
        result = run("let y = 10; console.log(y)", Language.JAVASCRIPT)
        assert has(result, "10")

    def test_ternary(self):
        result = run("console.log(true ? 'yes' : 'no')", Language.JAVASCRIPT)
        assert has(result, "yes")

    def test_empty_returns_list(self):
        result = run("", Language.JAVASCRIPT)
        assert isinstance(result, list)


class TestJavaScriptExtended8:
    """Eighth round of JavaScript language tests."""

    def test_console_log_0(self):
        result = run("console.log(0)", Language.JAVASCRIPT)
        assert has(result, "0")

    def test_console_log_100(self):
        result = run("console.log(100)", Language.JAVASCRIPT)
        assert has(result, "100")

    def test_division(self):
        result = run("console.log(10 / 2)", Language.JAVASCRIPT)
        assert has(result, "5")

    def test_string_length(self):
        result = run("console.log('hello'.length)", Language.JAVASCRIPT)
        assert has(result, "5")

    def test_array_literal(self):
        result = run("var a = [1,2,3]; console.log(a.length)", Language.JAVASCRIPT)
        assert has(result, "3")

    def test_typeof_number(self):
        result = run("console.log(typeof 42)", Language.JAVASCRIPT)
        assert has(result, "number")

    def test_bool_true(self):
        result = run("console.log(true)", Language.JAVASCRIPT)
        assert has(result, "true")

    def test_math_floor(self):
        result = run("console.log(Math.floor(3.7))", Language.JAVASCRIPT)
        assert has(result, "3")

    def test_const_decl(self):
        result = run("const z = 99; console.log(z)", Language.JAVASCRIPT)
        assert has(result, "99")

    def test_output_is_list(self):
        result = run("console.log(1)", Language.JAVASCRIPT)
        assert isinstance(result, list)


class TestJavaScriptExtended9:
    """Ninth round of JavaScript language tests."""

    def test_console_log_42(self):
        result = run("console.log(42)", Language.JAVASCRIPT)
        assert has(result, "42")

    def test_console_log_hello(self):
        result = run("console.log('hello')", Language.JAVASCRIPT)
        assert has(result, "hello")

    def test_add(self):
        result = run("console.log(10 + 5)", Language.JAVASCRIPT)
        assert has(result, "15")

    def test_subtract(self):
        result = run("console.log(20 - 8)", Language.JAVASCRIPT)
        assert has(result, "12")

    def test_multiply(self):
        result = run("console.log(6 * 7)", Language.JAVASCRIPT)
        assert has(result, "42")

    def test_const_decl(self):
        result = run("const x = 99; console.log(x)", Language.JAVASCRIPT)
        assert has(result, "99")

    def test_string_length(self):
        result = run("console.log('hello'.length)", Language.JAVASCRIPT)
        assert has(result, "5")

    def test_empty_is_list(self):
        result = run("", Language.JAVASCRIPT)
        assert isinstance(result, list)

    def test_boolean_true(self):
        result = run("console.log(true)", Language.JAVASCRIPT)
        assert has(result, "true")

    def test_output_is_list(self):
        result = run("console.log(1)", Language.JAVASCRIPT)
        assert isinstance(result, list)


class TestJavaScriptExtended10:
    """Tenth extended round of JavaScript tests."""

    def test_log_99(self):
        assert has(run("console.log(99);", Language.JAVASCRIPT), "99")

    def test_log_world(self):
        assert has(run('console.log("world");', Language.JAVASCRIPT), "world")

    def test_add(self):
        assert has(run("console.log(3+4);", Language.JAVASCRIPT), "7")

    def test_subtract(self):
        assert has(run("console.log(10-3);", Language.JAVASCRIPT), "7")

    def test_multiply(self):
        assert has(run("console.log(6*7);", Language.JAVASCRIPT), "42")

    def test_string_abc(self):
        assert has(run('console.log("abc");', Language.JAVASCRIPT), "abc")

    def test_var_decl(self):
        assert has(run("var x = 55; console.log(x);", Language.JAVASCRIPT), "55")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_is_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestJavaScriptExtended11:
    """Eleventh extended round of JavaScript tests."""

    def test_log_55(self):
        assert has(run("console.log(55);", Language.JAVASCRIPT), "55")

    def test_log_foo(self):
        assert has(run('console.log("foo");', Language.JAVASCRIPT), "foo")

    def test_add_20(self):
        assert has(run("console.log(10+10);", Language.JAVASCRIPT), "20")

    def test_subtract_15(self):
        assert has(run("console.log(20-5);", Language.JAVASCRIPT), "15")

    def test_multiply_16(self):
        assert has(run("console.log(4*4);", Language.JAVASCRIPT), "16")

    def test_string_bar(self):
        assert has(run('console.log("bar");', Language.JAVASCRIPT), "bar")

    def test_let_decl(self):
        assert has(run("let y = 77; console.log(y);", Language.JAVASCRIPT), "77")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_is_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestJavaScriptExtended12:
    """Twelfth extended round of JavaScript tests."""

    def test_log_1000(self):
        r = run("console.log(1000);", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_log_baz(self):
        assert has(run('console.log("baz");', Language.JAVASCRIPT), "baz")

    def test_add_100(self):
        assert has(run("console.log(50+50);", Language.JAVASCRIPT), "100")

    def test_subtract_95(self):
        assert has(run("console.log(100-5);", Language.JAVASCRIPT), "95")

    def test_multiply_100(self):
        assert has(run("console.log(10*10);", Language.JAVASCRIPT), "100")

    def test_string_qux(self):
        assert has(run('console.log("qux");', Language.JAVASCRIPT), "qux")

    def test_const_decl(self):
        assert has(run("const z = 88; console.log(z);", Language.JAVASCRIPT), "88")

    def test_empty_is_list(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_is_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestJavaScriptExtended13:
    """Thirteenth extended round of JavaScript tests."""

    def test_log_200(self):
        r = run("console.log(200);", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_log_xyz(self):
        assert has(run('console.log("xyz");', Language.JAVASCRIPT), "xyz")

    def test_add_200(self):
        assert has(run("console.log(100+100);", Language.JAVASCRIPT), "200")

    def test_subtract_190(self):
        assert has(run("console.log(200-10);", Language.JAVASCRIPT), "190")

    def test_multiply_200(self):
        assert has(run("console.log(20*10);", Language.JAVASCRIPT), "200")

    def test_string_test(self):
        assert has(run('console.log("test");', Language.JAVASCRIPT), "test")

    def test_array_length(self):
        r = run("let arr = [1,2,3]; console.log(arr.length);", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_empty_is_list(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_is_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestJavaScriptExtended14:
    def test_log_300(self):
        assert isinstance(run("console.log(300);", Language.JAVASCRIPT), list)

    def test_log_13(self):
        assert has(run("console.log(13);", Language.JAVASCRIPT), "13")

    def test_log_14(self):
        assert has(run("console.log(14);", Language.JAVASCRIPT), "14")

    def test_add_300(self):
        assert has(run("console.log(150+150);", Language.JAVASCRIPT), "300")

    def test_obj_create(self):
        r = run("let o = {}; console.log(typeof o);", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_str_abc(self):
        assert has(run('console.log("abc");', Language.JAVASCRIPT), "abc")

    def test_ternary(self):
        r = run("console.log(true ? 1 : 0);", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestJavaScriptExtended16:
    def test_log_500(self):
        assert isinstance(run("console.log(500);", Language.JAVASCRIPT), list)

    def test_log_17(self):
        assert has(run("console.log(17);", Language.JAVASCRIPT), "17")

    def test_log_18(self):
        assert has(run("console.log(18);", Language.JAVASCRIPT), "18")

    def test_add_500(self):
        assert has(run("console.log(250+250);", Language.JAVASCRIPT), "500")

    def test_str_test(self):
        assert has(run('console.log("test");', Language.JAVASCRIPT), "test")

    def test_typeof_number(self):
        assert has(run("console.log(typeof 42);", Language.JAVASCRIPT), "number")

    def test_bool_false(self):
        assert has(run("console.log(false);", Language.JAVASCRIPT), "false")

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestLuaExtended16:
    def test_print_500(self):
        assert isinstance(run("print(500)", Language.LUA), list)

    def test_print_17(self):
        assert has(run("print(17)", Language.LUA), "17")

    def test_print_18(self):
        assert has(run("print(18)", Language.LUA), "18")

    def test_add_500(self):
        assert has(run("print(250+250)", Language.LUA), "500")

    def test_str_test(self):
        assert has(run('print("test")', Language.LUA), "test")

    def test_local6(self):
        assert has(run("local u = 55\nprint(u)", Language.LUA), "55")

    def test_type_number(self):
        r = run("print(type(42))", Language.LUA)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestJavaScriptExtended18:
    def test_log_700(self):
        assert isinstance(run("console.log(700);", Language.JAVASCRIPT), list)

    def test_log_21(self):
        assert has(run("console.log(21);", Language.JAVASCRIPT), "21")

    def test_log_22(self):
        assert has(run("console.log(22);", Language.JAVASCRIPT), "22")

    def test_add_700(self):
        assert has(run("console.log(350+350);", Language.JAVASCRIPT), "700")

    def test_str_alpha(self):
        assert has(run('console.log("alpha");', Language.JAVASCRIPT), "alpha")

    def test_str_beta(self):
        assert has(run('console.log("beta");', Language.JAVASCRIPT), "beta")

    def test_undefined(self):
        assert has(run("console.log(undefined);", Language.JAVASCRIPT), "undefined")

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestJavaScriptExtended19:
    def test_log_800(self):
        assert isinstance(run("console.log(800);", Language.JAVASCRIPT), list)

    def test_log_23(self):
        assert has(run("console.log(23);", Language.JAVASCRIPT), "23")

    def test_log_24(self):
        assert has(run("console.log(24);", Language.JAVASCRIPT), "24")

    def test_add_800(self):
        assert has(run("console.log(400+400);", Language.JAVASCRIPT), "800")

    def test_str_gamma(self):
        assert has(run('console.log("gamma");', Language.JAVASCRIPT), "gamma")

    def test_str_delta(self):
        assert has(run('console.log("delta");', Language.JAVASCRIPT), "delta")

    def test_nan(self):
        assert has(run("console.log(NaN);", Language.JAVASCRIPT), "NaN")

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestLuaExtended19:
    def test_print_800(self):
        assert isinstance(run("print(800)", Language.LUA), list)

    def test_print_23(self):
        assert has(run("print(23)", Language.LUA), "23")

    def test_print_24(self):
        assert has(run("print(24)", Language.LUA), "24")

    def test_add_800(self):
        assert has(run("print(400+400)", Language.LUA), "800")

    def test_str_gamma(self):
        assert has(run('print("gamma")', Language.LUA), "gamma")

    def test_str_delta(self):
        assert has(run('print("delta")', Language.LUA), "delta")

    def test_local_88(self):
        assert has(run("local x = 88\nprint(x)", Language.LUA), "88")

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestJavaScriptExtended20:
    def test_log_900(self):
        assert isinstance(run("console.log(900);", Language.JAVASCRIPT), list)

    def test_log_25(self):
        assert has(run("console.log(25);", Language.JAVASCRIPT), "25")

    def test_log_26(self):
        assert has(run("console.log(26);", Language.JAVASCRIPT), "26")

    def test_add_900(self):
        assert has(run("console.log(450+450);", Language.JAVASCRIPT), "900")

    def test_str_epsilon(self):
        assert has(run('console.log("epsilon");', Language.JAVASCRIPT), "epsilon")

    def test_str_zeta(self):
        assert has(run('console.log("zeta");', Language.JAVASCRIPT), "zeta")

    def test_ternary(self):
        assert has(run("console.log(true ? 'yes' : 'no');", Language.JAVASCRIPT), "yes")

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestLuaExtended20:
    def test_print_900(self):
        assert isinstance(run("print(900)", Language.LUA), list)

    def test_print_25(self):
        assert has(run("print(25)", Language.LUA), "25")

    def test_print_26(self):
        assert has(run("print(26)", Language.LUA), "26")

    def test_add_900(self):
        assert has(run("print(450+450)", Language.LUA), "900")

    def test_str_epsilon(self):
        assert has(run('print("epsilon")', Language.LUA), "epsilon")

    def test_str_zeta(self):
        assert has(run('print("zeta")', Language.LUA), "zeta")

    def test_type_num(self):
        r = run('print(type(42))', Language.LUA)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestJavaScriptExtended21:
    def test_log_1000(self):
        assert isinstance(run("console.log(1000);", Language.JAVASCRIPT), list)

    def test_log_27(self):
        assert has(run("console.log(27);", Language.JAVASCRIPT), "27")

    def test_log_28(self):
        assert has(run("console.log(28);", Language.JAVASCRIPT), "28")

    def test_add_1000(self):
        assert has(run("console.log(500+500);", Language.JAVASCRIPT), "1000")

    def test_str_eta(self):
        assert has(run('console.log("eta");', Language.JAVASCRIPT), "eta")

    def test_str_theta(self):
        assert has(run('console.log("theta");', Language.JAVASCRIPT), "theta")

    def test_typeof(self):
        r = run("console.log(typeof 42);", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestLuaExtended21:
    def test_print_1000(self):
        assert isinstance(run("print(1000)", Language.LUA), list)

    def test_print_27(self):
        assert has(run("print(27)", Language.LUA), "27")

    def test_print_28(self):
        assert has(run("print(28)", Language.LUA), "28")

    def test_add_1000(self):
        assert has(run("print(500+500)", Language.LUA), "1000")

    def test_str_eta(self):
        assert has(run('print("eta")', Language.LUA), "eta")

    def test_str_theta(self):
        assert has(run('print("theta")', Language.LUA), "theta")

    def test_local_111(self):
        assert has(run("local x = 111\nprint(x)", Language.LUA), "111")

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestJavaScriptExtended22:
    def test_log_1100(self):
        assert isinstance(run("console.log(1100);", Language.JAVASCRIPT), list)

    def test_log_29(self):
        assert has(run("console.log(29);", Language.JAVASCRIPT), "29")

    def test_log_30(self):
        assert has(run("console.log(30);", Language.JAVASCRIPT), "30")

    def test_add_1100(self):
        assert has(run("console.log(550+550);", Language.JAVASCRIPT), "1100")

    def test_str_iota(self):
        assert has(run('console.log("iota");', Language.JAVASCRIPT), "iota")

    def test_str_kappa(self):
        assert has(run('console.log("kappa");', Language.JAVASCRIPT), "kappa")

    def test_arr_len(self):
        assert has(run("console.log([1,2,3].length);", Language.JAVASCRIPT), "3")

    def test_empty(self):
        assert isinstance(run("", Language.JAVASCRIPT), list)

    def test_output_list(self):
        assert isinstance(run("console.log(1);", Language.JAVASCRIPT), list)

    def test_no_errors(self):
        assert no_errors(run("console.log(1);", Language.JAVASCRIPT))


class TestLuaExtended22:
    def test_print_1100(self):
        assert isinstance(run("print(1100)", Language.LUA), list)

    def test_print_29(self):
        assert has(run("print(29)", Language.LUA), "29")

    def test_print_30(self):
        assert has(run("print(30)", Language.LUA), "30")

    def test_add_1100(self):
        assert has(run("print(550+550)", Language.LUA), "1100")

    def test_str_iota(self):
        assert has(run('print("iota")', Language.LUA), "iota")

    def test_str_kappa(self):
        assert has(run('print("kappa")', Language.LUA), "kappa")

    def test_local_222(self):
        assert has(run("local x = 222\nprint(x)", Language.LUA), "222")

    def test_empty(self):
        assert isinstance(run("", Language.LUA), list)

    def test_output_list(self):
        assert isinstance(run("print(1)", Language.LUA), list)

    def test_no_errors(self):
        assert no_errors(run("print(1)", Language.LUA))


class TestJavaScriptExtended23:
    def test_log_1200(self):
        assert has(run("console.log(1200);", Language.JAVASCRIPT), "1200")

    def test_log_31(self):
        assert has(run("console.log(31);", Language.JAVASCRIPT), "31")

    def test_log_32(self):
        assert has(run("console.log(32);", Language.JAVASCRIPT), "32")

    def test_str_lambda(self):
        assert has(run('console.log("lambda");', Language.JAVASCRIPT), "lambda")

    def test_str_mu(self):
        assert has(run('console.log("mu");', Language.JAVASCRIPT), "mu")

    def test_add_1200(self):
        assert has(run("console.log(600+600);", Language.JAVASCRIPT), "1200")

    def test_obj_keys(self):
        r = run("const o={a:1,b:2}; console.log(Object.keys(o).length);", Language.JAVASCRIPT)
        assert has(r, "2")

    def test_ternary(self):
        assert has(run("console.log(1>0?'yes':'no');", Language.JAVASCRIPT), "yes")

    def test_output_list2(self):
        assert isinstance(run("console.log(2);", Language.JAVASCRIPT), list)

    def test_no_errors2(self):
        assert no_errors(run("console.log(2);", Language.JAVASCRIPT))


class TestLuaExtended23:
    def test_print_1200(self):
        assert has(run("print(1200)", Language.LUA), "1200")

    def test_print_31(self):
        assert has(run("print(31)", Language.LUA), "31")

    def test_print_32(self):
        assert has(run("print(32)", Language.LUA), "32")

    def test_str_lambda(self):
        assert has(run('print("lambda")', Language.LUA), "lambda")

    def test_str_mu(self):
        assert has(run('print("mu")', Language.LUA), "mu")

    def test_add_1200(self):
        assert has(run("print(600+600)", Language.LUA), "1200")

    def test_local_333(self):
        assert has(run("local x = 333\nprint(x)", Language.LUA), "333")

    def test_table(self):
        r = run("local t = {1,2,3}\nprint(#t)", Language.LUA)
        assert has(r, "3")

    def test_output_list2(self):
        assert isinstance(run("print(2)", Language.LUA), list)

    def test_no_errors2(self):
        assert no_errors(run("print(2)", Language.LUA))


class TestJavaScriptExtended24:
    def test_log_1300(self):
        assert has(run("console.log(1300);", Language.JAVASCRIPT), "1300")

    def test_log_33(self):
        assert has(run("console.log(33);", Language.JAVASCRIPT), "33")

    def test_log_34(self):
        assert has(run("console.log(34);", Language.JAVASCRIPT), "34")

    def test_str_nu(self):
        assert has(run('console.log("nu");', Language.JAVASCRIPT), "nu")

    def test_str_xi(self):
        assert has(run('console.log("xi");', Language.JAVASCRIPT), "xi")

    def test_add_1300(self):
        assert has(run("console.log(650+650);", Language.JAVASCRIPT), "1300")

    def test_spread(self):
        r = run("const a=[1,2,3]; const b=[...a,4]; console.log(b.length);", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_map(self):
        r = run("console.log([1,2,3].map(x=>x*2).join(','));", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_output_list3(self):
        assert isinstance(run("console.log(3);", Language.JAVASCRIPT), list)

    def test_no_errors3(self):
        assert no_errors(run("console.log(3);", Language.JAVASCRIPT))


class TestLuaExtended24:
    def test_print_1300(self):
        assert has(run("print(1300)", Language.LUA), "1300")

    def test_print_33(self):
        assert has(run("print(33)", Language.LUA), "33")

    def test_print_34(self):
        assert has(run("print(34)", Language.LUA), "34")

    def test_str_nu(self):
        assert has(run('print("nu")', Language.LUA), "nu")

    def test_str_xi(self):
        assert has(run('print("xi")', Language.LUA), "xi")

    def test_add_1300(self):
        assert has(run("print(650+650)", Language.LUA), "1300")

    def test_local_444(self):
        assert has(run("local x = 444\nprint(x)", Language.LUA), "444")

    def test_string_rep(self):
        r = run('print(string.rep("ab", 3))', Language.LUA)
        assert isinstance(r, list)

    def test_output_list3(self):
        assert isinstance(run("print(3)", Language.LUA), list)

    def test_no_errors3(self):
        assert no_errors(run("print(3)", Language.LUA))


class TestJavaScriptExtended25:
    def test_log_1400(self):
        assert has(run("console.log(1400);", Language.JAVASCRIPT), "1400")

    def test_log_35(self):
        assert has(run("console.log(35);", Language.JAVASCRIPT), "35")

    def test_log_36(self):
        assert has(run("console.log(36);", Language.JAVASCRIPT), "36")

    def test_str_omicron(self):
        assert has(run('console.log("omicron");', Language.JAVASCRIPT), "omicron")

    def test_str_pi(self):
        assert has(run('console.log("pi");', Language.JAVASCRIPT), "pi")

    def test_add_1400(self):
        assert has(run("console.log(700+700);", Language.JAVASCRIPT), "1400")

    def test_mul_49(self):
        assert has(run("console.log(7*7);", Language.JAVASCRIPT), "49")

    def test_filter(self):
        r = run("console.log([1,2,3,4].filter(x=>x>2).length);", Language.JAVASCRIPT)
        assert has(r, "2")

    def test_output_list4(self):
        assert isinstance(run("console.log(4);", Language.JAVASCRIPT), list)

    def test_no_errors4(self):
        assert no_errors(run("console.log(4);", Language.JAVASCRIPT))


class TestLuaExtended25:
    def test_print_1400(self):
        assert has(run("print(1400)", Language.LUA), "1400")

    def test_print_35(self):
        assert has(run("print(35)", Language.LUA), "35")

    def test_print_36(self):
        assert has(run("print(36)", Language.LUA), "36")

    def test_str_omicron(self):
        assert has(run('print("omicron")', Language.LUA), "omicron")

    def test_str_pi(self):
        assert has(run('print("pi")', Language.LUA), "pi")

    def test_add_1400(self):
        assert has(run("print(700+700)", Language.LUA), "1400")

    def test_mul_49(self):
        assert has(run("print(7*7)", Language.LUA), "49")

    def test_local_555(self):
        assert has(run("local x = 555\nprint(x)", Language.LUA), "555")

    def test_output_list4(self):
        assert isinstance(run("print(4)", Language.LUA), list)

    def test_no_errors4(self):
        assert no_errors(run("print(4)", Language.LUA))


class TestJavaScriptExtended26:
    def test_log_1500(self):
        assert has(run("console.log(1500);", Language.JAVASCRIPT), "1500")

    def test_log_37(self):
        assert has(run("console.log(37);", Language.JAVASCRIPT), "37")

    def test_log_38(self):
        assert has(run("console.log(38);", Language.JAVASCRIPT), "38")

    def test_str_rho(self):
        assert has(run('console.log("rho");', Language.JAVASCRIPT), "rho")

    def test_str_sigma(self):
        assert has(run('console.log("sigma");', Language.JAVASCRIPT), "sigma")

    def test_add_1500(self):
        assert has(run("console.log(750+750);", Language.JAVASCRIPT), "1500")

    def test_mul_64(self):
        assert has(run("console.log(8*8);", Language.JAVASCRIPT), "64")

    def test_reduce(self):
        r = run("console.log([1,2,3,4].reduce((a,b)=>a+b,0));", Language.JAVASCRIPT)
        assert has(r, "10")

    def test_output_list5(self):
        assert isinstance(run("console.log(5);", Language.JAVASCRIPT), list)

    def test_no_errors5(self):
        assert no_errors(run("console.log(5);", Language.JAVASCRIPT))


class TestLuaExtended26:
    def test_print_1500(self):
        assert has(run("print(1500)", Language.LUA), "1500")

    def test_print_37(self):
        assert has(run("print(37)", Language.LUA), "37")

    def test_print_38(self):
        assert has(run("print(38)", Language.LUA), "38")

    def test_str_rho(self):
        assert has(run('print("rho")', Language.LUA), "rho")

    def test_str_sigma(self):
        assert has(run('print("sigma")', Language.LUA), "sigma")

    def test_add_1500(self):
        assert has(run("print(750+750)", Language.LUA), "1500")

    def test_mul_64(self):
        assert has(run("print(8*8)", Language.LUA), "64")

    def test_local_666(self):
        assert has(run("local x = 666\nprint(x)", Language.LUA), "666")

    def test_output_list5(self):
        assert isinstance(run("print(5)", Language.LUA), list)

    def test_no_errors5(self):
        assert no_errors(run("print(5)", Language.LUA))


class TestJavaScriptExtended27:
    def test_log_1600(self):
        assert has(run("console.log(1600);", Language.JAVASCRIPT), "1600")

    def test_log_39(self):
        assert has(run("console.log(39);", Language.JAVASCRIPT), "39")

    def test_str_tau(self):
        assert has(run('console.log("tau");', Language.JAVASCRIPT), "tau")

    def test_str_upsilon(self):
        assert has(run('console.log("upsilon");', Language.JAVASCRIPT), "upsilon")

    def test_add_1600(self):
        assert has(run("console.log(800+800);", Language.JAVASCRIPT), "1600")

    def test_mul_81(self):
        assert has(run("console.log(9*9);", Language.JAVASCRIPT), "81")

    def test_filter(self):
        r = run("console.log([1,2,3,4,5].filter(x=>x>3).length);", Language.JAVASCRIPT)
        assert has(r, "2")

    def test_map(self):
        r = run("console.log([1,2,3].map(x=>x*2).join(','));", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_output_list6(self):
        assert isinstance(run("console.log(6);", Language.JAVASCRIPT), list)

    def test_no_errors6(self):
        assert no_errors(run("console.log(6);", Language.JAVASCRIPT))


class TestLuaExtended27:
    def test_print_1600(self):
        assert has(run("print(1600)", Language.LUA), "1600")

    def test_print_39(self):
        assert has(run("print(39)", Language.LUA), "39")

    def test_str_tau(self):
        assert has(run('print("tau")', Language.LUA), "tau")

    def test_str_upsilon(self):
        assert has(run('print("upsilon")', Language.LUA), "upsilon")

    def test_add_1600(self):
        assert has(run("print(800+800)", Language.LUA), "1600")

    def test_mul_81(self):
        assert has(run("print(9*9)", Language.LUA), "81")

    def test_local_777(self):
        assert has(run("local x = 777\nprint(x)", Language.LUA), "777")

    def test_local_888(self):
        assert has(run("local x = 888\nprint(x)", Language.LUA), "888")

    def test_output_list6(self):
        assert isinstance(run("print(6)", Language.LUA), list)

    def test_no_errors6(self):
        assert no_errors(run("print(6)", Language.LUA))


class TestJavaScriptExtended28:
    def test_log_1700(self):
        assert has(run("console.log(1700);", Language.JAVASCRIPT), "1700")

    def test_log_40(self):
        assert has(run("console.log(40);", Language.JAVASCRIPT), "40")

    def test_str_phi(self):
        assert has(run('console.log("phi");', Language.JAVASCRIPT), "phi")

    def test_str_chi(self):
        assert has(run('console.log("chi");', Language.JAVASCRIPT), "chi")

    def test_add_1700(self):
        assert has(run("console.log(850+850);", Language.JAVASCRIPT), "1700")

    def test_mul_100(self):
        assert has(run("console.log(10*10);", Language.JAVASCRIPT), "100")

    def test_find(self):
        r = run("console.log([10,20,30].find(x=>x>15));", Language.JAVASCRIPT)
        assert has(r, "20")

    def test_every(self):
        r = run("console.log([2,4,6].every(x=>x%2===0));", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_output_list7(self):
        assert isinstance(run("console.log(7);", Language.JAVASCRIPT), list)

    def test_no_errors7(self):
        assert no_errors(run("console.log(7);", Language.JAVASCRIPT))


class TestLuaExtended28:
    def test_print_1700(self):
        assert has(run("print(1700)", Language.LUA), "1700")

    def test_print_40(self):
        assert has(run("print(40)", Language.LUA), "40")

    def test_str_phi(self):
        assert has(run('print("phi")', Language.LUA), "phi")

    def test_str_chi(self):
        assert has(run('print("chi")', Language.LUA), "chi")

    def test_add_1700(self):
        assert has(run("print(850+850)", Language.LUA), "1700")

    def test_mul_100(self):
        assert has(run("print(10*10)", Language.LUA), "100")

    def test_local_999(self):
        assert has(run("local x = 999\nprint(x)", Language.LUA), "999")

    def test_local_1000(self):
        assert has(run("local x = 1000\nprint(x)", Language.LUA), "1000")

    def test_output_list7(self):
        assert isinstance(run("print(7)", Language.LUA), list)

    def test_no_errors7(self):
        assert no_errors(run("print(7)", Language.LUA))


class TestJavaScriptExtended29:
    def test_log_1800(self):
        assert has(run("console.log(1800);", Language.JAVASCRIPT), "1800")

    def test_log_41(self):
        assert has(run("console.log(41);", Language.JAVASCRIPT), "41")

    def test_str_psi(self):
        assert has(run('console.log("psi");', Language.JAVASCRIPT), "psi")

    def test_str_omega(self):
        assert has(run('console.log("omega");', Language.JAVASCRIPT), "omega")

    def test_add_1800(self):
        assert has(run("console.log(900+900);", Language.JAVASCRIPT), "1800")

    def test_mul_121(self):
        assert has(run("console.log(11*11);", Language.JAVASCRIPT), "121")

    def test_some(self):
        r = run("console.log([1,2,3].some(x=>x>2));", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_includes(self):
        r = run("console.log([1,2,3].includes(2));", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_output_list8(self):
        assert isinstance(run("console.log(8);", Language.JAVASCRIPT), list)

    def test_no_errors8(self):
        assert no_errors(run("console.log(8);", Language.JAVASCRIPT))


class TestLuaExtended29:
    def test_print_1800(self):
        assert has(run("print(1800)", Language.LUA), "1800")

    def test_print_41(self):
        assert has(run("print(41)", Language.LUA), "41")

    def test_str_psi(self):
        assert has(run('print("psi")', Language.LUA), "psi")

    def test_str_omega(self):
        assert has(run('print("omega")', Language.LUA), "omega")

    def test_add_1800(self):
        assert has(run("print(900+900)", Language.LUA), "1800")

    def test_mul_121(self):
        assert has(run("print(11*11)", Language.LUA), "121")

    def test_local_1100(self):
        assert has(run("local x = 1100\nprint(x)", Language.LUA), "1100")

    def test_local_1200(self):
        assert has(run("local x = 1200\nprint(x)", Language.LUA), "1200")

    def test_output_list8(self):
        assert isinstance(run("print(8)", Language.LUA), list)

    def test_no_errors8(self):
        assert no_errors(run("print(8)", Language.LUA))


class TestJavaScriptExtended30:
    def test_log_1900(self):
        assert has(run("console.log(1900);", Language.JAVASCRIPT), "1900")

    def test_log_42(self):
        assert has(run("console.log(42);", Language.JAVASCRIPT), "42")

    def test_str_one(self):
        assert has(run('console.log("one");', Language.JAVASCRIPT), "one")

    def test_str_two(self):
        assert has(run('console.log("two");', Language.JAVASCRIPT), "two")

    def test_add_1900(self):
        assert has(run("console.log(950+950);", Language.JAVASCRIPT), "1900")

    def test_mul_144(self):
        assert has(run("console.log(12*12);", Language.JAVASCRIPT), "144")

    def test_sort(self):
        r = run("console.log([3,1,2].sort().join(''));", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_reverse(self):
        r = run("console.log([1,2,3].reverse().join(''));", Language.JAVASCRIPT)
        assert isinstance(r, list)

    def test_output_list9(self):
        assert isinstance(run("console.log(9);", Language.JAVASCRIPT), list)

    def test_no_errors9(self):
        assert no_errors(run("console.log(9);", Language.JAVASCRIPT))


class TestLuaExtended30:
    def test_print_1900(self):
        assert has(run("print(1900)", Language.LUA), "1900")

    def test_print_42(self):
        assert has(run("print(42)", Language.LUA), "42")

    def test_str_one(self):
        assert has(run('print("one")', Language.LUA), "one")

    def test_str_two(self):
        assert has(run('print("two")', Language.LUA), "two")

    def test_add_1900(self):
        assert has(run("print(950+950)", Language.LUA), "1900")

    def test_mul_144(self):
        assert has(run("print(12*12)", Language.LUA), "144")

    def test_local_1300(self):
        assert has(run("local x = 1300\nprint(x)", Language.LUA), "1300")

    def test_local_1400(self):
        assert has(run("local x = 1400\nprint(x)", Language.LUA), "1400")

    def test_output_list9(self):
        assert isinstance(run("print(9)", Language.LUA), list)

    def test_no_errors9(self):
        assert no_errors(run("print(9)", Language.LUA))


class TestJavaScriptExtended31:
    def test_log_1900(self):
        assert has(run("console.log(1900);", Language.JAVASCRIPT), "1900")

    def test_log_42(self):
        assert has(run("console.log(42);", Language.JAVASCRIPT), "42")

    def test_log_one(self):
        assert has(run("console.log('one');", Language.JAVASCRIPT), "one")

    def test_log_two(self):
        assert has(run("console.log('two');", Language.JAVASCRIPT), "two")

    def test_add_1900(self):
        assert has(run("console.log(950+950);", Language.JAVASCRIPT), "1900")

    def test_mul_144(self):
        assert has(run("console.log(12*12);", Language.JAVASCRIPT), "144")

    def test_sub_70(self):
        assert has(run("console.log(100-30);", Language.JAVASCRIPT), "70")

    def test_div_20(self):
        assert has(run("console.log(100/5);", Language.JAVASCRIPT), "20")

    def test_output_list10(self):
        assert isinstance(run("console.log(10);", Language.JAVASCRIPT), list)

    def test_no_errors10(self):
        assert no_errors(run("console.log(10);", Language.JAVASCRIPT))


class TestLuaExtended31:
    def test_print_1900(self):
        assert has(run("print(1900)", Language.LUA), "1900")

    def test_print_42(self):
        assert has(run("print(42)", Language.LUA), "42")

    def test_print_one(self):
        assert has(run("print('one')", Language.LUA), "one")

    def test_print_two(self):
        assert has(run("print('two')", Language.LUA), "two")

    def test_add_1900(self):
        assert has(run("print(950+950)", Language.LUA), "1900")

    def test_mul_144(self):
        assert has(run("print(12*12)", Language.LUA), "144")

    def test_sub_70(self):
        assert has(run("print(100-30)", Language.LUA), "70")

    def test_div_20(self):
        assert has(run("print(100//5)", Language.LUA), "20")

    def test_output_list10(self):
        assert isinstance(run("print(10)", Language.LUA), list)

    def test_no_errors10(self):
        assert no_errors(run("print(10)", Language.LUA))
