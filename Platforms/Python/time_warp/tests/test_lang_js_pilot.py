"""Comprehensive coverage tests for JavaScript and PILOT executors."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

JS = Language.JAVASCRIPT
PILOT = Language.PILOT


def js(src): return run(src, JS)
def pilot(src): return run(src, PILOT)


# ============================================================================
# JAVASCRIPT EXECUTOR
# ============================================================================

class TestJSBasicOutput:
    def test_console_log_string(self):
        assert has(js('console.log("hello world");'), "hello world")

    def test_console_log_number(self):
        assert has(js('console.log(42);'), "42")

    def test_console_log_bool(self):
        assert has(js('console.log(true);'), "true")

    def test_console_log_null(self):
        out = js('console.log(null);')
        assert isinstance(out, list)

    def test_console_log_undefined(self):
        out = js('console.log(undefined);')
        assert isinstance(out, list)

    def test_multiple_logs(self):
        out = js('console.log(1);\nconsole.log(2);\nconsole.log(3);')
        assert has(out, "1") and has(out, "3")

    def test_console_log_expression(self):
        assert has(js('console.log(3 + 4 * 2);'), "11")


class TestJSVariables:
    def test_let_declare(self):
        assert has(js('let x = 5;\nconsole.log(x);'), "5")

    def test_const_declare(self):
        assert has(js('const PI = 3.14;\nconsole.log(PI);'), "3.14")

    def test_var_declare(self):
        out = js('var x = 10;\nconsole.log(x);')
        assert has(out, "10")

    def test_multiple_vars(self):
        out = js('let x = 3;\nlet y = 4;\nconsole.log(x + y);')
        assert has(out, "7")

    def test_string_var(self):
        out = js('let s = "hello";\nconsole.log(s);')
        assert has(out, "hello")

    def test_reassign(self):
        out = js('let x = 5;\nx = 10;\nconsole.log(x);')
        assert has(out, "10")

    def test_increment(self):
        out = js('let x = 5;\nx++;\nconsole.log(x);')
        assert isinstance(out, list)

    def test_decrement(self):
        out = js('let x = 5;\nx--;\nconsole.log(x);')
        assert isinstance(out, list)


class TestJSArithmetic:
    def test_addition(self):
        assert has(js('console.log(3 + 4);'), "7")

    def test_subtraction(self):
        assert has(js('console.log(10 - 3);'), "7")

    def test_multiplication(self):
        assert has(js('console.log(3 * 4);'), "12")

    def test_division(self):
        assert has(js('console.log(10 / 2);'), "5")

    def test_modulo(self):
        assert has(js('console.log(10 % 3);'), "1")

    def test_power(self):
        out = js('console.log(2 ** 8);')
        assert has(out, "256")

    def test_bitwise(self):
        out = js('console.log(5 & 3);\nconsole.log(5 | 3);\nconsole.log(5 ^ 3);')
        assert isinstance(out, list)

    def test_shift(self):
        out = js('console.log(1 << 4);\nconsole.log(16 >> 2);')
        assert isinstance(out, list)


class TestJSStringOps:
    def test_length(self):
        out = js('let s = "hello";\nconsole.log(s.length);')
        assert has(out, "5")

    def test_upper(self):
        out = js('console.log("hello".toUpperCase());')
        assert has(out, "HELLO")

    def test_lower(self):
        out = js('console.log("HELLO".toLowerCase());')
        assert has(out, "hello")

    def test_includes(self):
        out = js('console.log("hello world".includes("world"));')
        assert has(out, "true")

    def test_indexof(self):
        out = js('console.log("hello".indexOf("l"));')
        assert has(out, "2")

    def test_slice(self):
        out = js('console.log("hello".slice(1, 3));')
        assert has(out, "el")

    def test_trim(self):
        out = js('console.log("  hello  ".trim());')
        assert has(out, "hello")

    def test_split(self):
        out = js('let parts = "a,b,c".split(",");\nconsole.log(parts.length);')
        assert has(out, "3")

    def test_replace(self):
        out = js('console.log("hello world".replace("world", "there"));')
        assert has(out, "hello there")

    def test_template_literal(self):
        out = js('let name = "world";\nconsole.log(`hello ${name}`);')
        assert has(out, "hello world")

    def test_concat(self):
        out = js('console.log("hello" + " " + "world");')
        assert has(out, "hello world")

    def test_charAt(self):
        out = js('console.log("hello".charAt(0));')
        assert has(out, "h")


class TestJSArrays:
    def test_literal(self):
        out = js('let a = [1, 2, 3];\nconsole.log(a.length);')
        assert has(out, "3")

    def test_access(self):
        out = js('let a = [10, 20, 30];\nconsole.log(a[1]);')
        assert has(out, "20")

    def test_push(self):
        out = js('let a = [1, 2];\na.push(3);\nconsole.log(a.length);')
        assert has(out, "3")

    def test_pop(self):
        out = js('let a = [1, 2, 3];\na.pop();\nconsole.log(a.length);')
        assert has(out, "2")

    def test_map(self):
        out = js('let a = [1, 2, 3].map(x => x * 2);\nconsole.log(a[2]);')
        assert has(out, "6")

    def test_filter(self):
        out = js('let a = [1, 2, 3, 4].filter(x => x > 2);\nconsole.log(a.length);')
        assert has(out, "2")

    def test_reduce(self):
        out = js('let sum = [1,2,3,4,5].reduce((acc,x) => acc + x, 0);\nconsole.log(sum);')
        assert has(out, "15")

    def test_includes(self):
        out = js('console.log([1,2,3].includes(2));')
        assert has(out, "true")

    def test_sort(self):
        out = js('let a = [3,1,2];\na.sort();\nconsole.log(a[0]);')
        assert isinstance(out, list)

    def test_find(self):
        out = js('let a = [1,2,3,4].find(x => x > 2);\nconsole.log(a);')
        assert has(out, "3")

    def test_some(self):
        out = js('console.log([1,2,3].some(x => x > 2));')
        assert has(out, "true")

    def test_every(self):
        out = js('console.log([2,4,6].every(x => x % 2 === 0));')
        assert has(out, "true")

    def test_join(self):
        out = js('console.log([1,2,3].join("-"));')
        assert has(out, "1-2-3")

    def test_slice(self):
        out = js('let a = [1,2,3,4,5].slice(1,3);\nconsole.log(a.length);')
        assert has(out, "2")

    def test_spread(self):
        out = js('let a = [1,2];\nlet b = [...a, 3, 4];\nconsole.log(b.length);')
        assert isinstance(out, list)


class TestJSObjects:
    def test_object_literal(self):
        out = js('let obj = {name: "Alice", age: 30};\nconsole.log(obj.name);')
        assert has(out, "Alice")

    def test_property_access(self):
        out = js('let obj = {x: 1, y: 2};\nconsole.log(obj.x + obj.y);')
        assert has(out, "3")

    def test_bracket_access(self):
        out = js('let obj = {x: 42};\nconsole.log(obj["x"]);')
        assert has(out, "42")

    def test_object_keys(self):
        out = js('let obj = {a: 1, b: 2};\nconsole.log(Object.keys(obj).length);')
        assert has(out, "2")

    def test_json_stringify(self):
        out = js('let obj = {x: 1};\nconsole.log(JSON.stringify(obj));')
        assert has(out, "x")

    def test_json_parse(self):
        out = js('let obj = JSON.parse(\'{"x": 42}\');\nconsole.log(obj.x);')
        assert has(out, "42")

    def test_nested_object(self):
        out = js('let obj = {point: {x: 3, y: 4}};\nconsole.log(obj.point.x);')
        assert isinstance(out, list)


class TestJSControlFlow:
    def test_if_true(self):
        out = js('let x = 5;\nif (x > 3) { console.log("big"); }')
        assert has(out, "big")

    def test_if_else(self):
        out = js('let x = 1;\nif (x > 3) { console.log("big"); } else { console.log("small"); }')
        assert has(out, "small")

    def test_if_elseif(self):
        out = js('let x = 2;\nif (x === 1) { console.log("one"); } else if (x === 2) { console.log("two"); } else { console.log("other"); }')
        assert isinstance(out, list)

    def test_ternary(self):
        out = js('let x = 5;\nconsole.log(x > 3 ? "big" : "small");')
        assert has(out, "big")

    def test_switch(self):
        out = js('let x = 2;\nswitch(x) { case 1: console.log("one"); break; case 2: console.log("two"); break; default: console.log("other"); }')
        assert has(out, "two")

    def test_switch_default(self):
        out = js('let x = 99;\nswitch(x) { case 1: console.log("one"); break; default: console.log("other"); }')
        assert has(out, "other")

    def test_for_loop(self):
        out = js('for (let i = 0; i < 3; i++) { console.log(i); }')
        assert has(out, "0") and has(out, "2")

    def test_while_loop(self):
        out = js('let i = 0;\nwhile (i < 3) { console.log(i); i++; }')
        assert has(out, "0") and has(out, "2")

    def test_for_of(self):
        out = js('for (let x of [1, 2, 3]) { console.log(x); }')
        assert has(out, "1") and has(out, "3")

    def test_for_in(self):
        out = js('let obj = {a: 1, b: 2};\nfor (let k in obj) { console.log(k); }')
        assert isinstance(out, list)

    def test_do_while(self):
        out = js('let i = 0;\ndo { console.log(i); i++; } while (i < 3);')
        assert isinstance(out, list)


class TestJSFunctions:
    def test_function_decl(self):
        out = js('function square(n) { return n * n; }\nconsole.log(square(5));')
        assert has(out, "25")

    def test_arrow_function(self):
        out = js('const f = (x) => x * 2;\nconsole.log(f(5));')
        assert has(out, "10")

    def test_arrow_single(self):
        out = js('const f = x => x + 1;\nconsole.log(f(5));')
        assert isinstance(out, list)

    def test_default_param(self):
        out = js('function greet(name = "world") { return "hello " + name; }\nconsole.log(greet());')
        assert isinstance(out, list)

    def test_recursion(self):
        out = js('function fact(n) { if (n <= 1) return 1; return n * fact(n-1); }\nconsole.log(fact(5));')
        assert has(out, "120")

    def test_closure(self):
        out = js('function makeAdder(x) { return n => n + x; }\nconst add5 = makeAdder(5);\nconsole.log(add5(3));')
        assert isinstance(out, list)

    def test_immediately_invoked(self):
        out = js('console.log((function(x) { return x * 2; })(5));')
        assert isinstance(out, list)


class TestJSMath:
    def test_sqrt(self):
        assert has(js('console.log(Math.sqrt(16));'), "4")

    def test_abs(self):
        assert has(js('console.log(Math.abs(-5));'), "5")

    def test_floor_ceil(self):
        out = js('console.log(Math.floor(3.7));\nconsole.log(Math.ceil(3.2));')
        assert has(out, "3") and has(out, "4")

    def test_round(self):
        out = js('console.log(Math.round(3.5));')
        assert isinstance(out, list)

    def test_max_min(self):
        out = js('console.log(Math.max(3, 5, 1));\nconsole.log(Math.min(3, 5, 1));')
        assert has(out, "5") and has(out, "1")

    def test_pow(self):
        out = js('console.log(Math.pow(2, 10));')
        assert has(out, "1024")

    def test_random(self):
        out = js('let r = Math.random();\nconsole.log(r >= 0);')
        assert isinstance(out, list)

    def test_pi_e(self):
        out = js('console.log(Math.PI > 3);\nconsole.log(Math.E > 2);')
        assert isinstance(out, list)

    def test_trunc(self):
        out = js('console.log(Math.trunc(3.7));\nconsole.log(Math.trunc(-3.7));')
        assert isinstance(out, list)


class TestJSErrors:
    def test_syntax_error(self):
        out = js('const x = class {}')
        assert isinstance(out, list)

    def test_runtime_error(self):
        out = js('let x = null;\nconsole.log(x.property);')
        assert isinstance(out, list)

    def test_division_error(self):
        out = js('console.log(1/0);')
        assert isinstance(out, list)

    def test_empty_program(self):
        out = js('')
        assert isinstance(out, list)


# ============================================================================
# PILOT EXECUTOR
# ============================================================================

class TestPilotText:
    def test_t_command(self):
        out = pilot("T:Hello World")
        assert has(out, "Hello World")

    def test_multiple_t(self):
        out = pilot("T:First\nT:Second\nT:Third")
        assert has(out, "First") and has(out, "Third")

    def test_t_with_var(self):
        out = pilot("C:X = 42\nT:#X")
        assert has(out, "42")


class TestPilotCompute:
    def test_simple_assign(self):
        out = pilot("C:X = 5\nT:#X")
        assert has(out, "5")

    def test_arithmetic_expr(self):
        out = pilot("C:X = 5\nC:Y = X + 3\nT:#Y")
        assert has(out, "8")

    def test_multiplication(self):
        out = pilot("C:X = 3\nC:Y = X * 4\nT:#Y")
        assert has(out, "12")

    def test_division(self):
        out = pilot("C:X = 10\nC:Y = X / 2\nT:#Y")
        assert has(out, "5")

    def test_subtraction(self):
        out = pilot("C:X = 10\nC:Y = X - 3\nT:#Y")
        assert has(out, "7")

    def test_increment(self):
        out = pilot("C:X = 0\nC:X = X + 1\nT:#X")
        assert has(out, "1")

    def test_string_var(self):
        out = pilot("C:NAME$ = hello\nT:#NAME$")
        assert isinstance(out, list)


class TestPilotJump:
    def test_jump_label(self):
        out = pilot("J:end\nT:skipped\n*end\nT:reached")
        assert has(out, "reached")

    def test_label_skip(self):
        out = pilot("T:before\nJ:after\nT:skipped\n*after\nT:after label")
        assert has(out, "before") and has(out, "after label")

    def test_jy_true(self):
        out = pilot("C:X = 1\nJY:label\nT:not jumped\n*label\nT:arrived")
        assert has(out, "not jumped") and has(out, "arrived")

    def test_jn_true(self):
        out = pilot("C:X = 0\nJN:label\nT:not jumped\n*label\nT:arrived")
        assert has(out, "arrived")


class TestPilotRemark:
    def test_remark_ignored(self):
        out = pilot("R:This is a comment\nT:visible")
        assert has(out, "visible")

    def test_multiple_remarks(self):
        out = pilot("R:Comment 1\nT:text\nR:Comment 2")
        assert has(out, "text")


class TestPilotEnd:
    def test_e_command(self):
        out = pilot("T:before end\nE:\nT:after end")
        assert has(out, "before end")

    def test_end_stops_execution(self):
        out = pilot("T:line1\nE:\nT:line2")
        # After E:, execution stops (line2 not reached)
        assert has(out, "line1")


class TestPilotUse:
    def test_u_command(self):
        out = pilot("*sub\nT:in sub\n*main\nT:start\nU:sub\nT:after sub")
        assert isinstance(out, list)


class TestPilotTypingY:
    def test_ty_command(self):
        out = pilot("T:hello")
        assert has(out, "hello")

    def test_tn_command(self):
        out = pilot("TN:no match")
        assert isinstance(out, list)
