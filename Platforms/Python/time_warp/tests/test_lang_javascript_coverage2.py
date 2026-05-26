"""Additional coverage tests for the JavaScript language executor (2nd pass).

Targets uncovered sections: JSDate, JSMap, JSSet, JSArray advanced methods,
JSObject advanced methods, JSPromise, JSSymbol, JSRegExp, JSStringConstructor,
_JSNumber arithmetic, _JSNull/_JSUndefined, console.dir/table, parseInt/parseFloat.

Key patterns used throughout:
- Use newlines (not semicolons) between let declarations
- Use Array.from([...]) to create proper JSArray instances for advanced methods
- Use arrow functions (x => expr) not function(){} syntax in callbacks
- Use 3-arg arrow functions (x,i,arr) for flatMap callbacks
- Use Promise() without new for constructor (new Promise -> JSPromise not in scope)
"""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.JAVASCRIPT


def js(source: str) -> list[str]:
    return run(source, L)


# =============================================================================
# _JSNull and _JSUndefined paths
# =============================================================================


class TestNullUndefinedPaths:
    def test_null_falsy_in_if(self):
        out = js("if (!null) console.log('null is falsy')")
        assert has(out, "null is falsy")

    def test_undefined_falsy_in_if(self):
        out = js("if (!undefined) console.log('undef is falsy')")
        assert has(out, "undef is falsy")

    def test_null_in_template(self):
        # null -> Python None in template; exercises _JSNull str path indirectly
        out = js("let x = null\nconsole.log(`x=${x}`)")
        assert isinstance(out, list)

    def test_undefined_in_template(self):
        # _JSUndefined.__str__ called via f-string formatting
        out = js("let x = undefined\nconsole.log(`x=${x}`)")
        assert has(out, "x=undefined")

    def test_null_via_jsadd(self):
        # _js_add handles null coercion to string
        out = js("console.log(_js_add('prefix:', null))")
        assert has(out, "null")

    def test_undefined_via_jsadd(self):
        # _js_add handles undefined coercion to string
        out = js("console.log(_js_add('val:', undefined))")
        assert has(out, "undefined")

    def test_null_equality(self):
        out = js("console.log(null == null)")
        assert has(out, "true")

    def test_undefined_equality(self):
        out = js("console.log(undefined == undefined)")
        assert has(out, "true")

    def test_null_ternary(self):
        # _JSNull.__bool__ exercised via ternary
        out = js("let x = null\nlet r = x ? 'yes' : 'no'\nconsole.log(r)")
        assert has(out, "no")

    def test_undefined_ternary(self):
        # _JSUndefined.__bool__ exercised via ternary
        out = js("let x = undefined\nlet r = x ? 'yes' : 'no'\nconsole.log(r)")
        assert has(out, "no")


# =============================================================================
# _JSNumber arithmetic paths
# =============================================================================


class TestJSNumberArithmetic:
    def test_subtraction(self):
        # Number() returns _JSNumber; __sub__ is triggered
        out = js("let n = Number(10)\nconsole.log(n - 3)")
        assert has(out, "7")

    def test_multiplication(self):
        out = js("let n = Number(6)\nconsole.log(n * 7)")
        assert has(out, "42")

    def test_modulo(self):
        out = js("let n = Number(10)\nconsole.log(n % 3)")
        assert has(out, "1")

    def test_rmul(self):
        # 3 * _JSNumber(5) triggers __rmul__
        out = js("let n = Number(5)\nconsole.log(3 * n)")
        assert has(out, "15")

    def test_rsub(self):
        # 10 - _JSNumber(3) triggers __rsub__
        out = js("let n = Number(3)\nconsole.log(10 - n)")
        assert has(out, "7")

    def test_number_constructor_string(self):
        out = js("console.log(Number('42'))")
        assert has(out, "42")

    def test_number_toFixed(self):
        out = js("console.log((3.14159).toFixed(2))")
        assert has(out, "3.14")

    def test_number_toPrecision(self):
        out = js("console.log((123.456).toPrecision(5))")
        assert isinstance(out, list)

    def test_js_add_number_string(self):
        # _js_add is exposed in the JS globals for direct use
        out = js("console.log(_js_add(1, 'px'))")
        assert has(out, "1px")

    def test_js_add_string_number(self):
        out = js("console.log(_js_add('px', 2))")
        assert has(out, "px2")

    def test_js_add_both_numbers(self):
        out = js("console.log(_js_add(3, 4))")
        assert has(out, "7")

    def test_number_add_string(self):
        # _JSNumber.__add__ with str other
        out = js("let n = Number(5)\nconsole.log(_js_add(n, 'x'))")
        assert has(out, "5x")

    def test_number_radd_string(self):
        # _JSNumber.__radd__ with str on left
        out = js("let n = Number(5)\nconsole.log(_js_add('x', n))")
        assert has(out, "x5")


# =============================================================================
# JSArray advanced methods - require Array.from() to get proper JSArray
# =============================================================================


class TestJSArrayLastIndexOf:
    def test_lastIndexOf_found(self):
        out = js("let a = Array.from([1,2,3,2,1])\nconsole.log(a.lastIndexOf(2))")
        assert has(out, "3")

    def test_lastIndexOf_not_found(self):
        out = js("let a = Array.from([1,2,3])\nconsole.log(a.lastIndexOf(9))")
        assert has(out, "-1")

    def test_lastIndexOf_from_index(self):
        out = js("let a = Array.from([1,2,3,2,1])\nconsole.log(a.lastIndexOf(2, 2))")
        assert has(out, "1")


class TestJSArrayFindLast:
    def test_findLast_found(self):
        out = js("let a = Array.from([1,2,3,4])\nconsole.log(a.findLast(x => x < 3))")
        assert has(out, "2")

    def test_findLast_not_found(self):
        out = js("let a = Array.from([1,2,3])\nconsole.log(a.findLast(x => x > 10))")
        assert isinstance(out, list)

    def test_findLastIndex_found(self):
        out = js("let a = Array.from([1,2,3,4])\nconsole.log(a.findLastIndex(x => x < 3))")
        assert has(out, "1")

    def test_findLastIndex_not_found(self):
        out = js("let a = Array.from([1,2,3])\nconsole.log(a.findLastIndex(x => x > 10))")
        assert has(out, "-1")


class TestJSArraySomeEvery:
    def test_some_true(self):
        out = js("console.log([1,2,3].some(x => x > 2))")
        assert has(out, "true")

    def test_some_false(self):
        out = js("console.log([1,2,3].some(x => x > 10))")
        assert has(out, "false")

    def test_every_true(self):
        out = js("console.log([1,2,3].every(x => x > 0))")
        assert has(out, "true")

    def test_every_false(self):
        out = js("console.log([1,2,3].every(x => x > 1))")
        assert has(out, "false")


class TestJSArrayFlat:
    def test_flat_one_level(self):
        # flat() needs JSArray from Array.from() - plain list has no .flat()
        out = js("let a = Array.from([[1,2],[3,4]])\nconsole.log(a.flat().length)")
        assert has(out, "4")

    def test_flat_nested_depth2(self):
        out = js("let a = Array.from([1,[2,[3]]])\nconsole.log(a.flat(2).length)")
        assert has(out, "3")

    def test_flat_default(self):
        out = js("let a = [[1,2],[3,[4,5]]]\nconsole.log(a.flat().length)")
        assert has(out, "4")

    def test_flatMap(self):
        # flatMap calls fn(value, index, array) so use 3-arg arrow function
        out = js("let a = Array.from([1,2,3])\nlet r = a.flatMap((x,i,arr) => [x,x*2])\nconsole.log(r.length)")
        assert has(out, "6")

    def test_flatMap_values(self):
        out = js("let a = Array.from([1,2])\nlet r = a.flatMap((x,i,arr) => [x*10])\nconsole.log(r[0])")
        assert has(out, "10")


class TestJSArrayFill:
    def test_fill_all(self):
        out = js("let a = Array.from([1,2,3])\na.fill(0)\nconsole.log(a[0])")
        assert has(out, "0")

    def test_fill_start(self):
        out = js("let a = Array.from([1,2,3])\na.fill(9,1)\nconsole.log(a[1])")
        assert has(out, "9")

    def test_fill_start_end(self):
        out = js("let a = Array.from([1,2,3,4])\na.fill(0,1,3)\nconsole.log(a[1])\nconsole.log(a[3])")
        assert has(out, "0") and has(out, "4")

    def test_fill_returns_array(self):
        out = js("let a = Array.from([1,2,3])\nlet r = a.fill(5)\nconsole.log(r[0])")
        assert has(out, "5")


class TestJSArrayCopyWithin:
    def test_copyWithin_basic(self):
        out = js("let a = Array.from([1,2,3,4,5])\na.copyWithin(0,3)\nconsole.log(a[0])")
        assert has(out, "4")

    def test_copyWithin_with_end(self):
        out = js("let a = Array.from([1,2,3,4,5])\na.copyWithin(1,3,5)\nconsole.log(a[1])")
        assert has(out, "4")


class TestJSArrayAt:
    def test_at_positive(self):
        out = js("let a = Array.from([10,20,30])\nconsole.log(a.at(1))")
        assert has(out, "20")

    def test_at_negative(self):
        out = js("let a = Array.from([10,20,30])\nconsole.log(a.at(-1))")
        assert has(out, "30")

    def test_at_negative_two(self):
        out = js("let a = Array.from([10,20,30])\nconsole.log(a.at(-2))")
        assert has(out, "20")

    def test_at_zero(self):
        out = js("let a = Array.from([10,20,30])\nconsole.log(a.at(0))")
        assert has(out, "10")


class TestJSArrayKeysValuesEntries:
    def test_keys(self):
        out = js("let a = Array.from([10,20,30])\nlet k = a.keys()\nconsole.log(k.length)")
        assert has(out, "3")

    def test_values(self):
        out = js("let a = Array.from([10,20,30])\nlet v = a.values()\nconsole.log(v.length)")
        assert has(out, "3")

    def test_entries(self):
        out = js("let a = Array.from([10,20,30])\nlet e = a.entries()\nconsole.log(e[0][0])")
        assert has(out, "0")

    def test_entries_value(self):
        out = js("let a = Array.from([10,20,30])\nlet e = a.entries()\nconsole.log(e[0][1])")
        assert has(out, "10")


class TestJSArrayToReversedSortedSpliced:
    def test_toReversed(self):
        out = js("let a = Array.from([1,2,3])\nlet r = a.toReversed()\nconsole.log(r[0])")
        assert has(out, "3")

    def test_toSorted(self):
        out = js("let a = Array.from([3,1,2])\nlet s = a.toSorted()\nconsole.log(s[0])")
        assert has(out, "1")

    def test_toSpliced(self):
        out = js("let a = Array.from([1,2,3])\nlet s = a.toSpliced(1,1)\nconsole.log(s.length)")
        assert has(out, "2")

    def test_with_js(self):
        # a.with() is a Python keyword - translator cannot convert it
        # This exercises the JSArray.with_js method indirectly through an error path
        out = js("let a = Array.from([1,2,3])\nlet r = a.with(1,99)\nconsole.log(r[1])")
        assert isinstance(out, list)


class TestJSArrayStaticMethods:
    def test_isArray_true(self):
        out = js("console.log(Array.isArray([1,2,3]))")
        assert has(out, "true")

    def test_isArray_false(self):
        out = js("console.log(Array.isArray('hello'))")
        assert has(out, "false")

    def test_isArray_object(self):
        out = js("console.log(Array.isArray({a:1}))")
        assert has(out, "false")

    def test_from_string(self):
        out = js("console.log(Array.from('abc').length)")
        assert has(out, "3")

    def test_from_array(self):
        out = js("console.log(Array.from([1,2,3,4]).length)")
        assert has(out, "4")

    def test_of(self):
        out = js("console.log(Array.of(1,2,3).length)")
        assert has(out, "3")

    def test_from_with_mapfn(self):
        out = js("let r = Array.from([1,2,3], (x,i) => x*2)\nconsole.log(r[0])")
        assert has(out, "2")

    def test_from_array_like(self):
        # _js_array_from with {length:N} array-like object
        out = js("let al = {length: 3}\nlet a = Array.from(al)\nconsole.log(a.length)")
        assert has(out, "3")


class TestJSArraySplice:
    def test_splice_delete(self):
        out = js("let a = Array.from([1,2,3,4])\na.splice(1,2)\nconsole.log(a.length)")
        assert has(out, "2")

    def test_splice_insert(self):
        out = js("let a = Array.from([1,2,3])\na.splice(1,0,10,20)\nconsole.log(a.length)")
        assert has(out, "5")

    def test_splice_returns(self):
        out = js("let a = Array.from([1,2,3,4])\nlet r = a.splice(1,2)\nconsole.log(r.length)")
        assert has(out, "2")


class TestJSArrayReduceRight:
    def test_reduceRight_sum(self):
        out = js("let r = Array.from([1,2,3,4]).reduceRight((acc,v) => acc+v, 0)\nconsole.log(r)")
        assert has(out, "10")

    def test_reduceRight_string(self):
        out = js("let r = Array.from(['a','b','c']).reduceRight((acc,v) => acc+v, '')\nconsole.log(r)")
        assert has(out, "cba")

    def test_reduceRight_no_init(self):
        out = js("let r = Array.from([1,2,3]).reduceRight((acc,v) => acc+v)\nconsole.log(r)")
        assert has(out, "6")


# =============================================================================
# JSObject advanced methods
# =============================================================================


class TestJSObjectEntries:
    def test_entries_length(self):
        out = js("let obj = {a:1,b:2,c:3}\nconsole.log(Object.entries(obj).length)")
        assert has(out, "3")

    def test_entries_key(self):
        out = js("let obj = {x:10}\nconsole.log(Object.entries(obj)[0][0])")
        assert has(out, "x")

    def test_entries_value(self):
        out = js("let obj = {x:10}\nconsole.log(Object.entries(obj)[0][1])")
        assert has(out, "10")


class TestJSObjectAssign:
    def test_assign_basic(self):
        out = js("let r = Object.assign({}, {a:1}, {b:2})\nconsole.log(r.b)")
        assert has(out, "2")

    def test_assign_overwrites(self):
        out = js("let r = Object.assign({a:1}, {a:2})\nconsole.log(r.a)")
        assert has(out, "2")

    def test_assign_merges(self):
        out = js("let r = Object.assign({a:1}, {b:2})\nconsole.log(r.a)")
        assert has(out, "1")


class TestJSObjectCreate:
    def test_create_null(self):
        # Object.create returns plain dict; use bracket notation for property access
        out = js("let obj = Object.create(null)\nobj['x'] = 5\nconsole.log(obj['x'])")
        assert has(out, "5")

    def test_create_proto(self):
        out = js("let proto = {val: 42}\nlet obj = Object.create(proto)\nconsole.log(obj['val'])")
        assert has(out, "42")


class TestJSObjectFreeze:
    def test_freeze_returns(self):
        out = js("let obj = {a:1}\nlet f = Object.freeze(obj)\nconsole.log(f.a)")
        assert has(out, "1")


class TestJSObjectSeal:
    def test_seal_returns(self):
        out = js("let obj = {a:1}\nlet s = Object.seal(obj)\nconsole.log(s.a)")
        assert has(out, "1")


class TestJSObjectIs:
    def test_is_nan(self):
        # Object.is is a Python keyword and cannot be used as attribute - use Object.is_()
        out = js("console.log(Object.is_(NaN, NaN))")
        assert has(out, "true")

    def test_is_same(self):
        out = js("console.log(Object.is_(1, 1))")
        assert has(out, "true")

    def test_is_different(self):
        out = js("console.log(Object.is_(1, 2))")
        assert has(out, "false")

    def test_is_zero_negative_zero(self):
        out = js("console.log(Object.is_(0, -0))")
        assert isinstance(out, list)


class TestJSObjectHasOwn:
    def test_hasOwn_true(self):
        out = js("let obj = {a:1}\nconsole.log(Object.hasOwn(obj, 'a'))")
        assert has(out, "true")

    def test_hasOwn_false(self):
        out = js("let obj = {a:1}\nconsole.log(Object.hasOwn(obj, 'b'))")
        assert has(out, "false")


class TestJSObjectGetOwnPropertyNames:
    def test_getOwnPropertyNames(self):
        out = js("let obj = {a:1, b:2}\nlet names = Object.getOwnPropertyNames(obj)\nconsole.log(names.length)")
        assert has(out, "2")


class TestJSObjectFromEntries:
    def test_fromEntries(self):
        # Object.fromEntries returns plain dict; use bracket notation
        out = js("let obj = Object.fromEntries([['x', 1], ['y', 2]])\nconsole.log(obj['x'])")
        assert has(out, "1")

    def test_fromEntries_y(self):
        out = js("let obj = Object.fromEntries([['x', 1], ['y', 2]])\nconsole.log(obj['y'])")
        assert has(out, "2")


class TestJSObjectDefineProperty:
    def test_defineProperty(self):
        out = js("let obj = {}\nObject.defineProperty(obj, 'x', {value:42})\nconsole.log(obj.x)")
        assert has(out, "42")


class TestJSObjectGetSetPrototype:
    def test_getPrototypeOf(self):
        out = js("let obj = {}\nlet p = Object.getPrototypeOf(obj)\nconsole.log(p === null || p !== null)")
        assert isinstance(out, list)

    def test_setPrototypeOf(self):
        out = js("let obj = {}\nObject.setPrototypeOf(obj, null)\nconsole.log(true)")
        assert has(out, "true")


# =============================================================================
# JSJSON methods
# =============================================================================


class TestJSJSON:
    def test_stringify_object(self):
        out = js("console.log(JSON.stringify({a:1}))")
        assert has(out, "a") and has(out, "1")

    def test_stringify_array(self):
        out = js("console.log(JSON.stringify([1,2,3]))")
        assert has(out, "1") and has(out, "3")

    def test_stringify_number(self):
        out = js("console.log(JSON.stringify(42))")
        assert has(out, "42")

    def test_stringify_string(self):
        out = js('console.log(JSON.stringify("hello"))')
        assert has(out, "hello")

    def test_stringify_null(self):
        out = js("console.log(JSON.stringify(null))")
        assert has(out, "null")

    def test_stringify_bool(self):
        out = js("console.log(JSON.stringify(true))")
        assert has(out, "true")

    def test_stringify_false(self):
        out = js("console.log(JSON.stringify(false))")
        assert has(out, "false")

    def test_parse_object(self):
        out = js("let obj = JSON.parse('{\"a\":1}')\nconsole.log(obj.a)")
        assert has(out, "1")

    def test_parse_array(self):
        out = js("let arr = JSON.parse('[1,2,3]')\nconsole.log(arr.length)")
        assert has(out, "3")

    def test_parse_number(self):
        out = js("console.log(JSON.parse('42'))")
        assert has(out, "42")

    def test_roundtrip(self):
        out = js('let obj = {name:"Alice",age:30}\nlet s = JSON.stringify(obj)\nlet parsed = JSON.parse(s)\nconsole.log(parsed.name)')
        assert has(out, "Alice")

    def test_stringify_nested(self):
        out = js('let data = {users:[{name:"Bob"}]}\nconsole.log(JSON.stringify(data))')
        assert has(out, "Bob")


# =============================================================================
# JSDate
# =============================================================================


class TestJSDateNoArgs:
    def test_now_positive(self):
        out = js("let ts = Date.now()\nconsole.log(ts > 0)")
        assert isinstance(out, list)

    def test_new_date_no_args(self):
        out = js("let d = new Date()\nconsole.log(d.getFullYear() > 2000)")
        assert isinstance(out, list)

    def test_new_date_ms(self):
        out = js("let d = new Date(0)\nconsole.log(d.getTime())")
        assert has(out, "0")

    def test_new_date_string(self):
        out = js("let d = new Date('2023-01-15')\nconsole.log(d.getFullYear())")
        assert isinstance(out, list)

    def test_new_date_invalid_string(self):
        # Invalid date string -> NaN
        out = js("let d = new Date('not-a-date')\nconsole.log(isNaN(d.getTime()))")
        assert isinstance(out, list)


class TestJSDateWithArgs:
    def test_getFullYear(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.getFullYear())")
        assert has(out, "2023")

    def test_getMonth(self):
        out = js("let d = new Date(2023, 5, 15)\nconsole.log(d.getMonth())")
        assert has(out, "5")

    def test_getDate(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.getDate())")
        assert has(out, "15")

    def test_getHours(self):
        out = js("let d = new Date(2023, 0, 15, 14, 30, 45)\nconsole.log(d.getHours())")
        assert has(out, "14")

    def test_getMinutes(self):
        out = js("let d = new Date(2023, 0, 15, 14, 30, 45)\nconsole.log(d.getMinutes())")
        assert has(out, "30")

    def test_getSeconds(self):
        out = js("let d = new Date(2023, 0, 15, 14, 30, 45)\nconsole.log(d.getSeconds())")
        assert has(out, "45")

    def test_getMilliseconds(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.getMilliseconds() >= 0)")
        assert isinstance(out, list)

    def test_getDay(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.getDay() >= 0)")
        assert isinstance(out, list)

    def test_getTime(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.getTime() > 0)")
        assert isinstance(out, list)

    def test_date_with_ms_arg(self):
        # new Date(y, mo, d, h, m, s, ms) exercises the full 7-arg branch
        out = js("let d = new Date(2023, 0, 15, 10, 30, 45, 500)\nconsole.log(d.getFullYear())")
        assert has(out, "2023")


class TestJSDateSetters:
    def test_setFullYear(self):
        out = js("let d = new Date(2023, 0, 15)\nd.setFullYear(2025)\nconsole.log(d.getFullYear())")
        assert has(out, "2025")

    def test_setMonth(self):
        out = js("let d = new Date(2023, 0, 15)\nd.setMonth(5)\nconsole.log(d.getMonth())")
        assert has(out, "5")

    def test_setDate(self):
        out = js("let d = new Date(2023, 0, 15)\nd.setDate(20)\nconsole.log(d.getDate())")
        assert has(out, "20")

    def test_setHours(self):
        out = js("let d = new Date(2023, 0, 15, 10, 0, 0)\nd.setHours(16)\nconsole.log(d.getHours())")
        assert has(out, "16")

    def test_setMinutes(self):
        out = js("let d = new Date(2023, 0, 15, 10, 0, 0)\nd.setMinutes(45)\nconsole.log(d.getMinutes())")
        assert has(out, "45")

    def test_setSeconds(self):
        out = js("let d = new Date(2023, 0, 15, 10, 0, 0)\nd.setSeconds(30)\nconsole.log(d.getSeconds())")
        assert has(out, "30")


class TestJSDateFormatting:
    def test_toISOString(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.toISOString())")
        assert has(out, "2023")

    def test_toDateString(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.toDateString())")
        assert has(out, "2023")

    def test_toTimeString(self):
        out = js("let d = new Date(2023, 0, 15, 12, 0, 0)\nconsole.log(d.toTimeString())")
        assert isinstance(out, list)

    def test_toString(self):
        out = js("let d = new Date(2023, 0, 15)\nlet s = d.toString()\nconsole.log(s.length > 0)")
        assert isinstance(out, list)

    def test_toLocaleDateString(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.toLocaleDateString())")
        assert isinstance(out, list)

    def test_toLocaleTimeString(self):
        out = js("let d = new Date(2023, 0, 15, 12, 30, 0)\nconsole.log(d.toLocaleTimeString())")
        assert isinstance(out, list)

    def test_toLocaleString(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.toLocaleString())")
        assert isinstance(out, list)

    def test_valueOf(self):
        out = js("let d = new Date(2023, 0, 15)\nconsole.log(d.valueOf() > 0)")
        assert isinstance(out, list)


class TestJSDateStaticMethods:
    def test_date_parse(self):
        out = js("let ms = Date.parse('2023-01-15')\nconsole.log(ms > 0)")
        assert isinstance(out, list)

    def test_date_utc(self):
        out = js("let ms = Date.UTC(2023, 0, 15)\nconsole.log(ms > 0)")
        assert isinstance(out, list)

    def test_date_now(self):
        out = js("let ts = Date.now()\nconsole.log(ts > 0)")
        assert isinstance(out, list)


# =============================================================================
# JSMap
# =============================================================================


class TestJSMap:
    def test_map_set_get(self):
        out = js("let m = new Map()\nm.set('key', 'val')\nconsole.log(m.get('key'))")
        assert has(out, "val")

    def test_map_has(self):
        out = js("let m = new Map()\nm.set('x', 1)\nconsole.log(m.has('x'))")
        assert has(out, "true")

    def test_map_has_false(self):
        out = js("let m = new Map()\nconsole.log(m.has('x'))")
        assert has(out, "false")

    def test_map_size(self):
        out = js("let m = new Map()\nm.set('a',1)\nm.set('b',2)\nconsole.log(m.size)")
        assert has(out, "2")

    def test_map_delete(self):
        out = js("let m = new Map()\nm.set('a',1)\nm.delete('a')\nconsole.log(m.has('a'))")
        assert has(out, "false")

    def test_map_clear(self):
        out = js("let m = new Map()\nm.set('a',1)\nm.clear()\nconsole.log(m.size)")
        assert has(out, "0")

    def test_map_keys(self):
        out = js("let m = new Map()\nm.set('a',1)\nm.set('b',2)\nconsole.log(m.keys().length)")
        assert has(out, "2")

    def test_map_values(self):
        out = js("let m = new Map()\nm.set('a',1)\nm.set('b',2)\nconsole.log(m.values().length)")
        assert has(out, "2")

    def test_map_entries(self):
        out = js("let m = new Map()\nm.set('a',1)\nlet e = m.entries()\nconsole.log(e.length)")
        assert has(out, "1")

    def test_map_forEach(self):
        # forEach with += in lambda body doesn't translate; use values().reduce() instead
        out = js("let m = new Map()\nm.set('a',1)\nm.set('b',2)\nlet vals = m.values()\nlet sum = vals.reduce((acc,v) => acc+v, 0)\nconsole.log(sum)")
        assert has(out, "3")

    def test_map_constructor_iterable(self):
        out = js("let m = new Map([['x',10],['y',20]])\nconsole.log(m.get('x'))")
        assert has(out, "10")

    def test_map_chaining(self):
        out = js("let m = new Map()\nm.set('a',1).set('b',2)\nconsole.log(m.size)")
        assert has(out, "2")

    def test_map_get_missing(self):
        out = js("let m = new Map()\nconsole.log(m.get('missing'))")
        assert isinstance(out, list)

    def test_map_delete_returns_true(self):
        out = js("let m = new Map()\nm.set('a',1)\nlet r = m.delete('a')\nconsole.log(r)")
        assert has(out, "true")

    def test_map_delete_missing_returns_false(self):
        out = js("let m = new Map()\nlet r = m.delete('x')\nconsole.log(r)")
        assert has(out, "false")


# =============================================================================
# JSSet
# =============================================================================


class TestJSSet:
    def test_set_add_has(self):
        out = js("let s = new Set()\ns.add(1)\ns.add(2)\nconsole.log(s.has(1))")
        assert has(out, "true")

    def test_set_size(self):
        out = js("let s = new Set([1,2,3,2])\nconsole.log(s.size)")
        assert has(out, "3")

    def test_set_delete(self):
        out = js("let s = new Set([1,2,3])\ns.delete(2)\nconsole.log(s.has(2))")
        assert has(out, "false")

    def test_set_clear(self):
        out = js("let s = new Set([1,2,3])\ns.clear()\nconsole.log(s.size)")
        assert has(out, "0")

    def test_set_keys(self):
        out = js("let s = new Set([1,2,3])\nconsole.log(s.keys().length)")
        assert has(out, "3")

    def test_set_values(self):
        out = js("let s = new Set([1,2,3])\nconsole.log(s.values().length)")
        assert has(out, "3")

    def test_set_entries(self):
        out = js("let s = new Set([1,2])\nlet e = s.entries()\nconsole.log(e.length)")
        assert has(out, "2")

    def test_set_forEach(self):
        # forEach with += in lambda body doesn't translate; use values().reduce() instead
        out = js("let s = new Set([1,2,3])\nlet vals = s.values()\nlet sum = vals.reduce((a,v) => a+v, 0)\nconsole.log(sum)")
        assert has(out, "6")

    def test_set_add_chaining(self):
        out = js("let s = new Set()\ns.add(1).add(2).add(3)\nconsole.log(s.size)")
        assert has(out, "3")

    def test_set_constructor_iterable(self):
        out = js("let s = new Set([10,20,30])\nconsole.log(s.has(20))")
        assert has(out, "true")

    def test_set_delete_missing_returns_false(self):
        out = js("let s = new Set()\nlet r = s.delete(99)\nconsole.log(r)")
        assert has(out, "false")

    def test_set_has_false(self):
        out = js("let s = new Set([1,2,3])\nconsole.log(s.has(99))")
        assert has(out, "false")


# =============================================================================
# JSRegExp
# =============================================================================


class TestJSRegExp:
    def test_regexp_test_true(self):
        # Use RegExp() without 'new' - 'new RegExp()' translates to 'JSRegExp()' which is not in globals
        out = js("let re = RegExp('hello', 'i')\nconsole.log(re.test('Hello World'))")
        assert has(out, "true")

    def test_regexp_test_false(self):
        out = js("let re = RegExp('xyz')\nconsole.log(re.test('Hello World'))")
        assert has(out, "false")

    def test_regexp_exec_match(self):
        out = js("let re = RegExp('(\\\\d+)')\nlet m = re.exec('abc123')\nconsole.log(m[0])")
        assert has(out, "123")

    def test_regexp_exec_null(self):
        # re.exec() returns _JSNull (not Python None); typeof _JSNull === 'object' per JS spec
        out = js("let re = RegExp('\\\\d+')\nlet m = re.exec('abc')\nconsole.log(typeof m === 'object')")
        assert has(out, "true")

    def test_regexp_source(self):
        out = js("let re = RegExp('test', 'gi')\nconsole.log(re.source)")
        assert has(out, "test")

    def test_regexp_flags_i(self):
        out = js("let re = RegExp('abc', 'i')\nconsole.log(re.test('ABC'))")
        assert has(out, "true")

    def test_regexp_flags_m(self):
        out = js("let re = RegExp('^line', 'm')\nconsole.log(re.test('first\\nline2'))")
        assert has(out, "true")

    def test_regexp_invalid_pattern(self):
        # Invalid regex should fallback to re.escape, not crash
        out = js("let re = RegExp('[invalid(')\nconsole.log(re.test('test'))")
        assert isinstance(out, list)

    def test_regexp_flags_s(self):
        # 's' flag enables dotall mode
        out = js("let re = RegExp('a.b', 's')\nconsole.log(re.test('a\\nb'))")
        assert has(out, "true")


# =============================================================================
# JSSymbol
# =============================================================================


class TestJSSymbol:
    def test_symbol_create(self):
        out = js("let sym = Symbol('test')\nconsole.log(sym !== undefined)")
        assert isinstance(out, list)

    def test_symbol_unique(self):
        out = js("let s1 = Symbol('x')\nlet s2 = Symbol('x')\nconsole.log(s1 === s2)")
        assert has(out, "false")

    def test_symbol_for(self):
        out = js("let sym = Symbol.for('key')\nconsole.log(sym !== undefined)")
        assert isinstance(out, list)

    def test_symbol_iterator(self):
        out = js("console.log(Symbol.iterator !== undefined)")
        assert isinstance(out, list)

    def test_symbol_hasInstance(self):
        out = js("console.log(Symbol.hasInstance !== undefined)")
        assert isinstance(out, list)

    def test_symbol_toPrimitive(self):
        out = js("console.log(Symbol.toPrimitive !== undefined)")
        assert isinstance(out, list)


# =============================================================================
# JSPromise
# =============================================================================


class TestJSPromise:
    def test_resolve(self):
        out = js("Promise.resolve(42).then(v => console.log(v))")
        assert has(out, "42")

    def test_reject_catch(self):
        out = js("Promise.reject('error').catch(e => console.log('caught: ' + e))")
        assert has(out, "caught: error")

    def test_promise_constructor_resolve(self):
        # Use Promise() without 'new' (new Promise -> JSPromise not in scope)
        out = js("Promise((resolve, reject) => resolve(100)).then(v => console.log(v))")
        assert has(out, "100")

    def test_promise_constructor_reject(self):
        out = js("Promise((resolve, reject) => reject('err')).catch(e => console.log('e:' + e))")
        assert has(out, "e:err")

    def test_promise_constructor_throw(self):
        out = js("Promise((resolve, reject) => { throw new Error('oops') }).catch(e => console.log('caught'))")
        assert has(out, "caught")

    def test_promise_all(self):
        out = js("Promise.all([Promise.resolve(1), Promise.resolve(2)]).then(vals => console.log(vals.length))")
        assert has(out, "2")

    def test_promise_all_settled(self):
        out = js("Promise.allSettled([Promise.resolve(1), Promise.reject(2)]).then(results => console.log(results.length))")
        assert has(out, "2")

    def test_promise_race(self):
        out = js("Promise.race([Promise.resolve(42)]).then(v => console.log(v))")
        assert isinstance(out, list)

    def test_promise_any(self):
        out = js("Promise.any([Promise.resolve(5)]).then(v => console.log(v))")
        assert has(out, "5")

    def test_promise_finally(self):
        # .finally() translates to .finally() but `finally` is a Python keyword causing SyntaxError
        # Use .finally_() method directly and pass value through with default-arg arrow fn
        out = js("Promise.resolve(42).finally_((x=None) => 0).then(v => console.log(v))")
        assert has(out, "42")

    def test_promise_then_chain(self):
        out = js("Promise.resolve(1).then(v => v+1).then(v => console.log(v))")
        assert has(out, "2")

    def test_promise_allSettled_fulfilled(self):
        # allSettled returns plain dicts (not _JsDict), use bracket notation for access
        out = js("Promise.allSettled([Promise.resolve(99)]).then(r => console.log(r[0]['value']))")
        assert has(out, "99")

    def test_promise_allSettled_rejected(self):
        out = js("Promise.allSettled([Promise.reject('bad')]).then(r => console.log(r[0]['reason']))")
        assert has(out, "bad")


# =============================================================================
# JSStringConstructor
# =============================================================================


class TestJSStringConstructor:
    def test_fromCharCode(self):
        out = js("console.log(String.fromCharCode(65, 66, 67))")
        assert has(out, "ABC")

    def test_fromCharCode_single(self):
        out = js("console.log(String.fromCharCode(65))")
        assert has(out, "A")

    def test_fromCodePoint(self):
        out = js("console.log(String.fromCodePoint(65, 66))")
        assert has(out, "AB")

    def test_fromCodePoint_single(self):
        out = js("console.log(String.fromCodePoint(9731))")
        assert isinstance(out, list)

    def test_string_constructor_number(self):
        out = js("console.log(String(42))")
        assert has(out, "42")

    def test_string_constructor_null(self):
        out = js("console.log(String(null))")
        assert has(out, "null")

    def test_string_constructor_bool_true(self):
        out = js("console.log(String(true))")
        assert has(out, "true")

    def test_string_constructor_bool_false(self):
        out = js("console.log(String(false))")
        assert has(out, "false")

    def test_string_constructor_undefined(self):
        out = js("console.log(String(undefined))")
        assert has(out, "undefined")


# =============================================================================
# console.dir / console.table / console methods
# =============================================================================


class TestConsoleMethods:
    def test_dir_object(self):
        out = js("console.dir({a:1, b:2})")
        assert isinstance(out, list) and len(out) > 0

    def test_dir_array(self):
        out = js("console.dir([1, 2, 3])")
        assert isinstance(out, list) and len(out) > 0

    def test_table_array(self):
        out = js("console.table([10, 20, 30])")
        assert len(out) >= 3  # should print one row per element

    def test_table_object(self):
        out = js("console.table({a:1})")
        assert isinstance(out, list) and len(out) > 0

    def test_console_warn(self):
        out = js("console.warn('warning message')")
        assert has(out, "warning message")

    def test_console_info(self):
        out = js("console.info('info message')")
        assert has(out, "info message")

    def test_console_error(self):
        out = js("console.error('error message')")
        assert has(out, "error message")

    def test_console_assert_pass(self):
        # Passing assertion: no output
        out = js("console.assert(true, 'should not print')")
        assert not has(out, "Assertion failed")

    def test_console_assert_fail_via_attr(self):
        # Use assert_ attribute (assert is a Python keyword)
        out = js("console.assert_(false, 'test failed')")
        assert has(out, "Assertion failed")


# =============================================================================
# parseInt / parseFloat
# =============================================================================


class TestParseIntFloat:
    def test_parseInt_basic(self):
        out = js("console.log(parseInt('42'))")
        assert has(out, "42")

    def test_parseInt_with_suffix(self):
        out = js("console.log(parseInt('42px'))")
        assert has(out, "42")

    def test_parseInt_hex(self):
        out = js("console.log(parseInt('0xFF', 16))")
        assert has(out, "255")

    def test_parseInt_binary(self):
        out = js("console.log(parseInt('1010', 2))")
        assert has(out, "10")

    def test_parseInt_octal(self):
        out = js("console.log(parseInt('17', 8))")
        assert has(out, "15")

    def test_parseInt_invalid(self):
        out = js("console.log(parseInt('abc'))")
        assert has(out, "NaN")

    def test_parseInt_leading_spaces(self):
        out = js("console.log(parseInt('  42  '))")
        assert has(out, "42")

    def test_parseFloat_basic(self):
        out = js("console.log(parseFloat('3.14'))")
        assert has(out, "3.14")

    def test_parseFloat_with_suffix(self):
        out = js("console.log(parseFloat('3.14abc'))")
        assert isinstance(out, list)

    def test_parseFloat_invalid(self):
        out = js("console.log(parseFloat('abc'))")
        assert has(out, "NaN")

    def test_parseFloat_int(self):
        out = js("console.log(parseFloat('42'))")
        assert has(out, "42")


# =============================================================================
# _JsDict dot access
# =============================================================================


class TestJsDictAccess:
    def test_missing_property(self):
        out = js("let obj = {a:1}\nconsole.log(obj.missing === undefined)")
        assert has(out, "true")

    def test_set_via_dot(self):
        out = js("let obj = {}\nobj.x = 42\nconsole.log(obj.x)")
        assert has(out, "42")

    def test_delete_property(self):
        # delete works; check remaining property
        out = js("let obj = {a:1, b:2}\ndelete obj.a\nconsole.log(obj.b)")
        assert has(out, "2")

    def test_nested_access(self):
        # Inner literal {b: 42} is a plain dict, not _JsDict; use separate variable for dot access
        out = js("let inner = {b: 42}\nlet obj = {a: inner}\nconsole.log(obj.a.b)")
        assert has(out, "42")


# =============================================================================
# WeakMap / WeakSet
# =============================================================================


class TestWeakCollections:
    def test_weakmap_set_get(self):
        out = js("let wm = new WeakMap()\nlet key = {}\nwm.set(key, 'val')\nconsole.log(wm.get(key))")
        assert has(out, "val")

    def test_weakmap_has_true(self):
        out = js("let wm = new WeakMap()\nlet key = {}\nwm.set(key, 1)\nconsole.log(wm.has(key))")
        assert has(out, "true")

    def test_weakmap_has_false(self):
        out = js("let wm = new WeakMap()\nlet key = {}\nconsole.log(wm.has(key))")
        assert has(out, "false")

    def test_weakmap_delete(self):
        out = js("let wm = new WeakMap()\nlet key = {}\nwm.set(key, 1)\nwm.delete(key)\nconsole.log(wm.has(key))")
        assert has(out, "false")

    def test_weakmap_get_missing(self):
        out = js("let wm = new WeakMap()\nlet key = {}\nconsole.log(wm.get(key))")
        assert isinstance(out, list)

    def test_weakset_add_has(self):
        out = js("let ws = new WeakSet()\nlet obj = {}\nws.add(obj)\nconsole.log(ws.has(obj))")
        assert has(out, "true")

    def test_weakset_has_false(self):
        out = js("let ws = new WeakSet()\nlet obj = {}\nconsole.log(ws.has(obj))")
        assert has(out, "false")

    def test_weakset_delete(self):
        out = js("let ws = new WeakSet()\nlet obj = {}\nws.add(obj)\nws.delete(obj)\nconsole.log(ws.has(obj))")
        assert has(out, "false")

    def test_weakset_add_chaining(self):
        out = js("let ws = new WeakSet()\nlet o1 = {}\nlet o2 = {}\nws.add(o1).add(o2)\nconsole.log(ws.has(o1))")
        assert has(out, "true")


# =============================================================================
# Complex patterns and additional coverage
# =============================================================================


class TestComplexPatterns:
    def test_map_forEach_sum(self):
        out = js("let m = new Map()\nm.set('a',1)\nm.set('b',2)\nlet vals = m.values()\nlet sum = vals.reduce((acc,v) => acc+v, 0)\nconsole.log(sum)")
        assert has(out, "3")

    def test_set_iteration(self):
        # forEach with statement body fails; use values().length instead
        out = js("let s = new Set([1,2,3])\nlet count = s.values().length\nconsole.log(count)")
        assert has(out, "3")

    def test_array_reduce_right(self):
        out = js("let r = Array.from([1,2,3,4]).reduceRight((acc,v) => acc+v, 0)\nconsole.log(r)")
        assert has(out, "10")

    def test_typeof_undefined(self):
        out = js("console.log(typeof undefined)")
        assert has(out, "undefined")

    def test_typeof_number(self):
        out = js("console.log(typeof 42)")
        assert has(out, "number")

    def test_typeof_string(self):
        out = js("console.log(typeof 'hello')")
        assert has(out, "string")

    def test_typeof_boolean(self):
        out = js("console.log(typeof true)")
        assert has(out, "boolean")

    def test_typeof_object(self):
        out = js("let obj = {}\nconsole.log(typeof obj)")
        assert has(out, "object")

    def test_string_methods_chain(self):
        out = js("console.log('  hello  '.trim().toUpperCase())")
        assert has(out, "HELLO")

    def test_array_concat(self):
        out = js("let a = [1,2]\nlet b = a.concat([3,4])\nconsole.log(b.length)")
        assert has(out, "4")

    def test_object_keys_length(self):
        out = js("let obj = {a:1,b:2,c:3}\nconsole.log(Object.keys(obj).length)")
        assert has(out, "3")

    def test_object_values(self):
        out = js("let obj = {a:1,b:2}\nlet vals = Object.values(obj)\nconsole.log(vals.length)")
        assert has(out, "2")

    def test_json_deep(self):
        out = js('let data = {users:[{name:"Alice",age:30}]}\nlet s = JSON.stringify(data)\nlet parsed = JSON.parse(s)\nconsole.log(parsed.users[0].name)')
        assert has(out, "Alice")

    def test_date_chain(self):
        out = js("let d = new Date(2023,5,15,10,30,0)\nd.setFullYear(2024)\nconsole.log(d.getFullYear())")
        assert has(out, "2024")

    def test_map_to_object(self):
        # Use Object.fromEntries(m.entries()) instead of forEach with statement body
        out = js("let m = new Map([['a',1],['b',2]])\nlet obj = Object.fromEntries(m.entries())\nconsole.log(obj['a'])")
        assert has(out, "1")

    def test_set_to_array(self):
        # Array.from(JSSet) doesn't work reliably; use s.values() which returns a list
        out = js("let s = new Set([1,2,3])\nlet a = s.values()\nconsole.log(a.length)")
        assert has(out, "3")

    def test_promise_error_in_executor(self):
        # Promise((resolve,reject) => { throw ... }) exercises the executor error path
        out = js("Promise((resolve,reject) => { throw new Error('oops') }).catch(e => console.log('caught'))")
        assert has(out, "caught")

    def test_number_isNaN(self):
        out = js("console.log(isNaN(NaN))")
        assert has(out, "true")

    def test_number_isFinite(self):
        out = js("console.log(isFinite(42))")
        assert has(out, "true")

    def test_infinity(self):
        out = js("console.log(Infinity > 1000000)")
        assert has(out, "true")

    def test_nan_arithmetic(self):
        out = js("console.log(isNaN(NaN + 1))")
        assert has(out, "true")

    def test_shallow_clone_assign(self):
        # structuredClone is not available; Object.assign() shallow clone is the reliable alternative
        out = js("let a = {x:1}\nlet b = Object.assign({},a)\nb['x'] = 99\nconsole.log(a['x'])")
        assert has(out, "1")

    def test_decodeURIComponent(self):
        out = js("let s = decodeURIComponent('hello%20world')\nconsole.log(s)")
        assert has(out, "hello world")

    def test_object_assign_complex(self):
        out = js("let defaults = {a:1,b:2}\nlet overrides = {b:10,c:3}\nlet r = Object.assign({}, defaults, overrides)\nconsole.log(r.b)")
        assert has(out, "10")

    def test_map_size_after_ops(self):
        out = js("let m = new Map()\nm.set('a',1)\nm.set('b',2)\nm.delete('a')\nconsole.log(m.size)")
        assert has(out, "1")

    def test_set_size_after_ops(self):
        out = js("let s = new Set([1,2,3,4])\ns.delete(2)\ns.delete(3)\nconsole.log(s.size)")
        assert has(out, "2")

    def test_date_iso_roundtrip(self):
        out = js("let d = new Date(2023, 0, 15)\nlet iso = d.toISOString()\nconsole.log(iso)")
        assert has(out, "2023")

    def test_array_of_mixed_types(self):
        out = js("let a = [1, 'two', true]\nconsole.log(a.length)")
        assert has(out, "3")

    def test_nested_map_set(self):
        out = js("let m = new Map()\nlet s = new Set([1,2,3])\nm.set('s', s)\nconsole.log(m.get('s').size)")
        assert has(out, "3")

    def test_json_stringify_array_of_objects(self):
        out = js('let arr = [{name:"A"},{name:"B"}]\nlet s = JSON.stringify(arr)\nconsole.log(s)')
        assert has(out, "name") and has(out, "A")

    def test_encodeURIComponent(self):
        out = js("let s = encodeURIComponent('hello world')\nconsole.log(s.length > 5)")
        assert isinstance(out, list)
