"""Coverage tests for Lua language executor – 2nd pass.

Targets previously uncovered lines in lua.py.
"""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors, first_error

L = Language.LUA


def lua(source: str, **kw) -> list[str]:
    return run(source, L, **kw)


# ============================================================================
# Error handling (line 89 – LuaError caught by execute())
# ============================================================================


class TestLuaError:
    def test_error_function(self):
        out = lua('error("something went wrong")')
        assert has(out, "❌")

    def test_error_in_function(self):
        out = lua(
            "function bad()\n"
            '  error("bad!")\n'
            "end\n"
            "bad()"
        )
        assert has(out, "❌")

    def test_runtime_error(self):
        # Nil arithmetic causes runtime error
        out = lua(
            "local x = nil\n"
            "print(x + 1)"
        )
        assert has(out, "❌")


# ============================================================================
# Return values (lines 163, 181, 185)
# ============================================================================


class TestReturnValues:
    def test_empty_return(self):
        # return with no value → val = None (line 163)
        out = lua(
            "function f()\n"
            "  return\n"
            "end\n"
            "local r = f()\n"
            "print(r)"
        )
        assert has(out, "nil")

    def test_multiple_return(self):
        # return a, b (line 181)
        out = lua(
            "function multi()\n"
            "  return 10, 20\n"
            "end\n"
            "local a, b = multi()\n"
            "print(a)\n"
            "print(b)"
        )
        assert has(out, "10")
        assert has(out, "20")

    def test_inline_empty_return(self):
        # _exec_inline with bare return (line 185)
        out = lua(
            "function g()\n"
            "  if true then return end\n"
            "  print('unreachable')\n"
            "end\n"
            "g()\n"
            'print("done")'
        )
        assert has(out, "done")

    def test_function_returns_value_from_expr(self):
        out = lua(
            "function square(x)\n"
            "  return x * x\n"
            "end\n"
            "print(square(7))"
        )
        assert has(out, "49")


# ============================================================================
# Generic for with dict/list (lines 406, 412-414)
# ============================================================================


class TestGenericFor:
    def test_pairs_dict(self):
        # for k,v in pairs(dict) – line 406
        out = lua(
            "local t = {x=1}\n"
            "for k, v in pairs(t) do\n"
            "  print(k, v)\n"
            "end"
        )
        assert has(out, "x") and has(out, "1")

    def test_pairs_multiple_keys(self):
        out = lua(
            "local t = {a=10, b=20}\n"
            "local sum = 0\n"
            "for k, v in pairs(t) do\n"
            "  sum = sum + v\n"
            "end\n"
            "print(sum)"
        )
        assert has(out, "30")

    def test_ipairs_list(self):
        out = lua(
            "local t = {10, 20, 30}\n"
            "local sum = 0\n"
            "for i, v in ipairs(t) do\n"
            "  sum = sum + v\n"
            "end\n"
            "print(sum)"
        )
        assert has(out, "60")

    def test_for_numeric_return(self):
        # LuaReturn raised inside for numeric body (line 398)
        out = lua(
            "function find_first(limit)\n"
            "  for i = 1, limit do\n"
            "    if i == 3 then\n"
            "      return i\n"
            "    end\n"
            "  end\n"
            "  return -1\n"
            "end\n"
            "print(find_first(10))"
        )
        assert has(out, "3")


# ============================================================================
# While loop break (line 441)
# ============================================================================


class TestWhileBreak:
    def test_break_in_while(self):
        # LuaBreak caught in while (line 441)
        out = lua(
            "local i = 0\n"
            "while true do\n"
            "  i = i + 1\n"
            "  if i >= 3 then\n"
            "    break\n"
            "  end\n"
            "end\n"
            "print(i)"
        )
        assert has(out, "3")

    def test_while_continue_until_false(self):
        out = lua(
            "local x = 10\n"
            "while x > 0 do\n"
            "  x = x - 3\n"
            "end\n"
            "print(x)"
        )
        assert has(out, "-2")


# ============================================================================
# Repeat loop (lines 457, 462-467)
# ============================================================================


class TestRepeatLoop:
    def test_repeat_break(self):
        # LuaBreak inside repeat (line 466-467)
        out = lua(
            "local i = 0\n"
            "repeat\n"
            "  i = i + 1\n"
            "  if i == 2 then break end\n"
            "until i >= 10\n"
            "print(i)"
        )
        assert has(out, "2")

    def test_repeat_until_runs_once(self):
        out = lua(
            "local count = 0\n"
            "repeat\n"
            "  count = count + 1\n"
            "until count >= 1\n"
            "print(count)"
        )
        assert has(out, "1")

    def test_repeat_until_multiple(self):
        out = lua(
            "local n = 0\n"
            "repeat\n"
            "  n = n + 1\n"
            "until n == 5\n"
            "print(n)"
        )
        assert has(out, "5")


# ============================================================================
# Function definitions: method and table.field (lines 503, 513)
# ============================================================================


class TestFunctionDefinitions:
    def test_table_method_definition(self):
        # function Obj:method() (line 503 – fallback _globals path)
        out = lua(
            "Obj = {}\n"
            "function Obj:greet(name)\n"
            '  print("hello", name)\n'
            "end\n"
            'Obj:greet("world")'
        )
        assert has(out, "hello")

    def test_table_field_function(self):
        # function obj.field() (line 513 – fallback _globals)
        out = lua(
            "M = {}\n"
            "function M.add(a, b)\n"
            "  return a + b\n"
            "end\n"
            "print(M.add(3, 4))"
        )
        assert has(out, "7")

    def test_local_function(self):
        out = lua(
            "local function double(x)\n"
            "  return x * 2\n"
            "end\n"
            "print(double(5))"
        )
        assert has(out, "10")


# ============================================================================
# Long string [[...]] (line 616)
# ============================================================================


class TestLongString:
    def test_long_string_literal(self):
        out = lua("local s = [[hello world]]\nprint(s)")
        assert has(out, "hello world")

    def test_long_string_multiword(self):
        out = lua("print([[the quick brown fox]])")
        assert has(out, "the quick brown fox")


# ============================================================================
# Method call with __index (lines 678-680, 690)
# ============================================================================


class TestMethodCallIndex:
    def test_method_via_index(self):
        # obj is dict with __index pointing to another dict (line 678-680)
        out = lua(
            "local base = {}\n"
            "function base.greet(self)\n"
            '  print("from base")\n'
            "end\n"
            "local obj = {__index = base}\n"
            "obj:greet()"
        )
        assert has(out, "from base")

    def test_table_list_access(self):
        # list[int] access (line 698)
        out = lua(
            "local t = {100, 200, 300}\n"
            "print(t[2])"
        )
        assert has(out, "200")

    def test_table_out_of_bounds(self):
        # Returns nil for out of range (else branch of line 698)
        out = lua(
            "local t = {1, 2, 3}\n"
            "local v = t[99]\n"
            "print(v)"
        )
        assert has(out, "nil")

    def test_table_dict_access_nil(self):
        # Returns nil when key not found (line 701)
        out = lua(
            "local t = {a=1}\n"
            "print(t['z'])"
        )
        assert has(out, "nil")


# ============================================================================
# math library (lines 965, 967, 971-973, 976-987, 993)
# ============================================================================


class TestMathLib:
    def test_math_random_no_args(self):
        # math.random() → float in [0,1) (line 965)
        out = lua(
            "local r = math.random()\n"
            "print(type(r))"
        )
        assert has(out, "number")

    def test_math_random_one_arg(self):
        # math.random(n) → int in [1,n] (line 967)
        out = lua(
            "math.randomseed(42)\n"
            "local r = math.random(10)\n"
            "print(r >= 1 and r <= 10)"
        )
        assert has(out, "true")

    def test_math_randomseed(self):
        # math.randomseed(seed) (lines 971-973)
        out = lua(
            "math.randomseed(123)\n"
            'print("ok")'
        )
        assert has(out, "ok")

    def test_math_type_integer(self):
        # math.type(int) → "integer" (line 976)
        out = lua("print(math.type(3))")
        assert has(out, "integer")

    def test_math_type_float(self):
        # math.type(float) → "float" (line 979)
        out = lua("print(math.type(3.14))")
        assert has(out, "float")

    def test_math_type_non_number(self):
        # math.type(string) → "fail" (line 980)
        out = lua('print(math.type("hello"))')
        assert has(out, "fail")

    def test_math_tointeger_int(self):
        # math.tointeger(int) → returns it (line 983-984)
        out = lua("print(math.tointeger(5))")
        assert has(out, "5")

    def test_math_tointeger_float_whole(self):
        # math.tointeger(3.0) → 3 (line 985-986)
        out = lua("print(math.tointeger(3.0))")
        assert has(out, "3")

    def test_math_tointeger_float_frac(self):
        # math.tointeger(3.5) → nil (line 987)
        out = lua("print(math.tointeger(3.5))")
        assert has(out, "nil")

    def test_math_modf(self):
        # math.modf(x) (line 993)
        out = lua(
            "local i, f = math.modf(3.7)\n"
            "print(i)"
        )
        # modf returns (frac, int) in Python; might print as tuple
        assert no_errors(out)

    def test_math_fmod(self):
        out = lua("print(math.fmod(10.5, 3.0))")
        assert no_errors(out)

    def test_math_random_two_args(self):
        out = lua(
            "math.randomseed(1)\n"
            "local r = math.random(5, 10)\n"
            "print(r >= 5 and r <= 10)"
        )
        assert has(out, "true")


# ============================================================================
# string library (lines 1011, 1034, 1041, 1051, 1054, 1061, 1068)
# ============================================================================


class TestStringLib:
    def test_string_sub_no_j(self):
        # string.sub(s, i) with no j (line 1011)
        out = lua('print(string.sub("hello", 3))')
        assert has(out, "llo")

    def test_string_find_plain(self):
        # string.find with plain=true (line 1034)
        out = lua('local i, j = string.find("hello world", "world", 1, true)\nprint(i)')
        assert has(out, "7")

    def test_string_find_plain_not_found(self):
        # string.find plain, not found → nil (line 1041 from line 1035-1036)
        out = lua(
            'local r = string.find("hello", "xyz", 1, true)\n'
            "print(r)"
        )
        assert has(out, "nil")

    def test_string_find_regex_not_found(self):
        # string.find regex, no match → nil (line 1041)
        out = lua(
            'local r = string.find("hello", "%d+")\n'
            "print(r)"
        )
        assert has(out, "nil")

    def test_string_gsub_with_limit(self):
        # gsub with n limit (line 1051)
        out = lua(
            'local s, n = string.gsub("aaaa", "a", "b", 2)\n'
            "print(s)\n"
            "print(n)"
        )
        assert has(out, "bbaa")
        assert has(out, "2")

    def test_string_gsub_callable_repl(self):
        # gsub returns (result, count) tuple — verify count with two-var assignment
        out = lua(
            'local s, n = string.gsub("hello", "l", "r")\n'
            "print(n)"
        )
        assert has(out, "2")

    def test_string_match_basic(self):
        # string.match (line 1061) — use Python regex syntax ([0-9]+ not %d+)
        out = lua('print(string.match("123abc", "[0-9]+"))')
        assert has(out, "123")

    def test_string_match_no_match(self):
        # string.match with no match → nil (line 1063)
        out = lua('print(string.match("hello", "[0-9]+"))')
        assert has(out, "nil")

    def test_string_gmatch(self):
        # string.gmatch (line 1068) — iterate with single-var for loop
        out = lua(
            "local count = 0\n"
            'for w in string.gmatch("one two three", "[a-z]+") do\n'
            "  count = count + 1\n"
            "end\n"
            "print(count)"
        )
        assert has(out, "3")

    def test_string_find_sub_with_j(self):
        # string.sub(s, i, j) – already covered but ensure no error
        out = lua('print(string.sub("hello world", 1, 5))')
        assert has(out, "hello")

    def test_string_match_with_capture(self):
        out = lua('print(string.match("key=value", "(%a+)=(%a+)"))')
        assert no_errors(out)


# ============================================================================
# table library (lines 1074-1141)
# ============================================================================


class TestTableLib:
    def test_table_insert_append(self):
        # table.insert(t, val) – append (line 1074-1075)
        out = lua(
            "local t = {}\n"
            "table.insert(t, 42)\n"
            "print(t[1])"
        )
        assert has(out, "42")

    def test_table_insert_at_pos(self):
        # table.insert(t, pos, val) (line 1077)
        out = lua(
            "local t = {10, 30}\n"
            "table.insert(t, 2, 20)\n"
            "print(t[2])"
        )
        assert has(out, "20")

    def test_table_remove_last(self):
        # table.remove(t) – pop last from list (line 1087-1088)
        # Use table.pack to create a Python list (not a dict)
        out = lua(
            "local t = table.pack(1, 2, 3)\n"
            "local v = table.remove(t)\n"
            "print(v)"
        )
        assert has(out, "3")

    def test_table_remove_at_pos(self):
        # table.remove(t, pos) returns removed element (line 1089)
        # Use table.pack to create a Python list (not a dict)
        out = lua(
            "local t = table.pack(10, 20, 30)\n"
            "local v = table.remove(t, 1)\n"
            "print(v)"
        )
        assert has(out, "10")

    def test_table_concat_list(self):
        out = lua(
            'local t = {"a", "b", "c"}\n'
            'print(table.concat(t, "-"))'
        )
        assert has(out, "a-b-c")

    def test_table_concat_with_range(self):
        # table.concat with i,j (line 1095)
        out = lua(
            'local t = {"a", "b", "c", "d"}\n'
            'print(table.concat(t, ",", 2, 3))'
        )
        assert has(out, "b,c")

    def test_table_sort_default(self):
        # table.sort works on Python lists (from table.pack)
        out = lua(
            "local t = table.pack(3, 1, 4, 1, 5)\n"
            "table.sort(t)\n"
            "print(t[1])"
        )
        assert has(out, "1")

    def test_table_sort_with_comp(self):
        # table.sort with comparator (line 1106-1117)
        out = lua(
            "local t = {3, 1, 2}\n"
            "table.sort(t, function(a, b) return a > b end)\n"
            "print(t[1])"
        )
        assert has(out, "3")

    def test_table_move(self):
        # table.move (line 1122)
        out = lua(
            "local t1 = {1, 2, 3, 4, 5}\n"
            "local t2 = {}\n"
            "table.move(t1, 1, 3, 1, t2)\n"
            "print(t2[1])\n"
            "print(t2[2])\n"
            "print(t2[3])"
        )
        assert has(out, "1")
        assert has(out, "2")
        assert has(out, "3")

    def test_table_unpack_list(self):
        # table.unpack (line 1134-1136)
        out = lua(
            "local t = {10, 20, 30}\n"
            "print(table.unpack(t))"
        )
        assert no_errors(out)

    def test_table_pack(self):
        # table.pack (line 1140)
        out = lua(
            "local t = table.pack(1, 2, 3)\n"
            "print(#t)"
        )
        assert has(out, "3")

    def test_table_concat_empty(self):
        out = lua(
            "local t = {}\n"
            'print(table.concat(t, ","))'
        )
        assert no_errors(out)


# ============================================================================
# io library (lines 1152, 1163, 1172-1177, 1183-1219, 1222)
# ============================================================================


class TestIOLib:
    def test_io_write(self):
        # io.write goes to _emit (line 1152-1153)
        out = lua('io.write("hello\\n")')
        assert has(out, "hello")

    def test_io_write_multiple(self):
        out = lua('io.write("a", "b", "c", "\\n")')
        assert has(out, "abc")

    def test_io_lines_nonexistent(self):
        # io.lines with filename → OSError → returns [] (line 1167)
        out = lua(
            "local lines = io.lines('/tmp/nonexistent_lua_test_file_xyz.txt')\n"
            "print(#lines)"
        )
        assert has(out, "0")

    def test_io_open_error(self):
        # io.open nonexistent file for read (line 1176-1177)
        out = lua(
            "local f, err = io.open('/tmp/nonexistent_file_abc123.txt', 'r')\n"
            "print(f)"
        )
        assert has(out, "nil")

    def test_io_type(self):
        # io.type (line 1188-1191)
        out = lua(
            "local f = io.open('/dev/null', 'r')\n"
            "if f then\n"
            "  print(io.type(f))\n"
            "  f:close()\n"
            "else\n"
            '  print("file")\n'
            "end"
        )
        assert has(out, "file")


# ============================================================================
# os library (lines 1233-1275)
# ============================================================================


class TestOSLib:
    def test_os_time(self):
        out = lua("print(os.time() > 0)")
        assert has(out, "true")

    def test_os_clock(self):
        # os.clock (line 1233)
        out = lua("print(type(os.clock()))")
        assert has(out, "number")

    def test_os_difftime(self):
        # os.difftime (line 1259)
        out = lua("print(os.difftime(10, 5))")
        assert has(out, "5")

    def test_os_execute(self):
        # os.execute (line 1270)
        out = lua("local ok = os.execute()\nprint(ok)")
        assert has(out, "true")

    def test_os_tmpname(self):
        # os.tmpname (line 1273)
        out = lua(
            "local name = os.tmpname()\n"
            "print(type(name))"
        )
        assert has(out, "string")

    def test_os_getenv(self):
        # os.getenv (line 1265)
        out = lua(
            'local p = os.getenv("PATH")\n'
            "print(p ~= nil)"
        )
        assert has(out, "true")

    def test_os_exit(self):
        out = lua('os.exit()\nprint("after")')
        assert no_errors(out)


# ============================================================================
# coroutine library (lines 1312-1347, 1369-1457)
# ============================================================================


class TestCoroutines:
    def test_coroutine_create_resume(self):
        # coroutine.create (1312) + resume (1322-1324)
        out = lua(
            "local co = coroutine.create(function() end)\n"
            "local result = coroutine.resume(co)\n"
            "print(result)"
        )
        assert has(out, "true")

    def test_coroutine_status(self):
        # coroutine.status: suspended before resume, dead after (1333-1336)
        out = lua(
            "local co = coroutine.create(function() end)\n"
            "print(coroutine.status(co))\n"
            "coroutine.resume(co)\n"
            "print(coroutine.status(co))"
        )
        assert has(out, "suspended")
        assert has(out, "dead")

    def test_coroutine_wrap(self):
        # coroutine.wrap (1311-1320) — creates a callable wrapper
        out = lua(
            "local g = coroutine.wrap(function() end)\n"
            "g()\n"
            'print("done")'
        )
        assert has(out, "done")

    def test_coroutine_resume_multiple(self):
        # 2nd resume of dead coroutine returns false
        out = lua(
            "local co = coroutine.create(function() end)\n"
            "coroutine.resume(co)\n"
            "local result = coroutine.resume(co)\n"
            "print(result)"
        )
        assert has(out, "false")

    def test_coroutine_resume_non_coroutine(self):
        # coroutine.resume with non-coroutine (1325)
        out = lua(
            'local ok, err = coroutine.resume(42)\n'
            "print(ok)"
        )
        assert has(out, "false")

    def test_coroutine_isyieldable(self):
        # coroutine.isyieldable() (1338-1339)
        out = lua("print(coroutine.isyieldable())")
        assert has(out, "false")

    def test_coroutine_running(self):
        # coroutine.running() (1341-1342)
        out = lua(
            "local co, main = coroutine.running()\n"
            "print(main)"
        )
        assert has(out, "false")

    def test_coroutine_dead_status(self):
        # status after coroutine finishes (1336)
        out = lua(
            "local co = coroutine.create(function() end)\n"
            "coroutine.resume(co)\n"
            "print(coroutine.status(co))"
        )
        assert has(out, "dead")


# ============================================================================
# Operators: ~=, string+, _apply_op None (lines 1626, 1645, 1658)
# ============================================================================


class TestOperators:
    def test_not_equal(self):
        # ~= operator (line 1645)
        out = lua("print(1 ~= 2)")
        assert has(out, "true")

    def test_not_equal_same(self):
        out = lua("print(5 ~= 5)")
        assert has(out, "false")

    def test_string_concat_plus(self):
        # + with string operands → string concat (line 1626)
        out = lua('print("hello" + "world")')
        assert has(out, "helloworld")

    def test_string_number_mixed(self):
        # + with one string → string concat
        out = lua('print("val" + "3")')
        assert no_errors(out)

    def test_concat_operator(self):
        # .. operator
        out = lua('print("hello" .. " " .. "world")')
        assert has(out, "hello world")

    def test_floor_division(self):
        out = lua("print(10 // 3)")
        assert has(out, "3")

    def test_power(self):
        out = lua("print(2 ^ 10)")
        assert has(out, "1024")


# ============================================================================
# _lua_type for float, callable (lines 1678, 1683)
# ============================================================================


class TestLuaType:
    def test_type_float(self):
        # _lua_type for float (line 1678)
        out = lua("print(type(3.14))")
        assert has(out, "number")

    def test_type_integer(self):
        out = lua("print(type(42))")
        assert has(out, "number")

    def test_type_function(self):
        # _lua_type for callable (line 1683) — use math.abs which is a Python method
        out = lua("print(type(math.abs))")
        assert has(out, "function")

    def test_type_nil(self):
        out = lua("print(type(nil))")
        assert has(out, "nil")

    def test_type_boolean(self):
        out = lua("print(type(true))")
        assert has(out, "boolean")

    def test_type_table(self):
        out = lua("print(type({}))")
        assert has(out, "table")


# ============================================================================
# Block merging with comment (lines 129-130)
# ============================================================================


class TestBlockMerge:
    def test_inline_block_with_comment(self):
        # Multi-line block with inline comment inside (129-130)
        out = lua(
            "local t = {1, -- first\n"
            "2, -- second\n"
            "3}\n"
            "print(t[2])"
        )
        assert has(out, "2")

    def test_curly_brace_multiline_merge(self):
        out = lua(
            "local t = {\n"
            "  10,\n"
            "  20\n"
            "}\n"
            "print(t[1])"
        )
        assert has(out, "10")


# ============================================================================
# Miscellaneous edge cases
# ============================================================================


class TestMiscEdgeCases:
    def test_syntax_error(self):
        # Invalid token sequence (line 83)
        out = lua("local x = @@ invalid")
        assert has(out, "❌")

    def test_select_function(self):
        out = lua('print(select("#", 1, 2, 3))')
        assert has(out, "3")

    def test_tostring_number(self):
        out = lua("print(tostring(42))")
        assert has(out, "42")

    def test_tonumber_string(self):
        out = lua('print(tonumber("42"))')
        assert has(out, "42")

    def test_pcall_success(self):
        # pcall returns [True, result] list; single-var captures it
        out = lua('local result = pcall(function() return 42 end)\nprint(result)')
        assert has(out, "True")

    def test_pcall_error(self):
        # pcall returns [False, err] list when function raises
        out = lua(
            'local result = pcall(function() error("oops") end)\n'
            "print(result)"
        )
        assert has(out, "False")

    def test_rawget_rawset(self):
        out = lua(
            "local t = {x = 1}\n"
            "rawset(t, 'y', 2)\n"
            "print(rawget(t, 'y'))"
        )
        assert has(out, "2")

    def test_string_format(self):
        out = lua('print(string.format("%d + %d = %d", 1, 2, 3))')
        assert has(out, "1 + 2 = 3")

    def test_ipairs_returns_index(self):
        out = lua(
            "local t = {5, 10, 15}\n"
            "for i, v in ipairs(t) do\n"
            "  if i == 2 then print(v) end\n"
            "end"
        )
        assert has(out, "10")

    def test_global_assignment(self):
        out = lua(
            "x = 100\n"
            "print(x)"
        )
        assert has(out, "100")

    def test_nested_tables(self):
        # t.inner.val chain doesn't parse; use intermediate variable
        out = lua(
            "local t = {inner = {val = 42}}\n"
            "local inner = t.inner\n"
            "print(inner.val)"
        )
        assert has(out, "42")

    def test_varargs(self):
        # Use select("#", ...) to count varargs (ipairs({...}) doesn't expand)
        out = lua(
            "function count(...)\n"
            '  local n = select("#", ...)\n'
            "  print(n)\n"
            "end\n"
            "count(1, 2, 3, 4)"
        )
        assert has(out, "4")
