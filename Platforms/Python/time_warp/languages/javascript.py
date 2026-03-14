"""JavaScript language executor for Time Warp Studio.

Educational JavaScript interpreter — comprehensive teaching subset.
Implements a restricted JavaScript evaluator in pure Python with full
standard-library coverage:
  - var/let/const, function, arrow functions, closures
  - if/else/else-if, for/for-of/for-in, while, do-while, switch
  - try/catch/finally/throw
  - Array (full prototype), String (full prototype), Number, Math, Date
  - Map, Set, WeakMap, WeakSet
  - JSON, console, RegExp (basic), Promise (synchronous simulation)
  - Template literals
  - Object.keys/values/entries/assign/freeze/create
  - typeof, instanceof, ternary, nullish coalescing (??)
  - async/await (runs synchronously -- educational simulation)
  - Symbol (unique identifiers, Symbol.iterator, Symbol.for)
"""

from __future__ import annotations

import json as _json
import math as _math
import random as _random
import re
import time as _time
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


def execute_javascript(
    interpreter: "Interpreter", source: str, turtle: "TurtleState"
) -> str:
    env = JSEnvironment(interpreter, turtle)
    return env.run(source)


# ━━━━━━━━━━━━━━━━━━━━━━ Sentinel values ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class _JSUndefined:
    def __repr__(self):
        return "undefined"

    def __str__(self):
        return "undefined"

    def __bool__(self):
        return False


_undefined = _JSUndefined()


class _JSNull:
    def __repr__(self):
        return "null"

    def __str__(self):
        return "null"

    def __bool__(self):
        return False


_null = _JSNull()


class _JsDict(dict):
    """Dict subclass supporting JS-style dot access."""

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            return _undefined

    def __setattr__(self, key, val):
        self[key] = val

    def __delattr__(self, key):
        try:
            del self[key]
        except KeyError:
            pass


def _js_str(v) -> str:
    if v is None or isinstance(v, _JSNull):
        return "null"
    if isinstance(v, _JSUndefined):
        return "undefined"
    if v is True:
        return "true"
    if v is False:
        return "false"
    if isinstance(v, float):
        if v != v:
            return "NaN"
        if v == float("inf"):
            return "Infinity"
        if v == float("-inf"):
            return "-Infinity"
        if v == int(v):
            return str(int(v))
    return str(v)


def _js_typeof(v) -> str:
    """Implement JavaScript typeof operator semantics."""
    if isinstance(v, _JSUndefined) or v is None:
        return "undefined"
    if isinstance(v, _JSNull):
        return "object"
    if isinstance(v, bool):
        return "boolean"
    if isinstance(v, (int, float)):
        return "number"
    if isinstance(v, str):
        return "string"
    if callable(v):
        return "function"
    return "object"


# ━━━━━━━━━━━━━━━━━━━━━━ JSArray ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSArray(list):
    @property
    def length(self):
        return len(self)

    def push(self, *items):
        self.extend(items)
        return len(self)

    def pop_js(self):
        return super().pop() if self else _undefined

    def shift(self):
        return self.pop(0) if self else _undefined

    def unshift(self, *items):
        for x in reversed(items):
            self.insert(0, x)
        return len(self)

    def splice(self, start, delete_count=None, *items):
        if delete_count is None:
            delete_count = len(self) - start
        start = max(0, start) if start >= 0 else max(0, len(self) + start)
        removed = JSArray(self[start : start + delete_count])
        del self[start : start + delete_count]
        for i, item in enumerate(items):
            self.insert(start + i, item)
        return removed

    def slice_js(self, start=None, end=None):
        return JSArray(self[start:end])

    def concat_js(self, *others):
        r = JSArray(self)
        for o in others:
            r.extend(o) if isinstance(o, list) else r.append(o)
        return r

    concat = concat_js

    def join(self, sep=","):
        return sep.join(_js_str(x) for x in self)

    def indexOf(self, item, start=0):
        try:
            return self.index(item, start)
        except ValueError:
            return -1

    def lastIndexOf(self, item, end=None):
        r = range(len(self) - 1 if end is None else end, -1, -1)
        for i in r:
            if self[i] == item:
                return i
        return -1

    def includes(self, item, start=0):
        return item in self[start:]

    def find(self, fn):
        for x in self:
            if fn(x):
                return x
        return _undefined

    def findIndex(self, fn):
        for i, x in enumerate(self):
            if fn(x):
                return i
        return -1

    def findLast(self, fn):
        for x in reversed(self):
            if fn(x):
                return x
        return _undefined

    def findLastIndex(self, fn):
        for i in range(len(self) - 1, -1, -1):
            if fn(self[i]):
                return i
        return -1

    def filter(self, fn):
        return JSArray(x for x in self if fn(x))

    def map(self, fn):
        return JSArray(fn(x) for x in self)

    def reduce(self, fn, init=_undefined):
        it = iter(self)
        acc = next(it) if init is _undefined else init
        for x in it:
            acc = fn(acc, x)
        return acc

    def reduceRight(self, fn, init=_undefined):
        it = iter(reversed(self))
        acc = next(it) if init is _undefined else init
        for x in it:
            acc = fn(acc, x)
        return acc

    def forEach(self, fn):
        for i, x in enumerate(self):
            fn(x, i, self)

    def some(self, fn):
        return any(fn(x) for x in self)

    def every(self, fn):
        return all(fn(x) for x in self)

    def sort(self, fn=None):
        if fn is None:
            list.sort(self, key=lambda x: _js_str(x))
        else:
            import functools

            list.sort(self, key=functools.cmp_to_key(fn))
        return self

    def reverse(self):
        list.reverse(self)
        return self

    def flat(self, depth=1):
        r = JSArray()

        def _f(a, d):
            for x in a:
                (
                    (r.extend if (isinstance(x, list) and d > 0) else r.append)(x)
                    if not isinstance(x, list)
                    else _f(x, d - 1) if d > 0 else r.append(x)
                )

        _f(self, depth)
        return r

    def flatMap(self, fn):
        r = JSArray()
        for i, x in enumerate(self):
            m = fn(x, i, self)
            r.extend(m) if isinstance(m, list) else r.append(m)
        return r

    def fill(self, v, start=0, end=None):
        if end is None:
            end = len(self)
        for i in range(start, end):
            if 0 <= i < len(self):
                self[i] = v
        return self

    def copyWithin(self, target, start=0, end=None):
        if end is None:
            end = len(self)
        src = self[start:end]
        t = target if target >= 0 else max(0, len(self) + target)
        for i, v in enumerate(src):
            if 0 <= t + i < len(self):
                self[t + i] = v
        return self

    def at(self, index):
        try:
            return self[index]
        except IndexError:
            return _undefined

    def keys(self):
        return JSArray(range(len(self)))

    def values(self):
        return JSArray(self)

    def entries(self):
        return JSArray(JSArray([i, v]) for i, v in enumerate(self))

    def with_js(self, index, v):
        r = JSArray(self)
        r[index] = v
        return r

    def toReversed(self):
        r = JSArray(self)
        r.reverse()
        return r

    def toSorted(self, fn=None):
        r = JSArray(self)
        r.sort(fn)
        return r

    def toSpliced(self, start, del_count, *items):
        r = JSArray(self)
        r.splice(start, del_count, *items)
        return r

    def __repr__(self):
        return "[" + ", ".join(map(_js_str, self)) + "]"

    @staticmethod
    def isArray(v):
        return isinstance(v, (list, JSArray))

    @staticmethod
    def from_(it, fn=None):
        a = JSArray(it)
        return JSArray(fn(x, i) for i, x in enumerate(a)) if fn else a

    @staticmethod
    def of(*args):
        return JSArray(args)


# ━━━━━━━━━━━━━━━━━━━━━━ Number helpers ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class _NumberMeta(type):
    EPSILON = 2.220446049250313e-16
    MAX_VALUE = 1.7976931348623157e308
    MIN_VALUE = 5e-324
    MAX_SAFE_INTEGER = 9007199254740991
    MIN_SAFE_INTEGER = -9007199254740991
    POSITIVE_INFINITY = float("inf")
    NEGATIVE_INFINITY = float("-inf")
    NaN = float("nan")
    isNaN = staticmethod(lambda x: isinstance(x, float) and x != x)
    isFinite = staticmethod(lambda x: isinstance(x, (int, float)) and _math.isfinite(x))
    isInteger = staticmethod(
        lambda x: isinstance(x, int)
        or (isinstance(x, float) and x == int(x) and _math.isfinite(x))
    )
    isSafeInteger = staticmethod(
        lambda x: isinstance(x, int)
        or (isinstance(x, float) and x == int(x) and abs(x) <= 9007199254740991)
    )
    parseFloat = staticmethod(lambda s: float(str(s).strip()))
    parseInt = staticmethod(lambda s, b=10: _parse_int(s, b))


class JSNumber(metaclass=_NumberMeta):
    def __new__(cls, x=0):
        if isinstance(x, str):
            x = x.strip()
            if not x:
                return 0
            try:
                return int(x)
            except (ValueError, TypeError, OverflowError):
                pass
            try:
                return float(x)
            except (ValueError, TypeError, OverflowError):
                return float("nan")
        return x if isinstance(x, (int, float)) else 0


# ━━━━━━━━━━━━━━━━━━━━━━ Math ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSMath:
    PI = _math.pi
    E = _math.e
    LN2 = _math.log(2)
    LN10 = _math.log(10)
    LOG2E = _math.log2(_math.e)
    LOG10E = _math.log10(_math.e)
    SQRT2 = _math.sqrt(2)
    SQRT1_2 = _math.sqrt(0.5)

    abs = staticmethod(abs)
    ceil = staticmethod(_math.ceil)
    floor = staticmethod(_math.floor)
    round = staticmethod(lambda x: int(_math.floor(x + 0.5)))
    sqrt = staticmethod(_math.sqrt)
    cbrt = staticmethod(lambda x: x ** (1 / 3) if x >= 0 else -((-x) ** (1 / 3)))
    pow = staticmethod(pow)
    max = staticmethod(lambda *a: max(a) if a else float("-inf"))
    min = staticmethod(lambda *a: min(a) if a else float("inf"))
    log = staticmethod(lambda x: _math.log(x) if x > 0 else float("nan"))
    log2 = staticmethod(lambda x: _math.log2(x) if x > 0 else float("nan"))
    log10 = staticmethod(lambda x: _math.log10(x) if x > 0 else float("nan"))
    exp = staticmethod(_math.exp)
    expm1 = staticmethod(_math.expm1)
    log1p = staticmethod(_math.log1p)
    sin = staticmethod(_math.sin)
    cos = staticmethod(_math.cos)
    tan = staticmethod(_math.tan)
    asin = staticmethod(_math.asin)
    acos = staticmethod(_math.acos)
    atan = staticmethod(_math.atan)
    atan2 = staticmethod(_math.atan2)
    sinh = staticmethod(_math.sinh)
    cosh = staticmethod(_math.cosh)
    tanh = staticmethod(_math.tanh)
    asinh = staticmethod(_math.asinh)
    acosh = staticmethod(_math.acosh)
    atanh = staticmethod(_math.atanh)
    hypot = staticmethod(_math.hypot)
    sign = staticmethod(lambda x: (1 if x > 0 else -1 if x < 0 else 0))
    trunc = staticmethod(_math.trunc)
    fround = staticmethod(float)
    imul = staticmethod(
        lambda a, b: ((int(a) * int(b)) & 0xFFFFFFFF)
        - (((int(a) * int(b)) & 0x80000000) << 1)
    )
    clz32 = staticmethod(
        lambda x: (32 - (int(x) & 0xFFFFFFFF).bit_length()) if x else 32
    )
    random = staticmethod(_random.random)


# ━━━━━━━━━━━━━━━━━━━━━━ Object ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSObject:
    @staticmethod
    def keys(obj):
        return (
            JSArray(obj.keys())
            if isinstance(obj, dict)
            else JSArray(k for k in vars(obj) if not k.startswith("_"))
        )

    @staticmethod
    def values(obj):
        return (
            JSArray(obj.values())
            if isinstance(obj, dict)
            else JSArray(vars(obj).values())
        )

    @staticmethod
    def entries(obj):
        items = list(obj.items()) if isinstance(obj, dict) else list(vars(obj).items())
        return JSArray(JSArray([k, v]) for k, v in items)

    @staticmethod
    def assign(target, *sources):
        for s in sources:
            if isinstance(s, dict) and isinstance(target, dict):
                target.update(s)
        return target

    @staticmethod
    def create(proto, props=None):
        return dict(proto) if isinstance(proto, dict) else {}

    @staticmethod
    def freeze(obj):
        return obj

    @staticmethod
    def seal(obj):
        return obj

    @staticmethod
    def is_(a, b):
        return a is b or (a == b and type(a) is type(b))

    @staticmethod
    def hasOwn(obj, key):
        return key in obj if isinstance(obj, dict) else hasattr(obj, key)

    @staticmethod
    def getOwnPropertyNames(obj):
        return JSArray(obj.keys()) if isinstance(obj, dict) else JSArray(dir(obj))

    @staticmethod
    def fromEntries(it):
        return {k: v for k, v in it}

    @staticmethod
    def defineProperty(obj, key, desc):
        if isinstance(obj, dict):
            obj[key] = desc.get("value", _undefined)
        return obj

    @staticmethod
    def getPrototypeOf(obj):
        return None

    @staticmethod
    def setPrototypeOf(obj, proto):
        return obj


# ━━━━━━━━━━━━━━━━━━━━━━ JSON ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSJSON:
    @staticmethod
    def stringify(obj, replacer=None, indent=None):
        def conv(o):
            if isinstance(o, JSArray):
                return [conv(x) for x in o]
            if isinstance(o, dict):
                return {k: conv(v) for k, v in o.items()}
            if isinstance(o, (_JSNull, _JSUndefined)) or o is None:
                return None
            if isinstance(o, bool):
                return o
            return o

        try:
            return _json.dumps(conv(obj), indent=indent, ensure_ascii=False)
        except Exception:
            return '""'

    @staticmethod
    def parse(s):
        def restore(o):
            if isinstance(o, list):
                return JSArray(restore(x) for x in o)
            if isinstance(o, dict):
                return _JsDict({k: restore(v) for k, v in o.items()})
            return o

        return restore(_json.loads(s))


# ━━━━━━━━━━━━━━━━━━━━━━ Date ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSDate:
    def __init__(self, *args):
        if not args:
            self._ms = _time.time() * 1000
        elif len(args) == 1:
            if isinstance(args[0], (int, float)):
                self._ms = float(args[0])
            elif isinstance(args[0], str):
                import datetime as _dt

                try:
                    self._ms = (
                        _dt.datetime.fromisoformat(
                            args[0].replace("Z", "+00:00")
                        ).timestamp()
                        * 1000
                    )
                except (ValueError, TypeError, AttributeError):
                    self._ms = float("nan")
            else:
                self._ms = _time.time() * 1000
        else:
            import datetime as _dt

            y, mo, d = (
                int(args[0]),
                int(args[1] if len(args) > 1 else 0),
                int(args[2] if len(args) > 2 else 1),
            )
            h, mi, s, ms = (
                int(args[3] if len(args) > 3 else 0),
                int(args[4] if len(args) > 4 else 0),
                int(args[5] if len(args) > 5 else 0),
                int(args[6] if len(args) > 6 else 0),
            )
            dt = _dt.datetime(y, mo + 1, d, h, mi, s, ms * 1000)
            self._ms = dt.timestamp() * 1000

    def _dt(self):
        import datetime

        return datetime.datetime.fromtimestamp(self._ms / 1000)

    def getTime(self):
        return int(self._ms)

    def getFullYear(self):
        return self._dt().year

    def getMonth(self):
        return self._dt().month - 1

    def getDate(self):
        return self._dt().day

    def getDay(self):
        return (self._dt().weekday() + 1) % 7  # 0=Sun

    def getHours(self):
        return self._dt().hour

    def getMinutes(self):
        return self._dt().minute

    def getSeconds(self):
        return self._dt().second

    def getMilliseconds(self):
        return self._dt().microsecond // 1000

    def setFullYear(self, y):
        d = self._dt().replace(year=int(y))
        self._ms = d.timestamp() * 1000

    def setMonth(self, m):
        d = self._dt().replace(month=int(m) + 1)
        self._ms = d.timestamp() * 1000

    def setDate(self, n):
        d = self._dt().replace(day=int(n))
        self._ms = d.timestamp() * 1000

    def setHours(self, h):
        d = self._dt().replace(hour=int(h))
        self._ms = d.timestamp() * 1000

    def setMinutes(self, m):
        d = self._dt().replace(minute=int(m))
        self._ms = d.timestamp() * 1000

    def setSeconds(self, s):
        d = self._dt().replace(second=int(s))
        self._ms = d.timestamp() * 1000

    def toISOString(self):
        return (
            self._dt().strftime("%Y-%m-%dT%H:%M:%S.")
            + f"{self._dt().microsecond//1000:03d}Z"
        )

    def toDateString(self):
        return self._dt().strftime("%a %b %d %Y")

    def toTimeString(self):
        return self._dt().strftime("%H:%M:%S GMT+0000")

    def toString(self):
        return self._dt().strftime("%a %b %d %Y %H:%M:%S GMT+0000")

    def toLocaleDateString(self):
        return self._dt().strftime("%m/%d/%Y")

    def toLocaleTimeString(self):
        return self._dt().strftime("%H:%M:%S")

    def toLocaleString(self):
        return str(self._dt())

    def valueOf(self):
        return int(self._ms)

    def __repr__(self):
        return self.toString()

    def __str__(self):
        return self.toString()

    @staticmethod
    def now():
        return int(_time.time() * 1000)

    @staticmethod
    def parse(s):
        import datetime as _dt

        try:
            return int(
                _dt.datetime.fromisoformat(s.replace("Z", "+00:00")).timestamp() * 1000
            )
        except (ValueError, TypeError, AttributeError):
            return float("nan")

    @staticmethod
    def UTC(*args):
        import datetime as _dt

        y, mo = int(args[0]), int(args[1] if len(args) > 1 else 0)
        d = int(args[2] if len(args) > 2 else 1)
        h, mi, s = (
            int(args[3] if len(args) > 3 else 0),
            int(args[4] if len(args) > 4 else 0),
            int(args[5] if len(args) > 5 else 0),
        )
        dt = _dt.datetime(y, mo + 1, d, h, mi, s, tzinfo=_dt.timezone.utc)
        return int(dt.timestamp() * 1000)


# ━━━━━━━━━━━━━━━━━━━━━━ Map / Set / WeakMap / WeakSet ━━━━━━━━━━━━━━━━━━━


class JSMap:
    def __init__(self, it=None):
        self._d = {}
        if it:
            for k, v in it:
                self._d[k] = v

    def set(self, k, v):
        self._d[k] = v
        return self

    def get(self, k):
        return self._d.get(k, _undefined)

    def has(self, k):
        return k in self._d

    def delete(self, k):
        removed = k in self._d
        self._d.pop(k, None)
        return removed

    def clear(self):
        self._d.clear()

    @property
    def size(self):
        return len(self._d)

    def keys(self):
        return JSArray(self._d.keys())

    def values(self):
        return JSArray(self._d.values())

    def entries(self):
        return JSArray(JSArray([k, v]) for k, v in self._d.items())

    def forEach(self, fn):
        [fn(v, k, self) for k, v in self._d.items()]

    def __repr__(self):
        return f"Map({{{', '.join(f'{k!r}: {v!r}' for k,v in self._d.items())}}})"


class JSSet:
    def __init__(self, it=None):
        self._d = {}
        if it:
            for x in it:
                self._d[x] = True

    def add(self, v):
        self._d[v] = True
        return self

    def has(self, v):
        return v in self._d

    def delete(self, v):
        removed = v in self._d
        self._d.pop(v, None)
        return removed

    def clear(self):
        self._d.clear()

    @property
    def size(self):
        return len(self._d)

    def keys(self):
        return JSArray(self._d.keys())

    def values(self):
        return JSArray(self._d.keys())

    def entries(self):
        return JSArray(JSArray([k, k]) for k in self._d)

    def forEach(self, fn):
        [fn(v, v, self) for v in self._d]

    def __repr__(self):
        return f"Set({{{', '.join(repr(k) for k in self._d)}}})"


class JSWeakMap:
    def __init__(self):
        self._d = {}

    def set(self, k, v):
        self._d[id(k)] = (k, v)
        return self

    def get(self, k):
        item = self._d.get(id(k))
        return item[1] if item else _undefined

    def has(self, k):
        return id(k) in self._d

    def delete(self, k):
        self._d.pop(id(k), None)


class JSWeakSet:
    def __init__(self):
        self._d = set()

    def add(self, v):
        self._d.add(id(v))
        return self

    def has(self, v):
        return id(v) in self._d

    def delete(self, v):
        self._d.discard(id(v))


# ━━━━━━━━━━━━━━━━━━━━━━ RegExp ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSRegExp:
    def __init__(self, pattern, flags_str=""):
        self.source = pattern
        self._fs = flags_str
        fl = 0
        if "i" in flags_str:
            fl |= re.IGNORECASE
        if "m" in flags_str:
            fl |= re.MULTILINE
        if "s" in flags_str:
            fl |= re.DOTALL
        try:
            self._re = re.compile(pattern, fl)
        except re.error:
            self._re = re.compile(re.escape(pattern), fl)
        self.global_flag = "g" in flags_str
        self.lastIndex = 0

    def test(self, s):
        return bool(self._re.search(str(s)))

    def exec(self, s):
        m = self._re.search(str(s))
        if not m:
            return _null
        r = JSArray([m.group(0)] + list(m.groups()))
        r.index = m.start()
        r.input = s
        return r

    def __repr__(self):
        return f"/{self.source}/{self._fs}"


# ━━━━━━━━━━━━━━━━━━━━━━ Symbol / Promise ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSSymbol:
    _c = 0

    def __init__(self, desc=""):
        JSSymbol._c += 1
        self._id = JSSymbol._c
        self._desc = desc

    def __repr__(self):
        return f"Symbol({self._desc})"

    def __hash__(self):
        return self._id

    def __eq__(self, o):
        return self is o

    @staticmethod
    def for_key(k):
        return JSSymbol(k)

    iterator: "JSSymbol | None" = None  # well-known Symbol.iterator


JSSymbol.iterator = JSSymbol("Symbol.iterator")
JSSymbol.hasInstance = JSSymbol("Symbol.hasInstance")
JSSymbol.toPrimitive = JSSymbol("Symbol.toPrimitive")


class JSPromise:
    def __init__(self, executor=None):
        self._val = _undefined
        self._err = _undefined
        self._state = "pending"
        if executor:

            def res(v):
                self._val = v
                self._state = "fulfilled"

            def rej(e):
                self._err = e
                self._state = "rejected"

            try:
                executor(res, rej)
            except Exception as e:
                rej(e)

    def then(self, ok=None, fail=None):
        if self._state == "fulfilled" and ok:
            try:
                return JSPromise(lambda r, j: r(ok(self._val)))
            except Exception as exc:
                return JSPromise(lambda r, j, _e=exc: j(_e))
        if self._state == "rejected" and fail:
            try:
                return JSPromise(lambda r, j: r(fail(self._err)))
            except Exception as exc:
                return JSPromise(lambda r, j, _e=exc: j(_e))
        return self

    def catch(self, fn=None):
        return self.then(None, fn)

    def finally_(self, fn=None):
        if fn:
            fn()
            return self

    @staticmethod
    def resolve(v):
        p = JSPromise()
        p._val = v
        p._state = "fulfilled"
        return p

    @staticmethod
    def reject(e):
        p = JSPromise()
        p._err = e
        p._state = "rejected"
        return p

    @staticmethod
    def all(it):
        return JSPromise.resolve(JSArray(p._val for p in it))

    @staticmethod
    def allSettled(it):
        def mk(p):
            return (
                {"status": p._state, "value": p._val}
                if p._state == "fulfilled"
                else {"status": p._state, "reason": p._err}
            )

        return JSPromise.resolve(JSArray(mk(p) for p in it))

    @staticmethod
    def race(it):
        return next(iter(it))

    @staticmethod
    def any(it):
        for p in it:
            if p._state == "fulfilled":
                return JSPromise.resolve(p._val)
        return JSPromise.reject("All promises rejected")


# ━━━━━━━━━━━━━━━━━━━━━━ String constructor ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSStringConstructor:
    @staticmethod
    def fromCharCode(*codes):
        return "".join(chr(int(c)) for c in codes)

    @staticmethod
    def fromCodePoint(*codes):
        return "".join(chr(int(c)) for c in codes)

    def __call__(self, v=""):
        return _js_str(v)


# ━━━━━━━━━━━━━━━━━━━━━━ Environment ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class JSEnvironment:
    def __init__(self, interpreter, turtle):
        self.interpreter = interpreter
        self.turtle = turtle

    def _build_globals(self, captured):
        interp = self.interpreter
        g = {}

        class _Con:
            @staticmethod
            def log(*a):
                captured.append(" ".join(_js_str(x) for x in a))

            @staticmethod
            def error(*a):
                captured.append("❌ " + " ".join(_js_str(x) for x in a))

            @staticmethod
            def warn(*a):
                captured.append("⚠️  " + " ".join(_js_str(x) for x in a))

            @staticmethod
            def info(*a):
                captured.append("ℹ️  " + " ".join(_js_str(x) for x in a))

            @staticmethod
            def dir(o):
                captured.append(repr(o))

            @staticmethod
            def table(o):
                if isinstance(o, list):
                    [captured.append(f"  {i}: {x}") for i, x in enumerate(o)]
                else:
                    captured.append(repr(o))

            assert_ = staticmethod(
                lambda c, *a: (
                    captured.append(
                        "Assertion failed: " + " ".join(_js_str(x) for x in a)
                    )
                    if not c
                    else None
                )
            )
            time = staticmethod(lambda l="": None)
            timeEnd = staticmethod(lambda l="": None)
            group = staticmethod(lambda *a: None)
            groupEnd = staticmethod(lambda: None)

        tc = [0]

        def _sto(fn, ms=0, *a):
            tc[0] += 1
            (fn(*a) if callable(fn) else None)
            return tc[0]

        def _sin(fn, ms=0, *a):
            tc[0] += 1
            return tc[0]

        g.update(
            {
                "console": _Con(),
                "Math": JSMath(),
                "Number": JSNumber,
                "Object": JSObject(),
                "Array": JSArray,
                "String": JSStringConstructor(),
                "Boolean": bool,
                "JSON": JSJSON(),
                "Date": JSDate,
                "Map": JSMap,
                "Set": JSSet,
                "WeakMap": JSWeakMap,
                "WeakSet": JSWeakSet,
                "RegExp": JSRegExp,
                "Symbol": JSSymbol,
                "Promise": JSPromise,
                "Error": Exception,
                "TypeError": TypeError,
                "RangeError": ValueError,
                "SyntaxError": SyntaxError,
                "ReferenceError": NameError,
                "parseInt": _parse_int,
                "parseFloat": _parse_float,
                "isNaN": lambda x: isinstance(x, float) and x != x,
                "isFinite": lambda x: isinstance(x, (int, float)) and _math.isfinite(x),
                "encodeURIComponent": lambda s: re.sub(
                    r"[^A-Za-z0-9\-_.!~*'()]",
                    lambda m: "%" + "".join(f"{b:02X}" for b in m.group(0).encode()),
                    str(s),
                ),
                "decodeURIComponent": lambda s: re.sub(
                    r"%([0-9A-Fa-f]{2})", lambda m: chr(int(m.group(1), 16)), str(s)
                ),
                "encodeURI": lambda s: str(s),
                "decodeURI": lambda s: str(s),
                "alert": lambda msg: captured.append(_js_str(msg)),
                "prompt": lambda msg="": (
                    interp.request_input(str(msg))
                    if hasattr(interp, "request_input")
                    else ""
                ),
                "confirm": lambda msg="": True,
                "print": lambda *a: captured.append(" ".join(_js_str(x) for x in a)),
                "setTimeout": _sto,
                "setInterval": _sin,
                "clearTimeout": lambda _: None,
                "clearInterval": lambda _: None,
                "queueMicrotask": lambda fn: fn() if callable(fn) else None,
                "requestAnimationFrame": lambda fn: fn(0) if callable(fn) else None,
                "cancelAnimationFrame": lambda _: None,
                "undefined": _undefined,
                "null": _null,
                "NaN": float("nan"),
                "Infinity": float("inf"),
                "JSArray": JSArray,
                "JSMap": JSMap,
                "JSSet": JSSet,
                "_JsDict": _JsDict,
                "_js_typeof": _js_typeof,
                "_undefined": _undefined,
            }
        )
        g["globalThis"] = g
        g["window"] = g
        g["self"] = g
        return g

    def run(self, source: str) -> str:
        captured: list = []
        try:
            g = self._build_globals(captured)
            py_code = _js_to_py(source)
            exec(py_code, g)  # noqa: S102
        except StopIteration:
            pass
        except _JSTranslationError as e:
            captured.append(f"❌ JavaScript translation error: {e}")
        except SyntaxError as e:
            captured.append(f"❌ JavaScript syntax error: {e}")
        except Exception as e:
            captured.append(f"❌ JavaScript runtime error: {type(e).__name__}: {e}")
        return "\n".join(captured)


# ━━━━━━━━━━━━━━━━━━━━━━ Parse helpers ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


def _parse_int(s, base=10):
    try:
        s = str(s).strip()
        if base == 16 or s.startswith(("0x", "0X")):
            s = s[2:] if s.startswith(("0x", "0X")) else s
            base = 16
        elif base == 8 or s.startswith("0o"):
            s = s[2:] if s.startswith("0o") else s
            base = 8
        elif base == 2 or s.startswith("0b"):
            s = s[2:] if s.startswith("0b") else s
            base = 2
        valid = "0123456789abcdefghijklmnopqrstuvwxyz"[:base]
        n = "".join(c for c in s.lower() if c in valid)
        return int(n, base) if n else float("nan")
    except (ValueError, TypeError, OverflowError):
        return float("nan")


def _parse_float(s):
    try:
        return float(str(s).strip())
    except (ValueError, TypeError, OverflowError):
        return float("nan")


# ━━━━━━━━━━━━━━━━━━━━━━ Transpiler ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


class _JSTranslationError(Exception):
    pass


JSTranslationError = _JSTranslationError  # public alias


def _js_to_py(source: str) -> str:
    if source.startswith("#!"):
        source = "//" + source[2:]
    source = re.sub(r"/\*.*?\*/", " ", source, flags=re.DOTALL)
    # BigInt literals: 42n → 42
    source = re.sub(r"\b(\d+)n\b", r"\1", source)
    # Symbol calls routed to JSSymbol class at runtime (no transpile rewrite needed)
    # Expand single-line blocks before line-by-line translation (two passes for nested)
    source = _expand_one_liners(source)
    source = _expand_one_liners(source)
    lines = source.splitlines()
    out = []
    for line in lines:
        out.append(_translate_line(line))
    # Post-process: fix switch/case indentation
    out = _fix_switch_indent(out)
    return "\n".join(out)


def _fix_switch_indent(lines: list[str]) -> list[str]:
    """Normalize switch/case indentation in translated Python."""
    result = []
    switch_indent = -1
    switch_body_offset = 0
    first_case = True

    for line in lines:
        stripped = line.lstrip()
        if not stripped:
            result.append(line)
            continue
        cur_indent = len(line) - len(stripped)

        if (
            "__switch__" in stripped
            and "=" in stripped
            and not stripped.startswith("if")
            and not stripped.startswith("elif")
        ):
            switch_indent = cur_indent
            switch_body_offset = 0
            first_case = True
            result.append(line)
        elif switch_indent >= 0 and stripped.startswith("if __switch__ =="):
            switch_body_offset = cur_indent - switch_indent
            pfx = " " * switch_indent
            if first_case:
                result.append(pfx + stripped)
                first_case = False
            else:
                result.append(pfx + "el" + stripped)
        elif (
            switch_indent >= 0
            and stripped == "else:"
            and cur_indent == switch_indent + switch_body_offset
        ):
            result.append(" " * switch_indent + "else:")
        elif switch_indent >= 0 and stripped == "break" and switch_body_offset > 0:
            pass  # Remove break inside switch (not needed in if/elif/else)
        elif (
            switch_indent >= 0 and cur_indent > switch_indent and switch_body_offset > 0
        ):
            new_indent = cur_indent - switch_body_offset
            result.append(" " * new_indent + stripped)
        elif switch_indent >= 0 and cur_indent <= switch_indent and stripped:
            switch_indent = -1
            result.append(line)
        else:
            result.append(line)

    return result


def _expand_one_liners(source: str) -> str:
    """Expand single-line JS blocks into multi-line format for the line translator.

    Handles patterns like:
      if (...) { ...; }            → if (...) {\n  ...;\n}
      if (...) { ...; } else { ...; } → multi-line
      for (...) { ...; }          → multi-line
      while (...) { ...; }        → multi-line
      do { ...; } while (...);    → multi-line
    """
    lines = source.split("\n")
    result: list[str] = []
    for line in lines:
        stripped = line.strip()
        # Skip empty or comment lines
        if not stripped or stripped.startswith("//"):
            result.append(line)
            continue
        expanded = _try_expand_line(stripped)
        if expanded != stripped:
            # Preserve original indent
            indent = len(line) - len(line.lstrip())
            pfx = " " * indent
            for el in expanded.split("\n"):
                result.append(pfx + el)
        else:
            result.append(line)
    return "\n".join(result)


def _try_expand_line(s: str) -> str:
    """Try to expand a single-line block statement into multi-line."""
    # do { ... } while (...);
    m = re.match(r"^do\s*\{\s*(.+?)\s*\}\s*while\s*\((.+)\)\s*;?$", s)
    if m:
        body = m.group(1)
        cond = m.group(2)
        lines_in_body = [b.strip().rstrip(";") for b in body.split(";") if b.strip()]
        body_str = "\n  ".join(lines_in_body)
        return f"do {{\n  {body_str}\n  }} while ({cond})"
    # if (...) { ... } else { ... }
    m = re.match(r"^(if\s*\(.+?\))\s*\{\s*(.+?)\s*\}\s*else\s*\{\s*(.+?)\s*\}$", s)
    if m:
        tb = "; ".join(b.strip() for b in m.group(2).split(";") if b.strip())
        fb = "; ".join(b.strip() for b in m.group(3).split(";") if b.strip())
        return f"{m.group(1)} {{\n  {tb}\n}} else {{\n  {fb}\n}}"
    # if (...) { ... }
    m = re.match(r"^(if\s*\(.+?\))\s*\{\s*(.+?)\s*\}$", s)
    if m:
        body = m.group(2)
        lines_in_body = [b.strip() for b in body.split(";") if b.strip()]
        body_str = "\n  ".join(lines_in_body)
        return f"{m.group(1)} {{\n  {body_str}\n}}"
    # for (...) { ... }
    m = re.match(r"^(for\s*\(.+?\))\s*\{\s*(.+?)\s*\}$", s)
    if m:
        body = m.group(2)
        lines_in_body = [b.strip() for b in body.split(";") if b.strip()]
        body_str = "\n  ".join(lines_in_body)
        return f"{m.group(1)} {{\n  {body_str}\n}}"
    # while (...) { ... }
    m = re.match(r"^(while\s*\(.+?\))\s*\{\s*(.+?)\s*\}$", s)
    if m:
        body = m.group(2)
        lines_in_body = [b.strip() for b in body.split(";") if b.strip()]
        body_str = "\n  ".join(lines_in_body)
        return f"{m.group(1)} {{\n  {body_str}\n}}"
    # function name(...) { ... } (single-line)
    m = re.match(r"^(function\s+\w+\s*\([^)]*\))\s*\{\s*(.+?)\s*\}$", s)
    if m:
        body = m.group(2)
        lines_in_body = [b.strip() for b in body.split(";") if b.strip()]
        body_str = "\n  ".join(lines_in_body)
        return f"{m.group(1)} {{\n  {body_str}\n}}"
    # case VAL: statement; break;  → case VAL:\n  statement;\n  break;
    m = re.match(r"^(case\s+.+?):\s*(.+)$", s)
    if m:
        rest = m.group(2)
        parts = [p.strip() for p in rest.split(";") if p.strip()]
        if parts:
            body_str = "\n  ".join(parts)
            return f"{m.group(1)}:\n  {body_str}"
    # default: statement; → default:\n  statement;
    m = re.match(r"^default:\s*(.+)$", s)
    if m:
        parts = [p.strip() for p in m.group(1).split(";") if p.strip()]
        if parts:
            body_str = "\n  ".join(parts)
            return f"default:\n  {body_str}"
    # if (COND) STMT (no braces, single-line)
    m = re.match(r"^(if\s*\(.+?\))\s+(?!\{)(\S.*)$", s)
    if m:
        stmt = m.group(2).rstrip(";").strip()
        return f"{m.group(1)} {{\n  {stmt}\n}}"
    # return function(params) { body } — closure
    m = re.match(r"^return\s+function\s*\(([^)]*)\)\s*\{(.+)\}\s*;?$", s)
    if m:
        params = m.group(1).strip()
        body = m.group(2).strip()
        parts = [p.strip() for p in body.split(";") if p.strip()]
        # Detect modified variables for nonlocal
        mods = set()
        for p in parts:
            vm = re.match(r"(\w+)\s*(?:\+\+|--|[+\-*/]=)", p)
            if vm:
                mods.add(vm.group(1))
        body_lines = []
        if mods:
            body_lines.append(f"__nonlocal__ {', '.join(sorted(mods))}")
        body_lines.extend(parts)
        body_str = "\n  ".join(body_lines)
        return f"function _closure({params}) {{\n  {body_str}\n}}\nreturn _closure"
    # Class method: name(...) { ... }  (single-line, not constructor)
    m = re.match(r"^(\w+\s*\([^)]*\))\s*\{\s*(.+?)\s*\}$", s)
    if (
        m
        and not s.startswith("if")
        and not s.startswith("for")
        and not s.startswith("while")
        and not s.startswith("function")
    ):
        body = m.group(2)
        lines_in_body = [b.strip() for b in body.split(";") if b.strip()]
        body_str = "\n  ".join(lines_in_body)
        return f"{m.group(1)} {{\n  {body_str}\n}}"
    return s


def _strip_inline_comment(s: str) -> str:
    res = []
    in_str = False
    sc = ""
    i = 0
    while i < len(s):
        c = s[i]
        if in_str:
            res.append(c)
            if c == "\\" and i + 1 < len(s):
                res.append(s[i + 1])
                i += 2
                continue
            if c == sc:
                in_str = False
        else:
            if c in ('"', "'", "`"):
                in_str = True
                sc = c
                res.append(c)
            elif c == "/" and i + 1 < len(s) and s[i + 1] == "/":
                break
            else:
                res.append(c)
        i += 1
    return "".join(res).rstrip()


def _translate_line(line: str) -> str:
    stripped = line.rstrip()
    if not stripped.strip():
        return ""
    indent = len(stripped) - len(stripped.lstrip())
    pfx = " " * indent
    s = stripped.lstrip()

    if re.match(r"^//", s):
        return pfx + "# " + s[2:]
    s = _strip_inline_comment(s)
    s = s.rstrip(";").rstrip()
    if not s:
        return ""
    if s.strip() in ("{", "}"):
        return ""

    # } while (...) — do-while terminator (must be BEFORE } stripping)
    m = re.match(r"^\}\s*while\s*\((.+)\)\s*;?$", s)
    if m:
        return pfx + f"if not ({_translate_expr(m.group(1).strip())}): break"

    # Strip leading } from compound statements like } catch, } else, } finally
    s = re.sub(r"^\}\s*", "", s)

    # Declarations
    m = re.match(r"^(?:var|let|const)\s+(.*)", s)
    if m:
        s = m.group(1)

    # strip async/await
    s = re.sub(r"^async\s+", "", s)
    s = re.sub(r"\bawait\s+", "", s)

    # ── for loops ─────────────────────────────────────────────────────────
    m = re.match(
        r"^for\s*\(\s*(?:var|let|const)?\s*(\w+)\s*=\s*(.+?);\s*.+?;\s*.+?\s*\)\s*\{?$",
        s,
    )
    if m:
        var, start = m.group(1), m.group(2).strip()
        cm = re.search(rf"\b{re.escape(var)}\s*(<=|<)\s*(.+?)\s*[;)]", s + ")")
        sm = re.search(rf"\b{re.escape(var)}\s*\+=\s*(-?\d+)", s)
        step = sm.group(1) if sm else "1"
        if cm:
            limit = _translate_expr(cm.group(2).strip())
            if cm.group(1) == "<=":
                limit = f"({limit})+1"
            return pfx + f"for {var} in range({_translate_expr(start)}, {limit}, {step}):"
        return pfx + f"for {var} in range(0, {_translate_expr(start)}, 100):"

    m = re.match(r"^for\s*\(\s*(?:var|let|const)?\s*(.+?)\s+of\s+(.+?)\s*\)\s*\{?$", s)
    if m:
        lhs = m.group(1).strip()
        rhs = _translate_expr(m.group(2).strip())
        if lhs.startswith("[") and lhs.endswith("]"):
            lhs = lhs[1:-1]
        return pfx + f"for {lhs} in {rhs}:"

    m = re.match(r"^for\s*\(\s*(?:var|let|const)?\s*(\w+)\s+in\s+(.+?)\s*\)\s*\{?$", s)
    if m:
        return pfx + f"for {m.group(1)} in {_translate_expr(m.group(2).strip())}:"

    # while
    m = re.match(r"^while\s*\((.+)\)\s*\{?$", s)
    if m:
        return pfx + f"while {_translate_expr(m.group(1).strip())}:"
    if re.match(r"^do\s*\{?$", s):
        return pfx + "while True:"
    m = re.match(r"^\}\s*while\s*\((.+)\)\s*;?$", s)
    if m:
        return pfx + f"if not ({_translate_expr(m.group(1).strip())}): break"

    # switch/case/default
    m = re.match(r"^switch\s*\((.+)\)\s*\{?$", s)
    if m:
        return pfx + f"__switch__ = {_translate_expr(m.group(1).strip())}"
    m = re.match(r"^case\s+(.+?)\s*:$", s)
    if m:
        return pfx + f"if __switch__ == {_translate_expr(m.group(1).strip())}:"
    if s.strip() == "default:":
        return pfx + "else:"
    if s.strip() == "break":
        return pfx + "break"
    if s.strip() == "continue":
        return pfx + "continue"

    # if/else
    m = re.match(r"^if\s*\((.+)\)\s*\{?$", s)
    if m:
        return pfx + f"if {_translate_expr(m.group(1).strip())}:"
    m = re.match(r"^else\s+if\s*\((.+)\)\s*\{?$", s)
    if m:
        return pfx + f"elif {_translate_expr(m.group(1).strip())}:"
    if re.match(r"^else\s*\{?$", s):
        return pfx + "else:"

    # try/catch/finally
    if re.match(r"^try\s*\{?$", s):
        return pfx + "try:"
    m = re.match(r"^catch\s*(?:\((\w+)\))?\s*\{?$", s)
    if m:
        return pfx + (
            f"except Exception as {m.group(1)}:" if m.group(1) else "except Exception:"
        )
    if re.match(r"^finally\s*\{?$", s):
        return pfx + "finally:"

    # class
    m = re.match(r"^class\s+(\w+)(?:\s+extends\s+(\w+))?\s*\{?$", s)
    if m:
        return pfx + (
            f"class {m.group(1)}({m.group(2)}):"
            if m.group(2)
            else f"class {m.group(1)}:"
        )
    if re.match(r"^constructor\s*\(", s):
        m2 = re.match(r"^constructor\s*\(([^)]*)\)\s*\{?$", s)
        return pfx + f"def __init__(self, {m2.group(1) if m2 else ''}):"

    # function
    m = re.match(r"^(?:async\s+)?function\s*\*?\s*(\w+)\s*\(([^)]*)\)\s*\{?$", s)
    if m:
        params = m.group(2).strip()
        rest_param = None
        if "..." in params:
            parts = [p.strip() for p in params.split(",")]
            new_parts = []
            for p in parts:
                if p.startswith("..."):
                    rest_param = p[3:]
                    new_parts.append(f"*{rest_param}")
                else:
                    new_parts.append(p)
            params = ", ".join(new_parts)
        result = pfx + f"def {m.group(1)}({params}):"
        if rest_param:
            result += f"\n{pfx}  {rest_param} = JSArray({rest_param})"
        return result

    # arrow: name = (...) => { or name = (...) => expr
    m = re.match(r"^(\w+)\s*=\s*(?:async\s+)?\(([^)]*)\)\s*=>\s*\{?$", s)
    if m:
        return pfx + f"def {m.group(1)}({m.group(2)}):"
    m = re.match(r"^(\w+)\s*=\s*(?:async\s+)?\(([^)]*)\)\s*=>\s*(.+)$", s)
    if m:
        return (
            pfx
            + f"def {m.group(1)}({m.group(2)}): return {_translate_expr(m.group(3))}"
        )
    m = re.match(r"^(\w+)\s*=\s*(?:async\s+)?(\w+)\s*=>\s*(.+)$", s)
    if m:
        return (
            pfx
            + f"def {m.group(1)}({m.group(2)}): return {_translate_expr(m.group(3))}"
        )
    m = re.match(r"^(\w+)\s*=\s*(?:async\s+)?function\s*\(([^)]*)\)\s*\{?$", s)
    if m:
        return pfx + f"def {m.group(1)}({m.group(2)}):"

    # return / throw
    m = re.match(r"^return\s*(.*)", s)
    if m:
        v = m.group(1).strip()
        return pfx + ("return" if not v else f"return {_translate_expr(v)}")
    m = re.match(r"^throw\s+(.*)", s)
    if m:
        return pfx + f"raise Exception({_translate_expr(m.group(1))})"

    # __nonlocal__ placeholder (generated by closure expansion)
    m = re.match(r"^__nonlocal__\s+(.+)$", s)
    if m:
        return pfx + f"nonlocal {m.group(1)}"

    # Class method: name(params) { — not if/for/while/function etc.
    m = re.match(r"^(\w+)\s*\(([^)]*)\)\s*\{$", s)
    if m and m.group(1) not in (
        "if",
        "for",
        "while",
        "switch",
        "catch",
        "function",
        "return",
    ):
        params = m.group(2).strip()
        # Handle rest parameters: ...name → *name
        if "..." in params:
            parts = [p.strip() for p in params.split(",")]
            params = ", ".join(f"*{p[3:]}" if p.startswith("...") else p for p in parts)
        if params:
            return pfx + f"def {m.group(1)}(self, {params}):"
        else:
            return pfx + f"def {m.group(1)}(self):"

    # this → self
    s = s.replace("this.", "self.")
    # super
    s = re.sub(r"\bsuper\(\)", "super().__init__()", s)

    # Simple assignment: translate RHS separately (handles ternary properly)
    am = re.match(r"^(\w+(?:\.\w+|\[[^\]]*\])*)\s*=(?!=)\s*(.+)$", s)
    if am:
        return pfx + f"{_translate_expr(am.group(1))} = {_translate_expr(am.group(2))}"

    return pfx + _translate_expr(s)


def _translate_inline_arrows(expr: str) -> str:
    """Convert inline arrow functions to Python lambdas.

    Handles:
      (x) => expr      →  lambda x: expr
      (x, y) => expr   →  lambda x, y: expr
      x => expr        →  lambda x: expr
    Does NOT handle multi-line arrow bodies (those are handled at line level).
    """
    # (params) => expr  (not followed by {)
    expr = re.sub(
        r"(?<!\w)\(([^)]*)\)\s*=>\s*(?!\{)([^,]+?)(?=[,;)\]]|$)",
        lambda m: f"lambda {m.group(1).strip()}: {m.group(2).strip()}",
        expr,
    )
    # single param arrow:  word => expr  (not followed by {)
    expr = re.sub(
        r"(?<![\w.])([a-zA-Z_]\w*)\s*=>\s*(?!\{)([^,]+?)(?=[,;)\]]|$)",
        lambda m: f"lambda {m.group(1)}: {m.group(2).strip()}",
        expr,
    )
    return expr


def _translate_expr(expr: str) -> str:
    if not expr:
        return expr

    # Inline arrow functions (before other transforms)
    expr = _translate_inline_arrows(expr)

    # Template literals
    def tl(m):
        inner = m.group(1)
        inner = re.sub(r"\$\{(.+?)\}", r"{\1}", inner)
        inner = inner.replace("\\", "\\\\").replace("'", "\\'")
        return f"f'{inner}'"

    expr = re.sub(r"`([^`]*)`", tl, expr)

    expr = expr.replace("===", "==").replace("!==", "!=")
    expr = re.sub(r"&&", " and ", expr)
    expr = re.sub(r"\|\|", " or ", expr)
    expr = expr.replace("??", " or ")
    expr = re.sub(r"\?\.", ".", expr)
    expr = re.sub(r"!(?!=)", "not ", expr)

    expr = re.sub(r"\btrue\b", "True", expr)
    expr = re.sub(r"\bfalse\b", "False", expr)
    expr = re.sub(r"\bnull\b", "None", expr)
    expr = re.sub(r"\bundefined\b", "_undefined", expr)

    expr = re.sub(r"\btypeof\s+(\w+)", r"_js_typeof(\1)", expr)
    expr = re.sub(r"(\w+)\s+instanceof\s+(\w+)", r"isinstance(\1, \2)", expr)
    expr = re.sub(r"\bvoid\s+", "", expr)
    expr = re.sub(r"\bdelete\s+\S+", "None", expr)

    # Destructuring: const [a, b, ...rest] = expr  →  a, b, *rest = expr
    m = re.match(r"^\[([^\]]+)\]\s*=\s*(.+)$", expr)
    if m:
        lhs = m.group(1).replace("...", "*")
        expr = f"{lhs} = {m.group(2)}"
    # Object destructuring: { a, b } = expr  →  _obj = expr; a = _obj["a"]; b = _obj["b"]
    m = re.match(r"^\{\s*(.+?)\s*\}\s*=\s*(.+)$", expr)
    if m:
        keys = [k.strip().split("=")[0].strip() for k in m.group(1).split(",")]
        rhs = m.group(2)
        lines = [f"_obj = {rhs}"]
        for k in keys:
            lines.append(f'{k} = _obj["{k}"]')
        expr = "\n".join(lines)

    expr = expr.replace("this.", "self.")
    expr = re.sub(r"\bsuper\(\)", "super().__init__()", expr)
    expr = re.sub(r"\bnew\s+(?:JSArray|Array)\b", "JSArray", expr)
    expr = re.sub(r"\bnew\s+(?:Map|JSMap)\b", "JSMap", expr)
    expr = re.sub(r"\bnew\s+(?:Set|JSSet)\b", "JSSet", expr)
    expr = re.sub(r"\bnew\s+Date\b", "JSDate", expr)
    expr = re.sub(r"\bnew\s+RegExp\b", "JSRegExp", expr)
    expr = re.sub(r"\bnew\s+Promise\b", "JSPromise", expr)
    expr = re.sub(r"\bnew\s+(\w+)\b", r"\1", expr)

    # String / Array method translations
    expr = re.sub(r"\.toUpperCase\(\)", ".upper()", expr)
    expr = re.sub(r"\.toLowerCase\(\)", ".lower()", expr)
    expr = re.sub(r"\.trim\(\)", ".strip()", expr)
    expr = re.sub(r"\.trimStart\(\)", ".lstrip()", expr)
    expr = re.sub(r"\.trimEnd\(\)", ".rstrip()", expr)
    expr = re.sub(r"\.startsWith\(", ".startswith(", expr)
    expr = re.sub(r"\.endsWith\(", ".endswith(", expr)
    expr = re.sub(r"\.charAt\(([^)]+)\)", r"[\1]", expr)
    expr = re.sub(
        r"\.charCodeAt\(([^)]+)\)",
        r"[ord(\1) if isinstance(\1, str) else ord(chr(int(\1)))]",
        expr,
    )
    expr = (
        re.sub(r"\.slice\(", "._js_slice(", expr)
        if "JSArray" in expr or ".slice(" not in expr
        else expr
    )
    # .slice() for strings/arrays → Python slice helper
    expr = re.sub(r"\.slice\(([^,)]+),\s*([^)]+)\)", r"[\1:\2]", expr)
    expr = re.sub(r"\.slice\(([^)]+)\)", r"[\1:]", expr)
    # .includes() for strings/arrays
    expr = re.sub(r"(\w+)\.includes\(([^)]+)\)", r"(\2 in \1)", expr)
    expr = re.sub(r'("[^"]*"|\'[^\']*\')\.includes\(([^)]+)\)', r"(\2 in \1)", expr)
    # .repeat() for strings
    expr = re.sub(r"\.repeat\(([^)]+)\)", r" * \1", expr)

    # .length → len() — handle all patterns
    expr = re.sub(r'("[^"]*"|\'[^\']*\')\.length\b', r"len(\1)", expr)
    expr = re.sub(r"(\[[^\]]*\])\.length\b", r"len(\1)", expr)
    expr = re.sub(r"(\w+)\.length\b", r"len(\1)", expr)
    expr = re.sub(r"\(([^)]+)\)\.length\b", r"len(\1)", expr)

    # Convert array literals that call methods to JSArray
    expr = re.sub(
        r"(\[[^\]]*\])\.(map|filter|reduce|forEach|some|every|find|findIndex|sort|reverse|join|indexOf|includes|concat|slice)\(",
        r"JSArray(\1).\2(",
        expr,
    )

    # Convert variable-based array method calls to JSArray: a.map( → JSArray(a).map(
    expr = re.sub(
        r"\b([A-Za-z_]\w*)\.(map|filter|reduce|forEach|some|every|find|findIndex|sort|reverse|join|indexOf|concat|flat|flatMap)\(",
        r"JSArray(\1).\2(",
        expr,
    )

    # push → append (JSArray handles it)
    expr = re.sub(r"\.push\(", ".append(", expr)
    expr = re.sub(r"\.shift\(\)", ".pop(0)", expr)
    expr = re.sub(r"\.unshift\(", ".insert(0, ", expr)
    # .indexOf → .find for strings only (not after ] or ) which are JSArray contexts)
    expr = re.sub(r"(?<![)\]])\.indexOf\(", ".find(", expr)

    # split/join - leave as Python equivalents
    expr = re.sub(r"\.toString\(\)", r".__str__()", expr)

    # Math.pow → pow  / ** operator
    expr = re.sub(r"Math\.pow\(([^,]+),\s*([^)]+)\)", r"pow(\1, \2)", expr)

    # Spread in function args: f(...args) → f(*args)
    expr = re.sub(r"\.\.\.(\w+)", r"*\1", expr)

    # Assignment ops
    expr = re.sub(r"^(\w+)\+\+$", r"\1 += 1", expr)
    expr = re.sub(r"^(\w+)--$", r"\1 -= 1", expr)
    for op in (
        "+=",
        "-=",
        "*=",
        "/=",
        "%=",
        "**=",
        "&=",
        "|=",
        "^=",
        "<<=",
        ">>=",
        ">>>=",
    ):
        py_op = op.replace(">>>=", ">>=")  # unsigned right shift → signed approx
        expr = re.sub(rf"^(\w+)\s*{re.escape(op)}\s*(.+)$", rf"\1 {py_op} \2", expr)

    # Ternary: a ? b : c — try to detect and rewrite
    # Only attempt if there's exactly one ? and one : at top level
    if "?" in expr and ":" in expr:
        depth = 0
        qi = ci = -1
        for idx, ch in enumerate(expr):
            if ch in "([{":
                depth += 1
            elif ch in ")]}":
                depth -= 1
            elif ch == "?" and depth == 0 and qi == -1:
                qi = idx
            elif ch == ":" and depth == 0 and ci == -1 and qi != -1:
                ci = idx
        if qi != -1 and ci != -1 and qi < ci:
            cond = expr[:qi].strip()
            tv = expr[qi + 1 : ci].strip()
            fv = expr[ci + 1 :].strip()
            expr = f"({tv}) if ({cond}) else ({fv})"

    # Convert JS object literals: {key: val} → _JsDict({"key": val})
    expr = _fix_object_literals(expr)

    return expr


def _fix_object_literals(expr: str) -> str:
    """Convert JS object literals {key: val} → _JsDict({"key": val})."""
    if "{" not in expr:
        return expr
    result = []
    i = 0
    n = len(expr)
    while i < n:
        if expr[i] == "{":
            # Find matching }
            depth = 1
            j = i + 1
            in_str = False
            sc = ""
            while j < n and depth > 0:
                ch = expr[j]
                if in_str:
                    if ch == "\\" and j + 1 < n:
                        j += 2
                        continue
                    if ch == sc:
                        in_str = False
                elif ch in "\"'":
                    in_str = True
                    sc = ch
                elif ch == "{":
                    depth += 1
                elif ch == "}":
                    depth -= 1
                j += 1
            inner = expr[i + 1 : j - 1]
            # Check if it looks like an object literal (has identifier: pattern)
            if re.search(r"(?:^|,)\s*(\w+)\s*:", inner) and not re.search(
                r"\bfor\b|\bif\b", inner
            ):
                # Quote unquoted keys
                inner = re.sub(r"(?<![\"'\w])(\w+)\s*:", r'"\1":', inner)
                result.append(f"_JsDict({{{inner}}})")
            else:
                result.append(expr[i:j])
            i = j
        elif expr[i] in "\"'":
            # Skip strings
            sc = expr[i]
            j = i + 1
            while j < n and expr[j] != sc:
                if expr[j] == "\\":
                    j += 1
                j += 1
            result.append(expr[i : j + 1])
            i = j + 1
        else:
            result.append(expr[i])
            i += 1
    return "".join(result)
