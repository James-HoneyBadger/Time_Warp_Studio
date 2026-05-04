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

    def __getitem__(self, key):
        try:
            return super().__getitem__(key)
        except KeyError:
            return _undefined

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


def _js_add(a, b):
    """JS-style addition: if either operand is a string, coerce both to strings."""
    if isinstance(a, str) or isinstance(b, str):
        return _js_str(a) + _js_str(b)
    return a + b


def _js_toFixed(val, digits):
    """JS Number.toFixed() — format float to fixed decimal places, return string."""
    try:
        return f"{float(val):.{int(digits)}f}"
    except Exception:
        return str(val)


def _js_toPrecision(val, digits):
    """JS Number.toPrecision() — format to significant digits, return string."""
    try:
        return f"{float(val):.{int(digits)}g}"
    except Exception:
        return str(val)


class _JSNumber(int):
    """Numeric type that supports JS-style string coercion with +."""

    def __new__(cls, v=0):
        try:
            return super().__new__(cls, int(v))
        except (TypeError, ValueError):
            return super().__new__(cls, 0)

    def _wrap(self, result):
        if isinstance(result, int) and not isinstance(result, bool):
            return _JSNumber(result)
        return result

    def __add__(self, other):
        if isinstance(other, str):
            return _js_str(self) + other
        return self._wrap(super().__add__(other))

    def __radd__(self, other):
        if isinstance(other, str):
            return other + _js_str(self)
        return self._wrap(super().__radd__(other))

    def __sub__(self, other): return self._wrap(super().__sub__(other))
    def __rsub__(self, other): return self._wrap(int.__sub__(other, self))
    def __mul__(self, other): return self._wrap(super().__mul__(other))
    def __rmul__(self, other): return self._wrap(super().__rmul__(other))
    def __floordiv__(self, other): return self._wrap(super().__floordiv__(other))
    def __mod__(self, other): return self._wrap(super().__mod__(other))

    def __repr__(self):
        return str(int(self))

    def __str__(self):
        return str(int(self))


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
    def __getitem__(self, key):
        if isinstance(key, float):
            key = int(key)
        return super().__getitem__(key)

    def __setitem__(self, key, val):
        if isinstance(key, float):
            key = int(key)
        super().__setitem__(key, val)

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
        import inspect as _inspect
        try:
            nargs = len(_inspect.signature(fn).parameters)
        except (ValueError, TypeError):
            nargs = 3
        for i, x in enumerate(self):
            if nargs == 1:
                fn(x)
            elif nargs == 2:
                fn(x, i)
            else:
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
            + f"{self._dt().microsecond // 1000:03d}Z"
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

    @classmethod
    def now(cls):
        """Date.now() — returns current time as milliseconds since epoch."""
        import time as _t
        return int(_t.time() * 1000)

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


def _js_array_from(iterable_or_len, map_fn=None):
    """Implement JS Array.from(arrayLike, mapFn)."""
    if isinstance(iterable_or_len, _JsDict):
        length = iterable_or_len.get("length", 0)
        try:
            length = int(length)
        except (TypeError, ValueError):
            length = 0
        items = [_undefined] * length
    else:
        try:
            items = list(iterable_or_len)
        except TypeError:
            items = []
    if map_fn is not None and callable(map_fn):
        try:
            result = JSArray(map_fn(v, i) for i, v in enumerate(items))
        except TypeError:
            result = JSArray(map_fn(v) for v in items)
    else:
        result = JSArray(items)
    return result


def _js_re_replace(s: object, pattern: str, flags: str, repl: str) -> str:
    """Implement JS str.replace(/pattern/flags, repl) for strings."""
    import re as _re
    s = str(s)
    py_flags = 0
    if "i" in flags: py_flags |= _re.IGNORECASE
    if "m" in flags: py_flags |= _re.MULTILINE
    if "s" in flags: py_flags |= _re.DOTALL
    try:
        count = 0 if "g" in flags else 1
        return _re.sub(pattern, str(repl), s, count=count, flags=py_flags)
    except _re.error:
        # Fall back to literal replacement
        return s.replace(pattern, str(repl))


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
        return f"Map({{{', '.join(f'{k!r}: {v!r}' for k, v in self._d.items())}}})"


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
            time = staticmethod(lambda label="": None)
            timeEnd = staticmethod(lambda label="": None)
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
                "JSDate": JSDate,
                "_JsDict": _JsDict,
                "_js_typeof": _js_typeof,
                "_undefined": _undefined,
                "_js_add": _js_add,
                "_js_toFixed": _js_toFixed,
                "_js_toPrecision": _js_toPrecision,
                "_JSNumber": _JSNumber,
                "_js_array_from": _js_array_from,
                "_js_re_replace": _js_re_replace,
            }
        )
        g["globalThis"] = g
        g["window"] = g
        g["self"] = g
        # Override len to return _JSNumber (JS-compatible numeric type)
        import builtins as _builtins_mod
        import inspect as _inspect_mod
        _orig_len = _builtins_mod.len
        def _js_len(x):
            if callable(x) and not isinstance(x, (list, str, dict, tuple, set)):
                try:
                    sig = _inspect_mod.signature(x)
                    return _JSNumber(sum(1 for p in sig.parameters.values()
                                        if p.default is _inspect_mod.Parameter.empty
                                        and p.kind not in (
                                            _inspect_mod.Parameter.VAR_POSITIONAL,
                                            _inspect_mod.Parameter.VAR_KEYWORD)))
                except (ValueError, TypeError):
                    return _JSNumber(0)
            return _JSNumber(_orig_len(x))
        g["len"] = _js_len
        g["sorted"] = lambda it, **kw: JSArray(sorted(it, **kw))
        # Restrict builtins to a safe allowlist.  The transpiler emits Python
        # that needs certain builtins (len, range, isinstance, __build_class__
        # for classes, etc.) but we must block dangerous ones like __import__,
        # eval, exec, open, compile, breakpoint, etc.
        _SAFE_BUILTINS = (
            "abs", "all", "any", "bin", "bool", "callable", "chr", "dict",
            "divmod", "enumerate", "filter", "float", "format", "frozenset",
            "getattr", "hasattr", "hash", "hex", "id", "int", "isinstance",
            "issubclass", "iter", "len", "list", "map", "max", "min", "next",
            "object", "oct", "ord", "pow", "print", "range", "repr",
            "reversed", "round", "set", "setattr", "slice", "sorted", "str",
            "sum", "tuple", "type", "zip",
            # Required by Python's class machinery used in transpiled JS classes
            "__build_class__", "__name__",
            # Exception types used by transpiled try/catch
            "Exception", "TypeError", "ValueError", "AttributeError",
            "KeyError", "IndexError", "RuntimeError", "StopIteration",
            "NotImplementedError", "OverflowError", "ZeroDivisionError",
        )
        import builtins as _builtins_mod
        g["__builtins__"] = {
            k: getattr(_builtins_mod, k)
            for k in _SAFE_BUILTINS
            if hasattr(_builtins_mod, k)
        }
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


def _join_continuation_lines(source: str) -> str:
    """Join lines that are continuation of standalone object literals spanning multiple lines.
    
    Handles:
      return {         → join until matching }
          a: 1,
      }
    But NOT block openers (function/class/if/for/while/do/else/try/catch/=>/method defs).
    """
    _BLOCK_OPENER = re.compile(
        r"\b(?:function|class|if|else|for|while|do|try|catch|finally|switch)\b|=>"
        r"|\b\w+\s*\("  # method/function definition: name( ... ) {
    )
    
    lines = source.split("\n")
    result: list[str] = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        stripped = line.rstrip()
        sline = stripped.lstrip()
        
        # Handle bare `{` scope blocks — join and de-indent (Python has no bare blocks)
        if sline == "{":
            # Collect lines until matching `}`
            depth = 1
            inner_lines = []
            j = i + 1
            in_str2, str_ch2, esc2 = False, "", False
            while j < len(lines) and depth > 0:
                cont = lines[j]
                cont_s = cont.strip()
                for ch in cont_s:
                    if esc2: esc2 = False
                    elif in_str2:
                        if ch == "\\": esc2 = True
                        elif ch == str_ch2: in_str2 = False
                    elif ch in ('"', "'", "`"): in_str2, str_ch2 = True, ch
                    elif ch in ("{", "(", "["): depth += 1
                    elif ch in ("}", ")", "]"): depth -= 1
                if depth > 0:
                    inner_lines.append(cont)
                j += 1
            # De-indent inner lines by finding minimum indent
            non_empty = [l for l in inner_lines if l.strip()]
            if non_empty:
                min_indent = min(len(l) - len(l.lstrip()) for l in non_empty)
                result.extend(l[min_indent:] if len(l) > min_indent else l for l in inner_lines)
            i = j
            continue

        # Don't join if: line is just "{", or contains block keywords, or method def
        if (stripped.endswith("{") and not sline.startswith("//") 
                and sline != "{"  # bare opening brace = block  
                and not _BLOCK_OPENER.search(stripped)):
            # Peek at next non-empty line to check if it looks like object content
            # Object content: "key: value" or "identifier" (shorthand) but NOT "const/let/var/return/if"
            _next_i = i + 1
            while _next_i < len(lines) and not lines[_next_i].strip():
                _next_i += 1
            _next_line = lines[_next_i].strip() if _next_i < len(lines) else ""
            _STMT_OPENER = re.compile(r"^(?:const|let|var|return|if|for|while|throw|break|continue|switch)\b")
            if _STMT_OPENER.match(_next_line):
                result.append(line)
                i += 1
                continue
            # This is a multi-line object literal — join until we close the brace
            depth = 1
            joined = [stripped]
            i += 1
            in_str, str_ch = False, ""
            escaped = False
            while i < len(lines) and depth > 0:
                cont = lines[i]
                cont_s = cont.strip()
                if cont_s.startswith("//"):
                    i += 1
                    continue
                for ch in cont_s:
                    if escaped:
                        escaped = False
                        continue
                    if in_str:
                        if ch == "\\": escaped = True
                        elif ch == str_ch: in_str = False
                    elif ch in ('"', "'", "`"):
                        in_str, str_ch = True, ch
                    elif ch in ("{", "(", "["):
                        depth += 1
                    elif ch in ("}", ")", "]"):
                        depth -= 1
                joined.append(cont_s)
                i += 1
            result.append(" ".join(joined))
            continue
        
        # Also join lines that have an unclosed { within them (not ending with {)
        # e.g. "return { n, mean,   ← unclosed"
        if not sline.startswith("//") and not _BLOCK_OPENER.search(stripped) and "{" in stripped:
            # Count net brace depth for this line
            _depth = 0
            _in_str, _str_ch, _esc = False, "", False
            for ch in stripped:
                if _esc: _esc = False
                elif _in_str:
                    if ch == "\\": _esc = True
                    elif ch == _str_ch: _in_str = False
                elif ch in ('"', "'", "`"): _in_str, _str_ch = True, ch
                elif ch == "{": _depth += 1
                elif ch == "}": _depth -= 1
            if _depth > 0:
                # Unclosed brace — join continuation lines
                joined = [stripped]
                depth = _depth
                i += 1
                in_str, str_ch = False, ""
                escaped = False
                while i < len(lines) and depth > 0:
                    cont = lines[i]
                    cont_s = cont.strip()
                    if cont_s.startswith("//"):
                        i += 1
                        continue
                    for ch in cont_s:
                        if escaped: escaped = False
                        elif in_str:
                            if ch == "\\": escaped = True
                            elif ch == str_ch: in_str = False
                        elif ch in ('"', "'", "`"): in_str, str_ch = True, ch
                        elif ch in ("{", "(", "["): depth += 1
                        elif ch in ("}", ")", "]"): depth -= 1
                    joined.append(cont_s)
                    i += 1
                result.append(" ".join(joined))
                continue
        
        result.append(line)
        i += 1
    
    return "\n".join(result)


def _join_braceless_bodies(source: str) -> str:
    """Join braceless bodies after for/while/if onto the same line.
    
    Handles cases like:
        for (...)
            if (...) { body }
    →   for (...) if (...) { body }
    """
    _BLOCK_HDR = re.compile(r"^\s*(?:for|while|if)\s*\(")
    lines = source.split("\n")
    result: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.rstrip()
        # Control header without trailing {
        if (_BLOCK_HDR.match(line) and not stripped.rstrip().endswith("{")
                and not stripped.rstrip().endswith("}")):
            j = i + 1
            while j < len(lines) and not lines[j].strip():
                j += 1
            if j < len(lines):
                next_s = lines[j].strip()
                next_indent = len(lines[j]) - len(lines[j].lstrip())
                cur_indent = len(line) - len(line.lstrip())
                if next_indent > cur_indent and next_s and not next_s.startswith("//"):
                    result.append(stripped.rstrip() + " " + next_s)
                    i = j + 1
                    continue
        result.append(line)
        i += 1
    return "\n".join(result)


def _join_ternary_continuations(source: str) -> str:
    """Join multi-line ternary expressions and arrow continuations."""
    lines = source.split("\n")
    result: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        j = i + 1
        while j < len(lines) and not lines[j].strip():
            j += 1
        if j < len(lines):
            next_stripped = lines[j].strip()
            # Ternary continuation
            if next_stripped.startswith(("?", ":")):
                joined = [line.rstrip()]
                while j < len(lines) and lines[j].strip().startswith(("?", ":")):
                    joined.append(lines[j].strip())
                    j += 1
                result.append(" ".join(joined))
                i = j
                continue
            # Arrow body continuation: line ends with => (body on next line, no {)
            stripped_cur = line.rstrip()
            if stripped_cur.endswith("=>") and not next_stripped.startswith("{"):
                result.append(stripped_cur + " " + next_stripped)
                i = j + 1
                continue
        result.append(line)
        i += 1
    return "\n".join(result)


def _join_method_chains(source: str) -> str:
    """Join method chain continuations (.methodName on next line) onto previous line.
    
    For method chains with multi-line arrow callbacks (.method(param => { body })),
    hoists the callback to a named function and rewrites inline.
    """
    lines = source.split("\n")
    result: list[str] = []
    _mc_counter = [0]

    i = 0
    while i < len(lines):
        line = lines[i]
        
        j = i + 1
        while j < len(lines) and not lines[j].strip():
            j += 1
        
        if j < len(lines):
            next_stripped = lines[j].strip()
            if next_stripped.startswith(".") and not next_stripped.startswith("..."):
                # Collect all chained .method calls
                chain_lines = []
                k = j
                while (k < len(lines) and lines[k].strip().startswith(".")
                       and not lines[k].strip().startswith("...")):
                    chain_lines.append(lines[k].strip())
                    k += 1
                
                # Check if any chain line opens a multi-line block
                has_block_chain = any(re.search(r"=>\s*\{", cl) for cl in chain_lines)
                
                if has_block_chain:
                    # Process chain lines, hoisting multi-line callbacks
                    base_line = line.rstrip()
                    ind = " " * (len(line) - len(line.lstrip()))
                    hoisted_fns: list[str] = []
                    translated_chain: list[str] = []
                    
                    ci = j
                    while (ci < len(lines) and lines[ci].strip().startswith(".")
                           and not lines[ci].strip().startswith("...")):
                        chain_line = lines[ci].strip()
                        if re.search(r"=>\s*\{", chain_line):
                            # Multi-line callback: hoist it
                            m = re.match(r"^(\.\w+\()\s*(.+?)\s*=>\s*\{$", chain_line)
                            if m:
                                _mc_counter[0] += 1
                                fn_name = f"_cb{_mc_counter[0]}"
                                params = m.group(2)
                                body_lines_raw = []
                                ci += 1
                                depth = 1
                                while ci < len(lines) and depth > 0:
                                    bl = lines[ci]
                                    bl_s = bl.strip()
                                    for ch in bl_s:
                                        if ch == "{": depth += 1
                                        elif ch == "}": depth -= 1
                                    if depth > 0:
                                        body_lines_raw.append(bl)
                                    ci += 1
                                # Preserve relative indentation
                                non_empty = [l for l in body_lines_raw if l.strip()]
                                if non_empty:
                                    min_ind = min(len(l) - len(l.lstrip()) for l in non_empty)
                                else:
                                    min_ind = 0
                                body_lines = [
                                    f"{ind}    {l[min_ind:]}" if l.strip() else ""
                                    for l in body_lines_raw
                                ]
                                # Accumulate hoisted function
                                hoisted_fns.append(f"{ind}function {fn_name}({params}) {{")
                                for bl in body_lines:
                                    hoisted_fns.append(bl)
                                hoisted_fns.append(f"{ind}}}")
                                translated_chain.append(f"{m.group(1)}{fn_name})")
                            else:
                                translated_chain.append(chain_line)
                                ci += 1
                        else:
                            translated_chain.append(chain_line)
                            ci += 1
                    
                    # Emit: hoisted fns, then base.chain1.chain2...
                    result.extend(hoisted_fns)
                    result.append(base_line + " " + " ".join(translated_chain))
                    i = ci
                    continue
                else:
                    # Simple method chain — join all onto one line
                    joined = [line.rstrip()]
                    for cl in chain_lines:
                        joined.append(cl)
                    result.append(" ".join(joined))
                    i = k
                    continue
        
        result.append(line)
        i += 1
    return "\n".join(result)


def _hoist_inline_callbacks(source: str) -> str:
    """Hoist inline multi-statement arrow callbacks with balanced-paren scanning."""
    _hcb_counter = [0]
    lines = source.split("\n")
    result: list[str] = []
    
    for line in lines:
        if "=>" not in line or "{" not in line:
            result.append(line)
            continue
        
        ind = " " * (len(line) - len(line.lstrip()))
        s = line.strip()
        # Find all (params) => { body } patterns in line using balanced scanning
        out_chars = list(s)
        hoisted: list[str] = []
        i = 0
        n = len(s)
        in_str = False
        str_ch = ""
        new_s = []
        
        while i < n:
            ch = s[i]
            if in_str:
                new_s.append(ch)
                if ch == "\\" and i + 1 < n:
                    new_s.append(s[i+1])
                    i += 2
                    continue
                if ch == str_ch:
                    in_str = False
                i += 1
                continue
            if ch in ('"', "'", "`"):
                in_str, str_ch = True, ch
                new_s.append(ch)
                i += 1
                continue
            # Detect `=> {` pattern
            if s[i:i+3] == "=> " and i > 0:
                # Scan backwards to find the opening ( for the params
                j = len(new_s) - 1
                while j >= 0 and new_s[j] in (" ", "\t"):
                    j -= 1
                j_close_paren = j  # position of closing )
                if j >= 0 and new_s[j] == ")":
                    # Find matching (
                    depth = 1
                    j -= 1
                    while j >= 0 and depth > 0:
                        if new_s[j] == ")": depth += 1
                        elif new_s[j] == "(": depth -= 1
                        j -= 1
                    j += 1  # j now points to opening (
                    params = "".join(new_s[j+1:j_close_paren])  # between ( and )
                    prefix = "".join(new_s[:j])
                    
                    # Check for block: => { body }
                    k = i + 2  # skip '=> '
                    while k < n and s[k] == " ": k += 1
                    if k < n and s[k] == "{":
                        # Find matching }
                        k += 1
                        body_start = k
                        depth2 = 1
                        while k < n and depth2 > 0:
                            if s[k] == "{": depth2 += 1
                            elif s[k] == "}": depth2 -= 1
                            k += 1
                        if depth2 != 0:
                            # No matching } on this line — skip hoisting
                            new_s.append(ch)
                            i += 1
                            continue
                        body = s[body_start:k-1].strip().rstrip(";")
                        _hcb_counter[0] += 1
                        fn_name = f"_icb{_hcb_counter[0]}"
                        hoisted.append(f"{ind}function {fn_name}({params}) {{ {body} }}")
                        new_s = list(prefix) + list(fn_name)
                        i = k
                        continue
            new_s.append(ch)
            i += 1
        
        for h in hoisted:
            result.append(h)
        result.append(ind + "".join(new_s).lstrip())
    return "\n".join(result)


def _js_to_py(source: str) -> str:
    if source.startswith("#!"):
        source = "//" + source[2:]
    source = re.sub(r"/\*.*?\*/", " ", source, flags=re.DOTALL)
    # Convert JS regex literals in .replace() calls to Python-compatible calls
    # e.g. str.replace(/ /g, '') → _js_re_replace(str, ' ', 'g', '')
    def _sub_js_regex_replace(m: re.Match) -> str:
        pre = m.group(1)   # expression before .replace(
        pattern = m.group(2)
        flags = m.group(3)
        repl = m.group(4)  # replacement string (without closing paren)
        return f"_js_re_replace({pre}, {pattern!r}, {flags!r}, {repl})"
    source = re.sub(
        r"([A-Za-z_\w.)\]'\"]+)\.replace\(/([^/]*)/([gimsuy]*)\s*,\s*([^)]*)\)",
        _sub_js_regex_replace,
        source,
    )
    # BigInt literals: 42n → 42
    source = re.sub(r"\b(\d+)n\b", r"\1", source)
    # yield* → yield from (JS generator delegation)
    source = re.sub(r"\byield\s*\*", "yield from", source)
    # gen.next().value → next(gen) (JS generator iteration)
    source = re.sub(r"(\w+)\.next\(\)\.value", r"next(\1)", source)
    # str.split('') — JS splits into chars; Python split("") raises ValueError
    # Replace `.split('')` / `.split("")` with `._jssplit("")` marker, handled post-translate
    source = re.sub(r"\.split\s*\(\s*(?:''|\"\")\s*\)", "._jssplitchars()", source)
    # Symbol calls routed to JSSymbol class at runtime (no transpile rewrite needed)
    # Join multi-line object/array literals into single lines
    source = _join_continuation_lines(source)
    # Pre-join method chain continuations (.method on next line) before expansion
    source = _join_method_chains(source)
    # Hoist inline multi-statement arrow callbacks: (params) => { stmts }
    source = _hoist_inline_callbacks(source)
    # Join multi-line ternary continuations (? and : on their own lines)
    source = _join_ternary_continuations(source)
    # Expand single-line blocks before line-by-line translation (two passes for nested)
    source = _expand_one_liners(source)
    source = _expand_one_liners(source)
    # Join braceless for/while/if bodies onto same line as the header
    source = _join_braceless_bodies(source)
    # Reset anonymous return-function counter
    _rfa_counter[0] = 0
    lines = source.splitlines()
    out = []
    for line in lines:
        out.append(_translate_line(line))
    # Post-process: fix switch/case indentation
    out = _fix_switch_indent(out)
    # Post-process: inject 'return name' after return-function blocks
    out = _inject_return_fns(out)
    # Post-process: fix self.xxx default args in methods
    out = _fix_self_defaults(out)
    # Post-process: add 'append = push' alias to user classes with a push() method
    out = _add_push_alias(out)
    # Post-process: inject global declarations for module-level names modified in functions
    out = _inject_global_decls(out)
    # Post-process: fix destructured lambda params: lambda [a, b]: → lambda _d: (lambda a, b: ...)(*_d)
    result_str = "\n".join(out)
    if "lambda [" in result_str:
        def _fix_destructured_lambda(expr: str) -> str:
            """Replace lambda [a, b]: body with lambda _dst: (lambda a, b: body)(*_dst)."""
            def _replace(m: re.Match) -> str:
                params = m.group(1)
                body_raw = m.group(2)
                # Find end of lambda body: stop at the first ) that takes depth to -1
                depth = 0
                body_end = len(body_raw)
                in_str = False; str_ch = ""
                for ci, ch in enumerate(body_raw):
                    if in_str:
                        if ch == str_ch: in_str = False
                        continue
                    if ch in ('"', "'"):
                        in_str = True; str_ch = ch; continue
                    if ch == "(": depth += 1
                    elif ch == ")":
                        if depth == 0:
                            body_end = ci; break
                        depth -= 1
                body = body_raw[:body_end]
                suffix = body_raw[body_end:]
                return f"lambda _dst: (lambda {params}: {body})(*_dst){suffix}"
            # Work line by line to avoid cross-line matches
            out_lines = []
            for ln in expr.split("\n"):
                if "lambda [" in ln:
                    ln = re.sub(r"lambda \[([^\]]+)\]: (.+)", _replace, ln)
                out_lines.append(ln)
            return "\n".join(out_lines)
        result_str = _fix_destructured_lambda(result_str)
    # Replace ._jssplitchars() sentinel — handles str.split('') → list(str)
    # Match any expression ending before ._jssplitchars() and wrap it in list()
    # We use a heuristic: replace foo._jssplitchars() with JSArray(list(foo))
    # and for chained calls like foo.bar()._jssplitchars() do the same
    def _fix_jssplitchars(line: str) -> str:
        if "._jssplitchars()" not in line:
            return line
        # Find each occurrence and replace by scanning backward for the expression
        result = []
        i = 0
        sentinel = "._jssplitchars()"
        while i < len(line):
            idx = line.find(sentinel, i)
            if idx < 0:
                result.append(line[i:])
                break
            # Everything before idx is the expression; find its start
            # by scanning backward for a balanced paren start or identifier start
            pre = line[i:idx]
            # scan backward to find expression start
            j = len(pre) - 1
            depth = 0
            while j >= 0:
                ch = pre[j]
                if ch == ')': depth += 1
                elif ch == '(': 
                    if depth == 0: break
                    depth -= 1
                elif ch in (' ', '\t', '=', ',', '(', '+', '-', '*', '/', ':', '[', '{') and depth == 0:
                    break
                j -= 1
            j += 1
            before_expr = pre[:j]
            expr_part = pre[j:]
            result.append(f"{before_expr}JSArray(list({expr_part}))")
            i = idx + len(sentinel)
        return "".join(result)
    result_str = "\n".join(_fix_jssplitchars(line) for line in result_str.split("\n"))
    return result_str


def _fix_self_defaults(lines: list[str]) -> list[str]:
    """Fix 'def method(self, param=self.xxx, ...)' default args.
    
    Python doesn't allow self.xxx as default parameter values.
    Converts: def method(self, param=self.xxx): body
    To:       def method(self, param=_undefined):
                  if param is _undefined: param = self.xxx
                  body
    """
    result: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        # Check for method def with self. in defaults
        m = re.match(r"^(\s*)def (\w+)\(self(.*)\):\s*$", line)
        if m and "self." in m.group(3):
            indent = m.group(1)
            fn_name = m.group(2)
            params_str = m.group(3)
            # Find all param=self.xxx patterns and replace with param=_undefined
            inits: list[str] = []
            def _fix_param(pm: re.Match) -> str:
                name = pm.group(1)
                val = pm.group(2)
                inits.append(f"{indent}    if {name} is _undefined: {name} = {val}")
                return f"{name}=_undefined"
            new_params = re.sub(r"(\w+)\s*=\s*(self\.\w+)", _fix_param, params_str)
            result.append(f"{indent}def {fn_name}(self{new_params}):")
            for init_line in inits:
                result.append(init_line)
            i += 1
            continue
        result.append(line)
        i += 1
    return result


def _add_push_alias(lines: list[str]) -> list[str]:
    """For user-defined classes with a 'push' method, add 'append = push' alias.
    
    This allows code like stack.append(x) (generated from stack.push(x))
    to work on user classes that define a push() method.
    Inserts 'append = push' as a class-level attribute after the push method body.
    """
    result: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        result.append(line)
        # Detect 'def push(' inside a class (indented method definition)
        m = re.match(r"^(\s+)def push\(", line)
        if m:
            method_indent = m.group(1)
            class_indent = method_indent[:-4] if len(method_indent) >= 4 else ""
            body_indent = method_indent + "    "
            # Skip the body of push (collect until we hit a line at method_indent or less)
            i += 1
            while i < len(lines):
                bl = lines[i]
                stripped = bl.lstrip()
                curr_indent_len = len(bl) - len(stripped)
                if stripped and curr_indent_len <= len(method_indent):
                    break
                result.append(bl)
                i += 1
            # Now insert 'append = push' at class-level indentation
            result.append(f"{method_indent}append = push")
            continue
        i += 1
    return result


# Counter for anonymous return-function names (reset per _js_to_py call)
_rfa_counter: list[int] = [0]


def _inject_return_fns(lines: list[str]) -> list[str]:
    """Post-process: after 'def name(): # __RETURN_FN__name__' blocks,
    inject 'return name' when the block closes (indentation drops).
    Also handles IIFEs: 'def _iifeN(): # __IIFE__var__fn__'
    → after block injects 'var = _iifeN()'."""
    result: list[str] = []
    stack: list[tuple[int, str, str]] = []  # (def_indent, fn_name, inject_stmt)

    for line in lines:
        stripped = line.lstrip()
        # Flush any pending injects when indentation drops back
        if stripped and stack:
            line_indent = len(line) - len(stripped)
            while stack and line_indent <= stack[-1][0]:
                inject_indent = " " * stack[-1][0]
                result.append(f"{inject_indent}{stack[-1][2]}")
                stack.pop()
        result.append(line)
        # Detect return-function block opener: def name(): # __RETURN_FN__name__
        m = re.match(r"^(\s*)def (\w+)\(.*\):  # __RETURN_FN__\2__$", line)
        if m:
            stack.append((len(m.group(1)), m.group(2), f"return {m.group(2)}"))
            continue
        # Detect IIFE block opener: def _iifeN(): # __IIFE__var___iifeN__
        m2 = re.match(r"^(\s*)def (_iife\d+)\(\):  # __IIFE__(\w+)__\2__$", line)
        if m2:
            stack.append((len(m2.group(1)), m2.group(2), f"{m2.group(3)} = {m2.group(2)}()"))

    # Flush anything remaining
    for indent_level, _, inject_stmt in reversed(stack):
        result.append(f"{' ' * indent_level}{inject_stmt}")
    return result


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


def _inject_global_decls(lines: list[str]) -> list[str]:
    """Inject 'global name' declarations in function/method bodies.

    Collects names assigned at module level (indent=0), then for each
    function/method body, if those names are assigned inside, adds
    'global name' at the start of the function body.
    """
    # Collect module-level names (simple assignments at indent 0)
    module_names: set[str] = set()
    for line in lines:
        if not line or line[0] == ' ':
            continue
        # Match simple assignment: name = ... (not class/def/if/for/...)
        m = re.match(r'^([a-zA-Z_]\w*)\s*(?:[+\-*/|&^%]=|=(?!=))', line)
        if m and m.group(1) not in ('class', 'def', 'if', 'for', 'while', 'return', 'import', 'from'):
            module_names.add(m.group(1))

    if not module_names:
        return lines

    result: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        result.append(line)
        # Detect function/method start
        stripped = line.lstrip()
        indent = len(line) - len(stripped)
        if stripped.startswith('def ') and stripped.endswith(':'):
            # Scan the body for assignments to module-level names
            body_start = i + 1
            body_indent = indent + 4
            j = body_start
            used_globals: set[str] = set()
            while j < len(lines):
                bl = lines[j]
                bstripped = bl.lstrip()
                if not bstripped:
                    j += 1
                    continue
                cur_ind = len(bl) - len(bstripped)
                if cur_ind <= indent:
                    break
                # Check for assignment to a module-level name
                m = re.match(r'^([a-zA-Z_]\w*)\s*(?:[+\-*/|&^%]=|=(?!=))', bstripped)
                if m and m.group(1) in module_names:
                    used_globals.add(m.group(1))
                j += 1
            # Insert global declarations after def line, at body_indent
            pfx = ' ' * body_indent
            for name in sorted(used_globals):
                result.append(pfx + f'global {name}')
        i += 1
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
    # for (...) stmt  (no braces, inline body)
    # Only match when the body is a simple statement (not starting another block)
    def _for_inline_body(s):
        # Find the closing ) of the for(...)
        if not s.startswith("for"):
            return None
        i2 = s.index("(")
        depth = 1; j2 = i2 + 1
        while j2 < len(s) and depth > 0:
            if s[j2] == "(": depth += 1
            elif s[j2] == ")": depth -= 1
            j2 += 1
        rest = s[j2:].strip().rstrip(";")
        if rest and not rest.startswith("{"):
            header = s[:j2]
            return f"{header} {{\n  {rest}\n}}"
        return None
    expanded = _for_inline_body(s)
    if expanded:
        return expanded
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


def _add_undefined_defaults(params: str) -> str:
    """Add =_undefined defaults to params that don't already have defaults.
    e.g. 'a, b=1, c' -> 'a=_undefined, b=1, c=_undefined'
    Needed so JS-style calls with fewer args than params don't raise TypeError.
    Skips *args params (rest params).
    """
    if not params.strip():
        return params
    parts = _split_decl_commas(params)
    result = []
    saw_default = False
    for p in parts:
        p = p.strip()
        if p.startswith('*') or '=' in p:
            if '=' in p and not p.startswith('*'):
                saw_default = True
            result.append(p)
        else:
            result.append(f"{p}=_undefined")
            saw_default = True
    return ", ".join(result)


def _split_decl_commas(s: str) -> list:
    """Split a multi-variable declaration body on commas at depth 0.
    e.g. 'a = 1, b = 2' -> ['a = 1', 'b = 2']
    """
    parts, depth, current = [], 0, []
    in_str, str_char = False, ""
    i = 0
    while i < len(s):
        ch = s[i]
        if in_str:
            current.append(ch)
            if ch == "\\" and i + 1 < len(s):
                current.append(s[i + 1])
                i += 2
                continue
            if ch == str_char:
                in_str = False
        elif ch in ('"', "'", "`"):
            in_str = True
            str_char = ch
            current.append(ch)
        elif ch in ('(', '[', '{'):
            depth += 1
            current.append(ch)
        elif ch in (')', ']', '}'):
            depth -= 1
            current.append(ch)
        elif ch == ',' and depth == 0:
            parts.append(''.join(current).strip())
            current = []
            i += 1
            continue
        else:
            current.append(ch)
        i += 1
    if current:
        parts.append(''.join(current).strip())
    return parts


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
    # IIFE closing: })() or })(); — the actual call is injected by _inject_return_fns
    if re.match(r"^\}\s*\)\s*\(\s*\)\s*;?$", s):
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
        decl_body = m.group(1)
        # Multi-variable declaration: let a = 1, b = 2  → a = 1\nb = 2
        # Split only on commas that are not inside brackets/parens
        decls = _split_decl_commas(decl_body)
        if len(decls) > 1:
            return "\n".join(_translate_line(pfx + d.strip()) for d in decls)
        s = decl_body

    # strip async/await
    s = re.sub(r"^async\s+", "", s)
    s = re.sub(r"\bawait\s+", "", s)

    # ── for loops ─────────────────────────────────────────────────────────
    m = re.match(
        r"^for\s*\(\s*(?:var|let|const)?\s*(\w+)\s*=\s*(.+?);\s*(.+?);\s*(.+?)\s*\)\s*\{?$",
        s,
    )
    if m:
        var, start = m.group(1), m.group(2).strip()
        cond_raw = m.group(3).strip()
        sm = re.search(rf"\b{re.escape(var)}\s*\+=\s*(-?\d+)", s)
        sm_dec = re.search(rf"\b{re.escape(var)}\s*-=\s*(-?\d+)", s)
        is_decrement = re.search(rf"\b{re.escape(var)}--", s) or sm_dec
        step = sm.group(1) if sm else ("1" if not is_decrement else "-1")
        if is_decrement and sm_dec:
            step = f"-{sm_dec.group(1)}"
        # Check if condition is a simple linear bound: `var <= expr` or `var < expr`
        cm_simple = re.match(rf"^{re.escape(var)}\s*(<=|<)\s*(.+)$", cond_raw)
        cm_dec_simple = re.match(rf"^{re.escape(var)}\s*(>=|>)\s*(.+)$", cond_raw)
        if cm_simple:
            limit = _translate_expr(cm_simple.group(2).strip())
            if cm_simple.group(1) == "<=":
                limit = f"({limit})+1"
            return pfx + f"for {var} in range({_translate_expr(start)}, {limit}, {step}):"
        if cm_dec_simple:
            limit = _translate_expr(cm_dec_simple.group(2).strip())
            if cm_dec_simple.group(1) == ">":
                limit = f"({limit})+1"
            return pfx + f"for {var} in range({_translate_expr(start)}, {limit}-1, {step}):"
        # Check for `var * var <= n` → range(start, int(n**0.5)+1, step)
        cm_sq = re.match(rf"^{re.escape(var)}\s*\*\s*{re.escape(var)}\s*(<=|<)\s*(.+)$", cond_raw)
        if cm_sq:
            limit_expr = _translate_expr(cm_sq.group(2).strip())
            if cm_sq.group(1) == "<=":
                limit = f"int(({limit_expr})**0.5)+1"
            else:
                limit = f"int(({limit_expr})**0.5)"
            return pfx + f"for {var} in range({_translate_expr(start)}, {limit}, {step}):"
        # Fallback: emit while loop with condition
        py_cond = _translate_expr(cond_raw)
        return pfx + f"for {var} in (lambda: (x for x in __import__('itertools').count({_translate_expr(start)}, {step}) if True))():\n{pfx}    if not ({py_cond}): break"


    # Single-line for-of with inline body: for (const x of xs) body
    m = re.match(r"^for\s*\(\s*(?:var|let|const)?\s*(.+?)\s+of\s+([^)]+?)\s*\)\s+(.+)$", s)
    if m and not m.group(3).strip().startswith("{"):
        # Verify the iterable (group 2) has balanced parens; if not, the regex grabbed a wrong )
        iter_candidate = m.group(2).strip()
        if iter_candidate.count("(") == iter_candidate.count(")"):
            lhs = m.group(1).strip()
            rhs = _translate_expr(iter_candidate)
            body = _translate_line(pfx + "    " + m.group(3).strip())
            if lhs.startswith("[") and lhs.endswith("]"):
                lhs = lhs[1:-1]
            return pfx + f"for {lhs} in {rhs}:\n{body}"

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
    # Single-line while with inline body: while (cond) stmt
    m = re.match(r"^while\s*\(([^)]+)\)\s+(.+)$", s)
    if m:
        cond = _translate_expr(m.group(1).strip())
        body = _translate_line(pfx + "    " + m.group(2).strip())
        return pfx + f"while {cond}:\n{body}"

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
        raw_params = m2.group(1) if m2 else ''
        params = _add_undefined_defaults(raw_params)
        return pfx + f"def __init__(self, {params}):"

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
        params = _add_undefined_defaults(params)
        result = pfx + f"def {m.group(1)}({params}):"
        if rest_param:
            result += f"\n{pfx}  {rest_param} = JSArray({rest_param})"
        return result

    # arrow: name = (...) => { or name = (...) => expr
    # First detect IIFE: name = (() => { — immediately invoked function expression
    m = re.match(r"^(\w+)\s*=\s*\(\s*\(\s*\)\s*=>\s*\{?$", s)
    if m:
        _rfa_counter[0] += 1
        iife_name = f"_iife{_rfa_counter[0]}"
        var_name = m.group(1)
        # Generate def _iifeN(): ... and will inject var_name = _iifeN() after block
        return pfx + f"def {iife_name}():  # __IIFE__{var_name}__{iife_name}__"
    m = re.match(r"^(\w+)\s*=\s*(?:async\s+)?\(([^)]*)\)\s*=>\s*\{?$", s)
    if m:
        params = re.sub(r"\.\.\.(\w+)", r"*\1", m.group(2))
        return pfx + f"def {m.group(1)}({params}):"
    m = re.match(r"^(\w+)\s*=\s*(?:async\s+)?\(([^)]*)\)\s*=>\s*(.+)$", s)
    if m:
        params = re.sub(r"\.\.\.(\w+)", r"*\1", m.group(2))
        return (
            pfx
            + f"def {m.group(1)}({params}): return {_translate_expr(m.group(3))}"
        )
    m = re.match(r"^(\w+)\s*=\s*(?:async\s+)?(\w+)\s*=>\s*\{?$", s)
    if m:
        return pfx + f"def {m.group(1)}({m.group(2)}):"
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
    # return function name(params) { — named inner function returned from block
    m = re.match(r"^return\s+function\s+(\w+)\s*\(([^)]*)\)\s*\{?$", s)
    if m:
        name = m.group(1)
        params = re.sub(r"\.\.\.(\w+)", r"*\1", m.group(2))
        params = _add_undefined_defaults(params)
        sig = f"{name}({params})" if params else f"{name}()"
        return pfx + f"def {sig}:  # __RETURN_FN__{name}__"
    # return (...args) => { — anonymous function returned as block
    m = re.match(r"^return\s+\(([^)]*)\)\s*=>\s*\{?$", s)
    if m:
        params = re.sub(r"\.\.\.(\w+)", r"*\1", m.group(1))
        params = _add_undefined_defaults(params)
        _rfa_counter[0] += 1
        name = f"_rfa{_rfa_counter[0]}"
        sig = f"{name}({params})" if params else f"{name}()"
        return pfx + f"def {sig}:  # __RETURN_FN__{name}__"
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
    # Use balanced-paren matching to find the params (handles nested parens)
    _meth_m = re.match(r"^(\w+)\s*\(", s)
    if _meth_m and _meth_m.group(1) not in (
        "if", "for", "while", "switch", "catch", "function", "return",
    ):
        _name = _meth_m.group(1)
        _rest = s[_meth_m.end():]
        # find balanced closing paren
        _depth, _idx = 1, 0
        for _ch in _rest:
            if _ch == "(": _depth += 1
            elif _ch == ")": _depth -= 1
            if _depth == 0: break
            _idx += 1
        _after = _rest[_idx + 1:].strip()
        if _after == "{":
            params = _rest[:_idx].strip()
            # Translate default parameter values (e.g. new Set() → JSSet())
            def _translate_param_defaults(params_str: str) -> str:
                out = []
                for part in _split_decl_commas(params_str):
                    part = part.strip()
                    if "=" in part:
                        pname, _, pdefault = part.partition("=")
                        pdefault = _translate_expr(pdefault.strip())
                        out.append(f"{pname.strip()} = {pdefault}")
                    else:
                        out.append(part)
                return ", ".join(out)
            params = _translate_param_defaults(params)
            # Handle rest parameters: ...name → *name
            if "..." in params:
                parts = [p.strip() for p in params.split(",")]
                params = ", ".join(f"*{p[3:]}" if p.startswith("...") else p for p in parts)
            params = _add_undefined_defaults(params)
            if params:
                return pfx + f"def {_name}(self, {params}):"
            else:
                return pfx + f"def {_name}(self):"

    # this → self
    s = s.replace("this.", "self.")
    s = re.sub(r"\bthis\b", "self", s)
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
    def _fix_lambda_params(params_str: str) -> str:
        """Convert ...rest → *rest in lambda params."""
        parts = [p.strip() for p in params_str.split(",")]
        return ", ".join(f"*{p[3:]}" if p.strip().startswith("...") else p for p in parts)
    expr = re.sub(
        r"(?<!\w)\(([^)]*)\)\s*=>\s*(?!\{)([^,]+?)(?=[,;)\]]|$)",
        lambda m: f"lambda {_fix_lambda_params(m.group(1).strip())}: {m.group(2).strip()}",
        expr,
    )
    # single param arrow:  word => expr  (not followed by {)
    expr = re.sub(
        r"(?<![\w.])([a-zA-Z_]\w*)\s*=>\s*(?!\{)([^,]+?)(?=[,;)\]]|$)",
        lambda m: f"lambda {m.group(1)}: {m.group(2).strip()}",
        expr,
    )
    return expr


def _fix_method_to_function(expr: str, method_name: str) -> str:
    """Convert expr._method_name(args) → _method_name(expr, args) by finding the expression start."""
    pattern = f".{method_name}("
    result = []
    i = 0
    while i < len(expr):
        pos = expr.find(pattern, i)
        if pos == -1:
            result.append(expr[i:])
            break
        # Find args end (closing paren)
        args_start = pos + len(pattern)
        depth = 1
        k = args_start
        in_str, str_ch, esc = False, "", False
        while k < len(expr) and depth > 0:
            ch = expr[k]
            if esc: esc = False
            elif in_str:
                if ch == "\\": esc = True
                elif ch == str_ch: in_str = False
            elif ch in ('"', "'", "`"): in_str, str_ch = True, ch
            elif ch == "(": depth += 1
            elif ch == ")": depth -= 1
            if depth > 0: k += 1
            else: break
        args = expr[args_start:k]
        # Find start of expression before .method_name
        j = pos
        while j > i:
            if expr[j-1] == ")":
                depth2 = 1
                m = j - 2
                while m >= i and depth2 > 0:
                    if expr[m] == ")": depth2 += 1
                    elif expr[m] == "(": depth2 -= 1
                    m -= 1
                j = m + 1
                while j > i and (expr[j-1].isalnum() or expr[j-1] in "_."):
                    j -= 1
            elif expr[j-1].isalnum() or expr[j-1] in "_.":
                while j > i and (expr[j-1].isalnum() or expr[j-1] in "_."):
                    j -= 1
            else:
                break
        sub_expr = expr[j:pos]
        sep = ", " if args else ""
        result.append(expr[i:j])
        result.append(f"{method_name}({sub_expr}{sep}{args})")
        i = k + 1
    return "".join(result)


def _fix_length_calls(expr: str) -> str:
    """Replace expr.length with len(expr) by scanning for .length and finding the corresponding start."""
    result = []
    i = 0
    while i < len(expr):
        pos = expr.find('.length', i)
        if pos == -1:
            result.append(expr[i:])
            break
        end = pos + 7
        # Ensure it's a word boundary (not .lengthOf etc.)
        if end < len(expr) and (expr[end].isalnum() or expr[end] == '_'):
            result.append(expr[i:end])
            i = end
            continue
        # Find start of the expression before .length by scanning backward
        j = pos
        # First, scan back through chained calls like a.b().c().length
        while j > i:
            if expr[j-1] == ')':
                # Find matching open paren
                depth = 1
                k = j - 2
                while k >= i and depth > 0:
                    if expr[k] == ')': depth += 1
                    elif expr[k] == '(': depth -= 1
                    k -= 1
                j = k + 1  # j now points to the '('
                # Scan back through method/object name (word chars and dots)
                while j > i and (expr[j-1].isalnum() or expr[j-1] in '_.'):
                    j -= 1
            elif expr[j-1] == ']':
                # Find matching open bracket
                depth = 1
                k = j - 2
                while k >= i and depth > 0:
                    if expr[k] == ']': depth += 1
                    elif expr[k] == '[': depth -= 1
                    k -= 1
                j = k + 1
                while j > i and (expr[j-1].isalnum() or expr[j-1] in '_.'):
                    j -= 1
            elif expr[j-1] == '"':
                # String literal
                k = j - 2
                while k >= i and expr[k] != '"':
                    k -= 1
                j = k
                break
            elif expr[j-1] == "'":
                k = j - 2
                while k >= i and expr[k] != "'":
                    k -= 1
                j = k
                break
            elif expr[j-1] == '\x00':
                # STR placeholder \x00STRn\x00 — scan to opening \x00
                k = j - 2
                while k >= i and expr[k] != '\x00':
                    k -= 1
                j = k
                break
            elif expr[j-1].isalnum() or expr[j-1] in '_.':
                while j > i and (expr[j-1].isalnum() or expr[j-1] in '_.'):
                    j -= 1
            else:
                break
        sub_expr = expr[j:pos]
        result.append(expr[i:j])
        result.append(f"len({sub_expr})")
        i = end
    return "".join(result)


def _translate_expr(expr: str) -> str:
    if not expr:
        return expr

    # Inline arrow functions (before other transforms)
    expr = _translate_inline_arrows(expr)

    # Protect string literals from transformations
    _str_placeholders: list[str] = []
    def _save_str(m: re.Match) -> str:
        _str_placeholders.append(m.group(0))
        return f"\x00STR{len(_str_placeholders)-1}\x00"
    # Protect double-quoted strings
    expr = re.sub(r'"(?:[^"\\]|\\.)*"', _save_str, expr)
    # Protect single-quoted strings (not template literals)
    expr = re.sub(r"'(?:[^'\\]|\\.)*'", _save_str, expr)

    # Template literals
    def tl(m):
        inner = m.group(1)
        # Restore protected string placeholders in the template literal content
        def _restore_in_tl(m2: re.Match) -> str:
            idx = int(m2.group(1))
            if idx < len(_str_placeholders):
                return _str_placeholders[idx]
            return m2.group(0)
        inner = re.sub(r"\x00STR(\d+)\x00", _restore_in_tl, inner)
        # Convert ${expr} to {expr}
        inner = re.sub(r"\$\{(.+?)\}", r"{\1}", inner)
        # Escape " chars that appear in the TEXT parts (outside {}) of the f-string
        # to avoid breaking the double-quoted f-string delimiter
        parts = re.split(r"(\{[^}]*\})", inner)
        new_parts = []
        for part in parts:
            if part.startswith("{") and part.endswith("}"):
                new_parts.append(part)  # inside {}, keep " as-is
            else:
                new_parts.append(part.replace('"', '\\"'))
        inner = "".join(new_parts)
        # Use double-quoted f-string to avoid escaping single quotes
        inner = inner.replace("\\`", "`")
        return f'f"{inner}"'

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
    expr = re.sub(r"\bthis\b", "self", expr)
    expr = re.sub(r"\bsuper\(\)", "super().__init__()", expr)
    # Array.from(...) — 'from' is a Python keyword so must be renamed
    expr = re.sub(r"\bArray\.from\b", "_js_array_from", expr)
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
    expr = re.sub(r"\.toFixed\((\d+)\)", r"._js_toFixed(\1)", expr)
    expr = _fix_method_to_function(expr, "_js_toFixed")
    expr = re.sub(r"\.toPrecision\((\d+)\)", r"._js_toPrecision(\1)", expr)
    expr = _fix_method_to_function(expr, "_js_toPrecision")
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
    # Also handle string placeholders: \x00STRn\x00.includes(...)
    expr = re.sub(r"(\x00STR\d+\x00)\.includes\(([^)]+)\)", r"(\2 in \1)", expr)
    # .repeat() for strings
    expr = re.sub(r"\.repeat\(([^)]+)\)", r" * \1", expr)

    # .length → len() — robust handler for all expression forms
    expr = _fix_length_calls(expr)

    # Convert array literals that call methods to JSArray
    # Use negative lookbehind to avoid matching subscripts (obj[key].method)
    expr = re.sub(
        r"(?<!\w)(\[[^\]]*\])\.(map|filter|reduce|forEach|some|every|find|findIndex|sort|reverse|join|indexOf|includes|concat|slice)\(",
        r"JSArray(\1).\2(",
        expr,
    )

    # Convert variable-based array method calls to JSArray: a.map( → JSArray(a).map(
    # Also handle subscripted access: obj[key].filter( → JSArray(obj[key]).filter(
    # Use a pattern that captures the full dotted path before .method(
    # but not function call results like foo().filter( (handled by JSArray runtime method)
    expr = re.sub(
        r"\b([A-Za-z_][\w.]*\[[^\]]*\])\.(map|filter|reduce|reduceRight|forEach|some|every|find|findIndex|sort|reverse|join|indexOf|concat|flat|flatMap)\(",
        r"JSArray(\1).\2(",
        expr,
    )
    # Match full dotted paths like arr, self._todos, obj.prop
    expr = re.sub(
        r"\b((?:[A-Za-z_]\w*\.)*[A-Za-z_]\w*)\.(map|filter|reduce|reduceRight|forEach|some|every|find|findIndex|sort|reverse|join|indexOf|concat|flat|flatMap)\(",
        r"JSArray(\1).\2(",
        expr,
    )
    expr = re.sub(r"\.push\(", ".append(", expr)
    expr = re.sub(r"\.shift\(\)", ".pop(0)", expr)
    expr = re.sub(r"\.unshift\(", ".insert(0, ", expr)
    # Wrap function-call results with JSArray when calling array methods
    # e.g. obj.method().join(', ') → JSArray(obj.method()).join(', ')
    # Use [^(),]* to avoid matching outer function calls (only the direct caller)
    # Exclude bare built-in names like sort(), filter() that are already methods
    expr = re.sub(
        r"(\b(?!sort\b|filter\b|map\b|forEach\b|reduce\b|reverse\b)\w[\w.]*\([^()]*\))\.(join|sort|reverse|filter|map|forEach|reduce|reduceRight|indexOf|includes|concat|flat|flatMap|some|every)\(",
        r"JSArray(\1).\2(",
        expr,
    )
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
    # Expression-level post/pre increment: var++ → (var := var + 1) - 1, ++var → (var := var + 1)
    # Only translate when not a standalone statement (i.e., there's surrounding content)
    def _translate_increments(e: str) -> str:
        # post-increment: var++ (not at start of full expr with nothing before)
        e = re.sub(r"(?<!\+)(\b\w+)\+\+(?!\+)", lambda m: f"({m.group(1)} := {m.group(1)} + 1) - 1", e)
        # post-decrement: var-- (not at start)
        e = re.sub(r"(?<!-)(\b\w+)--(?!-)", lambda m: f"({m.group(1)} := {m.group(1)} - 1) + 1", e)
        # pre-increment: ++var
        e = re.sub(r"\+\+(\b\w+)", lambda m: f"({m.group(1)} := {m.group(1)} + 1)", e)
        # pre-decrement: --var
        e = re.sub(r"--(\b\w+)", lambda m: f"({m.group(1)} := {m.group(1)} - 1)", e)
        return e
    if "++" in expr or ("--" in expr and not re.match(r"^(\w+)--$", expr)):
        # Don't transform standalone i++ / i-- (already handled above)
        if not re.match(r"^\w+\+\+$", expr) and not re.match(r"^\w+--$", expr):
            expr = _translate_increments(expr)
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
    # Process recursively: handle ternary inside parens/brackets too
    def _apply_ternary(s: str) -> str:
        """Apply ternary translation at top level of s."""
        if "?" not in s or ":" not in s:
            return s
        depth = 0
        qi = ci = -1
        arrow_at_depth0 = False
        for idx2, ch2 in enumerate(s):
            if ch2 in "([{":
                depth += 1
            elif ch2 in ")]}":
                depth -= 1
            elif ch2 == "=" and depth == 0 and idx2 + 1 < len(s) and s[idx2 + 1] == ">":
                arrow_at_depth0 = True
            elif ch2 == "?" and depth == 0 and qi == -1:
                if arrow_at_depth0:
                    return s  # Arrow before ?: don't mangle
                # If there's a lambda in the condition (before ?), bail
                cond_str = s[:idx2]
                if "lambda" in cond_str:
                    return s
                qi = idx2
            elif ch2 == ":" and depth == 0 and ci == -1 and qi != -1:
                ci = idx2
        if qi != -1 and ci != -1 and qi < ci:
            cond = s[:qi].strip()
            tv = s[qi + 1 : ci].strip()
            fv = s[ci + 1 :].strip()
            return f"({tv}) if ({cond}) else ({fv})"
        return s

    if "?" in expr and ":" in expr:
        # First try top-level ternary
        result_expr = _apply_ternary(expr)
        if result_expr != expr:
            expr = result_expr
        else:
            # Recursively process ternary inside balanced parens
            def _translate_inner_ternary(s: str) -> str:
                out = []
                i2 = 0
                n2 = len(s)
                while i2 < n2:
                    if s[i2] == "(":
                        # find matching )
                        d2 = 1
                        j2 = i2 + 1
                        while j2 < n2 and d2 > 0:
                            if s[j2] == "(": d2 += 1
                            elif s[j2] == ")": d2 -= 1
                            j2 += 1
                        inner2 = s[i2 + 1 : j2 - 1]
                        inner2 = _translate_inner_ternary(inner2)
                        inner2 = _apply_ternary(inner2)
                        out.append("(" + inner2 + ")")
                        i2 = j2
                    else:
                        out.append(s[i2])
                        i2 += 1
                return "".join(out)
            expr = _translate_inner_ternary(expr)

    # Translate ternary inside lambda bodies: lambda params: TERNARY
    def _fix_lambda_body_ternary(s: str) -> str:
        """Find 'lambda ...: body' and apply ternary to body if it contains ?."""
        return re.sub(
            r"(lambda [^:]+: )(.+)",
            lambda m: m.group(1) + _apply_ternary(m.group(2)) if "?" in m.group(2) else m.group(0),
            s,
        )
    if "lambda" in expr and "?" in expr:
        expr = _fix_lambda_body_ternary(expr)

    # Convert JS object literals: {key: val} → _JsDict({"key": val})
    expr = _fix_object_literals(expr)

    # Restore protected string literals (reverse order to handle nested placeholders)
    for idx in range(len(_str_placeholders) - 1, -1, -1):
        expr = expr.replace(f"\x00STR{idx}\x00", _str_placeholders[idx])

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
            # Check if it looks like an object literal (has identifier: pattern or empty)
            inner_stripped = inner.strip()
            if (inner_stripped == "" or re.search(r"(?:^|,)\s*(\w+)\s*:", inner)) and not re.search(
                r"\bfor\b|\bif\b|\blambda\b", inner
            ):
                # Expand shorthand properties BEFORE quoting keys
                # A shorthand property is: (start or ,) identifier (not followed by :)
                def _expand_shorthand_prop(inner_s: str) -> str:
                    """Expand shorthand {a, b: c} → {"a": a, "b": c}"""
                    parts = []
                    # Split on commas at depth 0, respecting strings
                    for prop in _split_decl_commas(inner_s):
                        prop = prop.strip()
                        # Check if it's a bare identifier (shorthand property)
                        if re.match(r'^[A-Za-z_]\w*$', prop):
                            parts.append(f'"{prop}": {prop}')
                        else:
                            parts.append(prop)
                    return ", ".join(parts)
                inner = _expand_shorthand_prop(inner)
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
