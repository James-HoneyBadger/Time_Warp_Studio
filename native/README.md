# Time Warp Studio — native engine ports (C++20)

Two dependency-free C++20 ports of Python modules from the scratch-built Time
Warp Studio interpreter:

| Library | Ported from | What it is |
| --- | --- | --- |
| `timewarp_expr` | `utils/expression_evaluator.py` | the expression lexer/evaluator shared by every language executor |
| `timewarp_scheme` | `languages/scheme.py` | a tree-walking Scheme interpreter with **no native recursion** |
| `timewarp_canvas` | bridging `ui/output.py` and `ui/canvas.py` | a lock-free single-producer/single-consumer canvas command queue, with an optional Qt6 adapter |

`timewarp_expr` came first: a self-contained, hot, allocation-sensitive unit with
no Qt dependency, whose behaviour was already pinned down by a test suite — which
makes "did the port stay faithful?" a measurable question rather than a matter of
opinion.

`timewarp_canvas` is not a port of a single file; it replaces the *mechanism* by
which the Python application moves drawing work from the interpreter thread to
the GUI thread.

`timewarp_scheme` is the harder one, and the reason the exercise was worth doing.
The reference evaluated by recursing, so its evaluation depth *was* native call
stack depth: CPython's 1000-frame limit capped a Scheme program at roughly 200
calls. The port replaces every recursive site with an explicit, heap-backed
continuation stack. **A tail-recursive loop of 1,000,000 calls now runs with a
continuation stack 4 frames deep**, and non-tail recursion is bounded by memory
instead of by the native stack.

## Layout

```
native/
├── CMakeLists.txt
├── include/timewarp/
│   ├── expr/
│   │   ├── errors.hpp            LexError / ParseError / EvalError / DivisionByZeroError
│   │   ├── token.hpp             TokenType enum + trivially-copyable Token
│   │   ├── lexer.hpp             Lexer class
│   │   ├── evaluator.hpp         ExpressionEvaluator + EvalContext
│   │   └── detail/ascii.hpp      shared ASCII classification primitives
│   └── scheme/
│       ├── errors.hpp            SchemeError / ReaderError / BudgetExhausted
│       ├── host.hpp              Host: step budget + turtle sink, no Qt dependency
│       ├── value.hpp             Value, Pair, Vector, Procedure, Symbol, Arena, Env
│       ├── reader.hpp            tokenizer + iterative parser
│       ├── printer.hpp           iterative write/display
│       └── machine.hpp           the explicit-stack evaluator and its frame types
│   └── canvas/
│       ├── command.hpp           the 72-byte, trivially-copyable command value
│       ├── command_queue.hpp     the lock-free SPSC ring
│       ├── host_adapter.hpp      scheme::Host -> commands (header-only)
│       └── qt_bridge.hpp         the only Qt-facing file (optional)
├── src/
│   ├── token.cpp
│   ├── lexer.cpp
│   ├── evaluator.cpp             shunting-yard + RPN virtual machine + builtins
│   └── scheme/
│       ├── value.cpp             list helpers + iterative structural equality
│       ├── reader.cpp
│       ├── printer.cpp
│       ├── machine.cpp           driver loop, frame handlers, special forms, quasiquote
│       └── builtins.cpp          builtin table + dispatch
│   └── canvas/
│       ├── command.cpp           command names
│       ├── command_queue.cpp     the ring, the wakeup protocol, coalescing
│       └── qt_bridge.cpp         queued-connection wakeup (optional)
├── tests/
│   ├── check.hpp                 dependency-free assertion harness
│   ├── lexer_tests.cpp
│   ├── evaluator_tests.cpp
│   ├── scheme_tests.cpp
│   ├── canvas_queue_tests.cpp    queue + concurrency (Qt-free)
│   └── canvas_bridge_tests.cpp   Qt bridge (built only when Qt6 is found)
└── tools/
    ├── eval_cli.cpp                  stdin → result CLI (for differential testing)
    ├── differential_check.py         expression corpus diff vs the Python reference
    ├── bench.cpp                     expression micro-benchmark
    ├── scheme_cli.cpp                runs one Scheme program from stdin
    ├── scheme_bench.cpp              depth and throughput measurement
    ├── scheme_reference.py           headless driver for the Python Scheme module
    ├── scheme_probe.py               records what the reference actually does
    ├── scheme_differential_check.py  Scheme corpus diff vs the Python reference
    └── canvas_queue_bench.cpp        queue throughput and coalescing ratio
```

## Build, test, benchmark

```bash
# The short way: native/Makefile drives the CMake build below.
make -C native                # release build
make -C native test           # build, then all 11 suites
make -C native test-debug     # unoptimised, with Q_ASSERT and hardening live
make -C native asan           # under AddressSanitizer, in a third tree
make -C native asan ASAN=undefined SAN_DIR=build-ubsan \
  ASAN_FLAGS='-fsanitize=undefined -fno-omit-frame-pointer -g'   # UB instead
make -C native run            # launch the IDE
make -C native screenshot     # render the window to build/layout.png and exit
make -C native help           # every target, every overridable variable

# The long way, if you would rather not use the Makefile:
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
ctest --test-dir native/build --output-on-failure

# Cross-check against the Python reference implementations
python3 native/tools/differential_check.py native/build/expr_cli
python3 native/tools/scheme_differential_check.py native/build/scheme_cli

# Every example program, in every language, through the bridge
python3 native/tools/example_smoke.py

# Performance
./native/build/expr_bench
./native/build/scheme_bench
./native/build/canvas_queue_bench
```

`native/Makefile` is an interface, not a second build system: it owns the targets,
the defaults, the job count and the out-of-tree layout, and CMake owns the
compilation. That split is deliberate. Compiling these sources directly would mean
reimplementing AUTOMOC (a header that is moc'd *and* compiled gives duplicate
symbols; one that is not fails at link time with a vtable error), Qt6 component
discovery, the `-fPIC` and include paths, five libraries in dependency order, and
eleven test registrations — and the copy would be the one that rots.

Each build type gets its own tree (`build`, `build-debug`, `build-sanitised`)
because changing `CMAKE_BUILD_TYPE` in place recompiles everything anyway, so
separate directories cost nothing and let a release build and a `-g` build
coexist. A sanitised tree is worth a directory of its own per sanitiser for the
same reason: reusing one across two sets of sanitizer flags is a rebuild that
depends on CMake noticing the flag change. `make info` reports the toolchain and
which Qt6 components were found; `make test-list` lists the suites. CMake options
pass straight through: `make WERROR=OFF`, `make BUILD_TYPE=RelWithDebInfo`,
`make CTEST_ARGS="-R ui_layout -V"`.

`.github/workflows/native.yml` runs the same targets, so the Makefile is what CI
tests. It also guards one failure mode a green build would hide: a configure that
cannot find Qt6 Widgets still exits 0 and silently skips the entire UI section, so
the job greps its own configure log and fails if the UI was skipped. Before the
suites it runs the shipped entry point (`--version`, `--help`, and a screenshot of
a file that does not exist), because "links" and "starts" are different claims.

Qt6 is discovered with `find_package(Qt6 QUIET COMPONENTS Core)`, plus `Widgets`
for `timewarp_ui`. When they are present the canvas bridge, the debugger runner
and the widget UI are built; when absent they are skipped and everything else —
including the queue's own tests — still builds and runs. Nothing outside
`qt_bridge.*`, `debugger_runner.*` and `ui/` includes a Qt header.

The build is dependency-free: a C++20 compiler (GCC 11+, Clang 14+, MSVC 19.30+)
and CMake 3.20+. Everything compiles clean under
`-Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast -Wlogical-op
-Wnull-dereference -Werror` unless `-DTIME_WARP_EXPR_WERROR=OFF` is passed.

> GCC 11+ is required because the lexer parses literals with floating-point
> `std::from_chars`, which older libstdc++ versions do not implement.

## API at a glance

```cpp
#include "timewarp/expr/evaluator.hpp"

using namespace timewarp::expr;

static const double table[] = {0.0, 10.0, 20.0, 30.0};

EvalContext context;
context.variables        = {{"X", 10.0}};
context.arrays           = {{"TAB", table}};
context.string_variables = {{"NAME$", "Time Warp"}};
context.special_variables = [](std::string_view name) -> std::optional<double> {
    if (name == "TIMER") return 42.0;   // replaces the Python game-state import
    return std::nullopt;
};

ExpressionEvaluator evaluator{std::move(context)};

const double value = evaluator.evaluate("2 + 3 * X");   // 32.0
```

Three phases are exposed separately, so a caller can compile once and execute
many times:

```cpp
const std::string source = "X * X";          // keep the source alive
const auto tokens = evaluator.tokenize(source);   // LexError
const auto rpn    = evaluator.to_rpn(tokens);     // ParseError
evaluator.set_variable("X", 3.0);
const double nine = evaluator.evaluate_rpn(rpn);  // EvalError
```

`evaluate()` is the convenience path and memoises the token stream per
expression text.

### Error types

| C++ | Raised for | Python equivalent |
| --- | --- | --- |
| `LexError` | stray byte, malformed literal, token budget | `ValueError` |
| `ParseError` | unbalanced parens, malformed RPN | `ValueError` |
| `EvalError` | undefined variable, unknown function, bad arity, out-of-range index | `ValueError` / `TypeError` |
| `DivisionByZeroError` | `/`, `//`, `\`, `%`, `MOD`, `REM`, `0 ^ -negative` | `DivisionByZeroError` / `ZeroDivisionError` |

All four derive from `ExprError`, so a host that used `except ValueError` can
migrate with a single `catch (const ExprError&)`.

## Design decisions

**Tokens are trivially copyable and own nothing.** The Python token carried an
`Any` payload that was either `float` or `str`; the obvious C++ mapping
(`std::variant<double, std::string>`) would allocate per identifier and make
the token vector non-memcpy-able. Instead `Token` is a 32-byte POD whose `text`
member is a `string_view` into the expression source (operators point at static
literals). Tokenizing performs **zero allocations**.

**Case-insensitive keyword matching without copying.** The reference built
`name.upper()` — allocating a `str` — merely to test membership in a 7-element
tuple. The port compares in place against a length-dispatched constexpr table.

**Variables are upper-cased on lookup, not at lex time.** Identifier case is
normalised into a 96-byte stack buffer (falling back to a reusable member string)
so tokens never need owned storage, which in turn makes the token cache safe to
share views with. The reference's data model — upper-case keys — is preserved.

**Builtins resolve through a sorted table + binary search**, then jump through a
`switch` over an enum. A dictionary of Python callables would have become a map
of `std::function`, adding an indirection and a potential allocation per call.
The table is `static_assert`ed to be sorted, so a future edit that breaks the
ordering fails at compile time.

**The token cache stays, and stores tokens only.** Variable values are resolved
during RPN execution, so mutating a variable and re-evaluating the same text
observes the new value. The cache inserts the key first and lexes from the
*stored* string: `unordered_map` is node-based, so the key's buffer never moves
and the borrowed views stay valid. A failed lex rolls the entry back rather than
poisoning the cache (the reference never cached failures either).

**Hardening where C++ has no equivalent to Python's bignums.** `int(x)` is range
checked before the cast to avoid signed-conversion UB, and shifts are clamped to
63 bits (Python clamps to 64 and widens). Domain errors are checked explicitly so
`SQRT(-1)` reports `math domain error` instead of leaking `nan` into the BASIC
console.

### Measured performance

Same 8-expression corpus, same values, 160 000 evaluations, `-O3`, GCC 16.1.
Figures are the range observed across runs on one machine (there is a few
percent of run-to-run noise, which is why the table shows intervals):

| Mode | Python reference | C++20 port | Speed-up |
| --- | --- | --- | --- |
| hot (token cache on) | 5 770 ns/eval | **180–200 ns/eval** | ~29–32× |
| cold (cache cleared) | 11 070 ns/eval | **300–315 ns/eval** | ~35–37× |

Both implementations return the identical aggregate (`3170000.000`), which is
independent corroboration of the numeric results.

## Deliberate divergences

Everything else in the corpus matches the reference exactly. These six cases do
not, on purpose:

1. **`0x` / `0o` literals now work** (`0xFF` → 255, `0o17` → 15). In the
   reference the radix branch was placed *after* the decimal branch and was
   therefore unreachable: `0x10` lexed as `0` followed by the variable `X10` and
   failed with `Undefined variable: X10`. The port restores the documented
   intent. `&HFF`, which did work, is unchanged.
2. **String equality compares strings** (`NAME$ = OTHER$` → 1.0,
   `NAME$ = MISSING$` → 0.0). The reference implemented `==` as
   `abs(a - b) < 1e-10`, which for two strings raises
   `TypeError: unsupported operand type(s) for -: 'str' and 'str'` — in the
   reference, string equality could never succeed. Ordering comparisons
   (`<`, `>`, …) are likewise supported lexicographically. An unset `$`
   variable is the empty string in both implementations.
3. **`0 ^ -1` raises `DivisionByZeroError`** instead of `ZeroDivisionError`,
   and **`(-8) ^ 0.5` raises a clear error** instead of producing a Python
   `complex` that then fails to convert.
4. **Math domain errors are explicit** for `SQRT`/`LOG*`/`ASIN`/`ACOS`, matching
   `math`'s behaviour (the reference's messages come from the old `math` module
   text; the port standardises on `math domain error`).
5. **Integers wider than 64 bits are rejected** (`… AND 1` on a 24-digit literal)
   rather than silently widened, since C++ has no bignum.
6. **Shifts by 64 are clamped to 63**, because shifting a 64-bit value by 64 is
   undefined behaviour in C++ while Python simply widens the result.

Two message strings were also reworded for clarity where the reference leaked
CPython internals (`LEN(5)` → `LEN requires a string argument` rather than
`object of type 'float' has no len()`; `1.2.3` → `Invalid number literal` rather
than `could not convert string to float`). Success/failure classification is
identical in both cases.

## Preserved quirks

Faithfully carried over, because changing them would alter existing behaviour:

* The token budget is checked at the top of the scan loop **before** whitespace
  is skipped, so a trailing blank after the budget is exhausted still errors.
* A type suffix on a *function* name is consumed and discarded: `LEN$(A$)` calls
  `LEN`.
* `RND` / `RAND` shadow any variable of the same name, and in call form pop and
  ignore one numeric argument (`RND(1)`).
* The number scanner consumes only digits and dots, so exponent notation is not a
  numeric literal: `1e300` lexes as `1` followed by the variable `E300`.
* `MIN`/`MAX`/`POW`/`ATAN2` are the only two-argument builtins; everything else
  takes exactly one.
* User arrays shadow builtins of the same name.

## Verification

* **485 unit checks** (85 lexer + 400 evaluator) across two suites, all of whose
  expectations were generated by running the Python reference.
* **Differential testing**: `tools/differential_check.py` evaluates a 159-case
  corpus through both implementations with an identical context —
  **153 agreements, 0 unexpected mismatches**, 6 documented divergences above.
* **Warning-free** under the strict flag set listed earlier, with `-Werror` on.

The differential run is what caught the two most interesting bugs in the port:
a `std::cout << "OK " << evaluate(...)` chain that emitted the prefix *before* a
throwing evaluation (so failures printed `OK ERR …`), and a cache that stored an
empty token vector when lexing threw, turning a syntax error into
`Invalid expression` on the *second* attempt.

# Scheme evaluator (`timewarp_scheme`)

Ported from the reference Scheme implementation in the Python engine
(`native/bridge/time_warp/languages/scheme.py`).

A tree-walking interpreter for the subset of Scheme used for teaching functional
programming, rewritten as an explicit-stack state machine. **No member of
`Machine` calls any other member recursively.**

## What was actually recursive

Reading the reference is what settles this. The Python module contains a
trampoline — `_TAIL`, `_PENDING`, `_TAIL_FORMS` — but it is dead code: no special
form ever returns `_TAIL`, nothing ever pushes to `_PENDING`, `_TAIL_FORMS` is
never read, and `_PENDING.pop()` would `IndexError` if it were ever reached. So
every one of these was ordinary native recursion:

| Recursive site | Became |
| --- | --- |
| `eval()` → `self.eval(head, env)` | `ApplicationFrame` |
| `args = [self.eval(a, env) for a in expr[1:]]` | `ApplicationFrame` accumulator |
| `apply()` → `self.eval(form, local)` per body form | `SequenceFrame` |
| `_sf_if`, `_sf_begin`, `_sf_and`, `_sf_or`, `_sf_when`, `_sf_unless` | `IfFrame`, `SequenceFrame`, `AndFrame`, `OrFrame`, `WhenFrame` |
| `_sf_cond`, `_sf_case` | `CondFrame`, `CondArrowFrame`, `CaseFrame` |
| `_sf_let`, `_sf_let_star`, `_sf_letrec` | `LetFrame` (one frame, three modes) |
| `_sf_do` | `DoFrame` |
| `_qq()` (quasiquote) | its own explicit work stack, nested by session |
| `_map`, `_for_each`, `_filter`, `_foldl` | `MapFrame`, `FilterFrame`, `FoldFrame` |
| `_write_str` / `_display_str` | explicit work stack in `printer.cpp` |
| `_equal` | explicit work stack in `value.cpp` |
| `_parse`'s `parse_expr()` | explicit frame stack in `reader.cpp` |

## The machine

Two states, one loop:

```
Eval    -- reduce the current expression, or push a frame and descend
Deliver -- a value is ready; pop the top frame and let it consume it
```

The rule that makes it work is: **a frame is popped before its handler runs, and
the handler pushes it back only when more work is needed.** Two consequences fall
out of that single rule:

* When a handler has no further work, the value flows to the enclosing frame —
  so the frame's own position is *reused*. That is exactly a tail call, and it is
  why `(loop 1000000)` runs at constant depth.
* When a handler does need to continue (more operands, more body forms), it
  pushes itself back, so non-tail recursion consumes heap frames rather than
  native stack frames.

`begin_sequence()` applies the same idea one level down: a sequence of one form
pushes nothing at all, and a sequence pops itself before evaluating its **last**
form, so the last form of every body is a tail position.

## Depth accounting

Measured by `tools/scheme_bench.cpp`:

| Workload | Scale | Peak continuation depth |
| --- | --- | --- |
| tail-call loop | 1,000,000 calls | **4** |
| non-tail recursion | 200,000 levels | 200,004 (heap) |
| list construction | 200,000 elements | 5 |
| printing a long list | 200,000 elements | 4 |
| deep structural `equal?` | 200,000 levels | 5 |

The reference aborts at roughly 200 calls with
`❌ Scheme error: Maximum recursion depth exceeded`.

## Data model

`Value` is a 16-byte tagged union whose heap members are *pointers into a region
allocator*, never owned. That keeps it trivially copyable and trivially
destructible — values get copied constantly (argument vectors, frame
accumulators, pair cars) — and it means `set-cdr!`-style cycles and `letrec`
self-reference cost nothing, where `shared_ptr` would leak them. Symbols are
interned, so symbol equality and variable lookup are pointer comparisons.

Two structural departures from the reference, both forced by dropping Python:

* A vector was a Python `list` in the reference, and parsed code was *also* a
  Python `list`. Those two collapsed into each other — see the `case` bug below.
  Here `Vector` is its own type and code is always built from `Pair`s, so they
  can no longer be confused.
* Parsed code being pairs also means `_quote()` is the identity, and specifically
  that `case` cannot reproduce the reference's bug.

## Host integration

The reference reached into an `Interpreter` for its step budget and into a
`TurtleState` for drawing, which is why it could not run without booting the IDE.
Both are virtual on `Host`, so the same machine embeds in the Qt application and
runs headless in a test. `Host::tick` is charged every 512 steps, matching the
reference's cadence; returning `false` raises `BudgetExhausted`.

## Verification

* **169 unit checks** in `scheme_tests`, covering the reader, printer, all
  special forms, every builtin family, the host seam, and explicit proofs that
  nobody recurses (`test_tail_calls_are_constant_space`,
  `test_deeply_nested_data_prints_without_recursing`,
  `test_reader_handles_deep_nesting`).
* **Differential testing**: `tools/scheme_differential_check.py` runs a 191-case
  corpus through both implementations and compares the **complete output text**,
  so rendering is checked as well as evaluation —
  **167 agreements, 0 unexpected mismatches, 24 documented divergences**.
* **Warning-free** under the strict flag set listed earlier, with `-Werror` on.

`tools/scheme_probe.py` records what the reference actually does; every
expectation in the test suite and the corpus came from running it rather than
from reading the source.

## A reference bug worth calling out: `case` never worked

`_sf_case` calls `_to_py_list(clause[0])`. But `clause[0]` is *parsed code* — a
Python `list`, not a `Pair` chain — so `_to_py_list` walks nothing, finds a
non-`NIL` value, and raises `Scheme error: expected a proper list`. Every `case`
form fails, including the `else` branch:

```
(display (case 1 ((1) 'one) (else 'other)))   →  ❌ Scheme error: expected a proper list
```

Because this port represents code as pairs throughout, the same operation does
the intended thing and `case` works. Reproducing this bug byte-for-byte would
have meant shipping a dead special form, so it is fixed and documented instead.

## Deliberate divergences

Beyond the `case` fix, the port differs from the reference in these ways. Each is
listed with its reason, and `scheme_differential_check.py` asserts the full set.

**Depth** — the point of the exercise. Deep tail and non-tail recursion, deep
list construction, and deep structural `equal?` all succeed here and abort with
`Maximum recursion depth exceeded` in the reference.

**The unspecified value renders as `#<unspecified>`.** The reference prints
`str(object())`, i.e. `<object object at 0x7f...>`, embedding a heap address. A
memory address is not reproducible information, so it cannot be matched.

**Python exceptions that escaped `execute_scheme()` now raise proper Scheme
errors.** The reference catches only `SchemeError`, `RecursionError` and
`MemoryError`, so several ordinary mistakes crashed the executor instead of
reporting a problem:

| Program | Reference | Port |
| --- | --- | --- |
| `(sqrt -4)` | escapes `ValueError` | `Scheme error: sqrt: math domain error` |
| `(list-ref '(1 2) 9)` | escapes `IndexError` | `Scheme error: list-ref: index out of range` |
| `(vector-ref (vector 1) 9)` | escapes `IndexError` | `Scheme error: vector-ref: index out of range` |
| `(list-ref '(1 2) 'a)` | escapes `ValueError` | `Scheme error: list-ref: expected a number, got symbol` |
| `(car)` | escapes `TypeError` (wrong arity) | `Scheme error: car: expected 1 argument(s), got 0` |
| `(/ )` | escapes `IndexError` | `Scheme error: /: expected at least 1 argument(s), got 0` |
| `(modulo 1 0)` | escapes `ZeroDivisionError` | `Scheme error: division by zero` |
| `(map +)` | escapes `ValueError` from `min()` | `Scheme error: map: expected at least 2 argument(s), got 1` |

**Integer range.** Python integers are arbitrary precision; `int64_t` is not.
Results that would exceed 64 bits promote to `double` instead of widening
(`(expt 2 100)` prints `1.2676506002282294e+30` rather than the exact
31 digits), and a literal too large for 64 bits becomes a real.

**Rendering of integral reals beyond 2^63** is *not* a divergence, despite
appearances: `_default_float()` renders an integral real via `str(int(x))`, which
Python evaluates exactly, so `(display 1e30)` must print
`1000000000000000019884624838656`. The port expands the digits with a fixed-format
`to_chars` for exactly this reason — the exponent form would have been wrong.

**Compound values passed to `string-append`** use Python's `repr` in the
reference (`x<time_warp.languages.scheme.Pair object at 0x...>`), which is again
an address. `python_str` falls back to the `write` rendering.

**Non-ASCII case mapping.** `string-upcase`/`string-downcase` are ASCII-only;
Python's are Unicode-aware.

## Preserved quirks

Carried over deliberately, each verified against the reference:

* Only `#f` is false — `0`, `()`, `""` and the unspecified value are all truthy,
  so `(if 0 't 'f)` is `t`.
* `#t` equals `1` and `#f` equals `0`, because Python compares booleans with
  numbers, so `(equal? #t 1)` is `#t`.
* A symbol equals a string with the same characters, because `Symbol` subclasses
  `str`: `(equal? 'abc "abc")` is `#t`.
* `eq?` is *not* deep for pairs: `(eq? '(1 2) '(1 2))` is `#f`.
* **There is no dotted-pair syntax.** `.` is just a symbol, so `'(1 . 2)` is a
  three-element list and `(length '(1 . 2))` is `3`. Only `lambda`'s `. rest`
  parameter gives it meaning, and only because `_parse_params` compares it by
  name.
* **There is no `#(...)` vector literal**; `#` lexes as a symbol.
* Characters are plain strings: `#\a` is `"a"`, and `#\A` keeps its case.
* A string nested inside a list is written *quoted* even under `display`, because
  `_display_str` delegates to `_write_str` for anything that is not a
  top-level string.
* Integral reals print as integers (`2.0` → `2`, `-0.0` → `0`).
* `round` is Python's, i.e. half-to-even: `(round 2.5)` is `2`.
* `quotient` truncates but `remainder` is `fmod` and `modulo` is floored:
  `(remainder -7 2)` is `-1`, `(modulo -7 2)` is `1`.
* `integer?` is true only for exact integers: `(integer? 3.0)` is `#f`.
* Missing arguments bind to the empty list and surplus ones are dropped:
  `((lambda (a b) (list a b)) 1)` is `(1 ())`.
* `set!` on an unbound variable *defines* it rather than failing.
* `(define x)` and a displayed unspecified value are distinguishable only by
  their rendering, which is why the port needs `#<unspecified>`.
* `do` initialisers are evaluated against the frame being built, and steps are
  assigned one at a time, so a later step sees an earlier update:
  `(do ((i 3 (+ i 1)) (j i)) ((> i 3) (list i j)))` is `(4 3)`.
* `cond` supports `=>`, and a test-only clause yields the test's own value:
  `(display (cond (7) (else 1)))` is `7`.
* `string->number` accepts and then *ignores* a radix argument.
* `substring` and `list-tail` clamp like Python slices, and negative indices
  count from the end.
* `string=? ` — with a trailing space — is registered as well as the correct
  spelling, so both are kept.
* `(append)` is `()`, `(append '(1 2))` is `(1 2)`, and the final argument
  becomes the tail rather than being copied.

## Bugs the differential run caught

Besides the `case` discovery, two were genuine port defects that only a
byte-for-byte comparison surfaced:

* **`mul_overflow` relied on undefined behaviour.** It computed `a * b` and then
  tested `product / b != a`. Signed overflow is UB, GCC folds that test away
  under the assumption that overflow cannot happen, and the port silently
  produced wrapped results: `(expt 2 63)` printed `-9223372036854775808` and
  `(* 4611686018427387904 4)` printed `0`. Magnitudes are now compared before
  multiplying. Both cases are regression-tested.
* **Integral reals beyond 2^63 were printed in exponent form.** `(display 1e30)`
  produced `1e+30` where the reference produces every digit. Now expanded with a
  fixed-format `to_chars`.

Two smaller ones came from the unit tests: `map` and `filter` skipped every other
element because the frame's `index` was initialised one step ahead of the row
just dispatched, and `parse_real_token` accepted `"."` as the real `0.0` (Python's
`float(".")` raises), which broke `(define (f x . r) ...)` by turning the rest
marker into a number.

The quasiquote driver also needed a *session stack* rather than a single flat
stack: `` `(a ,(let ((x 5)) `(b ,x)) c) `` evaluates a quasiquote inside an
unquote, and a single driver would have clobbered the outer template's
partially-built accumulator.

# Canvas command queue (`timewarp_canvas`)

Bridges an independent interpreter thread to the foreground Qt6 GUI thread.

## The problem it replaces

`ui/output.py` runs the interpreter on an `InterpreterThread(QThread)` and talks
to the GUI with **a Qt signal per message**. For turtle graphics that means one
queued-connection post — a heap allocation plus an event-loop wakeup — for every
single `forward(...)`. A loop drawing 200,000 segments pays that 200,000 times,
and the marshalling cost dominates the drawing it is marshalling. On top of
that, the interpreter mutates a `TurtleState` that the canvas reads, which is a
data race that CPython's GIL only covers by accident.

The queue replaces both mechanisms at once:

* the interpreter appends to a preallocated ring — no allocation, no syscall, no
  lock on the hot path;
* the GUI applies an entire batch under **one** wakeup instead of one per
  command;
* the only cross-thread interaction is the queue itself, so the shared mutable
  turtle state goes away. The GUI owns its own mirror and applies commands to it.

## Design

**Single producer / single consumer.** The application runs one interpreter at a
time, so the ring can be genuinely lock-free: `head_` is written only by the
producer and `tail_` only by the consumer, which is what makes the
read-test-publish sequences safe with no lock at all.

**A byte ring with free-running counters.** `head` and `tail` are monotonic byte
offsets, never wrapped, so "empty" and "full" are both representable and the
buffer index is just `offset & mask_`. Capacity is a power of two, so the mask
replaces a modulo.

**Release/acquire pairing.** The producer writes a whole record and *then*
publishes `head` with a release store; the consumer acquires it. Therefore
`tail < head` guarantees the entire record at `tail` is readable — no partial
read is possible.

**Variable-length records.** A record is a fixed 72-byte `CanvasCommand` header
followed by an optional point list and optional UTF-8 text. The lengths live in
the header, so there is no arbitrary cap on a polygon or a string. Records are
packed across the end of the ring (at most two `memcpy` each way) rather than
padded, so the ring stays dense.

**Blocking, without a mutex.** When the ring is full the producer parks in
`std::atomic::wait`. No `std::mutex`, no `condition_variable`, no kernel object
until the wait spins down.

**Coalescing happens in producer-private staging.** This is the subtle part.
Rewriting a slot that has already been published would let the consumer observe a
half-written record — a 72-byte store is not atomic. So the pending coalescible
command is *held back* instead: nothing published is ever mutated, and the race
cannot arise. The staged command is published when a non-coalescible command
arrives, on `flush()`, or once `set_coalesce_limit()` replacements have
accumulated, so a caller that only ever sets state still makes progress.

## Coalescing semantics

Only `is_coalescible()` kinds collapse, and only into an **identical, adjacent**
predecessor:

```
set_heading(90); forward(10); set_heading(45)
```

keeps both `set_heading` commands, because the second is not adjacent to the
first. Collapsing across the `forward` would silently change where the pen
travelled. `PenUp`/`PenDown` are deliberately excluded even from each other: the
pair is a toggle, not two assignments to one value.

The queue therefore **never drops and never reorders**. It will reorder nothing
because the staged command is held in order, and it will drop nothing because
state that is superseded is the one thing that cannot matter.

Liveness note: because a staged command is not yet visible, the consumer can lag
by up to one coalescible command. A paused interpreter should call `flush()` so
trailing state is published — the `Host` adapter does this, and the Qt bridge's
poll timer picks up the rest.

## The Qt bridge

`qt_bridge.hpp` is the only Qt-facing file in `native/`. It hands work to the GUI
thread with `QMetaObject::invokeMethod(..., Qt::QueuedConnection)`, Qt's
supported cross-thread mechanism, and **collapses a storm of wakes into a single
pending invocation**.

That collapsing is a two-phase handshake rather than a latch, because a plain
latch strands commands. Clearing the flag *after* a drain lets a push that lands
mid-drain see it still set, skip its post, and then wait for a wakeup already
consumed; clearing it *before* has the mirror-image problem when the push
completes just after the drain's last read. So `drainNow()` clears the flag,
drains, and loops while the flag was set again. The two sides agree through an
atomic read-modify-write on the same word: whichever of them observes the other's
write takes responsibility. They are totally ordered, so exactly one always does.

Signals are emitted **per batch, never per command** — a count, not the commands.
The commands go to a plain `std::function` sink, because emitting a signal per
command would reintroduce exactly the cost the queue exists to remove.

## Measured

`tools/canvas_queue_bench.cpp`, GCC 16.1, `-O3`:

| Workload | Throughput | Per command |
| --- | --- | --- |
| single-threaded push + drain | 58.1 M/s | 17.2 ns |
| SPSC through a ring too small to hold the run | 10.8 M/s | 92.3 ns |
| state pushes, coalescing on | 166 M/s | 6.0 ns |
| geometry pushes, nothing coalescible | 58.8 M/s | 17.0 ns |

The SPSC figure is the honest one: the producer genuinely fills the ring and
parks (21 blocking episodes in that run), and the consumer wakes it. The
coalescing figure is the payoff — **1,000,000 consecutive state changes collapse
to 977 published records**, so the Qt bridge posts ~977 event-loop wakeups rather
than 1,000,000. Signalling per command would have cost 1,000,000 posts.

## Verification

* **6,078 checks** in `canvas_queue_tests`, covering the value type, round trips,
  payloads that wrap the ring, coalescing (including that geometry is never
  collapsed), blocking, timeouts, close, and two concurrency stress tests.
* **26 checks** in `canvas_bridge_tests`, driving a real `QCoreApplication` and a
  real producer thread: cross-thread delivery, the wake storm costing one post,
  a race between a pumping consumer and a pushing producer, the poll fallback,
  and end-to-end translation from `scheme::Host` turtle calls.
* Stable across repeated runs (5× queue, 3× bridge) — the concurrency tests are
  not flaky.
* Warning-free under the strict flag set, with `-Werror` on.

## The bug the tests caught

`close()` originally unblocked a parked producer by calling `tail_.notify_all()`.
It deadlocked, and the reason is easy to get wrong:

**`std::atomic::wait` re-checks the value after every notification and goes back
to sleep if it is unchanged.** So notifying a word you do not modify never
releases a waiter. Measured on libstdc++ directly — notify-only leaves the waiter
parked; bump-then-notify wakes it. Since `tail_` has a single writer and `close()`
is not it, `close()` cannot bump it, so the producer now parks on a dedicated
`space_released_` word that *both* the consumer and `close()` increment.

This was the anti-deadlock valve itself deadlocking, which is the worst possible
place for that bug: the mechanism whose entire job is to guarantee a blocked
producer can always terminate was the mechanism that could hang forever.

Two tests had to be corrected as well, and both were my errors rather than the
queue's: one asserted that a 200,000-command run must have blocked the producer
(false — a consumer that keeps up means the ring legitimately never fills), and
two pushed more commands than the ring holds without draining, which deadlocks
*by design* under a never-drop policy.

# Background debugger runner (`timewarp_debugger`)

Runs a Scheme program on its own thread and services pause, step, breakpoint and
stop requests from the GUI thread. Built only when Qt6 is found; the debugging
*logic* lives in `timewarp_scheme` and needs no Qt at all.

## The problem it replaces

The Python IDE runs programs on a worker thread and drives the debugger from the
GUI thread. That works, but the two directions of control are asymmetric and the
asymmetry is where the bugs live: output and pause notifications travel one way
through Qt signals, while pause/step/continue travel the other way through shared
mutable state that both threads read. There is no single place that says what a
"pause request" *is*, so every new control has to reinvent the handshake.

## Design

**`DebuggerRunner` subclasses `QThread`, and `run()` is the interpreter loop.**
It is not a Qt event loop. That is the whole design, and it has one consequence
worth stating loudly: a `QThread` *object* lives in the thread that constructed
it, so its slots would execute on the **GUI** thread, not in `run()`. Subclassing
`QThread` and expecting its slots to arrive in the worker is the classic Qt
threading mistake, and queued slot invocations aimed at this object would never be
delivered at all, because there is no event loop on the runner side to deliver
them.

So control does **not** go through slots. `request_continue()`, `request_pause()`,
`request_step_*()` and `request_stop()` are plain thread-safe methods, callable
from anywhere, and they publish into an explicit command channel. They never touch
interpreter state. The reverse direction *does* use signals, because Qt signals
emitted from a worker thread are genuinely safe and arrive queued on the GUI
thread.

### The command channel

```text
GUI thread                          runner thread
----------                          -------------
post(cmd)                           take_command()      <- one atomic load per form
  lock, command_ = cmd                lock, read + clear
  release                             mutex only when a command is actually pending
  notification_.fetch_add(1)
  notification_.notify_all()
```

The wakeup word is **incremented**, never merely notified. `std::atomic::wait`
re-checks the value after every notification and goes back to sleep if it is
unchanged, so `notify_all()` on a word nobody modified releases nobody. The same
lesson cost an afternoon in the canvas queue; here it is load-bearing from the
start. Snapshotting the sequence *before* testing the command also means a request
that lands between the two cannot be slept through.

`Stop` is sticky as well as posted, so it survives the case where the runner is
not parked at all — including a stop requested before the thread has even started.

### Breakpoints

Replacement sets are published under a mutex with an atomic generation counter.
The runner caches the set and only re-reads it when the generation changes, so the
hot path is one atomic load per form and never takes a lock. Adding or removing a
breakpoint mid-run therefore takes effect on the very next form.

A breakpoint matches a form's **opening** line, and fires on *every* form that
opens there. The obvious refinement — stop only on the first form of the line —
requires knowing that execution left the line and came back, and that is not always
observable: a tail-recursive loop whose body sits on one line never reports another
line, so a breakpoint added there would silently never fire. Over-stopping is
recoverable with one extra press of continue; a breakpoint that does nothing is
not. This is a deliberate choice, not an oversight.

## Making the machine resumable

The evaluator already was a state machine — `mode_` plus the frame stack fully
describe where execution has got to — so what changed is that the loop can now
*return*:

* `load()` reads and prepares a program without running it, optionally attaching a
  `SourceMap`.
* `step_quantum(n)` runs at most `n` evaluation steps and returns
  `Finished`, `QuantumExhausted` or `Paused`. Calling it again resumes.
* `run_loaded()` is `step_quantum(unlimited)`.
* A `SuspensionController` is consulted at the start of every **compound form**.
  Returning `false` suspends; a null controller means the check is a single
  pointer test, so an ordinary run is untouched.

Suspension is cheap here precisely because the machine has no native stack to
preserve: returning out of the loop leaves everything in the heap-allocated frame
stack. That is the payoff from porting the recursive evaluator to an explicit
stack in the first place.

Two details make the resumption exact:

* The controller is consulted **before** the step is charged, and the suspended
  form is **re-tested rather than re-entered**. A program that suspends on every
  single form therefore produces byte-identical output *and an identical step
  count* to one that never suspends. Both facts are asserted directly.
* Resuming always leaves the form it was stopped at alone. The runner keys this
  off the step count, which is precise: the parked form keeps its count until it
  is charged, and anything after it has moved on.

## Tail calls versus a visible call stack

The conflict is real. A naive "one frame per call" activation marker turns a
tail-recursive loop with a million iterations into a million markers. The
resolution: a **tail call replaces the caller's marker instead of stacking inside
it**. That is not a compromise — a tail call genuinely does not deepen the stack,
so the reported stack stays semantically honest, and a 5,000-iteration loop reports
a two-entry stack (and a 1,000,000-iteration benchmark the same). Markers are
pushed *only* while a controller is attached, so a plain run's frame layout and
tail behaviour are exactly what they were.

Each marker also saves the **caller's** tail-position base and restores it on the
way out, and a tail call lets the callee inherit it. Deriving that base from the
marker's own height instead looks equivalent and is not: the frames between the
caller's base and the marker belong to the call site and are gone by the time the
marker is popped, so a derived height leaves `at_tail_position()` comparing against
a stack shape that no longer exists — and the next tail call is silently lost. A
test recurses 300 times through a nested call followed by a tail call specifically
to pin that down.

## Source maps

`Value` is a 16-byte tagged union chosen for cheap copying, and `Pair` is two
`Value`s with nothing to spare. Widening either would tax every value to serve a
feature almost no run uses, so positions live in a side table keyed by the
arena-stable `Pair*`, populated only when a map is requested. Every pair the reader
builds on a list's *spine* is recorded, not just the head — otherwise a
`cdr`-derived sublist, which is exactly what a procedure's body is, would have no
position at all.

`call_stack()` reports a frame at the line where the procedure's **body opens**,
not at the `define`. The body is a cdr node, and its recorded span is the whole
definition, so the naive lookup points at line 1 of a multi-line `define`.

## Verification

* **192 checks** in `debug_tests` (Qt-free): line tracking through comments,
  multi-line forms and multi-line strings; form spans including nested ones; the
  optional-map path; quanta of 1, 7 and 1000 converging on the same output *and*
  the same step count as a single run; suspending at every opportunity changing
  nothing; a line breakpoint landing on the right form, procedure and depth;
  repeated suspension not growing the frame stack; call-stack naming; tail calls
  staying flat while non-tail recursion nests; the tail-call-after-nested-call
  regression; and variable inspection with and without a frame limit.
* **191 checks** in `debugger_runner_tests`: a real `QCoreApplication` and a real
  second thread, covering breakpoint pauses and full snapshot contents, breakpoints
  added and removed mid-run, step-into/over/out (asserting that a step always wins
  over a breakpoint set on the same line), a runtime error held open when
  break-on-error is enabled, pausing a running program then stopping it, pausing
  while already paused (which must *not* un-pause), a stop requested before the
  thread exists, destroying a runner mid-run, and a 12,000 command storm from a
  third thread.
* Stable across 5× runner and 3× core repeats. The threads, the wakeups and the
  destructor-joins are the parts most likely to hang, so they get the repeats.
* Warning-free under the strict flag set, with `-Werror` on.

## The bugs the tests caught

* **A nested call losing the tail call that followed it** — the base bookkeeping
  described above. The tail-call tests passed the whole time; only the
  nested-then-tail case exposed it.
* **`Machine::emit` erased by a macro.** Qt defines `emit` as a macro, so a member
  with that name is silently deleted in any translation unit that also includes a
  Qt header. The symptom was an inexplicable parse error in `machine.hpp`, in a
  header that had compiled for weeks. Renamed to `append_output`, with a note
  saying why.
* **`load_forms` not resetting the step count.** Harmless until a debugger
  displayed it; a fresh program then inherited the previous one's count.
* **"Continue" re-firing the breakpoint it resumed from.** Returning from a
  suspension re-tests the same form, which matches the same breakpoint, which
  suspends again — so the program could never get past a breakpoint. This is
  precisely the behaviour every test in this area depends on, which is why the test
  controller models it too.
* **Only list heads having spans**, so every procedure body looked position-less.
* **A queued signal delivered after its receiver's frame had returned.** The test
  harness connected each runner signal with the *application* as the connection
  context and captured a stack-local recorder by reference. A queued connection
  outlives the emit — the event sits in the receiver's queue until the next
  `processEvents()` — so a `run_finished` event emitted during one test was
  delivered during the *next* one, writing into a frame that no longer existed.
  AddressSanitizer names this precisely (`stack-use-after-return`, and the ASan
  report even points at the frame that owned the recorder); without it the
  symptom was a segfault two tests later, in the middle of an unrelated
  `QString` destructor, which is where a day of debugging can go. The fix is to
  use the object that *owns* the captured state as the connection context, so
  `~QObject` discards its pending events and the functor cannot outlive what it
  captured. This is the failure mode `make asan` exists for.

## Known limits, stated plainly

* Stop granularity is a compound form. A program of nothing but literals has no
  compound forms, so nothing is checked between the first form and the end.
* Step-over is form-level, not line-level: it will stop again on a sibling
  sub-expression of the same line. A line-level stepper needs the source map to
  answer "the next form on a different line", which it cannot yet.
* A breakpoint on a line that no form *opens* on never fires, even if forms cover
  that line. The line reported at a stop is the one the form opens on, so the two
  are at least consistent with each other.

# Desktop UI (`timewarp_ui`)

The main-window layout in Qt 6 Widgets, translated from the PySide6
implementation's `main_window.py`. That implementation has since been removed from
the repository: this is the *layout* -- the widget tree, the container structure
and every sizing decision -- not the panels themselves, which are thousands of
lines each and are represented by clearly labelled stand-ins.

## The widget tree

```text
QMainWindow                                   #mainWindow
├── QMenuBar                                  #mainMenuBar     File Edit Run Debug View Tools Help
├── QToolBar                                  #mainToolBar     13 controls in five groups
├── centralWidget
│   └── QVBoxLayout  (margins 5,5,5,5)
│       ├── QSplitter  horizontal              #mainSplitter        handle 6, stretch 3:2, sizes 720:480
│       │   ├── QSplitter  vertical            #editorColumnSplitter handle 6, stretch 1:0, sizes 550:50
│       │   │   ├── QSplitter  horizontal      #editorPaneSplitter   handle 5, stretch 1:1
│       │   │   │   ├── ResizableTabWidget     #editorTabs          closable, reorderable
│       │   │   │   └── ResizableTabWidget     #editorTabsSecondary  hidden until a split is asked for
│       │   │   └── ImmediateModePanel         #immediateMode       minimum 44px
│       │   └── ResizableTabWidget             #rightTabs           fixed panes, not closable
│       │       ├── "📝 Output  🎨 Graphics" ──► QSplitter vertical  #outputCanvasSplitter  handle 5, sizes 200:300
│       │       │                                 ├── QTabWidget     #outputSubTabs   South, documentMode
│       │       │                                 │     Console | Errors | Turtle Log | Terminal
│       │       │                                 └── Canvas         #turtleCanvas
│       │       ├── "🔍 Variables"
│       │       ├── "⚡ Telemetry HUD"
│       │       └── "🐛 Debug"
│       └── FunctionKeyBar                     #functionKeyBar      fixed 30px
├── QStatusBar                                 #mainStatusBar
│       [message area: breadcrumb] ... [permanent: languageBadge | positionBadge | executionBadge]
└── QDockWidget                                #documentOutlineDock  Right|Left, hidden by default
```

Geometry that is load-bearing is transcribed rather than re-derived, and asserted:
`childrenCollapsible(false)` everywhere (a zero-width pane leaves no handle to
drag back), the handle widths, the stretch factors, and the initial sizes.

## Why a `ResizableTabWidget`

`QTabWidget::minimumSizeHint()` is the **maximum** of every page's hint. Qt hides
pages by reparenting them out of the layout, but the hint still considers all of
them -- so one large page pins the minimum size of the whole window even while a
small page is showing, and no splitter holding it can shrink. The subclass
returns a small constant minimum and derives the *preferred* size from the current
page instead. That single change is what makes the right-hand column draggable and
the window resizable on a small display, and it is asserted directly: a 1400x900
page next to a 60x40 page must still report a 100x100 minimum.

## What changed from the Python, and why

| Change | Reason |
| --- | --- |
| **No back-pointer to the window** | The Python tab widget holds a `MainWindow` reference and calls `mw.close_tab()`, reads `mw._tab_states`, and `getattr`s its way around two levels of privacy. Every non-local action is a signal here, so the widget can be constructed and tested alone -- and tab state stays owned by whoever owns the files. |
| **One stylesheet, applied once** | Qt copies a widget's stylesheet to its children and re-polishes all of them on every `setStyleSheet()` call; the Python makes six such calls on a window with hundreds of widgets. Composed into one string, and scoped by object name. |
| **Scoped selectors** | Bare `QTabWidget { ... }` cascades to every descendant tab widget, including the output sub-tabs nested inside a page of the styled widget -- which is why the Python needed a compensating override. Naming the widgets makes the cascade intentional. |
| **Splitter positions are persisted** | The Python saves window geometry and dock state but *not* splitter positions, so a dragged editor/output divider is forgotten on every restart. Fixed, with a round-trip test. |
| **The CRT overlay tracks its own target** | The Python resizes it from `MainWindow.resizeEvent` via `overlay.parent().rect()`, which silently stops working if the overlay is ever reparented. It installs an event filter on the widget it covers instead, so `MainWindow::resizeEvent` is gone entirely. |
| **Menus delegate** | `create_menus` is ~600 lines in one method. This defines the menu skeleton and their mnemonics; the action catalogue belongs to the feature that owns it. |
| **Widgets are named** | `saveState()` silently skips an unnamed toolbar or dock, the stylesheet scopes by name, and the tests locate nodes by name. Naming is a precondition for two of those, not a nicety. |
| **Standard key sequences** | `QKeySequence::New/Open/Save` instead of literal `Ctrl+N`, so the same code produces `Cmd+N` on macOS. The tooltip keeps the Linux spelling. |

## Verification

* **146 checks** in `ui_layout_tests`, run offscreen so they need no display and
  belong in the normal CTest set. They assert the tree shape, parent/child
  identity at every splitter index, the collapsible and handle-width flags, the
  stretch factors (read back from the child's size policy, since `QSplitter` has
  no getter), the split proportions, tab titles and order, the tooltip coverage of
  every toolbar button, status-bar badge placement, dock areas, menu titles, the
  overlay's geometry tracking across a resize, and the persistence round trip.
* The splitter *behaviour* is tested, not just the numbers: growing the window must
  preserve the 3:2 editor/output proportion. With equal stretch the canvas would
  grow as fast as the code, and the ratio would drift towards 1:1.
* **The document tests** in `ui_document_tests` cover the parts that touch a user's
  files: the extension-to-language table (every alternate spelling the Python
  registry knows), the editor's gutter width and 1-based cursor reporting, and the
  open/save/close lifecycle. Two of its assertions were bugs when they were written:
  `closeTab` accepted an out-of-range index because `removeTab` treats one as a
  no-op, and choosing a language in the selector updated nothing else, because the
  badge and the title are re-derived rather than set. The unreadable-file and
  cancelled-save paths are **not** tested, deliberately: both end in a modal dialog,
  so a test that triggered one would hang rather than fail. Both failures now go
  through a silent mode that returns a value, and that path is tested.
* **80 checks** in `turtle_canvas_tests` cover the state mirror and the
  projection, with no window involved. The projection gets the most attention
  because it is the one part a picture cannot verify: a canvas that draws a
  mirrored or quarter-turned version of a program produces something that looks
  like a perfectly good drawing. Each convention the reference uses -- north at
  0, clockwise headings, the y flip, `circle` as a polygon walk -- is an
  assertion, so choosing the *conventional* alternative would have failed.
* `ui_document_tests` runs a real Scheme program through the whole pipeline
  (editor text, runner thread, turtle host, queue, bridge, canvas) and checks the
  geometry rather than "something happened": 100 north then 100 east has to end
  at `(100, 100)` with a heading of 90. It also proves Stop interrupts a
  runaway loop and that the window still closes afterwards.
* `time_warp_ide` renders the window to a PNG with `--screenshot <path>`, so the
  result can be looked at without a display -- add `--run` to execute the file
  first. That is how the findings below were caught; a structural test would have
  passed either way, and the status bar painting a message on top of the document
  path was visible in exactly one place and nowhere else.

## Two findings from rendering it

* **The toolbar did not fit a 1400px window.** The single flat toolbar carries
  thirteen controls, and the trailing ones (Clear Canvas, A+, the turtle speed
  slider) are clipped at that width with no extension button to reach them; at
  1920px they all fit. This was inherited from the Python layout rather than
  introduced here. **Fixed**: the display controls now sit on a second toolbar
  row, so nothing is behind a chevron at 1000px.
* **A fresh window must have a document tab.** The Python calls `new_file()` while
  building the UI; without the equivalent the editor pane is an empty box and the
  tab bar is invisible, which makes the editor column look broken. **Fixed**: the
  window opens a real `CodeEditor`, and closing it goes through the same path the
  rest of the documents do.

## The editor and documents

`CodeEditor` is a `QPlainTextEdit` with a line-number gutter, a current-line
highlight and cursor tracking. On top of it the window implements the document
lifecycle: open, save, save-as, close, and the unsaved-changes prompt the Python
window does not have.

A document's identity is stored on its widget -- the path as a dynamic property,
its language derived from that path -- rather than in a table beside the tabs. A
table keyed by tab index has to be repaired on every close, move and reorder, and
the one time it is not is the one time the wrong file's text is written to the
wrong name.

* Opening a file that is already open raises its tab instead of loading a second
  copy. Two copies of one file means the last save wins and the other window's
  work disappears, which is data loss with no undo.
* Saves go through `QSaveFile`, so a failure leaves the previous contents intact
  instead of truncating the user's program to zero bytes.
* A UTF-8 byte-order mark is stripped on load. Left in place it is an invisible
  first character, and the first line then fails to parse with an error pointing
  at a column that looks empty.
* Newlines are normalised to `\n` on load, as the Python editor does; a CRLF file
  therefore round-trips as LF.

## The canvas, and what runs

**All nine languages run.** Scheme goes through the C++ interpreter on a background
thread; BASIC, PILOT, Logo, COMAL and REXX go through the **Python bridge**, which
runs the existing Python implementations in a child process and streams what they
draw back as canvas commands. Every one of the 44 examples in `Examples/` executes,
and the 14 that draw produce drawing -- checked by `tools/example_smoke.py`.

The bridge exists because reimplementing eight languages is the work it postpones.
Python is the oracle while each language is ported, and a language stops being
routed to it the day its C++ interpreter lands.

`TurtleCanvas` is a real graphics surface, not a stand-in. Whichever engine runs,
the path is the same: turtle calls become `CanvasCommand`s, the producer pushes
them into a `CanvasCommandQueue`, and `CanvasBridge` drains that queue on the GUI
thread and hands each command to the widget. The canvas keeps a mirror of the
turtle state, a display list of everything drawn in world coordinates, and a
painter.

### How the bridge captures a drawing

`QueuedTurtleHost` translates the C++ interpreter's turtle calls one-to-one. The
Python side cannot do that -- the reference *is* the interpreter -- so it **diffs
the reference's own line and shape lists** instead of overriding every turtle
method. That choice is deliberate: an override list has to be complete, and the one
method somebody forgets is a feature that silently stops working, whereas a diff of
an append-only list cannot miss anything that was drawn. The only overrides are
`clear`, `reset` and `setbgcolor`, because a diff cannot see an erasure or a
background change at all.

That last one is not a detail. The reference's pen is white and its canvas is
`(10, 10, 20)`; a client that ignored the background would draw white ink on its own
white canvas and show nothing -- the exact symptom this bridge was built to fix.

The protocol lives in `native/bridge/worker.py` and is documented at
the top of that file. It is one JSON object per line, because a line is the only
framing a pipe gives you for free.

### The conventions are the reference's, not the obvious ones

Transcribed from `graphics/turtle_state.py`, and asserted in
`turtle_canvas_tests`, because a canvas that draws a mirrored or quarter-turned
version of the same program produces something that looks like a perfectly good
drawing:

* heading 0 is **north** (+y), not east
* heading increases **clockwise**, so `right(a)` adds and `left(a)` subtracts
* `forward(d)` moves by `(d*sin h, d*cos h)`
* world +y is up, so the screen projection flips it
* `circle(r)` is a **polygon walk** of `max(12, |extent|/10)` chords, not an arc

That last one matters: asking the painter for a smooth arc would be prettier and
would not match the Python build. Matching also means the two agree on how many
strokes a circle costs.

### Design decisions worth knowing

* **A display list, not a pixmap.** Repainting on resize, zoom and pan needs the
  geometry again; a pixmap would go blurry when zoomed and cannot be
  re-projected at all.
* **The state mirror lives in the widget.** A `Forward` command carries only a
  distance -- not the heading to move along or the pen colour to draw with -- so
  something has to replay the sequence and hold the state. Putting it in the
  widget keeps every command a one-to-one translation.
* **The bridged path moves only the marker.** Its commands carry absolute geometry,
  so replaying `Forward` on top of them would draw every line twice; the turtle's
  position arrives as a separate message and moves the marker without leaving ink.
* **The view fits once, at the end of a run.** Refitting on every command would
  shrink the picture continuously while it is being drawn, which reads as the
  canvas fighting back.
* **Unrenderable commands are counted, not dropped.** Gradients, sprites, bezier
  curves and palette cycling need machinery this widget does not have;
  `commandsIgnored()` and the run's status message report them rather than leaving a
  silently missing shape.

## Known gaps, stated plainly

* The variables, telemetry HUD, debug, terminal and outline panels are still
  `PlaceholderPanel`s naming the Python module they stand for. The layout is real;
  their contents are not.
* The editor has no syntax highlighting, minimap, find/replace, outline or
  formatter yet. The Python editor is 2,254 lines because of those extras; they
  are worth adding one at a time against a working base, which now exists.
* `CrtOverlay` draws scanlines only. The Python also does curvature, vignette,
  glow and flicker with sliders for each; the point of including it here was the
  geometry tracking, which is where the Python approach was fragile.
* `FunctionKeyBar` reproduces the normal F1-F10 layer and the deck selector. The
  Shift and Ctrl layers are omitted rather than invented, because their actions do
  not exist yet.
* Debug, Continue and Trace are laid out and inert: they need the breakpoint and
  stepping UI of the debug panel. Stop is wired, and stops a bridged program by
  killing its process if it will not stop on its own.
* A bridged program's input prompts appear as a dialog, which is the one modal
  dialog in the program -- and the one place a scripted run could block. Nothing in
  the tests or in CI reaches it.

# Suggested next steps

1. Wire `native/` into the repository `Makefile` / CI as an independent job.
2. Add the runner bridge: an out-of-process Python worker speaking JSON lines, so
   the five unported languages can execute while their C++ replacements arrive.
3. Give the output console and the turtle canvas their real implementations, the
   canvas consuming the `CanvasCommandQueue` that already exists and is tested.
4. Extend the same treatment to the remaining interpreter units, in dependency
   order: `string_evaluator.py`, then the line dispatching in
   `core/interpreter.py`, then the smaller executors (PILOT → COMAL → REXX),
   leaving BASIC last.
4. When the interpreter core moves, `SpecialVariableHook` (expressions) and
   `Host` (Scheme) are the seams for engine-specific services — no further
   engine-specific imports should reappear in either library.
5. Consider a `CompiledExpression` handle (interned identifiers + owned RPN) and
   a Scheme `Procedure` cache, so programs can be compiled once at load time
   rather than re-read per call.
6. Move the Python application onto the queue: give `TurtleCanvas` a mirror of
   the turtle state that consumes `CanvasCommandView`s, hand
   `QueuedTurtleHost` to the interpreter in place of the shared `TurtleState`,
   and let `InterpreterThread` stop emitting a signal per drawing call.
7. Repoint the Python debug panel (`ui/debug_panel.py`, `core/debugger.py`) at
   `DebuggerRunner`: its breakpoints, step controls, call stack and watch list map
   onto `set_breakpoints()`, `request_step_*()`, `call_stack()` and
   `visible_variables()` respectively. The engine's line-level execution timeline
   is the one feature with no counterpart yet.
8. Port the panels into `timewarp_ui`, in dependency order: the text output panel
   first (it is self-contained), then the turtle canvas on top of
   `timewarp_canvas_qt`, then the variable inspector and telemetry HUD driven by
   `DebuggerRunner`. Each swap replaces one `PlaceholderPanel` construction and the
   layout tests keep passing unchanged.
9. Split the toolbar into two rows: primary actions (file, run, debug) and
   secondary display controls. The current single row overflows below roughly
   1900px, which is every laptop.
