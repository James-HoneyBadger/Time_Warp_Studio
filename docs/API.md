# Time Warp Studio API

The Studio exposes two API surfaces:

- **The automation API** — JSON-RPC 2.0 over newline-delimited JSON, for a
  script, a test or an editor plugin. `./api` starts it from a source checkout;
  an installed tree has `time_warp_api` on `PATH`.
- **The C++ libraries** — the same engines the application uses, linkable by
  another program. `timewarp::lang`, `timewarp::scheme`, `timewarp::sql`,
  `timewarp::project` and `timewarp::api`.

The [user guide](USER_GUIDE.md#9-automation) has a worked introduction; this page is
the method and payload reference.

Both are local. The automation API has no socket, no port and no network
listener: a caller that can write to its stdin can already run programs on the
machine, so there is no authentication layer pretending otherwise.

---

## The automation API

```bash
$ ./api
{"jsonrpc":"2.0","id":1,"method":"studio.info"}
{"jsonrpc":"2.0","id":1,"result":{"apiVersion":"1.0", ...}}
```

One request per line, one response per line, in request order. A line that is not
valid JSON gets a `-32700` response. `id` may be a string, a number or null; a
request without `id` is a notification and gets no response. Batches (a JSON
array of requests) are supported; notifications inside a batch are omitted from
the response array.

```bash
printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"language.run","params":{"language":"basic","source":"10 PRINT 2+2"}}' \
  | ./api
```

### Methods

#### `studio.info`

No parameters.

```json
{
  "apiVersion": "1.0",
  "version": "16.4.0",
  "languages": [{"key":"basic","display":"BASIC","wholeProgram":false}, ...],
  "features": {"repl":true,"sql":true,"projects":true,
               "watchExpressions":true,"dataBreakpoints":true,
               "debugger":true,"compiledPrograms":true}
}
```

#### `ping`

Returns `"pong"`. Useful to check that a process is alive.

#### `studio.shutdown`

Returns `true` and ends the process after the response.

#### `language.run`

| Parameter | Type | |
| --- | --- | --- |
| `language` | string | `basic`, `pilot`, `logo`, `comal`, `rexx`, `scheme`, `focal`, `chip8`, `apl`, `smalltalk` |
| `source` | string | the program text |
| `sourceBase64` | string | alternative to `source`, for a binary CHIP-8 ROM |
| `inputs` | array of strings | answers to `ASK`/`INPUT` prompts, in order; an exhausted list answers empty |

Result:

```json
{
  "output": "42\n",
  "lines": ["42"],
  "error": "",
  "variables": [{"name":"X","type":"number","value":"42"}],
  "drawing": {"commands": 4, "actions": 6, "svg": "<svg ...>"},
  "telemetry": {"steps": 3, "ms": 0.2, "timedOut": false, "iterationCapped": false}
}
```

`drawing.commands` counts ink commands (strokes and shapes); `actions` counts
every turtle operation, including turns and pen/colour changes that leave no
mark, so a caller can tell "accepted" from "silently ignored". `drawing.svg` is
empty when the program drew nothing. For Scheme, `variables` is the machine's
visible bindings and `telemetry` is empty.

#### `repl.create`, `repl.eval`, `repl.reset`, `repl.close`

A REPL session keeps its bindings between calls, exactly like the bar in the IDE.

```json
{"jsonrpc":"2.0","id":2,"method":"repl.create","params":{"language":"basic"}}
// -> {"session":"1","language":"basic"}

{"jsonrpc":"2.0","id":3,"method":"repl.eval",
 "params":{"session":"1","source":"LET X = 5"}}

{"jsonrpc":"2.0","id":4,"method":"repl.eval",
 "params":{"session":"1","source":"PRINT X * 3"}}
// -> {"output":"15\n","lines":["15"],"error":"", ...}
```

Scheme sessions echo the value of an expression (`(+ 20 22)` answers `42\n`),
and `define`/`set!` echo nothing. In a BASIC session, `NEW` clears the bindings and
answers `✅ BASIC workspace cleared`; `repl.reset` does the same without needing a
BASIC statement. `repl.close` releases the session. Sessions are per-process.

#### `sql.open`, `sql.close`, `sql.query`, `sql.tables`, `sql.schema`

`sql.open` takes `path` (`:memory:` for a scratch database) and creates the file
when it does not exist. It returns the path and the table list:

```json
{"path": ":memory:", "tables": []}
```

The connection is the one a run uses, so a database opened here is visible to a
`SQL` statement in a program run through `language.run`.

`sql.query` takes `sql` — one statement or a script — and returns:

```json
{
  "columns": ["name", "score"],
  "rows": [["Ada", "42"]],
  "changes": 1,
  "truncated": false,
  "error": null
}
```

At most 1000 rows are returned; `truncated` says when there were more.
`sql.tables` returns an array of names; `sql.schema` takes `table` and returns
its `CREATE` statement.

#### `project.create`, `project.open`, `project.save`, `project.info`

`project.create` takes `name`, `root` and optional `filePath`, writes the
`.twproject` and returns the project. `project.open` takes `path`.
`project.save` writes the open project. `project.info` returns:

```json
{
  "name": "Class",
  "root": "/home/learner/class",
  "filePath": "/home/learner/class/Class.twproject",
  "entry": "/home/learner/class/flower.logo",
  "groups": ["Art"],
  "files": [{"path":"/home/learner/class/flower.logo",
             "group":"Art","exists":true}],
  "missing": []
}
```

`entry` is the program the IDE's Run Project runs, as an absolute path, or
`null` when the project has none. A client that wants to run the project runs
that file -- it is the one place the project states which of its programs is
*the* program.

#### `project.addFile`, `project.addFiles`, `project.addFolder`, `project.removeFile`

`addFile` takes `path` and optional `group`; `addFiles` takes `paths` (an array)
and optional `group`. A group named while adding is created if it does not
exist. `removeFile` takes `path` and only edits the project, never the file.

`project.addFolder` takes `path`, `extensions` (suffixes with or without the
leading dot) and optional `group`. It walks the folder, adds every matching
program that is not already a member, and names each file's group after the
subfolder it was found in, prefixed by `group` when one is given. The reply is
the project plus `added`, the number of files this call added:

```json
{"added": 2, "name": "Class", "files": [], "groups": [], "missing": []}
```

#### `project.addGroup`, `project.renameGroup`, `project.removeGroup`, `project.moveFile`

`addGroup`/`removeGroup` take `name`; `renameGroup` takes `from` and `to`;
`moveFile` takes `path` and `group` (empty to ungroup). Removing a group returns
its files to the ungrouped section.

#### `project.setEntry`, `project.clearEntry`

`setEntry` takes `path` and makes it the project's entry point, adding the file
to the project when it is not already a member. `clearEntry` takes nothing and
fails when there was no entry. These are the two context-menu commands on the
Project dock, and the ones a build script would call:

Every project method returns the whole project (or, for `addFolder`, the project
plus `added`), so a caller never has to merge a delta into a copy it thinks it
has.

#### `debug.start`, `debug.snapshot`, `debug.continue`, `debug.step`, `debug.rewind`, `debug.stop`, `debug.close`

A debug session runs a program until its first pause and then answers one
request per state. `debug.start` takes the same `language`, `source`/`sourceBase64`
as `language.run`, plus optional `breakpoints` (1-based positions in the source
text, as the editor gutter numbers them), `dataBreakpoints` (variable names) and
`history` (how many snapshots the run keeps, 0 to 10000; 0 keeps none and costs
nothing). It returns when the run pauses or finishes:

```json
{
  "session": "1",
  "state": "paused",
  "paused": true,
  "finished": false,
  "line": 20,
  "depth": 0,
  "procedure": "",
  "reason": "breakpoint",
  "changedVariable": "",
  "error": "",
  "stack": [],
  "variables": [{"name":"X","type":"number","value":"1"}],
  "output": "",
  "drawing": {"commands": 0, "actions": 0, "svg": ""}
}
```

`reason` is `breakpoint`, `step`, `rewind`, `data`, `pause`, `finished` or `error`.
`stack` is the paused run's call chain, outermost first, one
`{"procedure": "…", "line": N}` per frame: the frame's own line while it is the
one running, and its call site while it is waiting. It is empty while the run
stands in the program's own body, because nothing is waiting then. Smalltalk fills
it with its message sends (`Counter>>bump`, or `DoIt` for a top-level block),
Scheme with its procedure activations, and a line-loop language with the calls it
is inside: a `PROGRAM` frame at the line that called, then one per `GOSUB`, `CALL`,
`PROC`, `DO` group or PILOT unit. Logo and CHIP-8 keep no return stack, so they
report none.
`line` is the program's own source line where it has one — a BASIC program with
`20 LET Y = 2` on the second line reports 20 — so a client that echoes it back as a
breakpoint should convert it to the source-text position first. For languages
without line labels, the two are the same number.
`debug.continue` runs to the next breakpoint or the end; `debug.step` takes
`kind` (`into`, `over` or `out`, default `into`) — the line-loop languages use
one step unit, so all three stop at the next statement; `debug.stop` ends the
run; `debug.snapshot` reads the current state without moving it; `debug.close`
ends and releases the session.

`debug.rewind` takes `steps` (default 1) and goes back that many statements,
answering a state with `reason` `rewind`, the bindings the run had there and the
output it had printed by then. Asking for more than the session kept goes as far
back as there is rather than failing: "back 10" at the oldest statement is a
request to go back, and moving *forward* is the one answer it must never give. It
needs a session started with `history`: without one, or when the run is already at
the oldest statement it kept, the answer carries `"unmoved": true` and an `error`
saying so, rather than looking like a rewind that happened to land where the run
already was. What is *not* rewound is stated rather than implied — the canvas keeps the ink it was
given (the IDE's scrubber steps back through it), and an open file stays open at
its offset. `output` *is* rolled back with the state, so a client that keeps the
transcript it was given and the one it gets after a rewind agree.

Which languages can be walked back: the six line-by-line ones, Logo, whose executor
hands its position (`pc`, procedures, words, lists, pen, pose and seeded generator)
to the whole-program pause point with every statement, Forth, which hands over
its position in the token list, both stacks, the dictionary, the compile state and
the text printed so far (with the 64K data space only when a program has written to
it), and CHIP-8, which hands over the machine itself — registers, memory, screen,
timers and the frame count — before each instruction. Smalltalk pauses at its chunk
boundaries but has no checkpoint yet, so `history` keeps nothing for it and
`debug.rewind` answers `unmoved`. A Scheme program cannot be
rewound at all, because its state is an arena and a continuation chain rather than
a value: `Scheme` answers with an error and does not move. Each request blocks until the run pauses or
finishes, and a `state` of `timeout` means the engine was still running after
15 seconds (the engine's own 10-second budget usually ends it first).

Scheme runs through the same API and reports its call depth, procedure and
frame bindings. The `SQL` forms in a debugged program use the API's open
database, like `language.run`.

#### `watch.evaluate`

Evaluates debugger-style expressions over a set of bindings. Text variables are
visible by name and as `NAME$`; arithmetic is the same evaluator a program uses.

```json
{
  "expressions": ["X * 2", "NAME", "MISSING"],
  "variables": {"X": 5, "NAME": "Ada"}
}
```

```json
[
  {"expression":"X * 2","value":"10","ok":true,"error":null},
  {"expression":"NAME","value":"Ada","ok":true,"error":null},
  {"expression":"MISSING","value":"?","ok":false,"error":"Undefined variable: MISSING"}
]
```

#### `ai.ask`

Asks the configured assistant one question and returns its answer. The
configuration is the one Settings > Assistant holds — endpoint, model and key —
or the `TIME_WARP_AI_*` environment variables that override it, so a script
sets those rather than passing a key in a request line. Nothing here echoes the
key: every failure sentence goes through the same `redact()` the panel uses.

| Parameter | Type | |
| --- | --- | --- |
| `prompt` | string | the question; required, and not blank |
| `system` | string | replaces the Studio's own system prompt for this one call |
| `history` | array | earlier turns, each `{"role":"user"\|"assistant","content":"..."}`, oldest first |

```json
{"prompt":"why does this draw nothing?",
 "history":[{"role":"user","content":"PRINT 1"}]}
```

```json
{"text":"...","model":"deepseek-chat","promptTokens":412,"completionTokens":96}
```

The call blocks until the service answers, because the protocol is one request
line in and one response line out. An assistant that is off, has no model, or
cannot be reached is `-32000` with a sentence saying which of those it is, so a
script can print the reason rather than guess. `studio.info` reports
`features.assistant`, which is `false` in a build without Qt Network — the
method still exists there and answers with that build's reason.

### Errors

Standard JSON-RPC codes:

| Code | Meaning |
| --- | --- |
| `-32700` | the line is not JSON |
| `-32600` | not a JSON-RPC 2.0 request, or an invalid `id`/batch |
| `-32601` | unknown method |
| `-32602` | invalid parameters (missing, wrong type, array `params`) |
| `-32603` | an unexpected internal failure |
| `-32000` | an application failure: the engine, the database or the project said no |

`error.data`, when present, is the engine's own message (a SQLite constraint
error, a project file's parse error, and so on). `params` must be an object;
positional arrays are refused because every method's parameters are named.

---

## The C++ libraries

Configure and link the target rather than compiling the source files directly:

```cmake
add_subdirectory(native)
target_link_libraries(my_tool PRIVATE
    timewarp::lang      # Interpreter, executors, watch expressions, REPL session
    timewarp::scheme    # Scheme's explicit-stack machine
    timewarp::sql       # SQLite database
    timewarp::project   # .twproject model
    timewarp::ai        # the AI assistant: configuration, prompts, the client
    timewarp::api       # the JSON-RPC server, if embedding it
)
```

| Header | What it gives you |
| --- | --- |
| `timewarp/lang/interpreter.hpp` | `Interpreter`: load a program, run it, variables, telemetry, `SQL` statements, the debug session hook |
| `timewarp/lang/repl.hpp` | `ReplSession`: persistent bindings across submissions |
| `timewarp/lang/watch.hpp` | `evaluate_watch`/`evaluate_watches` over `lang::Value` bindings |
| `timewarp/lang/turtle_host.hpp` | the drawing interface every language talks to |
| `timewarp/graphics/recording_host.hpp` | a host that records the drawing and renders SVG; used by `twc` and the API |
| `timewarp/scheme/machine.hpp` | `scheme::Machine`, run-to-completion and resumable |
| `timewarp/sql/database.hpp` | `sql::Database`, results, tables, schema |
| `timewarp/project/project.hpp` | `project::Project`, `.twproject` load/save, groups |
| `timewarp/ai/config.hpp` | `ai::Config`, the presets, the settings file and the environment overrides |
| `timewarp/ai/prompt.hpp` | `ai::build_prompt` and the system prompt that tells a model what this IDE runs |
| `timewarp/ai/client.hpp` | `ai::Client`, one request in flight, over Qt Network |
| `timewarp/api/server.hpp` | `api::Server`, the JSON-RPC dispatcher with no transport |
| `timewarp/api/debug.hpp` | `debug::Session`, a breakpoint/step session for either engine |

`timewarp::lang` and `timewarp::scheme` depend on nothing but the C++ standard
library. `timewarp::sql`, `timewarp::project`, `timewarp::ai` and `timewarp::api`
use Qt Core's string, JSON and file types, `timewarp::ai` adds Qt Network for the
transport, and `timewarp::debugger` is the Qt threading adapter the IDE uses around
the Scheme machine. `timewarp::ai` is built only where Qt has Network: a client with
no transport is not a smaller assistant.

The assistant is also a command, for a script or a terminal: `./native/build/time_warp_ai`
takes `ask`, `explain`, `fix`, `comment`, `translate`, `write`, `check` and
`config`, reads the same settings the IDE writes, and prints the answer on standard
output alone. [AI.md](AI.md) is its manual.

---

## Compiling a program

`./twc` embeds a program in a generated launcher and links the interpreter; see
`./twc --help`. The result is a native executable that needs neither Python nor
the Studio. A drawing program writes an SVG (to `--svg FILE`, or `<program>.svg`
beside the executable), so a compiled program cannot exit silently with nothing
to show. A program that uses `SQL` gets a SQLite database from `--db FILE` at run
time (default `$TIME_WARP_SQL_DB`, then a scratch database), and its `SQL`
statements run exactly as they do in the IDE.

The IDE's `--compile` remains available and produces a copy of the IDE carrying
the program inside it; it needs Qt wherever it runs. Use `twc` when the artifact
must be small and self-contained, and `--compile` when it must be the full
authoring experience.
