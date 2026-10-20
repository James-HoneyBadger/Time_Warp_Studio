# Time Warp Studio API

The Studio exposes two API surfaces:

- **The automation API** — JSON-RPC 2.0 over newline-delimited JSON, for a
  script, a test or an editor plugin. `./api` starts it from a source checkout;
  an installed tree has `time_warp_api` on `PATH`.
- **The C++ libraries** — the same engines the application uses, linkable by
  another program. `timewarp::lang`, `timewarp::scheme`, `timewarp::sql`,
  `timewarp::project` and `timewarp::api`.

The [user guide](USER_GUIDE.md#7-automation) has a worked introduction; this page is
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
  "version": "16.1.0",
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
and `define`/`set!` echo nothing. `repl.reset` forgets every binding;
`repl.close` releases the session. Sessions are per-process.

#### `sql.open`, `sql.close`, `sql.query`, `sql.tables`, `sql.schema`

`sql.open` takes `path` (`:memory:` for a scratch database) and creates the file
when it does not exist. It returns the path and the table list:

```json
{"path": ":memory:", "tables": []}
``` The connection is the one a
run uses, so a database opened here is visible to a `SQL` statement in a program
run through `language.run`.

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
  "groups": ["Art"],
  "files": [{"path":"/home/learner/class/flower.logo",
             "group":"Art","exists":true}],
  "missing": []
}
```

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

Every project method returns the whole project (or, for `addFolder`, the project
plus `added`), so a caller never has to merge a delta into a copy it thinks it
has.

#### `debug.start`, `debug.snapshot`, `debug.continue`, `debug.step`, `debug.stop`, `debug.close`

A debug session runs a program until its first pause and then answers one
request per state. `debug.start` takes the same `language`, `source`/`sourceBase64`
as `language.run`, plus optional `breakpoints` (1-based positions in the source
text, as the editor gutter numbers them) and `dataBreakpoints` (variable names). It returns when the run pauses or finishes:

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
  "variables": [{"name":"X","type":"number","value":"1"}],
  "output": "",
  "drawing": {"commands": 0, "actions": 0, "svg": ""}
}
```

`reason` is `breakpoint`, `step`, `data`, `pause`, `finished` or `error`.
`line` is the program's own source line where it has one — a BASIC program with
`20 LET Y = 2` on the second line reports 20 — so a client that echoes it back as a
breakpoint should convert it to the source-text position first. For languages
without line labels, the two are the same number.
`debug.continue` runs to the next breakpoint or the end; `debug.step` takes
`kind` (`into`, `over` or `out`, default `into`) — the line-loop languages use
one step unit, so all three stop at the next statement; `debug.stop` ends the
run; `debug.snapshot` reads the current state without moving it; `debug.close`
ends and releases the session. Each request blocks until the run pauses or
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
| `timewarp/api/server.hpp` | `api::Server`, the JSON-RPC dispatcher with no transport |
| `timewarp/api/debug.hpp` | `debug::Session`, a breakpoint/step session for either engine |

`timewarp::lang` and `timewarp::scheme` depend on nothing but the C++ standard
library. `timewarp::sql`, `timewarp::project` and `timewarp::api` use Qt Core's
string, JSON and file types, and `timewarp::debugger` is the Qt threading adapter
the IDE uses around the Scheme machine.

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
