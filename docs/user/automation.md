# Automation

Two surfaces let something other than a person drive the Studio:

- the **automation API**, JSON-RPC 2.0 over newline-delimited JSON, for a script, a test
  or an editor plugin. `./api` starts it from a checkout; an installed tree has
  `time_warp_api` on `PATH`;
- the **C++ libraries**, the same engines the application uses, for a program that links
  them.

Both are local. The automation API has no socket, no port and no network listener: a
caller that can write to its stdin can already run programs on the machine, so there is no
authentication layer pretending otherwise.

---

## The protocol

One request per line, one response per line, in request order. `id` may be a string, a
number or null; a request without one is a notification and gets no response. Batches are
arrays of requests, and notifications inside a batch are omitted from the response array. A
line that is not JSON gets a `-32700` response.

```bash
printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"language.run","params":{"language":"basic","source":"10 PRINT 2*21"}}' \
  | ./api
```

`./api` builds `time_warp_api` if it is missing; `TIME_WARP_SKIP_BUILD=1` skips that. The
same server is the `api::Server` library over an `istream` and an `ostream`, which is how
the tests drive it without a process.

| Family | Methods |
| --- | --- |
| Studio | `studio.info`, `ping`, `studio.shutdown` |
| Running | `language.run` |
| REPL | `repl.create`, `repl.eval`, `repl.reset`, `repl.close` |
| Debugging | `debug.start`, `debug.snapshot`, `debug.continue`, `debug.step`, `debug.rewind`, `debug.stop`, `debug.close` |
| SQL | `sql.open`, `sql.close`, `sql.query`, `sql.tables`, `sql.schema` |
| Projects | `project.create`, `project.open`, `project.save`, `project.info`, `project.addFile`, `project.addFiles`, `project.addFolder`, `project.removeFile`, `project.addGroup`, `project.renameGroup`, `project.removeGroup`, `project.moveFile`, `project.setEntry`, `project.clearEntry` |
| Expressions | `watch.evaluate` |

---

## Running a program

`studio.info` takes nothing and reports the API version, the languages this build carries,
and `features`. `ping` answers; `studio.shutdown` exits the service.

`language.run`:

| Parameter | Type | |
| --- | --- | --- |
| `language` | string | `basic`, `apl`, `logo`, `pilot`, `focal`, `scheme`, `comal`, `rexx`, `smalltalk`, `forth` |
| `source` | string | the program text; `sourceBase64` carries one that is not valid UTF-8 |
| `inputs` | array of strings | answers to `ASK` and `INPUT` prompts, in order; an exhausted list answers empty |

```json
{
  "output": "42\n",
  "lines": ["42"],
  "error": "",
  "variables": [{"name":"X","type":"number","value":"42"}],
  "drawing": {"commands": 4, "actions": 6, "svg": "<svg …>"},
  "telemetry": {"steps": 3, "ms": 0.2, "timedOut": false, "iterationCapped": false}
}
```

`drawing.commands` counts ink — strokes and shapes. `drawing.actions` counts every turtle
operation, including turns and pen changes that leave no mark, so a caller can tell
"accepted" from "silently ignored". `drawing.svg` is empty when the program drew nothing.
For Scheme, `variables` holds the machine's visible bindings and `telemetry` is empty.

A run that ends by hitting the budget reports `timedOut` or `iterationCapped` rather than
looking finished; the budget is two million statements or ten seconds, as in the IDE.

### REPL sessions

A session keeps its bindings between calls, exactly like the REPL bar.

```json
{"jsonrpc":"2.0","id":2,"method":"repl.create","params":{"language":"basic"}}
{"jsonrpc":"2.0","id":3,"method":"repl.eval","params":{"session":"1","source":"LET X = 5"}}
{"jsonrpc":"2.0","id":4,"method":"repl.eval","params":{"session":"1","source":"PRINT X * 3"}}
```

The third call answers `{"output":"15\n","lines":["15"],"error":""}`. Scheme echoes the
value of an expression — `(+ 20 22)` answers `42` — and `define` and `set!` echo nothing.
In a BASIC session `NEW` clears the bindings; `repl.reset` does the same without needing a
BASIC statement, and `repl.close` releases the session. Sessions are per process.

---

## Debugging a program

`debug.start` takes the same `language` and `source` as `language.run`, plus `breakpoints`
(1-based positions in the source text, as the editor gutter numbers them),
`dataBreakpoints` (variable names) and `history` (how many snapshots to keep, 0 to 10000;
0 keeps none and costs nothing). It answers when the run pauses or finishes:

```json
{
  "session": "1", "state": "paused", "paused": true, "finished": false,
  "line": 20, "depth": 0, "procedure": "", "reason": "breakpoint",
  "changedVariable": "", "error": "", "stack": [],
  "variables": [{"name":"X","type":"number","value":"1"}],
  "output": "", "drawing": {"commands": 0, "actions": 0, "svg": ""}
}
```

`reason` is `breakpoint`, `step`, `rewind`, `data`, `pause`, `finished` or `error`.

`line` is the program's **own** source line where the language has one, so a BASIC program
with `20 LET Y = 2` on its second line reports 20. A client that echoes it back as a
breakpoint converts it to a source-text position first. For languages without line labels
the two numbers are the same.

`stack` is the paused run's call chain, outermost first, one `{"procedure": "…", "line":
N}` per frame: the frame's own line while it runs, its call site while it waits. It is
empty while the run stands in the program's own body, because nothing is waiting. Smalltalk
fills it with message sends, Scheme with procedure activations, and a line-loop language
with the calls it is inside. Logo and Forth keep no return stack and report none.

`debug.continue` runs to the next breakpoint or the end. `debug.step` takes `kind` —
`into`, `over` or `out`, default `into`; the line-loop languages have one step unit, so all
three stop at the next statement. `debug.snapshot` reads the state without moving it,
`debug.stop` ends the run, and `debug.close` ends and releases the session. Each request
blocks until the run pauses or finishes; a `state` of `timeout` means the engine was still
running after 15 seconds, which the engine's own budget usually prevents.

### Walking back

`debug.rewind` takes `steps` (default 1) and goes back that many statements, answering a
state with `reason` `rewind`, the bindings the run had there, and the output it had printed
by then. Asking for more than the session kept goes as far back as there is rather than
failing, because moving forward is the one answer it must never give. It needs a session
started with `history`; without one, or at the oldest statement kept, the answer carries
`"unmoved": true` and an error saying so rather than looking like a rewind that happened to
land where the run already was.

What is not rewound is stated rather than implied: the canvas keeps the ink it was given,
and an open file stays open at its offset. `output` *is* rolled back, so a client that keeps
the transcript it was given and the one it gets after a rewind agree.

| Language | Can be walked back |
| --- | --- |
| The six line-by-line languages | Yes |
| Logo | Yes: its executor hands over `pc`, procedures, words, lists, pen, pose and the seeded generator at every pause point |
| Forth | Yes: position in the token list, both stacks, the dictionary, the compile state and the text printed so far |
| Smalltalk | No checkpoint yet: `history` keeps nothing and `debug.rewind` answers `unmoved` |
| Scheme | No: its state is an arena and a continuation chain rather than a value, so the method answers with an error and does not move |

A `SQL` statement inside a debugged program uses the API's open database, exactly as it
does under `language.run`.

---

## The database

`sql.open` takes `path` — `:memory:` for a scratch database — creates the file when it
does not exist, and answers the path and the table list. The connection is the one a run
uses, so a database opened here is visible to a `SQL` statement in `language.run`.

`sql.query` takes `sql`, one statement or a script:

```json
{"columns": ["name", "score"], "rows": [["Ada", "42"]],
 "changes": 1, "truncated": false, "error": null}
```

At most 1000 rows come back, and `truncated` says when there were more. `sql.tables`
returns names; `sql.schema` takes `table` and returns its `CREATE` statement.

---

## Projects

`project.create` takes `name`, `root` and optional `filePath`, writes the `.twproject` and
answers the project. `project.open` takes `path`, `project.save` writes the open project,
and `project.info` answers:

```json
{
  "name": "Class", "root": "/home/learner/class",
  "filePath": "/home/learner/class/Class.twproject",
  "entry": "/home/learner/class/flower.logo",
  "groups": ["Art"],
  "files": [{"path":"/home/learner/class/flower.logo","group":"Art","exists":true}],
  "missing": []
}
```

`entry` is the program **Run Project** would run, as an absolute path, or null. It is the
one place a project states which of its programs is *the* program.

`addFile` takes `path` and optional `group`; `addFiles` takes `paths` and an optional
`group`; `removeFile` takes `path` and edits the project, never the file.
`project.addFolder` takes `path`, `extensions` (with or without the leading dot) and an
optional `group`: it walks the folder, adds every matching program that is not already a
member, and names each group after the subfolder it was found in, prefixed by `group` when
one is given. It answers the project plus `added`.

`addGroup` and `removeGroup` take `name`, `renameGroup` takes `from` and `to`, and
`moveFile` takes `path` and `group` (empty to ungroup). Removing a group returns its files
to the ungrouped section rather than deleting them. `setEntry` takes `path` and adds the
file when it is not already a member; `clearEntry` takes nothing and fails when there was
no entry.

Every project method answers with the whole project, so a caller never merges a delta into
a copy it thinks it has.

---

## Expressions

`watch.evaluate` evaluates debugger-style expressions over supplied bindings. Text
variables are visible by name and as `NAME$`, and arithmetic is the same evaluator a
program uses.

```json
{"expressions": ["X * 2", "NAME", "MISSING"], "variables": {"X": 5, "NAME": "Ada"}}
```

```json
[{"expression":"X * 2","value":"10","ok":true,"error":null},
 {"expression":"NAME","value":"Ada","ok":true,"error":null},
 {"expression":"MISSING","value":"?","ok":false,"error":"Undefined variable: MISSING"}]
```

---

## Errors

| Code | Meaning |
| --- | --- |
| `-32700` | the line is not JSON |
| `-32600` | not a JSON-RPC 2.0 request, or an invalid `id` or batch |
| `-32601` | unknown method |
| `-32602` | invalid parameters: missing, wrong type, or an array where an object is required |
| `-32603` | an unexpected internal failure |
| `-32000` | an application failure: the engine, the database or the project said no |

`error.data`, when present, is the engine's own message — a SQLite constraint error, a
project file's parse error, and so on. `params` must be an object; positional arrays are
refused because every method's parameters are named.

---

## The C++ libraries

Link the target rather than compiling the sources directly:

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
| `timewarp/lang/interpreter.hpp` | `Interpreter`: load a program, run it, variables, telemetry, `SQL` statements, the debug hook |
| `timewarp/lang/repl.hpp` | `ReplSession`: bindings that persist across submissions |
| `timewarp/lang/watch.hpp` | `evaluate_watch` and `evaluate_watches` over `lang::Value` bindings |
| `timewarp/lang/turtle_host.hpp` | the drawing interface every language talks to |
| `timewarp/graphics/recording_host.hpp` | a host that records the drawing and renders SVG; used by `twc` and the API |
| `timewarp/scheme/machine.hpp` | `scheme::Machine`, run-to-completion and resumable |
| `timewarp/sql/database.hpp` | `sql::Database`, results, tables, schema |
| `timewarp/project/project.hpp` | `project::Project`, `.twproject` load and save, groups |
| `timewarp/api/server.hpp` | `api::Server`, the dispatcher with no transport |
| `timewarp/api/debug.hpp` | `debug::Session`, a breakpoint and step session for either engine |

`timewarp::lang` and `timewarp::scheme` depend on nothing but the C++ standard library.
`timewarp::sql`, `timewarp::project` and `timewarp::api` use Qt Core's string, JSON and
file types.

---

## Compiling a program

`./twc` embeds a program in a generated launcher and links the interpreter; `./twc --help`
lists the options. The result is a native executable that needs neither an interpreter nor
the Studio. A drawing program writes an SVG — to `--svg FILE`, or `<program>.svg` beside
the executable — so a compiled program cannot exit silently with nothing to show. A program
that uses `SQL` takes `--db FILE` at run time (default `$TIME_WARP_SQL_DB`, then a scratch
database).

The IDE's `--compile` is the other artifact: a copy of the player with the program inside
it, which needs Qt wherever it runs and opens one program with a canvas, its output, `R` to
run again and `Esc` to leave. Use `twc` when the artifact must be small and self-contained,
and `--compile` when it must open as a program with a canvas.
