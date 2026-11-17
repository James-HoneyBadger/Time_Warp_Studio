# The WebAssembly build

The Studio also builds for the browser: the same Qt application, compiled to
WebAssembly, served as one page and one `.wasm` module. Every language runs, the
debugger pauses, the REPL answers and the 68 example programs travel inside the module.
The project volume survives a reload because it is written to IndexedDB.

This is a build target, not a second application. No interpreter knows about the browser,
no behaviour is conditioned on it, and the desktop build is untouched: the option
defaults to `OFF`, and both modules that implement the target return immediately when it
is off.

---

## What the target consists of

| Part | Where | What it does |
| --- | --- | --- |
| The option | `native/CMakeLists.txt` | `TW_BUILD_WASM`, default `OFF` |
| The toolchain search | `native/cmake/wasm-toolchain.cmake` | Chosen before `project()`, because a toolchain file has to be named before a language is enabled |
| The target settings | `native/cmake/wasm.cmake` | Memory, output directory, tests off, and the one function the executable calls |
| The page | `wasm/index.html` | Full-window screen, status bar, spinner, and the Qt 6.6 `qtLoad()` call |
| The isolation worker | `wasm/coi-serviceworker.js` | Adds the two headers a threaded module needs, on a host that cannot set them |
| The examples | `Examples/` | Copied into the module as a resource by `native/cmake/wasm.cmake` |
| The storage | `native/src/ui/wasm_storage.cpp` | Mounts IndexedDB over the home directory and flushes it at the three points that matter |
| The workflow | `.github/workflows/wasm.yml` | Installs the threaded Qt, builds, uploads an artifact, publishes a tag to `gh-pages` |

`wasm/index.html` and `wasm/coi-serviceworker.js` are copied beside the module by the
build, so `wasm/dist` is the site. <!-- doc-claims: ignore --> Qt's own generated `.html` is a debug wrapper with a
fixed-size screen and the Qt logo; the page here fills the window and reports a failure in
the status bar instead of the console.

The page uses `qtLoad()`, the loading API Qt 6.6 introduced. It needs the module's own
entry function -- `window.time_warp_ide`, named after the target -- and a `<script>` tag
for `time_warp_ide.js`, because since 6.6 the loader no longer evaluates that file
itself. The browser build therefore needs Qt 6.8: the loader API arrived in 6.6, and the
`WebLocalStorageFormat` the settings use arrived in 6.8, which is what the workflow
installs.

---

## Building

A WebAssembly build needs three things that must agree on a version: emsdk, a host Qt
that supplies `moc` and `rcc`, and a WebAssembly Qt. The Qt package must be the threaded
one (`wasm_multithread`), for the reason in [Threads and isolation](#threads-and-isolation);
the single-threaded package configures, builds and links, so the difference does not
appear until someone presses Run. The toolchain file belongs to the *WebAssembly* Qt; it
finds the host Qt through `QT_HOST_PATH`, and this tree derives that path from the wasm
install when it sits beside a `gcc_64` directory, which is the layout the Qt container
uses:

```bash
cmake -S native -B native/build-wasm \
  -DCMAKE_BUILD_TYPE=Release \
  -DTW_BUILD_WASM=ON \
  -DCMAKE_TOOLCHAIN_FILE=/opt/Qt/6.8.1/wasm_multithread/lib/cmake/Qt6/qt.toolchain.cmake
cmake --build native/build-wasm --parallel
```

`QT_WASM` points the build at the WebAssembly install and needs no other argument, so a
shell inside the Qt container is the shortest route:

```bash
QT_WASM=/opt/Qt/6.8.1/wasm_multithread \
  cmake -S native -B native/build-wasm -DCMAKE_BUILD_TYPE=Release -DTW_BUILD_WASM=ON
```

`QT_WASM_TOOLCHAIN` names the file itself for a layout that is not the container's.

A configure with no toolchain file stops with a message naming both routes rather than
failing later on a missing header.

The build writes the deployable files to `wasm/dist`: `time_warp_ide.js`, <!-- doc-claims: ignore -->
`time_warp_ide.wasm`, `qtloader.js`, copies of `wasm/index.html` and
`wasm/coi-serviceworker.js`, and the `.nojekyll` GitHub Pages needs. Serve that directory
over HTTP -- not `file://`, which cannot load a module -- and open `index.html`.

```bash
python3 -m http.server 8000 --directory wasm/dist
```

The executable is the desktop `time_warp_ide`: there is no separate launcher and no
second entry point.

---

## Storage

The Emscripten filesystem is memory. A document written under `/home/web_user`
survives a repaint and is gone on reload unless `FS.syncfs` has written it to IndexedDB.
Nothing in Qt calls `syncfs` -- the browser has no `fsync` for it to translate -- so the
application calls it in the three places a user expects:

- **On Save**, immediately. In the browser "saved" means IndexedDB has it, not that the
  in-memory write returned.
- **Every thirty seconds**, so a tab closed by a crash loses at most half a minute.
- **On `beforeunload`**, registered by the same code that mounts the volume, so a page
  that forgot the hook and a page whose mount failed are the same bug.

The mounted volume is the home directory Emscripten gives the process, and the Projects
dialogs open on `/home/web_user/projects` inside it. Mounting the home rather than that
one folder is what keeps the IDE's own data -- lesson progress, the classroom roster and
the autosave snapshots, all under `~/.time_warp` -- on the same footing as the documents.
The settings are separate: they are kept in `localStorage` through Qt's
`WebLocalStorageFormat`, because the INI file they would otherwise use is read before an
asynchronous IndexedDB load could have restored it. A desktop build compiles every one of
these calls to a no-op, so the save path has no `#ifdef` in it.

---

## Examples

`Examples/` is compiled into the module as a Qt resource at `:/examples`, because the page
has no checkout or install directory beside it: a gallery that looked on a disk would
show nothing, and the browser build would open on an empty editor. `examplesDir()` in
`native/src/ui/app_paths.cpp` returns the resource path when it exists and searches the
filesystem otherwise, so the desktop build is unchanged. The list is a glob at configure
time -- the corpus is data, and a new program should not need a second edit to be
listed -- so a fresh configure picks up a new file.

---

## Threads and isolation

The module is built from `wasm_multithread`, and that is not an optimisation: every
runner in the IDE is a `QThread`. `NativeRunner` runs a program off the GUI thread so the
canvas can be drained while it draws, `ReplRunner` keeps a session alive between commands,
and `DebuggerRunner` steps the Scheme machine. Qt built without thread support compiles
`QThread::start()` to an empty function, so a single-threaded module loads, edits and
paints, and does nothing at all when Run is pressed.

Threads need `SharedArrayBuffer`, and a browser exposes that only to a cross-origin
isolated page: the document has to arrive with

```text
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

A server you control can send those. GitHub Pages cannot, which is what
`wasm/coi-serviceworker.js` is for: the page registers it, it adds both headers to every
response, and the page reloads once to be served through it. `require-corp` also means
every subresource must be same-origin or send `Cross-Origin-Resource-Policy`; on the
deployed site every file beside the worker is same-origin.

The worker's scope is its own directory, so the site works from a project path such as
`https://name.github.io/Time_Warp_Studio/` as well as from a domain root.

`QT_WASM_PTHREAD_POOL_SIZE` is four: enough for the runner, the REPL and the sound
thread at once, and every worker thread is memory the browser has to start.

---

## Troubleshooting

### The page loads and the canvas stays black

Served over `file://`. A WebAssembly module needs an `http://` origin; use the static
server line above.

### "qtloader.js did not provide the Qt 6.6 loader"

The page needs `qtLoad()`, which Qt 6.6 introduced, and the entry function the module
exports (`window.time_warp_ide`). An older Qt emits a loader with a different API, and the
page says which file was missing rather than failing silently. Build with Qt 6.6 or later.

### "The application needs cross-origin isolation"

The page was served without COOP and COEP, and the service worker that would add them
did not take effect. Served over `file://`, a service worker cannot register at all; use
the static server line above. On a host you control, send the two headers yourself. A
browser that blocks service workers, such as some private modes, cannot run the
application.

### The module runs out of memory

The ceiling is `QT_WASM_MAXIMUM_MEMORY`, 1 GB by default, and the initial reservation is
`QT_WASM_INITIAL_MEMORY`, 256 MB. Exceeding the ceiling aborts the module and the browser
console says so. Raise the initial figure only with a reason: Emscripten fixes the heap at
that size, so a larger figure is memory every visitor pays for whether the program uses it
or not.

```bash
cmake -S native -B native/build-wasm -DTW_BUILD_WASM=ON \
  -DQT_WASM_INITIAL_MEMORY=536870912 -DQT_WASM_MAXIMUM_MEMORY=2147483648
```

### The module is served as the wrong type

A server that does not know `.wasm` sends `application/octet-stream`, and the browser
refuses to instantiate the module. The three types that matter:

| Extension | Type |
| --- | --- |
| `.wasm` | `application/wasm` |
| `.js` | `text/javascript` |
| `.html` | `text/html` |

GitHub Pages serves all three correctly. A self-hosted server usually needs one line per
type in its configuration; nginx carries `application/wasm` in its default `mime.types`
from 1.21 onwards.

### The status bar shows an error and the console has more

The bar is one line. Every failure is also written to the browser console with the
`Time Warp Studio:` prefix, and an Emscripten abort prints the reason there.

---

## The workflow

`.github/workflows/wasm.yml` builds inside `stateoftheartio/qt6:6.8-wasm-aqt`, which
carries emsdk and a host Qt at the version Qt for WebAssembly needs, and installs the
threaded WebAssembly Qt into it with aqt. It uploads `wasm/dist` as an artifact on every <!-- doc-claims: ignore -->
run that touches `native/` or `wasm/`, and publishes it to the `gh-pages` branch only from
a `v*` tag: an ordinary commit cannot change what a visitor downloads.

The publish is four `git` commands against a temporary repository, using `GITHUB_TOKEN`.
That is deliberate: every `uses:` in this tree is pinned to a commit SHA, and a pages
action would be one more pin to review for a step that needs no third party.
