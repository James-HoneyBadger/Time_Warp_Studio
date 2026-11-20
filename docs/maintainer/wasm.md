# The WebAssembly build

The Studio also builds for the browser: the same Qt application, compiled to
WebAssembly, served as one page and one `.wasm` module. Every language runs, the
debugger pauses, the REPL answers and the 77 example programs travel inside the module.
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
| The target settings | `native/cmake/wasm.cmake` | Memory, output directory, tests off, Asyncify for browser suspension, and exception catching for interpreter errors |
| The page | `wasm/index.html` | Full-window screen, status bar, spinner, and the Qt 6.6 `qtLoad()` call |
| The isolation worker | `wasm/coi-serviceworker.js` | Adds the two headers a threaded module needs, on a host that cannot set them |
| The examples | `Examples/` | Copied into the module as a resource by `native/cmake/wasm.cmake` |
| The storage | `native/src/ui/wasm_storage.cpp` | Mounts IndexedDB over the home directory and flushes it at the three points that matter |
| The workflow | `.github/workflows/wasm-pages.yml` | Validates pull requests, installs the threaded Qt, builds and uploads an artifact; a separate tag job deploys it through GitHub Pages |

`wasm/index.html` and `wasm/coi-serviceworker.js` are copied beside the module by the
build, so `wasm/dist` is the site. <!-- doc-claims: ignore --> Qt's own generated `.html` is a debug wrapper with a
fixed-size screen and the Qt logo; the page here fills the window and reports a failure in
the status bar instead of the console.

The page uses `qtLoad()`, the loading API Qt 6.6 introduced. Qt 6.8 exports the module's
entry function as `window.time_warp_ide_entry`; the page passes that to the loader and
loads `time_warp_ide.js` separately, because since 6.6 the loader no longer evaluates
that file itself. The browser build therefore needs Qt 6.8: the loader API arrived in 6.6, and the
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

The browser target links with Emscripten Asyncify (`-sASYNCIFY -Os`). Qt's default
WebAssembly build cannot re-enter its event loop, which the Studio's file dialogs,
gallery and other synchronous Qt dialogs use. Asyncify lets those calls yield to browser
events without changing the desktop dialog paths. The Studio's C++ translation units
and final executable also use Emscripten exception catching (`-fexceptions`): the
interpreters catch program errors and report them as output rather than aborting the
module. This is Emscripten's JavaScript-based exception implementation; it does not turn
on Qt's separate WebAssembly-exception build feature.

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
The application waits for IndexedDB population before it creates the Projects directory
and restores the session. A save is only marked clean after the IndexedDB write succeeds;
on failure, the modified marker remains and both the application and page status report
that the copy is only in memory.
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

## Sharing a link and embedding

A browser page has no command line, so the query string is the startup argument a
shareable link carries. The window reads it at startup, the way it reads `[file...]` on
the desktop:

| Parameter | Meaning |
| --- | --- |
| `example=<path>` | Opens an example, relative to the gallery: `?example=logo/mandala.logo` |
| `lang=<key>&code=<base64>` | Opens a shared program. `lang` is a protocol key (`basic`, `logo`, `apl`, ...) and `code` is the source, base64 with the URL-safe alphabet and no padding |
| `notebook=<url>` | Opens a `.twnb` notebook fetched from `url`. The host must allow the page's origin (CORS), which is the browser's rule for any embedded fetch |
| `run=1` | Runs whatever the link opened, once the window is up |

`code` uses URL-safe base64 (`-` and `_` instead of `+` and `/`, padding dropped) so the
link survives a paste into an email or a message without escaping. The parser accepts the
standard alphabet too.

A link that names a bad example or an unknown language reports the reason on the console
and leaves the application open, the way a mistyped file argument does.

For a blog or a lesson, `?embed=1` drops the page's own chrome -- the status strip --
so the application sits in an `<iframe>` beside the prose. The strip reappears on error,
because an embedded program that fails silently is worse than one that was never
embedded:

```html
<iframe
  src="https://NAME.github.io/Time_Warp_Studio/?embed=1&lang=logo&code=…&run=1"
  width="640" height="480" title="A Logo program, running"
  style="border:0"></iframe>
```

The example, program and notebook links all combine with `embed`, so a lesson can point
an iframe at the exact cell it is explaining.

---

## What a page cannot offer

Four things in the desktop application have no browser equivalent, and each says so rather
than failing quietly:

- **The terminal.** `PtySession` reports that a browser has no shell to run, and the tab
  shows that reason. A serial port (`timewarp::io::SerialPort`) opens nothing.
- **The joystick.** `Gamepad::available()` is false, which is the answer every machine
  without one already gives: `STICK` reads a centred stick.
- **Sound out.** Tones are still synthesised and recorded, so **File > Export Audio**
  produces the same file, but there is no `QProcess` to hand a WAV to.
- **Printing.** Qt PrintSupport is not part of the WebAssembly package, so the two Print
  entries are absent, exactly as on a desktop build that lacks it.

zlib and SQLite are linked from Emscripten's ports (`-sUSE_ZLIB=1`, `-sUSE_SQLITE3=1`),
because a cross sysroot has no system copies for `find_package` to find; the classroom
bundle writer and the Database panel are therefore the same code as on the desktop.

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

`.github/workflows/wasm-pages.yml` builds inside `stateoftheartio/qt6:6.8-wasm-aqt`, which
carries emsdk and a host Qt at the version Qt for WebAssembly needs, and installs the
threaded WebAssembly Qt into it with aqt. Pull requests into `main` and branch pushes that
touch the native application, browser target, examples, version or workflow build and upload
`wasm/dist` <!-- doc-claims: ignore --> as an artifact. A separate job publishes it to the
GitHub Pages deployment only from a `v*` tag, keeping pull-request builds read-only.

The build uploads both a downloadable artifact and the Pages artifact. The tag job uses
the pinned `actions/deploy-pages` action with `pages: write` and `id-token: write`; the
repository's Pages source must be set to **GitHub Actions**. A `GITHUB_TOKEN` push to
`gh-pages` does not itself trigger a Pages deployment.
