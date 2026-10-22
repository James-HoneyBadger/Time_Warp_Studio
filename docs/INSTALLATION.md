# Installing and Building Time Warp Studio

Time Warp Studio is a **C++20 / Qt6** desktop application. Everything you need to
run all ten languages is in this repository; nothing has to be downloaded at run
time.

---

## 1. Requirements

| Requirement | Minimum | Notes |
| --- | --- | --- |
| C++ compiler | C++20 | GCC 11+, Clang 14+, or MSVC 19.30+ |
| CMake | 3.20 | |
| Qt6 | 6.2 | Widgets, Core and Gui modules must be installed; the Svg and PrintSupport modules are optional and enable SVG drawing export and printing |
| or Qt5 | 5.15 | Only for the Windows 7 build (`-DTIME_WARP_QT5=ON`); same modules, same role |
| SQLite3 | 3.35 | The database manager and the languages' `SQL` statement; the development package (`libsqlite3-dev`) is required to build |
| Python | 3.10 | *Optional* — only for the reference engine (parity gates, command matrix and lesson checks); running a language needs no Python |

All ten languages run in C++ inside the application, so running a program needs
**no Python at all**. The bundled Python engine in `native/bridge/` remains the
reference the parity gates and the command matrix compare against, and the engine a
lesson check runs in; see §6.

### Installing the toolchain

```bash
# Debian / Ubuntu
sudo apt install build-essential cmake qt6-base-dev libqt6svg6-dev libsqlite3-dev

# Fedora
sudo dnf install gcc-c++ cmake qt6-qtbase-devel qt6-qtsvg-devel sqlite-devel

# Arch
sudo pacman -S base-devel cmake qt6-base qt6-svg sqlite
```

On macOS, `brew install cmake qt` and pass `-DCMAKE_PREFIX_PATH=$(brew --prefix qt)`.
SQLite is part of the macOS SDK, so it needs no separate install. On macOS 26 and
later, use Qt 6.10 or newer: Qt 6.8 and 6.9 link `-framework AGL`, a framework the
newest SDK no longer ships, and the link fails on every executable. The released
disk image is built with Qt 6.12, which is universal and needs macOS 14.4.

On Windows, use the Qt online installer (or `aqtinstall`) for Qt 6 and either
`vcpkg install sqlite3:x64-windows-static-md` or a prebuilt SQLite to satisfy
SQLite3. The application builds with MSVC 2022 in a "x64 Native Tools" prompt:

```powershell
cmake -S native -B native/build -G "Visual Studio 17 2022" -A x64 `
  -DCMAKE_TOOLCHAIN_FILE="$env:VCPKG_INSTALLATION_ROOT/scripts/buildsystems/vcpkg.cmake" `
  -DVCPKG_TARGET_TRIPLET=x64-windows-static-md
cmake --build native/build --config Release --parallel
```

The Svg package above is optional. Without it the IDE builds and runs, and the
only difference is that drawing export offers PNG, JPEG and BMP but not SVG.

Printing needs Qt6's PrintSupport module, which ships with these Qt base
development packages already (`qt6-base-dev`, `qt6-qtbase-devel`, `qt6-base`), so
it adds nothing to the install line; the runtime library is `libqt6printsupport6`
on Debian, and part of `qt6-qtbase-gui` on Fedora. A build without it hides File
> Print Code and File > Print Drawing rather than showing dead entries.

---

## 2. Build

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
```

On macOS the same two commands produce `native/build/Time Warp Studio.app` -- a
bundle, because that is what the Finder runs and what the disk image ships. Run it
with `open "native/build/Time Warp Studio.app"`, or point at the executable inside
it (`.../Contents/MacOS/Time Warp Studio --help`). On Windows the executable is
`native\build\Release\time_warp_ide.exe`.

A successful build prints nothing alarming; the first one takes a few minutes and
later ones are incremental. Useful options:

| Option | Effect |
| --- | --- |
| `-DCMAKE_BUILD_TYPE=Debug` | Unoptimised build with symbols |
| `-DTIME_WARP_EXPR_WERROR=OFF` | Build without `-Werror` |
| `-DCMAKE_BUILD_TYPE=RelWithDebInfo` | Optimised, with symbols |

---

## 3. Run

The short way builds only if the binary is missing or out of date, then launches:

```bash
./run.sh                                          # empty workspace
./run.sh Examples/logo/mandala.logo
```

`run.sh` builds incrementally, then replaces itself with the IDE, so arguments and
the exit status are the application's. `TIME_WARP_BUILD_DIR` selects another build
tree (a relative path is resolved from the repository root), and
`TIME_WARP_SKIP_BUILD=1` launches an existing build without invoking CMake.

The binary directly:

```bash
./native/build/time_warp_ide                      # empty workspace
./native/build/time_warp_ide Examples/logo/mandala.logo
./native/build/time_warp_ide --run Examples/logo/mandala.logo
./native/build/time_warp_ide --help
```

Or through the Makefile, which also forwards the testing and sanitizer targets:

```bash
make -C native run
make help            # what the root Makefile offers
```

### The companion commands

All three are executable scripts at the repository root:

```bash
./twc Examples/basic/hello.bas -o hello   # small standalone executable
./twc Examples/basic/hello.bas -o hello --db school.db
./api                                     # JSON-RPC on stdin/stdout
```

`twc` links the language engine and, when the program uses it, SQLite; the result
runs without Qt. `api` builds and starts the automation service. `run.sh` also
forwards `--compile` to the IDE, which produces the larger artifact that carries
the IDE and opens in player mode. The full guide is [USER_GUIDE.md](USER_GUIDE.md).

---

## 4. Verify the build

```bash
ctest --test-dir native/build --output-on-failure     # 23 suites
python3 native/tools/example_smoke.py                 # all 60 examples
python3 native/tools/command_matrix.py                # every command-table row
python3 native/tools/repl_matrix.py                   # every row through the REPL
python3 native/tools/teach.py validate                # every lesson solution
```

All must pass. See [README_TESTING.md](README_TESTING.md) for the full set,
including the sanitizer builds and the parity gates.

With it built, meet the two machines the Studio carries: `Ctrl+Alt+W` for the
[HB-1](hb1/README.md) and `Ctrl+Shift+M` for the [CHIP-8](chip8/README.md), or
`./run.sh --computer` for the HB-1 from a terminal. [MACHINES.md](MACHINES.md)
explains why they are there.

---

## 5. Troubleshooting

**`Could not find a package configuration file provided by "Qt6"`.**
Qt is installed somewhere CMake does not look. Pass its prefix:
`cmake -S native -B native/build -DCMAKE_PREFIX_PATH=/path/to/qt6`.
If Qt is genuinely absent, the configure step says so and skips the widget UI
rather than failing obscurely — read its message.

**A stale build tree fails to regenerate**, usually with
`IMPORTED_LOCATION not set for Qt6::Gui configuration Debug`. Delete the tree
(`rm -rf native/build`) and configure again. Do **not** "fix" it with
`-DCMAKE_MAP_IMPORTED_CONFIG_DEBUG=Release`; that breaks `OpenGL::OpenGL` and
`OpenGL::GLX` in a way that is harder to diagnose.

**The Lessons panel says the curriculum was not found.** It is shipped data -
`share/time-warp-studio/lessons.json` beside the application - and
`TIME_WARP_LESSONS` overrides it. A checkout finds `native/share/lessons.json`
from the build tree. There is no worker and no Python involved: the check runs in
process.

**`make test` reports a missing `pytest`.** That is the reference engine's own
suite, which is development tooling and not part of the product. Create the
virtual environment with `python3 -m venv .venv` and `pip install pytest ruff`, or
skip it: `make test` is the C++ suite and needs no Python.

---

## 6. The reference engine (development only)

All ten languages run in C++ inside the IDE, and the lesson check does too. The
Python engine in `native/bridge/` is **not installed**: it is the fixture the
parity corpus was recorded from, and the harnesses (`example_smoke.py`,
`command_matrix.py`, `repl_matrix.py`) drive it while it exists. Nothing the
product runs reaches it.

Optional extras are imported defensively and are **not** required: `pyttsx3`
(the reference's BASIC `SAY` backend), `pygame` and `inputs` (BASIC `SOUND` and
`GAMEPAD`). Without them those commands degrade rather than fail. The native
engine reports `Speech synthesis not available` for `SAY`; `PLAY` and the sound
commands still produce their acknowledgement.

The engine is the part that goes away once lesson checking runs natively;
`docs/ROADMAP.md` tracks that.

---

## 7. Installing

A build can install itself, which is also how you check that the installed layout
finds the language bridge:

```bash
make -C native install                 # to /usr/local (may need sudo)
make -C native install PREFIX=$HOME/.local
make -C native uninstall               # removes what install wrote
```

`DESTDIR` is honoured for staging a package:

```bash
make -C native install DESTDIR=/tmp/stage PREFIX=/usr
```

That installs these, and they have to stay together:

| Path | What |
| --- | --- |
| `<prefix>/bin/time_warp_ide` | the application |
| `<prefix>/bin/time_warp_api` | the JSON-RPC automation surface |
| `<prefix>/bin/time_warp_teach` | validate the curriculum, and bundle or unpack a class |
| `<prefix>/bin/time_warp_learn` | the learner's command: list, show, start, check, hint |
| `<prefix>/share/time-warp-studio/lessons.json` | the curriculum the panel and the two commands read |
| `<prefix>/share/time-warp-studio/snippets.json` | the snippet catalogue Insert Snippet reads |
| `<prefix>/share/time-warp-studio/examples/` | the programs File > Open Example lists |
| `<prefix>/share/doc/time-warp-studio/` | the documentation Help > Documentation reads |
| `<prefix>/share/applications/time-warp-studio.desktop` | the desktop entry |
| `<prefix>/share/icons/hicolor/256x256/apps/time-warp-studio.png` | the icon |

The curriculum and the snippets go beside the application because that is where
it looks for them (`../share/time-warp-studio` relative to the binary), so an
installed build needs no path configuration. Nothing installed needs Python: the
reference engine stays in the checkout, as development tooling.

### Installing on Windows and macOS

`cmake --install` produces the package layout on every platform, and each one adds
the Qt runtime the application needs at run time; the Qt deployment tool runs as
part of the install, so an installed tree runs on a machine that has no Qt.

| Platform | `<prefix>` gets | Qt | To install |
| --- | --- | --- | --- |
| Linux | `bin/`, `share/` and the desktop entry, as in the table above | the distribution's | `cmake --install native/build --prefix /usr` |
| Windows | `bin\time_warp_ide.exe`, `share\…`, with `windeployqt`'s Qt DLLs and plugins beside it | copied in | `cmake --install native/build --config Release --prefix "C:/Program Files/Time Warp Studio"` |
| macOS | `Time Warp Studio.app`, with the bridge, examples, snippets, docs and `macdeployqt`'s frameworks inside `Contents` | copied in and ad-hoc signed | `cmake --install native/build --prefix /Applications` |

On macOS, the bundle layout is what the path lookup understands: the payload lives
in `Contents/Resources/share`, which is where the application looks for the bridge,
the examples, the snippets and the help pages when it is inside a bundle. Signing
is ad-hoc (`codesign --sign -`), which is enough for the application to start,
including on Apple silicon; it is not a Developer ID signature, so a copy that has
travelled over the network still needs the right-click > Open once.

The Windows installer and the macOS disk image are built from this same install
tree by CPack (`cpack -G NSIS` and `cpack -G DragNDrop`); see §8.

One feature differs by platform. `PLAY`, `SOUND` and the two machines' beeps write
a WAV and hand it to a command-line player: `afplay` on macOS, `paplay` or `aplay`
(or `ffplay`/`play`) on Linux. Windows has no equivalent command-line player, so
there those commands report that playback is unavailable rather than pretending to
make a sound. Everything else -- the ten languages, the debugger, the canvas, both
machines, the SQL tab and the automation surface -- is the same code on all three.

### Windows 7

Qt 6 requires Windows 10, so the Windows 7 artifact is the same application built
against **Qt 5.15** — the last Qt that runs on Windows 7 SP1 — from the same tree:

```bash
cmake -S native -B native/build -DTIME_WARP_QT5=ON -DTIME_WARP_WIN7=ON
cmake --build native/build --parallel
cpack --config native/build/CPackConfig.cmake -G NSIS
```

`-DTIME_WARP_QT5=ON` selects Qt 5 and `-DTIME_WARP_WIN7=ON` states the API level
(`WINVER` and `_WIN32_WINNT` at 6.01), so a call into anything newer than Windows 7
fails to compile here rather than failing to launch on the target machine. Both are
off by default; the release build is Qt 6.

What the pipeline checks is that this configuration builds, that all 23 suites pass
against Qt 5.15, and that the executable starts and renders a window -- on Windows
11, because that is what the runner is. What has **not** been verified is Windows 7
itself: no continuous-integration image is older than Windows Server 2019, and the
project has no Windows 7 machine to try it on. If it does not start there, the
likely causes are the Visual C++ runtime and a Qt 5.15 DLL that wants a newer API;
both are worth reporting rather than working around.

The version of Qt 5 the artifact is built with, and the `windeployqt` that deploys
it, come from `jurplel/install-qt-action`'s `win64_msvc2019_64` package of 5.15.2.

### 8. A release without building anything

Every release page carries a ready-built application, and none of it needs a
compiler on your machine. `uname -m` says which architecture is yours: `x86_64`
or `aarch64`. Each format spells it its own way — a `.deb` says `amd64`/`arm64`,
an `.rpm` says `x86_64`/`aarch64`, and the tarball and the AppImage say
`x86_64`/`arm64`.

| You have | Take | Then |
| --- | --- | --- |
| Windows 10 or 11 | `time-warp-studio-<version>-win64.exe` | Run it: it installs into Program Files, adds Start Menu and desktop shortcuts, and carries Qt and the SQLite library inside it. `time-warp-studio-<version>-win64.zip` is the same tree without the installer. |
| Windows 7 | `time-warp-studio-<version>-win64-win7.exe` | The same application built against Qt 5.15, the last Qt that runs on Windows 7 SP1 (Qt 6 requires Windows 10). It is built and tested on Windows 11 — see [the Windows 7 section](#windows-7) — and `time-warp-studio-<version>-win64-win7.zip` is the same tree without the installer. |
| macOS 14.4 or later, Intel or Apple silicon | `Time_Warp_Studio-<version>-macOS-universal.dmg` | Open it, drag **Time Warp Studio** to Applications. The image is universal, so one file covers both architectures. |
| a Linux desktop and no wish to install anything | `Time_Warp_Studio-<version>-x86_64.AppImage` | `chmod +x` it and double-click. It needs no root, no package manager and no Qt: everything is inside the file. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | Double-click it, or `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-x86_64.tar.gz`, or `-arm64` | `tar -xzf … -C / --strip-components=1` |

The **Windows installer** is a NSIS package: it puts the application and its Qt
runtime in `Program Files\Time Warp Studio`, with `bin\time_warp_ide.exe` and
`share\` beside it — the same layout the other packages use — and it uninstalls
from Settings > Apps. Neither it nor the disk image is signed with a paid
certificate, so SmartScreen may ask the first time ("More info", then "Run
anyway") and Gatekeeper will ask once on macOS (right-click the application and
choose Open).

The **AppImage is the answer for a non-technical Linux user**: one file, double-clicked,
nothing installed. It is built for glibc 2.39 or newer (Ubuntu 24.04 and later, or
a distribution of similar age); on anything older use the package for that
distribution, which resolves the libraries properly.

The **packages** put the binary, the bridge, the desktop entry and the icon where
the system expects them, so the application appears in the menu and can be removed
with the package manager. They declare `python3`, the Qt 6 runtime and SQLite as
dependencies, and they carry the example programs — **File > Open Example** lists
them, so a new install has something to run.

The **tarball** is the install tree above, plus the licence and the two READMEs.
It needs Qt 6, SQLite, a C++ runtime and `python3` for the lesson check — the
same requirements as a local build, minus the compiler.

The container image is `ghcr.io/james-honeybadger/time-warp-studio:<version>`
(or `:latest`), and it carries everything, including Qt and Python. It is named
after the product, as every other artifact here is, and the release tags the same
build as `time_warp_studio` too -- the repository's name, lowercased -- so a note
from an older release, or a script copied out of one, still pulls something
current rather than whatever was last published under that name.

So, on a machine with Docker:

```bash
# A window on your desktop
docker run --rm -e DISPLAY \
  -v /tmp/.X11-unix:/tmp/.X11-unix \
  -v "$HOME/.time_warp:/home/studio/.time_warp" \
  ghcr.io/james-honeybadger/time-warp-studio:latest

# Headless: render a drawing from a program, no display at all
docker run --rm -e QT_QPA_PLATFORM=offscreen \
  -v "$PWD:/work" \
  ghcr.io/james-honeybadger/time-warp-studio:latest \
  --run --export /work/out.png /work/Examples/logo/mandala.logo
```

Each tag is a manifest list for **linux/amd64 and linux/arm64**, so an ARM laptop
or a Raspberry Pi pulls its own architecture and runs it rather than emulating
every instruction; `docker run` picks the right one, and `--platform` is only
needed to ask for the other. The bind mount for `~/.time_warp` is what keeps
lesson progress between runs; the container runs as a non-root `studio` user with
uid 1000, so a directory your own desktop user owns is writable to it and its home
directory is the only place outside that mount it writes. Sound works if the host's
device is passed through (`--device /dev/snd`), because `SOUND` and `PLAY` hand
their WAV to `aplay`.

The install target above, plus the desktop entry and icon, is the foundation for
distro packaging. See [ROADMAP.md](ROADMAP.md).
