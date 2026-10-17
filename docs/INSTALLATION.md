# Installing and Building Time Warp Studio

Time Warp Studio is a **C++20 / Qt6** desktop application. Everything you need to
run all ten languages is in this repository; nothing has to be downloaded at run
time.

---

## 1. Requirements

| Requirement | Minimum | Notes |
| --- | --- | --- |
| C++ compiler | C++20 | GCC 13+, Clang 16+, or MSVC 19.3+ |
| CMake | 3.20 | |
| Qt6 | 6.2 | Widgets, Core and Gui modules must be installed |
| Python | 3.10 | *Optional* — only for BASIC and Smalltalk |

Scheme and the shared expression engine are native C++ and need **no Python at
all**. FOCAL, PILOT, COMAL, CHIP-8, Logo, APL and REXX run in process through `timewarp_lang`;
the other four run through a small bundled engine process; see §6.

### Installing the toolchain

```bash
# Debian / Ubuntu
sudo apt install build-essential cmake qt6-base-dev

# Fedora
sudo dnf install gcc-c++ cmake qt6-qtbase-devel

# Arch
sudo pacman -S base-devel cmake qt6-base
```

On macOS, `brew install cmake qt` and pass `-DCMAKE_PREFIX_PATH=$(brew --prefix qt)`.

---

## 2. Build

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio

cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
```

A successful build prints nothing alarming; the first one takes a few minutes and
later ones are incremental. Useful options:

| Option | Effect |
| --- | --- |
| `-DCMAKE_BUILD_TYPE=Debug` | Unoptimised build with symbols |
| `-DTIME_WARP_EXPR_WERROR=OFF` | Build without `-Werror` |
| `-DCMAKE_BUILD_TYPE=RelWithDebInfo` | Optimised, with symbols |

---

## 3. Run

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

---

## 4. Verify the build

```bash
ctest --test-dir native/build --output-on-failure     # 13 suites
python3 native/tools/example_smoke.py                 # all 48 examples
```

Both must pass. See [README_TESTING.md](README_TESTING.md) for the full set,
including the sanitizer builds and the parity gates.

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

**A bridged language says the worker was not found.** The IDE looks for
`native/bridge/worker.py` beside the executable, two levels up, and in the working
directory, in that order. Set `TIME_WARP_WORKER` (full path to `worker.py`) or
`TIME_WARP_PYTHON` (interpreter) to override either half. The error message lists
every path it tried.

**A bridged language says no interpreter was found.** Install Python 3.10+ or
point `TIME_WARP_PYTHON` at one. Only the two bridged languages need it.

**`make test` runs the wrong Python.** The bridge's pytest suite comes from
`.venv` if it exists. Create one with `python3 -m venv .venv` and
`pip install pytest ruff`, or override with `make engine-test PYTHON=python3`.

---

## 6. The language bridge (already bundled)

Nothing to install. What the two unported languages need is:

- `native/bridge/worker.py` — the worker the IDE spawns
- `native/bridge/time_warp/` — the engine it drives

Optional extras are imported defensively and are **not** required: `pyttsx3`
(BASIC `SAY`), `pygame` and `inputs` (BASIC `SOUND` and `GAMEPAD`). Without them
those commands degrade rather than fail.

This engine is temporary. Each language ported to C++ removes its path through the
bridge; `docs/ROADMAP.md` tracks the order.

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

That installs four things, and they have to stay together:

| Path | What |
| --- | --- |
| `<prefix>/bin/time_warp_ide` | the application |
| `<prefix>/share/time_warp/` | the language bridge — `worker.py` and the engine |
| `<prefix>/share/applications/time-warp-studio.desktop` | the desktop entry |
| `<prefix>/share/icons/hicolor/256x256/apps/time-warp-studio.png` | the icon |

The bridge goes in `share/time_warp` because that is where the application looks
for it (`../share/time_warp` relative to the binary), so an installed build needs
no path configuration. It still needs `python3` it needs `python3` on `PATH` for the two bridged
languages.

### What is not packaged yet

Distro packages (`.deb`, `.rpm`, Flatpak or Snap), an AppImage and Windows/macOS
installers do not exist: the ones in this repository packaged the removed PySide6
application and were deleted with it. The install target above, plus the desktop
entry and icon, is the foundation for them. See [ROADMAP.md](ROADMAP.md).
