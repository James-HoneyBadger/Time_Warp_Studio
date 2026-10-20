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
| Python | 3.10 | *Optional* — only for the reference engine (parity gates, command matrix and lesson checks); running a language needs no Python |

All ten languages run in C++ inside the application, so running a program needs
**no Python at all**. The bundled Python engine in `native/bridge/` remains the
reference the parity gates and the command matrix compare against, and the engine a
lesson check runs in; see §6.

### Installing the toolchain

```bash
# Debian / Ubuntu
sudo apt install build-essential cmake qt6-base-dev libqt6svg6-dev

# Fedora
sudo dnf install gcc-c++ cmake qt6-qtbase-devel qt6-qtsvg-devel

# Arch
sudo pacman -S base-devel cmake qt6-base qt6-svg
```

On macOS, `brew install cmake qt` and pass `-DCMAKE_PREFIX_PATH=$(brew --prefix qt)`.

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
ctest --test-dir native/build --output-on-failure     # 17 suites
python3 native/tools/example_smoke.py                 # all 50 examples
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

**The Lessons panel says the worker was not found.** Checking a lesson still runs
in the reference engine. The IDE looks for `native/bridge/worker.py` beside the
executable, two levels up, and in the working directory, in that order. Set
`TIME_WARP_WORKER` (full path to `worker.py`) or `TIME_WARP_PYTHON`
(interpreter) to override either half. The error message lists every path it
tried.

**The Lessons panel says no interpreter was found.** Install Python 3.10+ or
point `TIME_WARP_PYTHON` at one. Running a program needs no Python at all; only
the lesson check does.

**`make test` runs the wrong Python.** The bridge's pytest suite comes from
`.venv` if it exists. Create one with `python3 -m venv .venv` and
`pip install pytest ruff`, or override with `make engine-test PYTHON=python3`.

---

## 6. The reference engine (already bundled)

All ten languages run in C++ inside the IDE. The Python engine is still shipped
because it is the reference the parity gates compare against and the engine the
Lessons panel checks a program with:

- `native/bridge/worker.py` — the worker the IDE spawns for a lesson check
- `native/bridge/time_warp/` — the engine it drives

Optional extras are imported defensively and are **not** required: `pyttsx3`
(the reference's BASIC `SAY` backend), `pygame` and `inputs` (BASIC `SOUND` and
`GAMEPAD`). Without them those commands degrade rather than fail. The native
engine acknowledges `SAY` without producing audio, as it does `PLAY`.

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

That installs seven things, and they have to stay together:

| Path | What |
| --- | --- |
| `<prefix>/bin/time_warp_ide` | the application |
| `<prefix>/share/time_warp/` | the reference engine — `worker.py` and the Python package |
| `<prefix>/share/time-warp-studio/snippets.json` | the snippet catalogue Insert Snippet reads |
| `<prefix>/share/time-warp-studio/examples/` | the programs File > Open Example lists |
| `<prefix>/share/doc/time-warp-studio/` | the documentation Help > Documentation reads |
| `<prefix>/share/applications/time-warp-studio.desktop` | the desktop entry |
| `<prefix>/share/icons/hicolor/256x256/apps/time-warp-studio.png` | the icon |

The bridge goes in `share/time_warp` because that is where the application looks
for it (`../share/time_warp` relative to the binary), so an installed build needs
no path configuration. It still needs `python3` on `PATH` for lesson checks; no
language's Run uses it.

### What is not packaged yet

Flatpak, Snap, a Windows installer and a macOS bundle are not built: `release.yml`
produces Linux artifacts only. What a release *does* ship is described next — a
`.deb` and an `.rpm` for x86_64 and arm64, an AppImage for x86_64, a tarball for both
architectures, and a container image.

### 8. A release without building anything

Every release page carries a ready-built application, and none of it needs a
compiler on your machine. `uname -m` says which architecture is yours: `x86_64`
or `aarch64`. Each format spells it its own way — a `.deb` says `amd64`/`arm64`,
an `.rpm` says `x86_64`/`aarch64`, and the tarball and the AppImage say
`x86_64`/`arm64`.

| You have | Take | Then |
| --- | --- | --- |
| a desktop and no wish to install anything | `Time_Warp_Studio-<version>-x86_64.AppImage` | `chmod +x` it and double-click. It needs no root, no package manager and no Qt: everything is inside the file. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | Double-click it, or `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-x86_64.tar.gz`, or `-arm64` | `tar -xzf … -C / --strip-components=1` |

The **AppImage is the answer for a non-technical user**: one file, double-clicked,
nothing installed. It is built for glibc 2.39 or newer (Ubuntu 24.04 and later, or
a distribution of similar age); on anything older use the package for that
distribution, which resolves the libraries properly.

The **packages** put the binary, the bridge, the desktop entry and the icon where
the system expects them, so the application appears in the menu and can be removed
with the package manager. They declare `python3` and the Qt 6 runtime as
dependencies, and they carry the example programs — **File > Open Example** lists
them, so a new install has something to run.

The **tarball** is the install tree above, plus the licence and the two READMEs.
It needs Qt 6, a C++ runtime and `python3` for the lesson check — the same
requirements as a local build, minus the compiler.

The container image is `ghcr.io/james-honeybadger/time-warp-studio:<version>`
(or `:latest`), and it carries everything, including Qt and Python:

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

The bind mount for `~/.time_warp` is what keeps lesson progress between runs; the
container runs as a non-root `studio` user, so its home directory is the only
place it writes. Sound works if the host's device is passed through
(`--device /dev/snd`), because `SOUND` and `PLAY` hand their WAV to `aplay`.

The install target above, plus the desktop entry and icon, is the foundation for
distro packaging. See [ROADMAP.md](ROADMAP.md).
