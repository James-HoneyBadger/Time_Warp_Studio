# Installing and Building Time Warp Studio

Time Warp Studio is a Linux desktop application, built with **C++20 and Qt6**. Everything you need to run all eleven languages is in this
repository; nothing has to be downloaded at run time.

---

## 1. Requirements

| Requirement | Minimum | Notes |
| --- | --- | --- |
| C++ compiler | C++20 | GCC 11+ or Clang 14+ |
| CMake | 3.20 | |
| Qt6 | 6.2 | Widgets, Core and Gui modules must be installed; the Svg and PrintSupport modules are optional and enable SVG drawing export and printing |
| SQLite3 | 3.35 | The database manager and the languages' `SQL` statement; the development package (`libsqlite3-dev`) is required to build |
| zlib | 1.2 | Classroom bundles are zips, read and written with it; `zlib1g-dev` |
All eleven languages and the lesson check run in C++ inside the application.
Nothing in the product or its validation gates requires Python; the parity gate
compares against the frozen corpus rather than a live reference. See §6.

### Installing the toolchain

```bash
# Debian / Ubuntu
sudo apt install build-essential cmake qt6-base-dev libqt6svg6-dev libsqlite3-dev zlib1g-dev

# Fedora
sudo dnf install gcc-c++ cmake qt6-qtbase-devel qt6-qtsvg-devel sqlite-devel zlib-devel

# Arch
sudo pacman -S base-devel cmake qt6-base qt6-svg sqlite zlib
```

These are the three families the releases are built and tested on: Ubuntu 24.04
(GCC), Debian and a Fedora/RHEL-family distribution. Qt 6.2 or newer is the floor;
the release builds use 6.8.

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

The application is `native/build/time_warp_ide`.

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
ctest --test-dir native/build --output-on-failure   # 26 suites
make smoke                                          # all 81 examples
make command-matrix                                 # every command-table row
make repl-matrix                                    # every row through the REPL
./native/build/time_warp_teach validate             # every lesson solution
make doc-links doc-claims                           # the guides
make tree-check                                     # no Python in the tree
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

**Nothing in the tree needs Python.** There is no `pytest`, no `ruff`, no virtual
environment and no script to install: every check is a C++ target built from this
repository. If a guide you are following names a `python3` command, it is out of
date -- `make help` lists the real ones.

---

## 6. No interpreter anywhere

All eleven languages run in C++ inside the IDE, and so do the lesson check, the
classroom and every gate. The Python reference engine the ports were judged against
was deleted in 18.0, its answers are frozen in
`native/tests/golden/lang_corpus.json.gz`, and the harnesses that compare the ports
with those answers are `time_warp_gates`, built here.

The last Python in the tree — the two documentation gates, the version tool, the
icon generator and the `twc` wrapper — became C++ in 22.1.0. `make tree-check`
fails if a `.py` or a Python tool's config file comes back, so "the product needs
no interpreter to be built, tested or checked" is a claim with a gate behind it
rather than a wish.

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
reference engine was removed, and its recorded answers remain in the checkout.

### Installing into a prefix

`cmake --install` produces the package layout: `bin/`, `share/` and the desktop
entry, as in the table above. The Qt runtime is not copied in — a Linux
application uses the distribution's Qt 6, which is what the `.deb` and `.rpm`
declare as a dependency, and the AppImage is the one artifact that carries its own.

```bash
cmake --install native/build --prefix /usr
```

`cpack` builds the same tree into the packages from here (`cpack -G DEB`, and
`-G RPM` where `rpmbuild` is installed); see §8.

`PLAY`, `SOUND` and the two machines' beeps write a WAV and hand it to a
command-line player: `paplay` or `aplay` (or `ffplay`/`play`). With none of them
installed, those commands report that playback is unavailable rather than
pretending to make a sound.

### 8. A release without building anything

Every release page carries a ready-built application, and none of it needs a
compiler on your machine. `uname -m` says which architecture is yours: `x86_64`
or `aarch64`. Each format spells it its own way — a `.deb` says `amd64`/`arm64`,
an `.rpm` says `x86_64`/`aarch64`, and the tarball and the AppImage say
`x86_64`/`arm64`.

| You have | Take | Then |
| --- | --- | --- |
| a Linux desktop and no wish to install anything | `Time_Warp_Studio-<version>-x86_64.AppImage` | `chmod +x` it and double-click. It needs no root, no package manager and no Qt: everything is inside the file. |
| Ubuntu, Debian, Mint or Pop!_OS | `time-warp-studio_<version>_amd64.deb`, or `_arm64.deb` | Double-click it, or `sudo apt install ./time-warp-studio_<version>_amd64.deb` |
| Fedora, RHEL or openSUSE | `time-warp-studio-<version>-1.x86_64.rpm`, or `.aarch64.rpm` | `sudo dnf install ./time-warp-studio-<version>-1.x86_64.rpm` |
| any other Linux, or arm64 | `time-warp-studio-<version>-linux-x86_64.tar.gz`, or `-arm64` | `tar -xzf … -C / --strip-components=1` |

The **Flatpak** is the answer for anybody who does not want to think about Qt: the
KDE runtime carries the Qt this was built against, the application is sandboxed, and
the same file works on Arch, Fedora and Ubuntu. `flatpak install
./Time_Warp_Studio-<version>-x86_64.flatpak` and run **Time Warp Studio** from the
menu. The sandbox reaches the home directory — the IDE opens your programs — and
nothing else. `packaging/flatpak/` is the manifest, and `release.yml` builds it on
every tag so the file is exercised rather than only reviewed.

The **AppImage is the answer for a non-technical Linux user**: one file, double-clicked,
nothing installed. It is built for glibc 2.39 or newer (Ubuntu 24.04 and later, or
a distribution of similar age); on anything older use the package for that
distribution, which resolves the libraries properly.

The **packages** put the binary, the two lesson commands, the curriculum, the
desktop entry and the icon where the system expects them, so the application
appears in the menu and can be removed with the package manager. They declare the
Qt 6 runtime, SQLite and zlib as dependencies — and no Python, because none is
needed — and they carry the example programs, so **File > Open Example** lists them
and a new install has something to run.

The **tarball** is the install tree above, plus the licence and the two READMEs.
It needs Qt 6, SQLite, zlib and a C++ runtime — the same requirements as a local
build, minus the compiler.

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
