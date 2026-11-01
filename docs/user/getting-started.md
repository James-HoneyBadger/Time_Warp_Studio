# Getting started

Time Warp Studio is a Linux desktop application built with C++20 and Qt 6. Everything
needed to run all ten languages is in the repository: there is no interpreter to install
and nothing is fetched at run time. This page takes a machine with a Linux distribution
on it to a program running in the Studio, and then to a build you can work on.

If you would rather not build anything, the [front page](../../README.md#install) lists
the four release formats; come back here for the first program.

---

## What a build needs

| Requirement | Minimum | Notes |
| --- | --- | --- |
| C++ compiler | C++20 | GCC 11 or Clang 14 |
| CMake | 3.20 | |
| Qt 6 | 6.2 | Widgets, Core and Gui. Svg and PrintSupport are optional: they add SVG drawing export and the two Print entries |
| SQLite | 3.35 | The Database tab and the languages' `SQL` statement; the development package is needed to build |
| zlib | 1.2 | Classroom bundles are zips |

```bash
# Debian, Ubuntu, Mint, Pop!_OS
sudo apt install build-essential cmake qt6-base-dev libqt6svg6-dev libsqlite3-dev zlib1g-dev

# Fedora, RHEL, openSUSE
sudo dnf install gcc-c++ cmake qt6-qtbase-devel qt6-qtsvg-devel sqlite-devel zlib-devel

# Arch
sudo pacman -S base-devel cmake qt6-base qt6-svg sqlite zlib
```

Qt 6.2 is the floor; the release builds use 6.8. Without the Svg package the Studio
builds and runs, and drawing export offers PNG, JPEG and BMP instead of SVG. Print
Support comes with the Qt base packages above; a build without it removes **File > Print
Code** and **File > Print Drawing** rather than leaving dead entries.

Nothing here needs an interpreter. All ten languages, the lesson check, the classroom and
every validation gate are C++ compiled from this tree, and `make tree-check` fails if a
`.py` file or a Python tool's configuration file reappears.

---

## Build

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
```

The first build takes a few minutes and later ones are incremental. The tree carries no
binary: `native/build/time_warp_ide` exists because you built it.

| Option | Effect |
| --- | --- |
| `-DCMAKE_BUILD_TYPE=Debug` | Unoptimised, with symbols |
| `-DCMAKE_BUILD_TYPE=RelWithDebInfo` | Optimised, with symbols |
| `-DTIME_WARP_EXPR_WERROR=OFF` | Build without `-Werror` |
| `-DCMAKE_PREFIX_PATH=/path/to/qt6` | Find a Qt that is not on CMake's search path |

---

## Run

`run.sh` builds only if the binary is missing or out of date, then replaces itself with
the Studio, so arguments and the exit status are the application's:

```bash
./run.sh                                          # an empty workspace
./run.sh Examples/logo/mandala.logo               # open a program
./run.sh --run Examples/basic/hello.bas           # open it and run it
```

`TIME_WARP_BUILD_DIR` selects another build tree, and `TIME_WARP_SKIP_BUILD=1` launches
an existing build without invoking CMake.

The binary directly, for everything `run.sh` forwards:

```bash
./native/build/time_warp_ide                      # an empty workspace
./native/build/time_warp_ide --run Examples/logo/mandala.logo
./native/build/time_warp_ide --export drawing.png Examples/logo/mandala.logo
./native/build/time_warp_ide --help               # every option
make -C native run                                # the same launch, through make
```

Two companion commands sit at the repository root:

```bash
./twc Examples/basic/hello.bas -o hello   # a standalone executable, no Qt needed at run time
./twc Examples/basic/hello.bas -o hello --db school.db
./api                                     # JSON-RPC on stdin and stdout
```

[The Studio](the-studio.md) is the manual for the window itself, and
[automation](automation.md) covers `api` and `twc`.

---

## Your first program

Press **Ctrl+N**, type this, and press **F9**:

```basic
10 PRINT "HELLO, 1975"
20 FOR I = 1 TO 3
30 PRINT I * I
40 NEXT I
```

The console below the editor shows:

```text
HELLO, 1975
1
4
9
```

BASIC is a line-by-line language, so the line numbers are part of the program rather than
decoration: `RUN` starts at the lowest number and `GOTO` names one. Now open
`Examples/logo/mandala.logo` from **File > Open Example** and press **F9** again. The
canvas fills with a drawing as it is made, and the scrubber under it replays the strokes.

The two runs differ in more than their output. BASIC is called once per statement by the
shared loop, which is what lets the debugger pause between statements. Logo runs its own
loop and decides where a pause may land. Both report to the same console, the same
Variables table and the same canvas.

To try all ten, **File > Examples Gallery…** shows every shipped program with the drawing
it makes.

---

## Check the build

```bash
ctest --test-dir native/build --output-on-failure   # the 24 suites
make smoke                                          # the 67 example programs it runs
make command-matrix                                 # one row per command
make repl-matrix                                    # the same rows through the REPL
./native/build/time_warp_teach validate             # every lesson solution and starter
make doc-links doc-claims                           # the guides
make tree-check                                     # no Python in the tree
```

All of them must pass before a change is worth committing. The full set, including the
sanitizer builds and the parity corpora, is in
[testing](../maintainer/testing.md).

---

## Install

A build installs itself, which is also how to confirm the installed layout finds its own
data — the curriculum, the snippets and the command table — instead of only the build
tree's copy:

```bash
make -C native install                 # to /usr/local, which may need sudo
make -C native install PREFIX=$HOME/.local
make -C native uninstall               # removes what install wrote
make -C native install DESTDIR=/tmp/stage PREFIX=/usr   # for staging a package
```

These paths have to stay together:

| Path | What it is |
| --- | --- |
| `<prefix>/bin/time_warp_ide` | the application |
| `<prefix>/bin/time_warp_api` | the automation service |
| `<prefix>/bin/time_warp_teach` | validate the curriculum, and bundle or unpack a class |
| `<prefix>/bin/time_warp_learn` | the learner's command: list, show, start, check, hint |
| `<prefix>/share/time-warp-studio/lessons.json` | the curriculum the Lessons panel reads |
| `<prefix>/share/time-warp-studio/snippets.json` | the snippet catalogue |
| `<prefix>/share/time-warp-studio/examples/` | the programs **File > Open Example** lists |
| `<prefix>/share/doc/time-warp-studio/` | the documentation **Help > Documentation** reads |
| `<prefix>/share/applications/time-warp-studio.desktop` | the desktop entry |
| `<prefix>/share/icons/hicolor/256x256/apps/time-warp-studio.png` | the icon |

The data goes beside the binary because that is where the application looks,
`../share/time-warp-studio` relative to itself, so an installed build needs no path
configuration. Two environment variables override it for a development tree:
`TIME_WARP_LESSONS` names a curriculum and `TIME_WARP_SNIPPETS` a snippet catalogue.

The Qt runtime is not copied in: a Linux application uses the distribution's Qt 6, which
is what the `.deb` and `.rpm` declare as a dependency. The AppImage is the one artifact
that carries its own. [Release](../maintainer/release.md) covers the packages, the
container image and what each one contains.

---

## When something goes wrong

**`Could not find a package configuration file provided by "Qt6"`.** Qt is installed
somewhere CMake does not look. Pass its prefix with `-DCMAKE_PREFIX_PATH=/path/to/qt6`.
If Qt is genuinely absent the configure step says so and skips the widget interface
rather than failing obscurely.

**A stale build tree fails to regenerate**, usually with `IMPORTED_LOCATION not set for
Qt6::Gui configuration Debug`. Delete it (`rm -rf native/build`) and configure again. Do
not "fix" it with `-DCMAKE_MAP_IMPORTED_CONFIG_DEBUG=Release`: that breaks
`OpenGL::OpenGL` and `OpenGL::GLX` in a way that is harder to diagnose.

**The Lessons panel says the curriculum was not found.** It is shipped data,
`share/time-warp-studio/lessons.json` beside the application, and `TIME_WARP_LESSONS`
overrides it. A checkout finds `native/share/lessons.json` from the build tree.

**A guide names a `python3` command.** The guide is out of date. Every check is a C++
target built here; `make help` lists the real ones.

**`SOUND` and `PLAY` make no noise.** They write a WAV and hand it to a command-line
player: `pw-play` first (PipeWire), then `paplay`, `aplay`, `ffplay` or `play`. With none
installed the commands report that playback is unavailable rather than pretending.

The [FAQ](faq.md) answers the rest.
