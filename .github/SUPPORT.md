# Support

Time Warp Studio is a desktop application maintained by one person in their own
time. There is no support contract and no response-time promise — but reports and
questions from people actually using it are the reason it exists, so they are
welcome and they get read.

## Where to ask

| What | Where |
| ---- | ----- |
| Something is broken | [Open a bug report](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=bug_report.yml) — the form asks for the things needed to reproduce it |
| An idea for a change | [Open a feature request](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=feature_request.yml) |
| A language you would like supported | [Open a language request](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=language_request.yml) — the form explains what a language costs to add |
| Contributing code or docs | [CONTRIBUTING.md](../CONTRIBUTING.md) |
| A security problem | [SECURITY.md](../SECURITY.md) — **not** a public issue |
| Everything else | <james@honey-badger.org> |

## Read these first

Most questions are already answered, and the answer is usually better than the
one I would type:

- **[docs/INDEX.md](../docs/INDEX.md)** — every guide, in one table.
- **[docs/INSTALLATION.md](../docs/INSTALLATION.md)** — requirements, building
  from source, and what a failure at each step usually means.
- **[docs/ROADMAP.md](../docs/ROADMAP.md)** — what is deliberately not built
  yet. A dedicated player target, an automatic update check, code-signed Windows
  and macOS builds and Flatpak/Snap are on this list. A missing feature from it is
  not a bug, and reporting one as a regression costs a round trip.
- **[docs/USER_GUIDE.md](../docs/USER_GUIDE.md)** — the IDE manual: projects, the
  REPL, debugging, SQL, terminal tools and the API.
- **[docs/FAQ.md](../docs/FAQ.md)** — the questions that come up most.
- **[docs/LANGUAGE_GUIDE.md](../docs/LANGUAGE_GUIDE.md)** — the reference for all
  eleven languages, including what each one is *not* able to do.
- **[docs/API.md](../docs/API.md)** — the JSON-RPC methods and the linkable C++
  libraries.

## What is supported

**Platforms.** Linux, Windows and macOS are all built and tested on every change:
the 21 suites run under GCC on Ubuntu 24.04, under MSVC on Windows, under MSVC
with Qt 5.15 for the Windows 7 artifact and under Apple's Clang on macOS, and a
tagged release ships a Windows installer, a second installer for Windows 7 and a
universal macOS disk image beside the Linux packages. Say which platform you are
on when you report; the three differ in their file dialogs, their fonts and their
packaging, and a report that names the platform is one I can reproduce.

**Windows 7** is built and released, and tested on Windows 11 — no CI runner is
older than Windows Server 2019. It is expected to work; it is not verified on
Windows 7 hardware, and a report from a real 7 machine is the most useful one
anybody could send.

**Configurations.** Every build configuration is expected to be green: Release,
Debug, AddressSanitizer and UndefinedBehaviorSanitizer. A crash or a diagnostic
that appears in only one of them is a real bug and worth reporting.

**The languages.** All ten are supported, and each has an executor, a test suite,
an example, command coverage, a tutorial, a reference section and a lesson. Every
language runs in C++ in the application, and so does the lesson check. The Python
reference engine they were ported from was deleted at 18.0; what the parity gate
compares against is a recorded corpus, which is not a supported API either.

**The lessons.** Both tools are supported — `native/tools/learn.cpp` for a
learner, `native/tools/teach.cpp` for a teacher — and so is the curriculum. A
lesson that cannot be finished, or whose starter already passes, is a bug;
`time_warp_teach validate` is what proves it either way.

**The automation surface.** `./api` and its JSON-RPC methods are a supported
surface, as is the `./twc` terminal compiler. `./run.sh` is the supported way to
build and launch from a checkout.

## What is not

- **Every package format.** A tagged release builds a Windows installer and ZIP,
  a Windows 7 installer and ZIP, a universal macOS disk image, a `.deb`, an
  `.rpm`, a tarball, an AppImage (x86_64) and a container image, and `make -C native install` produces the same
  tree locally, with Qt deployed into it. Flatpak, Snap, an arm64 AppImage and
  code-signed (Developer ID / Authenticode) builds are not made.
- **A player executable.** `--compile` writes a copy of the IDE with the authoring
  UI hidden; a small `time_warp_player` that opens a program is queued work without a
  date.
- **Keeping your programs working across versions.** The languages are
  reimplementations of languages older than most of the people using them, so
  their semantics are stable — but Time Warp Studio's own file formats, settings
  and lesson ids are not yet frozen.
- **Homework.** Questions about how a language works, or why a checkpoint checks
  what it checks, are welcome and will be answered. Questions that amount to
  "write this for me" will not be.

## If you found a bug

Please include:

1. **What you did, and what happened instead** — the exact program if one is
   involved. A ten-line program that shows the problem is worth more than a
   paragraph describing it.
2. **How you are running it** — from a build tree, or installed. These find the
   curriculum and the examples differently, so the answer changes where to look.
3. **Your platform and Qt version** (`make -C native info` prints both).
4. **The Errors tab, or the terminal output.** If it crashed, a backtrace.

A report that says "the graphics don't show up" is genuinely hard to act on: the
project's most characteristic bug is a program that runs successfully and draws
nothing, so *how* you know it drew nothing — an empty canvas, an error in the
console, a stroke count — is the useful part.
