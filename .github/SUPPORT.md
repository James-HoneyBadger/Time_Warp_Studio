# Support

Time Warp Studio is maintained by one person in their own time. There is no support
contract and no response-time commitment. Reports and questions from people using it are
read, and one that includes what the form asks for is answered sooner than one that does
not.

## Where to ask

| What | Where |
| --- | --- |
| Something is broken | [Open a bug report](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=bug_report.yml); the form asks for what is needed to reproduce it |
| An idea for a change | [Open a feature request](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=feature_request.yml) |
| A language you would like supported | [Open a language request](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues/new?template=language_request.yml); the form explains what a language costs to add |
| Contributing code or documentation | [CONTRIBUTING.md](../CONTRIBUTING.md) |
| A security problem | [SECURITY.md](../SECURITY.md), and not a public issue |
| Everything else | <james@honey-badger.org> |

## Read these first

Most questions are answered already, and the answer is better than one typed again:

| Read | For |
| --- | --- |
| [docs/INDEX.md](../docs/INDEX.md) | Every guide, in one table. It is also the table of contents of **Help > Documentation** |
| [Getting started](../docs/user/getting-started.md) | Requirements, building, and what a failure at each step means |
| [The Studio](../docs/user/the-studio.md) | Projects, the REPL, debugging, SQL, the terminal, appearance |
| [The languages](../docs/languages/reference.md) | Every command, including what each language cannot do |
| [Questions](../docs/user/faq.md) | The ones that come up most |
| [Automation](../docs/user/automation.md) | The JSON-RPC methods and the linkable C++ libraries |
| [Roadmap](../docs/maintainer/roadmap.md) | What is deliberately not built. A missing feature from that list is not a regression, and reporting one costs a round trip |

## What is supported

**Platforms.** Linux, and only Linux. There is no Windows build and no macOS build, and a
report about either is out of scope: the project has no way to build, run or test one.

**Your distribution.** Releases are built and tested on Ubuntu 24.04. The `.deb` and `.rpm`
name the distribution's Qt 6 and SQLite, so a distribution with those in the ranges
[getting started](../docs/user/getting-started.md) lists should work. Say which distribution
and version you are on, and include the output of
`./native/build/time_warp_ide --version`.

**Configurations.** Every build configuration is expected to be green: Release, Debug,
AddressSanitizer and UndefinedBehaviorSanitizer. A crash or a diagnostic that appears in only
one of them is a real bug and worth reporting.

**The languages.** All ten are supported, and each has an executor, a suite, an example,
command coverage, a tutorial, a reference section and a lesson. Every language runs in C++
inside the application, and so does the lesson check. The Python reference engine they were
ported from was deleted at 18.0, and the last Python in the tree went at 22.1; the parity
gate compares against a recorded corpus, which is not a supported API either.

**The lessons.** Both commands are supported — `time_warp_learn` for a learner,
`time_warp_teach` for a teacher — and so is the curriculum. A lesson that cannot be finished,
or whose starter already passes, is a bug, and `time_warp_teach validate` proves it either
way.

**The automation surface.** `./api` and its JSON-RPC methods are supported, as is the `./twc`
terminal compiler. `./run.sh` is the supported way to build and launch from a checkout.

## What is not

- **Every package format.** A release builds a `.deb`, an `.rpm`, a tarball, an x86_64
  AppImage, a Flatpak bundle and a container image; `make -C native install` produces the
  same tree locally. Snap and an arm64 AppImage are not made.
- **A player for another platform.** `--compile` writes a real player with the program
  inside it — a window, the drawing, the output and three keys, not an editor with its docks
  hidden — but it is a Linux binary and links the system Qt 6.
- **Keeping your programs working across versions.** The languages are reimplementations of
  languages older than most of the people using them, so their semantics are stable. Time
  Warp Studio's own file formats, settings keys and lesson ids are not yet frozen.
- **Homework.** Questions about how a language works, or why a checkpoint checks what it
  checks, are welcome. Questions that amount to "write this for me" are not.

## If you found a bug

Include:

1. **What you did, and what happened instead** — the exact program if one is involved. A
   ten-line program that shows the problem is worth more than a paragraph describing it.
2. **How you are running it** — from a build tree, or installed. Those find the curriculum
   and the examples differently, so the answer changes where to look.
3. **Your platform and Qt version.** `make -C native info` prints both.
4. **The Errors panel, or the terminal output.** If it crashed, a backtrace.

A report that says "the graphics do not show up" is hard to act on, because the project's
most characteristic bug is a program that runs and draws nothing. *How* you know it drew
nothing — an empty canvas, an error in the console, a stroke count, the Turtle Log — is the
useful part.
