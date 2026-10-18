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
  yet. Themes, auto-completion, find-in-files, the examples browser, printing
  and autosave are on this list. A missing feature from it is not a bug, and
  reporting one as a regression costs a round trip.
- **[docs/FAQ.md](../docs/FAQ.md)** — the questions that come up most.
- **[docs/LANGUAGE_GUIDE.md](../docs/LANGUAGE_GUIDE.md)** — the reference for all
  ten languages, including what each one is *not* able to do.

## What is supported

**Platforms.** Linux is what the application is developed and tested on, and what
CI builds and runs on every change (Ubuntu 24.04, Qt6, GCC). macOS and Windows
should build — it is Qt6 and C++20 with no platform-specific code — but they are
not covered by CI, so a platform-specific problem there may be something I cannot
reproduce. Say which platform you are on when you report.

**Configurations.** Every build configuration is expected to be green: Release,
Debug, AddressSanitizer and UndefinedBehaviorSanitizer. A crash or a diagnostic
that appears in only one of them is a real bug and worth reporting.

**The languages.** All ten are supported, and each has an executor, a test
module, an example, command coverage, a tutorial, a reference section and a
lesson. Every language runs in C++ in the application. The Python engine in
`native/bridge/` is the reference the parity gates compare against and the
engine a lesson check runs in; it is not a supported API.

**The lessons.** Both tools are supported — `native/tools/learn.py` for a
learner, `native/tools/teach.py` for a teacher — and so is the curriculum. A
lesson that cannot be finished, or whose starter already passes, is a bug;
`teach.py validate` is what proves it either way.

## What is not

- **Packages and installers.** There are none. `make -C native install` installs
  the binary, the bridge, a desktop entry and an icon, and that is the whole of
  it. `.deb`, `.rpm`, Flatpak, AppImage and Windows installers are on the roadmap.
- **Moving lesson checking into C++.** The Lessons panel asks the Python engine
  for a verdict; that is the remaining use of the bridge, and it is queued work
  without a date.
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
2. **How you are running it** — from a build tree, or installed. These take
   different paths to the bridge, so the answer changes where to look.
3. **Your platform and Qt version** (`make -C native info` prints both).
4. **The Errors tab, or the terminal output.** If it crashed, a backtrace.

A report that says "the graphics don't show up" is genuinely hard to act on: the
project's most characteristic bug is a program that runs successfully and draws
nothing, so *how* you know it drew nothing — an empty canvas, an error in the
console, a stroke count — is the useful part.
