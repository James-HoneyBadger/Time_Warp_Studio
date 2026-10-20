# Time Warp Studio — Documentation Index

Time Warp Studio is a C++20 / Qt6 desktop IDE for ten educational programming
languages, with an integrated turtle graphics canvas. This page is the map.

> **New here?** Build and run it from the [README](../README.md), then try
> `Examples/logo/mandala.logo` with **Run (F9)**.

---

## The documents

| Document | What it covers |
| --- | --- |
| [INSTALLATION.md](INSTALLATION.md) | Requirements, building, running, troubleshooting a build |
| [ARCHITECTURE.md](ARCHITECTURE.md) | How the IDE, the engines and the bridge fit together |
| [USER_GUIDE.md](USER_GUIDE.md) | The IDE manual: projects, REPL, debugging, SQL, terminal tools and automation |
| [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) | Command reference for all ten languages |
| [lessons/](lessons/README.md) | The guided lessons: the curriculum, and the student and teacher guides |
| [README_TESTING.md](README_TESTING.md) | The suites, the example gate and the parity harnesses |
| [ROADMAP.md](ROADMAP.md) | What is built, what is next, what was deliberately deferred |
| [API.md](API.md) | The JSON-RPC automation API and the linkable C++ libraries |
| [PORTING_PLAN.md](PORTING_PLAN.md) | The completed porting plan: what the removed PySide6 application had, how each feature was ported, and the test that proves it |
| [FAQ.md](FAQ.md) | Short answers to common questions |
| [`native/README.md`](../native/README.md) | The application itself: design notes, decisions, tests |
| [`native/bridge/`](../native/bridge/) | The Python reference engine the parity gates and the lesson check compare against |

---

## Language tutorials

One per active language, each written to be worked through:

| Tutorial | Language |
| --- | --- |
| [basic.md](tutorials/basic.md) | BASIC — line-oriented imperative programming |
| [logo.md](tutorials/logo.md) | Logo — turtle graphics |
| [pilot.md](tutorials/pilot.md) | PILOT — computer-aided instruction |
| [comal.md](tutorials/comal.md) | COMAL — structured BASIC |
| [rexx.md](tutorials/rexx.md) | REXX — string processing and scripting |
| [scheme.md](tutorials/scheme.md) | Scheme — functional programming |
| [apl.md](tutorials/apl.md) | APL — arrays, and no operator precedence |
| [focal.md](tutorials/focal.md) | FOCAL — line labels and branching on a sign |
| [chip8.md](tutorials/chip8.md) | CHIP-8 — opcodes and a 64x32 framebuffer |
| [smalltalk.md](tutorials/smalltalk.md) | Smalltalk — objects and messages |

---

## I want to…

| Goal | Start here |
| --- | --- |
| Build and run the IDE | [README](../README.md) |
| Fix a build or Qt problem | [INSTALLATION.md](INSTALLATION.md) |
| Use projects, the REPL, debugging, SQL or the API | [USER_GUIDE.md](USER_GUIDE.md) |
| Learn a language's commands | [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) |
| Understand the design | [ARCHITECTURE.md](ARCHITECTURE.md) |
| See what the old application had and this one does not | [PORTING_PLAN.md](PORTING_PLAN.md) |
| Add or change a language | [`native/README.md`](../native/README.md), then the bridge |
| Contribute | [CONTRIBUTING.md](../CONTRIBUTING.md) |
| Ask a question, report a bug, ask for a feature | [`.github/SUPPORT.md`](../.github/SUPPORT.md), then the issue forms |
| Report a security problem | [SECURITY.md](../SECURITY.md) — privately |
| See example programs | [`Examples/CATALOG.md`](../Examples/CATALOG.md) |

---

## A note on removed documentation

Time Warp Studio was previously implemented in Python with PySide6. That
application has been removed: the Qt application is the product, and the Python
that remains is the reference engine the parity gates, the command matrix and the
lesson check run in; every language runs in C++.

Documentation describing that application's interface — its old user guide,
progressive how-to guides, shortcut reference, debugger guide and troubleshooting
guide — was removed with it rather than left to mislead. The replacement is
[USER_GUIDE.md](USER_GUIDE.md), written for this Qt application.
`native/README.md` documents the implementation, [ROADMAP.md](ROADMAP.md) lists
what is still deliberately unbuilt, and [PORTING_PLAN.md](PORTING_PLAN.md) is the
completed record of the port.
