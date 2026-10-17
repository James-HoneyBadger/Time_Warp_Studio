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
| [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) | Command reference for all ten languages |
| [lessons/](lessons/README.md) | The guided lessons: the curriculum, and the student and teacher guides |
| [README_TESTING.md](README_TESTING.md) | The suites, the example gate and the parity harnesses |
| [ROADMAP.md](ROADMAP.md) | What is built, what is next, what was deliberately deferred |
| [FAQ.md](FAQ.md) | Short answers to common questions |
| [`native/README.md`](../native/README.md) | The application itself: design notes, decisions, tests |
| [`native/bridge/`](../native/bridge/) | The temporary engine for the four languages that are not yet native |

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
| Learn a language's commands | [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) |
| Understand the design | [ARCHITECTURE.md](ARCHITECTURE.md) |
| Add or change a language | [`native/README.md`](../native/README.md), then the bridge |
| Contribute | [CONTRIBUTING.md](../CONTRIBUTING.md) |
| Ask a question, report a bug, ask for a feature | [`.github/SUPPORT.md`](../.github/SUPPORT.md), then the issue forms |
| Report a security problem | [SECURITY.md](../SECURITY.md) — privately |
| See example programs | [`Examples/CATALOG.md`](../Examples/CATALOG.md) |

---

## A note on removed documentation

Time Warp Studio was previously implemented in Python with PySide6. That
application has been removed: the Qt application is the product, and the Python
that remains is a temporary engine for the four languages that have not been
ported to C++ yet.

Documentation describing that application's interface — the user guide, the
progressive how-to guides, the keyboard-shortcut reference, the debugger guide and
the troubleshooting guide — was removed with it rather than left to mislead.
`native/README.md` documents the Qt interface, and
[ROADMAP.md](ROADMAP.md) lists the features that have not been ported yet.
