# Time Warp Studio — Documentation Index

Time Warp Studio is an old-school desktop programming environment: eleven
interpreters from 1964 to 1980, a full IDE and turtle canvas — and two complete
machines inside it, the [HB-1](hb1/README.md) home computer from a parallel 1982
and the [CHIP-8](chip8/README.md) virtual machine.
This page is the map.

> **New here?** Build and run it from the [README](../README.md), then try
> `Examples/logo/mandala.logo` with **Run (F9)** — or type at the
> [HB-1](hb1/README.md) with `Ctrl+Alt+W`.

---

## The machines

| Document | What it covers |
| --- | --- |
| [MACHINES.md](MACHINES.md) | Why there are two, what they share, and the parallel-universe premise item by item |
| [hb1/README.md](hb1/README.md) | **HB-1 Owner's Manual** — front panel, first boot, everyday operation, persistence, limits |
| [hb1/roms.md](hb1/roms.md) | The ROM library: all eleven languages as cartridges, with prompts, direct mode and examples |
| [hb1/hb-dos.md](hb1/hb-dos.md) | **HB DOS 1.0** — every word, the catalogue, the errors, the image format |
| [hb1/config.md](hb1/config.md) | CONFIG: every setting, the phosphors, the glass, where it is stored |
| [hb1/bbs.md](hb1/bbs.md) | **WARP BOARD** — the machine's modem, the bulletins, posting, and where the board is kept |
| [hb1/inside.md](hb1/inside.md) | Inside the HB-1: how a keystroke becomes ink |
| [chip8/README.md](chip8/README.md) | **CHIP-8 Machine Manual** — the two ways to run, the window, keys, debugging, sound, limits |
| [chip8/assembly.md](chip8/assembly.md) | The assembler: listings, labels, directives, errors, worked programs |
| [chip8/instruction-set.md](chip8/instruction-set.md) | Every opcode as implemented, the 1977 quirks, timing, a full disassembly |
| [hb1/enhancements.md](hb1/enhancements.md) | Proposals for the HB-1 after the 24.0 golden release: a tape drive, a monitor, sprites, DOS 1.1 |
| [hb1/model-92.md](hb1/model-92.md) | **The Model 92** — the same company's 1992 computer: a bus, a video chip, four-channel audio, DOS 2.0, HB-16 and a desktop |
| [chip8/enhancements.md](chip8/enhancements.md) | Proposals for the CHIP-8 after the 24.0 golden release: breakpoints in the window, a trace, state files, a library manifest |

---

## The documents

| Document | What it covers |
| --- | --- |
| [INSTALLATION.md](INSTALLATION.md) | Requirements, building, running, troubleshooting a build |
| [ARCHITECTURE.md](ARCHITECTURE.md) | How the IDE, the machines, the engines and the bridge fit together |
| [USER_GUIDE.md](USER_GUIDE.md) | The IDE manual: projects, the REPL, debugging, SQL, the AI assistant, terminal tools, automation and appearance |
| [AI.md](AI.md) | The AI assistant: the services it speaks to, how to configure one, and exactly what is sent |
| [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) | Command reference for all eleven languages |
| [lessons/](lessons/README.md) | The guided lessons: the curriculum, and the student and teacher guides |
| [README_TESTING.md](README_TESTING.md) | The suites, the example gate and the parity harnesses |
| [ROADMAP.md](ROADMAP.md) | What is built, what is next, what was deliberately deferred |
| [API.md](API.md) | The JSON-RPC automation API and the linkable C++ libraries |
| [PORTING_PLAN.md](PORTING_PLAN.md) | The completed porting plan: what the removed PySide6 application had, how each feature was ported, and the test that proves it |
| [FAQ.md](FAQ.md) | Short answers to common questions |
| [`native/README.md`](../native/README.md) | The application itself: design notes, decisions, tests |
| [`native/tests/golden/`](../native/tests/golden/) | The frozen corpus: the reference engine's answers to 479 programs, which `make golden-check` compares the ports with |

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
| Meet the two machines | [MACHINES.md](MACHINES.md), then the [HB-1](hb1/README.md) and [CHIP-8](chip8/README.md) manuals |
| Programme the HB-1 | [hb1/README.md](hb1/README.md), [hb1/hb-dos.md](hb1/hb-dos.md) |
| Programme the CHIP-8 | [chip8/README.md](chip8/README.md), [chip8/assembly.md](chip8/assembly.md) |
| Use projects, the REPL, debugging, SQL or the API | [USER_GUIDE.md](USER_GUIDE.md) |
| Ask an AI to explain or fix a program | [AI.md](AI.md) |
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
application and the Python reference engine have both been removed: the Qt
application is the product, every language runs in C++, and parity uses a frozen
corpus of recorded answers.

Documentation describing that application's interface — its old user guide,
progressive how-to guides, shortcut reference, debugger guide and troubleshooting
guide — was removed with it rather than left to mislead. The replacement is
[USER_GUIDE.md](USER_GUIDE.md), written for this Qt application.
`native/README.md` documents the implementation, [ROADMAP.md](ROADMAP.md) lists
what is still deliberately unbuilt, and [PORTING_PLAN.md](PORTING_PLAN.md) is the
completed record of the port.
