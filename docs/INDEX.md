# Documentation

Time Warp Studio is a desktop programming environment for ten interpreters of the 1964–1980
period, built with C++20 and Qt 6 for Linux.

This page is the map, and it is also the table of contents of **Help > Documentation**
inside the application: the Help pane is built from the headings and tables below, so a page
listed here is a page you can read from inside the Studio, and one that is not listed is a
page the Help pane does not know about.

The set is organised by who is reading: the four audiences want different things from
the same application.

---

## Using the Studio

For someone with a program to write.

| Guide | What it covers |
| --- | --- |
| [Getting started](user/getting-started.md) | Requirements, building, installing, and the first program |
| [The Studio](user/the-studio.md) | Documents, projects, the REPL bar, debugging, SQL, the terminal, appearance and shortcuts |
| [Automation](user/automation.md) | The JSON-RPC API, the linkable C++ libraries, and compiling a program |
| [Questions](user/faq.md) | Short answers, with a pointer to the long one |

## Language tutorials

One per language, each written to be worked through in order. The application's Help pane
links the tutorial for the language in front, so these rows are also what makes that link
work.

| Tutorial | Language |
| --- | --- |
| [BASIC](languages/tutorials/basic.md) | Line-numbered imperative programming |
| [APL](languages/tutorials/apl.md) | Arrays, and no operator precedence |
| [Logo](languages/tutorials/logo.md) | Turtle graphics and procedures |
| [PILOT](languages/tutorials/pilot.md) | Computer-aided instruction |
| [FOCAL](languages/tutorials/focal.md) | Line labels and sign branching |
| [Scheme](languages/tutorials/scheme.md) | Functional programming |
| [COMAL](languages/tutorials/comal.md) | Structured BASIC |
| [REXX](languages/tutorials/rexx.md) | String processing and scripting |
| [Smalltalk](languages/tutorials/smalltalk.md) | Objects and messages |
| [Forth](languages/tutorials/forth.md) | Stack, dictionary, words |

## The languages

| Guide | What it covers |
| --- | --- |
| [The ten languages](languages/README.md) | What each one is, how they differ, and how to choose a first |
| [Language reference](languages/reference.md) | Every command of every language, and the limits that stop a run |

## The machines

Three machines ship beside the Studio, each an application of its own. Their manuals are
listed here because the Help pane should be able to reach them too.

| Guide | What it covers |
| --- | --- |
| [The machines](MACHINES.md) | What the three are, how they differ, and what they share |
| [HB-1 Owner's Manual](hb1/README.md) | The 1982 home computer: front panel, first boot, everyday operation |
| [The ROM library](hb1/roms.md) | The ten language ROMs: banner, prompt, direct mode, session |
| [HB DOS 1.0](hb1/hb-dos.md) | The disk: every word, the catalogue, the errors, the image format |
| [HB-1 CONFIG](hb1/config.md) | Every setting, every default, the phosphors and the glass |
| [Inside the HB-1](hb1/inside.md) | How a keystroke becomes ink |
| [WARP BOARD](hb1/bbs.md) | The bulletin board on the other end of the line |
| [The Model 92 proposal](hb1/model-92.md) | The specification the HB-2 was built from |
| [CHIP-8 Machine Manual](chip8/README.md) | The 1977 virtual machine: the window, the keypad, debugging, sound |
| [The CHIP-8 assembler](chip8/assembly.md) | Listings: syntax, labels, directives, errors |
| [The CHIP-8 instruction set](chip8/instruction-set.md) | Every opcode as implemented, quirks and timing |
| [CHIP-8 tutorial](tutorials/chip8.md) | Writing a listing from the first instruction |
| [HB-2 Manual](hb2/README.md) | The 1992 workstation: the bus, the screen, the sound, HB DOS 2.0 |
| [HB-16](hb2/instruction-set.md) | The HB-2's own processor, instruction by instruction |

## The curriculum

| Guide | What it covers |
| --- | --- |
| [Lessons](lessons/README.md) | The twenty lessons, the sixty checkpoints, how one is checked, and how to run a class |
| [Lesson format](lessons/FORMAT.md) | The fields a lesson file carries |
| [Lesson plans](lessons/plans/README.md) | Objectives, notes and worked answers per lesson, generated from the curriculum |

## Working on the code

| Guide | What it covers |
| --- | --- |
| [Architecture](maintainer/architecture.md) | The process and thread model, the libraries, the canvas queue, the extension points |
| [Decisions](maintainer/decisions.md) | Why the code is shaped the way it is, and what was tried and removed |
| [Testing](maintainer/testing.md) | The suites, the example and command gates, the parity corpora, and what each is worth |
| [Roadmap](maintainer/roadmap.md) | What is built, what is deliberately not, and what is known to be wrong |
| [Release](maintainer/release.md) | The version, the procedure, and what a release contains |
| [WebAssembly](maintainer/wasm.md) | The browser build: the target, the page, the storage, and the workflow |

## The project

| Guide | What it covers |
| --- | --- |
| [History](project/history.md) | The archive: what the PySide6 port replaced, and every release |
| [Contributing](../CONTRIBUTING.md) | Setup, the gates, and the recipes for a language, a command or a lesson |
| [Security policy](../SECURITY.md) | The threat model, and how to report a problem privately |
| [Code of conduct](../CODE_OF_CONDUCT.md) | What is expected in this project's spaces |
| [Support](../.github/SUPPORT.md) | Where to ask, and what is supported |
| [CI and deployment](../.github/DEPLOYMENT.md) | What each workflow checks, and the settings it needs |
| [Examples](../Examples/CATALOG.md) | The 77 programs that ship, and the gate that runs them |

---

## Where to start

| Goal | Start here |
| --- | --- |
| Write a program in one of the languages | [Getting started](user/getting-started.md), then the tutorial for that language above |
| Learn a language's commands | [The reference](languages/reference.md) |
| Use the editor, the debugger, SQL or the terminal | [The Studio](user/the-studio.md) |
| Drive the Studio from a script | [Automation](user/automation.md) |
| Teach with it | [The curriculum](lessons/README.md) |
| Understand or change the code | [Architecture](maintainer/architecture.md), then [testing](maintainer/testing.md) |
| Report a bug or ask for a feature | [.github/SUPPORT.md](../.github/SUPPORT.md) |
| Report a security problem | [SECURITY.md](../SECURITY.md), privately |
| Use one of the machines | [The machines](MACHINES.md), then that machine's manual |

---

## What became of the earlier documentation

Two removals are worth knowing about, because both changed the shape of this set.

**The PySide6 application** was replaced by the Qt application in `native/`, and the guides
describing the old interface — its user guide, its how-to guides, its shortcut reference, its
debugger and troubleshooting guides — were removed with it rather than left to mislead. The
port's record, and what replaced each feature, is in [the history](project/history.md).

**The three machines** — the HB-1 home computer, the HB-2 of 1992 and the CHIP-8 virtual
machine — were removed at 28.0, with the language that ran on the last of them and the four
manuals that documented them. They returned at 29.0 as applications of their own rather than
windows inside the IDE, so the Studio kept its ten languages and each machine got a program,
a settings scope and a manual of its own. [The machines](MACHINES.md) is the map; the two
release entries are in [the history](project/history.md).
