# Documentation

Time Warp Studio is a desktop programming environment for ten interpreters of the 1964–1980
period, built with C++20 and Qt 6 for Linux.

This page is the map, and it is also the table of contents of **Help > Documentation**
inside the application: the Help pane is built from the headings and tables below, so a page
listed here is a page you can read from inside the Studio, and one that is not listed is a
page the Help pane does not know about.

The set is organised by who is reading, because the four audiences want different things
from the same application.

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

## The project

| Guide | What it covers |
| --- | --- |
| [History](project/history.md) | The archive: what the PySide6 port replaced, and every release |
| [Contributing](../CONTRIBUTING.md) | Setup, the gates, and the recipes for a language, a command or a lesson |
| [Security policy](../SECURITY.md) | The threat model, and how to report a problem privately |
| [Code of conduct](../CODE_OF_CONDUCT.md) | What is expected in this project's spaces |
| [Support](../.github/SUPPORT.md) | Where to ask, and what is supported |
| [CI and deployment](../.github/DEPLOYMENT.md) | What each workflow checks, and the settings it needs |
| [Examples](../Examples/CATALOG.md) | The 68 programs that ship, and the gate that runs them |

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
| Read about the removed machines | [History](project/history.md), where the 28.0 release records their removal |

---

## What became of the earlier documentation

Two removals are worth knowing about, because both changed the shape of this set.

**The PySide6 application** was replaced by the Qt application in `native/`, and the guides
describing the old interface — its user guide, its how-to guides, its shortcut reference, its
debugger and troubleshooting guides — were removed with it rather than left to mislead. The
port's record, and what replaced each feature, is in [the history](project/history.md).

**The three emulated machines** — the HB-1 home computer, the HB-2 of 1992 and the CHIP-8
virtual machine — were removed at 28.0, with the language that ran on the last of them and
the four manuals that documented them. What remains is the IDE and the ten languages. The
release entry that records it, and the reasoning, are in [the history](project/history.md).
