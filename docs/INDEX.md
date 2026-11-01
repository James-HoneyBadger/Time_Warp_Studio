# Time Warp Studio — Documentation Index

Time Warp Studio is a desktop programming environment for ten interpreters of the
1964–1980 period, built with C++20 and Qt 6 for Linux. This page is the map of its
documentation; every guide below is installed with the application and readable from
**Help > Documentation**.

> New here? Build and run it from the [README](../README.md), then open
> `Examples/logo/mandala.logo` and press **F9**.

---

## The guides

| Document | What it covers |
| --- | --- |
| [INSTALLATION.md](INSTALLATION.md) | Requirements, building, installing, packaging, troubleshooting a build |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Process and thread model, the canvas queue, the engine libraries, extension points, the test gates |
| [USER_GUIDE.md](USER_GUIDE.md) | The IDE: projects, the REPL bar, debugging, SQL, the embedded terminal, the assistant, automation, appearance |
| [AI.md](AI.md) | The assistant: the services it speaks to, how to configure one, and exactly what is sent |
| [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) | Command reference for all ten languages |
| [lessons/](lessons/README.md) | The curriculum, and the student and teacher guides |
| [README_TESTING.md](README_TESTING.md) | The suites, the example gate, the command matrix and the parity gate |
| [ROADMAP.md](ROADMAP.md) | What is built, what is next, what was deliberately deferred |
| [API.md](API.md) | The JSON-RPC automation API and the linkable C++ libraries |
| [PORTING_PLAN.md](PORTING_PLAN.md) | The completed record of the PySide6 port: what the old application had, and the test that proves each replacement |
| [FAQ.md](FAQ.md) | Short answers to specific questions |
| [`native/README.md`](../native/README.md) | The application itself: design notes, decisions, tests |
| [`native/tests/golden/`](../native/tests/golden/) | The frozen corpus: the reference engine's answers to 442 programs, against which `make golden-check` compares the engines |

---

## Language tutorials

One per language, each written to be worked through in order:

| Tutorial | Language |
| --- | --- |
| [basic.md](tutorials/basic.md) | BASIC — line-numbered imperative programming |
| [logo.md](tutorials/logo.md) | Logo — turtle graphics and procedures |
| [pilot.md](tutorials/pilot.md) | PILOT — computer-aided instruction |
| [comal.md](tutorials/comal.md) | COMAL — structured BASIC |
| [rexx.md](tutorials/rexx.md) | REXX — string processing and scripting |
| [scheme.md](tutorials/scheme.md) | Scheme — functional programming |
| [apl.md](tutorials/apl.md) | APL — arrays, and no operator precedence |
| [focal.md](tutorials/focal.md) | FOCAL — line labels and sign branching |
| [smalltalk.md](tutorials/smalltalk.md) | Smalltalk — objects and messages |
| [forth.md](tutorials/forth.md) | Forth — stack, dictionary, words |

---

## Where to start

| Goal | Document |
| --- | --- |
| Build and run the IDE | [README](../README.md), then [INSTALLATION.md](INSTALLATION.md) |
| Diagnose a build or Qt problem | [INSTALLATION.md](INSTALLATION.md#5-troubleshooting) |
| Use projects, the REPL, debugging, SQL or the API | [USER_GUIDE.md](USER_GUIDE.md) |
| Ask an assistant to explain or fix a program | [AI.md](AI.md) |
| Learn a language's commands | [LANGUAGE_GUIDE.md](LANGUAGE_GUIDE.md) |
| Understand the design | [ARCHITECTURE.md](ARCHITECTURE.md) |
| Add or change a language | [ARCHITECTURE.md](ARCHITECTURE.md#adding-a-language), then [`native/README.md`](../native/README.md) |
| Run or extend the test gates | [README_TESTING.md](README_TESTING.md) |
| Contribute | [CONTRIBUTING.md](../CONTRIBUTING.md) |
| Ask a question, report a bug, request a feature | [`.github/SUPPORT.md`](../.github/SUPPORT.md), then the issue forms |
| Report a security problem | [SECURITY.md](../SECURITY.md), privately |
| Read example programs | [`Examples/CATALOG.md`](../Examples/CATALOG.md) |

---

## Documentation that was removed

Time Warp Studio was previously implemented in Python with PySide6. That application
and the Python reference engine were removed; the Qt application is the product, every
language runs in C++, and parity is measured against a frozen corpus of recorded
answers. The guides describing the old interface — its user guide, progressive how-to
guides, shortcut reference, debugger guide and troubleshooting guide — were removed with
it rather than left to mislead. [USER_GUIDE.md](USER_GUIDE.md) replaces them.

A second removal is recorded rather than reversed. The three emulated machines that
earlier releases carried — the HB-1 home computer, its 1992 successor the HB-2, and the
CHIP-8 virtual machine — were removed in 28.0, together with the language that ran on
the last of them and the manuals that documented them. What remains is the IDE and the
ten languages. The [changelog](../CHANGELOG.md) records both removals entry by entry,
and [PORTING_PLAN.md](PORTING_PLAN.md) is the completed record of the port that preceded
them.
