# Contributing

Time Warp Studio is a C++20 and Qt 6 application for Linux, and a documentation set with
gates that hold the two together. What a change has to satisfy here is mostly mechanical:
build, run the gates, and say what you changed.

[CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) is the standard expected of everyone in the
project. [.github/SUPPORT.md](.github/SUPPORT.md) says where to ask a question;
[SECURITY.md](SECURITY.md) says where to report a vulnerability, privately.

---

## Ways to contribute

**A bug report** is most useful with a program that shows it. Paste the program, say what
happened and what you expected, and name the language and the version from **Help >
About**. A turtle drawing that came out wrong is worth a screenshot; the Turtle Log under
the canvas lists the drawing commands the canvas actually received, and that is usually
where the answer is.

**A feature request** should start from what you cannot do. The
[roadmap](docs/maintainer/roadmap.md) records what is deliberately not built, so a request
that one of those five items explains is a design difference rather than a gap.

**A documentation change** needs the same gate run as a code change, because
`make doc-claims` reads the guides and fails when one describes a path, a target or a name
that is not there.

**Code** arrives as a pull request against `main`. Read the two sections below first: the
gates are the review, and a change that runs them all has very little left to argue about.

---

## Setting up

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp_Studio.git
cd Time_Warp_Studio
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
```

The requirements, the per-distribution install lines and what to do when Qt is not found
are in [getting started](docs/user/getting-started.md).

### Install the hooks

`.githooks/pre-commit` runs `detect-secrets` against `.secrets.baseline`, so a credential
cannot reach GitHub in a commit:

```bash
pip install detect-secrets            # or your distribution's package
pre-commit install --install-hooks    # if you use pre-commit
git config core.hooksPath .githooks   # if you do not
```

The baseline is what the hook compares against, so it reports only what has been added
since. A false positive is silenced by adding its line to `.secrets.baseline`, in the same
commit that explains why.

---

## The gates

Everything is C++ built from this tree. There is no interpreter to install and none to run;
`make tree-check` fails if a `.py` file or a Python tool's configuration file reappears.

```bash
make test              # the 24 C++ suites
make test-all          # test, smoke, both command matrices, the corpus, curriculum and docs
make smoke             # every example program through the native engines
make command-matrix    # every command-table row as a program
make repl-matrix       # the same rows through a REPL session
make golden-check      # the ports against the frozen corpus, 636 programs
make teach-check       # every lesson solution, and every starter
make doc-links doc-claims # the guides: links, anchors, and the facts they claim
make lint-md           # the house Markdown rules
make tree-check        # no Python, no second toolchain
```

`make test-all` is what CI runs, and what a pull request should be able to say it passed.
`make help` lists every target with a line on each. The suites, the parity corpora and what
each gate is worth are in [testing](docs/maintainer/testing.md).

**Warnings are errors.** Every target compiles with `-Wall -Wextra -Wpedantic -Wshadow
-Wcast-qual -Wuseless-cast -Wlogical-op -Wnull-dereference` and `-Werror`;
`-DTIME_WARP_EXPR_WERROR=OFF` exists for experimenting and is not for a pull request.

---

## House rules for code

- **C++20, matching the file you are editing.** No new dependency without a decision
  recorded in the pull request.
- **Comments explain why.** The unusual decisions — the turtle's compass heading, the
  canvas command queue, the corpus's documented divergences — are documented where they are
  made, and a comment that restates the code is noise beside them.
- **An executor is a function, not a class.** Its contract is what
  `timewarp/lang/turtle_host.hpp` documents: it returns the text to display, with the
  message prefixes, and it never throws out. All UI state lives in the Qt application.
- **A new control ships working.** A menu entry is added when it does something; until
  then it does not exist. The one exception is an action that reports why it cannot act,
  and even that is a decision to write down.
- **One fact, one home.** The language table is `native/include/timewarp/lang/registry.hpp`
  and nothing else; `make registry-check` enforces it. A guide links to a fact rather than
  restating it.
- **A behaviour change edits the corpus in the same commit.** The parity gate compares with
  a recording, so a deliberate change is a corpus edit, and a recording that quietly
  disagrees with the code is the failure the gate exists to catch.

### Message prefixes

Every string an engine returns carries a prefix, and the IDE's presentation depends on it:
`❌` for an error, `✅` for a success confirmation, `ℹ️` for information, `🎨` for a theme
or display change, `🚀` for an execution event, `🐢` for a drawing action, `📝` for an input
prompt. This is the output convention and it is the only place emoji remain: an interface
control carries a drawn icon (`icons::glyph()`) and a word, never an emoji.

### Prose

The manuals are part of the product. British spelling, plain declarative technical prose,
second person only in a procedure, em dashes sparingly, no exclamation marks, no whimsy.
A claim in a guide is a claim a gate will check, so write what the code does and not what
it nearly does. `make lint-md` is the mechanical half of that rule, and review is the rest.

---

## Recipes

### Adding a command to an existing language

1. Implement it in the language's executor under `native/src/lang/`.
2. Add a row to `native/share/command_rows.json`: the language's protocol key, the
   command, a program that exercises it, and what that program must produce.
3. If the command is one a guide describes, add it to
   [the reference](docs/languages/reference.md).
4. `make command-matrix repl-matrix golden-check` — the corpus needs an entry for a
   command whose output is now pinned.

### Adding a language

The order matters, because the native implementation is the only implementation there is.

1. **The registry** — `native/include/timewarp/lang/registry.hpp` gains a row: the enum
   member, the protocol key, the display name and the extensions. It is the one table;
   `make registry-check` fails if a second list appears.
2. **The executor** — an `execute_x` in `native/src/lang/`, registered in `executor_for`
   and, if it runs its own loop, in `is_whole_program`. A line-by-line language gets one
   function per statement; a whole-program one gets the source and runs to completion.
3. **The Qt side** — `native/src/ui/language.cpp` maps the enum by position, with a
   `static_assert` against the registry.
4. **Coverage** — an example under `Examples/<language>/`, rows in `command_rows.json` for
   every command, a tutorial in `docs/languages/tutorials/` (the Help pane builds its
   per-language guide link from the settings key), a section in
   [the reference](docs/languages/reference.md), and a corpus entry.
5. **Teaching** — `time_warp_teach new <id> --language NAME` prints a lesson skeleton the
   loader accepts, and `time_warp_teach validate` is the test.
6. **The index** — add the tutorial to the `Language tutorials` table in
   [docs/INDEX.md](docs/INDEX.md). `help_browser_tests` fails when that section stops
   listing one tutorial per language.

### Adding a lesson

`time_warp_teach new` then `validate`, then regenerate the plans with
`time_warp_teach plans --write docs/lessons/plans`. The plans are generated: editing one by
hand fails CI. [The curriculum](docs/lessons/README.md) has the details and
[the format](docs/lessons/FORMAT.md) the fields.

### Adding a theme

A record in `native/src/ui/theme_table.inc` and, if it needs one, a high-contrast variant.
Everything else is derived: the toolbar, tabs, docks and menus come from the theme's colours
through `design::tokens()`, so a theme is its palette and not a stylesheet.
`ui_layout_tests` pins the count and the colour roles a theme must supply.

### Changing a guide

The documentation has four audiences and an index that is also the application's Help tree:

| Directory | Audience |
| --- | --- |
| `docs/user/` | Someone using the Studio |
| `docs/languages/` | Someone learning or looking up a language |
| `docs/maintainer/` | Someone working on the code |
| `docs/project/` | The archive, and where the project's records live |
| `docs/lessons/` | A learner or a teacher following the curriculum |

`docs/INDEX.md` lists every page inside the documentation root; a row that points outside
it is dropped from the Help pane, and `help_browser_tests` asserts that every row resolves.
Then run `make doc-links doc-claims lint-md`.

A claim you cannot make true yet can be marked: `<!-- doc-claims: ignore -->` after the
statement, or a region between `<!-- history -->` and `<!-- /history -->` for text about how
things used to be. Both are decisions to write down, not escapes from the gate.

---

## Commits and pull requests

**Commit messages** say what changed and why, in the imperative, with the reasoning in the
body. A commit that fixes a defect says how it was found; one that changes behaviour says
what the old behaviour was.

**Branches** are `feature/…`, `fix/…` or `docs/…` off `main`. Nothing else is required: the
project does not use a release branch, and a release is a tag on `main`.

**A pull request** answers the questions in
[the template](.github/pull_request_template.md), of which the two that matter are what the
change does to a user and which of the gates were run. If a gate could not be run, say so
and why — a claim that a suite passed is worth having only if it did.

Reviews are about the code and the claim it makes. The gates cover the rest.
