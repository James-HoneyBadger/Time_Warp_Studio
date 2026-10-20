<!--
The PR template asks for the things the gates cannot check.

Every checkbox below is either a gate that has to have been run (`make test`,
`make engine-test`, `make smoke`) or a decision no gate can make (whether a failure
was reported rather than swallowed, whether a document that overstates the product
was left alone). Nothing here is a formality: a PR that skips the engine suite can
break the lessons, and a PR that adds a language without a lesson is a language the
panel cannot offer.

Keep the headings. They are what `CONTRIBUTING.md` tells contributors to fill in,
and what a reader of the merged history scrolls to.
-->

# Description

<!-- What this changes, in a sentence or two. -->

## Why

<!-- The problem it solves. Link the issue if there is one: "Closes #12". -->

## Testing

<!-- How you know it works. Name the commands you ran and what they said. -->

| Gate | Command | Result |
| ---- | ------- | ------ |
| C++ suites | `make -C native test` | <!-- 20/20 --> |
| Debug tree | `make -C native test-debug` | <!-- 20/20, or "not run — no C++ changed" --> |
| Engine tests | `make engine-test` | <!-- N passed, 1 xfailed --> |
| Example corpus | `make smoke` | <!-- all 60, stroke counts unmoved --> |
| Command coverage | `python3 native/tools/command_matrix.py` | <!-- 447 rows, 0 failed, 0 gaps --> |
| Lesson curriculum | `python3 native/tools/teach.py validate` | <!-- clean, or "not run — no lesson changed" --> |
| Documentation lint | `make lint-md` | <!-- 0 issues --> |

## Checklist

- [ ] **The build is warning-free.** The C++ is compiled with `-Werror` and a wide warning set, so a warning fails the build rather than scrolling past.
- [ ] **A failure that used to be silent is now reported.** Silence is this project's characteristic bug: a language that ran successfully and drew nothing, a command that returns nothing at all, a lesson whose answer did not work. If you fixed one, say which.
- [ ] **Every failure is reported in the console with the right prefix** (`❌`, `ℹ️`, `✅`) rather than swallowed or turned into a bare exception.
- [ ] **Anything a document claims is true.** If a feature is partial, unported or unreachable, the document says so and `docs/ROADMAP.md` records it. This has been wrong before: a README claimed an in-process evaluator that nothing links.
- [ ] **A new language or command has been added everywhere it is listed**, not only where it was easiest. `docs/LANGUAGE_GUIDE.md` has the list: enum, registry, the IDE's language table, an example, command-table rows, a tutorial, a lesson, a reference section.
- [ ] **A new lesson passes `teach.py validate`**, which runs every checkpoint's own solution *and* its starter — so a solution that does not pass, or a starter that already does, is caught before a learner meets it.
- [ ] **Bridge code (if touched) is ruff clean and standard-library only.**
- [ ] **Tests were added for the behaviour**, not only for the code path. A regression test for a fixed bug is expected.
- [ ] **No unrelated changes.** A reformat, a rename or a reordered list makes the actual change hard to read.

## Notes for the reviewer

<!-- Anything you are unsure about, deliberately left alone, or would like a second opinion on. -->
