<!--
The template asks for the things the gates cannot check.

Every row below is either a gate that has to have been run or a decision no gate can
make: whether a failure was reported rather than swallowed, whether a document that
overstates the product was left alone. A pull request that skips the engine suites can
break the lessons, and one that adds a language without a lesson is a language the panel
cannot offer.

Keep the headings: CONTRIBUTING.md tells contributors to fill them in, and they are what
a reader of the merged history scrolls to.
-->

# Description

<!-- What this changes, in a sentence or two. -->

## Why

<!-- The problem it solves. Link the issue if there is one: "Closes #12". -->

## Testing

<!-- How you know it works. Name the commands you ran and what they said. -->

| Gate | Command | Result |
| --- | --- | --- |
| C++ suites | `make test` | <!-- 24/24 --> |
| Debug tree | `make test-debug` | <!-- 24/24, or "not run: no C++ changed" --> |
| Sanitizers | `make asan` | <!-- clean, or "not run: no C++ changed" --> |
| Example corpus | `make smoke` | <!-- all 67, stroke counts unmoved --> |
| Command coverage | `make command-matrix` | <!-- 532 rows, 0 failed, 0 gaps --> |
| REPL coverage | `make repl-matrix` | <!-- 532 rows, 0 failed --> |
| Parity | `make golden-check` | <!-- 0 differences, or which corpus entries you changed and why --> |
| Lesson curriculum | `./native/build/time_warp_teach validate` | <!-- clean, or "not run: no lesson changed" --> |
| Documentation lint | `make lint-md` | <!-- 0 issues --> |
| Documentation links | `make doc-links` | <!-- 275 checked, all resolve --> |
| Documentation claims | `make doc-claims` | <!-- no false claims --> |

## Checklist

- [ ] **The build is warning-free.** Every target compiles with `-Werror` and a wide warning set, so a warning fails the build rather than scrolling past.
- [ ] **A failure that used to be silent is now reported.** Silence is this project's characteristic bug: a language that ran successfully and drew nothing, a command that returns nothing, a lesson whose answer did not work. If you fixed one, say which.
- [ ] **Every failure reaches the console with the right prefix** (`❌`, `✅`, `ℹ️`) rather than being swallowed or thrown out of an executor.
- [ ] **Anything a document claims is true.** If a feature is partial or unreachable, the document says so and [the roadmap](../docs/maintainer/roadmap.md) records it. This has been wrong before: a README claimed an evaluator nothing linked to.
- [ ] **A new language or command is added everywhere it is listed**, not only where it was easiest. [CONTRIBUTING.md](../CONTRIBUTING.md) has the list: registry row, executor, the Qt language table, an example, command-table rows, a tutorial, a reference section, a lesson, a corpus entry.
- [ ] **A behaviour change edits the parity corpus in the same commit**, because the gate compares with a recording rather than with a live reference.
- [ ] **A new lesson passes `time_warp_teach validate`** — every solution *and* every starter — and the generated plans have been regenerated.
- [ ] **`docs/INDEX.md` is updated** for a new page, because it is also the application's Help tree and `help_browser_tests` fails when a row does not resolve.
- [ ] **Tests were added for the behaviour**, not only for the code path. A regression test for a fixed bug is expected.
- [ ] **No unrelated changes.** A reformat, a rename or a reordered list makes the actual change hard to read.

## Notes for the reviewer

<!-- Anything you are unsure about, deliberately left alone, or would like a second opinion on. -->
