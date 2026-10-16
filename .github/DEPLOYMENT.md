# CI and Deployment

Time Warp Studio is a desktop application: there is no hosted service, and a
release is a tagged build rather than a deployment. This page lists the workflows
that exist and what they gate.

## Workflows

| Workflow | Trigger | What it does |
| -------- | ------- | ------------ |
| `native.yml` | Push and pull request to `main`, when anything under `native/`, `Examples/` or the bridge changes | Configures and builds the Qt application, refuses to pass if the widget UI was skipped, then runs the 11 CTest suites in Release, Debug, AddressSanitizer and UndefinedBehaviorSanitizer configurations, both differential gates (each asserting **0 unexpected mismatches**), and every example program through the real bridge. Also smoke-tests the entry point (`--version`, `--help`, a bad-file screenshot) and renders an image artifact. |
| `docs.yml` | Push and pull request to `main`, when anything under `docs/`, `README.md`, the lesson curriculum (`native/bridge/time_warp/features/lessons.json`) or `teach.py` changes | That every guide `docs/INDEX.md` promises exists, that all ten languages have a tutorial, that the lesson plans still match the curriculum, and that every lesson can still be finished. Runs with nothing but `python3` from the runner image, so it answers in seconds and does not depend on the C++ build. |
| `markdown-lint.yml` | Push and pull request to `main`, when any `*.md` changes | Markdown lint against `.markdownlint.json`, and a spell check. **Both steps are `continue-on-error`**, so neither can fail a build today — see the note below. |
| `version.yml` | Push and pull request to `main`, when `VERSION`, the README, the changelog, `docs/`, `Scripts/` or the engine's `__init__.py` changes | That every hand-written copy of the version agrees with `VERSION`. Separate from the native build because it has to run on documentation changes, which the C++ job does not watch. |

Two of these exist because of a failure the others cannot see. The lesson plans in
`docs/lessons/plans/` are **generated** from the curriculum, and the failure that
matters is somebody editing a lesson without regenerating the document a teacher
would then teach from — two copies of one lesson, and the copy that drifts is always
the one being read. `docs.yml` regenerates them and fails on a diff. It also runs
`teach.py validate`, which executes all sixty checkpoint solutions and all sixty
starters; a solution that does not pass is a lesson nobody can finish, and a starter
that already passes has nothing to do.

**Why the markdown lint does not fail a build.** The linter is `super-linter` with
`VALIDATE_MARKDOWN`, so it reads every `*.md` in the repository rather than the
hand-picked set the project lints locally, and four files carry findings that predate
this work: `native/README.md` (10 — five extra top-level headings, four fenced blocks
with no language, one code span with a stray space), `.github/copilot-instructions.md`
(30, list and heading spacing in a file written for an agent to read),
`.github/RELEASE_CHECKLIST.md` (3, fenced blocks inside list items) and `SECURITY.md`
(2, table column alignment). Making the step blocking means clearing those first; it is
recorded here rather than fixed in passing so that the next person to touch it knows
what they are taking on.

The Python application's workflows — `ci.yml`, `build_release.yml`,
`publish-python.yml` and `publish-container.yml` — were removed with it. They
linted, tested, packaged and published the PySide6 application, and none of that
applies to a C++ build.

## What GitHub shows, and where it lives

Part of this repository's documentation is not in a file: it is the repository
page itself. Most of it comes from files and changes with a pull request; two
pieces do not, and they are the two that go stale without anybody noticing.

| Shown on | Source | Changed by |
| -------- | ------ | ---------- |
| The README under the file list | [`README.md`](../README.md) | A pull request |
| Contributing, Security and Code of Conduct links | [`CONTRIBUTING.md`](../CONTRIBUTING.md), [`SECURITY.md`](../SECURITY.md), [`CODE_OF_CONDUCT.md`](../CODE_OF_CONDUCT.md) | A pull request |
| The "New issue" chooser | [`.github/ISSUE_TEMPLATE/`](ISSUE_TEMPLATE) | A pull request |
| A new pull request's body | [`pull_request_template.md`](pull_request_template.md) | A pull request |
| Who is asked to review | [`CODEOWNERS`](CODEOWNERS) | A pull request |
| The **About** box: description, website, topics | **Repository settings** | Hand-edited in the web UI |
| Labels | **Repository settings** | Hand-edited in the web UI |

The two hand-edited rows are the risk, because nothing in CI can see them and
nothing fails when they fall behind. The mitigation is that the canonical text
lives in the repository: **the description and the topic list are in the
"About this repository" section of [`README.md`](../README.md)**, and the issue
templates only reference labels GitHub creates by default (`bug`, `enhancement`).
When the About box or the topics need changing, copy from the README.

The issue forms are structured YAML, so a mistake in one is not a broken page but
a form that quietly does not work. The rules are: every form needs `name`,
`description` and `body`; every element needs a `type` from
`markdown`, `input`, `textarea`, `dropdown`, `checkboxes`; everything except
`markdown` needs an `id` that is unique within its form and an `attributes.label`;
`dropdown` and `checkboxes` need `options`; and a `markdown` element needs
`attributes.value`. A colon inside an unquoted YAML value breaks the whole file,
which is easy to do in a description and invisible until a user clicks New issue.

## Releasing

There is no automated release pipeline yet. The manual checklist is in
[RELEASE_CHECKLIST.md](RELEASE_CHECKLIST.md), and these gaps are deliberate:

- **No distro packages or installers.** Linux/snap packaging and Windows
  installers were built for the removed Python application and were deleted with
  it, rather than left pointing at a binary that no longer exists. What exists
  today is `make -C native install` (with `DESTDIR` staging), which installs the
  binary, the bridge, a desktop entry and an icon.
- **Nothing is published to a package registry.** The PyPI project `timewarp-ide`
  and the GHCR image both packaged the Python application.

Turning the install target into `.deb`/`.rpm`/Flatpak/AppImage artifacts, a
Windows installer and a published release is tracked in `docs/ROADMAP.md`.

To produce a release today: build from source, run the release checklist, push an
annotated tag, and attach the built binary plus `SHA256SUMS.txt` to the GitHub
Release.
