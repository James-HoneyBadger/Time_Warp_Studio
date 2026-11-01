# CI and deployment

What each workflow checks, why it is wired the way it is, and what it needs from the
repository settings. The release procedure and the artifacts are in
[the release guide](../docs/maintainer/release.md).

---

## Workflows

| Workflow | Trigger | What it does |
| --- | --- | --- |
| `native.yml` | Push and pull request to `main`, when anything under `native/` or `Examples/` changes | Configures and builds the Qt application, refuses to pass if the widget interface was skipped, runs the suites in Release, Debug, AddressSanitizer and UndefinedBehaviorSanitizer, the parity gate against the frozen corpus, every example through the native engines, every command row as a program and through the REPL, the curriculum check and the regenerated lesson plans. Then it smoke-tests the entry point (`--version`, `--help`, a bad-file screenshot) and renders an image artifact. One job on one runner: the Windows, macOS and Qt 5.15 configurations were removed with those ports |
| `docs.yml` | Push and pull request to `main`, when anything under `docs/`, `README.md`, the curriculum or `time_warp_teach` changes | That every guide `docs/INDEX.md` promises exists, that every relative link and anchor resolves, that all ten languages have a tutorial, and that every claim about a path, a name or a `make` target is still true. Runs `native/tools/docs`, a small C++ target with no Qt, so it answers in seconds and does not depend on the product's build |
| `markdown-lint.yml` | Push and pull request to `main`, when any `*.md` changes | `time_warp_docs md`, the project's own thirty rules from `native/tools/docs/src/docs/md_*_rules.cpp`, over `git ls-files '*.md'`. This step fails a pull request, because it is the project's tool with a verdict it stands behind. The spell check beside it stays advisory: a dictionary is not the project's to enforce |
| `version.yml` | Push and pull request to `main`, when `VERSION`, the README, the history or the tool's own source changes | That every hand-written copy of the version agrees with `VERSION`. Separate from the native build because it has to run on documentation changes, which the C++ job does not watch |
| `security.yml` | Push and pull request to `main`, weekly on a schedule, and by hand | Trivy over the tree — the Dockerfile the release packages from, every file no secret should be in, and any lockfile a dependency brings later — uploaded as SARIF so a finding lands in the repository's Security tab. Advisory rather than a gate: the SARIF is the record and the scan's exit code is 0, because a CVE in an upstream package is not a change a pull request here can make. The upload is skipped for a pull request from a fork, where the token is read-only; the scan still runs |
| `release.yml` | A `v*` tag, or `workflow_dispatch` with a version | Refuses a tag that disagrees with `VERSION`, builds on `ubuntu-24.04` and `ubuntu-24.04-arm`, packages the four formats plus a Flatpak bundle, runs the suites and an offscreen screenshot against each tree before packaging it, attaches every artifact with a `.sha256`, and pushes the container image. The release job runs in the `production` environment, which is what makes GitHub record a Deployment |

### Why the actions are pinned to SHAs

Every `uses:` names a full commit SHA with the release it came from in the comment beside
it. A tag is a name upstream may move, and the artifacts built here are the ones people
install, so the pin is what makes "the action that ran" the action that was reviewed.
`.github/dependabot.yml` asks for a `github-actions` update weekly, because a SHA cannot say
when it has gone stale; the workflow runs in that pull request decide whether the new one is
worth keeping.

`jurplel/install-qt-action` is the case that says why: upstream publishes `v4.x.y` tags and
no `v4` tag at all — `v4` is a branch — so `@v4` was a ref that could be repointed under this
tree.

Seven of the pinned actions are JavaScript, and every one is pinned at a Node 24 release,
because GitHub runs a Node 20 action on Node 24 only until it stops. The rest carry no Node:
`upload-sarif` from `codeql-action` and `trivy-action` are composite, and the spell check is
a Docker action.

### Two checks that exist because of a failure the others cannot see

The lesson plans in `docs/lessons/plans/` are **generated** from the curriculum, and the
failure that matters is somebody editing a lesson without regenerating the document a
teacher would then teach from: two copies of one lesson, and the copy that drifts is the one
being read. `native.yml` regenerates them and fails on a diff.

The same job runs `time_warp_teach validate`, which executes all sixty checkpoint solutions
and all sixty starters. A solution that does not pass is a lesson nobody can finish, and a
starter that already passes has nothing to do.

### Why the Markdown lint is a gate

Until 22.2.0 the step was one `super-linter` invocation and it was advisory, for a good
reason at the time: that image runs its other validators in the same step, so making it
fatal would have gated a pull request on tools this project had not reviewed. It also meant
the check could not fail, and on a machine without Node it did not run at all, so the claim
that the Markdown was clean rested on a report nobody had seen.

`time_warp_docs md` is the project's own tool: thirty rules from markdownlint's set, built
from one C++ compiler, with no dependency to fetch, and its verdict is the project's. The
rules it does not implement are listed in `md.hpp` with a reason each, mostly that they
encode a house style or need a CommonMark parser to answer honestly.

---

## What GitHub shows, and where it lives

Part of the documentation is the repository page itself. Most of it comes from files and
changes with a pull request; two pieces do not, and those are the two that go stale without
anybody noticing.

| Shown on | Source | Changed by |
| --- | --- | --- |
| The README under the file list | [`README.md`](../README.md) | A pull request |
| Contributing, Security and Code of Conduct links | [`CONTRIBUTING.md`](../CONTRIBUTING.md), [`SECURITY.md`](../SECURITY.md), [`CODE_OF_CONDUCT.md`](../CODE_OF_CONDUCT.md) | A pull request |
| The "New issue" chooser | [`.github/ISSUE_TEMPLATE/`](ISSUE_TEMPLATE) | A pull request |
| A new pull request's body | [`pull_request_template.md`](pull_request_template.md) | A pull request |
| Who is asked to review | [`CODEOWNERS`](CODEOWNERS) | A pull request |
| The **About** box: description, website, topics | **Repository settings** | Hand-edited in the web UI |
| Labels | **Repository settings** | Hand-edited in the web UI |

The two hand-edited rows are the risk, because nothing in CI sees them and nothing fails when
they fall behind. The mitigation is that the canonical text lives in the repository: the
description and the topic list are in the "About this repository" section of
[`README.md`](../README.md), and the issue forms only reference labels GitHub creates by
default. When the About box needs changing, copy from the README.

The issue forms are structured YAML, so a mistake in one is not a broken page but a form that
quietly does not work. Every form needs `name`, `description` and `body`; every element needs
a `type` from `markdown`, `input`, `textarea`, `dropdown` or `checkboxes`; everything except
`markdown` needs an `id` unique within its form and an `attributes.label`; `dropdown` and
`checkboxes` need `options`; and a `markdown` element needs `attributes.value`. A colon
inside an unquoted YAML value breaks the whole file, which is easy to do in a description and
invisible until somebody clicks New issue.

---

## Repository settings the pipeline needs

1. **Settings > Actions > General > Workflow permissions** — read and write, or the default
   plus the `permissions:` block in `release.yml`, which asks for `contents: write` to create
   the release and `packages: write` to push the image.
2. **Settings > Environments** — a `production` environment. The first run creates it; add
   it by hand only to attach protection rules such as required reviewers first.
3. **Package visibility** — a package pushed by `GITHUB_TOKEN` starts private even when the
   repository is public, and the first pull then fails with `unauthorized` for everybody.
   Set it public once, from the package page.

## Not automated, on purpose

Snap, an arm64 AppImage, a Flathub submission and a distribution repository. Each is
recorded in [the roadmap](../docs/maintainer/roadmap.md) and in
`native/share/unbuilt.txt`, and each is a decision about a channel rather than a missing
build step.
