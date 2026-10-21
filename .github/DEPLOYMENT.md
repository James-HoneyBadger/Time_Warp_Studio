# CI and Deployment

Time Warp Studio is a desktop application: there is no hosted service, and a
release is a tagged build rather than a deployment. This page lists the workflows
that exist and what they gate.

## Workflows

| Workflow | Trigger | What it does |
| -------- | ------- | ------------ |
| `native.yml` | Push and pull request to `main`, when anything under `native/`, `Examples/` or the bridge changes | Configures and builds the Qt application, refuses to pass if the widget UI was skipped, then runs the 21 CTest suites in Release, Debug, AddressSanitizer and UndefinedBehaviorSanitizer configurations, all three differential gates (each asserting **0 unexpected mismatches**), every example program through the reference engine, and every command row both as a program and through the native REPL. Also smoke-tests the entry point (`--version`, `--help`, a bad-file screenshot) and renders an image artifact. A second job, `cross-platform`, builds the same tree with MSVC on `windows-latest` and with Apple's Clang on `macos-latest` and runs the same suites there, because each of those compilers rejects things GCC accepts and a release is the wrong place to find that out. A third, `windows-7`, configures with `-DTIME_WARP_QT5=ON -DTIME_WARP_WIN7=ON` against Qt 5.15.2 and runs the same suites, so the second Qt a release ships is not the first one anybody builds with it. |
| `docs.yml` | Push and pull request to `main`, when anything under `docs/`, `README.md`, the lesson curriculum (`native/bridge/time_warp/features/lessons.json`) or `teach.py` changes | That every guide `docs/INDEX.md` promises exists — including the HB-1 and CHIP-8 manuals — that every relative link and `#anchor` in the guides resolves, that all ten languages have a tutorial, that the lesson plans still match the curriculum, and that every lesson can still be finished. Runs with nothing but `python3` from the runner image, so it answers in seconds and does not depend on the C++ build. |
| `markdown-lint.yml` | Push and pull request to `main`, when any `*.md` changes | Markdown lint against `.markdownlint.json`, and a spell check. **Both steps are `continue-on-error`**, so neither can fail a build today — see the note below. |
| `version.yml` | Push and pull request to `main`, when `VERSION`, the README, the changelog, `docs/`, `Scripts/` or the engine's `__init__.py` changes | That every hand-written copy of the version agrees with `VERSION`. Separate from the native build because it has to run on documentation changes, which the C++ job does not watch. |
| `release.yml` | A `v*` tag, or `workflow_dispatch` with a version | Refuses a tag that disagrees with `VERSION` (running `version_sync.py` as the same check `version.yml` makes), then builds the release on five runners -- `ubuntu-24.04` and `ubuntu-24.04-arm` for the Linux packages, `windows-latest` for the installer and ZIP, `windows-2022` for the Windows 7 installer that Qt 5.15 makes possible, `macos-latest` for the universal disk image -- running the CTest suites and an offscreen screenshot against each tree before packaging it, and attaching every artifact with a `.sha256` to the GitHub Release. It also pushes the container image to `ghcr.io`. The release job runs in the `production` environment, which is what makes GitHub record a Deployment. |

**The actions, and the Node runtime behind them.** Every action is pinned to a
major tag, and the majors are the ones that run on Node 24 — `checkout@v5`,
`upload-artifact@v6`, `download-artifact@v7`, `docker/login-action@v4`,
`docker/build-push-action@v7`. GitHub runs a Node 20 action anyway, on Node 24,
but only until it stops, so a workflow that still pins one is a workflow with a
date on it. `ilammy/msvc-dev-cmd` is the exception: its newest release
(`v1.13.0`) is still a Node 20 action and upstream has published nothing newer,
so the annotation it draws is upstream's to fix.

Two of these exist because of a failure the others cannot see. The lesson plans in
`docs/lessons/plans/` are **generated** from the curriculum, and the failure that
matters is somebody editing a lesson without regenerating the document a teacher
would then teach from — two copies of one lesson, and the copy that drifts is always
the one being read. `docs.yml` regenerates them and fails on a diff. It also runs
`teach.py validate`, which executes all sixty checkpoint solutions and all sixty
starters; a solution that does not pass is a lesson nobody can finish, and a starter
that already passes has nothing to do.

**Why the markdown lint does not fail a build.** The step is one `super-linter`
invocation, and that image runs its other validators in the same step, so making the
step fatal would gate a pull request on tools this project has not reviewed. The
Markdown itself is clean: `make lint-md` reports nothing across the repository's own
Markdown files, and the 45 findings that used to sit in `native/README.md`,
`.github/copilot-instructions.md`, `.github/RELEASE_CHECKLIST.md` and `SECURITY.md`
are fixed. The editor is pointed at the same file set — `.vscode/settings.json` sets
`markdownlint.appliesTo` to `markdownOnly` and skips `.venv` and the build trees — so
the Problems panel and the command agree. The spellcheck step beside it is advisory on
its own terms.

The Python application's workflows — `ci.yml`, `build_release.yml`,
`publish-python.yml` and `publish-container.yml` — were removed with it. They
linted, tested, packaged and published the PySide6 application, and none of that
applies to a C++ build. `release.yml` is the replacement for the last two: it
packages the C++ build and publishes it, which is a different job from the Python
one rather than a port of it.

## Deployments and packages

Time Warp Studio is a desktop application with no hosted service, so there is
nothing to roll out to a server. What a release *does* produce is a Deployment
record and two installable artifacts, and both are wired to `release.yml`.

**The Deployment.** The `release` job names `environment: production`, and its URL
is the release page. That is the whole mechanism: GitHub records a Deployment
whenever a job in an environment runs, keeps the history under
Settings > Environments, and shows it on the repository's Environments page.
Nothing is deployed anywhere by it — the record is what says "16.0.0 was published
from this commit, by this workflow, at this time", which is the question a
Deployment is actually asked. Add **required reviewers** to the environment in the
web UI if a release should need a second pair of eyes; the workflow needs no
change.

**The artifacts.**

| Artifact | Where | What it is |
| -------- | ----- | ---------- |
| `time-warp-studio-<version>-linux-<arch>.tar.gz` + `.sha256` | Release assets | The staged `cmake --install` tree (`/usr/bin`, `/usr/share/applications`, the icon, the documentation at `/usr/share/doc/time-warp-studio`), the example programs and the licence. Unpack into `/` and run `time_warp_ide`. |
| `time-warp-studio_<version>_<arch>.deb`, `time-warp-studio-<version>-1.<arch>.rpm` | Release assets | CPack packages over the same install rules, with Qt 6, SQLite and `python3` as declared dependencies. x86_64 and arm64. The install includes `time_warp_ide` and the `time_warp_api` automation binary. |
| `Time_Warp_Studio-<version>-x86_64.AppImage` | Release assets | One file, no root, Qt bundled by `linuxdeploy`. **x86_64 only**: the ARM build of the bundling tool is not exercised by this pipeline, and an artifact nobody has run is not one to publish. |
| `time-warp-studio-<version>-win64.exe` + `.sha256` | Release assets | The NSIS installer CPack builds on `windows-latest`: `bin\time_warp_ide.exe` and `share\…` under `Program Files\Time Warp Studio`, with the Qt DLLs and plugins `windeployqt` copied in during the install, Start Menu and desktop shortcuts, and an uninstaller. |
| `time-warp-studio-<version>-win64.zip` + `.sha256` | Release assets | The same installed tree as a portable ZIP, for a user who would rather not run an installer. |
| `Time_Warp_Studio-<version>-macOS-universal.dmg` + `.sha256` | Release assets | A CPack DragNDrop disk image built on `macos-latest` from a universal (`arm64;x86_64`) build: `Time Warp Studio.app` with its frameworks from `macdeployqt`, the bridge, examples, snippets and documentation inside `Contents/Resources`, ad-hoc signed (`codesign --sign -`) so it starts on Apple silicon, and an `/Applications` symlink for drag-install. The MIT licence is a file in `Contents/Resources`, not a volume agreement in front of the image, so the image mounts without a dialog. Not notarised. |
| `ghcr.io/<owner>/time-warp-studio:<version>` and `:latest` | GitHub Packages (container registry) | Built by `packaging/docker/Dockerfile`: two stages, Arch base, Qt 6 and Python in the runtime layer, `alsa-utils` so `SOUND` can reach a device if one is mounted. Runs as a non-root user. The same tags go to `<owner>/time_warp_studio`, the repository's name lowercased, because that is where releases before 16.4.2 put it: a note or script naming it pulls the current build rather than the one that was last published there. |

The image is also the headless path: `-e QT_QPA_PLATFORM=offscreen` plus
`--run --export /work/out.png /work/program.logo` renders a drawing with no
display at all, which is what a teacher producing worksheets from a script wants.

**Repository settings the pipeline depends on.** All three are one-time:

1. **Settings > Actions > General > Workflow permissions** — "Read and write
   permissions", or leave the default and rely on the `permissions:` block at the
   top of `release.yml`. That block asks for `contents: write` (to create the
   release) and `packages: write` (to push the image); a repository restricted to
   read-only tokens for workflows refuses the push with a 403 on the first run.
2. **Settings > Environments** — a `production` environment. It is created
   automatically by the first run that uses it, so the only reason to add it by
   hand is to attach protection rules first.
3. **Package visibility** — a package pushed by `GITHUB_TOKEN` starts **private**
   even when the repository is public, and the first pull then fails with
   `unauthorized` for everybody else. Set it to public once, on the package page,
   or with `gh api --method PATCH /user/packages/container/time-warp-studio -f
   visibility=public`.

**Trying it without spending a tag.** `gh workflow run release.yml -f
version=X.Y.Z` runs the whole pipeline against `main`. It publishes a real release
and a real image, so either dispatch against a throwaway version or delete both
afterwards (`gh release delete vX.Y.Z`, and delete the image version in the
package page).

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
