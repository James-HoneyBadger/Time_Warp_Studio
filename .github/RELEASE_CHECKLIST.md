# Release Checklist - Time Warp Studio vX.Y.Z

## Pre-Release Tasks

### Version Updates

`VERSION` is the source of truth: the Qt application reads it at configure time, so
the binary, the window title and the About box follow on their own. Everything else is
a hand-written copy, kept in step by `time_warp_docs version` and checked by the
`Version metadata` workflow.

- [ ] Set `VERSION` to the new release number
- [ ] Add a `## [X.Y.Z] - YYYY-MM-DD` section to `CHANGELOG.md`. The tool checks that
      this exists but will not write it: the newest heading in that file is usually a
      release that has already shipped, so renaming it would rewrite history
- [ ] Run `./native/tools/docs/build/time_warp_docs version --write` (after
      `make docs-tool`) to update the copies it owns — the README badge and the
      architecture guide title
- [ ] Run `./native/tools/docs/build/time_warp_docs version` and confirm it exits 0
      before tagging
- [ ] Check any prose that names a version by hand — the script only knows the places
      listed above

### Testing

- [ ] `make test` — the C++ suites
- [ ] `make test-debug` and `make asan` — the other build trees
- [ ] `make golden-check` — the ports against the frozen corpus, reporting only the
      documented divergences: 476 language programs (473 agree, 3 written down) and
      192 Scheme programs (168 agree, 24 written down)
- [ ] `make smoke` — all 69 example programs, through the native engines
- [ ] `make command-matrix` — every command row as a program (566 rows)
- [ ] `make repl-matrix` — every command row through the native REPL
- [ ] `make lint-md`, `make doc-links`, `make doc-claims` and `make tree-check` — the
      documentation gates, and the one that fails if a `.py` comes back
- [ ] `make qt5-check` — no call site names a signal Qt 5.15 overloads and Qt 6 does
      not, so the Windows 7 artifact is not the first build to notice
- [ ] Manual smoke test of the IDE (launch, open an example, run a turtle program)

### Documentation

- [ ] Update `CHANGELOG.md` with new version entry
- [ ] Refresh documentation footers (docs/FAQ.md, docs/INDEX.md)
- [ ] Ensure screenshots or captions reference the new release where applicable
- [ ] Run spell check / link validation for key documents
- [ ] `./native/build/time_warp_teach validate` is silent, and regenerating the
      plans (`plans --write docs/lessons/plans`) leaves no diff
- [ ] Markdown lint is clean — `make lint-md` (the editor lints the same file set;
      see `.vscode/settings.json`). The `Markdown Lint` workflow stays advisory, for
      the reason recorded in `DEPLOYMENT.md`

## GitHub Preparation

### Repository

- [ ] Create release branch `release/vX.Y.Z`
- [ ] Merge final changes and ensure `main` is up to date
- [ ] Tag commit: `git tag -a vX.Y.Z -m "Time Warp Studio vX.Y.Z"`
- [ ] Push branch and tag to origin

### Release Assets

- [ ] Nothing to build by hand: pushing the tag runs `release.yml`, which builds
      the Linux tarball, the `.deb`, the `.rpm` and (on x86_64) the AppImage **for
      both architectures** — `ubuntu-24.04` for x86_64, `ubuntu-24.04-arm` for
      arm64 — plus the **Windows installer and ZIP** on `windows-latest`, the
      **Windows 7 installer and ZIP** on `windows-2022` (Qt 5.15; see
      `docs/INSTALLATION.md`) and the **universal macOS disk image** on
      `macos-latest`. Each job runs the suites
      and an offscreen screenshot against the exact tree it packages, publishes
      `ghcr.io/<owner>/time-warp-studio:X.Y.Z` and attaches every file with a
      `.sha256` beside it. See `DEPLOYMENT.md` for what each job does and what it
      needs from the repository settings.

- [ ] If the pipeline has to be tried without spending a tag:
      `gh workflow run release.yml -f version=X.Y.Z`. It refuses a version that
      disagrees with `VERSION`, and it publishes a real release, so delete that
      release afterwards (`gh release delete vX.Y.Z`) or dispatch against a
      throwaway version.

### GitHub Release Page

- [ ] Push the annotated tag `vX.Y.Z` from `main`, then watch the run:
      `gh run watch $(gh run list --workflow=release.yml --limit=1 --json databaseId --jq '.[0].databaseId')`
- [ ] Confirm the release has, for **each** Linux architecture, a `.tar.gz`, a
      `.deb` and a `.rpm`, plus one AppImage (x86_64) — seven files — and, for the
      other two desktops, `time-warp-studio-<version>-win64.exe`,
      `…-win64.zip`, `…-win64-win7.exe`, `…-win64-win7.zip` and
      `Time_Warp_Studio-<version>-macOS-universal.dmg`; each with
      a `.sha256` beside it. Every Linux name must carry the architecture its
      format uses: `amd64`/`arm64` in a `.deb`, `x86_64`/`aarch64` in an `.rpm`.
      A `-Linux.rpm` means `CPACK_RPM_FILE_NAME` is not `RPM-DEFAULT`.
- [ ] Run the Windows installer and the macOS disk image once each: the installer
      should put **Time Warp Studio** in the Start Menu and start, and the dragged
      application should open, show the examples (File > Open Example) and the
      documentation (Help > Documentation) from inside the bundle.
- [ ] Confirm the container image is tagged `X.Y.Z` and `latest`, and that the
      same two tags are on `<owner>/time_warp_studio`
- [ ] Download the package for a distribution you have and install it before
      announcing: a release nobody installed is a release nobody knows works.
- [ ] Confirm the Deployment appears under Settings > Environments > `production`
      with the release as its URL
- [ ] Check the image runs before announcing it:
      `docker run --rm ghcr.io/<owner>/time-warp-studio:X.Y.Z --version`
- [ ] If `X.Y.Z` is a patch release of a release that never published an artifact,
      move the tag instead of bumping again: `git tag -f -a vX.Y.Z`, then
      `git push --force origin vX.Y.Z` — safe only while the old tag has no
      published release or image attached

## Post-Release Tasks

- [ ] Verify release downloads and checksums
- [ ] Run smoke test from packaged artifacts
- [ ] Update project links (README badges, documentation index)
- [ ] Announce in GitHub Discussions / mailing list
- [ ] Open milestone for next release
- [ ] Triage any immediate feedback or bug reports
