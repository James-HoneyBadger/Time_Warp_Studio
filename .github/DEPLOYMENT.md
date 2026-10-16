# CI and Deployment

Time Warp Studio is a desktop application: there is no hosted service, and a
release is a tagged build rather than a deployment. This page lists the workflows
that exist and what they gate.

## Workflows

| Workflow | Trigger | What it does |
| -------- | ------- | ------------ |
| `native.yml` | Push and pull request to `main`, when anything under `native/`, `Examples/` or the bridge changes | Configures and builds the Qt application, refuses to pass if the widget UI was skipped, then runs the 11 CTest suites in Release, Debug, AddressSanitizer and UndefinedBehaviorSanitizer configurations, both differential gates (each asserting **0 unexpected mismatches**), and every example program through the real bridge. Also smoke-tests the entry point (`--version`, `--help`, a bad-file screenshot) and renders an image artifact. |
| `docs.yml` | Push and pull request to `main`, when files under `docs/` change | Checks that the core documentation files exist. |
| `markdown-lint.yml` | Push and pull request | Markdown lint and spell check. Both steps are `continue-on-error`. |

The Python application's workflows — `ci.yml`, `build_release.yml`,
`publish-python.yml` and `publish-container.yml` — were removed with it. They
linted, tested, packaged and published the PySide6 application, and none of that
applies to a C++ build.

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
