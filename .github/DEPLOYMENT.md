# CI and Release Workflows

Time Warp Studio is a desktop application. There is no server deployment and no container image. The repository has four GitHub Actions workflows in `.github/workflows/`.

## Workflows

| Workflow | Trigger | What it does |
|----------|---------|--------------|
| `ci.yml` | Push and pull request to `main` | Runs `ruff check` on `Platforms/Python`, `pytest` on `Platforms/Python/time_warp/tests`, `Platforms/Python/smoke_test.py`, and `test_all_examples.py`. Matrix: Python 3.10, 3.12, 3.13. |
| `build_release.yml` | Tags matching `v*`, or manual dispatch | Builds native binaries with `Scripts/build_native.sh` (PyInstaller) for Linux x86_64 and arm64. The arm64 job is `continue-on-error`. Binaries are uploaded as workflow artifacts. It does not create a GitHub Release. |
| `docs.yml` | Push and pull request to `main` when files under `docs/` change | Checks that the core documentation files exist. |
| `markdown-lint.yml` | Push and pull request | Markdown lint and spell check. Both steps are `continue-on-error`. |

## Release Procedure

The release steps are manual. The full checklist is in [RELEASE_CHECKLIST.md](RELEASE_CHECKLIST.md).

1. Set `VERSION` and run `python3 Scripts/version_sync.py --write --release-date YYYY-MM-DD`.
2. Add the release entry to `CHANGELOG.md`.
3. Run `make test` and `python3 Platforms/Python/smoke_test.py`.
4. Merge to `main` and confirm `ci.yml` passes.
5. Create and push an annotated tag:

   ```bash
   git tag -a vX.Y.Z -m "Time Warp Studio vX.Y.Z"
   git push origin vX.Y.Z
   ```

6. `build_release.yml` runs on the tag. Download the binaries from the workflow run's artifacts.
7. Create the GitHub Release for the tag manually and attach the binaries.
