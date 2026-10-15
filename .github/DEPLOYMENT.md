# CI and Release Workflows

Time Warp Studio is a desktop application, so its deployment channels publish downloadable builds rather than a hosted web service. GitHub Environments record the preview, staging, and production deployments and link to their build or release.

## Workflows

| Workflow | Trigger | What it does |
| -------- | ------- | ------------ |
| `ci.yml` | Push and pull request to `main` | Runs `ruff check` on `Platforms/Python`, `pytest` on `Platforms/Python/time_warp/tests`, `Platforms/Python/smoke_test.py`, and `test_all_examples.py`. Matrix: Python 3.10, 3.12, 3.13. |
| `build_release.yml` | Pull requests to `main`, version tags matching `v*`, or manual dispatch | Pull requests publish a short-lived x86_64 preview artifact. Manual dispatch can target preview or staging; staging is restricted to `main`. Version tags target production, build Linux x86_64 and arm64 (arm64 remains best-effort), build Python distributions, and publish a GitHub Release with checksums. |
| `publish-python.yml` | GitHub Release published for a `v*` tag | Builds and publishes the Python wheel and source distribution to PyPI using trusted publishing (OIDC). |
| `publish-container.yml` | Push of a `v*` tag | Builds and publishes an x86_64 desktop-app image to GHCR with the version tag and `latest`. |
| `docs.yml` | Push and pull request to `main` when files under `docs/` change | Checks that the core documentation files exist. |
| `markdown-lint.yml` | Push and pull request | Markdown lint and spell check. Both steps are `continue-on-error`. |

## Deployment Channels

The GitHub Environments `preview`, `staging`, and `production` are configured in repository settings. Preview is available for pull requests and manual runs. Staging is restricted to `main` and is started manually. Production is restricted to `v*` tags and publishes the GitHub Release automatically after the native build.

1. For a pull request preview, open a pull request targeting `main`. Download the x86_64 package from the `Build Native Binaries` workflow run; the artifact is retained for 30 days.
2. For staging, open **Actions → Build Native Binaries → Run workflow**, select `staging`, and run it from `main`. The Linux artifacts are attached to the workflow run for 30 days.
3. For a manual preview, dispatch the same workflow with `preview`; the selected branch must not require secrets because the job builds untrusted code without them.
4. For production, update `VERSION`, synchronize package metadata, add a release entry to `CHANGELOG.md`, run the release checks, and push an annotated `vX.Y.Z` tag from `main`:

   ```bash
   git tag -a vX.Y.Z -m "Time Warp Studio vX.Y.Z"
   git push origin vX.Y.Z
   ```

5. `build_release.yml` builds native and Python artifacts, extracts the matching changelog section, generates `SHA256SUMS.txt`, and publishes the latest GitHub Release. The arm64 build is best-effort; x86_64 is required.
6. Publishing that release starts `publish-python.yml`; a successful tag push also starts `publish-container.yml`.

## Package Registry Setup

### PyPI

Configure PyPI trusted publishing once; no API token secret is needed:

1. On PyPI, add a trusted publisher for owner `James-HoneyBadger`, repository `Time_Warp_Studio`, workflow `publish-python.yml`, and environment `pypi`.
2. For a package not yet created on PyPI, configure these values as a pending publisher first. For an existing project, add the publisher in its project settings.
3. The GitHub `pypi` environment permits only `main` and `v*` tags. Each published version tag then builds and uploads both Python distributions.

### GitHub Container Registry

The container workflow publishes `ghcr.io/james-honeybadger/time-warp-studio:vX.Y.Z` and `:latest` using the repository's `GITHUB_TOKEN`. No additional secret is needed. Set the package visibility to public in the repository's **Packages** settings if downloads should be public.

The image is a Linux desktop application, not a headless service. On an X11 Linux desktop, allow local root access to the display, run the image with the X11 socket mounted, then revoke that access:

```bash
xhost +si:localuser:root
docker run --rm -it \
   -e DISPLAY="$DISPLAY" \
   -v /tmp/.X11-unix:/tmp/.X11-unix:rw \
   -v time-warp-config:/root/.config \
   ghcr.io/james-honeybadger/time-warp-studio:latest
xhost -si:localuser:root
```

The complete release checklist is in [RELEASE_CHECKLIST.md](RELEASE_CHECKLIST.md). These channels publish desktop artifacts and do not host or deploy a web application.
