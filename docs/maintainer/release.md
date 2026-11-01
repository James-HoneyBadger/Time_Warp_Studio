# Releasing

A release is a tag. Pushing an annotated tag `vX.Y.Z` runs `release.yml`, which builds and
tests the tree on two architectures, packages it four ways, publishes the GitHub Release
and pushes the container image. Nothing is built by hand and nothing is uploaded by hand.

The workflow reference is [`.github/DEPLOYMENT.md`](../../.github/DEPLOYMENT.md): what each
workflow checks and why, the repository settings the pipeline depends on, and the pinned
actions. This page is the procedure and the artifacts.

---

## The version

One number lives in four places, and `time_warp_docs version` is what keeps them honest:

| Where | What it is |
| --- | --- |
| `VERSION` | The number itself; everything else is derived from it or checked against it |
| `README.md` | The badge |
| `docs/maintainer/architecture.md` | The title line |
| `docs/project/history.md` | A section headed `## [X.Y.Z] - YYYY-MM-DD` |

```bash
./native/tools/docs/build/time_warp_docs version          # check
./native/tools/docs/build/time_warp_docs version --write  # rewrite the copies
```

The check runs in `version.yml` on every change to `VERSION`, the README, the history or the
tool's own source, and again inside `release.yml`, which refuses a tag that disagrees with
`VERSION`. `--write` changes only those copies: prose that names a version by hand is
prose, and somebody has to read it.

---

## Before the tag

```bash
make test                 # the 24 C++ suites
make test-debug           # the same in Debug
make asan                 # AddressSanitizer; ASAN=undefined for UBSan
make golden-check         # the ports against the frozen corpus, 634 programs
make smoke                # 67 example programs through the native engines
make command-matrix       # every command row as a program
make repl-matrix          # every command row through the REPL
./native/build/time_warp_teach validate
make lint-md doc-links doc-claims tree-check registry-check
```

Then the one thing no gate replaces: launch the application, open an example, run a
drawing program, and look at it.

Two checks are easy to forget because they are not in `make test`: the lesson plans are
**generated**, so `time_warp_teach plans --write docs/lessons/plans` followed by
`git diff --exit-code --stat docs/lessons/plans` must be clean, and a language added since
the last release needs a row in `native/share/command_rows.json`, an example and a tutorial
before the reference is complete.

---

## The procedure

```bash
# 1. Move the version
$EDITOR VERSION                                  # X.Y.Z
$EDITOR docs/project/history.md                  # a new ## [X.Y.Z] - YYYY-MM-DD section
./native/tools/docs/build/time_warp_docs version --write
./native/tools/docs/build/time_warp_docs version  # exits 0

# 2. Land it on main, with the gates green
git commit -am "Release X.Y.Z"
git push

# 3. Tag the commit the release is built from
git tag -a vX.Y.Z -m "Time Warp Studio vX.Y.Z"
git push origin vX.Y.Z
```

The tag starts the pipeline. A release branch is optional and this project does not use
one: the tag names a commit on `main`, and `release.yml` builds exactly that commit.

To exercise the whole pipeline without spending a version, dispatch it:

```bash
gh workflow run release.yml -f version=X.Y.Z
```

That publishes a real release and a real image, so dispatch against a throwaway version and
delete both afterwards (`gh release delete vX.Y.Z`, and delete the image version from the
package page).

---

## What a release contains

| Artifact | Where | What it is |
| --- | --- | --- |
| `time-warp-studio-<version>-linux-<arch>.tar.gz` | Release assets | The staged `cmake --install` tree, the example programs and the licence. Unpack into `/` and run `time_warp_ide` |
| `time-warp-studio_<version>_<arch>.deb`, `time-warp-studio-<version>-1.<arch>.rpm` | Release assets | CPack packages over the same install rules, declaring Qt 6, SQLite and zlib as dependencies, for x86_64 and arm64 |
| `Time_Warp_Studio-<version>-x86_64.AppImage` | Release assets | One file, no root, Qt bundled by `linuxdeploy`. x86_64 only: the ARM build of the bundling tool is not exercised by this pipeline, and an artifact nobody has run is not one to publish |
| `Time_Warp_Studio-<version>-x86_64.flatpak` | Release assets | Built against the KDE runtime from `packaging/flatpak/`, so it carries its own Qt and is sandboxed. The desktop entry, icon and AppStream file are named after the application id inside the sandbox, and the application reads `FLATPAK_ID` to associate its window with the entry |
| `ghcr.io/<owner>/time-warp-studio:<version>` and `:latest` | GitHub Packages | Two-stage build from `packaging/docker/Dockerfile` on Ubuntu 24.04, with Qt 6 in the runtime layer, no interpreter of any kind, `alsa-utils` so `SOUND` can reach a device if one is mounted, and an `ldd` check that fails the build rather than the user if a library is missing |

Every asset carries a `.sha256` beside it. The container tags are manifest lists for
`linux/amd64` and `linux/arm64`, and the image runs as a non-root `studio` user with uid
1000, so a bind mount from a desktop user's directory is writable to it.

The local equivalents of the last two:

```bash
make -C native install PREFIX=$HOME/.local          # the tarball's layout
cpack --config native/build/CPackConfig.cmake -G DEB
```

`packaging/` holds the Flatpak manifest and the Dockerfile, and `release.yml` builds both,
so neither is a file that is only reviewed.

The release job runs in the `production` environment, which is what makes GitHub record a
Deployment with the release page as its URL. Nothing is deployed anywhere by it: the record
is what says which commit published which version, which is the question a Deployment is
actually asked.

---

## After the tag

- Confirm the release carries both architectures of the `.deb`, `.rpm` and tarball, the
  AppImage, the Flatpak and the image, and that every `.sha256` matches.
- Install the package for a distribution you have, and run it. A package that installs is
  not the same claim as an application that starts.
- Run the image before announcing it:

  ```bash
  docker run --rm -e QT_QPA_PLATFORM=offscreen -v "$PWD:/work" \
    ghcr.io/james-honeybadger/time-warp-studio:X.Y.Z \
    --run --export /work/out.png /work/Examples/logo/mandala.logo
  ```

- Check the [changelog section](../project/history.md) reads as a description of the
  release, because that is what the release page links to.

---

## Repository settings the pipeline needs

All three are one-time and none of them is in a file:

1. **Settings > Actions > General > Workflow permissions** — read and write, or the default
   plus the `permissions:` block in `release.yml`, which asks for `contents: write` to
   create the release and `packages: write` to push the image. A repository restricted to
   read-only tokens refuses the push with a 403 on the first run.
2. **Settings > Environments** — a `production` environment. The first run creates it; add
   it by hand only to attach protection rules such as required reviewers first.
3. **Package visibility** — a package pushed by `GITHUB_TOKEN` starts **private** even when
   the repository is public, and the first pull then fails with `unauthorized` for
   everybody. Set it public once, from the package page or with
   `gh api --method PATCH /user/packages/container/time-warp-studio -f visibility=public`.
