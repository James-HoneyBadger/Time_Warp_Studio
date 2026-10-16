# Release Checklist - Time Warp Studio vX.Y.Z

## Pre-Release Tasks

### Version Updates

- [ ] Set `VERSION` to the new release number
- [ ] Run `python3 Scripts/version_sync.py --write --release-date YYYY-MM-DD`
- [ ] Update versioned release text in docs that are intentionally human-authored

### Testing

- [ ] `make test` — the 11 C++ suites
- [ ] `make test-debug` and `make asan` — the other build trees
- [ ] `make engine-test` — the bridge engine's pytest suite
- [ ] `make smoke` — all 44 example programs through the real bridge
- [ ] Both differential gates report 0 unexpected mismatches
- [ ] Manual smoke test of the IDE (launch, open an example, run a turtle program)

### Documentation

- [ ] Update `CHANGELOG.md` with new version entry
- [ ] Refresh documentation footers (docs/FAQ.md, docs/INDEX.md)
- [ ] Ensure screenshots or captions reference the new release where applicable
- [ ] Run spell check / link validation for key documents

## GitHub Preparation

### Repository

- [ ] Create release branch `release/vX.Y.Z`
- [ ] Merge final changes and ensure `main` is up to date
- [ ] Tag commit: `git tag -a vX.Y.Z -m "Time Warp Studio vX.Y.Z"`
- [ ] Push branch and tag to origin

### Release Assets

- [ ] Build the application
  ```bash
  cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
  cmake --build native/build -j
  ```
  > Installer packaging (Linux packages, Windows installers) is not built yet;
  > see `docs/ROADMAP.md`. Until it is, a release ships sources plus the
  > binary built above.

- [ ] Package the examples (optional)
  ```bash
  tar -czf time-warp-examples-vX.Y.Z.tar.gz Examples/
  ```

- [ ] Generate checksums for uploaded files (optional)

### GitHub Release Page

- [ ] Push the annotated tag `vX.Y.Z` from `main`; confirm the deployment workflow succeeds
- [ ] Verify the automatically created `Time Warp Studio vX.Y.Z` release is marked latest
- [ ] Confirm release notes match the `CHANGELOG.md` section for `X.Y.Z`

## Post-Release Tasks

- [ ] Verify release downloads and checksums
- [ ] Run smoke test from packaged artifacts
- [ ] Update project links (README badges, documentation index)
- [ ] Announce in GitHub Discussions / mailing list
- [ ] Open milestone for next release
- [ ] Triage any immediate feedback or bug reports
