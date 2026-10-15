# Release Checklist - Time Warp Studio vX.Y.Z

## Pre-Release Tasks

### Version Updates

- [ ] Set `VERSION` to the new release number
- [ ] Run `python3 Scripts/version_sync.py --write --release-date YYYY-MM-DD`
- [ ] Update versioned release text in docs that are intentionally human-authored

### Testing

- [ ] `python3 Platforms/Python/smoke_test.py`
- [ ] `PYTHONPATH=Platforms/Python pytest Platforms/Python/time_warp/tests -q`
- [ ] Manual smoke test of Python IDE (launch, open example, run turtle program)

### Documentation

- [ ] Update `CHANGELOG.md` with new version entry
- [ ] Refresh documentation footers (FAQ.md, docs/INDEX.md)
- [ ] Ensure screenshots or captions reference the new release where applicable
- [ ] Run spell check / link validation for key documents

## GitHub Preparation

### Repository

- [ ] Create release branch `release/vX.Y.Z`
- [ ] Merge final changes and ensure `main` is up to date
- [ ] Tag commit: `git tag -a vX.Y.Z -m "Time Warp Studio vX.Y.Z"`
- [ ] Push branch and tag to origin

### Release Assets

- [ ] Build Python distributions
  ```bash
  cd Platforms/Python
  python -m build
  ```

- [ ] Package refreshed examples (optional)
  ```bash
  tar -czf time-warp-examples-vX.Y.Z.tar.gz Examples/
  ```

- [ ] Generate checksums for uploaded files (optional)

### GitHub Release Page

- [ ] Push the annotated tag `vX.Y.Z` from `main`; confirm the production deployment workflow succeeds
- [ ] Verify the automatically created `Time Warp Studio vX.Y.Z` release is marked latest
- [ ] Verify the Linux amd64 package and tarball, optional arm64 assets, Python wheel and source tarball, and `SHA256SUMS.txt` are attached
- [ ] Confirm release notes match the `CHANGELOG.md` section for `X.Y.Z`

## Post-Release Tasks

- [ ] Verify release downloads and checksums
- [ ] Run smoke test from packaged artifacts
- [ ] Update project links (README badges, documentation index)
- [ ] Announce in GitHub Discussions / mailing list
- [ ] Open milestone for next release
- [ ] Triage any immediate feedback or bug reports
