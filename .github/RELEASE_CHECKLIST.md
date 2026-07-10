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
- [ ] Refresh documentation footers (FAQ, quick reference, index)
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

- [ ] Collect artifacts from `Platforms/Browser` if publishing web bundle
- [ ] Package refreshed examples (optional)
  ```bash
  tar -czf time-warp-examples-vX.Y.Z.tar.gz Examples/
  ```

- [ ] Generate checksums for uploaded files (optional)

### GitHub Release Page

- [ ] Create new release from tag `vX.Y.Z`
- [ ] Title: `Time Warp Studio vX.Y.Z`
- [ ] Paste highlights from `CHANGELOG.md`
- [ ] Attach wheel, source tarball, examples archive, and any supplemental assets
- [ ] Mark release as latest and publish

## Post-Release Tasks

- [ ] Verify release downloads and checksums
- [ ] Run smoke test from packaged artifacts
- [ ] Update project links (README badges, documentation index)
- [ ] Announce in GitHub Discussions / mailing list
- [ ] Open milestone for next release
- [ ] Triage any immediate feedback or bug reports
