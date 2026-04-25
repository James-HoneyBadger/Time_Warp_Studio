# Release Checklist - Time Warp Studio v10.0.0

## Pre-Release Tasks

### Version Updates

- [ ] Update product version to **10.0.0** in:
  - `Platforms/Python/time_warp/__init__.py`
  - `Platforms/Python/time_warp/ui/main_window.py`
  - `Scripts/startup.py`
  - `Scripts/build_native.sh`
  - `Scripts/install-user.sh`
  - `install.sh` (fallback echo)
  - `VERSION` (canonical)
  - Root `README.md` and key docs

### Testing

- [ ] `python test_runner.py --basic`
- [ ] `python test_runner.py --comprehensive`
- [ ] Manual smoke test of Python IDE (launch, open example, run turtle program)

### Documentation

- [ ] Update `CHANGELOG.md` with new version entry
  - [ ] Refresh documentation footers (FAQ, quick reference, index)
  - [ ] Ensure screenshots or captions reference v10.0.0 where applicable
- [ ] Run spell check / link validation for key documents

## GitHub Preparation

### Repository

- [ ] Create release branch `release/v10.0.0`
- [ ] Merge final changes and ensure `main` is up to date
- [ ] Tag commit: `git tag -a v10.0.0 -m "Time Warp Studio v10.0.0"`
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
  tar -czf time-warp-examples-v10.0.0.tar.gz Examples/
  ```

- [ ] Generate checksums for uploaded files (optional)

### GitHub Release Page

- [ ] Create new release from tag `v10.0.0`
- [ ] Title: `Time Warp Studio v10.0.0`
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
