# Release Checklist - Time Warp IDE v4.0.0

## Pre-Release Tasks

### Version Updates

- [ ] Update product version to **4.0.0** in:
  - `Platforms/Python/pyproject.toml`
  - `Platforms/Python/time_warp/__init__.py`
  - `Platforms/Python/time_warp/ui/main_window.py`
  - `Platforms/Python/README.md`
  - `Platforms/Browser/package.json`, `index.html`, `js/app*.js`, `js/ui.js`
  - `Platforms/DOS/src/timewarp_dos.c`
  - `Scripts/install.sh`, `Scripts/install-user.sh`
  - Root `README.md` and key docs in `Docs/`

### Testing

- [ ] `python test_runner.py --basic`
- [ ] `python test_runner.py --comprehensive`
- [ ] Manual smoke test of Python IDE (launch, open example, run turtle program)
- [ ] Manual smoke test of Browser build (`Platforms/Browser/index.html`)
- [ ] Manual smoke test of DOS interpreter (verify banner shows v4.0.0)

### Documentation

- [ ] Update `Docs/misc/RELEASE_NOTES.md` with 4.0.0 entry
- [ ] Refresh documentation footers (FAQ, quick reference, index)
- [ ] Ensure screenshots or captions reference v4.0.0 where applicable
- [ ] Run spell check / link validation for key documents

## GitHub Preparation

### Repository

- [ ] Create release branch `release/v4.0.0`
- [ ] Merge final changes and ensure `main` is up to date
- [ ] Tag commit: `git tag -a v4.0.0 -m "Time Warp IDE v4.0.0"`
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
  tar -czf time-warp-examples-v4.0.0.tar.gz Examples/
  ```

- [ ] Generate checksums for uploaded files (optional)

### GitHub Release Page

- [ ] Create new release from tag `v4.0.0`
- [ ] Title: `Time Warp IDE v4.0.0`
- [ ] Paste highlights from `Docs/misc/RELEASE_NOTES.md`
- [ ] Attach wheel, source tarball, examples archive, and any supplemental assets
- [ ] Mark release as latest and publish

## Post-Release Tasks

- [ ] Verify release downloads and checksums
- [ ] Run smoke test from packaged artifacts
- [ ] Update project links (README badges, documentation index)
- [ ] Announce in GitHub Discussions / mailing list
- [ ] Open milestone for next release
- [ ] Triage any immediate feedback or bug reports
