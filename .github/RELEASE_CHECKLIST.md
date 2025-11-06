# Release Checklist - Time Warp IDE v2.0.0

## Pre-Release Tasks

### Code Preparation

- [x] Update version to 2.0.0 in `Time_Warp_Rust/Cargo.toml`
- [x] Update version to 2.0.0 in `Time_Warp_Python/setup.py`
- [x] Update version in Python About dialog
- [x] Verify all tests pass (Python: pytest, Rust: cargo test)
- [x] Run code formatters (Python: black, Rust: cargo fmt)
- [x] Run linters (Python: pylint/flake8, Rust: cargo clippy)
- [x] Check for compiler warnings
- [x] Remove debug code and print statements

### Documentation

- [x] Create CHANGELOG.md with complete v2.0.0 entry
- [x] Update main README.md with version badges
- [x] Create/update CONTRIBUTING.md
- [x] Create/update CODE_OF_CONDUCT.md
- [x] Create/update SECURITY.md
- [x] Write RELEASE_NOTES_v2.0.0.md
- [x] Update USER_GUIDE.md
- [x] Verify all doc links work
- [x] Check for typos and formatting issues
- [x] Ensure consistent terminology across docs

### Folder Organization

- [x] Move outdated Python status docs to archive/
- [x] Clean up duplicate files
- [x] Verify examples/ folder organized
- [x] Ensure consistent naming conventions
- [x] Remove temporary/test files

### Testing

- [ ] Run full Python test suite
  ```bash
  cd Time_Warp_Python
  pytest tests/ -v --cov=time_warp
  ```

- [ ] Run full Rust test suite
  ```bash
  cd Time_Warp_Rust
  cargo test --all-features
  ```

- [ ] Test Python IDE startup and basic operations
- [ ] Test Rust IDE startup and basic operations
- [ ] Run all 33 example programs in both implementations
- [ ] Test on multiple platforms (Linux, macOS, Windows)
- [ ] Verify theme switching works
- [ ] Test file open/save functionality
- [ ] Test graphics canvas zoom/pan
- [ ] Test keyboard shortcuts

### Examples Verification

- [ ] Verify all 33 examples run without errors
- [ ] Check example categorization is correct
- [ ] Ensure README.md in examples/ is up to date
- [ ] Test beginner examples for clarity
- [ ] Test advanced examples for complexity
- [ ] Verify all examples demonstrate features

## GitHub Preparation

### Repository Setup

- [ ] Create release branch: `release/v2.0.0`
- [ ] Tag commit: `git tag -a v2.0.0 -m "Release version 2.0.0"`
- [ ] Push tags: `git push origin v2.0.0`
- [ ] Verify GitHub Actions pass (if configured)
- [ ] Check branch protection rules
- [ ] Ensure main branch is up to date

### Release Assets

- [ ] Build Python source distribution
  ```bash
  cd Time_Warp_Python
  python setup.py sdist
  ```

- [ ] Build Rust release binaries
  ```bash
  cd Time_Warp_Rust
  cargo build --release
  # Linux binary: target/release/time-warp
  # Create tar.gz: tar -czf time-warp-linux-x86_64.tar.gz -C target/release time-warp
  ```

- [ ] Create Windows binary (if applicable)
- [ ] Create macOS binary (if applicable)
- [ ] Package examples separately
  ```bash
  tar -czf time-warp-examples-v2.0.0.tar.gz examples/
  ```

- [ ] Create documentation archive
  ```bash
  tar -czf time-warp-docs-v2.0.0.tar.gz Time_Warp_Rust/docs/ Time_Warp_Python/docs/ *.md USER_GUIDE.md
  ```

### GitHub Release Page

- [ ] Create new release on GitHub
- [ ] Use tag v2.0.0
- [ ] Title: "Time Warp IDE v2.0.0 - Feature Complete Release"
- [ ] Copy RELEASE_NOTES_v2.0.0.md content to release description
- [ ] Upload release assets:
  - [ ] Python source distribution (.tar.gz)
  - [ ] Rust Linux binary (.tar.gz)
  - [ ] Rust Windows binary (.zip) - if available
  - [ ] Rust macOS binary (.tar.gz) - if available
  - [ ] Examples archive (.tar.gz)
  - [ ] Documentation archive (.tar.gz)
  - [ ] CHANGELOG.md
- [ ] Mark as latest release
- [ ] Publish release

## Post-Release Tasks

### Verification

- [ ] Verify release appears on GitHub
- [ ] Test download links work
- [ ] Extract and test release binaries
- [ ] Verify version numbers display correctly
- [ ] Check release appears in GitHub Releases page

### Communication

- [ ] Update project README.md links to point to v2.0.0
- [ ] Create announcement (GitHub Discussions)
- [ ] Update project website (if applicable)
- [ ] Share on social media (optional)
- [ ] Notify contributors
- [ ] Update any external documentation

### Next Steps

- [ ] Create milestone for v2.1.0 or v3.0.0
- [ ] Move incomplete tasks to new milestone
- [ ] Plan next development phase
- [ ] Review and address any feedback
- [ ] Monitor for bug reports
- [ ] Prepare hotfix process if needed

## Platform-Specific Builds

### Linux

```bash
cd Time_Warp_Rust
cargo build --release
strip target/release/time-warp
tar -czf time-warp-v2.0.0-linux-x86_64.tar.gz \
  -C target/release time-warp \
  -C ../../ README.md CHANGELOG.md LICENSE USER_GUIDE.md
```

### Windows (Cross-compile or native)

```bash
# On Linux with cross-compiler
cargo build --release --target x86_64-pc-windows-gnu
# Or build on Windows natively
cargo build --release
```

### macOS (Native build)

```bash
cargo build --release --target x86_64-apple-darwin
# Or for Apple Silicon
cargo build --release --target aarch64-apple-darwin
```

## Version Control

```bash
# Create release branch
git checkout -b release/v2.0.0

# Make final commits
git add .
git commit -m "chore: Prepare v2.0.0 release"

# Tag the release
git tag -a v2.0.0 -m "Time Warp IDE v2.0.0 - Feature Complete Release"

# Push to GitHub
git push origin release/v2.0.0
git push origin v2.0.0

# Merge to main
git checkout main
git merge release/v2.0.0
git push origin main
```

## Quality Checks

- [ ] All documentation has consistent formatting
- [ ] All code follows project style guidelines
- [ ] No TODO or FIXME comments in release code
- [ ] All deprecation warnings addressed
- [ ] Security audit completed
- [ ] License information accurate
- [ ] Copyright years updated
- [ ] Author attribution correct

## Installation Verification

### Python

```bash
# Fresh install test
python3 -m venv test_venv
source test_venv/bin/activate
pip install PySide6 pillow
python Time_Warp_Python/time_warp_ide.py
# Verify: Version shows 2.0.0
deactivate
rm -rf test_venv
```

### Rust

```bash
# Fresh build test
cd Time_Warp_Rust
cargo clean
cargo build --release
./target/release/time-warp
# Verify: Version shows 2.0.0
```

## Documentation Review

- [ ] README.md is clear and accurate
- [ ] USER_GUIDE.md has complete installation instructions
- [ ] CONTRIBUTING.md explains contribution process
- [ ] CHANGELOG.md has all changes documented
- [ ] All cross-references between docs work
- [ ] No broken links
- [ ] Screenshots are up-to-date (if any)
- [ ] Code examples are tested and work

## Final Sign-Off

- [ ] Project maintainer approval
- [ ] All checklist items completed
- [ ] Release notes reviewed
- [ ] Assets uploaded
- [ ] Version tagged
- [ ] Release published

## Emergency Rollback Plan

If critical issues are discovered post-release:

1. **Document the issue** - Create GitHub issue with details
2. **Assess severity** - Determine if hotfix or next version
3. **Communicate** - Update release notes with known issues
4. **Prepare fix** - Create hotfix branch (v2.0.1)
5. **Test thoroughly** - Don't rush the fix
6. **Release hotfix** - Follow abbreviated checklist

---

**Release Manager**: James Temple  
**Release Date**: October 28, 2025  
**Target Version**: 2.0.0  
**Status**: In Progress

**Last Updated**: October 28, 2025
