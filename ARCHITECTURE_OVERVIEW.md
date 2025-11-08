# Architecture Overview (Repository Consolidation Proposal)

This document outlines a proposed consolidation of the Time Warp / TempleCode repository structure to improve clarity, discoverability, and long-term maintainability.

## Current State (High-Level)

- Multiple platform-specific implementations at the root: `Time_Warp_Rust`, `Time_Warp_Go`, `Time_Warp_Python`, `Time_Warp_Web`, `Time_Warp_DOS`, `Time_Warp_Apple`, `Time_Warp_Windows`, plus experimental (`Time_Warp_Haiku`, `Time_Warp_OS2`, `Time_Warp_Amiga`, `Time_Warp_Win2000`).
- Mixed technology stacks (Rust, Go, Python, C/C++, JavaScript, Haiku C++).
- Duplicate licensing artifacts: `LICENSE` (MIT) and `LICENSE.txt` (Apache 2.0).
- Root contains a large number of meta-docs and installation guides.

## Guiding Principles

1. Prefer a single canonical location per platform implementation.
2. Separate core language specification and shared examples from platform-specific code.
3. Preserve backward compatibility temporarily via legacy folder names & shim README notices.
4. Minimize disruptive renames until CI/test scripts are updated.
5. Centralize documentation and licensing references.

## Target Layout (Phased)

```text
platforms/
  rust/          (migrated from Time_Warp_Rust/)
  go/            (migrated from Time_Warp_Go/)
  python/        (archived; minimal maintenance)
  web/
  dos/
  apple/
  windows/
  haiku/         (prototype)
  amiga/         (experimental)
  os2/           (experimental)
  win2000/       (experimental)
core-spec/
  language.md    (unified language reference & grammar)
  turtle.md      (turtle graphics command spec)
  executors.md   (execution model and emoji conventions)
examples/        (shared programs, cross-platform)
docs/            (guides, install, rebranding, feature matrix, etc.)
legacy/          (original folders retained during migration window)
scripts/         (build/test utilities)
packaging/       (release, installers, CI-related assets)
fastlane/        (mobile/mac pipelines if retained)
LICENSE          (primary chosen license)
LICENSES/        (additional or historical licenses + rationale)
ARCHITECTURE_OVERVIEW.md
README.md        (top-level orientation + quick start)
```

## Migration Plan

- Phase 1: Introduce `platforms/` and start mirroring Rust & Go folders there without deleting originals.
- Phase 2: Add `core-spec/` with unified docs extracted from scattered README fragments.
- Phase 3: Update CI workflows to reference new paths; add shim README notes in old folders.
- Phase 4: Move remaining platform folders; archive originals under `legacy/`.
- Phase 5: Consolidate licensing (choose MIT or Apache as canonical; document rationale in `LICENSES/`).

## Risk Mitigation

- Avoid breaking relative example paths (e.g., Rust referencing `../examples`). Update code after mirror is validated.
- Keep tests green in each phase before proceeding.
- Document each step in CHANGELOG or a MIGRATION log.

## Open Questions

- Which license variant should be canonical? Currently MIT and Apache 2.0 coexist.
- Should Python implementation remain active or be fully archived?
- How much of experimental OS targets should be retained vs archived?

## Next Actions

See TODO list in the assistant workflow. Pending user confirmation before physical moves.
