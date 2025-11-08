# Platforms Overview

This directory provides a unified index of the various platform-specific implementations of Time Warp / TempleCode.

Legacy directory names are preserved at the repository root for backward compatibility (scripts, CI, external references). Future development should prefer organizing platform-specific code inside this folder.

## Mapping (Existing Root Folder → Canonical Name)

- `Time_Warp_Rust/`    -> `rust/` (Primary modern GUI + interpreter)
- `Time_Warp_Go/`      -> `go/` (Fast CLI interpreter)
- `Time_Warp_Python/`  -> `python/` (Tkinter GUI; archived)
- `Time_Warp_Web/`     -> `web/` (WebAssembly/JavaScript demo)
- `Time_Warp_Windows/` -> `windows/` (Launchers & packaging helpers)
- `Time_Warp_Apple/`   -> `apple/` (macOS launchers & packaging)
- `Time_Warp_DOS/`     -> `dos/` (DJGPP Mode 13h implementation)
- `Time_Warp_Haiku/`   -> `haiku/` (Haiku OS native app prototype)
- `Time_Warp_OS2/`     -> `os2/` (OS/2 experiments)
- `Time_Warp_Amiga/`   -> `amiga/` (Amiga experiments)
- `Time_Warp_Win2000/` -> `win2000/` (Windows 2000 compatibility work)

## Proposed Future Layout

Rather than physically moving existing folders immediately (risking breakage of relative paths and scripts), we will:

1. Add subfolders here gradually, starting with `rust/` and `go/` mirrors.
2. Introduce shim READMEs inside old root folders pointing to the new canonical location once migrated.
3. Update CI configurations to use `platforms/rust` etc.
4. After an adoption period, archive old top-level folder names under `legacy/`.

## Rationale

- Reduces root directory sprawl
- Clarifies primary vs experimental implementations
- Provides a migration path without breaking existing references

## Next Steps (tracked in TODOs)

- Migrate Rust implementation into `platforms/rust/` (phase 1)
- Migrate Go CLI into `platforms/go/` (phase 1)
- Add shared `core-spec/` for language documentation (phase 2)
- Deprecate duplicate LICENSE files and centralize licensing docs (phase 2)
