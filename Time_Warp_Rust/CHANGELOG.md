# Changelog

All notable changes to this project will be documented in this file.

## [3.0.0] - 2025-10-06

### Interpreter & Language Core

- Conditional jump semantics stabilized: `J:` consumes sentinel after `Y:` / `N:`.
- Extended compute command: `C:VAR=EXPR` assigns evaluated expression.
- Safer variable interpolation (word-boundary + `*VAR*`).
- Nested `REPEAT` implementation with guarded expansion.
- Macro system: `DEFINE NAME [ ... ]` + `CALL NAME` (recursion protection & depth limit).
- Performance profiling (`PROFILE ON|OFF|RESET|REPORT`).

### Turtle / Logo Enhancements

- Centered canvas coordinate system with auto-pan & scroll expansion.
- Auto color cycling per new drawn shape (pen-up → pen-down transition).
- New commands: `COLOR`, `TRACE`, `KEEP_CANVAS`, `CENTER`, `PENSTYLE`, `DEBUGLINES`, `FIT`, aliases (`SETCOLOR/SETCOLOUR`, `SETPENSIZE`).
- Pen style customization (`PENSTYLE solid|dashed|dotted`).
- Start-of-shape markers for orientation.
- Line metadata capture (coords, width, color, style) accessible via `DEBUGLINES`.

### UI / IDE Improvements

- Output panel context menu: Copy / Copy All / Clear.
- Turtle menu: trace & canvas preservation toggles, manual clear.
- Theme accent & dark/light switching.
- Extended syntax highlighting & auto-complete for new Logo & profiler commands.
- Reduced duplicate completion logs.

### Debug & Diagnostics

- TRACE mode outputs movement, heading, pen state after Logo actions.
- `PROFILE REPORT` provides per-command counts, avg, max, total time.
- `DEBUGLINES` surfaces early geometry metadata for first N segments.

### Quality & Housekeeping

- Refactored turtle initialization guard to avoid accidental re-init / wiping lines mid-run.
- Added defensive background redraw (`draw_turtle_background`) to eliminate missing attribute warnings.
- Scrollregion and viewport logic reduce user confusion about off-screen shapes.

### Known Limitations / Deferred

- No polygon fill / export-to-image command (planned >3.0.0).
- Expression evaluator still uses constrained `eval` (plan hardening pass).
- No persisted user settings yet (trace, keep-canvas, theme, profiling preference).
- No ZOOM / BOUNDS commands yet (only FIT / CENTER).

## [Unreleased]

- (Reserved for upcoming patches after 3.0.0 release.)

## Previous

- Initial project files.
