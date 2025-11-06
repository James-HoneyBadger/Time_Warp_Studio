# Time Warp (Rust)

A lightweight TempleCode IDE and interpreter written in Rust using eframe/egui. Includes a GUI editor/canvas and a headless CLI runner.

## Features

- In-process TempleCode interpreter (no Python subprocess)
- Turtle graphics rendered on an egui canvas
- Control flow: IF/THEN, REPEAT/ENDREPEAT, FOR/NEXT, WHILE/ENDWHILE
- Procedures with parameters: PROC/CALL/RETURN/ENDPROC
- Helpers: sin, cos, int, abs, str, randint, `pi`
- Debugging: TRACE, DUMPVARS, ASSERT, PAUSE
- Database commands: stubs by default; optional MySQL (feature-gated)
- PNG export: `EXPORTPNG "path.png"` rasterizes the canvas (Bresenham)
- GUI conveniences: examples loader, Run (Ctrl+R), Save PNG (Ctrl+S), top menu, toast on save

## Build and Run

GUI app:

```bash
cd Rust
cargo run
```

Headless CLI runner:

```bash
cd Rust
cargo run --bin tc_run ../examples/proc_params.tc
```

MySQL support (optional):

```bash
cd Rust
cargo run --features mysql --bin tc_run ../examples/mysql_demo_rust.tc
```

## Shortcuts

- Ctrl+R — Run program
- Ctrl+S — Save PNG (defaults to `<project_root>/exports` and remembers last save dir)

## PNG export

In code:

```text
EXPORTPNG "out.png"
```

From GUI: use File > Save PNG… or toolbar button; a brief toast confirms after saving.

## Tests

Run smoke tests:

```bash
cd Rust
cargo test
```

## Notes

- The ashpd dependency emits a future-compatibility note; current builds are unaffected.
- The CLI reports `[lines_drawn=N]` on stderr for quick verification.
