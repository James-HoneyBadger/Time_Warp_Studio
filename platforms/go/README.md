# Time Warp (Go)

Go edition of Time Warp IDE — includes a CLI interpreter and a Fyne GUI with turtle graphics.

Status: functional with a growing command set:

- BASIC-like: `PRINT <text>`, `LET x = 1`, `FOR/NEXT`, `IF/THEN`, assignments
- Logo-like: `FORWARD|FD <n>`, `BACK|BK <n>`, `RIGHT/LEFT <deg>`, `SETXY x y`, `SETHEADING <deg>`, `PENUP/PENDOWN`, `CLEAR/CS`, `SETCOLOR r g b`, `PENWIDTH n`
- PILOT-like: `T:<text>`, `A:<var>`, plus basic flow helpers

## Build and run

```bash
cd platforms/go

# CLI
go run ./cmd/timewarp ECHO Hello
go run ./cmd/timewarp "PRINT \"Hello from BASIC\""

# GUI
go build ./cmd/timewarp-gui
./timewarp-gui
```

### Turtle export

From the GUI, use File → Export Turtle Image… to save a PNG of the current canvas.

Options in the dialog:

- Preset sizes: 400×400, 800×800, 1024×1024, 1600×1600 (or enter custom width/height, 1–8192)
- Transparent background toggle (disables color selection when enabled)
- Background color picker (#RRGGBB displayed on the button)
- Include turtle indicator (draws the turtle circle + heading line)

Rendering details:

- Lines and pen widths are scaled to the chosen size with dampening and clamps to keep visuals consistent.
- A timestamped copy is also written to /tmp for convenience.

## Tests

```bash
cd platforms/go
go test ./...
```

### Benchmarks

We include a couple of micro-benchmarks for the interpreter:

```bash
go test -run TestNonExistent -bench . -benchmem ./pkg/timewarp
```

This runs without executing regular tests and reports allocations.

## Notes

```bash
cd platforms/go

