# Time Warp (Go)

Go edition of Time Warp IDE — includes a CLI interpreter and a Fyne GUI with turtle graphics.

Status: functional with a growing command set:

- BASIC-like: `PRINT <text>`, `LET x = 1`, `FOR/NEXT`, `IF/THEN`, assignments
- Logo-like: `FORWARD|FD <n>`, `BACK|BK <n>`, `RIGHT/LEFT <deg>`, `SETXY x y`, `SETHEADING <deg>`, `PENUP/PENDOWN`, `CLEAR/CS`, `SETCOLOR r g b`, `PENWIDTH n`
- PILOT-like: `T:<text>`, `A:<var>`, plus basic flow helpers

## Build and run

```bash
cd Time_Warp_Go
# CLI
go run ./cmd/timewarp ECHO Hello
go run ./cmd/timewarp "PRINT \"Hello from BASIC\""

# GUI
go build ./cmd/timewarp-gui
./timewarp-gui
```

## Tests

```bash
cd Time_Warp_Go
go test ./...
```

### Benchmarks

We include a couple of micro-benchmarks for the interpreter:

```bash
go test -run TestNonExistent -bench . -benchmem ./pkg/timewarp
```

This runs without executing regular tests and reports allocations.

## Notes

- Executors return text with emoji prefixes; the GUI is stateless relative to executors.
- Logo turtle rendering now emits structured turtle events used by the GUI (with text parsing fallback for compatibility).
