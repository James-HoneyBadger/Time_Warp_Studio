# Time Warp — Go (canonical)

**Canonical location:** `../../Time_Warp_Go/`

**Status:** Active (CLI + new Fyne GUI)

## Quick Start

### CLI Version

```bash
cd ../../Time_Warp_Go
go build ./cmd/timewarp
echo "PRINT \"Hello World\"" | ./timewarp --batch BASIC
```

### GUI Version (New!)

```bash
cd ../../Time_Warp_Go
go build -o timewarp-gui ./cmd/timewarp-gui
./timewarp-gui
```

## Features

- Fast CLI interpreter (99%+ test coverage)
- New Fyne-based GUI (v2.7.0)
- BASIC, PILOT, and Logo executors
- Batch processing mode
- Cross-platform support

## Migration Status

Active development in `Time_Warp_Go/` at repository root.

See `../../ARCHITECTURE_OVERVIEW.md` for migration timeline.
