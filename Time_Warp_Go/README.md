# Time Warp (Go)

Experimental Go port for Time Warp IDE — a minimal console-based interpreter skeleton.

Status: early scaffold with tiny command set:

- BASIC-like: `PRINT <text>`, `ECHO <text>` -> `✅ <text>`
- Logo-like: `FORWARD|FD <n>`, `RIGHT <deg>`, `LEFT <deg>` -> `🐢 ...`
- PILOT-like: `T:<text>`, `A:<text>` -> `ℹ️` / `📝`

## Build and run

```bash
cd Time_Warp_Go
# Run with a single command
go run ./cmd/timewarp ECHO Hello

# Or interactive mode
go run ./cmd/timewarp
```

## Tests

```bash
cd Time_Warp_Go
go test ./...
```

## Notes

- This Go version follows the project convention: executors return text with emoji prefixes; no UI state is stored in executors.
- Intended as a learning scaffold. The Rust implementation remains the primary officially supported version.
