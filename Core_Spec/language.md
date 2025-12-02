# Time Warp Language Specification (stub)

This document will centralize the cross-platform language reference (BASIC/PILOT/Logo blend) including:

- Statements and aliases (PRINT/TYPE, INPUT/ACCEPT, GOTO/JUMP, loops)
- Expression rules and operator precedence
- Error conventions and emoji prefixes (❌, ✅, ℹ️, 🎨, 🚀, 🐢)

Initial content to be extracted from root `README.md` and platform docs.

## Cross-Language Conformance (Golden Tests)

To ensure consistent behavior across interpreters and avoid regressions, we define canonical outputs for a curated set of programs in BASIC, PILOT, and Logo. Golden tests verify that executing these programs yields exactly the expected text output and that turtle actions are recorded consistently.

### Canonical Output Rules
- Output is normalized with trailing whitespace trimmed per line.
- Line endings must be `\n` in canonical snapshots.
- Emoji prefixes are preserved (`❌`, `✅`, `ℹ️`, `🎨`, `🚀`, `🐢`).
- Turtle actions are logged as textual events (e.g., `🐢 MOVE dx=10 dy=0`, `🐢 TURN angle=90`).
- Randomness must be seeded or disabled for deterministic outputs.

### Test Categories
- Basic I/O: print, input, variables, arithmetic.
- Control flow: conditionals and loops.
- Procedures/subroutines: call/return semantics.
- Turtle Graphics: movement, drawing commands, pen state.

### Golden Fixtures
- Location: `Examples/golden/` for source programs; `Examples/golden_fixtures/` for input sequences.
- Snapshots: `Tests/golden_snapshots/` hold expected outputs per language and program.
- Naming convention: `{language}/{program}.out` for snapshots; inputs as `{language}/{program}.in`.

### Execution Contract
- Interpreters must run programs to completion using provided fixture inputs via the standard `provide_input` mechanism.
- For Logo turtle graphics, interpreters shall emit textual `🐢` events alongside any graphical rendering; golden tests match on the textual log only.
- Errors must include line numbers where available and preserve the `❌` prefix.

### Determinism Requirements
- Any time-based, random, or environment-dependent features must be controlled via seeds and config flags exposed through the interpreter API.
- Examples used in golden tests avoid non-deterministic behaviors or explicitly set seeds.

### CI Integration
- A nightly workflow executes all golden tests and headless example runs.
- Artifacts include failure diffs, last actual outputs, and a summary report.

### Adding New Golden Tests
1. Create the program under `Examples/golden/{language}/`.
2. Add optional input fixture to `Examples/golden_fixtures/{language}/`.
3. Run locally to generate the actual output and add a snapshot to `Tests/golden_snapshots/{language}/`.
4. Extend the conformance test file to include the new program.
5. Ensure determinism (seeds/configs) and update documentation if special setup is required.
