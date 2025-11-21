# Development Guide

## Repository Layout

- Rust IDE sources: `src/`
- Docs: `docs/`
- Tests: `tests/`

## Prerequisites

- Rust stable toolchain (rustup)

## Rust Setup

```bash
cargo fmt --all
cargo clippy -- -D warnings || true
cargo build
```

## Code Style

- Rust: rustfmt and clippy. See rustfmt.toml.

## Running

- Run the IDE: `cargo run`
- Experimental compiler: `cargo run -- --compile <input.tc> -o <output>`

## Tests

- `cargo test`

## Docs

Project documentation is written in Markdown under `docs/` and rendered on CI.
