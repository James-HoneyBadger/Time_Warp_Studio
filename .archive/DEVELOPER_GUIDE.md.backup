# TempleCode & Time Warp v3.0.0 - Developer Guide

This guide is for contributors who want to understand, extend, and maintain TempleCode (the interpreter) and Time Warp v3.0.0 (the multi-platform GUI IDE).

## Version 3.0.0 - Multi-Platform Architecture

Time Warp v3.0.0 is available on 6 platforms with distinct implementations:

- **Python** (`Python/`) - Original Tkinter GUI
- **Rust** (`Rust/`) - Cross-platform eframe/egui with shared engine
- **Windows** (`Windows/`) - Native Windows `.exe` with installer
- **Apple** (`Apple/`) - Native macOS `.app` bundle
- **DOS** (`DOS/`) - DJGPP/Mode 13h VGA retro implementation
- **Browser** (`Browser/`) - WebAssembly/JavaScript web version

For complete documentation, see:
- User Guide: `docs/user_guide.md`
- Technical Reference: `docs/technical_reference.md`
- Student Handbook: `docs/student_handbook.md`
- Teacher Guide: `docs/teacher_guide.md`

## Repository Structure

See README.md for platform-specific build instructions.

## Architecture

### Python Implementation
- Original Tkinter GUI
- AST-based safe expression evaluator
- Pluggable I/O and Turtle abstractions

### Rust Implementation
- High-performance with evalexpr
- Shared by Rust, Windows, Apple via Cargo path dependency
- Thread-safe with Arc<Mutex<T>>

### DOS Implementation
- Standalone C for DJGPP
- Direct VGA Mode 13h graphics
- DPMI BIOS calls, nearptr VRAM

## Contributing

1. Fork repository
2. Create feature branch  
3. Add tests
4. Update docs
5. Submit PR

All platforms should remain functional.

## License

MIT
