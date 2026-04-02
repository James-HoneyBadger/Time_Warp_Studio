# Development Roadmap

## Vision

Time Warp Studio is an educational desktop IDE for learning 24 programming languages spanning six decades of computing history. This roadmap outlines planned improvements and contribution opportunities.

---

## Current Release: v9.0.0 (March 2026)

- 24 language executors with turtle graphics integration
- Step-through debugger with execution timeline and rewind
- 28 built-in themes (dark, light, retro CRT, accessibility)
- 93 example programs across all languages
- Lesson system with auto-verification
- AI-powered code suggestions and error explanations
- 14 dynamic feature panels

---

## Near-Term (Q2 2026)

### Language Improvements

- Expand COBOL executor: `PERFORM VARYING`, table handling, `EVALUATE`
- Add HyperTalk container navigation (`go to card`, `find`)
- Improve Prolog cut/negation and built-in predicates

### IDE Enhancements

- Code folding for all languages
- Split editor view (side-by-side files)
- Persistent breakpoints across sessions
- Export turtle graphics as SVG/PNG

### Quality

- Increase test coverage to 90%+
- Add property-based testing for expression evaluator
- Performance benchmarks for all executors

---

## Mid-Term (Q3–Q4 2026)

### New Languages (Candidates)

- **Ruby** — Object-oriented scripting
- **Erlang** — Concurrent/functional programming
- **Rust** — Systems programming with ownership model

### Plugin System

- Plugin API for third-party language executors
- Custom panel registration
- Theme marketplace

### Hardware Integration

- Raspberry Pi GPIO simulation and bridging
- Arduino serial communication via `pyfirmata`
- Sensor data visualization in canvas

### Collaboration

- Shared editing sessions (local network)
- Classroom assignment distribution and collection

---

## Long-Term (2027+)

### Platform Expansion

- WebAssembly build for browser-based version (experimental)
- Flatpak/Snap packaging for Linux distribution

### Advanced Features

- Multi-file project support with file tree
- Integrated terminal emulator
- Language interop (call one language from another)
- Recording and playback of coding sessions

---

## How to Contribute

1. **Fork** the repository and create a feature branch
2. Read [CONTRIBUTING.md](CONTRIBUTING.md) for code style and PR guidelines
3. Check [open issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues) for `good first issue` labels
4. Submit a pull request with a clear description

**Maintainer:** James Temple — [james@honey-badger.org](mailto:james@honey-badger.org)
