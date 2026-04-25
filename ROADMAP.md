# Development Roadmap

## Vision

Time Warp Studio is an educational desktop IDE for learning **20 programming languages** spanning six decades of computing history. This roadmap outlines planned improvements and contribution opportunities.

---

## Current Release: v10.0.0 (April 2026)

- **20 language executors** — added Perl 5 (scalars/arrays/hashes, regex, subroutines, turtle graphics); Ruby, Erlang, Rust also added this cycle
- **SVGA graphics mode** — 800×600 virtual canvas with anti-aliasing, zoom/pan, and pixel-accurate rendering
- **Vector graphics** — cubic Bezier curves, gradient-filled shapes (linear & radial), dash pen styles, Z-ordered layers
- **Sprite system** — define named pixel-art sprites via `define_sprite()`, stamp/animate with rotation and scale
- **Enhanced MML sound engine** — ADSR envelopes per note, 4-channel polyphonic mixing, pulse waveform (duty cycle), white-noise waveform, chord notation `[CEG]`, `@W`/`@D`/`@P`/`@C` MML extensions
- Ruby, Erlang, Rust language executors with turtle graphics integration
- Step-through debugger with execution timeline and rewind
- 28 built-in themes (dark, light, retro CRT, accessibility)
- Lesson system with auto-verification
- AI-powered code suggestions and error explanations
- Learning Hub with guided challenges and Project Explorer

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

- **Tcl** — scripting and embeddable extension language
- **PostScript** — page-description / stack-based graphics
- **LISP** — classic AI / symbolic computation

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
