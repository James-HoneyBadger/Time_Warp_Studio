# Development Roadmap

## Vision

Time Warp Studio is an educational desktop IDE for learning **9 active programming languages**. This roadmap outlines planned improvements and contribution opportunities.

---

## Current Release: v14.0.0 (October 2026)

- **9 active language executors** — BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Brainfuck, and Python
- **SVGA graphics mode** — 800×600 virtual canvas with anti-aliasing, zoom/pan, and pixel-accurate rendering
- **Vector graphics** — cubic Bezier curves, gradient-filled shapes (linear & radial), dash pen styles, Z-ordered layers
- **Sprite system** — define named pixel-art sprites via `define_sprite()`, stamp/animate with rotation and scale
- **Enhanced MML sound engine** — ADSR envelopes per note, 4-channel polyphonic mixing, pulse waveform (duty cycle), white-noise waveform, chord notation `[CEG]`, `@W`/`@D`/`@P`/`@C` MML extensions
- Step-through debugger with execution timeline and rewind
- 28 built-in themes (dark, light, retro CRT, accessibility)
- Lesson system with auto-verification
- AI-powered code suggestions and error explanations
- Learning Hub with guided challenges and Project Explorer
- Prolog `\+` negation-as-failure parsing, extended built-ins (`format/2`, `split_string/4`, `string_concat/3`, etc.)
- Example programs across all 9 active languages (0 timeouts)

---

## Near-Term (Q2 2026)

### Language Improvements

- Improve Prolog cut/negation and built-in predicates — **done in v10.2.0**

### IDE Enhancements

- Code folding for all languages
- Split editor view (side-by-side files)
- Persistent breakpoints across sessions
- Export turtle graphics as SVG/PNG *(SVG export added in v10.0.0)*

### Quality

- Increase test coverage to 90%+
- Add property-based testing for expression evaluator
- Performance benchmarks for all executors

---

## Mid-Term (Q3–Q4 2026)

### Previously Explored Languages (removed from active scope)

The following were built and shipped in earlier releases (v10.2.0–v13.0.0)
but have since been removed from `time_warp/languages/` and are **not part of
the current 9-language release**: Tcl, PostScript, LISP/Scheme, COBOL, Ruby,
sandboxed Python variant, Perl 5, REXX, Smalltalk, APL. There is no plan to
reintroduce them in this release cycle; the active scope is intentionally
fixed at 9 languages (see Vision above).

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
