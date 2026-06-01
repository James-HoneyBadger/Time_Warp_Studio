# Development Roadmap

## Vision

Time Warp Studio is an educational desktop IDE for learning **24 programming languages** spanning seven decades of computing history. This roadmap outlines planned improvements and contribution opportunities.

---

## Current Release: v13.0.0 (June 2026)

- **24 language executors** — BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Lua, Brainfuck, JavaScript, HyperTalk, Erlang, LISP/Scheme, COBOL, Tcl, PostScript, Ruby, Python (sandboxed), Haskell, 6502 Assembly, Perl 5, **REXX**, **Smalltalk**, **APL**
- **SVGA graphics mode** — 800×600 virtual canvas with anti-aliasing, zoom/pan, and pixel-accurate rendering
- **Vector graphics** — cubic Bezier curves, gradient-filled shapes (linear & radial), dash pen styles, Z-ordered layers
- **Sprite system** — define named pixel-art sprites via `define_sprite()`, stamp/animate with rotation and scale
- **Enhanced MML sound engine** — ADSR envelopes per note, 4-channel polyphonic mixing, pulse waveform (duty cycle), white-noise waveform, chord notation `[CEG]`, `@W`/`@D`/`@P`/`@C` MML extensions
- Step-through debugger with execution timeline and rewind
- 28 built-in themes (dark, light, retro CRT, accessibility)
- Lesson system with auto-verification
- AI-powered code suggestions and error explanations
- Learning Hub with guided challenges and Project Explorer
- HyperTalk `find` and `go to card` navigation commands
- Prolog `\+` negation-as-failure parsing, extended built-ins (`format/2`, `split_string/4`, `string_concat/3`, etc.)
- 70 passing demo programs (0 timeouts)

---

## Near-Term (Q2 2026)

### Language Improvements

- Expand COBOL executor: `PERFORM VARYING`, table handling, `EVALUATE`
- ✅ Add HyperTalk container navigation (`go to card`, `find`) — **done in v10.2.0**
- ✅ Improve Prolog cut/negation and built-in predicates — **done in v10.2.0**

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

### New Languages (Completed in v10.2.0)

- ✅ **Tcl** — scripting and embeddable extension language — **done**
- ✅ **PostScript** — page-description / stack-based graphics — **done**
- ✅ **LISP/Scheme** — classic AI / symbolic computation — **done**
- ✅ **COBOL** — business data processing — **done**

### New Languages (Candidates for v10.3+)

- ✅ **Ruby** — dynamic, object-oriented scripting — **done in v11.0.0**
- ✅ **Python (sandboxed)** — modern scripting with stdlib access — **done in v11.0.0**
- ✅ **Perl 5** — practical scripting, regex, and text processing — **done in v12.0.0**
- ✅ **REXX** — IBM Restructured eXtended eXecutor, SAY/DO/SELECT — **done in v13.0.0**
- ✅ **Smalltalk** — Smalltalk-80 with blocks, collections, closures — **done in v13.0.0**
- ✅ **APL** — array programming with APL glyphs, reduction/scan — **done in v13.0.0**

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
