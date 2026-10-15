# Development Roadmap

## Vision

Time Warp Studio is an educational desktop IDE for learning **9 active programming languages**. This roadmap outlines planned improvements and contribution opportunities.

---

## Current Release: v14.1.0 (October 15, 2026)

- **9 active language executors** — BASIC, PILOT, Logo, C, Pascal, Prolog, Forth, Brainfuck, and Python
- **SVGA graphics mode** — 800×600 virtual canvas with anti-aliasing, zoom/pan, and pixel-accurate rendering
- **Vector graphics** — cubic Bezier curves, gradient-filled shapes (linear & radial), dash pen styles, Z-ordered layers
- **Sprite system** — define named pixel-art sprites via `define_sprite()`, stamp/animate with rotation and scale
- **Enhanced MML sound engine** — ADSR envelopes per note, 4-channel polyphonic mixing, pulse waveform (duty cycle), white-noise waveform, chord notation `[CEG]`, `@W`/`@D`/`@P`/`@C` MML extensions
- Step-through debugger with execution timeline and rewind
- 33 built-in themes (dark, light, retro CRT, accessibility)
- Lessons dock with 18 built-in lessons and automatic checkpoint verification
- Borland Turbo-style keymap with a live Function-Key Bar
- Prolog `\+` negation-as-failure parsing, extended built-ins (`format/2`, `split_string/4`, `string_concat/3`, etc.)
- Example programs across all 9 active languages (81 programs, all verified by `test_all_examples.py`)

---

## Near-Term (Q2 2026)

### Language Improvements

- Improve Prolog cut/negation and built-in predicates — **done in v10.2.0**

### IDE Enhancements

- Code folding for all languages — **done** (indentation-based; Ctrl+Shift+[ / Ctrl+Shift+])
- Split editor view (side-by-side files) — **done** (Shift+F5 / Ctrl+\\)
- Persistent breakpoints across sessions — **done**
- Export turtle graphics as SVG/PNG — **done** (canvas toolbar; SVG since v10.0.0)

### Quality

- Increase test coverage to 90%+
- Add property-based testing for expression evaluator
- Performance benchmarks for all executors

---

## Mid-Term (Q3–Q4 2026)

### Previously Explored Languages (removed from active scope)

The following were built and shipped in earlier releases (v10.2.0–v13.0.0)
but have since been removed from `time_warp/languages/` and are **not part of
the current 9-language release**: Tcl, PostScript, LISP/Scheme, Ruby,
sandboxed Python variant, Perl 5, Smalltalk. There is no plan to
reintroduce them in this release cycle; the active scope is intentionally
fixed at 9 languages (see Vision above).

---

## Long-Term (2027+)

### Platform Expansion

- WebAssembly build for browser-based version (experimental)
- Flatpak/Snap packaging for Linux distribution *(manifests exist in packaging/linux/)*

### Advanced Features

- Multi-file project support with file tree *(project manager exists in core; the file-tree panel is not yet wired into the main window)*
- Integrated terminal emulator *(lightweight QProcess terminal tab ships; full PTY/ANSI emulation remains open)*
- Language interop (call one language from another)
- Recording and playback of coding sessions

---

## How to Contribute

1. **Fork** the repository and create a feature branch
2. Read [CONTRIBUTING.md](CONTRIBUTING.md) for code style and PR guidelines
3. Check [open issues](https://github.com/James-HoneyBadger/Time_Warp_Studio/issues) for `good first issue` labels
4. Submit a pull request with a clear description

**Maintainer:** James Temple — [james@honey-badger.org](mailto:james@honey-badger.org)
