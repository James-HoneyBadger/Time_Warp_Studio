# Time Warp IDE

Time Warp IDE is an educational programming environment that unifies BASIC, PILOT, and Logo with experimental Pascal, Prolog, and C support. The official implementation is a PySide6 desktop application that keeps the language executors stateless while the UI manages canvas rendering, editor state, and theming.

**Current release:** 5.0.0 (December 11, 2025)

## Highlights

- **Three core languages**: BASIC, PILOT, and Logo with full feature parity (Pascal, Prolog, and C available experimentally)
- **Advanced UI toolkit**: Multiple screen modes, code editor with snippets, variable inspector, debug panel, focus mode, CRT effect, and accessibility features
- **Real-time turtle graphics**: Full-featured canvas with zoom/pan, color support, pen width control, sprite visibility, and graphical output modes
- **Rich development environment**: Syntax highlighting, error explorer, code search/replace, snippet system, and retro cassette animations
- **Extended capabilities**: Music playback, particle systems, fractal generation, gamepad support, collaborative editing client, and speech synthesis
- **Educational workflow**: Emoji-prefixed output, immediate feedback, comprehensive examples, and intuitive error messages

## Quick Start

```bash
# Clone and navigate to Python implementation
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp/Platforms/Python

# Create virtual environment (recommended)
python3 -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate

# Install and run
pip install -r requirements.txt
python time_warp_ide.py
```

For quick validation:
```bash
cd ../../  # Back to repo root
python Tests/run_tests.py --quick
```

## Documentation

All guides live in `Docs/` with an index at `Docs/INDEX.md`:

- `Docs/student/` – Student lesson book with progressive projects
- `Docs/teacher/` – Curriculum plans, assessments, and classroom strategies
- `Docs/user/` – User manual and programming guide
- `Docs/developer/` – Architecture, contributing standards, and technical references
- `Docs/installation/` – Platform-specific setup guides and troubleshooting

## Repository Layout

```
Time_Warp_Studio/
├── Platforms/
│   └── Python/                     # Official actively-maintained implementation
│       ├── time_warp_ide.py       # Main entry point (PySide6)
│       └── time_warp/
│           ├── core/              # Interpreter and support modules
│           │   ├── interpreter.py # Main dispatch logic
│           │   ├── game_support.py
│           │   ├── gamepad.py
│           │   ├── music.py
│           │   ├── particles.py
│           │   ├── shapes.py
│           │   ├── speech.py
│           │   └── fractals.py
│           ├── languages/         # BASIC, PILOT, Logo (+ experimental Pascal, Prolog, C)
│           │   ├── basic.py
│           │   ├── pilot.py
│           │   ├── logo.py
│           │   ├── pascal.py
│           │   ├── prolog.py
│           │   └── c_lang_fixed.py
│           ├── ui/                # Qt6 UI components
│           │   ├── main_window.py
│           │   ├── editor.py
│           │   ├── canvas.py
│           │   ├── output.py
│           │   ├── debug_panel.py
│           │   ├── variable_inspector.py
│           │   ├── error_explorer.py
│           │   ├── screen_modes.py
│           │   ├── themes.py
│           │   ├── focus_mode.py
│           │   ├── crt_effect.py
│           │   ├── cassette_animation.py
│           │   ├── accessibility.py
│           │   ├── collaboration_client.py
│           │   ├── snippets.py
│           │   ├── snippet_dialog.py
│           │   └── onboarding.py
│           └── graphics/          # Turtle graphics state and rendering
├── Docs/                          # Complete documentation library
├── Examples/                      # Sample programs (BASIC, PILOT, Logo, Pascal, C, Prolog)
├── Tests/                         # Pytest suite (unit, integration, conformance)
├── Core_Spec/                     # Language specifications
└── .github/                       # GitHub workflows and configurations
```

## Platform Overview

| Platform | Location | Status | Notes |
| - | - | - | - |
| Python | `Platforms/Python/` | ✅ Active | PySide6 IDE with full feature set |
| Browser | `Platforms/Browser/` | 🧪 Experimental | HTML/JS prototype for research |
| Windows2000 | `Platforms/Windows2000/` | 🧪 Historical | Legacy build scripts (archival) |

Other platforms (Rust, Go, Amiga, Haiku, Apple, OS/2, DOS) have been removed or archived.

## Features Overview

### Languages
- **BASIC**: Full interpreter with variables, loops, conditionals, subroutines, and I/O
- **PILOT**: Structured language with acceptance, computation, and output features
- **Logo**: Complete turtle graphics with commands for movement, drawing, color, and sprites
- **Experimental**: Pascal, Prolog, and C implementations available but incomplete

### User Interface
- **Multiple Screen Modes**: Switch between text, graphics, single, and combined modes
- **Code Editor**: Syntax highlighting, line numbers, code snippets, search/replace
- **Variable Inspector**: Real-time display of program variables and values
- **Debug Panel**: Step through execution, breakpoints, and execution history
- **Error Explorer**: Interactive error details with suggestions
- **Focus Mode**: Distraction-free coding environment
- **Accessibility**: Keyboard navigation, high contrast themes, screen reader support

### Graphics & Animation
- **Turtle Canvas**: Full 2D drawing with color, pen width, and sprite visibility
- **Zoom & Pan**: Interactive canvas navigation
- **Cassette Animation**: Retro loading animation effects
- **CRT Effect**: Vintage monitor screen emulation
- **Particle Systems**: Visual effects for educational demonstrations

### Advanced Features
- **Code Snippets**: Reusable template library for common patterns
- **Collaborative Editing**: Real-time multi-user code editing client
- **Music Playback**: MIDI/WAV support for multimedia projects
- **Gamepad Support**: Controller input for interactive programs
- **Speech Synthesis**: Text-to-speech capabilities for accessibility and games
- **Fractal Generation**: Mathematical visualization tools
- **Onboarding Wizard**: Interactive first-run guide

## Development & Contribution

### Project Philosophy
1. **Educational First** – Every design choice prioritizes learning
2. **Clear Feedback** – Error messages explain, never confuse
3. **Visual Learning** – Built-in graphics make abstract concepts tangible
4. **Stateless Executors** – Language processors return text; UI manages state
5. **Safe Evaluation** – Use `safe_eval()` for expressions, never Python's `eval()`

### Development Guidelines
- Language executors are stateless command processors
- All output uses emoji prefixes: ❌ (error), ✅ (success), ℹ️ (info), 🐢 (turtle), 🚀 (event)
- Turtle graphics state lives in the UI, not the executor
- Optional hardware integrations default to simulation mode

### Testing

```bash
cd Platforms/Python

# Activate virtual environment
source .venv/bin/activate

# Run quick smoke tests
python ../../Tests/run_tests.py --quick

# Run full test suite
python ../../Tests/run_tests.py --comprehensive

# Run specific test file
pytest ../../Tests/test_conformance_basic_pilot_logo.py -v
```

### Adding a New Language

1. Create executor in `time_warp/languages/new_language.py` implementing the `LanguageExecutor` interface
2. Register in `time_warp/core/interpreter.py`
3. Add syntax highlighting to UI
4. Add unit tests and example programs

## System Requirements

### Minimum
- **OS**: Linux (kernel 3.10+), macOS 10.13+, Windows 7+
- **CPU**: 1 GHz processor with SSSE3/SSE4.1 support
- **RAM**: 256 MB
- **Disk**: 50 MB free space
- **Display**: 800×600 resolution

### Recommended
- **CPU**: Modern multi-core processor
- **RAM**: 512 MB or more
- **Disk**: 100 MB for full documentation and examples
- **Display**: 1024×768 or higher

## Contributing

Contributions are welcome! Start with:
1. Read [Docs/developer/00-developer-guide.md](Docs/developer/00-developer-guide.md)
2. Check [FEATURE_IMPLEMENTATION_SUMMARY.md](FEATURE_IMPLEMENTATION_SUMMARY.md)
3. Review [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)

Focus areas that help most:
- Example programs and educational materials
- Documentation improvements
- Bug reports with detailed reproduction steps
- Feature suggestions with educational use cases

## Community & Support

- **Issues & Bug Reports**: <https://github.com/James-HoneyBadger/Time_Warp/issues>
- **Discussions & Q&A**: <https://github.com/James-HoneyBadger/Time_Warp/discussions>
- **Releases & Downloads**: <https://github.com/James-HoneyBadger/Time_Warp/releases>

## License

Time Warp IDE is released under the MIT License. See [LICENSE](LICENSE) for details.

---

**Maintained by:** James Temple  
**Primary Contact:** james@honey-badger.org  
**Repository:** <https://github.com/James-HoneyBadger/Time_Warp>
