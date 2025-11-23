# Time Warp IDE

Time Warp IDE is an educational programming environment that unifies BASIC, PILOT, and Logo with experimental Pascal, Prolog, and C support. The official implementation is a PySide6 desktop application that keeps the language executors stateless while the UI manages canvas rendering, editor state, and theming.

**Current release:** 4.0.0 (November 22, 2025)

## Highlights

- **Three core languages**: BASIC, PILOT, and Logo ship with full feature parity; Pascal, Prolog, and C remain experimental inside the Python interpreter
- **Integrated turtle graphics**: Logo commands draw on a shared canvas that is rendered by the UI and persisted across executions
- **Education-first workflows**: Emoji-prefixed output, immediate feedback loops, and a comprehensive lesson library make classroom use straightforward
- **Hardware simulation**: Arduino and Raspberry Pi controllers default to simulation mode, enabling IoT lessons without extra hardware
- **Cross-language examples**: Reusable demos live in `Examples/` and are referenced throughout the curriculum

## Quick Start (Python)

```bash
git clone https://github.com/James-HoneyBadger/Time_Warp.git
cd Time_Warp
python Time_Warp_IDE.py
```

The entry point auto-installs PySide6 if necessary and checks for required CPU instructions (SSSE3/SSE4). Run `python test_runner.py --basic` for a fast smoke test or `python test_runner.py --comprehensive` for the full suite.

## Documentation

All guides live in `Docs/` with an index at `Docs/INDEX.md`:

- `Docs/student/` – Student lesson book with progressive projects
- `Docs/teacher/` – Curriculum plans, assessments, and classroom strategies
- `Docs/user/` – User manual and programming guide
- `Docs/developer/` – Architecture, contributing standards, and technical references
- `Docs/installation/` – Platform-specific setup guides and troubleshooting

## Platform Overview

| Platform | Location | Status | Notes |
| - | - | - | - |
| Python | `Platforms/Python/` | ✅ Active | PySide6 IDE, turtle graphics, language executors |
| Browser | `Platforms/Browser/` | 🧪 Experimental | HTML/JS prototype kept for research |
| DOS | `Platforms/DOS/` | 🧪 Experimental | C89 text-mode interpreter for historical study |
| Windows2000 | `Platforms/Windows2000/` | 📜 Archived | Vintage native build retained for reference |

Legacy implementations (Rust, Go, Amiga, Haiku, Apple, OS/2) have been removed to keep maintenance focused.

## Repository Layout

```
Time_Warp/
├── Platforms/        # Implementations (Python primary)
├── Docs/             # Documentation library (index + guides)
├── Examples/         # Sample programs for BASIC, PILOT, and Logo
├── Core_Spec/        # Language specifications and turtle docs
├── Scripts/          # Utility scripts for builds and packaging
├── Tests/            # Pytest suite and helpers
└── Packaging/        # Distribution resources
```

## Development Notes

- Use `core/safe_expression_evaluator.py::safe_eval()` for expression parsing—`eval()` is forbidden
- Language executors return emoji-prefixed strings (`❌`, `ℹ️`, `🚀`, etc.) and never manipulate UI objects directly
- The turtle canvas is managed by the UI; executors only update coordinates and emit draw commands
- Optional hardware integrations remain in simulation mode unless dependencies are installed

### Testing

```bash
python test_runner.py --basic     # fast validation
python test_runner.py --comprehensive
pytest Tests/test_core_interpreter.py -v
```

## Contributing

Contributions are welcome. Start with `Docs/developer/DEVELOPER_GUIDE.md` for architecture, testing expectations, and submission guidelines. Example programs and documentation improvements are especially helpful for educators.

## Community & Support

- Issues & enhancements: <https://github.com/James-HoneyBadger/Time_Warp/issues>
- Discussions: <https://github.com/James-HoneyBadger/Time_Warp/discussions>
- Releases: <https://github.com/James-HoneyBadger/Time_Warp/releases>

## License

Time Warp IDE is released under the MIT License. See `LICENSE` for details.
