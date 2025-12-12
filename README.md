# Time Warp IDE

Time Warp IDE is an educational programming environment that unifies BASIC, PILOT, and Logo with experimental Pascal, Prolog, and C support. The official implementation is a PySide6 desktop application that keeps the language executors stateless while the UI manages canvas rendering, editor state, and theming.

**Current release:** 5.0.0 (December 2, 2025) - Phase III-A: AI Foundation

## Highlights

- **Three core languages**: BASIC, PILOT, and Logo ship with full feature parity; Pascal, Prolog, and C remain experimental inside the Python interpreter
- **Integrated turtle graphics**: Logo commands draw on a shared canvas that is rendered by the UI and persisted across executions
- **AI-powered assistance**: Intelligent code completion, error explanation, and learning tips powered by GitHub Copilot and local Ollama models
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

## AI Features (Phase III)

Time Warp IDE now includes AI-powered development assistance:

- **Code Completion** (Ctrl+Space): Context-aware code suggestions for BASIC, PILOT, and Logo
- **Error Explanation** (Ctrl+E): AI-powered analysis of runtime errors with detailed explanations
- **Code Review** (Ctrl+R): Automated code review with improvement suggestions
- **Learning Tips** (Ctrl+L): Personalized learning recommendations based on your code

### AI Provider Setup

The IDE supports multiple AI providers:

1. **GitHub Copilot**: Set `GITHUB_TOKEN` environment variable for GitHub-hosted models
2. **Ollama (Local)**: Install Ollama and run `ollama serve` for local AI models
3. **Other providers**: Additional providers can be added via the plugin system

Configure your preferred AI provider in the **AI → AI Provider** menu.

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
| Browser | `Platforms/Browser/` | 🧪 Experimental | HTML/JS prototype retained for research |
| Windows2000 | `Platforms/Windows2000/` | 🧪 Historical | Vintage build scripts and artifacts kept for archival/reference |

The Python implementation is the sole actively maintained version. Other platforms (Rust, Go, Amiga, Haiku, Apple, OS/2, DOS) have been removed or archived.

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

Contributions are welcome. Start with `Docs/developer/00-developer-guide.md` for architecture, testing expectations, and submission guidelines. Example programs and documentation improvements are especially helpful for educators.

## Community & Support

- Issues & enhancements: <https://github.com/James-HoneyBadger/Time_Warp/issues>
- Discussions: <https://github.com/James-HoneyBadger/Time_Warp/discussions>
- Releases: <https://github.com/James-HoneyBadger/Time_Warp/releases>

Note: Windows installers are produced as `TimeWarpIDE-Setup-<VERSION>.exe` and attached to releases. The Windows 2000 NSIS script lives in `Platforms/Windows2000/installer/timewarp.nsi` and is invoked by CI with `-DVERSION` and `-DOUTDIR`.

## License

Time Warp IDE is released under the MIT License. See `LICENSE` for details.
