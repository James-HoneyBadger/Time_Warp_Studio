# native/

The Qt application: ten language engines, the editor, the canvas, the debugger, the
curriculum and every validation gate.

```bash
cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
cmake --build native/build -j
./native/build/time_warp_ide                    # or: ./run.sh
ctest --test-dir native/build --output-on-failure
```

| Target | What it is |
| --- | --- |
| `time_warp_ide` | The application |
| `timewarp_lang`, `timewarp_scheme`, `timewarp_expr` | The engines: the shared loop and ten executors, the Scheme machine, the expression evaluator |
| `timewarp_ui`, `timewarp_ui_core` | The window, and the part of it a standalone player needs |
| `timewarp_project`, `timewarp_sql`, `timewarp_api` | Projects, the one SQLite connection, JSON-RPC |
| `time_warp_check` | One program through the engines: what it printed and what it drew |
| `time_warp_gates` | `smoke`, `commands`, `repl`, `golden`, `tree`: the validation harnesses |
| `time_warp_teach`, `time_warp_learn` | The curriculum, from a teacher's and a learner's side |
| `time_warp_player`, `twc` | A program as a window, or as a small standalone executable |

The design notes that used to live here are now in the documentation set:

| Read | For |
| --- | --- |
| [Architecture](../docs/maintainer/architecture.md) | The process and thread model, the libraries, the canvas queue, the extension points |
| [Decisions](../docs/maintainer/decisions.md) | Why the code is shaped the way it is, and what was tried and removed |
| [Testing](../docs/maintainer/testing.md) | The suites, the example and command gates, the parity corpora |
| [Contributing](../CONTRIBUTING.md) | Setup, the gates, and the recipes for a language, a command or a lesson |

Building, installing and troubleshooting a build are in
[getting started](../docs/user/getting-started.md).
