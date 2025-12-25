# Technical Reference

Complete technical documentation for developers.

## Table of Contents

1. [Architecture](#architecture)
2. [Project Structure](#project-structure)
3. [Core Components](#core-components)
4. [Language Implementations](#language-implementations)
5. [Interpreter API](#interpreter-api)
6. [Graphics System](#graphics-system)
7. [Building & Development](#building--development)

## Architecture

### Design Philosophy

**Time Warp IDE** uses a **plugin-based interpreter architecture**:

```
┌─────────────────────────────────────┐
│       Time Warp IDE (PySide6)       │
├─────────────────────────────────────┤
│         UI Layer (Qt Widgets)       │
│    ├─ Editor          ├─ Canvas     │
│    ├─ REPL            ├─ Variables  │
│    └─ Status Bar      └─ Output     │
├─────────────────────────────────────┤
│    Core Interpreter (Python)        │
│  ├─ BASIC Executor                  │
│  ├─ PILOT Executor                  │
│  ├─ Logo Executor                   │
│  ├─ Pascal Executor                 │
│  ├─ Prolog Executor                 │
│  ├─ Forth Executor                  │
│  └─ C Executor                      │
├─────────────────────────────────────┤
│     Graphics Engine & I/O           │
│    ├─ Turtle Graphics               │
│    ├─ Canvas Rendering              │
│    └─ Output System                 │
└─────────────────────────────────────┘
```

### Key Principles

1. **Stateless Executors** - Each language executor is independent
2. **Unified Output** - All languages produce the same output format
3. **Real-time Rendering** - Graphics update as code runs
4. **Error Transparency** - Clear, actionable error messages
5. **Educational Focus** - Designed for learning, not production

## Project Structure

```
Time_Warp_Studio/
├── Platforms/Python/
│   ├── time_warp_ide.py          ← Entry point
│   ├── time_warp/
│   │   ├── core/
│   │   │   ├── interpreter.py    ← Main dispatcher
│   │   │   ├── safe_eval.py      ← Safe math evaluation
│   │   │   └── *_executor.py     ← Language implementations
│   │   ├── ui/
│   │   │   ├── main_window.py    ← Main IDE window
│   │   │   ├── editor.py         ← Code editor
│   │   │   ├── canvas.py         ← Graphics canvas
│   │   │   └── theme.py          ← Theme system
│   │   └── tools/
│   │       ├── theme.py          ← Color schemes
│   │       └── syntax.py         ← Syntax highlighting
│   ├── requirements.txt
│   └── .venv/                    ← Virtual environment
├── Platforms/Rust/               ← Experimental port
└── Examples/                      ← Sample programs
```

## Core Components

### 1. Interpreter (core/interpreter.py)

Main dispatch logic:

```python
class TimeWarpInterpreter:
    def execute(code: str, language: str) -> str
    def start_execution(code: str) -> Tuple[str, ExecutionState]
    def continue_execution() -> Tuple[str, ExecutionState]
    def provide_input(value: str) -> None
```

### 2. Language Executors

Each language has an executor with:

```python
class LanguageExecutor:
    def execute(command: str) -> str       # Execute code, return output
    def start_execution(code: str) -> Tuple[str, ExecutionState]
    def continue_execution() -> Tuple[str, ExecutionState]
    def provide_input(value: str) -> None  # Handle INPUT command
    def get_draw_commands() -> List[DrawCommand]  # Get graphics
```

### 3. Graphics System

#### DrawCommand Types

```python
class DrawCommand:
    Line = (x1, y1, x2, y2, color, width)
    Circle = (x, y, radius, color, filled)
    Rect = (x, y, width, height, color, filled)
    Text = (x, y, text, color, font_size)
    Clear = (color)
    Turtle = (x, y, angle, visible, color)
```

#### Turtle State

```python
class TurtleState:
    x: float = 0              # X position
    y: float = 0              # Y position
    angle: float = 0          # Heading (0-360)
    pen_down: bool = True     # Drawing?
    pen_color: RGB = (0, 0, 0)
    pen_width: float = 1.0
    visible: bool = True
```

### 4. UI Components

#### Main Window (ui/main_window.py)

```
┌─────────────────────────────────────────┐
│ Menu Bar (File, Edit, View, Theme)      │
├─────────────────────────────────────────┤
│ Toolbar (Language, Run, Themes)         │
├──────────────────┬──────────────────────┤
│ Left (Editor)    │ Right (Output Panel) │
│ 85% height       │                      │
│ ↓                │                      │
│ Immediate Mode   │                      │
│ 15% height       │                      │
├─────────────────────────────────────────┤
│ Status Bar (Status messages)            │
└─────────────────────────────────────────┘
```

## Language Implementations

### Language Features Matrix

| Feature | BASIC | PILOT | Logo | Pascal | Prolog | Forth | C |
|---------|-------|-------|------|--------|--------|-------|---|
| Variables | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Loops | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Functions | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Graphics | ⚠️ | ⚠️ | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Pattern Match | ❌ | ✅ | ❌ | ❌ | ✅ | ❌ | ❌ |
| Recursion | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

### Adding a New Language

1. **Create executor** in `core/executors/my_lang.py`:
   ```python
   from core.interpreter import LanguageExecutor
   
   class MyLangExecutor(LanguageExecutor):
       def execute(self, command: str) -> str:
           # Parse and execute
           return "Output\n"
   ```

2. **Register** in `core/interpreter.py`:
   ```python
   from core.executors.my_lang import MyLangExecutor
   
   class TimeWarpInterpreter:
       def __init__(self):
           self.my_lang = MyLangExecutor(self)
   ```

3. **Add detection** in `execute()` method

4. **Add examples** in `Examples/my_lang/`

## Interpreter API

### ExecutionState

```python
class ExecutionState:
    FINISHED = "finished"
    RUNNING = "running"
    WAITING_FOR_INPUT = "waiting_for_input"
    ERROR = "error"
```

### Error Handling

All errors use consistent format:

```
❌ Error Type: Description
   at line N
```

### Output Format

Standard emoji prefixes:

```
✅ Success message
❌ Error message
ℹ️ Information
🎨 Graphics command
🚀 Execution event
🐢 Turtle command
📝 Input prompt
```

## Graphics System

### Coordinate System

- **Origin** (0, 0) = center of canvas
- **X-axis** = left-right (positive = right)
- **Y-axis** = up-down (positive = up)
- **Angles** = degrees (0° = right, 90° = up)

### Color Format

RGB tuple: `(red, green, blue)` where each is 0-255

### Turtle Commands (Logo Example)

```logo
FORWARD 100     → Move forward 100 units
RIGHT 90        → Turn right 90°
LEFT 45         → Turn left 45°
PENUP           → Stop drawing
PENDOWN         → Start drawing
PENCOLOR 255 0 0 → Set color to red
REPEAT 5 [...]  → Repeat block 5 times
```

## Building & Development

### Development Environment

```bash
cd Platforms/Python
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

### Running Tests

```bash
python test_runner.py --comprehensive
```

### Code Style

- **Python** - Follow PEP 8
- **Rust** - Use `cargo fmt`
- **Documentation** - Docstrings for all public functions

### Common Tasks

#### Add a New Theme
1. Edit `tools/theme.py`
2. Define color palette
3. Register in UI

#### Add a Command to a Language
1. Edit language executor in `core/executors/`
2. Implement command parsing
3. Update documentation
4. Add test case

#### Debug a Program

Use the debug panel:
1. Set breakpoints by clicking line numbers
2. Step through code
3. Watch variables in Variables panel
4. Use Immediate Mode to test expressions

## Performance Considerations

### Optimization Tips

1. **Avoid deep recursion** - Stack depth limited
2. **Cache expressions** - Don't re-parse same code
3. **Batch graphics** - Group FORWARD/RIGHT commands
4. **Use Rust version** - 10-100x faster for compute-heavy code

### Profiling

Enable profiling in settings to see:
- Time per command
- Memory usage
- Graphics rendering time

## Testing

### Unit Tests

```python
# test_basic_interpreter.py
def test_basic_print():
    result = basic_interpreter.execute('PRINT "Hello"')
    assert "Hello" in result
```

### Integration Tests

```python
# test_integration.py
def test_logo_square():
    code = "REPEAT 4 [FORWARD 100 RIGHT 90]"
    cmds = logo_interpreter.execute(code)
    assert len(cmds.draw_commands) > 0
```

## Troubleshooting Development

| Issue | Solution |
|-------|----------|
| PySide6 won't install | Use Python 3.8-3.11 |
| Module import fails | Check .venv/bin/activate |
| Graphics not showing | Check canvas size and zoom |
| Slow startup | Check theme loading |

## Further Reading

- [User Guide](../user-guide/) - Using the IDE
- [Tutorials](../tutorials/) - Learning each language
- [Examples](../../Examples/) - Sample programs
- [API Reference](api.md) - Detailed API docs

---

For questions, check the Examples folder or read the source code - it's well-documented!
