# Time Warp IDE API Documentation

## Core Modules

### `interpreter::Interpreter`
Central execution engine supporting PILOT, BASIC, and Logo languages.

**Key Methods:**
- `load_program(text: &str)` - Parse and load program into memory
- `execute(&mut self, turtle: &mut TurtleState)` - Run loaded program with timeout/iteration limits
- `evaluate_expression(expr: &str)` - Safely evaluate math expressions
- `interpolate_text(text: &str)` - Replace *VAR* with variable values
- `get_output() -> &[String]` - Access output without cloning (performance)

**State Management:**
- `variables: HashMap<String, f64>` - Numeric variables
- `string_variables: HashMap<String, String>` - String variables
- `program_lines: Vec<(Option<usize>, String)>` - Loaded program
- `labels: HashMap<String, usize>` - Jump targets for GOTO/JUMP

**Security:**
- Max iterations: 100,000 (prevents infinite loops)
- Execution timeout: 10 seconds (prevents DoS)
- Expression complexity limits in ExpressionEvaluator

---

### `compiler::TempleCodeCompiler`
Experimental transpiler for TempleCode → C → executable compilation.

**Workflow:**
1. `compile_to_c(source: &str) -> Result<String>` - Generate C code
2. `compile_to_executable(c_code: &str, output: &str) -> Result<PathBuf>` - Invoke system compiler

**Supported Features:**
- BASIC: PRINT, LET, INPUT, IF...THEN, GOTO, END, REM
- PILOT: T:, A: commands
- Logo: Ignored (converted to C comments)

**Variable Mapping:**
- Numeric: `double V_<name>`
- String: `char S_<name>[256]`
- Expressions: Minimal transformation, `^` → `pow()`

---

### `graphics::TurtleState`
Logo-style turtle graphics state and line storage.

**Drawing Methods:**
- `forward(distance: f32)` - Move forward, draw if pen down
- `back(distance: f32)` - Move backward
- `left(angle: f32)` - Turn left (degrees)
- `right(angle: f32)` - Turn right (degrees)
- `goto(x, y)` - Move to absolute position
- `home()` - Return to center (0,0), heading 0°
- `clear()` - Erase all drawn lines

**Pen Control:**
- `pen_down: bool` - Drawing enabled/disabled
- `pen_color: Color32` - Current drawing color
- `pen_width: f32` - Line thickness

**Canvas:**
- Coordinate system: (0,0) at center, Y-axis inverted
- Lines stored as `Vec<TurtleLine>` for rendering/export

---

### `utils::ExpressionEvaluator`
Safe mathematical expression parser and evaluator.

**Features:**
- Operators: `+`, `-`, `*`, `/`, `%`, `^`
- Functions: `sin`, `cos`, `tan`, `sqrt`, `abs`, `log`, `exp`, `min`, `max`, `pow`, `rand`
- Variables: Dynamic via `set_variable()` or `with_variables()`
- Parentheses: Arbitrary nesting depth (max 100 for security)

**Performance:**
- Token caching: 10-50x speedup on repeated expressions
- Complexity limits: MAX_TOKENS=1000, MAX_DEPTH=100

**Security:**
- No `eval()` or code execution - pure arithmetic only
- Detailed error messages with expression context

---

### `app::TimeWarpApp`
Main application state managing IDE lifecycle.

**Components:**
- File management: Multi-file editing, open/save/new
- Execution: Run programs via Interpreter
- Graphics: Turtle visualization with zoom/pan
- Themes: 8 built-in themes (Dracula, Monokai, Solarized, etc.)
- Editing: Undo/redo (max 100 steps), find/replace, step debugging

**UI Tabs:**
- 0: Editor (code editing)
- 1: Output & Graphics (program results, turtle canvas)
- 2: Debug (step execution, variable inspector)
- 3: Explorer (file tree - future)
- 4: Help (documentation, quick reference)

---

## Language Executors

### `languages::pilot`
PILOT language executor with commands T:, A:, M:, Y:, N:, C:, U:, J:, L:, E:, R:

**Key Functions:**
- `execute()` - Dispatch command to handler
- `execute_text()` - T: output with interpolation
- `execute_accept()` - A: user input
- `execute_match()` - M: pattern matching
- `execute_compute()` - C: arithmetic

### `languages::basic`
BASIC language executor supporting PRINT, LET, INPUT, IF, GOTO, FOR, SCREEN, CLS, LOCATE, INKEY$

**Key Functions:**
- `execute()` - Dispatch BASIC command
- `execute_print()` - PRINT with comma-separated items
- `execute_let()` - LET variable assignment
- `execute_input()` - INPUT with optional prompt
- `execute_if()` - IF condition THEN action

### `languages::logo`
Logo language executor for turtle graphics commands.

**Key Functions:**
- `execute()` - Dispatch Logo command or procedure call
- `execute_procedure()` - Run user-defined procedures with parameters
- Commands: FORWARD, BACK, LEFT, RIGHT, PENUP, PENDOWN, HOME, CLEAR, SETCOLOR, etc.

---

## Performance Notes

### Optimizations Applied
1. **interpolate_text()** - O(n) single-pass instead of O(n*m) repeated replace()
2. **Token caching** - Expression evaluator caches parsed tokens
3. **Lazy regex** - VAR_INTERPOLATION_PATTERN compiled once via once_cell
4. **Pre-allocated vectors** - with_capacity() for known sizes
5. **Avoided clones** - Use references where borrow checker allows

### Hot Paths
- `execute()` loop (100K+ iterations)
- `evaluate_expression()` (called per PRINT/LET)
- `interpolate_text()` (called per T: command)

### Known Bottlenecks
- `evaluate_expression()` clones variables HashMap (cannot avoid without lifetime complexity)
- `execute()` returns cloned output vector (use `get_output()` for zero-copy reads)

---

## Testing

Run comprehensive test suite:
```bash
cargo test --quiet
```

Current coverage:
- 69 tests passing
- 18 unit tests
- 22 comprehensive integration tests
- 14 edge case tests
- 4 compiler tests
- 5 ignored (hardware-dependent)

---

## Examples

See `examples/` directory for 64+ sample programs:
- BASIC: Games, graphics demos, educational programs
- PILOT: Interactive tutorials, quizzes
- Logo: Fractals, spirals, geometric art
