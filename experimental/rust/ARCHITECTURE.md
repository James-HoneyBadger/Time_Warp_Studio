# Time Warp IDE - Architecture Documentation

**Last Updated:** January 2025  
**Version:** 2.0.0  
**Build System:** Cargo/Rust

## Overview

Time Warp IDE is a Rust-based educational programming environment supporting three vintage languages (PILOT, BASIC, Logo) with modern UI (egui), turtle graphics, and optional audio/IoT/ML extensions.

## Core Architecture

### Module Hierarchy

```
time_warp_unified/
├── src/
│   ├── main.rs              # Entry point, eframe setup
│   ├── app.rs               # TimeWarpApp (egui app state)
│   ├── lib.rs               # Library exports, test suite
│   │
│   ├── interpreter/         # Core execution engine
│   │   └── mod.rs          # Interpreter, ExecutionResult, ForContext
│   │
│   ├── languages/           # Language-specific executors
│   │   ├── mod.rs          # Language enum
│   │   ├── pilot/mod.rs    # PILOT (95% complete)
│   │   ├── basic/mod.rs    # BASIC (85% complete)
│   │   └── logo/mod.rs     # Logo (75% complete)
│   │
│   ├── graphics/            # Turtle graphics
│   │   └── mod.rs          # TurtleState, path rendering
│   │
│   ├── ui/                  # egui UI components
│   │   ├── mod.rs          # Module exports
│   │   ├── editor.rs       # Code editor widget
│   │   ├── menubar.rs      # File/Edit/Run/Help menus
│   │   ├── output.rs       # Text output display
│   │   ├── statusbar.rs    # Status line
│   │   ├── themes.rs       # Theme definitions (8 themes)
│   │   ├── debugger.rs     # Debugger panel
│   │   ├── explorer.rs     # File explorer
│   │   └── help.rs         # Help system
│   │
│   ├── utils/               # Utilities
│   │   ├── mod.rs          # Module exports
│   │   ├── expr_eval.rs    # Safe expression evaluator
│   │   ├── async_exec.rs   # Async execution (tokio)
│   │   └── error.rs        # Custom error types (unused)
│   │
│   └── extensions/          # Optional features
│       ├── audio/mod.rs    # AudioMixer (rodio)
│       ├── game/mod.rs     # GameEngine (stub)
│       ├── ml/mod.rs       # MLEngine (stub)
│       ├── iot/mod.rs      # IoTManager (stub)
│       └── plugins/mod.rs  # PluginManager (stub)
│
├── Cargo.toml               # Dependencies, features
└── target/                  # Build artifacts
```

## Data Flow

### 1. Program Execution Path

```
User Input (UI)
    ↓
TimeWarpApp::execute_code()
    ↓
Interpreter::load_program(program_text)
    ├── Parse lines → (Option<line_num>, command_str)
    └── Extract PILOT labels → HashMap<label, line_idx>
    ↓
Interpreter::execute(&mut turtle)
    ├── Security: timeout (10s), max_iterations (100k)
    └── Loop: for each program line
        ├── Interpreter::execute_line(command, turtle)
        │   ├── Determine language (PILOT/BASIC/Logo)
        │   └── Dispatch to language module
        │       ├── pilot::execute(interp, cmd, turtle)
        │       ├── basic::execute(interp, cmd, turtle)
        │       └── logo::execute(interp, cmd, turtle)
        │
        ├── Match ExecutionResult:
        │   ├── Continue → current_line++
        │   ├── End → break
        │   └── Jump(line) → current_line = line
        │
        └── Error recovery (continues on non-fatal)
    ↓
Return Vec<String> (output)
    ↓
TimeWarpApp::display_output()
```

### 2. Language Detection Logic

**Order of precedence:**

1. **PILOT**: Commands start with `[A-Z]:`
   - Examples: `T:Hello`, `J:START`, `U:X=5`
   - Match pattern: `cmd.len() > 1 && cmd.chars().nth(1) == Some(':')`

2. **Logo**: First word matches Logo keyword list
   - Keywords: `FORWARD`, `FD`, `BACK`, `BK`, `LEFT`, `LT`, `RIGHT`, `RT`, `PENUP`, `PU`, `PENDOWN`, `PD`, `CLEARSCREEN`, `CS`, `HOME`, `SETXY`, `REPEAT`, `TO`, `END`
   - Case-insensitive matching

3. **BASIC**: First word matches BASIC keyword list
   - Keywords: `LET`, `PRINT`, `INPUT`, `GOTO`, `IF`, `THEN`, `FOR`, `NEXT`, `GOSUB`, `RETURN`, `REM`, `DIM`, `DATA`, `READ`
   - Case-insensitive matching

4. **Fallback**: PILOT (default)

### 3. State Management

#### Interpreter State
```rust
pub struct Interpreter {
    // Variables
    variables: HashMap<String, f64>,         // Numeric vars (X=5)
    string_variables: HashMap<String, String>, // String vars (N$="Joe")
    
    // Program
    program_lines: Vec<(Option<usize>, String)>, // [(line_num, cmd)]
    current_line: usize,                     // Execution pointer
    labels: HashMap<String, usize>,          // PILOT labels (L:START → idx)
    
    // Output
    output: Vec<String>,                     // Accumulated output lines
    
    // Control flow
    gosub_stack: Vec<usize>,                 // GOSUB/RETURN (BASIC, PILOT)
    for_stack: Vec<ForContext>,              // FOR/NEXT loops (BASIC)
    
    // PILOT-specific
    match_flag: bool,                        // M: result (Y:/N: conditional)
    last_match_set: bool,                    // Y:/N: executed this cycle
    stored_condition: Option<bool>,          // C: stored condition
    
    // I/O
    input_callback: Option<Box<dyn FnMut(&str) -> String>>, // Input handler
    last_input: String,                      // Last A:/INPUT response
    
    // Language detection
    current_language: Language,              // Reserved for future
}
```

#### Turtle State
```rust
pub struct TurtleState {
    pub x: f32,                // Position (center = 0,0)
    pub y: f32,
    pub heading: f32,          // Angle in degrees (0 = north)
    pub pen_down: bool,        // Drawing state
    pub visible: bool,         // Turtle cursor visible
    pub paths: Vec<Vec<(f32, f32)>>, // Drawing paths for rendering
}
```

### 4. Expression Evaluation

**Safe evaluation (no `eval()`):**

```
Input: "2 + 3 * X"
    ↓
ExpressionEvaluator::tokenize()
    ↓
Tokens: [2, +, 3, *, X]
    ↓
substitute_variables(vars: {"X": 5})
    ↓
Tokens: [2, +, 3, *, 5]
    ↓
to_rpn() (Shunting Yard Algorithm)
    ↓
RPN: [2, 3, 5, *, +]
    ↓
evaluate_rpn()
    ↓
Result: 17
```

**Supported operators:**
- Arithmetic: `+`, `-`, `*`, `/`, `^` (power)
- Parentheses: `(`, `)`
- Functions: `SIN`, `COS`, `TAN`, `ATAN`, `SQRT`, `ABS`, `EXP`, `LOG`, `INT`, `ROUND`, `CEILING`, `FLOOR`, `SGN`, `RND`, `MAX`, `MIN`, `POW`, `ASIN`, `ACOS`

**Operator precedence:**
1. `^` (power)
2. `*`, `/` (multiplication, division)
3. `+`, `-` (addition, subtraction)

## Design Patterns

### 1. Stateless Language Executors

Each language module (`pilot`, `basic`, `logo`) implements a pure function:

```rust
pub fn execute(
    interp: &mut Interpreter, 
    command: &str, 
    turtle: &mut TurtleState
) -> Result<ExecutionResult>
```

**Key principle:** Executors don't store state. All state lives in `Interpreter` and `TurtleState`.

**Benefits:**
- Testability: No hidden state between calls
- Composition: Easy to call one executor from another (e.g., Logo REPEAT)
- Thread safety: No Arc<Mutex<>> needed for language logic

### 2. Lazy Regex Compilation

Using `once_cell::sync::Lazy` for compile-time regex optimization:

```rust
static VAR_INTERPOLATION_PATTERN: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"\*([A-Z_][A-Z0-9_]*)\*").expect("Invalid regex")
});
```

**Performance:** 5-10x speedup over `Regex::new()` per call.

### 3. Feature-Gated Modules

Optional dependencies controlled by Cargo features:

```toml
[features]
default = []
audio = ["rodio"]
ml = []
plugins = []
```

**Usage:**
```rust
#[cfg(feature = "audio")]
use rodio::{Decoder, OutputStream, Sink};
```

**Benefit:** Faster compile times, smaller binaries when features unused.

### 4. Error Recovery with Continue-on-Error

Interpreter continues execution on non-fatal errors:

```rust
let result = match self.execute_line(command, turtle) {
    Ok(r) => r,
    Err(e) => {
        // Log error, continue
        self.log_output(format!("❌ Error: {}", e));
        ExecutionResult::Continue
    }
};
```

**Educational rationale:** Students see partial output instead of crash on first error.

## Extension Points

### 1. Adding a New Language

**Steps:**

1. Create `src/languages/newlang/mod.rs`:
```rust
use anyhow::Result;
use crate::interpreter::{Interpreter, ExecutionResult};
use crate::graphics::TurtleState;

pub fn execute(
    interp: &mut Interpreter, 
    command: &str, 
    turtle: &mut TurtleState
) -> Result<ExecutionResult> {
    // Parse command
    // Execute logic
    // Update interp.variables, turtle state
    // Return Continue/End/Jump
}
```

2. Register in `src/languages/mod.rs`:
```rust
pub enum Language {
    Pilot,
    Basic,
    Logo,
    NewLang, // Add here
}
```

3. Add detection in `interpreter/mod.rs::determine_command_type()`:
```rust
let newlang_keywords = ["KEYWORD1", "KEYWORD2"];
if newlang_keywords.contains(&first_word.to_uppercase().as_str()) {
    return Language::NewLang;
}
```

4. Dispatch in `interpreter/mod.rs::execute_line()`:
```rust
match cmd_type {
    Language::NewLang => newlang::execute(self, command, turtle),
    // ...
}
```

### 2. Adding UI Components

**Pattern:** Create widget in `src/ui/newwidget.rs`:

```rust
use egui::Ui;

pub struct NewWidget {
    state: SomeState,
}

impl NewWidget {
    pub fn new() -> Self {
        Self { state: SomeState::default() }
    }
    
    pub fn show(&mut self, ui: &mut Ui) {
        ui.label("My Widget");
        // egui code
    }
}
```

**Register in `app.rs`:**
```rust
pub struct TimeWarpApp {
    // ...
    new_widget: NewWidget,
}

impl eframe::App for TimeWarpApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            self.new_widget.show(ui);
        });
    }
}
```

### 3. Custom Themes

**Define in `src/ui/themes.rs`:**

```rust
pub fn create_my_theme() -> egui::Visuals {
    let mut visuals = egui::Visuals::dark();
    visuals.override_text_color = Some(egui::Color32::from_rgb(255, 0, 255));
    visuals.window_fill = egui::Color32::from_rgb(30, 30, 50);
    visuals
}
```

**Register in `themes.rs::get_theme()`:**
```rust
pub fn get_theme(name: &str) -> egui::Visuals {
    match name {
        "My Theme" => create_my_theme(),
        // ...
    }
}
```

## Security Considerations

### Execution Limits

1. **Max iterations:** 100,000 loops (prevents infinite loops)
2. **Timeout:** 10 seconds max execution (prevents DoS)
3. **Expression complexity:** ExpressionEvaluator limits token count

### Safe Expression Evaluation

**No `eval()` usage:**
- Custom tokenizer + RPN evaluator
- Whitelist of allowed functions
- No dynamic code execution

### Input Sanitization

- Input callback returns sanitized strings
- Variable names restricted to `[A-Z_][A-Z0-9_]*` pattern
- File I/O uses Rust's safe Path APIs (no shell injection)

## Testing Strategy

### Unit Tests (18 tests)

**Location:** `src/lib.rs::tests`, inline doctests

**Coverage:**
- Expression evaluator: 5 tests (arithmetic, precedence, functions, vars, complex)
- Audio system: 2 tests (mixer, music string)
- Interpreter: 3 tests (creation, evaluation, interpolation)
- PILOT: 6 tests (all commands, conditional flow)
- Embedded: 4 doctests in expr_eval.rs

**Run:** `cargo test --lib`

### Integration Tests (Future)

**Planned:**
- Multi-language programs (PILOT calling BASIC subroutines)
- Turtle graphics verification (path correctness)
- UI state persistence (save/load)
- Plugin loading (when implemented)

## Build Configuration

### Cargo.toml Structure

```toml
[package]
name = "time_warp_unified"
version = "2.0.0"
edition = "2021"

[dependencies]
eframe = "0.29"         # egui app framework
egui = "0.29"           # Immediate mode GUI
anyhow = "1.0"          # Error handling
regex = "1.10"          # Pattern matching
once_cell = "1.19"      # Lazy statics
thiserror = "2.0"       # Custom error derive
tokio = { version = "1.42", features = ["rt", "time", "sync"] }
rodio = { version = "0.19", optional = true }  # Audio

[features]
default = []
audio = ["rodio"]
ml = []
plugins = []

[[bin]]
name = "time-warp"
path = "src/main.rs"

[lib]
name = "time_warp_unified"
path = "src/lib.rs"
```

### Build Commands

```bash
# Development (fast compile, debug symbols)
cargo build

# Release (optimized, smaller binary)
cargo build --release

# With audio feature
cargo build --features audio

# Run tests
cargo test --lib

# Run with logging
RUST_LOG=debug cargo run

# Check without building
cargo check
```

## Performance Characteristics

### Compilation Times
- Clean build: ~45s (eframe/egui are heavy)
- Incremental: ~3-10s
- Test suite: ~3s

### Runtime Performance
- Interpreter loop: ~100k iterations/sec
- Expression evaluation: ~10μs per eval (cached regex)
- Turtle graphics: 60 FPS rendering (egui)
- Memory: ~50MB base (egui), +variable based on program

### Optimization Tips
1. Use `--release` for production (10x faster)
2. Minimize regex compilation (use Lazy statics)
3. Profile with `cargo flamegraph` (requires cargo-flamegraph)
4. Reduce eframe dependencies if binary size matters

## Roadmap

### Completed (7/12)
1. ✅ Safe Expression Evaluator
2. ✅ Async Execution Support
3. ✅ Audio System
4. ✅ PILOT Language (95%)
5. ✅ BASIC Language (85%)
6. ✅ Logo Language (75%)
7. ✅ Test Suite (18 tests)

### Next Steps (5 remaining)
8. 🔲 Complete BASIC (DATA/READ, arrays)
9. 🔲 Complete Logo (TO/END procedures, nested REPEAT)
10. 🔲 Game Engine (sprite system, collision)
11. 🔲 ML Integration (scikit-learn bindings)
12. 🔲 IoT/Robotics (GPIO, Arduino/RPi)

### Future Enhancements
- WebAssembly target (eframe supports wasm32)
- Cloud storage integration (save programs online)
- Multiplayer coding (shared editor)
- Voice coding (speech-to-text)
- VR/AR mode (turtle in 3D space)

---

**For more information:**
- User Guide: `USER_GUIDE.md`
- Implementation Status: `IMPLEMENTATION_STATUS.md`
- Testing: `TEST_RESULTS.md`
- Contributing: `CONTRIBUTING.md`
