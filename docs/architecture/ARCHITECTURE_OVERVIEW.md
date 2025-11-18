# Architecture Overview

**Time Warp IDE - System Architecture Documentation**

**Document Type:** Architecture  
**Audience:** Developers and Technical Contributors  
**Last Updated:** November 18, 2025

---

## Table of Contents

- [Executive Summary](#executive-summary)
- [System Architecture](#system-architecture)
- [Design Principles](#design-principles)
- [Component Architecture](#component-architecture)
- [Platform Implementations](#platform-implementations)
- [Data Flow](#data-flow)
- [Security Architecture](#security-architecture)
- [Performance Considerations](#performance-considerations)
- [Future Architecture](#future-architecture)

---

## Executive Summary

Time Warp IDE is a multi-platform educational programming environment designed around a unified language interpreter (TempleCode) with platform-specific UI implementations. The architecture prioritizes educational clarity, safety, and cross-platform consistency while allowing platform-specific optimizations.

**Key Architectural Decisions:**
- Stateless language executors with UI-managed state
- Modular platform implementations sharing core concepts
- Safe expression evaluation without code injection
- Emoji-prefixed output for standardized communication
- Turtle graphics as a first-class visualization system

**Current Version:** 3.0.0  
**Primary Implementation:** Rust (egui/eframe)  
**Supported Platforms:** Rust, Python, Go, Web, DOS, Windows, macOS

---

## System Architecture

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   Time Warp IDE                          │
│                                                           │
│  ┌───────────────┐  ┌───────────────┐  ┌──────────────┐ │
│  │   UI Layer    │  │  Interpreter  │  │   Graphics   │ │
│  │               │  │     Core      │  │    Engine    │ │
│  │  - Editor     │←→│               │←→│              │ │
│  │  - Output     │  │  - BASIC      │  │  - Turtle    │ │
│  │  - Menus      │  │  - PILOT      │  │  - Canvas    │ │
│  │  - Themes     │  │  - Logo       │  │  - Export    │ │
│  └───────────────┘  └───────────────┘  └──────────────┘ │
│           ↑                  ↑                  ↑         │
│           └──────────────────┴──────────────────┘         │
│                    Platform Layer                         │
│         (Rust/egui, Python/PySide6, Go/Fyne, etc.)       │
└─────────────────────────────────────────────────────────┘
```

### Layer Responsibilities

**UI Layer:**
- User interaction (editor, buttons, menus)
- State management (themes, settings)
- Visual rendering (turtle canvas, output display)

**Interpreter Core:**
- Command parsing and execution
- Variable management
- Language-specific logic (BASIC, PILOT, Logo)
- Output generation with emoji prefixes

**Graphics Engine:**
- Turtle state management
- Line drawing and rendering
- Color management
- Image export (PNG)

**Platform Layer:**
- OS integration
- File I/O
- Window management
- Native UI widgets

---

## Design Principles

### 1. Stateless Executors

Language executors are stateless command processors that:
- Parse individual commands
- Update internal state (variables, turtle position)
- Return text output only
- Never store UI components or graphics contexts

**Benefits:**
- Easy to test in isolation
- Clear separation of concerns
- Simple to extend with new commands
- Platform-independent logic

**Example:**

```rust
pub trait LanguageExecutor {
    fn execute_command(&mut self, command: &str) -> Result<String, String>;
    fn reset(&mut self);
}

impl BasicExecutor {
    pub fn execute_command(&mut self, command: &str) -> Result<String, String> {
        // Parse and execute
        // Update internal state only
        // Return string output
        Ok("✅ Command executed\n".to_string())
    }
}
```

### 2. UI-Centric State Management

All visual state lives in the UI layer:
- Turtle canvas and drawing state
- Theme colors and styles
- Output buffer and scrolling
- Editor content and cursor position

**Benefits:**
- Consistent rendering across platforms
- Easy to save/restore UI state
- Decoupled from interpreter logic

### 3. Emoji-Prefixed Communication

Standardized output markers for interpreter communication:

| Emoji | Meaning | Use Case |
|-------|---------|----------|
| ❌ | Error | Syntax errors, runtime errors |
| ✅ | Success | Successful command execution |
| ℹ️ | Info | Status messages, help text |
| 🎨 | Theme | Theme change notifications |
| 🚀 | Run | Program start/end |
| 🐢 | Turtle | Turtle graphics actions |
| 📝 | Input | Input prompts |

**Benefits:**
- Easy parsing and filtering
- Visual clarity in output
- Standardized across platforms
- Machine-readable format

### 4. Safe Expression Evaluation

No use of `eval()` or equivalent unsafe operations:
- Custom expression parser for math
- Whitelist of allowed operations
- Type checking and validation
- Timeout protection

**Example:**

```rust
// Safe evaluation
let result = safe_eval("2 + 3 * x", &variables)?;

// NOT this:
// let result = eval(user_input);  // NEVER!
```

### 5. Educational Focus

Architecture prioritizes learning:
- Clear error messages
- Beginner-friendly design
- Visual feedback (turtle graphics)
- Progressive disclosure of complexity

---

## Component Architecture

### Interpreter Core

```
TimeWarpInterpreter
├── BasicExecutor
│   ├── Variable management
│   ├── Control flow (IF/GOTO/FOR)
│   └── I/O (PRINT/INPUT)
│
├── PilotExecutor
│   ├── TYPE/ACCEPT commands
│   ├── Label/JUMP system
│   └── Variable substitution (*NAME*)
│
└── LogoExecutor
    ├── Turtle commands (FD/LT/RT)
    ├── Procedures (TO/END)
    └── REPEAT loops
```

**Shared Components:**
- Variable storage (HashMap)
- Expression evaluator
- Output buffer management

### UI Components

**Editor:**
- Syntax highlighting (optional)
- Line numbers
- Copy/paste support
- Undo/redo

**Output Display:**
- Scrolling text area
- Emoji rendering
- Color-coded messages
- Copy-to-clipboard

**Turtle Canvas:**
- Vector drawing
- Zoom and pan
- Clear/reset
- PNG export

**Menu System:**
- File (New/Open/Save/Export)
- Edit (Copy/Paste/Clear)
- View (Themes/Zoom)
- Help (About/Documentation)

### Graphics Engine

```rust
pub struct TurtleState {
    pub x: f32,
    pub y: f32,
    pub angle: f32,
    pub pen_down: bool,
    pub pen_color: Color32,
    pub pen_width: f32,
    pub lines: Vec<Line>,
}

pub struct Line {
    pub x1: f32,
    pub y1: f32,
    pub x2: f32,
    pub y2: f32,
    pub color: Color32,
    pub width: f32,
}
```

**Rendering Pipeline:**
1. Executor updates turtle state
2. UI reads turtle state each frame
3. Lines are drawn to canvas
4. Canvas can be exported to PNG

---

## Platform Implementations

### Rust Implementation (Primary)

**Technology Stack:**
- **UI:** egui/eframe
- **Graphics:** egui painter
- **Build:** Cargo
- **Language:** Rust 2021 edition

**Architecture:**

```
time_warp_unified/
├── src/
│   ├── main.rs           # Entry point, CLI handling
│   ├── app.rs            # TimeWarpApp struct, UI logic
│   ├── interpreter/
│   │   └── mod.rs        # Interpreter core
│   ├── languages/
│   │   ├── basic.rs      # BASIC executor
│   │   ├── pilot.rs      # PILOT executor
│   │   └── logo.rs       # Logo executor
│   ├── graphics/
│   │   └── turtle.rs     # Turtle graphics
│   ├── ui/
│   │   ├── editor.rs     # Code editor
│   │   ├── output.rs     # Output panel
│   │   └── themes.rs     # Theme system
│   └── utils/
│       └── expression.rs # Safe evaluator
└── Cargo.toml
```

**Key Features:**
- Native performance
- Single binary distribution
- Cross-platform (Windows, macOS, Linux)
- 8 built-in themes

### Python Implementation

**Technology Stack:**
- **UI:** PySide6 (Qt6)
- **Graphics:** QPainter
- **Language:** Python 3.10+

**Architecture:**

```
Time_Warp_Python/
├── run_time_warp.py      # Entry point
├── core/
│   ├── interpreter.py    # Main interpreter (1500+ lines)
│   ├── interpreters/
│   │   ├── basic.py      # BASIC executor
│   │   ├── pilot.py      # PILOT executor
│   │   └── logo.py       # Logo executor
│   └── safe_expression_evaluator.py
├── ui/
│   └── qt_ui.py          # Qt UI factory
└── tests/                # Pytest suite
```

**Key Features:**
- Easy to extend
- Rich ecosystem
- Good for prototyping

### Go Implementation

**Technology Stack:**
- **UI:** Fyne
- **CLI:** Standard library
- **Language:** Go 1.20+

**Features:**
- Fast CLI interpreter
- Batch processing
- Alternative GUI option

### Web Implementation

**Technology Stack:**
- **Backend:** Rust → WebAssembly
- **Frontend:** JavaScript + HTML5 Canvas
- **Build:** wasm-pack

**Features:**
- Zero installation
- Browser-based
- Progressive Web App (PWA)

### DOS Implementation

**Technology Stack:**
- **Language:** C (DJGPP)
- **Graphics:** VGA Mode 13h (320x200, 256 colors)
- **Platform:** FreeDOS, MS-DOS

**Features:**
- Retro computing
- Historical education
- Minimal dependencies

---

## Data Flow

### Command Execution Flow

```
User Input → UI Layer → Interpreter → Executor → State Update
     ↓                                                  ↓
  Display ← ← ← Output Buffer ← ← ← ← ← ← ← ← ← ← ← ← ┘
```

**Detailed Steps:**

1. **User enters command** in editor
2. **UI sends command** to interpreter
3. **Interpreter detects language** (BASIC/PILOT/Logo)
4. **Executor parses command** and validates syntax
5. **Executor updates state** (variables, turtle position)
6. **Executor returns output** (text with emoji prefix)
7. **UI displays output** in output panel
8. **UI renders graphics** (if turtle command)

### File Operations

```
Open File:
UI → File Dialog → Read File → Set Editor Content

Save File:
UI → Get Editor Content → File Dialog → Write File

Export Graphics:
UI → Capture Canvas → PNG Encoder → File Dialog → Write Image
```

### Theme System

```
User Selects Theme:
UI → Theme Manager → Load Colors → Apply to Widgets → Persist to Config
```

---

## Security Architecture

### Threat Model

**Potential Threats:**
- Code injection via user input
- File system access abuse
- Memory exhaustion
- Infinite loops
- Malicious file loading

### Security Measures

**1. Safe Expression Evaluation:**
- No `eval()` or dynamic code execution
- Whitelist of allowed operations
- Type checking and validation

**2. Resource Limits:**
- Maximum execution time (configurable)
- Memory allocation limits
- Stack depth limits

**3. File System Protection:**
- Sandboxed file access (optional)
- Path validation
- Read-only mode for sensitive directories

**4. Input Validation:**
- Sanitize all user input
- Check file extensions
- Validate numeric ranges

**5. Error Handling:**
- Graceful degradation
- No sensitive info in error messages
- Logging for security events

---

## Performance Considerations

### Optimization Strategies

**1. Expression Caching:**
```rust
// Cache parsed expressions for reuse
let cache = LruCache::new(100);
if let Some(result) = cache.get(&expression) {
    return Ok(*result);
}
```

**2. Lazy Rendering:**
- Only redraw when state changes
- Use dirty flags
- Batch updates

**3. Async Execution:**
```rust
// Non-blocking code execution
tokio::spawn(async move {
    interpreter.execute(code).await
});
```

**4. Memory Management:**
- Reuse buffers
- Clear old turtle lines
- Limit output buffer size

### Performance Targets

| Metric | Target | Actual (Rust) |
|--------|--------|---------------|
| Startup Time | < 1s | 0.3s |
| Command Response | < 50ms | 10ms |
| Turtle Draw (100 lines) | < 16ms | 5ms |
| Memory Usage | < 100 MB | 50 MB |

---

## Future Architecture

### Planned Enhancements

**1. Plugin System:**
```
plugins/
├── audio/           # Sound effects
├── sensors/         # IoT integration
└── ml/              # Machine learning
```

**2. Cloud Sync:**
- Save programs to cloud
- Share with classrooms
- Collaborative editing

**3. Mobile Support:**
- iOS app (Swift)
- Android app (Kotlin)
- Touch-optimized UI

**4. Enhanced Graphics:**
- 3D turtle graphics
- Multiple turtles
- Sprite support
- Animation timeline

**5. AI Assistance:**
- Code completion
- Error suggestions
- Learning hints

### Migration Strategy

**Phase 1:** Stabilize current architecture
**Phase 2:** Add plugin system
**Phase 3:** Cloud integration
**Phase 4:** Mobile platforms
**Phase 5:** Advanced features

---

## Dependencies

### Rust Dependencies

```toml
[dependencies]
eframe = "0.29"         # UI framework
egui = "0.29"          # Immediate mode GUI
rfd = "0.15"           # File dialogs
anyhow = "1.0"         # Error handling
serde = "1.0"          # Serialization
regex = "1.10"         # Parsing
tokio = "1.42"         # Async runtime
image = "0.25"         # Image encoding
```

### Python Dependencies

```
PySide6>=6.5.0         # Qt6 bindings
Pillow>=10.0.0         # Image processing
pytest>=7.0.0          # Testing
pytest-cov>=4.0.0      # Coverage
```

---

## Architectural Decisions

### ADR-001: Stateless Executors

**Status:** Accepted  
**Date:** 2025-10-27

**Context:** Need separation between interpreter logic and UI rendering.

**Decision:** Language executors are stateless command processors that only return text output.

**Consequences:**
- ✅ Easy to test
- ✅ Platform-independent
- ✅ Simple to extend
- ❌ Requires UI to poll for graphics state

### ADR-002: Emoji-Prefixed Output

**Status:** Accepted  
**Date:** 2025-10-27

**Context:** Need standardized communication between interpreter and UI.

**Decision:** Use emoji prefixes for all interpreter output.

**Consequences:**
- ✅ Visual clarity
- ✅ Easy to parse
- ✅ Cross-platform compatible
- ❌ Requires Unicode support

### ADR-003: Primary Rust Implementation

**Status:** Accepted  
**Date:** 2025-10-27

**Context:** Need performant, distributable implementation.

**Decision:** Make Rust/egui the primary recommended implementation.

**Consequences:**
- ✅ Native performance
- ✅ Single binary distribution
- ✅ Memory safety
- ❌ Steeper learning curve for contributors

---

## Conclusion

Time Warp IDE's architecture is designed for educational use with a focus on safety, clarity, and cross-platform consistency. The stateless executor pattern and UI-centric state management provide a clean separation of concerns while enabling platform-specific optimizations.

**Key Takeaways:**
- Simple, testable components
- Safe expression evaluation
- Consistent cross-platform behavior
- Room for future enhancements

For implementation details, see:
- [Developer Guide](DEVELOPER_GUIDE.md)
- [Technical Reference](docs/TECHNICAL_REFERENCE.md)
- [Platform READMEs](Time_Warp_Rust/README.md)

---

**Last Updated:** November 18, 2025  
**Maintainer:** James Temple <james@honey-badger.org>  
**License:** MIT
