# Time Warp IDE - Developer Reference

This document describes the internal APIs and extension points for building on the Time Warp IDE.

## Architecture Overview

- Core interpreter: `src/interpreter/mod.rs`
- Language executors: `src/languages/{pilot,basic,logo}/mod.rs`
- UI (egui/eframe): `src/ui/*`
- Graphics/turtle: `src/graphics/mod.rs` (state) and `src/ui/canvas.rs` (render)

Executors are stateless command processors that operate on the `Interpreter` and `TurtleState` and return `ExecutionResult`.

## Key Types

### Interpreter

- Variables
  - `variables: HashMap<String, f64>` numeric
  - `string_variables: HashMap<String, String>` strings
- Program
  - `program_lines: Vec<(Option<usize>, String)>`
  - `current_line: usize`
  - `labels: HashMap<String, usize>` (PILOT)
- Control flow
  - `gosub_stack: Vec<usize>`
  - `for_stack: Vec<ForContext>`
- Input
  - `input_callback: Option<Box<dyn FnMut(&str) -> String>>`
  - `last_input: String`
  - `pending_input: Option<InputRequest>` (UI prompt)
  - `pending_resume_line: Option<usize>`
- Logo procedures
  - `logo_procedures: HashMap<String, LogoProcedure>`

Methods:

- `load_program(&mut self, program_text: &str)`
- `execute(&mut self, turtle: &mut TurtleState) -> Result<Vec<String>>`
  - Returns once program finishes or pauses on `WaitForInput`
- `request_input(&mut self, prompt: &str) -> String`
- `start_input_request(&mut self, prompt: &str, var: &str, prefer_numeric: bool)`
- `provide_input(&mut self, value: &str)`
- `evaluate_expression(&self, expr: &str) -> Result<f64>`
- `interpolate_text(&self, text: &str) -> String`

### ExecutionResult

```rust
pub enum ExecutionResult {
    Continue,
    End,
    Jump(usize),
    WaitForInput,
}
```

- Use `WaitForInput` to suspend execution and let the UI collect input.

### Language Dispatch

`Interpreter::determine_command_type` routes each line based on syntax and keyword tables. Executors implement:

```rust
pub fn execute(interp: &mut Interpreter, command: &str, turtle: &mut TurtleState) -> Result<ExecutionResult>;
```

### TurtleState and Graphics

- Tracks turtle position, pen state, color, width, background
- Stores `lines: Vec<Line>` for rendering and PNG export
- `save_png(path)` uses anti-aliased line drawing

## UI Integration

- `app.rs` creates `TimeWarpApp` with `interpreter` and `turtle_state`
- `ui/output.rs` renders output text and the canvas
- Input prompts: when `pending_input` is Some, a modal appears; submitting calls `provide_input()` and resumes execution

## Extending the IDE

- Add new language keywords in the respective executor module
- Use the expression evaluator (`utils/expr_eval.rs`) for safe math and functions
- Keep executors stateless: return messages via `interp.log_output(...)` and mutate only interpreter/turtle state

## Error Handling and Limits

- Execution timeout (10s) and max iterations (100k)
- Non-fatal errors are logged to output and execution continues

## Building and Testing

- Build: `cargo build`
- Run: `cargo run`
- Tests: `cargo test`

## Examples and Demos

See `examples/` for programs showcasing input, graphics, Logo procedures, and gameplay patterns.
